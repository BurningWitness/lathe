{-# LANGUAGE BangPatterns
           , MagicHash
           , PatternSynonyms
           , RankNTypes
           , UnboxedTuples
           , UnboxedSums
           , UnliftedNewtypes #-}

module Parser.Lathe.Internal
  ( ByteOffset
  , Result (..)
  , Resupply (..)
  , Partial (..)

  , TotalOffset
  , ChunkOffset
  , More (..)
  , Rollback (..)
  , Policy (..)

  , Core
  , Res (Yes, No, ..)
  , Dec (Re, Fin, ..)
  , Parser (..)

  , UnexpectedPartial (..)
  , parse

  , Blank (..)
  , prepare
  , Scrap (..)
  , scrap
  , draw

  , catch
  , match

  , bytesRead
  , atEnd

  , err
  , mapError

  , skip
  , unsafeSkip
  , skipEndOr
  , unsafeSkipEndOr
  , skipNul
  , skipUntil
  , skipUntilEndOr

  , byteString
  , unsafeByteString
  , byteStringNul
  , byteStringUntil

  , int8
  , word8
  , skip1
  , skipEndOr1

  , unsafeRead

  , shortByteString
  , unsafeShortByteString
  , shortByteStringNul
  , shortByteStringUntil

  , lazyByteString
  , unsafeLazyByteString
  , lazyByteStringNul
  , lazyByteStringUntil
  , lazyByteStringRest
  ) where

import           Parser.Lathe.Internal.ByteString

import           Control.Applicative
import           Control.Exception (Exception, throw)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Internal as L (ByteString (..), chunk)
import           Data.ByteString.Short (ShortByteString)
import qualified Data.ByteString.Short as Short
import qualified Data.ByteString.Unsafe as B
import           Data.Int
import           Data.Word



-- | An offset, counted in bytes.
type ByteOffset = Int64

-- | Providing additional input to the decoder.
data Resupply = Supply
                  !B.ByteString
                    -- ^ A chunk of the input. It __should not__ be empty.
                    --
                    --   N.B.: Lazy 'Data.ByteString.Lazy.ByteString's have an internal
                    --   "no empty chunks" invariant. While this parser does not
                    --   malfunction when encountering an empty chunk,
                    --   it does not purge empty chunks and will faithfully relay
                    --   them in lazy 'Data.ByteString.Lazy.ByteString's it produces.
                    --   This may in turn break downstream consumers.

              | EndOfInput

-- | Final parsing outcome.
data Result a = Result
                  L.ByteString               -- ^ Remaining unconsumed input.
                  {-# UNPACK #-} !ByteOffset -- ^ Total number of bytes consumed.
                  a
                deriving Show

instance Functor Result where
  fmap f (Result s i a) = Result s i (f a)

-- | Intermediate parsing outcome.
data Partial a = Partial (Resupply -> Partial a)
               | Done a

instance Functor Partial where
  fmap f (Partial k) = Partial (fmap f . k)
  fmap f (Done a)    = Done (f a)

instance Show a => Show (Partial a) where
  showsPrec d x =
    showParen (d > 10) $
      case x of
        Partial _ -> showString "Partial _"
        Done res  -> showString "Done " . showsPrec 11 res



-- | Global offset of the start of the current chunk, in bytes.
type TotalOffset = Int64

-- | Local offset inside the chunk, in bytes.
type ChunkOffset = Int

-- | Chunk retention policy.
data Policy = Drop -- ^ Do not keep the reference.
            | Keep -- ^ Keep the reference in the t'Rollback'.
              deriving Show

-- | A snoc-list of all consumed chunks needed in the future.
data Rollback = Rollback !Rollback {-# UNPACK #-} !B.ByteString
              | Bottom
                deriving Show

-- | Whether more input can be supplied.
data More = More -- ^ Can prompt for more state.
          | End  -- ^ End has been reached.
            deriving (Show, Eq)

-- | Common parser state bundled together for convenience.
type Core =
       (# TotalOffset, ChunkOffset, B.ByteString, L.ByteString, More, Rollback #)

-- | Unboxed 'Either' counterpart.
newtype Res e a = Res (# a | e #)

-- | Unboxed t'Partial' counterpart.
newtype Dec e a = Dec (# (# Core, Res e a #) | Resupply -> Dec e a #)

-- | The parser type, parametrized by an error type @e@ and a return type @a@.
--
--   Note that there is no 'Control.Applicative.Alternative' instance for this parser,
--   see instead 'Parser.Lathe.catch'.
newtype Parser e a =
          Parser
            { runParser
                :: Core
                -> Policy
                -> Dec e a
            }

{-# COMPLETE Re, Fin #-}
pattern Re :: (Resupply -> Dec e a) -> Dec e a
pattern Re loop = Dec (# | loop #)

pattern Fin :: Core -> Res e a -> Dec e a
pattern Fin core ea = Dec (# (# core, ea #) | #)

{-# COMPLETE Yes, No #-}
pattern Yes :: a -> Res e a
pattern Yes a = Res (# a | #)

pattern No :: e -> Res e a
pattern No e = Res (# | e #)

instance Functor (Parser e) where
  {-# INLINE fmap #-}
  fmap f (Parser p) =
    Parser $ \core loc ->

      let tie !x =
            case x of
              Re loop       -> Re $ \re -> tie (loop re)
              Fin core' res ->
                case res of
                  Yes a -> Fin core' (Yes (f a))
                  No e  -> Fin core' (No e)

      in tie (p core loc)

instance Applicative (Parser e) where
  {-# INLINE pure #-}
  pure = \x ->
    Parser $ \core _ -> Fin core (Yes x)

  {-# INLINE (<*>) #-}
  g <*> b = do
    f <- g
    a <- b
    pure (f a)

  {-# INLINE liftA2 #-}
  liftA2 f a b = f <$> a <*> b

instance Monad (Parser e) where
  {-# INLINE return #-}
  return = pure

  {-# INLINE (>>=) #-}
  Parser p >>= m =
    Parser $ \core pol ->

      let tie !x =
            case x of
              Re loop       -> Re $ \re -> tie (loop re)
              Fin core' res ->
                case res of
                  Yes a -> runParser (m a) core' pol
                  No e  -> Fin core' (No e)

      in tie (p core pol)



{-# INLINEABLE catch #-}
-- | Execute the left parser;
--   should an error occur, backtrack and execute the right parser.
--
--   References to all new input chunks consumed by the left parser
--   are kept until it completes.
catch :: Parser e a -> (e -> Parser x a) -> Parser x a
catch (Parser f) g =
  Parser $ \(# i, o, bs, lbs, more, roll #) pol ->

    let tie !x =
          case x of
            Re loop -> Re $ \re -> tie (loop re)

            Fin (# i', o', bs', lbs_, more', delta #) res ->
              case res of
                Yes a ->
                  case pol of
                    Drop -> Fin (# i', o', bs', lbs_, more', Bottom #) (Yes a)
                    Keep ->
                      let !(# roll' #) = retain roll delta
                      in Fin (# i', o', bs', lbs_, more', roll' #) (Yes a)

                No e           ->
                  let !(# lbs' #) = rollback delta lbs_

                  in runParser (g e) (# i, o, bs, lbs', more', roll #) pol

    in tie (f (# i, o, bs, lbs, more, Bottom #) Keep)



{-# INLINEABLE match #-}
-- | Execute the supplied parser, returning the slice of input consumed in the
--   process alongside the result.
--
--   References to all new input chunks consumed by the supplied parser
--   are kept until it completes.
match :: Parser e a -> Parser e (L.ByteString, a)
match (Parser f) =
  Parser $ \(# i, o, bs, lbs, more, roll #) pol ->

    let tie !x =
          case x of
            Re loop -> Re $ \re -> tie (loop re)

            Fin (# i', o', bs', lbs', more', delta #) res ->
              case res of
                Yes a ->
                  case pol of
                    Drop ->
                      let !r = case delta of
                                 Rollback delta' _ ->
                                   let !r0 = L.chunk (takeCopy Copy o' bs') L.empty

                                       !(# r1 #) = rollback delta' r0

                                   in L.chunk (dropCopy Copy o bs) r1

                                 Bottom            ->
                                   L.chunk (sliceCopy Copy o (o' - o) bs) L.empty

                      in Fin (# i', o', bs', lbs', more', Bottom #) (Yes (r, a))

                    Keep ->
                      let !(# roll', !r #) =
                             case delta of
                               Rollback _ _ ->
                                 let !r0 = L.chunk (takeCopy Copy o' bs') L.empty

                                     !(# roll1, r1 #) = retainRollback roll delta r0

                                 in (# roll1, L.chunk (dropCopy Copy o bs) r1 #)

                               Bottom            ->
                                 (# roll
                                  , L.chunk (sliceCopy Copy o (o' - o) bs) L.empty
                                  #)

                      in Fin (# i', o', bs', lbs', more', roll' #) (Yes (r, a))

                No e ->
                  case pol of
                    Drop ->
                      Fin (# i', o', bs', lbs', more', Bottom #) (No e)

                    Keep ->
                      let !(# roll' #) = retain roll delta
                      in Fin (# i', o', bs', lbs', more', roll' #) (No e)

    in tie (f (# i, o, bs, lbs, more, Bottom #) Keep)



-- | Prepend all stored chunks to the 'L.ByteString'.
rollback :: Rollback -> L.ByteString -> (# L.ByteString #)
rollback Bottom         bs = (# bs #)
rollback (Rollback r b) bs = rollback r $ L.Chunk b bs

-- | Append right t'Rollback' to the left one.
retain :: Rollback -> Rollback -> (# Rollback #)
retain x Bottom         = (# x #)
retain x (Rollback r b) = let !(# r' #) = retain x r
                          in (# Rollback r' b #)

-- | 'retain' and 'rollback' in one motion.
retainRollback :: Rollback -> Rollback -> L.ByteString -> (# Rollback, L.ByteString #)
retainRollback x Bottom         bs = (# x, bs #)
retainRollback x (Rollback r b) bs =
  let !(# r', cs #) = retainRollback x r (L.Chunk b bs)
  in (# Rollback r' b, cs #)



-- | Helper exception for excluding the /hopefully/ impossible v'Partial' result
--   in 'parse'.
data UnexpectedPartial = UnexpectedPartial

instance Show UnexpectedPartial where
  showsPrec _ _ =
    showString
      "lathe#Parser.Lathe.parse: \
        \parser was instructed to never prompt for more input, yet prompted anyway"

instance Exception UnexpectedPartial

-- | Run a parser by providing all of the input immediately.
parse :: Parser e a -> L.ByteString -> (Scrap, Either e a)
parse g lbs0 =
  let !(# bs, lbs #) = case lbs0 of
                         L.Chunk bs' lbs' -> (# bs', lbs' #)
                         L.Empty          -> (# B.empty, L.Empty #)

  in case draw g (Blank 0 0 bs lbs End) of
       Partial _         -> throw UnexpectedPartial
       Done (blank, res) ->
         let !s = scrap blank
         in (s, res)



-- | Internal processing state.
data Blank =
       -- | Mirrors the 'Core'.
       Blank
         {-# UNPACK #-} !TotalOffset
         {-# UNPACK #-} !ChunkOffset
         !B.ByteString
         !L.ByteString
         !More

-- | Define the initial parser state.
prepare
  :: ByteOffset   -- ^ Initial byte offset.
  -> B.ByteString -- ^ First chunk of the stream. It may be empty.
  -> L.ByteString -- ^ Rest of the known stream.
  -> More         -- ^ Whether more input can be requested.
  -> Blank
prepare i = Blank i 0

-- | Remaining bits of state.
data Scrap =
       Scrap
         {-# UNPACK #-} !ByteOffset -- ^ Number of bytes consumed.
         !L.ByteString              -- ^ Remaining input.
         !More                      -- ^ Whether more input could still be requested.
       deriving Show

-- | Convert excess input into readable form.
scrap :: Blank -> Scrap
scrap (Blank i o bs lbs more) =
  let !i' = i + fromIntegral o

      !(# lbs' #) | o == B.length bs = (# lbs #)
                  | otherwise        = let !lbs_ = L.chunk (B.unsafeDrop o bs) lbs
                                       in (# lbs_ #)

  in Scrap i' lbs' more



-- | Run a parser incrementally.
draw :: Parser e a -> Blank -> Partial (Blank, Either e a)
draw (Parser p) (Blank i0 o0 bs0 lbs0 more0) =

  let tie !x =
        case x of
          Re loop -> Partial $ \resupply -> tie (loop resupply)

          Fin (# i, o, bs, lbs, more, _ #) res ->
            let !ei = case res of
                        Yes a -> Right a
                        No e  -> Left e

            in Done (Blank i o bs lbs more, ei)

  in tie (p (# i0, o0, bs0, lbs0, more0, Bottom #) Drop)



{-# INLINE advance #-}
advance
  :: Res e a
  -> TotalOffset
  -> B.ByteString
  -> L.ByteString
  -> More
  -> Rollback
  -> Policy
  -> (TotalOffset -> B.ByteString -> L.ByteString -> Rollback -> Dec e a)
  -> Dec e a
advance eof i bs lbs more roll !pol next =
  case lbs of
    L.Chunk bs' lbs' ->
      let !i' = i + fromIntegral (B.length bs)

          !(# roll' #) = case pol of
                           Drop -> (# roll #)
                           Keep -> (# Rollback roll bs' #)

      in next i' bs' lbs' roll'

    L.Empty          ->
      case more of
        More ->
          Re $ \resupply ->
            case resupply of
              Supply bs' ->
                let !i' = i + fromIntegral (B.length bs)

                    !(# roll' #) =
                      case pol of
                        Drop -> (# roll #)
                        Keep -> (# Rollback roll bs' #)

                in next i' bs' L.empty roll'

              EndOfInput -> Fin (# i, B.length bs, bs, lbs, End, roll #) eof

        End  -> Fin (# i, B.length bs, bs, lbs, End, roll #) eof



{-# INLINE err #-}
-- | Fail with the given error.
err :: e -> Parser e a
err e = Parser $ \core _ -> Fin core (No e)



{-# INLINE mapError #-}
-- | Modify the error type, or forget an error.
mapError :: (e -> Either x a) -> Parser e a -> Parser x a
mapError f (Parser p) =
  Parser $ \core pol ->

    let tie !x =
          case x of
            Re loop       -> Re $ \re -> tie (loop re)
            Fin core' res ->
              case res of
                Yes a -> Fin core' (Yes a)
                No e  ->
                  let !ea = case f e of
                              Left e' -> No e'
                              Right a -> Yes a

                  in Fin core' ea

    in tie (p core pol)



{-# INLINE bytesRead #-}
-- | Get the total number of bytes read to this point.
bytesRead :: Parser never ByteOffset
bytesRead =
  Parser $ \core@(# i, o, _, _, _, _ #) _ ->
    let !i' = i + fromIntegral o
    in Fin core (Yes i')



{-# INLINE atEnd #-}
-- | Test whether all input has been consumed.
atEnd :: Parser never Bool
atEnd =
  Parser $ \core@(# i, o, bs, lbs, more, roll #) pol ->
    if o == B.length bs && L.null lbs
      then case more of
             More -> Re (atEnd_ i o bs lbs roll pol)
             End  -> Fin core (Yes True)

      else Fin core (Yes False)

atEnd_
  :: TotalOffset
  -> ChunkOffset
  -> B.ByteString
  -> L.ByteString
  -> Rollback
  -> Policy
  -> Resupply
  -> Dec e Bool
atEnd_ i o bs lbs roll pol resupply =
  case resupply of
    Supply bs' ->
      let !i' = i + fromIntegral (B.length bs)

          !(# roll' #) =
            case pol of
              Drop -> (# roll #)
              Keep -> (# Rollback roll bs' #)

      in if B.length bs' <= 0
           then Re (atEnd_ i' 0 bs' L.Empty roll pol)
           else Fin (# i', 0, bs', L.empty, More, roll' #) (Yes False)

    EndOfInput -> Fin (# i, o, bs, lbs, End, roll #) (Yes True)



-- | Chunk copying strategy, for cases when input stream chunks
--   can be directly used to form the result instead of performing a copy.
data Copying = Original -- ^ Keep original references.
             | Copy     -- ^ Copy the chunk(s).
               deriving Show

{-# INLINE dropCopy #-}
-- | Drop the given number of bytes and optionally copy a chunk.
dropCopy :: Copying -> Int -> B.ByteString -> B.ByteString
dropCopy !copy o b
  | o == 0    = b
  | otherwise = let b' = B.unsafeDrop o b
                in case copy of
                     Original -> b'
                     Copy     -> B.copy b'

{-# INLINE sliceCopy #-}
-- | Slice the number of bytes from an offset and optionally copy a chunk.
sliceCopy
  :: Copying
  -> Int          -- ^ offset
  -> Int          -- ^ number of bytes
  -> B.ByteString
  -> B.ByteString
sliceCopy !copy o !n b
  | o == 0 && n == B.length b = b
  | otherwise                 = let b' = B.unsafeTake n $ B.unsafeDrop o b
                                in case copy of
                                     Original -> b'
                                     Copy     -> B.copy b'

{-# INLINE takeCopy #-}
-- | Take the given number of bytes and optionally copy a chunk.
takeCopy :: Copying -> Int -> B.ByteString -> B.ByteString
takeCopy !copy n b
  | n == B.length b = b
  | otherwise       = let b' = B.unsafeTake n b
                      in case copy of
                           Original -> b'
                           Copy     -> B.copy b'



{-# INLINE byteString #-}
-- | Consume \(n\) bytes into a strict 'B.ByteString'.
--
--   Returns an empty string if \(n \le 0\).
byteString :: Int -> end -> Parser end B.ByteString
byteString n e =
  Parser $ \core pol ->
    if n <= 0
      then Fin core (Yes B.empty)
      else runParser (unsafeByteString n e) core pol

{-# INLINE unsafeByteString #-}
-- | Consume \(n\) bytes into a 'ShortByteString'.
--
--   \(n\) __must__ be non-negative.
unsafeByteString :: Int -> end -> Parser end B.ByteString
unsafeByteString n e =
  Parser $ \(# i, o, bs, lbs, more, roll #) pol ->
    let o' = o + n

        n' = o' - B.length bs

    in if n' <= 0
         then let !r = sliceCopy Copy o n bs

              in Fin (# i, o', bs, lbs, more, roll #) (Yes r)

         else let !(# acc #) | o == B.length bs = (# id #)
                             | otherwise        = let !r = B.unsafeDrop o bs
                                                  in (# L.Chunk r #)

              in take_ Original e (toStrictLen n) more pol n' acc i bs lbs roll



{-# INLINE shortByteString #-}
-- | Consume \(n\) bytes into a 'ShortByteString'.
--
--   Returns an empty string if \(n \le 0\).
shortByteString :: Int -> end -> Parser end ShortByteString
shortByteString n e =
  Parser $ \core pol ->
    if n <= 0
      then Fin core (Yes Short.empty)
      else runParser (unsafeShortByteString n e) core pol

{-# INLINE unsafeShortByteString #-}
-- | Consume \(n\) bytes into a 'ShortByteString'.
--
--   \(n\) __must__ be non-negative.
unsafeShortByteString :: Int -> end -> Parser end ShortByteString
unsafeShortByteString n e =
  Parser $ \(# i, o, bs, lbs, more, roll #) pol ->
    let o' = o + n

        n' = o' - B.length bs

    in if n' <= 0
         then let !r = Short.toShort . B.unsafeTake n $ B.unsafeDrop o bs

              in Fin (# i, o', bs, lbs, more, roll #) (Yes r)

         else let !(# acc #) | o == B.length bs = (# id #)
                             | otherwise        = let !r = B.unsafeDrop o bs
                                                  in (# L.Chunk r #)

              in take_ Original e (toShortLen n) more pol n' acc i bs lbs roll



{-# INLINE lazyByteString #-}
-- | Consume \(n\) bytes into a lazy 'L.ByteString'.
--
--   Returns an empty string if \(n \le 0\).
lazyByteString :: Int -> end -> Parser end L.ByteString
lazyByteString n e =
  Parser $ \core pol ->
    if n <= 0
      then Fin core (Yes L.empty)
      else runParser (unsafeLazyByteString n e) core pol

{-# INLINE unsafeLazyByteString #-}
-- | Consume \(n\) bytes into a lazy 'L.ByteString'.
--
--   \(n\) __must__ be non-negative.
unsafeLazyByteString :: Int -> end -> Parser end L.ByteString
unsafeLazyByteString n e =
  Parser $ \(# i, o, bs, lbs, more, roll #) pol ->
    let o' = o + n

        n' = o' - B.length bs

    in if n' <= 0
         then let !raw = sliceCopy Copy o n bs

                  !res = L.chunk raw L.empty

              in Fin (# i, o', bs, lbs, more, roll #) (Yes res)

         else let !(# acc #) | o == B.length bs = (# id #)
                             | otherwise        = let !r = dropCopy Copy o bs
                                                  in (# L.Chunk r #)

              in take_ Copy e id more pol n' acc i bs lbs roll

take_
  :: Copying
  -> e
  -> (L.ByteString -> a)
  -> More
  -> Policy

  -> Int
  -> (L.ByteString -> L.ByteString)
  -> TotalOffset
  -> B.ByteString
  -> L.ByteString
  -> Rollback
  -> Dec e a
take_ copy e conv more pol = go
  where
    go !n acc i bs lbs roll =
      advance (No e) i bs lbs more roll pol $ \i' bs' lbs' roll' ->
        let n' = n - B.length bs'

        in if n' <= 0
             then let !raw = takeCopy copy n bs'

                      !r = conv . acc $! L.Chunk raw L.empty

                  in Fin (# i', n, bs', lbs', more, roll' #) (Yes r)

             else go n' (\r -> acc $! L.Chunk bs' r) i' bs' lbs' roll'



{-# INLINE int8 #-}
-- | Consume 1 byte into an 'Int8'.
int8 :: end -> Parser end Int8
int8 e =
  Parser $ \(# i, o, bs, lbs, more, roll #) pol ->
    if o < B.length bs
      then let !w = fromIntegral $ B.unsafeIndex bs o

               !o' = o + 1

           in Fin (# i, o', bs, lbs, more, roll #) (Yes w)

      else int8_ e more pol i bs lbs roll

int8_
  :: e
  -> More
  -> Policy

  -> TotalOffset
  -> B.ByteString
  -> L.ByteString
  -> Rollback
  -> Dec e Int8
int8_ e more pol = go
  where
    go i bs lbs roll =
      advance (No e) i bs lbs more roll pol $ \i' bs' lbs' roll' ->
        if B.length bs' > 0
          then let !w = fromIntegral $ B.unsafeIndex bs' 0

               in Fin (# i', 1, bs', lbs', more, roll' #) (Yes w)

          else go i' bs' lbs' roll'



{-# INLINE word8 #-}
-- | Consume 1 byte into a 'Word8'.
word8 :: end -> Parser end Word8
word8 e =
  Parser $ \(# i, o, bs, lbs, more, roll #) pol ->
    if o < B.length bs
      then let !w = B.unsafeIndex bs o

               !o' = o + 1

           in Fin (# i, o', bs, lbs, more, roll #) (Yes w)

      else word8_ e more pol i bs lbs roll

word8_
  :: e
  -> More
  -> Policy

  -> TotalOffset
  -> B.ByteString
  -> L.ByteString
  -> Rollback
  -> Dec e Word8
word8_ e more pol = go
  where
    go i bs lbs roll =
      advance (No e) i bs lbs more roll pol $ \i' bs' lbs' roll' ->
        if B.length bs' > 0
          then let !w = B.unsafeIndex bs' 0

               in Fin (# i', 1, bs', lbs', more, roll' #) (Yes w)

          else go i' bs' lbs' roll'



{-# INLINE skip1 #-}
-- | Skip ahead 1 byte.
skip1 :: end -> Parser end ()
skip1 e =
  Parser $ \(# i, o, bs, lbs, more, roll #) pol ->
    if o < B.length bs
      then let !o' = o + 1
           in Fin (# i, o', bs, lbs, more, roll #) (Yes ())

      else skip1_ e more pol i bs lbs roll

skip1_
  :: e
  -> More
  -> Policy

  -> TotalOffset
  -> B.ByteString
  -> L.ByteString
  -> Rollback
  -> Dec e ()
skip1_ e more pol = go
  where
    go i bs lbs roll =
      advance (No e) i bs lbs more roll pol $ \i' bs' lbs' roll' ->
        if B.length bs' > 0
          then Fin (# i', 1, bs', lbs', more, roll' #) (Yes ())
          else go i' bs' lbs' roll'



{-# INLINE skipEndOr1 #-}
-- | Skip ahead 1 byte or do nothing.
skipEndOr1 :: Parser never ()
skipEndOr1 =
  Parser $ \(# i, o, bs, lbs, more, roll #) pol ->
    if o < B.length bs
      then let !o' = o + 1
           in Fin (# i, o', bs, lbs, more, roll #) (Yes ())

      else skipEndOr1_ more pol i bs lbs roll

skipEndOr1_
  :: More
  -> Policy

  -> TotalOffset
  -> B.ByteString
  -> L.ByteString
  -> Rollback
  -> Dec e ()
skipEndOr1_ more pol = go
  where
    go i bs lbs roll =
      advance (Yes ()) i bs lbs more roll pol $ \i' bs' lbs' roll' ->
        if B.length bs' > 0
          then Fin (# i', 1, bs', lbs', more, roll' #) (Yes ())
          else go i' bs' lbs' roll'



{-# INLINE unsafeRead #-}
-- | Consume \(n\) bytes into a strict 'B.ByteString' and use that to produce a result.
--
--   \(n\) __must__ be non-negative.
--
--   The returned string, if it points to a continuous segment of a single input chunk,
--   is not a copy and is not trimmed properly.
--
--   Only the unboxed tuple is evaluated to WHNF before returning;
--   the t'Res' inside it is not touched.
unsafeRead :: Int -> (B.ByteString -> (# Res e a #)) -> e -> Parser e a
unsafeRead n conv = \e ->
  Parser $ \(# i, o, bs, lbs, more, roll #) pol ->
    let o' = o + n

        n' = o' - B.length bs

    in if n' <= 0
         then let !(# r #) = conv $ B.unsafeDrop o bs
              in Fin (# i, o', bs, lbs, more, roll #) r

         else let !(# acc #) | o == B.length bs = (# id #)
                             | otherwise        = let !r = B.unsafeDrop o bs
                                                  in (# L.Chunk r #)

              in read_ e (\b -> conv $ toStrictLen n b) more pol n' acc i bs lbs roll


read_
  :: e
  -> (L.ByteString -> (# Res e a #))
  -> More
  -> Policy

  -> Int
  -> (L.ByteString -> L.ByteString)
  -> TotalOffset
  -> B.ByteString
  -> L.ByteString
  -> Rollback
  -> Dec e a
read_ e conv more pol = go
  where
    go !n acc i bs lbs roll =
      advance (No e) i bs lbs more roll pol $ \i' bs' lbs' roll' ->
        let n' = n - B.length bs'

        in if n' <= 0
             then let !(# r #) = conv (acc $ L.Chunk (B.unsafeTake n bs') L.Empty)
                  in Fin (# i', n, bs', lbs', more, roll' #) r

             else go n' (\r -> acc $! L.Chunk bs' r) i' bs' lbs' roll'



{-# INLINE byteStringNul #-}
-- | Consume input into a strict 'B.ByteString' until a NUL byte (inclusive) is reached.
--   The returned string does not contain the NUL byte.
byteStringNul :: end -> Parser end B.ByteString
byteStringNul e =
  Parser $ \(# i, o, bs, lbs, more, roll #) pol ->
    let long acc = takeNul_ Original e
                     (\r -> toStrictLen (fromIntegral $ L.length r) r) more pol
                     acc i bs lbs roll

        bs' = B.unsafeDrop o bs

    in if o == B.length bs
         then long id
         else
           case B.elemIndex 0 bs' of
             Just x ->
               let x' = x + 1

                   !o' = o + x'

                   !r = sliceCopy Copy o x bs

               in Fin (# i, o', bs, lbs, more, roll #) (Yes r)

             Nothing -> long (L.Chunk bs')

{-# INLINE shortByteStringNul #-}
-- | Consume input into a 'ShortByteString' until a NUL byte (inclusive) is reached.
--   The returned string does not contain the NUL byte.
shortByteStringNul :: end -> Parser end ShortByteString
shortByteStringNul e =
  Parser $ \(# i, o, bs, lbs, more, roll #) pol ->
    let long acc = takeNul_ Original e
                     (\r -> toShortLen (fromIntegral $ L.length r) r) more pol
                     acc i bs lbs roll

        bs' = B.unsafeDrop o bs

    in if o == B.length bs
         then long id
         else
           case B.elemIndex 0 bs' of
             Just x ->
               let x' = x + 1

                   !o' = o + x'

                   !r = Short.toShort $ B.unsafeTake x bs'

               in Fin (# i, o', bs, lbs, more, roll #) (Yes r)

             Nothing -> long (L.Chunk bs')

{-# INLINE lazyByteStringNul #-}
-- | Consume input into a lazy 'L.ByteString' until a NUL byte (inclusive) is reached.
--   The returned string does not contain the NUL byte.
lazyByteStringNul :: end -> Parser end L.ByteString
lazyByteStringNul e =
  Parser $ \(# i, o, bs, lbs, more, roll #) pol ->
    let long acc = takeNul_ Copy e id more pol acc i bs lbs roll

        bs' = B.unsafeDrop o bs

    in if o == B.length bs
         then long id
         else
           case B.elemIndex 0 bs' of
             Just x ->
               let x' = x + 1

                   !o' = o + x'

                   !raw = sliceCopy Copy o x bs

                   !r = L.Chunk raw L.Empty

               in Fin (# i, o', bs, lbs, more, roll #) (Yes r)

             Nothing -> long (L.Chunk bs')

takeNul_
  :: Copying
  -> e
  -> (L.ByteString -> a)
  -> More
  -> Policy

  -> (L.ByteString -> L.ByteString)
  -> TotalOffset
  -> B.ByteString
  -> L.ByteString
  -> Rollback
  -> Dec e a
takeNul_ copy e conv more pol = go
  where
    go acc i bs lbs roll =
      advance (No e) i bs lbs more roll pol $ \i' bs' lbs' roll' ->
        case B.elemIndex 0 bs' of
          Just x ->
            let !x' = x + 1

                !r = conv . acc $ L.Chunk (takeCopy copy x bs') L.Empty

            in Fin (# i', x', bs', lbs', more, roll' #) (Yes r)

          Nothing -> go (\r -> acc $! L.Chunk bs' r) i' bs' lbs' roll'



{-# INLINE byteStringUntil #-}
-- | Consume input into a strict 'B.ByteString' until the predicate holds (exclusive).
byteStringUntil :: (Word8 -> Bool) -> end -> Parser end B.ByteString
byteStringUntil f e =
  Parser $ \(# i, o, bs, lbs, more, roll #) pol ->
    let long acc = takeUntil_ f Original e
                     (\r -> toStrictLen (fromIntegral $ L.length r) r) more pol
                     acc i bs lbs roll

        bs' = B.unsafeDrop o bs

    in if o == B.length bs
         then long id
         else
           case B.findIndex f bs' of
             Just x ->
               let !o' = o + x

                   !r = sliceCopy Copy o x bs

               in Fin (# i, o', bs, lbs, more, roll #) (Yes r)

             Nothing -> long (L.Chunk bs')

{-# INLINE shortByteStringUntil #-}
-- | Consume input into a 'ShortByteString' until the predicate holds (exclusive).
shortByteStringUntil :: (Word8 -> Bool) -> end -> Parser end ShortByteString
shortByteStringUntil f e =
  Parser $ \(# i, o, bs, lbs, more, roll #) pol ->
    let long acc = takeUntil_ f Original e
                     (\r -> toShortLen (fromIntegral $ L.length r) r) more pol
                     acc i bs lbs roll

        bs' = B.unsafeDrop o bs

    in if o == B.length bs
         then long id
         else
           case B.findIndex f bs' of
             Just x ->
               let !o' = o + x

                   !r = Short.toShort $ B.unsafeTake x bs'

               in Fin (# i, o', bs, lbs, more, roll #) (Yes r)

             Nothing -> long (L.Chunk bs')

{-# INLINE lazyByteStringUntil #-}
-- | Consume input into a lazy 'L.ByteString' until the predicate holds (exclusive).
lazyByteStringUntil :: (Word8 -> Bool) -> end -> Parser end L.ByteString
lazyByteStringUntil f e =
  Parser $ \(# i, o, bs, lbs, more, roll #) pol ->
    let long acc = takeUntil_ f Copy e id more pol acc i bs lbs roll

        bs' = B.unsafeDrop o bs

    in if o == B.length bs
         then long id
         else
           case B.findIndex f bs' of
             Just x ->
               let !o' = o + x

                   !raw = sliceCopy Copy o x bs

                   !r = L.Chunk raw L.Empty

               in Fin (# i, o', bs, lbs, more, roll #) (Yes r)

             Nothing -> long (L.Chunk bs')

takeUntil_
  :: (Word8 -> Bool)
  -> Copying
  -> e
  -> (L.ByteString -> a)
  -> More
  -> Policy

  -> (L.ByteString -> L.ByteString)
  -> TotalOffset
  -> B.ByteString
  -> L.ByteString
  -> Rollback
  -> Dec e a
takeUntil_ f copy e conv more pol = go
  where
    go acc i bs lbs roll =
      advance (No e) i bs lbs more roll pol $ \i' bs' lbs' roll' ->
        case B.findIndex f bs' of
          Just x ->
            let !r = conv . acc $ L.fromStrict (takeCopy copy x bs')

            in Fin (# i', x, bs', lbs', more, roll' #) (Yes r)

          Nothing -> go (\r -> acc $! L.Chunk bs' r) i' bs' lbs' roll'



type Carry = (# TotalOffset, L.ByteString -> L.ByteString #)

carry1 :: B.ByteString -> Carry -> Carry
carry1 bs (# n, acc #) =
  let !n' = n + fromIntegral (B.length bs)
  in (# n', \r -> acc $! L.Chunk bs r #)

carryN :: L.ByteString -> Carry -> Carry
carryN bss c =
  case bss of
    L.Empty      -> c
    L.Chunk b bs ->
      let !c' = carry1 b c
      in carryN bs c'

type CarryRoll = (# TotalOffset, L.ByteString -> L.ByteString, Rollback #)

carryRoll :: B.ByteString -> CarryRoll -> CarryRoll
carryRoll b (# n, acc, roll #) =
  let !n' = n + fromIntegral (B.length b)
  in (# n', \r -> acc $! L.Chunk b r, Rollback roll b #)



{-# INLINE lazyByteStringRest #-}
-- | Consume all remaining input into a lazy 'L.ByteString'.
lazyByteStringRest :: Parser never L.ByteString
lazyByteStringRest =
  Parser $ \(# i, o, bs, lbs, more, roll #) pol ->
    case more of
      End  ->
        let !i' = i + fromIntegral (B.length bs) + L.length lbs

            !r = L.chunk (dropCopy Copy o bs) lbs

        in Fin (# i', 0, B.empty, L.Empty, End, roll #) (Yes r)

      More ->
        let !i' = i + fromIntegral (B.length bs)

            !(# acc0 #) | o == B.length bs = (# id #)
                        | otherwise        = let !r = dropCopy Copy o bs
                                             in (# L.chunk r #)

        in lazyByteStringRest_ acc0 i' lbs roll pol

lazyByteStringRest_
  :: (L.ByteString -> L.ByteString)
  -> TotalOffset -> L.ByteString -> Rollback -> Policy -> Dec e L.ByteString
lazyByteStringRest_ acc0 i' lbs roll pol =
  case pol of
    Drop ->
      let !c = carryN lbs (# i', acc0 #)
      in flush c
      where
        flush carry@(# n, acc #) =
          Re $ \resupply ->
            case resupply of
              Supply bsR ->
                let !carry' = carry1 bsR carry
                in flush carry'

              EndOfInput    ->
                let !r = acc L.empty
                in Fin (# n, 0, B.empty, L.Empty, End, roll #) (Yes r)

    Keep ->
      let !(# i'', acc1 #) = carryN lbs (# i', acc0 #)
      in flush (# i'', acc1, roll #)
      where
        flush carry@(# n, acc, roll' #) =
          Re $ \resupply ->
            case resupply of
              Supply bsR ->
                let !carry' = carryRoll bsR carry
                in flush carry'

              EndOfInput    ->
                let !r = acc L.empty
                in Fin (# n, 0, B.empty, L.Empty, End, roll' #) (Yes r)



{-# INLINE skip #-}
-- | Skip ahead \(n\) bytes.
--
--   Does nothing if \(n \le 0\).
skip :: Int64 -> end -> Parser end ()
skip n e =
  Parser $ \core pol ->
    if n <= 0
      then Fin core (Yes ())
      else runParser (unsafeSkip n e) core pol

{-# INLINE unsafeSkip #-}
-- | Skip ahead \(n\) bytes.
--
--   \(n\) __must__ be non-negative.
unsafeSkip :: Int64 -> end -> Parser end ()
unsafeSkip n e =
  Parser $ \(# i, o, bs, lbs, more, roll #) pol ->
    let o' = fromIntegral o + n
        n' = o' - fromIntegral (B.length bs)

    in if n' <= 0
         then let !m = fromIntegral o'
              in Fin (# i, m, bs, lbs, more, roll #) (Yes ())
         else skip_ e more pol n' i bs lbs roll

skip_
  :: e
  -> More
  -> Policy

  -> Int64
  -> TotalOffset
  -> B.ByteString
  -> L.ByteString
  -> Rollback
  -> Dec e ()
skip_ e more pol = go
  where
    go !n i bs lbs roll =
      advance (No e) i bs lbs more roll pol $ \i' bs' lbs' roll' ->
        let n' = n - fromIntegral (B.length bs')

        in if n' <= 0
             then let !m = fromIntegral n
                  in Fin (# i', m, bs', lbs', more, roll' #) (Yes ())

             else go n' i' bs' lbs' roll'



{-# INLINE skipEndOr #-}
-- | Skip ahead \(n\) or fewer bytes.
--
--   Does nothing if \(n \le 0\).
skipEndOr :: Int64 -> Parser never ()
skipEndOr n =
  Parser $ \core pol ->
    if n <= 0
      then Fin core (Yes ())
      else runParser (unsafeSkipEndOr n) core pol

{-# INLINE unsafeSkipEndOr #-}
-- | Skip ahead \(n\) or fewer bytes.
--
--   \(n\) __must__ be non-negative.
unsafeSkipEndOr :: Int64 -> Parser never ()
unsafeSkipEndOr n =
  Parser $ \(# i, o, bs, lbs, more, roll #) pol ->
    let o' = fromIntegral o + n
        n' = o' - fromIntegral (B.length bs)

    in if n' <= 0
         then let !m = fromIntegral o'
              in Fin (# i, m, bs, lbs, more, roll #) (Yes ())

         else skipEndOr_ more pol n' i bs lbs roll

skipEndOr_
  :: More
  -> Policy

  -> Int64
  -> TotalOffset
  -> B.ByteString
  -> L.ByteString
  -> Rollback
  -> Dec e ()
skipEndOr_ more pol = go
  where
    go !n i bs lbs roll =
      advance (Yes ()) i bs lbs more roll pol $ \i' bs' lbs' roll' ->
        let n' = n - fromIntegral (B.length bs')

        in if n' <= 0
             then let !m = fromIntegral n
                  in Fin (# i', m, bs', lbs', more, roll' #) (Yes ())

             else go n' i' bs' lbs' roll'



{-# INLINE skipNul #-}
-- | Skip ahead until a NUL byte is reached (inclusive).
skipNul :: end -> Parser end ()
skipNul e =
  Parser $ \(# i, o, bs, lbs, more, roll #) pol ->
    let long n = skipNul_ e more pol n i bs lbs roll

    in if o == B.length bs
         then long 0
         else
           case B.elemIndex 0 (B.unsafeDrop o bs) of
             Just x ->
               let !o' = o + x + 1
               in Fin (# i, o', bs, lbs, more, roll #) (Yes ())

             Nothing -> long (fromIntegral $ B.length bs - o)

skipNul_
  :: e
  -> More
  -> Policy

  -> Int64
  -> TotalOffset
  -> B.ByteString
  -> L.ByteString
  -> Rollback
  -> Dec e ()
skipNul_ e more pol = go
  where
    go !n i bs lbs roll =
      advance (No e) i bs lbs more roll pol $ \i' bs' lbs' roll' ->
        case B.elemIndex 0 bs' of
          Just x  -> let !o' = x + 1
                     in Fin (# i', o', bs', lbs', more, roll' #) (Yes ())

          Nothing -> go (n + fromIntegral (B.length bs')) i' bs' lbs' roll'



{-# INLINE skipUntil #-}
-- | Skip ahead until the predicate holds (exclusive).
skipUntil :: (Word8 -> Bool) -> end -> Parser end ()
skipUntil f e =
  Parser $ \(# i, o, bs, lbs, more, roll #) pol ->
    let long n = skipUntil_ f e more pol n i bs lbs roll

    in if o == B.length bs
         then long 0
         else
           case B.findIndex f (B.unsafeDrop o bs) of
             Just x ->
               let !o' = o + x
               in Fin (# i, o', bs, lbs, more, roll #) (Yes ())

             Nothing -> long (fromIntegral $ B.length bs - o)

skipUntil_
  :: (Word8 -> Bool)
  -> e
  -> More
  -> Policy

  -> Int64
  -> TotalOffset
  -> B.ByteString
  -> L.ByteString
  -> Rollback
  -> Dec e ()
skipUntil_ f e more pol = go
  where
    go !n i bs lbs roll =
      advance (No e) i bs lbs more roll pol $ \i' bs' lbs' roll' ->
        case B.findIndex f bs' of
          Just x  -> Fin (# i', x, bs', lbs', more, roll' #) (Yes ())
          Nothing -> go (n + fromIntegral (B.length bs')) i' bs' lbs' roll'




{-# INLINE skipUntilEndOr #-}
-- | Skip ahead until either the end is reached or the predicate holds (exclusive).
skipUntilEndOr :: (Word8 -> Bool) -> Parser never ()
skipUntilEndOr f =
  Parser $ \(# i, o, bs, lbs, more, roll #) pol ->
    let long n = skipUntilEndOr_ f more pol n i bs lbs roll

    in if o == B.length bs
         then long 0
         else
           case B.findIndex f (B.unsafeDrop o bs) of
             Just x ->
               let !o' = o + x
               in Fin (# i, o', bs, lbs, more, roll #) (Yes ())

             Nothing -> long (fromIntegral $ B.length bs - o)

skipUntilEndOr_
  :: (Word8 -> Bool)
  -> More
  -> Policy

  -> Int64
  -> TotalOffset
  -> B.ByteString
  -> L.ByteString
  -> Rollback
  -> Dec e ()
skipUntilEndOr_ f more pol = go
  where
    go !n i bs lbs roll =
      advance (Yes ()) i bs lbs more roll pol $ \i' bs' lbs' roll' ->
        case B.findIndex f bs' of
          Just x  -> Fin (# i', x, bs', lbs', more, roll' #) (Yes ())
          Nothing -> go (n + fromIntegral (B.length bs')) i' bs' lbs' roll'
