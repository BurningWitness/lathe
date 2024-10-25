# lathe [![Hackage](http://img.shields.io/hackage/v/lathe.svg)](https://hackage.haskell.org/package/lathe)


Pure incremental byte parser.

Spiritually a successor to `binary`'s
[`Get`](https://hackage.haskell.org/package/binary-0.8.9.2/docs/Data-Binary-Get.html)
monad, with several changes in favor of power:

- Does not compress input internally during processing.

  Reading a `LazyByteString` always reuses input chunks, regardless of parser state.

- Error type is polymorphic.

  Helper parsers get to fail with errors that can be pattern matched on, and
  higher-level libraries are no longer restricted to only `String`s.

- Properly supports incremental output.

  Higher-level libraries can properly support streaming data alongside their
  regular decoding functions.
