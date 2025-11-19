# Haskell ODT Writer

## Overview

Haskell ODT Writer provides an interface in Haskell for reading and writing Open Document Format `.odt` files.

The program is set up to enable the user to compose their own `.odt` files using Haskell syntax. 

Haskell ODT Writer is in beta.

## Installing and building

So far I have only tested this process on Linux (Ubuntu), not Windows or MacOS. You can install with [Stack](https://docs.haskellstack.org/en/stable/). I installed Haskell and Stack with [GHCup](https://www.haskell.org/ghcup/). 

## Build and test environment

Haskell ODT Writer has been built and tested on GHC 9.4.7 using Stack 2.11.1.

### 1. Clone and `cd` into the repo

```
$ git clone https://github.com/rsdc2/haskell-odt
$ cd haskell-odt
```

### 2. Build

```
$ stack build
```

### 3. Run the tests

```
$ stack test
```

### 4. Run the Main app

```
$ stack run
```

## Composing OpenDocument Text documents

An OpenDocument Text file is a Zip archive comprising a directory structure of `.xml` files. From a writer's perspective the two most important files are:

- `content.xml` which carries the text content of the file
- `styles.xml` which carries the style information

These are the two files that `haskell-odt-writer` allows the user to write to.

In the simplest case, you want simply to write some text content into a file that can be read e.g. by LibreOffice / OpenOffice Writer. In [`Main.hs`](app/Main.hs) you will find a function with the following signature:

```haskell
content :: Writer ODT ()
```

This functions provides the content of your document. There are two primary functions to be aware of:

- `writeTextSpan`
- `writeNewPara`
- `writePara` 

#### `writeTextSpan :: TextStyle -> T.Text -> Writer ODT ()`

This function takes a `TextStyle` and a section of text, applies this style to the text, and appends it to the document. For example, the following will write out "Hello world." to the document without any formatting:

```haskell

content :: Writer ODT ()
content = do
    writeTextSpan normal "Hello world."
```

If you want to apply some formatting, you can use a `TextStyle` other than `normal`, e.g.:

```haskell
content :: Writer ODT ()
content = do
    writeTextSpan italic "Hello world."
```

This will write "_Hello world._" to the document, this time in italics.

To append further text to the document, simply add further lines to the function, e.g.:

```haskell
content :: Writer ODT ()
content = do
    writeTextSpan italic "Hello world. "
    writeTextSpan bold "This is bold text."
```

In this case, the document will comprise a single line: "*Hello world.* **This is bold text.**"

#### `writeNewPara :: Writer ODT ()`

If you want to start a new paragraph, use the `writeNewPara` function, which takes no arguments. You can then follow this with further `TextSpan`s:

```haskell
content :: Writer ODT ()
content = do
    writeTextSpan italic "Hello world. "
    writeTextSpan bold "This is bold text."
    writeNewPara
    writeTextSpan normal "This is normal text."
```

The text "This is normal text." will appear in a new paragraph.


#### `writePara :: ParaStyle -> T.Text -> Writer ODT ()`

To write a new section of text as a new paragraph with a _paragraph style_ (as opposed to a _text style_), use the `writePara` function, e.g.:

```haskell

```haskell
content :: Writer ODT ()
content = do
    writeTextSpan italic "Hello world. "
    writePara normalPara "This text is in normal paragraph style."
    writePara italicPara "This text is in italic paragraph style."
```


## Saving the document to disk

There are two main functions which can write a new `.odt` file to disk, use the function `writeNewODT`.

#### `writeNewODT :: Folderpath -> Filename -> Writer ODT () -> IO ()`

Writes a new document to disk with all styling applied as _direct_ formatting. For example, applying bold formatting using this function would be equivalent to pressing the `B` button in LibreOffice Writer, e.g.:

```haskell
writeNewODT "./examples/output" "SimpleExample.odt" content
```

This will save the content we have described in `content` to a file `SimpleExample.odt` in the `./examples/output` folder.

<!-- - `writeNewODTWithStyles`: Writes a new document to disk with all styling applied via _styles_. Applying e.g. bold formatting using this function would be equivalent to using the `Strong Emphasis` style in LibreOffice Writer.  -->

<!-- ## Key concepts -->

## Acknowledgements 

The inspiration for modelling an `.odt` file as a monoid, and for using the `Writer` monad for document composition, came from the [HaTeX](https://gitlab.com/daniel-casanueva/haskell/HaTeX) project, which provides an interface in Haskell for composing and otherwise analysing LaTeX documents. 

## Further information

- [OpenDocument (Wikipedia)](https://en.wikipedia.org/wiki/OpenDocument)

- [OpenDocument technical specification (Wikipedia)](https://en.wikipedia.org/wiki/OpenDocument_technical_specification)

- [OpenDocument standard (OASIS Open)](https://www.oasis-open.org/2021/06/16/opendocument-v1-3-oasis-standard-published/)

## Dependencies and licenses

- [bytestring](https://hackage.haskell.org/package/bytestring-0.12.1.0/docs/Data-ByteString.html): BSD-style
- [containers](https://hackage.haskell.org/package/containers): [BSD-3](https://hackage.haskell.org/package/containers-0.7/src/LICENSE)
- [directory](https://hackage.haskell.org/package/directory): [BSD-3](https://hackage.haskell.org/package/directory-1.3.9.0/src/LICENSE)
- [extra](https://hackage.haskell.org/package/extra): [BSD-3](https://hackage.haskell.org/package/extra-1.8/src/LICENSE)
- [mtl](https://hackage.haskell.org/package/mtl): [BSD-3](https://hackage.haskell.org/package/mtl-2.3.1/src/LICENSE)
- [text](https://hackage.haskell.org/package/text-2.1.2/docs/Data-Text.html): BSD-style
- [time](https://hackage.haskell.org/package/time): [BSD-2](https://hackage.haskell.org/package/time-1.14/src/LICENSE)
- [xml](https://hackage.haskell.org/package/xml): [BSD-3](https://hackage.haskell.org/package/xml-1.3.14/src/LICENSE)
- [xml-conduit](https://hackage.haskell.org/package/xml-conduit): [MIT](https://hackage.haskell.org/package/xml-conduit-1.10.0.0/src/LICENSE)
- [zip-archive](https://hackage.haskell.org/package/zip-archive-0.4.3.2/docs/Codec-Archive-Zip.html): BSD-3
- [HUnit](https://hackage.haskell.org/package/HUnit): [BSD-3](https://hackage.haskell.org/package/HUnit-1.6.2.0/src/LICENSE)