# Building an executable file

The `droidsfmin` tool is written in Haskell. Executables may be compiled on
and for Linux, Windows and Mac OS X.

You need:

 1. [The Haskell Platform](https://www.haskell.org/platform) which includes a
    Haskell compiler (GHC) and a build environment (Cabal).
 2. [The `xml` package](http://hackage.haskell.org/package/xml).
 3. The `droidsfmin` source files.

Install the prerequisites, `cd` into the `droidsfmin` source directory and run
the following command:

    $ cabal build

You will find your shiny new executable somewhere below the `./dist/build`
directory.

To clean up your workspace use the (surprise, surprise) `cabal clean` command.
You may create a source distribution (i.e., a `*.tar.gz` or `*.zip` file) with
the `cabal sdist` command, for example to move the whole stuff from your Linux
development system to a Windows system for building a Windows executable.

