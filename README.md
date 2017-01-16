# Rationale

[DROID] is a file format identification tool. It relies upon a so-called
signature file which is an XML file based on data from the [PRONOM] technical
registry. The official DROID signature file currently contains entries for
more than 1300 file formats. This is generally a good thing since it means
more than 1300 file formats that may be identified by DROID.

However, [file format identification with DROID can be faster][blog] when
using a signature file that is restricted to identify only those formats that
are actually of interest for the analysis at hand. If, for example, a digital
archive accepts only a small, well-defined set of file formats for
preservation purposes, only those need to be identified exactly in the ingest
process. All other formats will be rejected anyway, so it may be sufficient to
just label them as "unknown file format" and treat them as corrupt data that
has to be revised.

Such a restriction of a signature file based on a list of [PUID]s (denoting
the accepted file formats) can be automated by the DROID Signature File
Minimizer, or `droidsfmin`.

# Download and usage

See the [website][droidsfmin].

# Building an executable file

The `droidsfmin` tool is written in Haskell. Executables may be compiled on
and for Linux, Windows and Mac OS X.

You need:

 1. [The Haskell Platform](https://www.haskell.org/platform) which includes a
    Haskell compiler (GHC) and a build environment (Cabal).
 2. [The `xml` package](http://hackage.haskell.org/package/xml).
 3. The `droidsfmin` source files.

Install the prerequisites, `cd` into the `droidsfmin` source directory and run
the following commands:

    $ cabal sandbox init
    $ cabal build

You will find your shiny new executable somewhere below the `./dist/build`
directory.

To clean up your workspace use the (surprise, surprise) `cabal clean` command.
You may create a source distribution (i.e., a `*.tar.gz` or `*.zip` file) with
the `cabal sdist` command, for example to move the whole stuff from your Linux
development system to a Windows system for building a Windows executable.

# References

Kudos to the <https://github.com/KOST-CECO/KaD_SignatureFile> project where
the idea this tool is based on was already manually implemented.

[DROID]: https://www.nationalarchives.gov.uk/information-management/manage-information/preserving-digital-records/droid/
[PRONOM]: https://www.nationalarchives.gov.uk/PRONOM
[PUID]: https://www.nationalarchives.gov.uk/aboutapps/pronom/puid.htm
[droidsfmin]: https://martin.hoppenheit.info/code/droidsfmin
[blog]: https://martin.hoppenheit.info/blog/2017/minimizing-the-droid-signature-file/

