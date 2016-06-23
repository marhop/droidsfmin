# Rationale

[DROID] is a file format identification tool. It relies upon a so-called
signature file which is an XML file based on data from the [PRONOM] technical
registry. The official DROID signature file currently contains entries for
more than 1300 file formats. This is generally a good thing since it means
more than 1300 file formats that may be identified by DROID.

However, file format identification with DROID can be faster (TODO citation
needed) when using a signature file that is restricted to identify only those
formats that are actually of interest for the analysis at hand. If, for
example, a digital archive accepts only a small, well-defined set of file
formats for preservation purposes, only those need to be identified exactly in
the ingest process. All other formats will be rejected anyway, so it may be
sufficient to just label them as "unknown file format" and treat them as
corrupt data that has to be revised.

Such a restriction of a signature file based on a list of [PUID]s (denoting
the accepted file formats) can be automated by the `droidsfmin` tool.

[DROID]: https://www.nationalarchives.gov.uk/information-management/manage-information/preserving-digital-records/droid/
[PRONOM]: https://www.nationalarchives.gov.uk/PRONOM
[PUID]: https://www.nationalarchives.gov.uk/aboutapps/pronom/puid.htm

# Usage

The `droidsfmin` tool is a command line tool. It is a standalone executable
(compiling see below, TODO offer public download of executables) meaning it
needs no installation but may be run directly. Put it into some directory,
`cd` there (or put it in your `$PATH`) and run the following command to get a
nice and helpful message:

    $ droidsfmin -h

    The DROID Signature File Minimizer - filter a signature file based on
    a list of PUIDs and keep only entries for those file formats that you
    really need.

    Usage: droidsfmin [options] [signature-file]

    -h       --help                  show help message
    -p PUID  --puid=PUID             include file format with this PUID in the
                                     output
    -P FILE  --puids-from-file=FILE  like -p, but read list of PUIDs from file
                                     (one PUID per line)
    -o FILE  --output=FILE           output file

Suppose you have a signature file `DROID_SignatureFile_V84.xml`. The following
command will create a new signature file that contains entries only for the
formats `x-fmt/111` (plain text) and `fmt/95` (PDF/A-1a):

    $ droidsfmin -p x-fmt/111 -p fmt/95 DROID_SignatureFile_V84.xml

The list of PUIDs may also be read from a file (one PUID per line). Suppose
you have a file `puids.txt` with the following content:

    fmt/95
    x-fmt/111

The following command will create the expected new signature file:

    $ droidsfmin -P puids.txt DROID_SignatureFile_V84.xml

The `-p` and `-P` options may also be combined:

    $ droidsfmin -P puids.txt -p fmt/354 DROID_SignatureFile_V84.xml

If you don't specify input/output files then `droidsfmin` will read from STDIN
and write to STDOUT, just like any decent command line tool would.

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

# References

Kudos to the <https://github.com/KOST-CECO/KaD_SignatureFile> project where
the idea this tool is based on was already manually implemented.

