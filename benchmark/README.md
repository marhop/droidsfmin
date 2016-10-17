Benchmarking tools that compare the performance of [DROID] when using
different signature files.

# Quick start

 1. Optionally, replace the dummy files in the `./data/` directory with more
    significant test files like the [Govdocs Selected
    corpus](http://openpreservation.org/technology/corpora/govdocs-selected/).

 2. Run the benchmarking tests:

        $ make

 3. Wait. After some time you will find the results in the `./benchmark.csv`
    file.

# Prerequisites

Make, Bash and Perl. DROID and droidsfmin. (I guess only Linux or another Unix
will make you really happy.) Curl and xmlstarlet, but only if you want the
latest DROID signature file to be downloaded automatically.

# How it works

The Makefile looks for a signature file at `./sig/orig.xml` (you can put your
own there) and, if none is found, it downloads the latest version from the
[National Archives](https://www.nationalarchives.gov.uk). Then it creates
several new signature files with the droidsfmin tool, each containing a subset
of the original signature file's entries. Finally, it runs DROID several times
with the different signature files on the test files in the `./data/`
directory and outputs the benchmarking results to a CSV file.

The `./util/benchmark.pl` script does most of the benchmarking work and may
also be useful in its own right.

# Configuration

The following parameters can be configured in the Makefile that runs the
tests:

  * number of times DROID is run with each signature file (variable
    `iterations`)
  * directory that contains the signature files (variable `sigdir`)
  * name of the original signature file (variable `sigfile_orig`)
  * list of names of the filtered signature files (variable `sigfiles`)
  * directory that contains the test data (variable `datadir`)
  * name of the CSV file that will contain the benchmarking results (variable
    `result`)

To create additional signature files, in the Makefile:

 1. Add the file name to the `sigfiles` variable.
 2. Add a Makefile recipe that describes how the signature file can be created
    based on a search pattern. See the existing recipes for examples.

If that's still not enough to configure, modify the scripts in the `./util`
directory.

[DROID]: https://www.nationalarchives.gov.uk/information-management/manage-information/preserving-digital-records/droid/

