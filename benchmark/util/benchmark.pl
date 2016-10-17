#!/usr/bin/env perl

use strict;
use warnings;
use 5.010;
use Getopt::Long;
use Benchmark;

my $datadir    = '.';
my $iterations = 1;
my $cmd        = 'droid';
my $help       = 0;
GetOptions(
    'data|d=s'       => \$datadir,
    'iterations|i=i' => \$iterations,
    'cmd|c=s'        => \$cmd,
    'help|h'         => \$help,
);
my @sigfiles = @ARGV;

if ($help or not @sigfiles) {
say <<EOF;
Usage: perl $0 -[dich] sigfile ...

This script is used to compare the performance of DROID when using different
signature files. For each given signature file DROID will be run for a number
of iterations specified by the '-i' option, each time analyzing the same test
data set specified by the '-d' option. The benchmark results (i.e., how long
the analysis took with each signature file) will be output as a CSV table.

Arguments:

    sigfile ...         List of DROID signature files.

Options:

    -d --data DIR       Directory containing test data (default '$datadir').
    -i --iterations NUM Number of test iterations (default '$iterations').
    -c --cmd PATH       Path to the DROID executable/starter script (default
                        '$cmd').
    -h --help           Display this message and exit.

Example:

    \$ perl $0 -i 10 -d ~/data/ DROID_SignatureFile_V86.xml pdf.xml tiff.xml
EOF
exit;
}

my %calls = ();
for my $f (@sigfiles) {
    $calls{"$f"} =
        qq(system '$cmd -R -Nr "$datadir" -Ns "$f" > /dev/null 2>&1');
}

say 'signature file,test data dir,number of iterations,average CPU seconds';

for my $k (keys %calls) {
    my $result = timeit($iterations, $calls{$k});
    my $avg = sprintf("%.2f", $result->cpu_a / $iterations);
    say qq("$k","$datadir",$iterations,$avg);
}

