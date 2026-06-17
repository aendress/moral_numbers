#!/Users/endress/bin/perl
use strict;
use warnings;

# Check if a filename argument was provided
if (@ARGV != 1) {
    die "Usage: perl check_recodes.pl <input_filename>\n";
}

my ($filename) = @ARGV;
my $expected_recode_values = q({"1":"1","2":"2","3":"3","4":"4","5":"5","6":"6"});
my $found_match_flag = 0;

# Open the input file
open(my $fh, '<', $filename) or die "Cannot open file '$filename': $!\n";

# Iterate over each line in the file
while (my $line = <$fh>) {
    chomp $line; # Remove trailing newline

    # Use a regex to find the RecodeValues snippet within the line
    # We capture the value part to compare it against our expected string
    if ($line =~ /"RecodeValues":($expected_recode_values)/) {
        # This line matches the *default* values you want to ignore.
        next; # Skip this line
    }

    # If the line contains "RecodeValues" but did NOT match the 'next' condition above,
    # it must contain different, customized values. Print it.
    if ($line =~ /"RecodeValues"/) {
        print "$line\n";
        $found_match_flag = 1;
    }
}

close($fh);

if (!$found_match_flag) {
    print "All questions with RecodeValues match the default sequence $expected_recode_values, or no RecodeValues were found.\n";
}


