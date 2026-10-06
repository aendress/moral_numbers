#!/Users/endress/bin/perl
use strict;
use warnings;

# Check if a filename argument was provided
if (@ARGV != 1) {
    die "Usage: perl check_answerorder.pl <input_filename>\n";
}

my ($filename) = @ARGV;
my $found_mismatch_flag = 0;

# Open the input file
open(my $fh, '<', $filename) or die "Cannot open file '$filename': $!\n";

# Iterate over each line in the file
while (my $line = <$fh>) {
    chomp $line; # Remove trailing newline

    # The square brackets MUST be escaped with a backslash (\)
    if ($line =~ /"AnswerOrder":\s*\["1","2","3","4","5","6"\]/) {
        # This line matches the default sequence, regardless of minor spacing differences. Skip it.
        next;
    }

    # If the line contains "AnswerOrder" but did NOT match the 'next' condition above,
    # it must contain a different, non-sequential order. Print it.
    if ($line =~ /"AnswerOrder"/) {
        print "$line\n\n\n";
        $found_mismatch_flag = 1;
    }
}

close($fh);

if (!$found_mismatch_flag) {
    print "All questions with AnswerOrder match the default sequence, or no AnswerOrder was found.\n";
}
