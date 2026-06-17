#!/Users/endress/bin/perl

use strict;
use warnings;

use utf8;

use List::Util 'shuffle';
#use List::MoreUtil 'apply';

use XML::LibXML;

use Text::CSV_XS qw (csv);


$\ = "\n";

############################
# ~    READ ARGUMENTS    ~ #
############################

my ($scenario_file, $condition_file, $design_matrix_file, $scenario_filter, $block_prefix, $qualtrics_file);

if (@ARGV > 4){
  $design_matrix_file = $ARGV[4];
} else {
  $design_matrix_file = "";
}

if (@ARGV > 3){
  $block_prefix = $ARGV[3];
} else {
  $block_prefix = "Scenarios";
}


if (@ARGV > 2){
  $scenario_filter = $ARGV[2];
} else {
  $scenario_filter = ".";
}


if (@ARGV > 1){
  $condition_file = $ARGV[1];
} else {
  $condition_file = 'number_conds.csv';
}

if (@ARGV > 0){
 $scenario_file = $ARGV[0];
} elsif (@ARGV < 3){
 $scenario_file = "scenarios.xml";
}


$qualtrics_file = $scenario_file;
$qualtrics_file =~ s/\..{3}$/\.qualtrics.txt/;

#######################
# ~    READ FILES   ~ #
#######################

# Read in the nummber conditions
# This is a reference to an array of hash references
my $number_conds = csv(
    in      => $condition_file,
    sep     => ',',
    headers => "auto"
);  
#map {print $_->{'n_victims'} } @$number_conds;


# Read in the scenarios
#my $dom = XML::LibXML->load_xml(string => $text);
#my $dom = XML::LibXML->load_xml(location => $scenario_file);
# For UTF-8 support
open my $fh_xml, '<', $scenario_file;
binmode $fh_xml; # drop all PerlIO layers possibly created by a use open pragma
my $dom = XML::LibXML->load_xml(IO => $fh_xml);


#my $n_scenarios = scalar @{$dom->findnodes('/experiment/scenario')};
my $n_scenarios = 0;
foreach my $scenario ($dom->findnodes('/experiment/scenario')) {
  
  if($scenario->findvalue('./label') =~ /$scenario_filter/){
    $n_scenarios += 1;
  }
}



# Generate a randomized latin square
# This is an array of arrays and might be loaded from a file in the future
my @design_matrix = random_ls ($n_scenarios);

open (OUT, ">$qualtrics_file")
  or die "Cannot open file $qualtrics_file: $!";


##############################
# ~    DO VERIFICATIONS    ~ #
##############################

warn "Number of scenarios (", $n_scenarios, ") does not match number of conditions (", scalar @$number_conds, ")."
  unless ($n_scenarios == scalar @$number_conds);

#########################
# ~    PRINT STUFF    ~ #
#########################

# print_demographics ();
print OUT "[[AdvancedFormat]]\n";

# Print blocks
# Each block is a row in the design matrix  
foreach my $block (0..$#design_matrix){
  # Print header for block
  print OUT "[[Block:" . $block_prefix . "." . $block . "]]\n";

  my $n_scenario = 0;
  foreach my $scenario ($dom->findnodes('/experiment/scenario')) {

    if($scenario->findvalue('./label') =~ /$scenario_filter/){
      print_scenario ($scenario, $number_conds->[$design_matrix[$block]->[$n_scenario]]);

      $n_scenario++;
    }
  }

  print OUT "[[PageBreak]]\n"
    if ($block < $#design_matrix);
    
}

######################
# ~    CLEAN UP    ~ #
######################

close (OUT);

##############################
# ~    HELPER FUNCTIONS    ~ #
##############################

sub print_scenario
{
  my ($scenario, $replacement_hash) = @_;

  my $label = get_question_id (trim($scenario->findvalue('./label')),
			      $replacement_hash);

  my @out_lines = ();
  
  # The scenario text, the severity question and the acceptability questions are now separate

  # 1. Scenario text - Start
  push (@out_lines,
	("[[Question:Text]]",
	 "[[ID:" . $label . ".text]]", # Create label from label and number condition
	 
	 '<b>' . trim($scenario->findvalue('./title')) . "</b><br><br>\n",
	 
	 replace_vars( trim($scenario->findvalue('./text')), $replacement_hash ) . "<br><br>\n"));

  

  # 2. Scenario text - End
  
  # # 2. (Single) severity question - Start
  # push (@out_lines,
  # 	("[[Question:Matrix]]",
  # 	 "[[ID:" . $label . ".severity]]", # Create label from label and number condition
  # 	 "", # To avoid "undefined" massage in qualtrics
  # 	 "[[AdvancedChoices]]",
  # 	 "[[Choice]]",
  # 	 remove_line_breaks_and_extra_space ("<p style=\"min-width:350px;font-size:125%;\">" . replace_vars( trim($scenario->findvalue('./question.severity.text')), $replacement_hash ) . "<br></p>"),	 
	 
  # 	 "\n[[AdvancedAnswers]]"));
  
  # foreach my $choice (1..trim($scenario->findvalue('./nchoices'))){
  #   push (@out_lines, "[[Answer:$choice]]");
  #   if ($choice == 1){
  #     push (@out_lines, "$choice: " . remove_line_breaks_and_extra_space ( trim($scenario->findvalue('./question.severity.anchor.left')) ));
  #   } elsif ($choice == $scenario->findvalue('./nchoices')) {
  #     push (@out_lines, "$choice: " . remove_line_breaks_and_extra_space ( trim($scenario->findvalue('./question.severity.anchor.right')) ));
  #   } else {
  #     push (@out_lines, $choice);
  #   }
           
  # }
  # # 2. (Single) severity question - End
  
  # 3. Acceptability question - Start

  push (@out_lines,
	("[[Question:Matrix]]",
	 "[[ID:" . $label . ".acceptability]]", # Create label from label and number condition
	 "", # To avoid "undefined" massage in qualtrics
	 "[[AdvancedChoices]]",
	 "[[Choice]]",
	 remove_line_breaks_and_extra_space ("<p style=\"min-width:350px;font-size:125%;\">" . replace_vars( trim($scenario->findvalue('./question.acceptability')), $replacement_hash ) . "<br></p>"),	 
	 
	 "\n[[AdvancedAnswers]]"));
  
  foreach my $choice (1..trim($scenario->findvalue('./nchoices'))){
    push (@out_lines, "[[Answer:$choice]]");
    if ($choice == 1){
      push (@out_lines, "$choice: Not at all");
    } elsif ($choice == $scenario->findvalue('./nchoices')) {
      push (@out_lines, "$choice: Very much so");
    } else {
      push (@out_lines, $choice);
    }
           
  }
  
  # 3. Acceptability question - End
  
  push (@out_lines, "\n[[PageBreak]]\n");


  @out_lines = map { s/\[br\]/<br>/g; $_ } @out_lines;
  @out_lines = map { s/\[(\/*)b\]/<$1b>/g; $_ } @out_lines;

  map { print OUT $_ } @out_lines;
}

sub get_question_id
{
  my ($scenario_label, $replacement_hash) = @_;

  my @label = ("scenario", $scenario_label);

  foreach my $k (sort (keys %{$replacement_hash})){
    push (@label, ($k, $replacement_hash->{$k}))
          unless ($k =~ /experiment/i);
  }

  return join(".", @label);
  
}
  
sub replace_vars
{
  my ($text, $replacement_hash) = @_;

  foreach my $k (keys %{$replacement_hash}){

    if (($k !~ /experiment/i) &&
	($text =~ /$k/)) {
      if ($replacement_hash->{$k} == 1){

	$text =~ /$k\s+(\S+)[\s,.]/;

	my $nextWord = $1;
	
	if ($nextWord eq "people"){

	  $text =~ s/$k\s+people/1 person/;

	} elsif ($nextWord =~ /s$/) {

	  my $nextWordSing = $nextWord;
	  $nextWordSing =~ s/s$//;

	  $text =~ s/$k\s$nextWord/1 $nextWordSing/;
	  
	} else {

	  my $replacement = commify ($replacement_hash->{$k});

	  $text =~ s/$k/$replacement/g
	  
	}
	
      } else {
	
	my $replacement = commify ($replacement_hash->{$k});
	
	$text =~ s/$k/$replacement/g
	  
      }
    }
  }

  return ($text);
  
}

# From https://rosettacode.org/wiki/Random_Latin_Squares
sub random_ls {
    my($n) = @_;
    my(@cols,@symbols,@ls_sym);
 
    # build n-sized latin square
    my @ls = [0,];
    for my $i (1..$n-1) {
        @{$ls[$i]} = @{$ls[0]};
        splice(@{$ls[$_]}, $_, 0, $i) for 0..$i;
    }
 
    # shuffle rows and columns
    @cols = shuffle 0..$n-1;
    @ls = map [ @{$_}[@cols] ], @ls[shuffle 0..$n-1];
 
}

# Added 02/03/2023 for Experiment 5 and later
# Add thousands separators to script
# From https://www.oreilly.com/library/view/perl-cookbook/1565922433/ch02s18.html
sub commify {
    my $text = reverse $_[0];
    $text =~ s/(\d\d\d)(?=\d)(?!\d*\.)/$1,/g;
    return scalar reverse $text;
}


sub  trim {

  my $text = shift;
  $text =~ s/^\s+|\s+$//g;

  return $text;
}

sub remove_line_breaks_and_extra_space {
  my $text = shift;

  $text =~ s/\n//g;
  $text =~ s/\t/ /g;
  $text =~ s/ +/ /g;

  return $text;

    
}

sub print_demographics
{

  print OUT <<'END_DEMOGRAPHICS';

[[Block:Demographics]]
[[Question:TextEntry]]
[[ID:d1]]
Some demographics


[[PageBreak]]

END_DEMOGRAPHICS
}

  
