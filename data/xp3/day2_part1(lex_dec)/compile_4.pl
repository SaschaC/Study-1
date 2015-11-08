use warnings;
use strict;

# spalten fuer phonotactic legality hinzufuegen und Liste; output = compiled_4.txt
print("go?");
my $go = <STDIN>;

open (OUT, ">compiled_4.txt");

open(COMP3 , "compiled_3.txt") or die("$!");
open(PHON, "W:\\EXPERIMENTS\\AUDITORY_RESTORED\\REDUCTIES\\SASCHA\\Study_1\\main experiment\\Results - Compiled\\xp3\\phonotactics.txt") or die ("$!");

my @results;
my %phon;

foreach my $line (<COMP3>) {
	chomp($line);
	push(@results, $line);
}


foreach my $line (<PHON>) {
	chomp($line);
	my @temp = split(/\t/, $line);
	$phon{$temp[0]} = $temp[3];
}

#### titelzeile ########

my @temp = split(/\t/, $results[0]);
splice(@temp, 11, 0, 'p_universal');
for (my $i = 0; $i < $#temp; $i++) {
	print(OUT "$temp[$i]\t");
}
print(OUT "$temp[$#temp]\n");

##########################

for (my $i = 1; $i <= $#results; $i++) {
	my $line = $results[$i];
	my @temp = split(/\t/, $line);
	my $file = $temp[9];
	my $set = get_set($file);
	my $leg;
	if ($set =~ /^t/) {
		$leg = $phon{$set};
	}
	else {
		$leg = 'NA'
	}
		
	splice(@temp, 11, 0, $leg);
	for (my $i = 0; $i < $#temp; $i++) {
		print(OUT "$temp[$i]\t");
	}
	print(OUT "$temp[$#temp]\n");
}

sub get_set {
	my $set = shift;
	my @temp = split(/_/, $set);
	return $temp[0];
}