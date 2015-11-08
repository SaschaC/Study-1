use warnings;
use strict;

# die stimulidauern und die rt von stimulusende berechnen und hinzufuegen, output: compiled_2.txt
print("go?");
my $go = <STDIN>;

open (OUT, ">compiled_2.txt");

open(WAVLIST, "W:\\EXPERIMENTS\\AUDITORY_RESTORED\\REDUCTIES\\SASCHA\\Study_1\\main experiment\\ExperimentalRoom\\stimuli_final\\dur_list.txt") or die("$!");
open(COMP1 , "compiled_1.txt") or die("$!");

my @results;
my %dur;

foreach my $line (<WAVLIST>) {
	chomp($line);
	my @temp = split(/\t/, $line);
	$dur{$temp[0]} = $temp[1];

}

foreach my $line (<COMP1>) {
	chomp($line);
	push(@results, $line);
}
my @temp = split(/\t/, $results[0]);
splice(@temp, 15, 0, 'duration');
splice(@temp, 17, 0,  'rt_end');

for (my $i = 0; $i < $#temp; $i++) {
	print(OUT "$temp[$i]\t");
}
print(OUT "$temp[$#temp]\n");


for (my $i = 1; $i <= $#results; $i++) {
	my $line = $results[$i];
	my @temp = split(/\t/, $line);
	my $file = $temp[9];
	my $rt = $temp[15];
	my $dur = $dur{$file};
	print("$file\n");
	my $new_rt = $rt - $dur;
	splice(@temp, 15, 0, $dur);
	splice(@temp, 17, 0, $new_rt);
	for (my $i = 0; $i < $#temp; $i++) {
		print(OUT "$temp[$i]\t");
	}
	print(OUT "$temp[$#temp]\n");
}

