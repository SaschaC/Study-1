use warnings;
use strict;

# die ergebnisse aus den ergebnisfiles zusammenstellen, output: compiled_1.txt
print("go?");
my $go = <STDIN>;

open (OUT, ">compiled_1.txt");

print(OUT "session\tlist\tgroup\tsubject\tage\tsex\thand\tassist\ttrial\tfile\tstimulus\tis_target\tis_word\tred\tis_schwa\trt\tresponse\tcorrectness\n");

opendir(Dir, "W:\\EXPERIMENTS\\AUDITORY_RESTORED\\REDUCTIES\\SASCHA\\Study_1\\main experiment\\Results - Compiled\\xp3\\day2_part1\(lex_dec\)\\") or die "$!";
my @files = grep(/^Day.+\.txt$/,readdir(Dir));

FILE:
foreach my $txt(@files) {
	print("$txt");
	(my $explist) = $txt =~ m/(A\d|B\d|C\d|D\d)/;
	print("$explist\t$txt\n");
	open(IN, "$txt") or die("$!");
	my @input = [];
	
	foreach my $line(<IN>) {
	
		push(@input, $line);
	}

	my $start;
	my $end;

#### subject, group (header) ########


	my @expname = split(/_/, $input[12]);
	my $list = $expname[$#expname];
	chomp($list);


	my @temp = @input;
	# wtf???
	my @header = splice(@temp, 17, 7);
	for (my $i = 0; $i <= $#header; $i++) {	
		chomp($header[$i]);
		$header[$i] =~ s/\s+//;
		$header[$i] =~ s/.+://;
	
	}

	my $subject = $header[0];
	my $session = $header[1];
	my $group = $header[2];
	my $age = $header[3];
	my $sex = $header[4];
	my $hand = $header[5];
	my $assist = $header[6];


#### TRIALS #####

	for (my $i = 0; $i <= $#input; $i++) {

		my $line = $input[$i];

		if ($line =~ /Level: 1/) { print("ok"); next FILE;}
	
		if ($line =~ /LogFrame Start/) {

		$start = $i + 1;
		}
		if ($line =~ /LogFrame End/) {
			$end = $i -1;
			my $cutoff = $end - $start +1;
			@temp = @input;
			my @chunk = splice(@temp, $start, $cutoff);
			
			output($explist,$txt, \@chunk, $session, $group, $subject, $age, $sex, $hand, $assist);
		}
	}
}
sub output {
	my $explist = shift;
	my $txt = shift;
	my $chunk = shift;
	my @chunk = @{$chunk};
	my $session = shift;
	my $group = shift;
	my $subject = shift;
	my $age = shift;
	my $sex = shift;
	my $hand = shift;
	my $assist = shift;
	for (my $i = 0; $i <= $#chunk; $i++) {
		chomp($chunk[$i]);
		$chunk[$i] =~ s/.+:\s//;
	}
	my $trial = $chunk[0];
	my $file = $chunk[2];
	my $is_word = is_word($txt, $file);
	my $stimulus = $chunk[5];
	my $istarget = $chunk[6];
	my $red = $chunk[7];
	my $isschwa = $chunk[8];
	my $rt = $chunk[17];
	my $response = $chunk[18];
	my $cresp = $chunk[19];
	my $correctness;
	
	if ($response == $cresp) {
		$correctness = 'yes';
	}
	else {
		$correctness = 'no';
	}
	print(OUT "$session\t$explist\t$group\t$subject\t$age\t$sex\t$hand\t$assist\t$trial\t$file\t$stimulus\t$istarget\t$is_word\t$red\t$isschwa\t$rt\t$response\t$correctness\n");
}

sub is_word {
	my $txt = shift;
	my $file = shift;
	if ($file =~ /^n/) {
		return 'no';
	}
	elsif (($file =~ /^f/) or ($file =~ /^t/)) {
		return 'yes';
	}
	else {
		die("error: filename $file in $txt");
	}
}

my $end = <STDIN>;


