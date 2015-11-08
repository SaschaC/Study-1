use warnings;
use strict;

# kennzeichnung fuer variable TARGET TYPE (match vs. mismatch)
print("go?");
my $go = <STDIN>;

open (OUT, ">compiled_3.txt");

open(COMP2 , "compiled_2.txt") or die("$!");

my @results;

foreach my $line (<COMP2>) {
	chomp($line);
	push(@results, $line);
}

#### titelzeile ########

my @temp = split(/\t/, $results[0]);
splice(@temp, 13, 0, 'targettype');
for (my $i = 0; $i < $#temp; $i++) {
	print(OUT "$temp[$i]\t");
}
print(OUT "$temp[$#temp]\n");

##########################

for (my $i = 1; $i <= $#results; $i++) {
	my $line = $results[$i];
	my @temp = split(/\t/, $line);
	my $group = $temp[2];
	my $is_target = $temp[11];
	my $red = $temp[13];	
	my $type = get_type($group, $is_target, $red);
	
	splice(@temp, 13, 0, $type);
	for (my $i = 0; $i < $#temp; $i++) {
		print(OUT "$temp[$i]\t");
	}
	print(OUT "$temp[$#temp]\n");
}

sub get_type {
	my $group = shift;
	my $is_target = shift;
	my $red = shift;

	if ($is_target eq "yes") {
		if ((($group == 1) and ($red eq "u")) or (($group == 2) and ($red eq "r"))) {
			return 'match';
		}
		elsif ((($group == 1) and ($red eq "r")) or (($group == 2) and ($red eq "u"))) {
			return 'mismatch';
		}
		else { die("error targettype"); }
	}
	elsif ($is_target eq "no") {
		return 'filler'
	}
	else { die("error targettype"); }
}

