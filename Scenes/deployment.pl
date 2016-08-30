#!/usr/bin/perl

$argc = @ARGV;
if ($argc < 4 ) 
{
	printf(" num params error\n");
	printf(" %s %s %s %s %s\n", $ARGV[0], $ARGV[1], $ARGV[2],$ARGV[3]);
}

if (!(-e $ARGV[0]))
{
  printf("Input file1 (%s)  does not exist\n", $ARGV[0]);
  exit(-1);
}


$input_file1 = $ARGV[0];
$mode = $ARGV[1];
$maxCP = $ARGV[2];
$thr= $ARGV[3];


open(IN,$input_file1);

while (defined (my $line = <IN>)) {
	@tmp = split(' ', $line); 
	push(@mat, [@tmp]);
}

$num_rows = int(@mat); #@mat == # of rows
$num_cols = int(@{mat->[0]}); # @{mat->[0]} == # of cols

$informedTotal=0;
$informedRate=0;
$placedCPs=0;


if($mode==0){ #muestra la cantidad de vehiculos que pasan por cada CP
	for ($f=0; $f<@mat; $f++){
		@visits[$f] = &sumRow($f);
		printf("CP$f = @visits[$f]\n");
	}
}elsif($mode== 4){
	for ($c=0; $c<@{mat->[0]}; $c++){ 	@aimedTime[$c] = $thr;	}

	if($maxCP == 0) #mode: put as many CP as needed to cover 100% of the nodes.
	{
		while ($placedCPs < @mat)
		{
			($mostVisited, $visits) = &placeCP5;
			$placedCPs++;
			printf("$mostVisited\t");
		}
		printf("\n");
	
	}elsif($maxCP>0){ #mode: switches "on" #maxCPs
		while ($maxCP != 0)
		{
			($mostVisited, $visits) = &placeCP5;
			$maxCP--;
			$placedCPs++;
			printf("$mostVisited,\n");
		}


	}
}
close(IN);


sub placeCP5{

		for ($f=0; $f<@mat; $f++){
			$sR = &sumRow3($f);
			@visits[$f] = $sR; #( $sR * $CPsumRows[$f]);
		}

		($mostVisited, $visits) = &maxWithIndex(@visits);

		# at row most visited, each column marked with a 1 must be cleared
		for ($c=0; $c<$num_cols; $c++){
			if($mat[$mostVisited][$c]>0){
				if($mat[$mostVisited][$c]>=$aimedTime[$c]){
					$aimedTime[$c]=0;
				}
				else{
					$aimedTime[$c]-= $mat[$mostVisited][$c];
				}
				$mat[$mostVisited][$c]=0;
			}
		}
		return ($mostVisited, $visits);
}


sub sumRow ($)
{
	
	$sum=0;
	
	for ($cAux=0; $cAux<$num_cols; $cAux++)
	{
		$sum+=$mat[$_[0]][$cAux];		
	}

	return $sum;

}

sub sumRow3 ($)
{
	
	$sum=0;
	
	for ($cAux=0; $cAux<$num_cols; $cAux++)
	{

		$t0=$mat[$_[0]][$cAux];				
		if ($aimedTime[$cAux]<$mat[$_[0]][$cAux])
		{
			$t0=$aimedTime[$cAux];
		}

		
		$sum+=$t0;
	}

	return $sum;

}




sub maxWithIndex {
    my $max = shift(@_);
	my $maxIndex = 0;
	my $i=1;
    foreach $foo (@_) {
		$maxIndex = $i if $max < $foo;
        $max = $foo if $max < $foo;
		$i++;
    }
    return ($maxIndex, $max);
}
