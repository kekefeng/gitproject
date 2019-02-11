#!/usr/bin/perl
# ¶à½ø³Ì
#usage: perl runmultimap.pl tissue strand(minus|plus)
use 5.012;
my @list=(1..22,"X","Y");
my @chrs=map {"chr".$_} @list;

my $tissue=shift;
my $strand=shift;
my $active_proc_num=0;
my $finish_proc_num=0;

$SIG{CHLD} = \&handle_signal;

foreach my $chr(@chrs){
    ## == fork a new process ==
    my $pid = fork();
 
    if (!defined($pid)) {
        print "Error in fork: $!";
        exit 1;
    }
 
    if ($pid == 0) {
 
        ## == child proc ==
        print "tissue $tissue Child $chr : My pid = $$ \n";
        exec "perl bed_map.pl $tissue/$strand/$chr peak2gene/$strand/$chr $tissue/peaktag/${strand}_$chr $tissue/genebed/${strand}_$chr";
        die "Child $chr: cannot exec!\n";
        exit 0;
    }
	
	$active_proc_num++;
    ## == if need to collect zombies ==
	sleep 5 while ($active_proc_num > 12);
}

while (1){
	sleep 5;
	last if ($finish_proc_num == 24);
}
#wait for all process finish

sub handle_signal{
	my $pid=0;
	while ( ($pid=waitpid -1,1)>0 ){
		$active_proc_num--;
		say "reaped $pid";
		$finish_proc_num++;
	}
}