#!/usr/bin/perl
# ¶à½ø³Ì

use 5.012;
my @list=(1..22,"X","Y");
my @chrs=map {"chr".$_} @list;

my $active_proc_num=0;
my $finish_proc_num=0;

$SIG{CHLD} = \&handle_signal;

foreach my $chr(@chrs){
	my $npf_path="/mnt/data2/songyf/project/apa13tissue/4step/fseq/minus/".$chr.".npf";
    ## == fork a new process ==
    my $pid = fork();
 
    if (!defined($pid)) {
        print "Error in fork: $!";
        exit 1;
    }
 
    if ($pid == 0) {
 
        ## == child proc ==
        print "Child $npf : My pid = $$ \n";
        exec "Rscript call_peak_mode.R minus.site.count.on.genome $npf_path ${chr}_minus_peak_mode";
        die "Child $npf: cannot exec!\n";
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

sub handle_signal{
	my $pid=0;
	while ( ($pid=waitpid -1,1)>0 ){
		$active_proc_num--;
		say "reaped $pid";
		$finish_proc_num++;
	}
}