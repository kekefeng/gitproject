#!/usr/bin/perl -w
use strict;
open(GTF_REMODEL,"< $ARGV[0]")
    or die "Can not open the file\n";
open(PEAK,"< $ARGV[1]")
    or die "Can not open the file\n";
open(PEAK_MAP,"> $ARGV[2]")
    or die "Can not open the file\n";
my @line=();
my %gtf=();
my $i=0;
while (<GTF_REMODEL>){
chomp($_);
$gtf{$i}=$_;
$i=$i+1;
}
#
my @peak_line=();
my @gtf_line=();
while (<PEAK>) {
my %temp_peak_map=();
chomp($_);
@peak_line=split /\s+/, $_;
foreach my $key (sort keys %gtf)
{
@gtf_line=split /\s+/, $gtf{$key};	
if (($peak_line[0] eq $gtf_line[4]) and ($peak_line[12]>=$gtf_line[6]) and ($peak_line[12]<=$gtf_line[7]))
{
@{$temp_peak_map{$gtf_line[5]}}=@gtf_line;
}
}
#
if (exists $temp_peak_map{'\'PA\''})
{
print PEAK_MAP "$_";
foreach my $location_item (@{$temp_peak_map{'\'PA\''}})
{
print PEAK_MAP "\t$location_item";
}
print PEAK_MAP "\n";
}
elsif (exists $temp_peak_map{'\'3\'UTR\''})
{
print PEAK_MAP "$_";
foreach my $location_item (@{$temp_peak_map{'\'3\'UTR\''}})
{
print PEAK_MAP "\t$location_item";
}
print PEAK_MAP "\n";
}
elsif (exists $temp_peak_map{'\'extend_3\'UTR\''})
{
print PEAK_MAP "$_";
foreach my $location_item (@{$temp_peak_map{'\'extend_3\'UTR\''}})
{
print PEAK_MAP "\t$location_item";
}
print PEAK_MAP "\n";
}
elsif (exists $temp_peak_map{'\'exon\''})
{
print PEAK_MAP "$_";
foreach my $location_item (@{$temp_peak_map{'\'exon\''}})
{
print PEAK_MAP "\t$location_item";
}
print PEAK_MAP "\n";
}
elsif (exists $temp_peak_map{'\'intron\''})
{
print PEAK_MAP "$_";
foreach my $location_item (@{$temp_peak_map{'\'intron\''}})
{
print PEAK_MAP "\t$location_item";
}
print PEAK_MAP "\n";
}
elsif (exists $temp_peak_map{'\'5\'UTR\''})
{
print PEAK_MAP "$_";
foreach my $location_item (@{$temp_peak_map{'\'5\'UTR\''}})
{
print PEAK_MAP "\t$location_item";
}
print PEAK_MAP "\n";
}
elsif (exists $temp_peak_map{'\'TSS\''})
{
print PEAK_MAP "$_";
foreach my $location_item (@{$temp_peak_map{'\'TSS\''}})
{
print PEAK_MAP "\t$location_item";
}
print PEAK_MAP "\n";
}
elsif (exists $temp_peak_map{'\'promoter\''})
{
print PEAK_MAP "$_";
foreach my $location_item (@{$temp_peak_map{'\'promoter\''}})
{
print PEAK_MAP "\t$location_item";
}
print PEAK_MAP "\n";
}
}
close(GTF_REMODEL);
close(PEAK);
close(PEAK_MAP);
