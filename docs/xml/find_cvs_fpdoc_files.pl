#!/usr/bin/env perl
#
# *****************************************************************************
# *                                                                           *
# *  See the file COPYING.modifiedLGPL, included in this distribution,        *
# *  for details about the copyright.                                         *
# *                                                                           *
# *  This program is distributed in the hope that it will be useful,          *
# *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
# *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
# *                                                                           *
# *****************************************************************************
#
# Author: Mattias Gaertner

use vars qw/ %opt /;
use Getopt::Std;
my $opt_string = 'hs:l:o:x';
getopts( "$opt_string", \%opt ) or usage();
usage() if $opt{h};

sub usage(){
  print STDERR << "EOF";

Create fpdoc sceletons for pascal units

usage: $0 [-hsolx]

 -h                    : this (help) message
 -s <source directory> : the source directory with the .pp and .pas files
 -o <output directory> : the output directory for the xml files
 -l new|old            : new = show new fpdoc files, not yet in CVS
                         old = show old CVS files, not needed by fpdoc
 -x                    : call cvs

examples:
  find_cvs_fpdoc_files.pl -s ../../lcl -o lcl -l new

EOF
  exit;
}

($opt{s}) || usage(); # source directory needed
($opt{o}) || usage(); # output directory needed
($opt{l}) || usage(); # list option needed
($opt{l}=~/^(new|old)$/) || usage();

$SrcDir=$opt{s};
(-d $SrcDir) || die "source directory $SrcDir is not a directory\n";
$SrcDir=~s#//#/#;
$SrcDir=~s#/$##;

$OutDir=$opt{o};
(-d $OutDir) || die "output directory $OutDir is not a directory\n";
$OutDir=~s#//#/#;
$OutDir=~s#/$##;


# get pascal units
opendir(DIR, $SrcDir);
@SrcFiles = grep(/\.(pas|pp)$/,readdir(DIR));
for ($i=0; $i<=$#SrcFiles; $i++){
  ($SrcFiles[$i]=~/^([a-zA-Z0-9_]+)\./);
  $SrcUnits.=" ".$1;
}
closedir(DIR);
#print "$SrcUnits\n";

# get FPDoc xml files
opendir(DIR, $OutDir);
@XMLFiles = grep(/\.xml$/,readdir(DIR));
closedir(DIR);

# get CVS files
open(F, $OutDir."/CVS/Entries");
while($line=<F>){
  ($line=~/^\/([^\/]+)\//);
  $CVSEntries.="/".$1;
}
close(F);
#print "$CVSEntries\n";

for $XMLFile(@XMLFiles){
  ($XMLFile=~/^([a-zA-Z0-9_]+).xml/);
  $UnitName=$1;
  if($SrcUnits=~/\b$UnitName\b/){
    $UnitNeeded=1;
  } else {
    $UnitNeeded=0;
  }
  if($CVSEntries=~/\b$UnitName\.xml\b/){
    $UnitInCVS=1;
  } else {
    $UnitInCVS=0;
  }
  if (($UnitNeeded) && (! $UnitInCVS)){
    $NewFilesNotYetInCVS.=$OutDir."/".$XMLFile." ";
  }
  if ((! $UnitNeeded) && ($UnitInCVS)){
    $OldFilesInCVS.=$OutDir."/".$XMLFile." ";
  }
  #print $XMLFile." ".$UnitName." $UnitNeeded $UnitInCVS\n";
}

if ($opt{l} eq "new"){
  print "Adding new Files:\n";
  if ($NewFilesNotYetInCVS){
    $Command="cvs -z3 add $NewFilesNotYetInCVS";
    DoIt($Command);
  }
}
if ($opt{l} eq "old"){
  print "Removing unneeded files:\n";
  if ($OldFilesInCVS){
    $Command="rm $OldFilesInCVS";
    DoIt($Command);
    $Command="cvs -z3 remove $OldFilesInCVS";
    DoIt($Command);
  }
}


if(!$opt{x}){
  print "\nThis was a simulation. To really remove files and change cvs, use the -x option.\n";
}

sub DoIt(){
  (my $Command)=@_;
  print $Command."\n";
  if($opt{x}){
    my $Output=`$Command`;
    if($?){
      die "Command failed:\n".$Output;
    }
    print $Output;
  }
}

# end.

