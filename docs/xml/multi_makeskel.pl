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
my $opt_string = 'hp:s:o:i:m:x';
getopts( "$opt_string", \%opt ) or usage();
usage() if $opt{h};

sub usage(){
  print STDERR << "EOF";

Create fpdoc sceletons for pascal units

usage: $0 [-hpsoimx]

 -h                    : this (help) message
 -p <packagename>      : the FPDoc package name to which the units belong
 -s <source directory> : the source directory with the .pp and .pas files
 -o <output directory> : the output directory for the xml files
 
 optional:
 -i <parser options>   : the input options for the parser
 -m <makeskel>         : alternative path to makeskel
 -x : do not simulate, execute makeskel and overwrite all existing xml files

examples:
  multi_makeskel.pl -p lcl -s ../../lcl -o lcl -i '-Fi/path/to/lazarus/lcl/include'

EOF
  exit;
}

($opt{p}) || usage(); # packagename needed
($opt{s}) || usage(); # source directory needed
($opt{o}) || usage(); # output directory needed

$PackageName=$opt{p};
($PackageName=~/^[a-z0-9][a-z0-9_]*$/) || die "invalid packagename $PackageName\n";

$SrcDir=$opt{s};
(-d $SrcDir) || die "source directory $SrcDir is not a directory\n";
$SrcDir=~s#//#/#;
$SrcDir=~s#/$##;

$OutDir=$opt{o};
(-d $OutDir) || die "output directory $OutDir is not a directory\n";
$OutDir=~s#//#/#;
$OutDir=~s#/$##;

$Makeskel="makeskel";
if($opt{m}){
  $Makeskel=$opt{m};
}

$ParserOptions="";
if($opt{p}){
  $ParserOptions=$opt{p};
}

# get pascal unit files
opendir(DIR, $SrcDir);
@Files = grep(/\.(pas|pp)$/,readdir(DIR));
closedir(DIR);

for $SrcFile(@Files){
  $OutFile=$SrcFile;
  ($OutFile=~s/\.(pas|pp)$/.xml/);
  print $SrcFile." -> ".$OutFile."\n";
  $Input=$SrcDir."/".$SrcFile;
  if ($opt{i}){
    $Input.="' ".$opt{i}."'";
  }
  $Output=$OutDir."/".$OutFile;
  $Command="$Makeskel --package=$PackageName --input=$Input --output=$Output";
  print $Command."\n";
  if($opt{x}){
    system($Command);
    if($?){
      exit;
    }
  }
}

if(!$opt{x}){
  print "\nThis was a simulation. To really overwrite the xml files, use the -x option.\n";
}

# end.

