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
#
# This perl script lists all files not in CVS.

use vars qw/ %opt /;
use Getopt::Std;
use Cwd;
my $opt_string = 'hd:rfl';
getopts( "$opt_string", \%opt ) or usage();
usage() if $opt{h};

sub usage(){
  print STDERR << "EOF";

lists all files not in CVS

usage: $0 [-hdrfl]

 -h             : this (help) message
 -d <directory> : the directory where to search for files, default: current dir
 -r             : recursive: search in sub directories too
 -f             : show only files, not directories
 -l             : show as space divided list

examples:
  find_non_cvs_files.pl -r -d /your/lazarus/path

EOF
  exit;
}

if (!$opt{d}){
  $opt{d}=&getcwd;
}

$StartDir=$opt{d};
(-d $StartDir) || die "directory $StartDir is not a directory\n";
$StartDir=~s#//#/#g;
$StartDir=~s#/$##g;

&SearchDir($StartDir);

exit;

sub SearchDir(){
  (my $CurDir)=@_;
  my $line;
  my $CVSEntries;
  my @Files;
  my $i;
  my $CurFile;
  my $CurFullFile;

  # get CVS files
  open(DIR, $CurDir."/CVS/Entries");
  while($line=<DIR>){
    ($line=~/^\/([^\/]+)\//);
    $CVSEntries.="\n".$1."\n";
  }
  close(DIR);
  #print "$CVSEntries\n";

  # get files
  opendir(DIR, $CurDir);
  @Files = readdir(DIR);
  closedir(DIR);
  for ($i=0; $i<=$#Files; $i++){
    $CurFile=$Files[$i];
    $CurFullFile=$CurDir."/".$CurFile;
    # skip special files
    next unless ($CurFile);
    next if (($CurFile eq '.') || ($CurFile eq '..') || ($CurFile eq 'CVS'));
    # skip directories if not wanted
    next if (($opt{f}) && (-d $CurFullFile));
    # search file in CVS files
    if($CVSEntries!~/\n$CurFile\n/){
      print $CurFullFile;
      if($opt{l}){
        print " ";
      } else {
        print "\n";
      }
    }
  }

  if($opt{r}){
    # search recursive
    for ($i=0; $i<=$#Files; $i++){
      $CurFile=$Files[$i];
      $CurFullFile=$CurDir."/".$CurFile;
      # skip special files
      next unless ($CurFile);
      next if (($CurFile eq '.') || ($CurFile eq '..') || ($CurFile eq 'CVS'));
      &SearchDir($CurFullFile);
    }
  }
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

