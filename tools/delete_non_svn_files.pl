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
# This perl script deletes all files not in CVS.

use vars qw/ %opt /;
use Getopt::Std;
use Cwd;
my $opt_string = 'hd:rflqx';
getopts( "$opt_string", \%opt ) or usage();
usage() if $opt{h};

sub usage(){
  print STDERR << "EOF";

Delete all files not in CVS

usage: $0 [-hdrflqx]

 -h             : this (help) message
 -d <directory> : the directory where to search for files, default: current dir
 -r             : recursive: search in sub directories too
 -f             : delete only files, not directories
 -q             : quiet
 -x             : really delete them, default: simulate

examples:
  delete_non_cvs_files.pl -r -d /your/lazarus/path

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

if(!$opt{x}){
  print "\nThis was a simulation. To really delete files, use the -x option.\n";
}

exit;

sub SearchDir(){
  (my $CurDir)=@_;
  my $line;
  my $SVNFiles;
  my @Files;
  my $i;
  my $CurFile;
  my $CurFullFile;

  if($opt{r}){
    # search recursive
    # get files
    opendir(DIR, $CurDir);
    @Files = readdir(DIR);
    closedir(DIR);
    for ($i=0; $i<=$#Files; $i++){
      $CurFile=$Files[$i];
      $CurFullFile=$CurDir."/".$CurFile;
      # skip special files
      next unless ($CurFile);
      next if (($CurFile eq '.') || ($CurFile eq '..') || ($CurFile eq '.svn'));
      &SearchDir($CurFullFile);
    }
  }

  # get files
  opendir(DIR, $CurDir);
  @Files = readdir(DIR);
  closedir(DIR);
  for ($i=0; $i<=$#Files; $i++){
    $CurFile=$Files[$i];
    $CurFullFile=$CurDir."/".$CurFile;
    # skip special files
    next unless ($CurFile);
    next if (($CurFile eq '.') || ($CurFile eq '..') || ($CurFile eq '.svn'));
    # skip directories if not wanted
    next if (($opt{f}) && (-d $CurFullFile));
    # search file in SVN files
    $Output=`svn info $CurFullFile 2>&1`;
    #print "AAA1 $CurFullFile: $? BBB1$Output BBB2 \n";
    if ($Output!~/\nURL:/){
      if (-d $CurFullFile){
        print "rmdir ".$CurFullFile."\n";
      } else {
        if (!$opt{q}){
          print "unlink ".$CurFullFile."\n";
        }
      }
      if($opt{x}){
        if (-d $CurFullFile){
          rmdir($CurFullFile);
        } else {
          unlink($CurFullFile);
        }
      }
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

