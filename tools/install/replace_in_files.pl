#!/usr/bin/env perl
#
# Copyright 2004 Mattias Gaertner
#
# Search and replace in multiple files with perl syntax

use vars qw/ %opt /;
use Getopt::Std;
my $opt_string = 'hvcsinbRf:r:m:';
getopts( "$opt_string", \%opt ) or usage();
usage() if $opt{h};

sub usage(){
  print STDERR << "EOF";

Replace strings in files with perl search and replace syntax

usage: $0 [-hRvcnbis] -f text -r replacetext -m filemask file1 file2 ...

 -h            : this (help) message
 -f <text>     : find text
 -r <text>     : replace with this text
 -R            : recursive
 -i            : case insensitive
 -v            : verbose
 -c            : print changes
 -s            : write summary
 -n            : simulate, do not change files
 -b            : replace in binary files too
 -m <filemask> : file mask, useful for recursive

examples:
  Replace in file1.txt and file2.txt all a with b
    $0 -vsn -f 'a' -r 'b' file1.txt file2.txt

  Replace in file1.txt all a([0-9]) with b$1
    $0 -vsn -f 'a([0-9])' -r 'b$1' file1.txt

EOF
  exit;
}

usage() unless $opt{f}; # Text needed
usage() unless $opt{r}; # Replacetext needed

$FindText=$opt{f};
$ReplaceText=$opt{r};
$FileMask=$opt{m};

if($opt{v}){
  print "Parameters:";
  print "FindText=$FindText\n";
  print "ReplaceText=$ReplaceText\n";
  print "FileMask=$FileMask\n";
  print "\n";
}

$TotalFiles=0;
$ChangedFiles=0;
$TotalLines=0;
$ChangedLines=0;
for $Filename(@ARGV){
  DoFile($Filename,1);
}
if($opt{s}){
  print "\nSummary:\n";
  print "changed files: $ChangedFiles/$TotalFiles\n";
  print "changed lines: $ChangedLines/$TotalLines\n";
}

#==============================================================================

sub DoFile(){
  (my $Filename,$Level) = @_;
  
  # skip special files
  if($Filename eq "."){ return; }
  if($Filename eq ".."){ return; }

  #print "DoFile $Filename\n";

  #
  if(-d $Filename){
    if($opt{R}){
      if($opt{v}){
        #print "Entering directory $Filename\n";
      }
      $Filenames=`ls -1 $Filename`;
      for $SubFilename(split("\n",$Filenames)){
        DoFile($Filename."/".$SubFilename,$Level+1);
      }
      if($opt{v}){
        #print "Leaving directory $Filename\n";
      }
    }
    return;
  }
  if ((! $opt{b}) && (-B $Filename)){
    if($opt{v} && ($Level==1)){
      print "Skipping binary file $Filename\n";
    }
    return;
  }
  if(($FileMask) && ($Filename !~ /$FileMask/)){
    return;
  }

  my $FilenamePrinted = 0;
  if($opt{v}){
    print "Editing $Filename ...\n";
    $FilenamePrinted = 1;
  }

  $TotalFiles++;
  open F, "< $Filename" or die "Unable to read file $Filename: $!";
  $NewText="";
  $LineNumber=1;
  $FileChanged=0;
  while ($Line=<F>){
    $TotalLines++;
    $OldLine=$Line;
    if($opt{i}){
      $Line=~s/$FindText/$ReplaceText/goi;
    } else {
      $Line=~s/$FindText/$ReplaceText/go;
    }
    if($OldLine ne $Line){
      $ChangedLines++;
      $FileChanged=1;
      if($opt{v} || $opt{c}){
        if (! $FilenamePrinted){
          print "Editing $Filename ...\n";
          $FilenamePrinted = 1;
        }
        print "($LineNumber,old):".$OldLine;
        print "($LineNumber,new):".$Line;
      }
    }
    $NewText.=$Line;
    $LineNumber++;
  }
  if($FileChanged==1){
    $ChangedFiles++;
  }
  close F;

  if(! $opt{n}){
    open F, "> $Filename" or die "Unable to write file $Filename: $!";
    print F $NewText;
    close F;
  }
}

# end.

