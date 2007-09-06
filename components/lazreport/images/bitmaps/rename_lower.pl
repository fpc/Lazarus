 #!/usr/bin/perl
 opendir(FILES,".")|| die "can't open .";
 while($filename=readdir(FILES)){
 $file=$filename;
 $filename=~tr/[A-Z]/[a-z]/;
 $filename=~tr/ /_/d;
 $filename=~tr/!/_/d;
 rename($file,$filename) || warn "can't rename $file";
 }
 
