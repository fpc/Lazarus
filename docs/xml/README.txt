These are the fpdoc xml files of the lazarus units.

The lcl directory contains the fpdoc xml files for the lcl. For example
lcl/controls.xml for controls.pp.

--------------------------------------------------------------------------------
They were created with:
perl multi_makeskel.pl -p lcl -s ../../lcl -o lcl \
  -i '-Fi/path/to/lazarus/lcl/include' -x
  
At the moment the fpdoc tool 'makeskel' is not yet capable of updating the
files. But this will follow in a few weeks.

--------------------------------------------------------------------------------
Finding the new files, not yet in CVS:

perl find_cvs_fpdoc_files.pl -s ../../lcl -o lcl -l new

--------------------------------------------------------------------------------
Finding old the files in CVS not needed any more:

perl find_cvs_fpdoc_files.pl -s ../../lcl -o lcl -l old


--------------------------------------------------------------------------------
Examples for gtk interface:

Creation:
perl multi_makeskel.pl -p gtk -s ../../lcl/interfaces/gtk \
  -o lcl/interfaces/gtk -i '-Fi/path/to/lazarus/lcl/interfaces/gtk' -x

Finding the new files, not yet in CVS:
perl find_cvs_fpdoc_files.pl -s ../../lcl/interfaces/gtk \
  -o lcl/interfaces/gtk -l new

Finding old the files in CVS not needed any more:
perl find_cvs_fpdoc_files.pl -s ../../lcl/interfaces/gtk \
  -o lcl/interfaces/gtk -l old
  

