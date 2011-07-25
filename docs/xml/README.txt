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
If you don't have perl installed, you can use the following command to update
lcl/forms.xml file:

makeskel --update --package=lcl --input="..\..\lcl\forms.pp" --descr=lcl\forms.xml --output=lcl\forms.xml.new

To create a new file use:
makeskel --package=lcl --input="..\..\lcl\forms.pp" --output=lcl\forms.xml

--------------------------------------------------------------------------------
Examples for gtk interface:

Creation:
perl multi_makeskel.pl -p lclgtk -s ../../lcl/interfaces/gtk \
  -o lcl/interfaces/gtk -i '-Fi/path/to/lazarus/lcl/interfaces/gtk' -x

  

