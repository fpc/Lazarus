These are the fpdoc xml files of the lazarus units.

The lcl directory contains the fpdoc xml files for the lcl. For example
lcl/controls.xml for controls.pp.

You can create new entries with the fpdoc editor of the IDE (View / FPDoc Editor).

--------------------------------------------------------------------------------
You can create xml files for new units with the IDE fpdoc editor or with:
perl multi_makeskel.pl -p lcl -s ../../lcl -o lcl \
  -i '-Fi/path/to/lazarus/lcl/include' -x
  
At the moment the fpdoc tool 'makeskel' is not yet capable of updating the
files.

--------------------------------------------------------------------------------
If you don't have perl installed, you can use the following command to update
lcl/forms.xml file:

makeskel --update --package=lcl --input="..\..\lcl\forms.pp" --descr=lcl\forms.xml --output=lcl\forms.xml.new

To create a new file use:
makeskel --package=lcl --input="..\..\lcl\forms.pp" --output=lcl\forms.xml

See also: StyleGuide.txt

--------------------------------------------------------------------------------
Examples for gtk interface:

Creation:
perl multi_makeskel.pl -p lclgtk -s ../../lcl/interfaces/gtk \
  -o lcl/interfaces/gtk -i '-Fi/path/to/lazarus/lcl/interfaces/gtk' -x

  

