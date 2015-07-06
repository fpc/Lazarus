BEWARE: This is all work in progress.

UNDER CONSTRUCTION

This directory contains tools to create the offline help of the wiki.

Downloading the wiki:

Compile wikiget.lpi

Download all pages and images:
./wikiget --allmissing

Download changes of last 2 days
./wikiget --recent=2 --deletenotusedpages --deletenotusedimages

Note: you can stop/kill the tool at any time and run it again. It will download
only the missing files.
See ./wikiget -h for all options.


Creating XHTML pages:
./wikiconvert --format=xhtml --css=css/wiki.css 'wikixml/*.xml'


Creating HTML pages:
./wikiconvert --format=html --css=css/wiki.css 'wikixml/*.xml'


Creating chm:
./wikiconvert --format=chm --css=css/wiki.css wikixml/Lazarus_Documentation.g400.xml 'wikixml/*.xml'


ToDos
ToDos wiki parser: see wikiparser.pas

ToDos iphtml:
-too big space between paragraphs
-background for pre
-slow on some pages (e.g. lazarus_documentation)

Todos: convert to fpdoc
 ./wikiconvert --format=fpdoc --outputdir=fpdocxml wikixml/1-dimensional_arrays.0000.xml
 cd fpdochtml/
 fpdoc --format=html --charset=UTF8 --image-url=../images --package=wiki --descr=../fpdocxml/1-dimensional_arrays.0000.xml --verbose
ToDo: find out why fpdoc does not use the description.

