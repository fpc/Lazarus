#!/bin/bash

#==========================================================================
# removes files generated using make-docs.sh and make-archive.sh
#==========================================================================
echo ""
echo "Removing generated files"
echo ""

# generated footers
rm -v ./datetimectrls-{chm,html}-footer.xml
# chm output
rm -v ./datetimectrls.{chm,xct}
# html archive file
rm -v ./docs-html-datetimectrls-*.7z
# html files
rm -vrf ./datetimectrls/
rm -v build_{chm,html}.log
