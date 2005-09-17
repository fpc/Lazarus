#!/usr/bin/env bash

if [ -f ~/.rpmmacros ]; then
  RPMDir=$(cat ~/.rpmmacros | egrep '^%_topdir ' | sed -e 's#^%_topdir ##')
else
  RPMDir=/usr/src/redhat
fi

echo $RPMDir

# end.

