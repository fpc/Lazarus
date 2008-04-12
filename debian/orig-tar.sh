#!/bin/sh -e

# called by uscan with '--upstream-version' <version> <file>
DIR=lazarus

# clean up the upstream tarball
tar xzf $3
rm -f $DIR/debian/*.ex $DIR/debian/*.EX $DIR/debian/files
tar czf $3 $DIR
rm -rf $DIR

# move to directory 'tarballs'
if [ -r .svn/deb-layout ]; then
    . .svn/deb-layout
    mv $3 $origDir
    echo "moved $3 to $origDir"
fi

