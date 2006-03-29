set -x
LAZDEBDIR=$1
for i in `find -maxdepth 1 -not -name debian -and -not -name '.'`; do
    cp -Rl $i $LAZDEBDIR/usr/share/lazarus
done
rm $LAZDEBDIR/usr/share/lazarus/lcl/COPYING
rm $LAZDEBDIR/usr/share/lazarus/COPYING.GPL
rm $LAZDEBDIR/usr/share/lazarus/COPYING.LGPL
rm $LAZDEBDIR/usr/share/lazarus/COPYING.modifiedLGPL
rm $LAZDEBDIR/usr/share/lazarus/COPYING
cd $LAZDEBDIR/usr/share/lazarus
mv lazarus ../../lib/lazarus
mv startlazarus ../../lib/lazarus
mv tools/svn2revisioninc ../../lib/lazarus/tools/svn2revisioninc
for i in `find -type d -name 'lib' -or -name 'units'`; do
        d=`dirname $i`
        mkdir -p ../../lib/lazarus/$d
        mv $i ../../lib/lazarus/$d			  
done

