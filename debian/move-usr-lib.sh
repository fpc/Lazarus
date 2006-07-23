LAZDEBDIR=$1
LAZTARGET=lib
for i in `find -maxdepth 1 -not -name debian -and -not -name install -and -not -name '.'`; do
    cp -Rl $i $LAZDEBDIR/usr/$LAZTARGET/lazarus
done
rm -f $LAZDEBDIR/usr/$LAZTARGET/lazarus/COPYING.GPL
rm -f $LAZDEBDIR/usr/$LAZTARGET/lazarus/COPYING.LGPL
rm -f $LAZDEBDIR/usr/$LAZTARGET/lazarus/COPYING.modifiedLGPL
rm -f $LAZDEBDIR/usr/$LAZTARGET/lazarus/COPYING
if [ "$LAZTARGET" = "share" ]; then
    cd $LAZDEBDIR/usr/share/lazarus
    mv lazarus ../../lib/lazarus
    mv startlazarus ../../lib/lazarus
    mv tools/svn2revisioninc ../../lib/lazarus/tools/svn2revisioninc
    for i in `find -type d -name 'lib' -or -name 'units'`; do
            d=`dirname $i`
            mkdir -p ../../lib/lazarus/$d
            mv $i ../../lib/lazarus/$d			  
    done
fi
