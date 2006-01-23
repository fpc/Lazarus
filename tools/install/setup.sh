#!/bin/bash
WHERE=`pwd`

FPC="y"
echo "Install fpc-1.9.6 (Only say no if you already have it/newer) [y/n] ?"
read FPC
if [ $FPC == "y" ] ; then
FPCSRC="u"
echo "Installing freepascal"
cd $WHERE/fpc
yes "" | ./install.sh
echo "Installing freepascal sources"
cd /
rm -frv /usr/src/fpc-1.9.6
rm -frv /usr/src/fpc
tar zxvf $WHERE/fpcsrc.tar.gz
fi

echo "Installing lazarus"
rm -frv /usr/share/lazarus
cp -arfv $WHERE/lazarus /usr/share
chown -R root:root /usr/share/lazarus
chmod -R 755 /usr/share/lazarus

COMPILE="ASK"
echo "CVS allows you to update your lazarus installation at any point to the latest version"
echo "Would you like to do this now [y/n]?"
read CVS
if [ "$CVS.x" == "y.x" ] ; then
	cd /usr/share
	cd /usr/share/lazarus
	cvs -z3 update
	COMPILE="y"
fi

if [ "$COMPILE" == "ASK" ] ; then
	echo "Would you like to recompile lazarus now [y/n]?"
	echo "This is optional"
	read $COMPILE
fi

if [ "$COMPILE" == "y" ] ; then
	cd /usr/share/lazarus
	make
fi

ln -s /usr/share/lazarus/lazarus /usr/bin/lazarus

echo "All done"

	
