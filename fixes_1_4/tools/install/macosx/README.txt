How to create the lazarus release dmg:
Download and install iceberg from
  http://s.sudre.free.fr/Software/Iceberg.html

=== Creating the dmg ======================================

cd tools/install/macosx
./create_lazarus_dmg.sh


=== Editing the Iceberg file ==============================
cd tools/install/macosx
./create_lazarus_dmg.sh edit

This will create ~/tmp/buildlaz/lazarus.packproj. Open the file with Iceberg.
When changed, save the file. Copy it back to the lazarus sources:
cp ~/tmp/buildlaz/lazarus.packproj <lazarus>/tools/install/macosx/lazarus_release.packproj.template

The script will replace the following:
  _LAZVERSION_ with $LazVersion
  _DATESTAMP_ with $DATESTAMP
  18273645 with $LAZMAJORVERSION
  45362718 with $LAZMINORVERSION

