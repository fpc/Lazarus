In order to be able to run this test-case please follow the steps below.

Rename or copy the files:
 fpclist.txt.sample  to  fpclist.txt
 gdblist.txt.sample  to  gdblist.txt

Edit each file according to the comment in it. You must have at least one section in each file.

* In both files, each section must have the following line:
symbols=gw,gwset

and a line like
version=020604

where the number is VVmmss
VV Major version (with leading zero if required)
mm Minor version (with leading zero if required)
ss Sub-version (with leading zero if required)

e.g.
For fpc 2.6.4: 020604
For gdb 7.5.1: 070501


* In fpclist.txt you must specify your fpc.exe. You may additionally add lines with options ("opts=").
You may want to create sections for each of the following lines (-Xe is only for windows)
opts=-O1
opts=-O-
opts=-Xe
opts=-Xe -O1
opts=-Xe -O-

Do not specify any "-g..." options.

* In gdblist.txt  you must specify where your gdb is.
You can specify several location each in one section, if you have more than one gdb.
However this is not needed, one gdb should be fine.

---------------------

You also may want to create a folder "logs" in the test dir.

On none windows it may be necessary to run the test with "Always create logs" checked in the 2nd form.


Should the previous run have crashed, it may be needed to delete all exe, in the TestApps/lib folder

