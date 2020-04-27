Installation tools
==================

This document describes how to create the various packages for lazarus
including the required freepascal packages (fpc and fpc-src).

--------------------------------------------------------------------------------
TGZ

Creating the fpc-src tgz:

  There are two ways to do it and so there are two scripts. The first downloads
  the fpc svn and packs it. The second uses an existing fpc svn directory,
  copies it, remove unnecessary files and packs it. The second script is
  therefore much faster, but it may miss removing every old, unnecessary files.
  
  The download and pack script:
  The script create_fpc_export_tgz.sh makes a svn export of the fpc.

  For 2.1.x: ./create_fpc_export_tgz.sh devel 05/20/04

  The 05/20/04 means may 20th 2004.
  
  The copy and pack script:
  The script create_fpc_tgz_from_local_dir.sh needs as parameter the fpc cvs
  directory and compresses it to a tgz. For example
  ./create_fpc_tgz_from_local_dir.sh ~/pascal/fpc fpc-2.0.1.tgz


Creating the lazarus tgz:

  There are two ways to do it and so there are two scripts. The first downloads
  the fpc cvs and packs it. The second uses an existing fpc cvs directory,
  copies it, remove unnecessary files and packs it. The second script is
  therefore much faster, but it may miss removing all old, unnecessary files.

  The download and pack script:
  The script create_lazarus_export_tgz.sh makes a cvs export of lazarus and
  compress it. For example:
  ./create_lazarus_export_tgz.sh lazarus-20030316.tgz
  
  The copy and pack script:
  The script create_lazarus_tgz_from_local_dir.sh needs as parameter the
  lazarus cvs directory and compress it to lazarus-0.8.5.tgz.

--------------------------------------------------------------------------------

RPM:

How to create RPM packages as normal user / non root:

  You don't need to be root to built rpm packages.
  You only need a ~/.rpmacros file overriding some values. The script
  <lazarusdir>/tools/install/rpm/create_nonroot_macros.sh will setup one. Do:

    []$ cd <lazarusdir>/tools/install/rpm
    []$ ./create_nonroot_macros.sh

  From now all new rpms will be built in ~/rpm/.


Creating the fpc and the fpc-src rpm:

  There are two versions of the compiler: the old 2.0.x and the new (but not
  yet released) developer version 2.1.x. To build them, you must install fpc
  first. Install the stable fpc release (2.0.x).
  Note: The 2.0.x can not be built with a 2.1.x compiler and the recommended
  startup compiler for the 2.1.x is the released fpc 2.0.x.

  If you have your own fpc sources you can build the fpc rpm with:

  []$ cd <lazarus_directory>/tools/install
  []$ ./create_fpc_rpm.sh nodocs /path/to/your/fpc/sources/fpc

  Then build the fpc-src rpm:

  []$ ./create_fpc-src_rpm.sh /path/to/your/fpc/sources/fpc

  Normally you can find the rpms under /usr/src/redhat/RPMS/i386/
  and /usr/src/redhat/SRPMS/.
  Or if you have a ~/.rpmmacros file the new rpm will be in ~/rpm/RPMS/i386/
  and ~/rpm/SRPMS/.


Creating the lazarus rpm:

  Important:
  If you have created a new fpc RPM, then you should install it *before* you
  build the lazarus RPM. Otherwise the compiled lazarus RPM is not compatible
  to the fpc RPM.

  []$ ./create_lazarus_rpm.sh

CHM files:
  Download chm files and put them into docs/chm/
  Then run

  []$ ./create_lazarus_rpm.sh chmhelp


Creating other system specific rpms:

  rpm --rebuild lazarus-0.9.20.src.rpm


--------------------------------------------------------------------------------

Debian:

NOTE:
  These packages are maintained by the Lazarus team. Debian distribution
  maintains its version of packages separately here:
  https://salsa.debian.org/pascal-team/lazarus/tree/master/debian
  https://salsa.debian.org/pascal-team/fpc/tree/master/debian

Creating the debian fpc and fpc-src packages:

  There are two versions of the compiler: the stable 2.0.x and the unstable
  developer version 2.1.x. To build them, you must install fpc first. Install
  the stable fpc release (2.0.x). Note: The 2.0.x can not be built with a 2.1.x
  compiler.
  The create_fpc_deb.sh works nearly automatically. Become root and execute it.
  For 2.0.x: ./create_fpc_deb.sh stable
  For 2.1.x: ./create_fpc_deb.sh devel

  What it does: First it checks if the fpc source tgz is already created and
  if not, it automatically downloads it. The script does not use the current
  version, but the latest known to work with lazarus. Then it unpacks the
  source, compiles it and builds the debs. Normally you can find the debs under
  /usr/src/ and the current directory.


Creating the debian lazarus package:

  Important:
  If you have created new fpc debs, then you should install them, *before* you
  build the lazarus deb. Otherwise the compiled lazarus deb is not compatible
  to the fpc debs.

  []$ ./create_lazarus_deb.sh

CHM files:
  Download chm files and put them into docs/chm/
  Then run

  []$ ./create_lazarus_deb.sh chmhelp

--------------------------------------------------------------------------------

Mac OS X:

see macosx/README.txt
