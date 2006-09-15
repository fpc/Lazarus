#!/bin/sh
cd $1
if [ -n "$FPCCfg" ]; then
	  MAKEOPTS=" -n @$FPCCfg"
  fi
  make
  make bigide OPT="$MAKEOPTS" USESVN2REVISIONINC=0
  make tools OPT="$MAKEOPTS"
  make lazbuilder OPT="$MAKEOPTS"
  # build gtk2 .ppu
  export LCL_PLATFORM=gtk2
  make lcl ideintf packager/registration bigidecomponents OPT="$MAKEOPTS"
  export LCL_PLATFORM=
strip lazarus
strip startlazarus
strip lazbuild
  
