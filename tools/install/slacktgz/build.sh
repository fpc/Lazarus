#!/bin/sh
cd $1
if [ -n "$FPCCfg" ]; then
	  MAKEOPTS=" -n @$FPCCfg"
  fi
  make bigide OPT="$MAKEOPTS" USESVN2REVISIONINC=0
strip lazarus
strip startlazarus
strip lazbuild
  
