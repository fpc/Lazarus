#!/bin/sh
#
# Supposes that wasm cross compiler is in scope. 
# The browser sub-target file (~/.fpc-browser.cfg) needs to contain at least the following 2 lines:
# -O-
# -CTwasmexceptions
#
#
exec ppcrosswasm32  -Twasip1  -tbrowser @wasm.cfg -ojcf.wasm jcf.lpr $*
