
Compiling:

Using FPReport requires FPC trunk (or 3.2). 

However, it can be compiled with FPC 3.0.4 by doing the following first:

- Make a directory "src" (the packages will assume this directory) here.
- To this directory, copy from FPC trunk the following files:
  - all files from packages/fcl-report/src
  - all files from packages/fcl-pdf/src
  - packages/fcl-image/src/fpbarcode.pp
  - packages/fcl-image/src/fpimgbarcode.pp
  - packages/fcl-image/src/fpqrcodegen.pp
  - packages/fcl-image/src/fpimgqrcode.pp

Then, the following packages can be compiled:

- Run-time package: 
  lclfpreport.lpk

- FPreport component IDE registration and design-time support: 
  design/lazidefpreport.lpk

- Report Designer windows and functionality:
  design/lazfprepordesign.lpk

- Standalone designer: 
  design/reportdesign.lpr

- Demos: demos/fcldemo.lpr


Before compiling demos:

Copy the contents of the FPC packages/fcl-report/demo directory to the demo directory.

Check the various defines in udapp.pp and demos.inc:

EXPORTPDF - enable export to PDF
EXPORTFPIMAGE - enable export to image
EXPORTHTML - enable export to html

To enable the following defines, check the lcldemo project
EXPORTLCL - enable export to LCL (preview)

Running on windows:
If EXPORTFPIMAGE is defined, the freetype.dll is needed.

