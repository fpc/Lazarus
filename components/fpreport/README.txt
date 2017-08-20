
Compiling:

- Run-time package: 
  lclfpreport.lpk
- Report Designer windows and functionality:
  design/lazfprepordesign.lpk
- FPreport component IDE registration and design-time support: 
  design/lazidefpreport.lpk
- Standalone designer: 
  design/reportdesign.lpr
- Demos: demos/fcldemo.lpr

Before compiling demos:

Copy the contents of the FPC fcl-report/demo directory to the demo
directory.

Check the various defines in udapp.pp and demos.inc:

EXPORTPDF - enable export to PDF
EXPORTFPIMAGE - enable export to image
EXPORTHTML - enable export to html

To enable the following defines, check the lcldemo project
EXPORTLCL - enable export to LCL (preview)

Running on windows:
If EXPORTFPIMAGE is defined, the freetype.dll is needed.

