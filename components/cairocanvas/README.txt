Package for use libcairo in fpc.

Author: Petr Kristan  (petr.kristan@epos.cz)

Licence: LGPL 

Base is TCairoPrinterCanvas class which implements TCanvas interface using 
cairo calls.

Their descendants TCairoPdfCanvas, TCairoPsCanvas, TCairoSvgCanvas, TCairoPngCanvas 
renders to file.

TCairoPaintBox can be used as TPaintBoxReplacement. 
One of Win32CairoCanvas or GdkCairoCanvas must be in uses clausule

To use TCairoPsCanvas as default Printer.Canvas set
Printer.CanvasClass := TCairoPsCanvas

Note.
Currently only TCairoPsCanvas has been tested and it's only functional
under linux/gtk2

