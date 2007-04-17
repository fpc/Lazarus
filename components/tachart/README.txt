TAChart is a charting LGPL component for Lazarus (TeeChart like). 
It contains the functions that Philippe Martinole developed for the TeleAuto project and lots 
of changes introduced by Luis Rodrigues while porting the Epanet application from Delphi to Lazarus.


Its main caracteristics are :

    * Pie Series
    * Bar Series
    * Area Series
    * Line Series (can work as Point Series)    
    * Unlimited number of  graphs
    * Unlimited number of points
    * Graph legend
    * Graph title
    * Graph footer
    * Axis labels
    * Interactive zoom
    * Reticule or vertical reticule with point measure
    * Mirror on X axis
    * Auto or manual graph limits
    * Smart marks drawing
    * Vertical and horizontal line graph type
    * Easily extensible to new graph types


Why Create This
---------------

I've been porting Epanet from Delphi into Lazarus, since I need a TeeChart like component and TeeChart was not free
I had to build a new component.

I looked arround the net for a good component and found Philippe's TAChart and decided to improve it.

There is still plenty of bugs on the component but at least it works with all the cases I need. I will continue to develop
this, but since it's allready working I decided to submit to general review.
    
Installation
------------
    * Open the package tachartlazaruspkg.lpk with Component / Open package file (.lpk)
    * Click on Compile
    * Click on Install

Changes
-------
* 17/04/2007 - Series clipping bugfixes and partially implement TSeriesPointer
* 12/03/2007 - Bugfixes and implement ShowInLegend
* 01/01/2007 - Initial Release


Copyright (C) 2005-2006 by Philippe Martinole  <philippe.martinole@teleauto.org>
Copyright (C) 2006-2007 by Luis Rodrigues  <lr@neei.uevora.pt>
