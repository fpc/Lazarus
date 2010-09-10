Is required that the following lazarus packages
are installed before running successfully this sample.

Except lazreport itself, All needed packages are included 
with lazarus in the <lazarus Install Path>/components 
subdirectory. In the following table this directory is 
denoted by {comp}.


Package			Package File
----------		----------------------------------
LazReport		../../source/lazreport.lpk
DBFLaz.			{comp}/tdbf/dbflaz.lpk
Printer4Lazarus		{comp}/printer/printer4lazarus.lpk

You can find instructions about installing components
in the Lazarus-CCR Knowledge-Base site at
http://lazarus-ccr.sourceforge.net/kb/index.php/Main_Page


INCLUDED SAMPLES.

1. Thumbnails report.

This sample demostrates how to use OnEnterRect TFrReport event
to print a thumbnails sheet, it finds all png images found
under lazarus/images/components directory. This report was done
following this steps.

* modifying the project

- The idea is to read the images/component directory of lazarus
  to get a list of png files, we use this list as a source of
  data to feed the report, as we don't have a TDataset component
  that does this we have to do it by hand by collecting the file
  and using a TFrUserDataset component on form (frUserDataset1),
  we setup this component so it counts from first to a number
  of times (RangeEnd=reCount) and we need to set RangeEndCount 
  property to the number of png files found. LazReport do the
  rest by emulating a dataset with RangeEndCound Records, then
  we need to handle TfrReport OnEnterRect event to print each
  record, in our case, we use a picture object to load for each
  "record" a different file.
  OnEnterRect is called for ALL printable objects on the report 
  so we need to find the moment the object we are interested is 
  being processed, in our case, as we will name the picture object
  as "thumbnail" this is used to select the right object.
  See TfrmMain.accThumbnailsExecute and TfrmMain.TheReportEnterRect
  procedure for details.
  
After the project has been modified, recompile and run the program.

* creating the report

- With the "New Report" button a new report is created.
- A new "Page header" band is added to report by clicking the "bands"
  icon on the left tool bar (the one above the picture button) and 
  then clicking the report designer.
- A Text object (memo) is added to report by clicking over the
  page header band, near the left border of printable page, in the
  Text Editor dialog that appear, "Thumbnails Report" was written. Then
  it was resized so it's right side touches the right border of printable
  page. Then font was made bold, and size is selected to 16 points, also
  a bottom border is added by clicking the bottom border button (the 
  third one from the left) on Rectangle toolbar.
- A new Text object is added over the right side of previous text object,
  in Text Editor dialog, press the "variable" button and from the list
  choose "Page#" by doubleclicking the item, next type "of " and next
  clicking again the "Variable" button we choose the "Total pages" item
  at the end in Text Editor dialog we should end with "[Page#] of [TotalPages]"
- Next we add a "Master Data Band" to report, from the datasource list
  that pops up, we choose frUserDataset1 as this is the right dataset
  for this report.
- Next using menu File->Page Options->Options we add 4 columns
- We add a Picture object to the report placing it over the "Master Data
  Band", with this object selected, press F11 to bring Object Inspector
  to front, and change name property to "thumbnail"
- As we use [TotalPages] internal variable we need to be sure to check
  that this report is a "two-pass" report by ticking the checkbox
  found in File->Report Options->Two-Pass report.
  
The resulting report can be found in thumbnails.lrf file.

2. Master-Detail sample.

The button "Master-Detail test" adds another dataset to project and
modify all dataset and controls layout and properties to create a 
master-detail relationship. 

This test use the masterdetail.lrf report file included in this directory.

In this project some aggregate functions are evaluated: COUNT and SUM. 
Note how COUNT function references the detail band name to count the number 
or records. The SUM function used to sum the result of the expression 
QTY*PRICE for each record.
