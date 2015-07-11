DEPENDENCIES.

This package depends on PowerPDF version 0.9 which is available to download from
the Lazarus Code and Components Repository using the released package at:

https://sourceforge.net/project/showfiles.php?group_id=92177&package_id=294009

or using SVN by checking out powerpdf directory:

svn co https://lazarus-ccr.svn.sourceforge.net/svnroot/lazarus-ccr/components/powerpdf powerpdf


HOW TO USE.

Install package lazreportpdfexport.lpk as usually into Lazarus, an icon for exporter should appear in LazReport page on component palette.

Drop a component TFrReport onto a form
Drop a component TFrTFNPDFExport onto a form
Run the exporter by doing:

	frReport1.LoadFromFile('TheReportFile.lrf');
	if frReport1.PrepareReport then
	  frReport1.ExportTo(TFrTNPDFExportFilter, 'TheOutputPDFReport.pdf');


