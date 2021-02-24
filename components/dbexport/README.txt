Requirements: FPC 2.4+

This is the Lazarus support for the Free Pascal DBExport units.
These units provide exports from any TDataset to various file formats.

The package registers on the tab 'Data Export' the following components:

TSCVExporter - Export to CSV (Comma Separated Values) text file.

TFixedLengthExporter - Export to Fixed Length format text file.

TSimpleXMLExporter - Export to XML file (no UTF-8, just ASCII)

TSimpleJSONExporter - Export to JSON file (no UTF-8, just ASCII)

TSQLExporter - Export data as a series of SQL insert/update statements.

TFPDBFEport - Export to a DBF file.

TXMLXSDExporter - Export to various kinds of UTF-8 XML file formats (Microsoft 
Access, Microsoft Excel, ADO.NET, Delphi ClientDataset)

TDataExporter - Dialog component which allows one to select a registered export
format, configures the export settings, and exports the data. Right-click on
the component and select 'Execute' from the component editor menu to try
this in the designer.

TStandardExports - Drop this on a form to register standard export types in
your application.

--------------------------------------------------------------------------------

The following component palette icons (including equally named
files with appendix _150 and _200) were created by Roland Hahn.

These images can be used freely without any restrictions.

svg sources can be found on Lazarus Components and Code Repository, 
folder image_sources.
