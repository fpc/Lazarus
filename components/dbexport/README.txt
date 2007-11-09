Requirements: FPC 2.3.1 revision 9169

This is the Lazarus support for the Free Pascal DBExport units.
These units provide exports from any TDataset to various file formats.

The package registers on the tab 'Data Export' the following components:

TSCVExporter - Export to CSV (Comma Separated Values) file.

TFixedLengthExporter - Export to Fixed Length format file.

TSimpleXMLExporter - Export to XML file (no UTF-8, just ASCII)

TSimpleJSONExporter - Export to JSON file (no UTF-8, just ASCII)

TSQLExporter - Export data as a series of SQL insert/update statements.

TFPDBFEport - Export to a DBF file.

TDataExporter - Dialog component which allows to select a registered export
format, configures the export settings, and exports the data. Right-click on
the component and select 'Execute' from the component editor menu to try
this in the designer.

TStandardExports - Drop this on a form to register standard export types in
your application.
