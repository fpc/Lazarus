The 2 directories below contain demonstration programs for the 
export components.

The simple directory contains a program which demonstrates how
to work with the export components directly. 

Both programs work on a DBase data file. The File|New menu
can be used to create a new data file (it is saved at once,
a filename is asked) or the File|Open can be used to load
(any) Dbase file.

You can configure the components as you wish, at runtime the 
design-time settings will be used. (unless the user changes them
with the dialog)

When the 'show config dialog' menu option is set in the program,
exporting will first show the configuration dialog, so the
export can be configured at runtime by the user. This is a generic dialog,
so it may not be the summit of ease-of-use.

The second (stdexports) program demonstrates how the automatic
registration routines (TStandardExportFormats component) work in 
conjunction with the TFPDataExporter component. The former controls
which export formats are registered (and hence visible to the end 
user), the second shows a selection and configuration dialog.

The set of registered formats can be set at design time, only the
selected formats will be available to the end user.

Note that currently, only data that can be represented in text format
can be exported, i.e., blobs will not work (unless they contain text).
