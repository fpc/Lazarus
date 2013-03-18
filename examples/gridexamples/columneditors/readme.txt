Readme.txt

There are seven different kinds of column, and they have different events for
control. Mastering them requires some study.

This example was started because I did not understand how to make a checkbox
column work in a TStringGrid. I also needed to have a sticky output, so that the
form could be destroyed leaving the results. All results are therefore passed to
an array as they are generated. The types of each field in the array are
appropriate to this program. You may choose other types for the array fields,
and change your code accordingly.

The basic functionality of each of the seven kinds of column plus a basic (Auto)
column with an EditMask are demonstrated. The Editmask constrains the input
to between 00:00:00 and 99:99:99 but cannot limit it to 23:59:59. The
onValidateEntry event is used to do this.

Other buttons change cell colours, show grid position, or add results from a
calculator. Some are changed manually and others programmatically.

The cbsButton can call the DrawCell event and change the colour immediately
the ColorDialog closes. The cbsEllipsis can only call the DrawCell event when
focus moves to another cell.
In Grids.pas it can be seen that cbsEllipsis calls TButtonCellEditor, but
cbsButton calls both TButtonCellEditor and TStringEditor.
Changing the ButtonStyle of Column 'Button' from cbsButton to cbsEllipsis will
demonstrate this.

If the cell focus is changed to the next column in code, this is not noticeable.
Example:
With StringGrid1 do
  If (Col < ColCount-1) then
    Col := Col + 1;

The code is commented, but also read
http://wiki.freepascal.org/Grids_Reference_Page.

Windsurfer 2013
