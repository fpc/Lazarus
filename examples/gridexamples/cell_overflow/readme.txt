--------------------------------------------------------------------------------
cell_overflow
--------------------------------------------------------------------------------

This sample project implements overflowing text in a StringGrid descendant. This
means that text which is longer than the width of the column is not truncated
at the cell border, but is allowed to flow into adjacent empty cells.

The code is based on a forum contribution by user Geepster 
(http://forum.lazarus.freepascal.org/index.php/topic,35869.msg238079.html#msg238079)

It was extended to correctly handle left, right and centered text alignments.
