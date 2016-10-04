--------------------------------------------------------------------------------
merged_cells
--------------------------------------------------------------------------------

In this sample project you'll find a derived stringgrid which allows to combine
adjacent cells to a larger block ("merge") easily.

For this purpose you must
- add goColSpanning to the grid's Options
- write an event handler for OnMergeCells to define the range of cells to be 
  merged - see the demo project.
  
In addition, this enhanced grid implements another event handler, OnDrawCellText,
which allows to hook into the text painting process for each cell. This way it
is relatively easy to draw word-wrapped text, vertical text, 
centered/right-aligned text, etc - again, see the demo project.
