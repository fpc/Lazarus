--------------------------------------------------------------------------------
attribute_grid
--------------------------------------------------------------------------------

This sample project implements a derived TStringGrid in which cell attributes
can be assigned simply by properties, rather than having to write code in the
OnPrepareCanvas event handler. 

Cell attributes are 
* font (name, size, style, color),
* background color,
* horizontal and vertical text alignment, 
* word-wrap.

Examples:
  StringGrid1.CellBkColor[3, 3] := clYellow;
  StringGrid1.CellAlignment[4, 4] := taRightJustify;
  StringGrid1.CellWordWrap[4, 4] := true;
  
To simplify usage this sub-classed StringGrid is contained in a separate unit,
AttrGrid, and keeps the old name of its ancestor, TStringGrid. When you want to
apply these new features in a project add unit AttrGrid to the "uses" clause of
the form containing a standard TStringGrid. But list it after unit Grids in the
"uses" clause, otherwise the stringgrid will not be created as an instance of the
new class.

Internal details
----------------
The TStringGrid (or TCustomDrawGrid, to be more precise) stores all its data in
an internal FGrid of type TVirtualGrid. Essentially this is a 2D pointer array,
and each pointer points to a TCellProps record:

type
  TCellProps=record
    Attr: pointer;
    Data: TObject;
    Text: pchar;
  end; 

While the Text and Data fields are used by the stringgrid itself for the Cells 
and Objects properties, the Attr field is available for additional features.

Unit AttrGrid declares a TCellAttr record containing various formatting 
properties which the user might want to change:

type
  TCellAttr = record
    Alignment: TAlignment;
    BkColor: TColor;
    FontColor: TColor;
    FontName: String;
    FontSize: Integer;
    FontStyle: TFontStyles;
    Layout: TTextlayout;
    Wordwrap: Boolean;
  end;
  PCellAttr = ^TCellAttr;
  
The pointer to this record is assigned to the Attr field of the TCellProps 
record.

The sub-classed TStringGrid introduces properties for these attributes:

type
  TStringGrid = class(Grids.TStringGrid)
  ...
    property CellAlignment[ACol, ARow: Integer]: TAlignment 
      read GetCellAlignment write SetCellAlignment;
    property CellAttr[ACol, ARow: Integer]: PCellAttr 
      read GetCellAttr write SetCellAttr;
    property CellBkColor[ACol, ARow: Integer]: TColor 
      read GetCellBkColor write SetCellBkColor;
    property CellFontColor[ACol, ARow: Integer]: TColor 
      read GetCellFontColor write SetCellFontColor;
    property CellFontName[ACol, ARow: Integer]: String 
      read GetCellFontName write SetCellFontName;
    property CellFontSize[ACol, ARow: Integer]: Integer 
      read GetCellFontSize write SetCellFontSize;
    property CellFontStyle[ACol, ARow: Integer]: TFontStyles 
      read GetCellFontStyle write SetCellFontStyle;
    property CellLayout[ACol, ARow: Integer]: TTextLayout 
      read GetCellLayout write SetCellLayout;
    property CellWordWrap[ACol, ARow: Integer]: Boolean 
      read GetCellWordWrap write SetCellWordWrap;
  end;
  
For accessing these properties FGrid.Celda[ACol, ARow] is invoked to find the
TCellProps record of the cell at column ACol and row ARow in the virtual grid, 
FGrid.

When writing to the property and no CellProps records is found a new record
is allocated; similarly, when a cell props record is found, but with Attr = nil 
a new TCellAttr record is allocated and linked to the Attr field.
All this is done in the NewCellAttr function of the modified TStringGrid.

It is required to implement a descendant of TVirtualGrid, TCellAttrVirtualGrid,
for disposing the  memory that was additionally allocated for the Attr fields. 
To make sure that TStringGrid really creates this new virtualgrid class rather 
than the default TVirtualGrid, the TStringGrid method CreateVirtualGrid is 
overridden.

For application of the cell attributes in painting the TStringGrid method
PrepareCanvas is extended: this method is called immediately before a specific
cell is drawn and must set up the canvas for this cell. If the CellProps record 
of the cell has a non-nil Attr field, the attributes are extracted and applied 
to the canvas.
