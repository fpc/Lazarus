{
 *****************************************************************************
 *                                                                           *
 *  See the file COPYING.modifiedLGPL, included in this distribution,        *
 *  for details about the copyright.                                         *
 *                                                                           *
 *  This program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
 *                                                                           *
 *****************************************************************************

  Author: Mattias Gaertner

  Abstract:
   Provides LCL controls that access lists of properties of TPersistent objects
   via RTTI
   - the FreePascal Run Time Type Information.
}
unit RTTIGrids;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ObjectInspector, PropEdits, RTTICtrls, Grids;

type
  { TTICustomPropertyGrid }

  TTICustomPropertyGrid = class(TCustomPropertiesGrid)
  end;


  { TTIPropertyGrid }

  TTIPropertyGrid = class(TTICustomPropertyGrid)
  published
    property Align;
    property Anchors;
    property BackgroundColor;
    property BorderSpacing;
    property BorderStyle;
    property Constraints;
    property DefaultItemHeight;
    property DefaultValueFont;
    property Indent;
    property NameFont;
    property OnChangeBounds;
    property OnClick;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnModified;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnResize;
    property PopupMenu;
    property PrefferedSplitterX;
    property SplitterX;
    property Tabstop;
    property TIObject;
    property ValueFont;
    property Visible;
  end;
  

  TTIListDirection = (tldObjectsAsRows, tldObjectsAsColumns);
  TTIGridState = (tgsRebuildTIListNeeded);
  TTIGridStates = set of TTIGridState;
  
  { TTICustomGrid }
  
  TTICustomGrid =  class(TCustomGrid)
  private
    FListDirection: TTIListDirection;
    FListObject: TObject;
    FTIStates: TTIGridStates;
    procedure SetListDirection(const AValue: TTIListDirection);
    procedure SetListObject(const AValue: TObject);
  protected
    procedure ReloadTIList;
    procedure LoadCollection;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure Loaded; override;
    property ListObject: TObject read FListObject write SetListObject;
    property ListDirection: TTIListDirection read FListDirection write SetListDirection;
    property DefaultRowHeight default 20;
  end;
  
  
  { TTIGrid }
  
  TTIGrid = class(TTICustomGrid)
  published
    property Align;
    property Anchors;
    property AutoAdvance;
    property AutoFillColumns;
    property BorderSpacing;
    property BorderStyle;
    property Color;
    property Constraints;
    property DefaultDrawing;
    property DefaultRowHeight;
    property Enabled;
    property FixedColor;
    property Flat;
    property Font;
    property OnDblClick;
    property OnEditButtonClick;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnPrepareCanvas;
    property Options;
    property ParentColor;
    property ParentFont;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property TitleFont;
    property Visible;
  end;
  

procedure Register;


implementation


procedure Register;
begin
  RegisterComponents('RTTI',[TTIPropertyGrid,TTIGrid]);
end;

{ TTICustomStringGrid }

procedure TTICustomGrid.SetListDirection(const AValue: TTIListDirection);
begin
  if FListDirection=AValue then exit;
  FListDirection:=AValue;
  ReloadTIList;
end;

procedure TTICustomGrid.SetListObject(const AValue: TObject);
begin
  if FListObject=AValue then exit;
  FListObject:=AValue;
  ReloadTIList;
end;

procedure TTICustomGrid.ReloadTIList;
begin
  if ([csLoading,csDestroying]*ComponentState)<>[] then begin
    Include(FTIStates,tgsRebuildTIListNeeded);
    exit;
  end;
  Exclude(FTIStates,tgsRebuildTIListNeeded);
  if FListObject is TCollection then begin
    LoadCollection;
  end else begin
    // ListObject is not valid -> Clear
    Clear;
  end;
end;

procedure TTICustomGrid.LoadCollection;
var
  TheCollection: TCollection;
  ObjectCount: LongInt;
begin
  TheCollection:=FListObject as TCollection;
  ObjectCount:=TheCollection.Count;
  if ListDirection=tldObjectsAsRows then
    RowCount:=FixedRows+ObjectCount
  else
    ColCount:=FixedCols+ObjectCount;
    
end;

constructor TTICustomGrid.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
end;

destructor TTICustomGrid.Destroy;
begin
  inherited Destroy;
end;

procedure TTICustomGrid.Loaded;
begin
  inherited Loaded;
  ReloadTIList;
end;

initialization
  // property editor for TTICustomPropertyGrid.TIObject
  RegisterPropertyEditor(ClassTypeInfo(TPersistent),
    TTICustomPropertyGrid, 'TIObject', TTIObjectPropertyEditor);

end.

