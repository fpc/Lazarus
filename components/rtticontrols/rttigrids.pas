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
  Classes, SysUtils, LCLProc, ObjectInspector, PropEdits, TypInfo, RTTICtrls,
  Grids;

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
  TTIGridState = (
    tgsRebuildTIListNeeded,
    tgsDefaultDrawing // set during default drawing
    );
  TTIGridStates = set of TTIGridState;
  TTICustomGrid =  class;
  
  { TTIGridProperty }

  TTIGridProperty = class
  private
    FEditor: TPropertyEditor;
    FGrid: TTICustomGrid;
    FIndex: integer;
    FTitle: string;
    procedure SetTitle(const AValue: string);
  public
    constructor Create(TheGrid: TTICustomGrid; TheEditor: TPropertyEditor;
      TheIndex: integer);
    function PropInfo: PPropInfo;
  public
    property Editor: TPropertyEditor read FEditor;
    property Grid: TTICustomGrid read FGrid;
    property Index: integer read FIndex;
    property Title: string read FTitle write SetTitle;
  end;
  
  { TTICustomGrid }
  
  TTICustomGrid =  class(TCustomGrid)
  private
    FFilter: TTypeKinds;
    FListDirection: TTIListDirection;
    FListObject: TObject;
    FOnHeaderClick: THdrEvent;
    FOnHeaderSized: THdrEvent;
    FPropertyEditorHook: TPropertyEditorHook;
    FSaveOnChangeTIObject: boolean;
    FTIStates: TTIGridStates;
    FTIObjectCount: integer;
    FProperties: TList;
    function GetProperties(Index: integer): TTIGridProperty;
    function GetPropertyCount: integer;
    procedure SetFilter(const AValue: TTypeKinds);
    procedure SetListDirection(const AValue: TTIListDirection);
    procedure SetListObject(const AValue: TObject);
  protected
    procedure ReloadTIList;
    procedure LoadCollection;
    procedure AddHeaderPropertyEditor(Prop: TPropertyEditor);
    procedure DrawCell(aCol, aRow: Integer; aRect: TRect;
                       aState: TGridDrawState); override;
    procedure CalcCellExtent(aCol, aRow: Integer; var aRect: TRect); virtual;
    procedure HeaderClick(IsColumn: Boolean; index: Integer); override;
    procedure HeaderSized(IsColumn: Boolean; index: Integer); override;
    procedure GetAutoFillColumnInfo(const Index: Integer;
                                    var aMin,aMax,aPriority: Integer); override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure Loaded; override;
    procedure SaveChanges; virtual;
    procedure ClearProperties;
    procedure DefaultDrawCell(aCol, aRow: Integer; var aRect: TRect;
                              aState: TGridDrawState); virtual;
    procedure GetCellEditor(aCol, aRow: integer;
                            var PropEditor: TPropertyEditor;
                            var IndependentEditor: boolean);
    procedure FreeCellEditor(PropEditor: TPropertyEditor);
    function GridStateToPropEditState(GridState: TGridDrawState
                                      ): TPropEditDrawState;
    function GetTIObject(Index: integer): TPersistent;
  public
    property ListObject: TObject read FListObject write SetListObject;
    property ListDirection: TTIListDirection read FListDirection
                    write SetListDirection default tldObjectsAsRows;
    property DefaultRowHeight default 20;
    property SaveOnChangeTIObject: boolean read FSaveOnChangeTIObject
                                           write FSaveOnChangeTIObject
                                           default true;
    property Filter: TTypeKinds read FFilter write SetFilter default AllTypeKinds;
    property PropertyEditorHook: TPropertyEditorHook read FPropertyEditorHook;
    property TIObjectCount: integer read FTIObjectCount;
    property PropertyCount: integer read GetPropertyCount;
    property Properties[Index: integer]: TTIGridProperty read GetProperties;
    property OnHeaderClick: THdrEvent read FOnHeaderClick write FOnHeaderClick;
    property OnHeaderSized: THdrEvent read FOnHeaderSized write FOnHeaderSized;
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
    property FixedCols;
    property FixedRows;
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
  RegisterComponents('RTTI',[TTIPropertyGrid]);
end;

{ TTICustomStringGrid }

procedure TTICustomGrid.SetListDirection(const AValue: TTIListDirection);
begin
  if FListDirection=AValue then exit;
  FListDirection:=AValue;
  ReloadTIList;
end;

function TTICustomGrid.GetPropertyCount: integer;
begin
  Result:=FProperties.Count;
end;

function TTICustomGrid.GetProperties(Index: integer): TTIGridProperty;
begin
  Result:=TTIGridProperty(FProperties[Index]);
end;

procedure TTICustomGrid.SetFilter(const AValue: TTypeKinds);
begin
  if FFilter=AValue then exit;
  FFilter:=AValue;
  ReloadTIList;
end;

procedure TTICustomGrid.SetListObject(const AValue: TObject);
begin
  if FListObject=AValue then exit;
  FListObject:=AValue;
  if SaveOnChangeTIObject then
    SaveChanges;
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
  CurItem: TCollectionItem;
  HeaderLines: LongInt;
  PropCount: LongInt;
begin
  TheCollection:=FListObject as TCollection;
  FTIObjectCount:=TheCollection.Count;
  if ListDirection=tldObjectsAsRows then begin
    HeaderLines:=FixedRows;
    RowCount:=HeaderLines+FTIObjectCount
  end else begin
    HeaderLines:=FixedCols;
    ColCount:=HeaderLines+FTIObjectCount;
  end;
  // get first object to create the grid header
  if FTIObjectCount=0 then exit;
  CurItem:=TheCollection.Items[0];
  if not (CurItem is TPersistent) then begin
    debugln('TTICustomGrid.LoadCollection First CollectionItem=',dbgsName(CurItem));
    exit;
  end;
  // get header properties
  FPropertyEditorHook.LookupRoot:=CurItem;
  ClearProperties;
  GetPersistentProperties(CurItem, FFilter, FPropertyEditorHook,
    @AddHeaderPropertyEditor,nil);
  PropCount:=PropertyCount;
  if ListDirection=tldObjectsAsRows then begin
    ColCount:=FixedCols+PropCount;
  end else begin
    RowCount:=FixedRows+PropCount;
  end;
end;

procedure TTICustomGrid.AddHeaderPropertyEditor(Prop: TPropertyEditor);
var
  NewProperty: TTIGridProperty;
begin
  NewProperty:=TTIGridProperty.Create(Self,Prop,FProperties.Count);
  FProperties.Add(NewProperty);
end;

procedure TTICustomGrid.DrawCell(aCol, aRow: Integer; aRect: TRect;
  aState: TGridDrawState);
begin
  if Assigned(OnDrawCell) and not (csDesigning in ComponentState) then begin
    PrepareCanvas(aCol, aRow, aState);
    if DefaultDrawing then
      Canvas.FillRect(aRect);
    OnDrawCell(Self,aCol,aRow,aRect,aState)
  end else
    DefaultDrawCell(aCol,aRow,aRect,aState);
  DrawCellGrid(aCol,aRow,aRect,aState);
end;

procedure TTICustomGrid.CalcCellExtent(aCol, aRow: Integer; var aRect: TRect);
begin
  if (aCol=0) and (aRow=0) and (ARect.Left=0) then ;
  //
end;

procedure TTICustomGrid.HeaderClick(IsColumn: Boolean; index: Integer);
begin
  inherited HeaderClick(IsColumn, index);
  if Assigned(OnHeaderClick) then OnHeaderClick(Self, IsColumn, index);
end;

procedure TTICustomGrid.HeaderSized(IsColumn: Boolean; index: Integer);
begin
  inherited HeaderSized(IsColumn, index);
  if Assigned(OnHeaderSized) then OnHeaderSized(Self, IsColumn, index);
end;

procedure TTICustomGrid.GetAutoFillColumnInfo(const Index: Integer; var aMin,
  aMax, aPriority: Integer);
begin
  if (aMin=0) and (aMax=0) then ;
  if (Index<FixedCols) then
    aPriority := 0
  else
    aPriority := 1;
end;

constructor TTICustomGrid.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  FPropertyEditorHook:=TPropertyEditorHook.Create;
  FFilter:=[{tkUnknown,}tkInteger,tkChar,tkEnumeration,
            tkFloat,{tkSet,tkMethod,}tkSString,tkLString,tkAString,
            tkWString,tkVariant,{tkArray,tkRecord,tkInterface,}
            {tkClass,tkObject,}tkWChar,tkBool,tkInt64,
            tkQWord{,tkDynArray,tkInterfaceRaw}];
  FProperties:=TList.Create;
  FListDirection:=tldObjectsAsRows;
end;

destructor TTICustomGrid.Destroy;
begin
  ClearProperties;
  FreeThenNil(FProperties);
  FreeThenNil(FPropertyEditorHook);
  inherited Destroy;
end;

procedure TTICustomGrid.Loaded;
begin
  inherited Loaded;
  ReloadTIList;
end;

procedure TTICustomGrid.SaveChanges;
begin
  // TODO
end;

procedure TTICustomGrid.ClearProperties;
var
  i: Integer;
begin
  if FProperties=nil then exit;
  for i:=0 to FProperties.Count-1 do TObject(FProperties[i]).Free;
end;

procedure TTICustomGrid.DefaultDrawCell(aCol, aRow: Integer; var aRect: TRect;
  aState: TGridDrawState);
var
  OldDefaultDrawing: boolean;
  PropEditor: TPropertyEditor;
  IndependentEditor: boolean;
begin
  OldDefaultDrawing:=tgsDefaultDrawing in FTIStates;
  Include(FTIStates,tgsDefaultDrawing);
  try
    PrepareCanvas(aCol, aRow, aState);
  finally
    if OldDefaultDrawing then
      Include(FTIStates,tgsDefaultDrawing)
    else
      Exclude(FTIStates,tgsDefaultDrawing);
  end;
  if goColSpanning in Options then CalcCellExtent(acol, arow, aRect);
  debugln('TTICustomGrid.DefaultDrawCell A Col=',dbgs(aCol),' Row=',dbgs(aRow));
  // fetch a property editor and draw cell
  PropEditor:=nil;
  GetCellEditor(aCol,aRow,PropEditor,IndependentEditor);
  if PropEditor<>nil then begin
    debugln('TTICustomGrid.DefaultDrawCell B ',dbgsName(PropEditor),' ',PropEditor.GetName,' ',PropEditor.GetValue);
    try
      if gdFixed in aState then
        PropEditor.PropDrawName(Canvas,aRect,GridStateToPropEditState(aState))
      else
        PropEditor.PropDrawValue(Canvas,aRect,GridStateToPropEditState(aState));
    finally
      if IndependentEditor then PropEditor.Free;
    end;
  end;
end;

procedure TTICustomGrid.GetCellEditor(aCol, aRow: integer;
  var PropEditor: TPropertyEditor; var IndependentEditor: boolean);
var
  ObjectIndex: Integer;
  PropertyIndex: Integer;
  EditorClass: TPropertyEditorClass;
  Hook: TPropertyEditorHook;
  GridProperty: TTIGridProperty;
  PersistentList: TPersistentSelectionList;
  ok: Boolean;
  CurObject: TPersistent;
begin
  PropEditor:=nil;
  IndependentEditor:=true;
  if ListDirection=tldObjectsAsRows then begin
    ObjectIndex:=aRow-FixedRows;
    PropertyIndex:=aCol-FixedCols;
  end else begin
    ObjectIndex:=aCol-FixedCols;
    PropertyIndex:=aRow-FixedRows;
  end;
  if (PropertyIndex>=0) and (PropertyIndex<PropertyCount)
  and (ObjectIndex>=0) and (ObjectIndex<TIObjectCount) then begin
    CurObject:=GetTIObject(ObjectIndex);
    if CurObject<>nil then begin
      ok:=false;
      Hook:=nil;
      PersistentList:=nil;
      try
        Hook:=TPropertyEditorHook.Create;
        Hook.LookupRoot:=CurObject;
        PersistentList:=TPersistentSelectionList.Create;
        PersistentList.Add(CurObject);
        GridProperty:=Properties[PropertyIndex];
        EditorClass:=TPropertyEditorClass(GridProperty.Editor.ClassType);
        PropEditor:=EditorClass.Create(Hook,PersistentList,1);
        PropEditor.SetPropEntry(0,CurObject,GridProperty.PropInfo);
        PropEditor.Initialize;
        ok:=true;
      finally
        if not ok then begin
          try
            PropEditor.free;
          except
          end;
          try
            PersistentList.free;
          except
          end;
          try
            Hook.free;
          except
          end;
        end;
      end;
    end;
  end;
end;

procedure TTICustomGrid.FreeCellEditor(PropEditor: TPropertyEditor);
var
  Hook: TPropertyEditorHook;
  PersistentList: TPersistentSelectionList;
begin
  if PropEditor=nil then exit;
  Hook:=PropEditor.PropertyHook;
  PersistentList:=PropEditor.ComponentList;
  try
    PropEditor.free;
  except
  end;
  try
    PersistentList.free;
  except
  end;
  try
    Hook.free;
  except
  end;
end;

function TTICustomGrid.GridStateToPropEditState(GridState: TGridDrawState
  ): TPropEditDrawState;
begin
  Result:=[];
  if gdSelected in GridState then Include(Result,pedsSelected);
  if gdFocused in GridState then Include(Result,pedsFocused);
end;

function TTICustomGrid.GetTIObject(Index: integer): TPersistent;
begin
  Result:=nil;
  if (Index<0) or (Index>=TIObjectCount) then exit;
  if ListObject is TCollection then begin
    Result:=TCollection(ListObject).Items[Index];
  end;
end;

{ TTIGridProperty }

procedure TTIGridProperty.SetTitle(const AValue: string);
begin
  if FTitle=AValue then exit;
  FTitle:=AValue;
end;

constructor TTIGridProperty.Create(TheGrid: TTICustomGrid;
  TheEditor: TPropertyEditor; TheIndex: integer);
begin
  FGrid:=TheGrid;
  FEditor:=TheEditor;
  FIndex:=TheIndex;
  FTitle:=TheEditor.GetName;
end;

function TTIGridProperty.PropInfo: PPropInfo;
begin
  Result:=Editor.GetPropInfo;
end;

initialization
  // property editor for TTICustomPropertyGrid.TIObject
  RegisterPropertyEditor(ClassTypeInfo(TPersistent),
    TTICustomPropertyGrid, 'TIObject', TTIObjectPropertyEditor);

end.

