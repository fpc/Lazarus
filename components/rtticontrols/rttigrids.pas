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
  Classes, SysUtils, Controls, LCLProc, LCLType, ObjectInspector, PropEdits,
  GraphPropEdits, TypInfo, RTTICtrls, Grids;

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
  TTIGridCellType = (
    tgctNone,       // out or undefined
    tgctValue,      // a normal property cell
    tgctPropName,   // header cell for property name
    tgctPropNameAlt,// header cell for alternative prop name (e.g. FixedRows>1)
    tgctObjectName, // header cell for object name
    tgctObjectNameAlt,// header cell for alternative obj name (e.g. FixedCols>1)
    tgctCorner      // corner cell left, top of grid
    );
  TTIGridOption = (
    tgoStartIndexAtOne // start shown object index at 1
    );
  TTIGridOptions = set of TTIGridOption;
  TTICustomGrid =  class;

  { TTIGridProperty }

  TTIGridProperty = class
  private
    FEditor: TPropertyEditor;
    FEditorControl: TWinControl;
    FGrid: TTICustomGrid;
    FIndex: integer;
    FTitle: string;
    procedure SetTitle(const AValue: string);
    procedure EditorControlKeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
  public
    constructor Create(TheGrid: TTICustomGrid; TheEditor: TPropertyEditor;
                       TheIndex: integer);
    function PropInfo: PPropInfo;
    function GetEditorControl: TWinControl;
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
    FHeaderPropHook: TPropertyEditorHook;
    FTIOptions: TTIGridOptions;
    FTIStates: TTIGridStates;
    FTIObjectCount: integer;
    FProperties: TList;
    function GetProperties(Index: integer): TTIGridProperty;
    function GetPropertyCount: integer;
    procedure SetFilter(const AValue: TTypeKinds);
    procedure SetListDirection(const AValue: TTIListDirection);
    procedure SetListObject(const AValue: TObject);
    procedure SetTIOptions(const NewOptions: TTIGridOptions);
  protected
    procedure LoadCollection;
    procedure LoadTList;
    procedure RebuildGridLayout; virtual;
    procedure AddHeaderPropertyEditor(Prop: TPropertyEditor);
    procedure DrawCell(aCol, aRow: Integer; aRect: TRect;
                       aState: TGridDrawState); override;
    procedure CalcCellExtent(aCol, aRow: Integer; var aRect: TRect); virtual;
    procedure HeaderClick(IsColumn: Boolean; index: Integer); override;
    procedure HeaderSized(IsColumn: Boolean; index: Integer); override;
    procedure GetAutoFillColumnInfo(const Index: Integer;
                                    var aMin,aMax,aPriority: Integer); override;
    procedure SelectEditor; override;
    procedure DoEditorControlKeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState); virtual;
    procedure EditorHide; override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure Loaded; override;
    procedure ReloadTIList;
    procedure ClearProperties;
    procedure DefaultDrawCell(aCol, aRow: Integer; var aRect: TRect;
                              aState: TGridDrawState); virtual;
    procedure DrawObjectName(Index: integer; aRect: TRect;
                             aState: TGridDrawState);
    procedure GetCellEditor(aCol, aRow: integer;
                            var PropEditor: TPropertyEditor;
                            var IndependentEditor: boolean);
    procedure FreeCellEditor(PropEditor: TPropertyEditor);
    function GridStateToPropEditState(GridState: TGridDrawState
                                      ): TPropEditDrawState;
    function GetTIObject(Index: integer): TPersistent;
    procedure MapCell(aCol, aRow: integer;
                      var ObjectIndex, PropertyIndex: integer;
                      var CellType: TTIGridCellType);
    function GetCurrentGridProperty: TTIGridProperty;
  public
    property DefaultRowHeight default 20;
    property Filter: TTypeKinds read FFilter write SetFilter default AllTypeKinds;
    property ListDirection: TTIListDirection read FListDirection
                    write SetListDirection default tldObjectsAsRows;
    property ListObject: TObject read FListObject write SetListObject;
    property OnHeaderClick: THdrEvent read FOnHeaderClick write FOnHeaderClick;
    property OnHeaderSized: THdrEvent read FOnHeaderSized write FOnHeaderSized;
    property Properties[Index: integer]: TTIGridProperty read GetProperties;
    property PropertyCount: integer read GetPropertyCount;
    property TIObjectCount: integer read FTIObjectCount;
    property TIOptions: TTIGridOptions read FTIOptions write SetTIOptions;
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
    property Filter;
    property FixedColor;
    property FixedCols;
    property FixedRows;
    property Flat;
    property Font;
    property ListDirection;
    property OnDblClick;
    property OnEditButtonClick;
    property OnEnter;
    property OnExit;
    property OnHeaderClick;
    property OnHeaderSized;
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
    property TIOptions;
    property TitleFont;
    property Visible;
  end;
  
  
  { TRegisteredTIGridControl }

  TRegisteredTIGridControl = class
  private
    FPropEditorClass: TPropertyEditorClass;
    FWinControlClass: TWinControlClass;
  public
    property PropEditorClass: TPropertyEditorClass read FPropEditorClass
                                                   write FPropEditorClass;
    property WinControlClass: TWinControlClass read FWinControlClass
                                               write FWinControlClass;
  end;


procedure RegisterTIGridControl(PropEditorClass: TPropertyEditorClass;
  WinControlClass: TWinControlClass);
function FindTIGridControl(PropEditorClass: TPropertyEditorClass
  ): TWinControlClass;

procedure Register;


implementation

var
  RegisteredTIGridControls: TList;

procedure RegisterTIGridControl(PropEditorClass: TPropertyEditorClass;
  WinControlClass: TWinControlClass);
var
  NewItem: TRegisteredTIGridControl;
begin
  if (PropEditorClass=nil) or (WinControlClass=nil) then exit;
  if RegisteredTIGridControls=nil then RegisteredTIGridControls:=TList.Create;
  NewItem:=TRegisteredTIGridControl.Create;
  if NewItem=nil then ;
  NewItem.PropEditorClass:=PropEditorClass;
  NewItem.WinControlClass:=WinControlClass;
  RegisteredTIGridControls.Add(NewItem);
end;

function FindTIGridControl(PropEditorClass: TPropertyEditorClass
  ): TWinControlClass;
var
  BestItem: TRegisteredTIGridControl;
  i: Integer;
  CurItem: TRegisteredTIGridControl;
begin
  Result:=nil;
  if RegisteredTIGridControls=nil then exit;
  BestItem:=nil;
  for i:=0 to RegisteredTIGridControls.Count-1 do begin
    CurItem:=TRegisteredTIGridControl(RegisteredTIGridControls[i]);
    debugln('FindTIGridControl PropEditorClass=',PropEditorClass.ClassName,
      ' CurItem.PropEditorClass=',CurItem.PropEditorClass.ClassName,
      ' CurItem.WinControlClass=',CurItem.WinControlClass.ClassName,
      ' Candidate=',dbgs(PropEditorClass.InheritsFrom(CurItem.PropEditorClass))
      );
    if PropEditorClass.InheritsFrom(CurItem.PropEditorClass)
    and ((BestItem=nil)
         or (CurItem.PropEditorClass.InheritsFrom(BestItem.PropEditorClass)))
    then begin
      BestItem:=CurItem;
    end;
  end;
  if BestItem<>nil then
    Result:=BestItem.WinControlClass;
end;

procedure FinalizeTIGrids;
var
  i: Integer;
begin
  if RegisteredTIGridControls=nil then exit;
  for i:=0 to RegisteredTIGridControls.Count-1 do
    TObject(RegisteredTIGridControls[i]).Free;
  RegisteredTIGridControls.Free;
end;

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
  ReloadTIList;
end;

procedure TTICustomGrid.SetTIOptions(const NewOptions: TTIGridOptions);
var
  ChangedOptions: TTIGridOptions;
begin
  if FTIOptions=NewOptions then exit;
  ChangedOptions:=(FTIOptions-NewOptions)+(NewOptions-FTIOptions);
  FTIOptions:=NewOptions;
  if tgoStartIndexAtOne in ChangedOptions then Invalidate;
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
  end else if FListObject is TList then begin
    LoadTList;
  end else begin
    // ListObject is not valid
  end;
end;

procedure TTICustomGrid.LoadCollection;
var
  TheCollection: TCollection;
begin
  TheCollection:=FListObject as TCollection;
  FTIObjectCount:=TheCollection.Count;
  RebuildGridLayout;
end;

procedure TTICustomGrid.LoadTList;
var
  TheList: TList;
begin
  TheList:=FListObject as TList;
  FTIObjectCount:=TheList.Count;
  RebuildGridLayout;
end;

procedure TTICustomGrid.RebuildGridLayout;
var
  CurItem: TPersistent;
  HeaderLines: LongInt;
  PropCount: LongInt;
begin
  if ListDirection=tldObjectsAsRows then begin
    HeaderLines:=FixedRows;
    RowCount:=HeaderLines+FTIObjectCount
  end else begin
    HeaderLines:=FixedCols;
    ColCount:=HeaderLines+FTIObjectCount;
  end;
  // get first object to create the grid header
  if FTIObjectCount=0 then exit;
  CurItem:=GetTIObject(0);
  if not (CurItem is TPersistent) then begin
    debugln('TTICustomGrid.LoadCollection First CollectionItem=',dbgsName(CurItem));
    exit;
  end;
  // get header properties
  FHeaderPropHook.LookupRoot:=CurItem;
  ClearProperties;
  GetPersistentProperties(CurItem, FFilter, FHeaderPropHook,
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

procedure TTICustomGrid.SelectEditor;
var
  NewEditor: TWinControl;
  ObjectIndex: integer;
  PropertyIndex: integer;
  CellType: TTIGridCellType;
  NewRect: TRect;
  PropLink: TCustomPropertyLink;
  CurObject: TPersistent;
  CurProp: TTIGridProperty;
  PropName: String;
begin
  NewEditor:=nil;
  MapCell(Col,Row,ObjectIndex,PropertyIndex,CellType);
  if CellType=tgctValue then begin
    CurProp:=Properties[PropertyIndex];
    NewEditor:=CurProp.GetEditorControl;
    // position
    NewRect:=CellRect(Col,Row);
    NewEditor.BoundsRect:=NewRect;
    // connect to cell property
    PropLink:=GetPropertyLinkOfComponent(NewEditor);
    if PropLink<>nil then begin
      CurObject:=GetTIObject(ObjectIndex);
      PropName:=CurProp.Editor.GetPropInfo^.Name;
      PropLink.SetObjectAndProperty(CurObject,PropName);
    end;
    if (goEditing in Options) and Assigned(OnSelectEditor) then
      OnSelectEditor(Self,Col,Row,NewEditor);
  end;
  Editor:=NewEditor;
end;

procedure TTICustomGrid.DoEditorControlKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);

  procedure MoveSel(Rel: Boolean; aCol,aRow: Integer);
  begin
    SelectActive:=false;
    MoveNextSelectable(Rel, aCol, aRow);
    Key:=0;
  end;

var
  Relaxed: Boolean;
  GridProp: TTIGridProperty;
  CurLink: TCustomPropertyLink;
begin
  if Sender=nil then ;
  if (Shift=[ssCtrl]) then begin
    Relaxed:=not (goRowSelect in Options) or (goRelaxedRowSelect in Options);
    case Key of
    VK_UP:
      begin
        MoveSel(True, 0, -1);
      end;
    VK_DOWN:
      begin
        MoveSel(True, 0, 1);
      end;
    VK_HOME:
      begin
        if ssCtrl in Shift then MoveSel(False, Col, FixedRows)
        else
          if Relaxed then MoveSel(False, FixedCols, Row)
          else            MoveSel(False, Col, FixedRows);
      end;
    VK_END:
      begin
        if ssCtrl in Shift then MoveSel(False, Col, RowCount-1)
        else
          if Relaxed then MoveSel(False, ColCount-1, Row)
          else            MoveSel(False, Col, RowCount-1);
      end;
    VK_F2:
      begin
        GridProp:=GetCurrentGridProperty;
        if (GridProp<>nil) and (paDialog in GridProp.Editor.GetAttributes) then
        begin
          GridProp.Editor.Edit;
          CurLink:=GetPropertyLinkOfComponent(Editor);
          if CurLink<>nil then
            CurLink.LoadFromProperty;
        end;
      end;
    end;
  end;
end;

procedure TTICustomGrid.EditorHide;
var
  PropLink: TCustomPropertyLink;
begin
  if Editor<>nil then begin
    PropLink:=GetPropertyLinkOfComponent(Editor);
    if PropLink<>nil then
      PropLink.SetObjectAndProperty(nil,'');
  end;
  inherited EditorHide;
end;

constructor TTICustomGrid.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  FHeaderPropHook:=TPropertyEditorHook.Create;
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
  FreeThenNil(FHeaderPropHook);
  inherited Destroy;
end;

procedure TTICustomGrid.Loaded;
begin
  inherited Loaded;
  ReloadTIList;
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
  ObjectIndex: integer;
  PropertyIndex: integer;
  CellType: TTIGridCellType;
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
  Canvas.FillRect(aRect);
  //debugln('TTICustomGrid.DefaultDrawCell A Col=',dbgs(aCol),' Row=',dbgs(aRow));
  MapCell(aCol,aRow,ObjectIndex,PropertyIndex,CellType);
  if CellType in [tgctValue,tgctPropName] then begin
    // fetch a property editor and draw cell
    PropEditor:=nil;
    GetCellEditor(aCol,aRow,PropEditor,IndependentEditor);
    if PropEditor<>nil then begin
      //debugln('TTICustomGrid.DefaultDrawCell B ',dbgsName(PropEditor),' ',PropEditor.GetName,' ',PropEditor.GetValue);
      try
        if gdFixed in aState then
          PropEditor.PropDrawName(Canvas,aRect,GridStateToPropEditState(aState))
        else
          PropEditor.PropDrawValue(Canvas,aRect,GridStateToPropEditState(aState));
      finally
        if IndependentEditor then PropEditor.Free;
      end;
    end;
  end else if CellType=tgctObjectName then begin
    DrawObjectName(ObjectIndex,aRect,aState);
  end;
end;

procedure TTICustomGrid.DrawObjectName(Index: integer; aRect: TRect;
  aState: TGridDrawState);

  function GetTIObjectName(ObjIndex: integer): string;
  var
    ACollectionItem: TCollectionItem;
    AnObject: TPersistent;
  begin
    Result:='';
    AnObject:=GetTIObject(ObjIndex);
    if AnObject is TComponent then
      Result:=TComponent(AnObject).Name
    else if AnObject is TCollectionItem then begin
      ACollectionItem:=TCollectionItem(AnObject);
      Result:=ACollectionItem.DisplayName;
      // the default DisplayName is the ClassName, which is not informative
      if CompareText(Result,ACollectionItem.ClassName)=0 then Result:='';
    end;
    if Result='' then begin
      if tgoStartIndexAtOne in TIOptions then
        Result:=IntToStr(ObjIndex+1)
      else
        Result:=IntToStr(ObjIndex);
    end;
  end;

  procedure FixRectangle;
  begin
    case Canvas.TextStyle.Alignment of
      Classes.taLeftJustify: Inc(aRect.Left, 3);
      Classes.taRightJustify: Dec(aRect.Right, 3);
    end;
    Inc(aRect.Top, 2);
  end;

var
  ObjectName: String;
begin
  if aState=[] then ;
  if (Index<0) or (Index>=TIObjectCount) then exit;
  ObjectName:=GetTIObjectName(Index);
  if ObjectName<>'' then begin
    FixRectangle;
    Canvas.TextRect(aRect,ARect.Left,ARect.Top,ObjectName);
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
  ok: Boolean;
  CurObject: TPersistent;
  CellType: TTIGridCellType;
begin
  PropEditor:=nil;
  IndependentEditor:=true;
  MapCell(aCol,aRow,ObjectIndex,PropertyIndex,CellType);
  if CellType in [tgctValue,tgctPropName] then begin
    GridProperty:=Properties[PropertyIndex];
    if CellType=tgctPropName then begin
      IndependentEditor:=false;
      PropEditor:=GridProperty.Editor;
    end
    else begin
      CurObject:=GetTIObject(ObjectIndex);
      if (CurObject<>nil) then begin
        ok:=false;
        Hook:=nil;
        try
          Hook:=TPropertyEditorHook.Create;
          Hook.LookupRoot:=CurObject;
          EditorClass:=TPropertyEditorClass(GridProperty.Editor.ClassType);
          PropEditor:=EditorClass.Create(Hook,1);
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
              Hook.free;
            except
            end;
          end;
        end;
      end;
    end;
  end;
end;

procedure TTICustomGrid.FreeCellEditor(PropEditor: TPropertyEditor);
var
  Hook: TPropertyEditorHook;
begin
  if PropEditor=nil then exit;
  Hook:=PropEditor.PropertyHook;
  try
    PropEditor.free;
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
var
  List: TList;
  AnObject: TObject;
  ACollection: TCollection;
begin
  Result:=nil;
  if (Index<0) or (Index>=TIObjectCount) then exit;
  if ListObject is TCollection then begin
    ACollection:=TCollection(ListObject);
    if csDesigning in ComponentState then begin
      try
        Result:=ACollection.Items[Index];
      except
      end;
    end else begin
      Result:=ACollection.Items[Index];
    end;
  end else if ListObject is TList then begin
    List:=TList(ListObject);
    if csDesigning in ComponentState then begin
      try
        AnObject:=TObject(List[Index]);
        Result:=AnObject as TPersistent;
      except
      end;
    end else begin
      AnObject:=TObject(List[Index]);
      Result:=AnObject as TPersistent;
    end;
  end;
end;

procedure TTICustomGrid.MapCell(aCol, aRow: integer; var ObjectIndex,
  PropertyIndex: integer; var CellType: TTIGridCellType);
var
  PropHeaderLines: LongInt;
  ObjectHeaderLines: LongInt;
  PropertyIndexValid: Boolean;
  ObjectIndexValid: Boolean;
  PropertyIndexInHeader: Boolean;
  ObjectIndexInHeader: Boolean;
begin
  if ListDirection=tldObjectsAsRows then begin
    ObjectIndex:=aRow-FixedRows;
    PropertyIndex:=aCol-FixedCols;
    PropHeaderLines:=FixedRows;
    ObjectHeaderLines:=FixedCols;
  end else begin
    ObjectIndex:=aCol-FixedCols;
    PropertyIndex:=aRow-FixedRows;
    PropHeaderLines:=FixedCols;
    ObjectHeaderLines:=FixedRows;
  end;
  PropertyIndexValid:=(PropertyIndex>=0) and (PropertyIndex<PropertyCount);
  ObjectIndexValid:=(ObjectIndex>=0) and (ObjectIndex<TIObjectCount);
  PropertyIndexInHeader:=(PropertyIndex<0)
                         and (PropertyIndex>=-PropHeaderLines);
  ObjectIndexInHeader:=(ObjectIndex<0)
                         and (ObjectIndex>=-ObjectHeaderLines);
  //debugln('TTICustomGrid.MapCell A ',dbgs(aCol),',',dbgs(aRow),' ',
  //  dbgs(PropertyIndex),',',dbgs(ObjectIndex),' ',
  //  dbgs(PropertyIndexValid),',',dbgs(ObjectIndexValid),
  //  ' ',dbgs(PropertyIndexInHeader),',',dbgs(ObjectIndexInHeader));
  CellType:=tgctNone;
  if PropertyIndexValid then begin
    if ObjectIndexValid then
      CellType:=tgctValue
    else if ObjectIndexInHeader then begin
      if ObjectIndex=-1 then
        CellType:=tgctPropName
      else
        CellType:=tgctPropNameAlt;
    end;
  end else if ObjectIndexValid then begin
    if PropertyIndexInHeader then begin
      if PropertyIndex=-1 then
        CellType:=tgctObjectName
      else
        CellType:=tgctObjectNameAlt;
    end;
  end else begin
    if PropertyIndexInHeader and ObjectIndexInHeader then
      CellType:=tgctCorner;
  end;
end;

function TTICustomGrid.GetCurrentGridProperty: TTIGridProperty;
var
  ObjectIndex: Integer;
  PropertyIndex: Integer;
  CellType: TTIGridCellType;
begin
  Result:=nil;
  MapCell(Col,Row,ObjectIndex,PropertyIndex,CellType);
  if CellType=tgctValue then
    Result:=Properties[PropertyIndex];
end;

{ TTIGridProperty }

procedure TTIGridProperty.EditorControlKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  Grid.DoEditorControlKeyUp(Sender,Key,Shift);
end;

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

function TTIGridProperty.GetEditorControl: TWinControl;
var
  EditorClass: TWinControlClass;
  Attr: TPropertyAttributes;
begin
  if FEditorControl=nil then begin
    EditorClass:=FindTIGridControl(TPropertyEditorClass(Editor.ClassType));
    if EditorClass=nil then begin
      Attr:=Editor.GetAttributes;
      if paValueList in Attr then
        EditorClass:=TTIComboBox
      else if (paDialog in Attr) and (paReadOnly in Attr) then
        EditorClass:=TTIButton
      else
        EditorClass:=TTIEdit;
    end;
    FEditorControl:=EditorClass.Create(FGrid);
    FEditorControl.OnKeyUp:=@EditorControlKeyUp;
    FEditorControl.AutoSize:=false;
  end;
  Result:=FEditorControl;
end;

initialization
  RegisteredTIGridControls:=nil;
  // property editor for TTICustomPropertyGrid.TIObject
  RegisterPropertyEditor(ClassTypeInfo(TPersistent),
    TTICustomPropertyGrid, 'TIObject', TTIObjectPropertyEditor);

finalization
  FinalizeTIGrids;

end.

