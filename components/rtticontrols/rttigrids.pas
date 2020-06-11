{
 *****************************************************************************
  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************

  Author: Mattias Gaertner

  Abstract:
   Provides LCL controls that access lists of properties of TPersistent objects
   via RTTI
   - the FreePascal Run Time Type Information.
   
  ToDo:
   - better keyboard navigation
   - property editor for 'ListObject'
   - persistent selected cell after rebuild
   - moving objects
   - adding, deleting objects
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
    property CheckboxForBoolean;
    property Constraints;
    property DefaultItemHeight;
    property DefaultValueFont;
    property Filter;
    property Indent;
    property NameFont;
    property OnChangeBounds;
    property OnClick;
    property OnEditorFilter;
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
    property PreferredSplitterX;
    property SplitterX;
    property Tabstop;
    property TIObject;
    property ValueFont;
    property Visible;
  end;
  
  TTICustomGrid =  class;


  { TTIGridProperty }

  TTIGridProperty = class
  private
    FEditor: TPropertyEditor;
    FEditorControl: TWinControl;
    FButtonEditorControl: TWinControl;
    FGrid: TTICustomGrid;
    FIndex: integer;
    FTitle: string;
    procedure SetTitle(const AValue: string);
    procedure EditorControlKeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
  public
    constructor Create(TheGrid: TTICustomGrid; TheEditor: TPropertyEditor;
                       TheIndex: integer);
    destructor Destroy; override;
    function PropInfo: PPropInfo;
    function GetEditorControl: TWinControl;
    function GetButtonEditorControl: TWinControl;
    function PropName: string;
  public
    property Editor: TPropertyEditor read FEditor;
    property Grid: TTICustomGrid read FGrid;
    property Index: integer read FIndex;
    property Title: string read FTitle write SetTitle;
  end;
  
  
  TTIListDirection = (tldObjectsAsRows, tldObjectsAsColumns);
  TTIGridState = (
    tgsRebuildTIListNeeded,
    tgsRebuildingTIList,
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
  TTIGridCellTypes = set of TTIGridCellType;

  TTIGridOption = (
    tgoStartIndexAtOne, // start shown object index at 1
    tgoShowOnlyProperties // show only properties in ShowOnlyProperties
    );
  TTIGridOptions = set of TTIGridOption;

  TTIGridGetObject = procedure(Sender: TTICustomGrid; Index: integer;
                               var TIObject: TPersistent) of object;
  TTIGridGetObjectCount = procedure(Sender: TTICustomGrid;
                                    ListObject: TObject;
                                    var ObjCount: integer) of object;
  TTIGridGetObjectName = procedure(Sender: TObject; Index: integer;
                                   TIObject: TPersistent;
                                   var ObjName: string) of object;
  TTIGridCreateCellEditor = procedure(GridProp: TTIGridProperty;
                                      var NewEditorControl: TControl) of object;
  TTIGridInitCellEditor = procedure(GridProp: TTIGridProperty;
                                    TheEditorControl: TControl) of object;

  { TTICustomGrid }
  
  TTICustomGrid =  class(TCustomGrid)
  private
    FAliasPropertyNames: TAliasStrings;
    FFilter: TTypeKinds;
    FHideProperties: TStrings;
    FListDirection: TTIListDirection;
    FListObject: TObject;
    FOnCreateCellEditor: TTIGridCreateCellEditor;
    FOnGetObject: TTIGridGetObject;
    FOnGetObjectCount: TTIGridGetObjectCount;
    FOnGetObjectName: TTIGridGetObjectName;
    FOnHeaderClick: THdrEvent;
    FOnHeaderSized: THdrEvent;
    FHeaderPropHook: TPropertyEditorHook;
    FOnInitCellEditor: TTIGridInitCellEditor;
    FOnPropertiesCreated: TNotifyEvent;
    FPropertyOrder: TStrings;
    FShowOnlyProperties: TStrings;
    FTIOptions: TTIGridOptions;
    FTIStates: TTIGridStates;
    FTIObjectCount: integer;
    FProperties: TList;
    FExtraBtnEditor: TWinControl;
    function GetProperties(Index: integer): TTIGridProperty;
    function GetPropertyCount: integer;
    procedure SetAliasPropertyNames(const AValue: TAliasStrings);
    procedure SetFilter(const AValue: TTypeKinds);
    procedure SetHideProperties(const AValue: TStrings);
    procedure SetListDirection(const AValue: TTIListDirection);
    procedure SetListObject(const AValue: TObject);
    procedure SetPropertyOrder(const AValue: TStrings);
    procedure SetShowOnlyProperties(const AValue: TStrings);
    procedure SetTIOptions(const NewOptions: TTIGridOptions);
    {$IFDEF DebugEditor}
    procedure DebugEditor(msg: String; aEditor: TWinControl);
    {$ENDIF}
  protected
    procedure RebuildGridLayout; virtual;
    procedure AddHeaderPropertyEditor(Prop: TPropertyEditor);
    procedure BeforeMoveSelection(const DCol,DRow: Integer); override;
    procedure CalcCellExtent(aCol, aRow: Integer; var aRect: TRect); virtual;
    procedure DoEditorHide; override;
    procedure DoEditorShow; override;
    procedure DrawCell(aCol, aRow: Integer; aRect: TRect;
                       aState: TGridDrawState); override;
    procedure EditorPosChanged(aEditor: TWinControl);
    procedure EditorWidthChanged(aCol, aWidth: Integer); override;
    procedure HeaderClick(IsColumn: Boolean; index: Integer); override;
    procedure HeaderSized(IsColumn: Boolean; index: Integer); override;
    procedure GetAutoFillColumnInfo(const Index: Integer;
                                    var aMin,aMax,aPriority: Integer); override;
    procedure SelectEditor; override;
    procedure DoEditorControlKeyUp(Sender: TObject; var Key: Word;
                                   Shift: TShiftState); virtual;
    procedure WriteCellText(aRect: TRect; const aText: string);
    procedure UnlinkPropertyEditor(aEditor: TWinControl);
    procedure SetFixedCols(const AValue: Integer); override;
    procedure SetFixedRows(const AValue: Integer); override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure Loaded; override;
    procedure ReloadTIList;
    procedure ClearProperties;
    procedure DefaultDrawCell(aCol, aRow: Integer; var aRect: TRect;
                              aState: TGridDrawState); virtual;
    procedure DrawObjectName(Index: integer; const aRect: TRect;
                             aState: TGridDrawState);
    procedure GetCellEditor(aCol, aRow: integer;
                            out aPropEditor: TPropertyEditor;
                            out aIndependentEditor: boolean);
    procedure FreeCellEditor(PropEditor: TPropertyEditor);
    function GridStateToPropEditState(GridState: TGridDrawState
                                      ): TPropEditDrawState;
    function GetTIObject(Index: integer): TPersistent;
    procedure MapCell(aCol, aRow: integer;
                      out ObjectIndex, PropertyIndex: integer;
                      out CellType: TTIGridCellType);
    function GetCurrentGridProperty: TTIGridProperty;
    function IndexOfGridProperty(const PropName: string): integer;
    function FindGridProperty(const PropName: string): TTIGridProperty;
    procedure MoveProperty(FromID, ToID: integer);
  public
    property AliasPropertyNames: TAliasStrings read FAliasPropertyNames
                                               write SetAliasPropertyNames;
    property DefaultRowHeight default 20;
    property Filter: TTypeKinds read FFilter write SetFilter default AllTypeKinds;
    property HideProperties: TStrings read FHideProperties
                                      write SetHideProperties;
    property ListDirection: TTIListDirection read FListDirection
                                write SetListDirection default tldObjectsAsRows;
    property ListObject: TObject read FListObject write SetListObject;
    property OnCreateCellEditor: TTIGridCreateCellEditor
                             read FOnCreateCellEditor write FOnCreateCellEditor;
    property OnGetObject: TTIGridGetObject read FOnGetObject write FOnGetObject;
    property OnGetObjectCount: TTIGridGetObjectCount read FOnGetObjectCount
                                                     write FOnGetObjectCount;
    property OnGetObjectName: TTIGridGetObjectName read FOnGetObjectName
                                                    write FOnGetObjectName;
    property OnHeaderClick: THdrEvent read FOnHeaderClick write FOnHeaderClick;
    property OnHeaderSized: THdrEvent read FOnHeaderSized write FOnHeaderSized;
    property OnInitCellEditor: TTIGridInitCellEditor read FOnInitCellEditor
                                                     write FOnInitCellEditor;
    property OnPropertiesCreated: TNotifyEvent read FOnPropertiesCreated
                                               write FOnPropertiesCreated;
    property Properties[Index: integer]: TTIGridProperty read GetProperties;
    property PropertyCount: integer read GetPropertyCount;
    property PropertyOrder: TStrings read FPropertyOrder write SetPropertyOrder;
    property ShowOnlyProperties: TStrings read FShowOnlyProperties
                                          write SetShowOnlyProperties;
    property TIObjectCount: integer read FTIObjectCount;
    property TIOptions: TTIGridOptions read FTIOptions write SetTIOptions;
  end;
  
  
  { TTIGrid }
  
  TTIGrid = class(TTICustomGrid)
  published
    property AliasPropertyNames;
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
    property HideProperties;
    property ListDirection;
    property OnCreateCellEditor;
    property OnDblClick;
    property OnEditButtonClick;
    property OnEnter;
    property OnExit;
    property OnGetObject;
    property OnGetObjectCount;
    property OnGetObjectName;
    property OnHeaderClick;
    property OnHeaderSized;
    property OnInitCellEditor;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnPrepareCanvas;
    property OnPropertiesCreated;
    property Options;
    property ParentColor;
    property ParentFont;
    property PopupMenu;
    property PropertyOrder;
    property ShowHint;
    property ShowOnlyProperties;
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
  FreeAndNil(RegisteredTIGridControls);
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
  if FProperties <> nil then
    Result := FProperties.Count
  else
    Result := 0;
end;

procedure TTICustomGrid.SetAliasPropertyNames(const AValue: TAliasStrings);
begin
  if FAliasPropertyNames=AValue then exit;
  if FAliasPropertyNames.Equals(AValue) then exit;
  FAliasPropertyNames.Assign(AValue);
  Invalidate;
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

procedure TTICustomGrid.SetHideProperties(const AValue: TStrings);
begin
  if FHideProperties=AValue then exit;
  if FHideProperties.Equals(AValue) then exit;
  FHideProperties.Assign(AValue);
  ReloadTIList;
end;

procedure TTICustomGrid.SetListObject(const AValue: TObject);
begin
  if FListObject=AValue then exit;
  FListObject:=AValue;
  ReloadTIList;
end;

procedure TTICustomGrid.SetPropertyOrder(const AValue: TStrings);
begin
  if FPropertyOrder=AValue then exit;
  if FPropertyOrder.Equals(AValue) then exit;
  FPropertyOrder.Assign(AValue);
  ReloadTIList;
end;

procedure TTICustomGrid.SetShowOnlyProperties(const AValue: TStrings);
begin
  if FShowOnlyProperties=AValue then exit;
  if FShowOnlyProperties.Equals(AValue) then exit;
  FShowOnlyProperties.Assign(AValue);
  if tgoShowOnlyProperties in FTIOptions then
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
  if tgoShowOnlyProperties in ChangedOptions then ReloadTIList;
end;

{$IFDEF DebugEditor}
procedure TTICustomGrid.DebugEditor(msg: String; aEditor: TWinControl);
begin
  DbgOut(Msg,': Editor=');
  if aEditor=nil then DbgOut('nil')
  else DbgOut(AEditor.className);
  DebugLn;
end;
{$ENDIF}

procedure TTICustomGrid.ReloadTIList;
begin
  if tgsRebuildingTIList in FTIStates then exit;
  if ([csLoading,csDestroying]*ComponentState)<>[] then begin
    Include(FTIStates,tgsRebuildTIListNeeded);
    exit;
  end;
  Exclude(FTIStates,tgsRebuildTIListNeeded);
  Include(FTIStates,tgsRebuildingTIList);
  try
    EditorHide;
    ClearProperties;
    FTIObjectCount:=0;
    if Assigned(OnGetObjectCount) then
      OnGetObjectCount(Self,ListObject,FTIObjectCount)
    else if FListObject is TCollection then
      FTIObjectCount:=TCollection(FListObject).Count
    else if FListObject is TList then
      FTIObjectCount:=TList(FListObject).Count
    else if FListObject is TFPList then
      FTIObjectCount:=TFPList(FListObject).Count
    else begin
      // ListObject is not valid
    end;
    RebuildGridLayout;
  finally
    Exclude(FTIStates,tgsRebuildingTIList);
  end;
end;

procedure TTICustomGrid.RebuildGridLayout;
var
  CurItem: TPersistent;
  HeaderLines: LongInt;
  PropCount: LongInt;
  i: Integer;
  ToID: Integer;
  FromID: integer;
begin
  if FProperties=nil then
    exit;
  
  ClearProperties;
  // set column/row count for objects
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
  // reorder
  ToID:=0;
  for i:=0 to FPropertyOrder.Count-1 do begin
    FromID:=IndexOfGridProperty(FPropertyOrder[i]);
    if FromID>=0 then begin
      MoveProperty(FromID,ToID);
      inc(ToID);
    end;
  end;
  
  // set column/row count for properties
  PropCount:=PropertyCount;
  if ListDirection=tldObjectsAsRows then begin
    ColCount:=FixedCols+PropCount;
  end else begin
    RowCount:=FixedRows+PropCount;
  end;
  if Assigned(OnPropertiesCreated) then OnPropertiesCreated(Self);
end;

procedure TTICustomGrid.AddHeaderPropertyEditor(Prop: TPropertyEditor);
var
  NewProperty: TTIGridProperty;
begin
  if (FHideProperties.IndexOf(Prop.GetPropInfo^.Name)>=0)
  or ((tgoShowOnlyProperties in FTIOptions)
      and (FShowOnlyProperties.IndexOf(Prop.GetPropInfo^.Name)<0))
  then begin
    // skip property
    Prop.Free;
    exit;
  end;
  NewProperty:=TTIGridProperty.Create(Self,Prop,FProperties.Count);
  FProperties.Add(NewProperty);
end;

procedure TTICustomGrid.BeforeMoveSelection(const DCol, DRow: Integer);
begin
  inherited BeforeMoveSelection(DCol, DRow);
  if (FExtraBtnEditor<>nil) and (FExtraBtnEditor.Visible) then begin
    {$IFDEF DebugEditor}
    DebugEditor('BeforeMoveSelection: ', FExtraBtnEditor);
    {$ENDIF}
    LockEditor;
    FExtraBtnEditor.Parent := nil;
    UnlinkPropertyEditor(FExtraBtnEditor);
    FExtraBtnEditor.Visible := false;
    FExtraBtnEditor := nil;
    UnlockEditor;
  end;
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

procedure TTICustomGrid.EditorPosChanged(aEditor: TWinControl);
var
  NewRect, ARect: TRect;
begin
  // position
  NewRect:=CellRect(Col,Row);
  if FExtraBtnEditor<>nil then begin
    ARect := NewRect;
    ARect.Left := ARect.Right-20;
    Dec(NewRect.Right,20);
    FExtraBtnEditor.BoundsRect := ARect;
  end;
  aEditor.BoundsRect:=NewRect;
end;

procedure TTICustomGrid.EditorWidthChanged(aCol, aWidth: Integer);
begin
  if (aCol=0) and (aWidth=0) then ;
  EditorPosChanged(Editor);
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
  PropLink: TCustomPropertyLink;
  CurObject: TPersistent;
  CurProp: TTIGridProperty;
  PropName: String;
begin
  if FProperties=nil then begin
    // still creating ancestor grid
    Editor := nil;
    FExtraBtnEditor := nil;
    exit;
  end;
  NewEditor:=nil;
  MapCell(Col,Row,ObjectIndex,PropertyIndex,CellType);
  if CellType=tgctValue then begin
    CurProp:=Properties[PropertyIndex];
    NewEditor:=CurProp.GetEditorControl;
    FExtraBtnEditor := CurProp.GetButtonEditorControl;
    {$IFDEF DebugEditor}
    DebugEditor('SelectEditor', NewEditor);
    DebugEditor('SelectEditor extra', FExtraBtnEditor);
    {$ENDIF}
    
    EditorPosChanged(NewEditor);
    // connect to cell property
    PropLink:=GetPropertyLinkOfComponent(NewEditor);
    if PropLink<>nil then begin
      CurObject:=GetTIObject(ObjectIndex);
      PropName:=CurProp.PropName;
      PropLink.SetObjectAndProperty(CurObject,PropName);
    end;
    if (FExtraBtnEditor<>nil) then begin
      PropLink:=GetPropertyLinkOfComponent(FExtraBtnEditor);
      if PropLink<>nil then begin
        CurObject:=GetTIObject(ObjectIndex);
        PropName:=CurProp.PropName;
        PropLink.SetObjectAndProperty(CurObject,PropName);
      end;
      if FExtraBtnEditor.Parent = nil then
        FExtraBtnEditor.Visible := False;
      FExtraBtnEditor.Parent := Self;
    end;
    if Assigned(OnSelectEditor) then
      OnSelectEditor(Self,Col,Row,NewEditor);
  end else
    FExtraBtnEditor := nil;
  Editor:=NewEditor;
  // options
  //EditorOptions := EO_HOOKKEYPRESS or EO_HOOKKEYDOWN or EO_HOOKKEYDOWN;
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

procedure TTICustomGrid.DoEditorHide;
begin
  {$IFDEF DebugEditor}
  DebugEditor('doEditorHide', Editor);
  {$ENDIF}
  UnlinkPropertyEditor(Editor);
  UnlinkPropertyEditor(FExtraBtnEditor);
  inherited DoEditorHide;
end;

procedure TTICustomGrid.DoEditorShow;
begin
  {$IFDEF DebugEditor}
  DebugEditor('doEditorShow', Editor);
  {$ENDIF}
  inherited DoEditorShow;
  if FExtraBtnEditor<>nil then begin
    {$IFDEF DebugEditor}
    DebugEditor('doEditorShow Extra', FExtraBtnEditor);
    {$ENDIF}
    FExtraBtnEditor.Parent := Self;
    FExtraBtnEditor.Visible := True;
  end;
end;

procedure TTICustomGrid.WriteCellText(aRect: TRect; const aText: string);
begin
  if aText='' then exit;
  case Canvas.TextStyle.Alignment of
    Classes.taLeftJustify: Inc(aRect.Left, 3);
    Classes.taRightJustify: Dec(aRect.Right, 3);
  end;
  Inc(aRect.Top, 2);
  Canvas.TextRect(aRect,ARect.Left,ARect.Top,aText);
end;

procedure TTICustomGrid.UnlinkPropertyEditor(aEditor: TWinControl);
var
  PropLink: TCustomPropertyLink;
begin
  PropLink:=GetPropertyLinkOfComponent(aEditor);
  if PropLink<>nil then
    PropLink.SetObjectAndProperty(nil,'');
end;

procedure TTICustomGrid.SetFixedCols(const AValue: Integer);
begin
  inherited SetFixedCols(AValue);
  RebuildGridLayout;
end;

procedure TTICustomGrid.SetFixedRows(const AValue: Integer);
begin
  inherited SetFixedRows(AValue);
  RebuildGridLayout;
end;

constructor TTICustomGrid.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  FHeaderPropHook:=TPropertyEditorHook.Create(Self);
  FFilter:=[{tkUnknown,}tkInteger,tkChar,tkEnumeration,
            tkFloat,{tkSet,tkMethod,}tkSString,tkLString,tkAString,
            tkWString,tkVariant,{tkArray,tkRecord,tkInterface,}
            {tkClass,tkObject,}tkWChar,tkBool,tkInt64,
            tkQWord{,tkDynArray,tkInterfaceRaw}];
  FProperties:=TList.Create;
  FListDirection:=tldObjectsAsRows;
  FHideProperties:=TStringList.Create;
  FPropertyOrder:=TStringList.Create;
  FShowOnlyProperties:=TStringList.Create;
  FAliasPropertyNames:=TAliasStrings.Create;
end;

destructor TTICustomGrid.Destroy;
begin
  ClearProperties;
  FreeThenNil(FProperties);
  FreeThenNil(FHeaderPropHook);
  FreeThenNil(FHideProperties);
  FreeThenNil(FPropertyOrder);
  FreeThenNil(FShowOnlyProperties);
  FreeThenNil(FAliasPropertyNames);
  inherited Destroy;
end;

procedure TTICustomGrid.Loaded;
begin
  inherited Loaded;
  if tgsRebuildTIListNeeded in FTIStates then
    ReloadTIList;
end;

procedure TTICustomGrid.ClearProperties;
var
  i: Integer;
begin
  if FProperties=nil then exit;
  for i:=0 to FProperties.Count-1 do begin
    TObject(FProperties[i]).Free;
    FProperties[i]:=nil;
  end;
  FProperties.Clear;
  Editor := nil;
  FExtraBtnEditor := nil;
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
  AliasPropName: String;
  PropName: String;
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
  if (PropertyIndex=0) then ;
  if CellType in [tgctValue,tgctPropName] then begin
    // fetch a property editor and draw cell
    PropEditor:=nil;
    GetCellEditor(aCol,aRow,PropEditor,IndependentEditor);
    if PropEditor<>nil then begin
      //debugln('TTICustomGrid.DefaultDrawCell B ',dbgsName(PropEditor),' ',PropEditor.GetName,' ',PropEditor.GetValue);
      try
        if gdFixed in aState then begin
          if Properties[PropertyIndex].Title <> '' then
          begin
            WriteCellText(aRect,Properties[PropertyIndex].Title);
          end
          else begin
            PropName:=PropEditor.GetName;
            AliasPropName:=AliasPropertyNames.ValueToAlias(PropName);
            if AliasPropName=PropName then begin
              PropEditor.PropDrawName(Canvas,aRect,
                                      GridStateToPropEditState(aState));
            end else begin
              WriteCellText(aRect,AliasPropName);
            end;
          end;
        end else
          PropEditor.PropDrawValue(Canvas,aRect,GridStateToPropEditState(aState));
      finally
        if IndependentEditor then PropEditor.Free;
      end;
    end;
  end else if CellType=tgctObjectName then begin
    DrawObjectName(ObjectIndex,aRect,aState);
  end;
end;

procedure TTICustomGrid.DrawObjectName(Index: integer; const aRect: TRect;
  aState: TGridDrawState);

  function GetTIObjectName(ObjIndex: integer): string;
  var
    ACollectionItem: TCollectionItem;
    AnObject: TPersistent;
  begin
    Result:='';
    AnObject:=GetTIObject(ObjIndex);
    if Assigned(OnGetObjectName) then begin
      OnGetObjectName(Self,Index,AnObject,Result);
      exit;
    end;
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

var
  ObjectName: String;
begin
  if aState=[] then ;
  if (Index<0) or (Index>=TIObjectCount) then exit;
  ObjectName:=GetTIObjectName(Index);
  WriteCellText(aRect,ObjectName);
end;

procedure TTICustomGrid.GetCellEditor(aCol, aRow: integer;
  out aPropEditor: TPropertyEditor; out aIndependentEditor: boolean);
var
  ObjectIndex: Integer;
  PropertyIndex: Integer;
  EditorClass: TPropertyEditorClass;
  GridProperty: TTIGridProperty;
  NewEditor: TPropertyEditor;
  CurObject: TPersistent;
  CellType: TTIGridCellType;
begin
  aPropEditor:=nil;
  aIndependentEditor:=true;
  MapCell(aCol,aRow,ObjectIndex,PropertyIndex,CellType);
  if CellType in [tgctValue,tgctPropName] then begin
    GridProperty:=Properties[PropertyIndex];
    if CellType=tgctPropName then begin
      aIndependentEditor:=false;
      aPropEditor:=GridProperty.Editor;
    end
    else begin
      CurObject:=GetTIObject(ObjectIndex);
      if (CurObject<>nil) then begin
        NewEditor:=nil;
        try
          EditorClass:=TPropertyEditorClass(GridProperty.Editor.ClassType);
          NewEditor:=EditorClass.Create(FHeaderPropHook,1);
          NewEditor.SetPropEntry(0,CurObject,GridProperty.PropInfo);
          NewEditor.Initialize;
          aPropEditor := NewEditor;
        finally
          if aPropEditor = nil then begin
            try
              NewEditor.Free;
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
  FPList: TFPList;
begin
  Result:=nil;
  if (Index<0) or (Index>=TIObjectCount) then exit;
  
  // use event
  if Assigned(OnGetObject) then begin
    Result:=nil;
    OnGetObject(Self,Index,Result);
    exit;
  end;

  // try standard lists: TCollection and TList
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
  end else if ListObject is TFPList then begin
    FPList:=TFPList(ListObject);
    if csDesigning in ComponentState then begin
      try
        AnObject:=TObject(FPList[Index]);
        Result:=AnObject as TPersistent;
      except
      end;
    end else begin
      AnObject:=TObject(FPList[Index]);
      Result:=AnObject as TPersistent;
    end;
  end;
end;

procedure TTICustomGrid.MapCell(aCol, aRow: integer; out ObjectIndex,
  PropertyIndex: integer; out CellType: TTIGridCellType);
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
  // tldObjectsAsRows:
  //      PropertyIndex is a Col index, needs to be checked against ObjectHeaderLines (fixedCols)
  //      ObjectIndex is a Row index, needs to be checked against PropHeaderLines (fixedRows)
  // tldObjectsAsColumns:
  //      PropertyIndex is a Row index, needs to be checked against ObjectHeaderLines (fixedRows)
  //      ObjectIndex is a Col index, needs to be checked against PropHeaderLines (fixedCols)
  PropertyIndexInHeader:=(PropertyIndex<0)
                         and (PropertyIndex>=-ObjectHeaderLines);
  ObjectIndexInHeader:=(ObjectIndex<0)
                         and (ObjectIndex>=-PropHeaderLines);
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
      else if Objectindex=-2 then
        CellType:=tgctPropNameAlt;
    end;
  end else if ObjectIndexValid then begin
    if PropertyIndexInHeader then begin
      if PropertyIndex=-1 then
        CellType:=tgctObjectName
      else if PropertyIndex=-2 then
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
  if (ObjectIndex=0) or (PropertyIndex=0) then ;
  if CellType=tgctValue then
    Result:=Properties[PropertyIndex];
end;

function TTICustomGrid.IndexOfGridProperty(const PropName: string
  ): integer;
begin
  Result:=FProperties.Count-1;
  while (Result>=0) and (CompareText(Properties[Result].PropName,PropName)<>0)
  do dec(Result);
end;

function TTICustomGrid.FindGridProperty(const PropName: string
  ): TTIGridProperty;
var
  i: integer;
begin
  i:=IndexOfGridProperty(PropName);
  if i>=0 then
    Result:=Properties[i]
  else
    Result:=nil;
end;

procedure TTICustomGrid.MoveProperty(FromID, ToID: integer);
begin
  if FromID=ToID then exit;
  EditorHide;
  FProperties.Move(FromID,ToID);
  Properties[FromID].FIndex:=FromID;
  Properties[ToID].FIndex:=ToID;
  Invalidate;
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

destructor TTIGridProperty.Destroy;
begin
  FreeThenNil(FButtonEditorControl);
  FreeThenNil(FEditorControl);
  FreeThenNil(FEditor);
  inherited Destroy;
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
    FButtonEditorControl := nil;
    if Assigned(Grid.OnCreateCellEditor) then
      Grid.OnCreateCellEditor(Self,FEditorControl);
    if FEditorControl=nil then begin
      EditorClass:=FindTIGridControl(TPropertyEditorClass(Editor.ClassType));
      if EditorClass=nil then begin
        Attr:=Editor.GetAttributes;
        if (paDialog in Attr) and (paReadOnly in Attr) then
          EditorClass:=TTIButton
        else if (paValueList in Attr) then begin
          EditorClass:=TTIComboBox;
          if (paDialog in Attr) then
            FButtonEditorControl := TTIButton.Create(FGrid);
        end else
          EditorClass:=TTIEdit;
      end;
      FEditorControl:=EditorClass.Create(FGrid);
    end;
    FEditorControl.OnKeyUp:=@EditorControlKeyUp;
    //FEditorControl.AutoSize:=false;
    if Assigned(Grid.OnInitCellEditor) then
      Grid.OnInitCellEditor(Self,FEditorControl);
    if Assigned(Grid.OnInitCellEditor) and (FButtonEditorControl<>nil) then
      Grid.OnInitCellEditor(Self,FButtonEditorControl);
  end;
  Result:=FEditorControl;
end;

function TTIGridProperty.GetButtonEditorControl: TWinControl;
begin
  Result := FButtonEditorControl;
end;

function TTIGridProperty.PropName: string;
begin
  Result:=PropInfo^.Name;
end;

initialization
  RegisteredTIGridControls:=nil;
  // property editor for TTICustomPropertyGrid.TIObject
  RegisterPropertyEditor(ClassTypeInfo(TPersistent),
    TTICustomPropertyGrid, 'TIObject', TTIObjectPropertyEditor);

finalization
  FinalizeTIGrids;

end.

