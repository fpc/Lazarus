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
   This unit defines the TObjectInspector which is a descendent of TCustomForm.
   It uses TOIPropertyGrid and TOIPropertyGridRow which are also defined in this
   unit. The object inspector uses property editors (see TPropertyEditor) to
   display and control properties, thus the object inspector is merely an
   object viewer than an editor. The property editors do the real work.


  ToDo:
   - TCustomComboBox has a bug: it can not store objects
   - clipping (almost everywhere)
   - TCustomComboBox don't know custom draw yet
   - combobox can't sort (exception)
   - backgroundcolor=clNone
   - DoubleClick on Property

   - a lot more ...  see XXX
}
unit ObjectInspector;

{$MODE OBJFPC}{$H+}

interface

uses
  Forms, SysUtils, Buttons, Classes, Graphics, GraphType, StdCtrls, LCLType,
  LCLLinux, LMessages, Controls, ComCtrls, ExtCtrls, PropEdits, GraphPropEdits,
  TypInfo, Messages, LResources, Laz_XMLCfg, Menus, Dialogs;


type
  EObjectInspectorException = class(Exception);
  
  TObjectInspector = class;

  TOIOptions = class
  private
    FFilename:string;
    
    FSaveBounds: boolean;
    FLeft: integer;
    FTop: integer;
    FWidth: integer;
    FHeight: integer;
    FPropertyGridSplitterX: integer;
    FEventGridSplitterX: integer;

    FGridBackgroundColor: TColor;
    FShowHints: boolean;
  public
    constructor Create;
    destructor Destroy;  override;
    function Load:boolean;
    function Save:boolean;
    procedure Assign(AnObjInspector: TObjectInspector);
    procedure AssignTo(AnObjInspector: TObjectInspector);
    property Filename:string read FFilename write FFilename;

    property SaveBounds:boolean read FSaveBounds write FSaveBounds;
    property Left:integer read FLeft write FLeft;
    property Top:integer read FTop write FTop;
    property Width:integer read FWidth write FWidth;
    property Height:integer read FHeight write FHeight;
    property PropertyGridSplitterX:integer
      read FPropertyGridSplitterX write FPropertyGridSplitterX;
    property EventGridSplitterX:integer
      read FEventGridSplitterX write FEventGridSplitterX;

    property GridBackgroundColor: TColor 
      read FGridBackgroundColor write FGridBackgroundColor;
    property ShowHints: boolean
      read FShowHints write FShowHints;
  end;

  TOIPropertyGrid = class;

  TOIPropertyGridRow = class
  private
    FTop:integer;
    FHeight:integer;
    FLvl:integer;
    FName:string;
    FExpanded: boolean;
    FTree:TOIPropertyGrid;
    FChildCount:integer;
    FPriorBrother,
    FFirstChild,
    FLastChild,
    FNextBrother,
    FParent:TOIPropertyGridRow;
    FEditor: TPropertyEditor;
    procedure GetLvl;
  public
    Index:integer;
    LastPaintedValue:string;
    property Editor:TPropertyEditor read FEditor;
    property Top:integer read FTop write FTop;
    property Height:integer read FHeight write FHeight;
    function Bottom:integer;
    property Lvl:integer read FLvl;
    property Name:string read FName;
    property Expanded:boolean read FExpanded;
    property Tree:TOIPropertyGrid read FTree;
    property Parent:TOIPropertyGridRow read FParent;
    property ChildCount:integer read FChildCount;
    property FirstChild:TOIPropertyGridRow read FFirstChild;
    property LastChild:TOIPropertyGridRow read FFirstChild;
    property NextBrother:TOIPropertyGridRow read FNextBrother;
    property PriorBrother:TOIPropertyGridRow read FPriorBrother;
    constructor Create(PropertyTree:TOIPropertyGrid;  PropEditor:TPropertyEditor;
       ParentNode:TOIPropertyGridRow);
    destructor Destroy; override;
  end;

  //----------------------------------------------------------------------------
  TOIPropertyGridState = (pgsChangingItemIndex, pgsApplyingValue);
  TOIPropertyGridStates = set of TOIPropertyGridState;
  
  TOIPropertyGrid = class(TCustomControl)
  private
    FComponentList: TComponentSelectionList;
    FPropertyEditorHook:TPropertyEditorHook;
    FFilter: TTypeKinds;
    FItemIndex:integer;
    FRows:TList;
    FExpandingRow:TOIPropertyGridRow;
    FTopY:integer;
    FDefaultItemHeight:integer;
    FSplitterX:integer; // current splitter position
    FPreferredSplitterX: integer; // best splitter position
    FIndent:integer;
    FBackgroundColor:TColor;
    FNameFont,FValueFont:TFont;
    FCurrentEdit:TWinControl;  // nil or ValueEdit or ValueComboBox
    FCurrentButton:TWinControl; // nil or ValueButton
    FDragging:boolean;
    FOnModified: TNotifyEvent;
    FExpandedProperties:TStringList;
    FBorderStyle:TBorderStyle;
    FStates: TOIPropertyGridStates;

    //hint stuff
    FHintTimer : TTimer;
    FHintWindow : THintWindow;
    FLastMouseMovePos : TPoint;
    Procedure HintTimer(sender: TObject);
    Procedure ResetHintTimer(Sender: TObject; Shift: TShiftstate; X,Y: Integer);
    procedure CurrentEditMouseDown(Sender: TObject; Button:TMouseButton;
      Shift:TShiftState; X,Y:integer);


    Procedure CurrentEditDblClick(Sender : TObject);

    function GetRow(Index:integer):TOIPropertyGridRow;
    function GetRowCount:integer;
    procedure ClearRows;
    procedure SetItemIndex(NewIndex:integer);

    procedure SetItemsTops;
    procedure AlignEditComponents;
    procedure EndDragSplitter;
    procedure SetSplitterX(const NewValue:integer);
    procedure SetTopY(const NewValue:integer);

    function GetTreeIconX(Index:integer):integer;
    function RowRect(ARow:integer):TRect;
    procedure PaintRow(ARow:integer);
    procedure DoPaint(PaintOnlyChangedValues:boolean);

    procedure SetSelections(const NewSelections:TComponentSelectionList);
    procedure SetPropertyEditorHook(NewPropertyEditorHook:TPropertyEditorHook);

    procedure AddPropertyEditor(PropEditor: TPropertyEditor);
    procedure AddStringToComboBox(const s:string);
    procedure ExpandRow(Index:integer);
    procedure ShrinkRow(Index:integer);
    procedure AddSubEditor(PropEditor:TPropertyEditor);

    procedure SetRowValue;
    procedure ValueEditKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure ValueEditExit(Sender: TObject);
    procedure ValueEditChange(Sender: TObject);
    procedure ValueComboBoxExit(Sender: TObject);
    procedure ValueComboBoxChange(Sender: TObject);
    procedure ValueComboBoxKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure ValueButtonClick(Sender: TObject);

    procedure WMVScroll(var Msg: TWMScroll); message WM_VSCROLL;
    procedure WMMouseMove(var Msg: TWMMouseMove); message WM_MOUSEMOVE;
    procedure WMSize(var Msg: TWMSize); message WM_SIZE;
    procedure SetBorderStyle(Value: TBorderStyle);
    procedure SetBackgroundColor(const AValue: TColor);
    procedure UpdateScrollBar;
  protected
    procedure CreateParams(var Params: TCreateParams); override;

    procedure MouseDown(Button:TMouseButton; Shift:TShiftState; X,Y:integer);  override;
    procedure MouseMove(Shift:TShiftState; X,Y:integer);  override;
    procedure MouseUp(Button:TMouseButton; Shift:TShiftState; X,Y:integer);  override;
  public
    ValueEdit:TEdit;
    ValueComboBox:TComboBox;
    ValueButton:TButton;

    property Selections:TComponentSelectionList read FComponentList write SetSelections;
    property PropertyEditorHook:TPropertyEditorHook
       read FPropertyEditorHook write SetPropertyEditorHook;
    procedure BuildPropertyList;
    procedure RefreshPropertyValues;

    property RowCount:integer read GetRowCount;
    property Rows[Index:integer]:TOIPropertyGridRow read GetRow;

    property TopY:integer read FTopY write SetTopY;
    function GridHeight:integer;
    function TopMax:integer;
    property DefaultItemHeight:integer read FDefaultItemHeight write FDefaultItemHeight;
    property SplitterX:integer read FSplitterX write SetSplitterX;
    property PrefferedSplitterX:integer read FPreferredSplitterX write FPreferredSplitterX;
    property Indent:integer read FIndent write FIndent;
    property BackgroundColor:TColor
       read FBackgroundColor write SetBackgroundColor default clBtnFace;
    property NameFont:TFont read FNameFont write FNameFont;
    property ValueFont:TFont read FValueFont write FValueFont;
    property BorderStyle: TBorderStyle read FBorderStyle write SetBorderStyle
       default bsSingle;
    property ItemIndex:integer read FItemIndex write SetItemIndex;
    property ExpandedProperties:TStringList 
       read FExpandedProperties write FExpandedProperties;
    function PropertyPath(Index:integer):string;
    function GetRowByPath(PropPath:string):TOIPropertyGridRow;

    function MouseToIndex(y:integer;MustExist:boolean):integer;

    property OnModified: TNotifyEvent read FOnModified write FOnModified;
    procedure SetBounds(aLeft,aTop,aWidth,aHeight:integer); override;
    procedure Paint;  override;
    procedure Clear;
    constructor Create(AnOwner:TComponent;
       APropertyEditorHook:TPropertyEditorHook;  TypeFilter:TTypeKinds);
    destructor Destroy;  override;
  end;

  //============================================================================
  TOnAddAvailableComponent = procedure(AComponent:TComponent;
    var Allowed:boolean) of object;

  TOnSelectComponentInOI = procedure(AComponent:TComponent) of object;

  TObjectInspector = class (TCustomForm)
    AvailCompsComboBox : TComboBox;
    NoteBook:TNoteBook;
    PropertyGrid:TOIPropertyGrid;
    EventGrid:TOIPropertyGrid;
    StatusBar:TStatusBar;
    MainPopupMenu:TPopupMenu;
    ColorsPopupMenuItem:TMenuItem;
    BackgroundColPopupMenuItem:TMenuItem;
    ShowHintsPopupMenuItem:TMenuItem;
    procedure AvailComboBoxChange(Sender:TObject);
    procedure OnBackgroundColPopupMenuItemClick(Sender :TObject);
    procedure OnShowHintPopupMenuItemClick(Sender :TObject);
  private
    FComponentList: TComponentSelectionList;
    FPropertyEditorHook:TPropertyEditorHook;
    FUpdatingAvailComboBox:boolean;
    FOnAddAvailableComponent:TOnAddAvailableComponent;
    FOnSelectComponentInOI:TOnSelectComponentInOI;
    FOnModified: TNotifyEvent;
    function ComponentToString(c:TComponent):string;
    procedure SetPropertyEditorHook(NewValue:TPropertyEditorHook);
    procedure SetSelections(const NewSelections:TComponentSelectionList);
    procedure AddComponentToAvailComboBox(AComponent:TComponent);
    procedure PropEditLookupRootChange;
    procedure OnGridModified(Sender: TObject);
  public
    property Selections:TComponentSelectionList 
      read FComponentList write SetSelections;
    procedure RefreshSelections;
    procedure RefreshPropertyValues;
    procedure FillComponentComboBox;
    property OnAddAvailComponent:TOnAddAvailableComponent
      read FOnAddAvailableComponent write FOnAddAvailableComponent;
    property OnSelectComponentInOI:TOnSelectComponentInOI
      read FOnSelectComponentInOI write FOnSelectComponentInOI;
    property PropertyEditorHook:TPropertyEditorHook 
      read FPropertyEditorHook write SetPropertyEditorHook;
    property OnModified: TNotifyEvent read FOnModified write FOnModified;
    constructor Create(AnOwner: TComponent); override;
    destructor Destroy; override;
  end;

const
  DefaultObjectInspectorName: string = 'ObjectInspector';


//******************************************************************************


implementation

const
  ScrollBarWidth=0;

function SortGridRows(Item1, Item2 : pointer) : integer; 
begin
  Result:= AnsiCompareText(TOIPropertyGridRow(Item1).Name, TOIPropertyGridRow(Item2).Name);
end;


{ TOIPropertyGrid }

constructor TOIPropertyGrid.Create(AnOwner:TComponent;
  APropertyEditorHook:TPropertyEditorHook;  TypeFilter:TTypeKinds);
begin
  inherited Create(AnOwner);
  if LazarusResources.Find(ClassName)=nil then begin
    SetBounds(1,1,200,300);
    ControlStyle:=ControlStyle+[csAcceptsControls,csOpaque];
    BorderWidth:=1;
  end;
    
  FComponentList:=TComponentSelectionList.Create;
  FPropertyEditorHook:=APropertyEditorHook;
  FFilter:=TypeFilter;
  FItemIndex:=-1;
  FStates:=[];
  FRows:=TList.Create;
  FExpandingRow:=nil;
  FDragging:=false;
  FExpandedProperties:=TStringList.Create;

  // visible values
  FTopY:=0;
  FSplitterX:=100;
  FPreferredSplitterX:=FSplitterX;
  FIndent:=9;
  FBackgroundColor:=clBtnFace;
  FNameFont:=TFont.Create;
  FNameFont.Color:=clWindowText;
  FValueFont:=TFont.Create;
  FValueFont.Color:=clActiveCaption;
  fBorderStyle := bsSingle;

  // create sub components
  FCurrentEdit:=nil;
  FCurrentButton:=nil;

  ValueEdit:=TEdit.Create(Self);
  with ValueEdit do begin
    Name:='ValueEdit';
    Parent:=Self;
    Visible:=false;
    Enabled:=false;
    OnMouseMove := @ResetHintTimer;
    OnMouseDown := @CurrentEditMouseDown;
    OnDblClick := @CurrentEditDblClick;
    OnExit:=@ValueEditExit;
    OnChange:=@ValueEditChange;
    OnKeyDown:=@ValueEditKeyDown;
  end;

  ValueComboBox:=TComboBox.Create(Self);
  with ValueComboBox do begin
    Name:='ValueComboBox';
    Visible:=false;
    Enabled:=false;
    Parent:=Self;
    OnMouseMove := @ResetHintTimer;
    OnMouseDown := @CurrentEditMouseDown;
    OnDblClick := @CurrentEditDblClick;
    OnExit:=@ValueComboBoxExit;
    //OnChange:=@ValueComboBoxChange; the on change event is called even,
                                   // if the user is editing
    OnKeyDown:=@ValueComboBoxKeyDown;
  end;

  ValueButton:=TButton.Create(Self);
  with ValueButton do begin
    Name:='ValueButton';
    Visible:=false;
    Enabled:=false;
    OnClick:=@ValueButtonClick;
    Caption := '...';
    Parent:=Self;
  end;

  FDefaultItemHeight:=ValueComboBox.Height-3;

  BuildPropertyList;
  
  FHintTimer := TTimer.Create(nil);
  FHintTimer.Interval := 500;
  FHintTimer.Enabled := False;
  FHintTimer.OnTimer := @HintTimer;

  FHintWindow := THintWindow.Create(nil);

  FHIntWindow.Visible := False;
  FHintWindow.Caption := 'This is a hint window'#13#10'Neat huh?';
  FHintWindow.HideInterval := 4000;
  FHintWindow.AutoHide := True;
end;

procedure TOIPropertyGrid.UpdateScrollBar;
var
  ScrollInfo: TScrollInfo;
begin
  if HandleAllocated then begin
    ScrollInfo.cbSize := SizeOf(ScrollInfo);
    ScrollInfo.fMask := SIF_ALL or SIF_DISABLENOSCROLL;
    ScrollInfo.nMin := 0;
    ScrollInfo.nTrackPos := 0;
    ScrollInfo.nMax := TopMax+ClientHeight;
    ScrollInfo.nPage := ClientHeight;
    if ScrollInfo.nPage<1 then ScrollInfo.nPage:=1;
    ScrollInfo.nPos := TopY;
    SetScrollInfo(Handle, SB_VERT, ScrollInfo, True);
    ShowScrollBar(Handle,SB_VERT,True);
  end;
end;

procedure TOIPropertyGrid.CreateParams(var Params: TCreateParams);
const
  BorderStyles: array[TBorderStyle] of DWORD = (0, WS_BORDER);
  ClassStylesOff = CS_VREDRAW or CS_HREDRAW;
begin
  inherited CreateParams(Params);
  with Params do begin
    {$R-}
    WindowClass.Style := WindowClass.Style and not ClassStylesOff;
    Style := Style or WS_VSCROLL or BorderStyles[fBorderStyle]
      or WS_CLIPCHILDREN;
    {$R+}
    if NewStyleControls and Ctl3D and (fBorderStyle = bsSingle) then begin
      Style := Style and not Cardinal(WS_BORDER);
      ExStyle := ExStyle or WS_EX_CLIENTEDGE;
    end;
  end;
end;

procedure TOIPropertyGrid.SetBorderStyle(Value: TBorderStyle);
begin
  if fBorderStyle <> Value then begin
    fBorderStyle := Value;
    Invalidate;
  end;
end;

procedure TOIPropertyGrid.WMVScroll(var Msg: TWMScroll);
begin
  case Msg.ScrollCode of
      // Scrolls to start / end of the text
    SB_TOP:        TopY := 0;
    SB_BOTTOM:     TopY := TopMax;
      // Scrolls one line up / down
    SB_LINEDOWN:   TopY := TopY + DefaultItemHeight div 2;
    SB_LINEUP:     TopY := TopY - DefaultItemHeight div 2;
      // Scrolls one page of lines up / down
    SB_PAGEDOWN:   TopY := TopY + ClientHeight - DefaultItemHeight;
    SB_PAGEUP:     TopY := TopY - ClientHeight + DefaultItemHeight;
      // Scrolls to the current scroll bar position
    SB_THUMBPOSITION,
    SB_THUMBTRACK: TopY := Msg.Pos;
      // Ends scrolling
    SB_ENDSCROLL: ;
  end;
end;

procedure TOIPropertyGrid.WMSize(var Msg: TWMSize);
begin
  inherited;
  UpdateScrollBar;
end;

destructor TOIPropertyGrid.Destroy;
var a:integer;
begin
  FItemIndex:=-1;
  for a:=0 to FRows.Count-1 do Rows[a].Free;
  FreeAndNil(FRows);
  FreeAndNil(FComponentList);
  FreeAndNil(FValueFont);
  FreeAndNil(FNameFont);
  FreeAndNil(FExpandedProperties);
  FreeAndNil(FHintTimer);
  FreeAndNil(FHintWindow);
  inherited Destroy;
end;

procedure TOIPropertyGrid.SetSelections(
  const NewSelections:TComponentSelectionList);
var a:integer;
  CurRow:TOIPropertyGridRow;
  OldSelectedRowPath:string;
begin
  OldSelectedRowPath:=PropertyPath(ItemIndex);
  ItemIndex:=-1;
  for a:=0 to FRows.Count-1 do
    Rows[a].Free;
  FRows.Clear;
  FComponentList.Assign(NewSelections);
  BuildPropertyList;
  CurRow:=GetRowByPath(OldSelectedRowPath);
  if CurRow<>nil then
    ItemIndex:=CurRow.Index;
end;

procedure TOIPropertyGrid.SetPropertyEditorHook(
NewPropertyEditorHook:TPropertyEditorHook);
begin
  if FPropertyEditorHook=NewPropertyEditorHook then exit;
  FPropertyEditorHook:=NewPropertyEditorHook;
  SetSelections(FComponentList);
end;

function TOIPropertyGrid.PropertyPath(Index:integer):string;
var CurRow:TOIPropertyGridRow;
begin
  if (Index>=0) and (Index<FRows.Count) then begin
    CurRow:=Rows[Index];
    Result:=CurRow.Name;
    CurRow:=CurRow.Parent;
    while CurRow<>nil do begin
      Result:=CurRow.Name+'.'+Result;
      CurRow:=CurRow.Parent;
    end;
  end else Result:='';
end;

function TOIPropertyGrid.GetRowByPath(PropPath:string):TOIPropertyGridRow;
// searches PropPath. Expands automatically parent rows
var CurName:string;
  s,e:integer;
  CurParentRow:TOIPropertyGridRow;
begin
  Result:=nil;
  if FRows.Count=0 then exit;
  CurParentRow:=nil;
  s:=1;
  while (s<=length(PropPath)) do begin
    e:=s;
    while (e<=length(PropPath)) and (PropPath[e]<>'.') do inc(e);
    CurName:=uppercase(copy(PropPath,s,e-s));
    s:=e+1;
    // search name in childs
    if CurParentRow=nil then
      Result:=Rows[0]
    else
      Result:=CurParentRow.FirstChild;
    while (Result<>nil) and (uppercase(Result.Name)<>CurName) do
      Result:=Result.NextBrother;
    if Result=nil then begin
      exit;
    end else begin
      // expand row
      CurParentRow:=Result;
      ExpandRow(CurParentRow.Index);
    end;
  end;
  if s<=length(PropPath) then Result:=nil;
end;

procedure TOIPropertyGrid.SetRowValue;
var CurRow:TOIPropertyGridRow;
  NewValue:string;
begin
  if (FStates*[pgsChangingItemIndex,pgsApplyingValue]=[])
  and (FCurrentEdit<>nil)
  and (FItemIndex>=0) and (FItemIndex<FRows.Count) then begin
    CurRow:=Rows[FItemIndex];
    if FCurrentEdit=ValueEdit then
      NewValue:=ValueEdit.Text
    else
      NewValue:=ValueComboBox.Text;
    if length(NewValue)>CurRow.Editor.GetEditLimit then
      NewValue:=LeftStr(NewValue,CurRow.Editor.GetEditLimit);
    if NewValue<>CurRow.Editor.GetVisualValue then begin
      Include(FStates,pgsApplyingValue);
      try
        CurRow.Editor.SetValue(NewValue);
      except
        on E: Exception do begin
          MessageDlg('Error',E.Message,mtError,[mbOk],0);
        end;
      end;
      if FCurrentEdit=ValueEdit then
        ValueEdit.Text:=CurRow.Editor.GetVisualValue
      else
        ValueComboBox.Text:=CurRow.Editor.GetVisualValue;
      Exclude(FStates,pgsApplyingValue);
      DoPaint(true);
      if Assigned(FOnModified) then FOnModified(Self);
    end;
  end;
end;

procedure TOIPropertyGrid.ValueEditKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  case Key of
  VK_UP:
    if (FItemIndex>0) then ItemIndex:=ItemIndex-1;

  VK_Down:
    if (FItemIndex<FRows.Count-1) then ItemIndex:=ItemIndex+1;
    
  VK_RETURN:
    SetRowValue;

  end;
end;

procedure TOIPropertyGrid.ValueEditExit(Sender: TObject);
begin
  SetRowValue;
end;

procedure TOIPropertyGrid.ValueEditChange(Sender: TObject);
var CurRow:TOIPropertyGridRow;
begin
  if (FCurrentEdit<>nil) and (FItemIndex>=0) and (FItemIndex<FRows.Count) then
  begin
    CurRow:=Rows[FItemIndex];
    if paAutoUpdate in CurRow.Editor.GetAttributes then
      SetRowValue;
  end;
end;

procedure TOIPropertyGrid.ValueComboBoxExit(Sender: TObject);
begin
  SetRowValue;
end;

procedure TOIPropertyGrid.ValueComboBoxChange(Sender: TObject);
var i:integer;
begin
  i:=TComboBox(Sender).Items.IndexOf(TComboBox(Sender).Text);
  if i>=0 then SetRowValue;
end;

procedure TOIPropertyGrid.ValueComboBoxKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (Key=VK_UP) and (FItemIndex>0) then begin
    ItemIndex:=ItemIndex-1;
  end;
  if (Key=VK_Down) and (FItemIndex<FRows.Count-1) then begin
    ItemIndex:=ItemIndex+1;
  end;
end;

procedure TOIPropertyGrid.ValueButtonClick(Sender: TObject);
var CurRow:TOIPropertyGridRow;
begin
writeln('#################### TOIPropertyGrid.ValueButtonClick');
  if (FCurrentEdit<>nil) and (FItemIndex>=0) and (FItemIndex<FRows.Count) then
  begin
    CurRow:=Rows[FItemIndex];
    if paDialog in CurRow.Editor.GetAttributes then begin
      CurRow.Editor.Edit;
      if FCurrentEdit=ValueEdit then
        ValueEdit.Text:=CurRow.Editor.GetVisualValue
      else
        ValueComboBox.Text:=CurRow.Editor.GetVisualValue;
    end;
  end;
end;

procedure TOIPropertyGrid.SetItemIndex(NewIndex:integer);
var NewRow:TOIPropertyGridRow;
  NewValue:string;
begin
  SetRowValue;
  Include(FStates,pgsChangingItemIndex);
  if (FItemIndex<>NewIndex) then begin
    if (FItemIndex>=0) and (FItemIndex<FRows.Count) then
      Rows[FItemIndex].Editor.Deactivate;
    SetCaptureControl(nil);
    FItemIndex:=NewIndex;
    if FCurrentEdit<>nil then begin
      FCurrentEdit.Visible:=false;
      FCurrentEdit.Enabled:=false;
      FCurrentEdit:=nil;
    end;
    if FCurrentButton<>nil then begin
      FCurrentButton.Visible:=false;
      FCurrentButton.Enabled:=false;
      FCurrentButton:=nil;
    end;
    if (NewIndex>=0) and (NewIndex<FRows.Count) then begin
      NewRow:=Rows[NewIndex];
      NewRow.Editor.Activate;
writeln('  NewRow.Editor.ClassName=',NewRow.Editor.ClassName);
      if paDialog in NewRow.Editor.GetAttributes then begin
        FCurrentButton:=ValueButton;
        FCurrentButton.Visible:=true;
      end;
      NewValue:=NewRow.Editor.GetVisualValue;
      if paValueList in NewRow.Editor.GetAttributes then begin
        FCurrentEdit:=ValueComboBox;
        ValueComboBox.MaxLength:=NewRow.Editor.GetEditLimit;
        ValueComboBox.Items.BeginUpdate;
        ValueComboBox.Items.Text:='';
        ValueComboBox.Items.Clear;
        // XXX
        //ValueComboBox.Sorted:=paSortList in Node.Director.GetAttributes;
        NewRow.Editor.GetValues(@AddStringToComboBox);
        ValueComboBox.Text:=NewValue;
        ValueComboBox.Items.EndUpdate;
        ValueComboBox.Visible:=true;
      end else begin
        FCurrentEdit:=ValueEdit;
        // XXX
        ValueEdit.ReadOnly:=paReadOnly in NewRow.Editor.GetAttributes;
        ValueEdit.MaxLength:=NewRow.Editor.GetEditLimit;
        ValueEdit.Text:=NewValue;
        ValueEdit.Visible:=true;
      end;
      AlignEditComponents;
      if FCurrentEdit<>nil then begin
        FCurrentEdit.Enabled:=true;
        if (FDragging=false) and (FCurrentEdit.Showing) then begin
          FCurrentEdit.SetFocus;
        end;
      end;
      if FCurrentButton<>nil then
        FCurrentButton.Enabled:=true;
    end;
  end;
  Exclude(FStates,pgsChangingItemIndex);
end;

function TOIPropertyGrid.GetRowCount:integer;
begin
  Result:=FRows.Count;
end;

procedure TOIPropertyGrid.BuildPropertyList;
var a:integer;
  CurRow:TOIPropertyGridRow;
  OldSelectedRowPath:string;
begin
  OldSelectedRowPath:=PropertyPath(ItemIndex);
  ItemIndex:=-1;
  for a:=0 to FRows.Count-1 do Rows[a].Free;
  FRows.Clear;
  GetComponentProperties(FPropertyEditorHook,FComponentList,FFilter,
    @AddPropertyEditor);
    
  FRows.Sort(@SortGridRows);
  SetItemsTops;
  for a:=FExpandedProperties.Count-1 downto 0 do begin
    CurRow:=GetRowByPath(FExpandedProperties[a]);
    if CurRow<>nil then
      ExpandRow(CurRow.Index);
  end;
  CurRow:=GetRowByPath(OldSelectedRowPath);
  if CurRow<>nil then begin
    ItemIndex:=CurRow.Index;
  end;
  FTopY:=0;
  UpdateScrollBar;
  Invalidate;
end;

procedure TOIPropertyGrid.AddPropertyEditor(PropEditor: TPropertyEditor);
var NewRow:TOIPropertyGridRow;
begin
  NewRow:=TOIPropertyGridRow.Create(Self,PropEditor,nil);
  FRows.Add(NewRow);
  if FRows.Count>1 then begin
    NewRow.FPriorBrother:=Rows[FRows.Count-2];
    NewRow.FPriorBrother.FNextBrother:=NewRow;
  end;
end;

procedure TOIPropertyGrid.AddStringToComboBox(const s:string);
var NewIndex:integer;
begin
  NewIndex:=ValueComboBox.Items.Add(s);
  if ValueComboBox.Text=s then
    ValueComboBox.ItemIndex:=NewIndex;
end;

procedure TOIPropertyGrid.ExpandRow(Index:integer);
var a:integer;
  CurPath:string;
  AlreadyInExpandList:boolean;
begin
  FExpandingRow:=Rows[Index];
  if (FExpandingRow.Expanded)
  or (not (paSubProperties in FExpandingRow.Editor.GetAttributes))
  then begin
    FExpandingRow:=nil;
    exit;
  end;
  FExpandingRow.Editor.GetProperties(@AddSubEditor);
  SetItemsTops;
  FExpandingRow.FExpanded:=true;
  a:=0;
  CurPath:=uppercase(PropertyPath(FExpandingRow.Index));
  AlreadyInExpandList:=false;
  while a<FExpandedProperties.Count do begin
    if FExpandedProperties[a]=copy(CurPath,1,length(FExpandedProperties[a]))
    then begin
      if length(FExpandedProperties[a])=length(CurPath) then begin
        AlreadyInExpandList:=true;
        inc(a);
      end else begin
        FExpandedProperties.Delete(a);
      end;
    end else begin
      inc(a);
    end;
  end;
  if not AlreadyInExpandList then
    FExpandedProperties.Add(CurPath);
  FExpandingRow:=nil;
  UpdateScrollBar;
  Invalidate;
end;

procedure TOIPropertyGrid.ShrinkRow(Index:integer);
var CurRow, ARow:TOIPropertyGridRow;
  StartIndex,EndIndex,a:integer;
  CurPath:string;
begin
  CurRow:=Rows[Index];
  if (not CurRow.Expanded) then exit;
  EndIndex:=CurRow.Index+1;
  StartIndex:=FRows.Count-1;
  ARow:=CurRow;
  while ARow<>nil do begin
    if ARow.NextBrother<>nil then begin
      StartIndex:=ARow.NextBrother.Index-1;
      break;
    end;
    ARow:=ARow.Parent;
  end;
  for a:=StartIndex downto EndIndex do begin
    Rows[a].Free;
    FRows.Delete(a);
  end;
  SetItemsTops;
  CurRow.FExpanded:=false;
  CurPath:=uppercase(PropertyPath(CurRow.Index));
  a:=0;
  while a<FExpandedProperties.Count do begin
    if copy(FExpandedProperties[a],1,length(CurPath))=CurPath then
      FExpandedProperties.Delete(a)
    else
      inc(a);
  end;
  if CurRow.Parent<>nil then
    FExpandedProperties.Add(PropertyPath(CurRow.Parent.Index));
  UpdateScrollBar;
  Invalidate;
end;

procedure TOIPropertyGrid.AddSubEditor(PropEditor:TPropertyEditor);
var NewRow:TOIPropertyGridRow;
  NewIndex:integer;
begin
  NewRow:=TOIPropertyGridRow.Create(Self,PropEditor,FExpandingRow);
  NewIndex:=FExpandingRow.Index+1+FExpandingRow.ChildCount;
  FRows.Insert(NewIndex,NewRow);
  if FExpandingRow.FFirstChild=nil then
    FExpandingRow.FFirstChild:=NewRow;
  NewRow.FPriorBrother:=FExpandingRow.FLastChild;
  FExpandingRow.FLastChild:=NewRow;
  if NewRow.FPriorBrother<>nil then NewRow.FPriorBrother.FNextBrother:=NewRow;
  inc(FExpandingRow.FChildCount);
end;

function TOIPropertyGrid.MouseToIndex(y:integer;MustExist:boolean):integer;
var l,r,m:integer;
begin
  l:=0;
  r:=FRows.Count-1;
  inc(y,FTopY);
  while (l<=r) do begin
    m:=(l+r) shr 1;
    if Rows[m].Top>y then begin
      r:=m-1;
    end else if Rows[m].Bottom<=y then begin
      l:=m+1;
    end else begin
      Result:=m;  exit;
    end;
  end;
  if (MustExist=false) and (FRows.Count>0) then begin
    if y<0 then Result:=0
    else Result:=FRows.Count-1;
  end else Result:=-1;
end;

procedure TOIPropertyGrid.MouseDown(Button:TMouseButton;  Shift:TShiftState;
  X,Y:integer);
begin
  //ShowMessageDialog('X'+IntToStr(X)+',Y'+IntToStr(Y));
  inherited MouseDown(Button,Shift,X,Y);

  //hide the hint
  if FHintWindow.Visible then FHintWindow.Visible := False;
  
  if Button=mbLeft then begin
    if Cursor=crHSplit then begin
      FDragging:=true;
    end;
  end;
end;

procedure TOIPropertyGrid.MouseMove(Shift:TShiftState;  X,Y:integer);
var SplitDistance:integer;
begin
  inherited MouseMove(Shift,X,Y);
  
  SplitDistance:=X-SplitterX;
  if FDragging then begin
    if ssLeft in Shift then begin
      SplitterX:=SplitterX+SplitDistance;
    end else begin
      EndDragSplitter;
    end;
  end else begin
    if (abs(SplitDistance)<=2) then begin
      Cursor:=crHSplit;
    end else begin
      Cursor:=crDefault;
    end;
  end;
end;

procedure TOIPropertyGrid.MouseUp(Button:TMouseButton;  Shift:TShiftState;
  X,Y:integer);
var
  IconX,Index:integer;
  PointedRow:TOIpropertyGridRow;
  WasDragging: boolean;
begin
  WasDragging:=FDragging;
  if FDragging then EndDragSplitter;
  inherited MouseUp(Button,Shift,X,Y);

  if Button=mbLeft then begin
    if not WasDragging then begin
      Index:=MouseToIndex(Y,false);
      if (Index>=0) and (Index<FRows.Count) then begin
        IconX:=GetTreeIconX(Index);
        if (X>=IconX) and (X<=IconX+FIndent) then begin
          PointedRow:=Rows[Index];
          if paSubProperties in PointedRow.Editor.GetAttributes then begin
            if PointedRow.Expanded then
              ShrinkRow(Index)
            else
              ExpandRow(Index);
          end;
        end else begin
          ItemIndex:=Index;
        end;
      end;
    end;
  end;
end;

procedure TOIPropertyGrid.EndDragSplitter;
begin
  if FDragging then begin
    Cursor:=crDefault;
    FDragging:=false;
    FPreferredSplitterX:=FSplitterX;
    if FCurrentEdit<>nil then begin
      SetCaptureControl(nil);
      FCurrentEdit.SetFocus;
    end;
  end;
end;

procedure TOIPropertyGrid.SetSplitterX(const NewValue:integer);
var AdjustedValue:integer;
begin
  AdjustedValue:=NewValue;
  if AdjustedValue>ClientWidth then AdjustedValue:=ClientWidth;
  if AdjustedValue<1 then AdjustedValue:=1;
  if FSplitterX<>AdjustedValue then begin
    FSplitterX:=AdjustedValue;
    AlignEditComponents;
    Invalidate;
  end;
end;

procedure TOIPropertyGrid.SetTopY(const NewValue:integer);
begin
  if FTopY<>NewValue then begin
    FTopY:=NewValue;
    if FTopY<0 then FTopY:=0;
    UpdateScrollBar;
    ItemIndex:=-1;
    Invalidate;
  end;
end;

procedure TOIPropertyGrid.SetBounds(aLeft,aTop,aWidth,aHeight:integer);
begin
//writeln('[TOIPropertyGrid.SetBounds] ',Name,' ',aLeft,',',aTop,',',aWidth,',',aHeight,' Visible=',Visible);
  inherited SetBounds(aLeft,aTop,aWidth,aHeight);
  if Visible then begin
    if (SplitterX<5) and (aWidth>20) then
      SplitterX:=100
    else
      SplitterX:=FPreferredSplitterX;
    AlignEditComponents;
  end;
end;

function TOIPropertyGrid.GetTreeIconX(Index:integer):integer;
begin
  Result:=Rows[Index].Lvl*Indent+2;
end;

function TOIPropertyGrid.TopMax:integer;
begin
  Result:=GridHeight-ClientHeight+2*BorderWidth;
  if Result<0 then Result:=0;
end;

function TOIPropertyGrid.GridHeight:integer;
begin
  if FRows.Count>0 then
    Result:=Rows[FRows.Count-1].Bottom
  else
    Result:=0;
end;

procedure TOIPropertyGrid.AlignEditComponents;
var RRect,EditCompRect,EditBtnRect:TRect;

  function CompareRectangles(r1,r2:TRect):boolean;
  begin
    Result:=(r1.Left=r2.Left) and (r1.Top=r2.Top) and (r1.Right=r2.Right)
       and (r1.Bottom=r2.Bottom);
  end;

// AlignEditComponents
begin
  if ItemIndex>=0 then begin
    RRect:=RowRect(ItemIndex);
    EditCompRect:=RRect;
    EditCompRect.Top:=EditCompRect.Top-1;
    EditCompRect.Left:=RRect.Left+SplitterX;
    if FCurrentButton<>nil then begin
      // edit dialog button
      with EditBtnRect do begin
        Top:=RRect.Top;
        Left:=RRect.Right-20;
        Bottom:=RRect.Bottom;
        Right:=RRect.Right;
        EditCompRect.Right:=Left;
      end;
      if not CompareRectangles(FCurrentButton.BoundsRect,EditBtnRect) then begin
        FCurrentButton.BoundsRect:=EditBtnRect;
        //FCurrentButton.Invalidate;
      end;
    end;
    if FCurrentEdit<>nil then begin
      // edit component
      EditCompRect.Left:=EditCompRect.Left-1;
      // XXX
      //
      EditCompRect.Bottom:=EditCompRect.Top+FCurrentEdit.Height;
      if not CompareRectangles(FCurrentEdit.BoundsRect,EditCompRect) then begin
        FCurrentEdit.BoundsRect:=EditCompRect;
        FCurrentEdit.Invalidate;
      end;
    end;
  end;
end;

procedure TOIPropertyGrid.PaintRow(ARow:integer);
var ARowRect,NameRect,NameIconRect,NameTextRect,ValueRect:TRect;
  IconX,IconY:integer;
  CurRow:TOIPropertyGridRow;
  DrawState:TPropEditDrawState;
  OldFont:TFont;

  procedure DrawTreeIcon(x,y:integer;Plus:boolean);
  begin
    with Canvas do begin
      Brush.Color:=clWhite;
      Pen.Color:=clBlack;
      Rectangle(x,y,x+8,y+8);
      MoveTo(x+2,y+4);
      LineTo(x+7,y+4);
      if Plus then begin
        MoveTo(x+4,y+2);
        LineTo(x+4,y+7);
      end;
    end;
  end;

// PaintRow
begin
  CurRow:=Rows[ARow];
  ARowRect:=RowRect(ARow);
  NameRect:=ARowRect;
  ValueRect:=ARowRect;
  NameRect.Right:=SplitterX;
  ValueRect.Left:=SplitterX;
  IconX:=GetTreeIconX(ARow);
  IconY:=((NameRect.Bottom-NameRect.Top-9) div 2)+NameRect.Top;
  NameIconRect:=NameRect;
  NameIconRect.Right:=IconX+Indent;
  NameTextRect:=NameRect;
  NameTextRect.Left:=NameIconRect.Right;
  DrawState:=[];
  if ARow=FItemIndex then Include(DrawState,pedsSelected);
  with Canvas do begin
    // draw name background
    if FBackgroundColor<>clNone then begin
      Brush.Color:=FBackgroundColor;
      FillRect(NameIconRect);
      FillRect(NameTextRect);
    end;
    // draw icon
    if paSubProperties in CurRow.Editor.GetAttributes then begin
      DrawTreeIcon(IconX,IconY,not CurRow.Expanded);
    end;
    // draw name
    OldFont:=Font;
    Font:=FNameFont;
    CurRow.Editor.PropDrawName(Canvas,NameTextRect,DrawState);
    Font:=OldFont;
    // draw frame
    Pen.Color:=cl3DDkShadow;
    MoveTo(NameRect.Left,NameRect.Bottom-1);
    LineTo(NameRect.Right-1,NameRect.Bottom-1);
    LineTo(NameRect.Right-1,NameRect.Top-1);
    if ARow=FItemIndex then begin
      Pen.Color:=cl3DDkShadow;
      MoveTo(NameRect.Left,NameRect.Bottom-1);
      LineTo(NameRect.Left,NameRect.Top);
      LineTo(NameRect.Right-1,NameRect.Top);
      Pen.Color:=cl3DLight;
      MoveTo(NameRect.Left+1,NameRect.Bottom-2);
      LineTo(NameRect.Right-1,NameRect.Bottom-2);
    end;
    // draw value background
    if FBackgroundColor<>clNone then begin
      Brush.Color:=FBackgroundColor;
      FillRect(ValueRect);
    end;
    // draw value
    if ARow<>ItemIndex then begin
      OldFont:=Font;
      Font:=FValueFont;
      CurRow.Editor.PropDrawValue(Canvas,ValueRect,DrawState);
      Font:=OldFont;
    end;
    CurRow.LastPaintedValue:=CurRow.Editor.GetVisualValue;
    // draw frame
    Pen.Color:=cl3DDkShadow;
    MoveTo(ValueRect.Left-1,ValueRect.Bottom-1);
    LineTo(ValueRect.Right,ValueRect.Bottom-1);
    Pen.Color:=cl3DLight;
    MoveTo(ValueRect.Left,ValueRect.Bottom-1);
    LineTo(ValueRect.Left,ValueRect.Top);
    if ARow=FItemIndex then begin
      MoveTo(ValueRect.Left,ValueRect.Bottom-2);
      LineTo(ValueRect.Right,ValueRect.Bottom-2);
    end;
  end;
end;

procedure TOIPropertyGrid.DoPaint(PaintOnlyChangedValues:boolean);
var a:integer;
  SpaceRect:TRect;
begin
  if not PaintOnlyChangedValues then begin
    with Canvas do begin
      // draw properties
      for a:=0 to FRows.Count-1 do begin
        PaintRow(a);
      end;
      // draw unused space below rows
      SpaceRect:=Rect(BorderWidth,BorderWidth,
                      ClientWidth-BorderWidth+1,ClientHeight-BorderWidth+1);
      if FRows.Count>0 then
        SpaceRect.Top:=Rows[FRows.Count-1].Bottom-FTopY+BorderWidth;
// TWinControl(Parent).InvalidateRect(Self,SpaceRect,true);
      if FBackgroundColor<>clNone then begin
        Brush.Color:=FBackgroundColor;
        FillRect(SpaceRect);
      end;
      // draw border
      Pen.Color:=cl3DDkShadow;
      for a:=0 to BorderWidth-1 do begin
        MoveTo(a,Self.Height-1-a);
        LineTo(a,a);
        LineTo(Self.Width-1-a,a);
      end;
      Pen.Color:=cl3DLight;
      for a:=0 to BorderWidth-1 do begin
        MoveTo(Self.Width-1-a,a);
        LineTo(Self.Width-1-a,Self.Height-1-a);
        LineTo(a,Self.Height-1-a);
      end;
    end;
  end else begin
    for a:=0 to FRows.Count-1 do begin
      if Rows[a].Editor.GetVisualValue<>Rows[a].LastPaintedValue then
        PaintRow(a);
    end;
  end;
end;

procedure TOIPropertyGrid.Paint;
begin
  inherited Paint;
  DoPaint(false);
end;

procedure TOIPropertyGrid.RefreshPropertyValues;
begin
  DoPaint(true);
end;

function TOIPropertyGrid.RowRect(ARow:integer):TRect;
begin
  Result.Left:=BorderWidth;
  Result.Top:=Rows[ARow].Top-FTopY+BorderWidth;
  Result.Right:=ClientWidth-ScrollBarWidth;
  Result.Bottom:=Rows[ARow].Bottom-FTopY+BorderWidth;
end;

procedure TOIPropertyGrid.SetItemsTops;
// compute row tops from row heights
// set indices of all rows
var a,scrollmax:integer;
begin
  for a:=0 to FRows.Count-1 do
    Rows[a].Index:=a;
  if FRows.Count>0 then
    Rows[0].Top:=0;
  for a:=1 to FRows.Count-1 do
    Rows[a].FTop:=Rows[a-1].Bottom;
  if FRows.Count>0 then
    scrollmax:=Rows[FRows.Count-1].Bottom-Height
  else
    scrollmax:=10;
  if scrollmax<10 then scrollmax:=10;
end;

procedure TOIPropertyGrid.ClearRows;
var a:integer;
begin
  for a:=0 to FRows.Count-1 do begin
    Rows[a].Free;
  end;
  FRows.Clear;
end;

procedure TOIPropertyGrid.Clear;
begin
  ClearRows;
end;

function TOIPropertyGrid.GetRow(Index:integer):TOIPropertyGridRow;
begin
  Result:=TOIPropertyGridRow(FRows[Index]);
end;


Procedure TOIPropertyGrid.HintTimer(sender : TObject);
var
  Rect : TRect;
  AHint : String;
  Position : TPoint;
  Index: integer;
  PointedRow:TOIpropertyGridRow;
  TypeKind : TTypeKind;
  Window :TWinControl;
  Window2 : TWInControl;
begin
  FHintTimer.Enabled := False;
  if not ShowHint then exit;

  Position := Mouse.CursorPos;
  Window := FindLCLWindow(Position);
  if not(Assigned(window)) then Exit;

  //get the parent until parent is nil
  While Window.Parent <> nil do
    Window := Window.Parent;

  Window2 := self;
  while Window2.Parent <> nil do
    Window2 := Window2.Parent;
     
  if (window <> Window2) then Exit;

  if ( (FLastMouseMovePos.X <= 0) or (FLastMouseMOvePos.Y <= 0)
  or (FLastMouseMovePos.X >= Width) or (FLastMouseMovePos.Y >= Height)) then
    Exit;

  Position := ScreenToClient(Position);
  if ((Position.X <=0) or (Position.X >= Width) or (Position.Y <= 0)
  or (Position.Y >= Height)) then
    Exit;
  AHint := '';
  Index:=MouseToIndex(Position.Y,false);
  if (Index>=0) and (Index<FRows.Count) then
     begin
        //IconX:=GetTreeIconX(Index);
        PointedRow:=Rows[Index];
        if Assigned(PointedRow) then
        Begin
          if Assigned(PointedRow.Editor) then
            AHint := PointedRow.Editor.GetName;

          AHint := AHint + #10'Value:'+PointedRow.LastPaintedValue;
          TypeKind := PointedRow.Editor.GetPropType^.Kind;
          case TypeKind of
                 tkInteger : AHint := AHInt + #10'Integer';
                 tkInt64 : AHint := AHInt + #10'Int64';
                 tkBool : AHint := AHInt + #10'Boolean';
                 tkEnumeration : AHint := AHInt + #10'Enumeration';
                 tkChar,tkWChar : AHint := AHInt + #10'Char';
                 tkUnknown : AHint := AHInt + #10'Unknown';
                 tkObject : AHint := AHInt + #10'Object';
                 tkClass : AHint := AHInt + #10'Class';
                 tkQWord : AHint := AHInt + #10'Word';
                 tkString,tkLString,tkAString,tkWString : AHint := AHInt + #10'String';
                 tkFloat : AHint := AHInt + #10'Float';
                 tkSet : AHint := AHInt + #10'Set';
                 tkMethod : AHint := AHInt + #10'Method';
                 tkVariant : AHint := AHInt + #10'Variant';
                 tkArray : AHint := AHInt + #10'Array';
                 tkRecord : AHint := AHInt + #10'Record';
                 tkInterface : AHint := AHInt + #10'Interface';
           end;
         end;
     end;
  if AHint = '' then Exit;
  Rect := FHintWindow.CalcHintRect(0,AHint,nil);  //no maxwidth
  Position := Mouse.CursorPos;
  Rect.Left := Position.X+10;
  Rect.Top := Position.Y+10;
  Rect.Right := Rect.Left + Rect.Right+3;
  Rect.Bottom := Rect.Top + Rect.Bottom+3;

  FHintWindow.ActivateHint(Rect,AHint);
end;

Procedure TOIPropertyGrid.ResetHintTimer(Sender : TObject; Shift: TShiftstate; X,Y : Integer);
begin
  if FHintWIndow.Visible then
     FHintWindow.Visible := False;
     
  FHintTimer.Enabled := False;
  FHintTimer.Enabled := not ((ssLeft in Shift) or (ssRight in Shift) or (ssMiddle in Shift));

  if (Sender is TOIPropertyGrid) then
    Begin
       FLastMouseMovePos.X := X;
       FLastMouseMovePos.Y := Y;
    end
    else
    begin  //account for the controls position.  THis is used for the FCurrentEdit control
       FLastMouseMovePos.X := TWinControl(sender).Left+X;
       FLastMouseMovePos.Y := TWinControl(Sender).Top+Y;
    end;
end;

procedure TOIPropertyGrid.WMMouseMove(var Msg: TWMMouseMove);
var
  Shift : TShiftState;
begin
  inherited;
  Shift := KeystoShiftState(Msg.Keys);  //KeystoShiftState found in Forms.pp
  
  ResetHintTimer(self,Shift,Msg.pos.x,Msg.Pos.Y);
end;

procedure TOIPropertyGrid.CurrentEditMouseDown(Sender : TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: integer);
begin
  //hide the hint window!
  if FHintWindow.Visible then FHintWindow.Visible := False;
end;

PRocedure TOIPropertyGrid.CurrentEditDblClick(Sender : TObject);
var
  //Rect : TRect;
  Position : TPoint;
  Index: integer;
  PointedRow:TOIpropertyGridRow;
  TypeKind : TTypeKind;
  Temp : String;
begin
  FHintTimer.Enabled := False;
  //if event, then either create it or go to it.
  //if Edit box, then nothing
  //if combobox, then select the next value

  if (sender is TComboBox) then
     Begin  //either an Event of a enumeration or Boolean

      Position := Mouse.CursorPos;
      if ( (FLastMouseMovePos.X <= 0) or (FLastMouseMOvePos.Y <= 0)
         or (FLastMouseMovePos.X >= Width) or (FLastMouseMovePos.Y >= Height)) then
      Exit;

      Position := ScreenToClient(Position);
      if ((Position.X <=0) or (Position.X >= Width) or (Position.Y <= 0)
         or (Position.Y >= Height)) then
      Exit;

      Index:=MouseToIndex(Position.Y,false);
      if (Index>=0) and (Index<FRows.Count) then
       begin
        PointedRow:=Rows[Index];
        if Assigned(PointedRow) then
        Begin

          TypeKind := PointedRow.Editor.GetPropType^.Kind;
          case TypeKind of
               tkMethod : begin
                          //event
                          //if blank then create event!
                          if TComboBox(sender).Text = '' then
                             Begin
                                //see if it's in the list first.
                                Temp := TMethodPropertyEditor(PointedRow.Editor).GetFormMethodName;
                                if (TComboBox(sender).Items.Indexof(Temp) = -1) then
                                    TComboBox(sender).Items.Add(Temp);
                                //set the text
                                TComboBox(sender).Text := Temp;
                             end
                             else  //jump to event!
                             Begin
                             
                             end;
                             
                          end;
                else
                   Begin
                     if TComboBox(Sender).Items.Count = 0 then Exit;
                     if TComboBox(sender).ItemIndex < (TComboBox(sender).Items.Count-1) then
                          TComboBox(sender).ItemIndex := TComboBox(sender).ItemIndex +1
                          else
                          TComboBox(sender).ItemIndex := 0;
                   end;
           end;
         end;
     end;
     end;
end;

procedure TOIPropertyGrid.SetBackgroundColor(const AValue: TColor);
begin
  if FBackgroundColor=AValue then exit;
  FBackgroundColor:=AValue;
  Invalidate;
end;

//------------------------------------------------------------------------------

{ TOIPropertyGridRow }

constructor TOIPropertyGridRow.Create(PropertyTree:TOIPropertyGrid;
PropEditor:TPropertyEditor;  ParentNode:TOIPropertyGridRow);
begin
  inherited Create;
  // tree pointer
  FTree:=PropertyTree;
  FParent:=ParentNode;
  FNextBrother:=nil;
  FPriorBrother:=nil;
  FExpanded:=false;
  // child nodes
  FChildCount:=0;
  FFirstChild:=nil;
  FLastChild:=nil;
  // director
  FEditor:=PropEditor;
  GetLvl;
  FName:=FEditor.GetName;
  FTop:=0;
  FHeight:=FTree.DefaultItemHeight;
  Index:=-1;
  LastPaintedValue:='';
end;

destructor TOIPropertyGridRow.Destroy;
begin
  if FPriorBrother<>nil then FPriorBrother.FNextBrother:=FNextBrother;
  if FNextBrother<>nil then FNextBrother.FPriorBrother:=FPriorBrother;
  if FParent<>nil then begin
    if FParent.FFirstChild=Self then FParent.FFirstChild:=FNextBrother;
    if FParent.FLastChild=Self then FParent.FLastChild:=FPriorBrother;
    dec(FParent.FChildCount);
  end;
  if FEditor<>nil then FEditor.Free;
  inherited Destroy;
end;

procedure TOIPropertyGridRow.GetLvl;
var n:TOIPropertyGridRow;
begin
  FLvl:=0;
  n:=FParent;
  while n<>nil do begin
    inc(FLvl);
    n:=n.FParent;
  end;
end;

function TOIPropertyGridRow.Bottom:integer;
begin
  Result:=FTop+FHeight;
end;

//==============================================================================


{ TOIOptions }

constructor TOIOptions.Create;
begin
  inherited Create;
  FFilename:='';

  FSaveBounds:=false;
  FLeft:=0;
  FTop:=0;
  FWidth:=250;
  FHeight:=400;
  FPropertyGridSplitterX:=110;
  FEventGridSplitterX:=110;

  FGridBackgroundColor:=clBtnFace;
end;

destructor TOIOptions.Destroy;
begin

  inherited Destroy;
end;

function TOIOptions.Load:boolean;
var XMLConfig: TXMLConfig;
begin
  Result:=false;
  if not FileExists(FFilename) then exit;
  try
    XMLConfig:=TXMLConfig.Create(FFileName);
    try
      FSaveBounds:=XMLConfig.GetValue('ObjectInspectorOptions/Bounds/Valid'
                       ,false);
      if FSaveBounds then begin
        FLeft:=XMLConfig.GetValue('ObjectInspectorOptions/Bounds/Left',0);
        FTop:=XMLConfig.GetValue('ObjectInspectorOptions/Bounds/Top',0);
        FWidth:=XMLConfig.GetValue('ObjectInspectorOptions/Bounds/Width',250);
        FHeight:=XMLConfig.GetValue('ObjectInspectorOptions/Bounds/Height',400);
      end;
      FPropertyGridSplitterX:=XMLConfig.GetValue(
         'ObjectInspectorOptions/Bounds/PropertyGridSplitterX',110);
      if FPropertyGridSplitterX<10 then FPropertyGridSplitterX:=10;
      FEventGridSplitterX:=XMLConfig.GetValue(
         'ObjectInspectorOptions/Bounds/EventGridSplitterX',110);
      if FEventGridSplitterX<10 then FEventGridSplitterX:=10;

      FGridBackgroundColor:=XMLConfig.GetValue(
           'ObjectInspectorOptions/GridBackgroundColor',clBtnFace);
      FShowHints:=XMLConfig.GetValue(
           'ObjectInspectorOptions/ShowHints',false);
    finally
      XMLConfig.Free;
    end;
  except
    exit;
  end;
  Result:=true;
end;

function TOIOptions.Save:boolean;
var XMLConfig: TXMLConfig;
begin
  Result:=false;
  try
    XMLConfig:=TXMLConfig.Create(FFileName);
    try
      XMLConfig.SetValue('ObjectInspectorOptions/Bounds/Valid',FSaveBounds);
      if FSaveBounds then begin
        XMLConfig.SetValue('ObjectInspectorOptions/Bounds/Left',FLeft);
        XMLConfig.SetValue('ObjectInspectorOptions/Bounds/Top',FTop);
        XMLConfig.SetValue('ObjectInspectorOptions/Bounds/Width',FWidth);
        XMLConfig.SetValue('ObjectInspectorOptions/Bounds/Height',FHeight);
      end;
      XMLConfig.SetValue(
         'ObjectInspectorOptions/Bounds/PropertyGridSplitterX'
         ,FPropertyGridSplitterX);
      XMLConfig.SetValue(
         'ObjectInspectorOptions/Bounds/EventGridSplitterX'
         ,FEventGridSplitterX);

      XMLConfig.SetValue('ObjectInspectorOptions/GridBackgroundColor'
         ,FGridBackgroundColor);
      XMLConfig.SetValue('ObjectInspectorOptions/ShowHints',FShowHints);
    finally
      XMLConfig.Flush;
      XMLConfig.Free;
    end;
  except
    exit;
  end;
  Result:=true;
end;

procedure TOIOptions.Assign(AnObjInspector: TObjectInspector);
begin
  FLeft:=AnObjInspector.Left;
  FTop:=AnObjInspector.Top;
  FWidth:=AnObjInspector.Width;
  FHeight:=AnObjInspector.Height;
  FPropertyGridSplitterX:=AnObjInspector.PropertyGrid.PrefferedSplitterX;
  FEventGridSplitterX:=AnObjInspector.EventGrid.PrefferedSplitterX;
  FGridBackgroundColor:=AnObjInspector.PropertyGrid.BackgroundColor;
  FShowHints:=AnObjInspector.PropertyGrid.ShowHint;
end;

procedure TOIOptions.AssignTo(AnObjInspector: TObjectInspector);
begin
  if FSaveBounds then begin
    AnObjInspector.SetBounds(FLeft,FTop,FWidth,FHeight);
  end;
  AnObjInspector.PropertyGrid.PrefferedSplitterX:=FPropertyGridSplitterX;
  AnObjInspector.PropertyGrid.SplitterX:=FPropertyGridSplitterX;
  AnObjInspector.EventGrid.PrefferedSplitterX:=FEventGridSplitterX;
  AnObjInspector.EventGrid.SplitterX:=FEventGridSplitterX;
  AnObjInspector.PropertyGrid.BackgroundColor:=FGridBackgroundColor;
  AnObjInspector.PropertyGrid.ShowHint:=FShowHints;
  AnObjInspector.EventGrid.BackgroundColor:=FGridBackgroundColor;
  AnObjInspector.EventGrid.ShowHint:=FShowHints;
end;


//==============================================================================


{ TObjectInspector }

constructor TObjectInspector.Create(AnOwner: TComponent);

  procedure AddPopupMenuItem(var NewMenuItem:TmenuItem;
     ParentMenuItem:TMenuItem; AName,ACaption,AHint:string;
     AOnClick: TNotifyEvent; CheckedFlag,EnabledFlag,VisibleFlag:boolean);
  begin
    NewMenuItem:=TMenuItem.Create(Self);
    with NewMenuItem do begin
      Name:=AName;
      Caption:=ACaption;
      Hint:=AHint;
      OnClick:=AOnClick;
      Checked:=CheckedFlag;
      Enabled:=EnabledFlag;
      Visible:=VisibleFlag;
    end;
    if ParentMenuItem<>nil then
      ParentMenuItem.Add(NewmenuItem)
    else
      MainPopupMenu.Items.Add(NewMenuItem);
  end;

begin
  inherited Create(AnOwner);
  Caption := 'Object Inspector';
  FPropertyEditorHook:=nil;
  FComponentList:=TComponentSelectionList.Create;
  FUpdatingAvailComboBox:=false;
  Name := DefaultObjectInspectorName;

  // StatusBar
  StatusBar:=TStatusBar.Create(Self);
  with StatusBar do begin
    Name:='StatusBar';
    Parent:=Self;
    SimpleText:='All';
    Align:= alBottom;
    Visible:=true;
  end;

  // PopupMenu
  MainPopupMenu:=TPopupMenu.Create(Self);
  with MainPopupMenu do begin
    Name:='MainPopupMenu';
    AutoPopup:=true;
  end;
  AddPopupMenuItem(ColorsPopupmenuItem,nil,'ColorsPopupMenuItem','Colors',''
     ,nil,false,true,true);
  AddPopupMenuItem(BackgroundColPopupMenuItem,ColorsPopupMenuItem
     ,'BackgroundColPopupMenuItem','Background','Grid background color'
     ,@OnBackgroundColPopupMenuItemClick,false,true,true);
  AddPopupMenuItem(ShowHintsPopupMenuItem,nil
     ,'ShowHintPopupMenuItem','Show Hints','Grid hints'
     ,@OnShowHintPopupMenuItemClick,false,true,true);

  PopupMenu:=MainPopupMenu;

  // combobox at top (filled with available components)
  AvailCompsComboBox := TComboBox.Create (Self);
  with AvailCompsComboBox do begin
    Name:='AvailCompsComboBox';
    Parent:=Self;
    Style:=csDropDown;
    Text:='';
    OnChange:=@AvailComboBoxChange;
    //Sorted:=true;
    Align:= alTop;
    Visible:=true;
  end;

  // NoteBook
  NoteBook:=TNoteBook.Create(Self);
  with NoteBook do begin
    Name:='NoteBook';
    Parent:=Self;
    if PageCount>0 then
      Pages.Strings[0]:='Properties'
    else
      Pages.Add('Properties');
    Pages.Add('Events');
    PopupMenu:=MainPopupMenu;
    Align:= alClient;
    Visible:=true;
  end;

  // property grid
  PropertyGrid:=TOIPropertyGrid.Create(Self,PropertyEditorHook
      ,[tkUnknown, tkInteger, tkChar, tkEnumeration, tkFloat, tkSet{, tkMethod}
      , tkSString, tkLString, tkAString, tkWString, tkVariant
      {, tkArray, tkRecord, tkInterface}, tkClass, tkObject, tkWChar, tkBool
      , tkInt64, tkQWord]);
  with PropertyGrid do begin
    Name:='PropertyGrid';
    Parent:=NoteBook.Page[0];
    ValueEdit.Parent:=Parent;
    ValueComboBox.Parent:=Parent;
    ValueButton.Parent:=Parent;
    Selections:=Self.FComponentList;
    Align:=alClient;
    PopupMenu:=MainPopupMenu;
    OnModified:=@OnGridModified;
    Visible:=true;
  end;

  // event grid
  EventGrid:=TOIPropertyGrid.Create(Self,PropertyEditorHook,[tkMethod]);
  with EventGrid do begin
    Name:='EventGrid';
    Parent:=NoteBook.Page[1];
    ValueEdit.Parent:=Parent;
    ValueComboBox.Parent:=Parent;
    ValueButton.Parent:=Parent;
    Selections:=Self.FComponentList;
    Align:=alClient;
    PopupMenu:=MainPopupMenu;
    OnModified:=@OnGridModified;
    Visible:=true;
  end;

end;

destructor TObjectInspector.Destroy;
begin
  FComponentList.Free;
  inherited Destroy;
end;

procedure TObjectInspector.SetPropertyEditorHook(NewValue:TPropertyEditorHook);
begin
  if FPropertyEditorHook<>NewValue then begin
    FPropertyEditorHook:=NewValue;
    FPropertyEditorHook.OnChangeLookupRoot:=@PropEditLookupRootChange;
    // select root component
    FComponentList.Clear;
    if (FPropertyEditorHook<>nil) and (FPropertyEditorHook.LookupRoot<>nil) then
      FComponentList.Add(FPropertyEditorHook.LookupRoot);
    FillComponentComboBox;
    PropertyGrid.PropertyEditorHook:=FPropertyEditorHook;
    EventGrid.PropertyEditorHook:=FPropertyEditorHook;
    RefreshSelections;
  end;
end;

function TObjectInspector.ComponentToString(c: TComponent): string;
begin
  Result:=c.GetNamePath+': '+c.ClassName;
end;

procedure TObjectInspector.AddComponentToAvailComboBox(AComponent: TComponent);
var Allowed:boolean;
begin
  Allowed:=true;
  if Assigned(FOnAddAvailableComponent) then
    FOnAddAvailableComponent(AComponent,Allowed);
  if Allowed then
    AvailCompsComboBox.Items.AddObject(
      ComponentToString(AComponent),AComponent);
end;

procedure TObjectInspector.PropEditLookupRootChange;
begin
  FillComponentComboBox;
end;

procedure TObjectInspector.FillComponentComboBox;
var a:integer;
  Root:TComponent;
  OldText:AnsiString;
begin
//writeln('[TObjectInspector.FillComponentComboBox] A ',FUpdatingAvailComboBox
//,' ',FPropertyEditorHook<>nil,'  ',FPropertyEditorHook.LookupRoot<>nil);
  if FUpdatingAvailComboBox then exit;
  FUpdatingAvailComboBox:=true;
  AvailCompsComboBox.Items.BeginUpdate;
  OldText:=AvailCompsComboBox.Text;
  AvailCompsComboBox.Items.Clear;
  if (FPropertyEditorHook<>nil)
  and (FPropertyEditorHook.LookupRoot<>nil) then begin
    Root:=FPropertyEditorHook.LookupRoot;
    AddComponentToAvailComboBox(Root);
//writeln('[TObjectInspector.FillComponentComboBox] B  ',Root.Name,'  ',Root.ComponentCount);
    for a:=0 to Root.ComponentCount-1 do
      AddComponentToAvailComboBox(Root.Components[a]);
  end;
  AvailCompsComboBox.Items.EndUpdate;
  FUpdatingAvailComboBox:=false;
  a:=AvailCompsComboBox.Items.IndexOf(OldText);
  if (OldText='') or (a<0) then begin
    if AvailCompsComboBox.Items.Count>0 then
      AvailCompsComboBox.Text:=AvailCompsComboBox.Items[0]
    else
      AvailCompsComboBox.Text:='';
  end else
    AvailCompsComboBox.ItemIndex:=a;
end;

procedure TObjectInspector.SetSelections(
  const NewSelections:TComponentSelectionList);
begin
//writeln('[TObjectInspector.SetSelections]');
  if FComponentList.IsEqual(NewSelections) then exit;
  FComponentList.Assign(NewSelections);
  if FComponentList.Count=1 then begin
    AvailCompsComboBox.Text:=ComponentToString(FComponentList[0]);
  end else begin
    AvailCompsComboBox.Text:='';
  end;
  RefreshSelections;
end;

procedure TObjectInspector.RefreshSelections;
begin
  PropertyGrid.Selections:=FComponentList;
  EventGrid.Selections:=FComponentList;
  if (not Visible) and (FComponentList.Count>0) then
    Visible:=true;
end;

procedure TObjectInspector.RefreshPropertyValues;
begin
  PropertyGrid.RefreshPropertyValues;
  EventGrid.RefreshPropertyValues;
end;

procedure TObjectInspector.AvailComboBoxChange(Sender:TObject);
var NewComponent,Root:TComponent;
  a:integer;

  procedure SetSelectedComponent(c:TComponent);
  begin
    if (FComponentList.Count=1) and (FComponentList[0]=c) then exit;
    FComponentList.Clear;
    FComponentList.Add(c);
    RefreshSelections;
    if Assigned(FOnSelectComponentInOI) then
      FOnSelectComponentInOI(c);
  end;

// AvailComboBoxChange
begin
  if FUpdatingAvailComboBox then exit;
  if (FPropertyEditorHook=nil) or (FPropertyEditorHook.LookupRoot=nil) then
    exit;
  Root:=FPropertyEditorHook.LookupRoot;
  if AvailCompsComboBox.Text=ComponentToString(Root)
  then begin
    SetSelectedComponent(Root);
  end else begin
    for a:=0 to Root.ComponentCount-1 do begin
      NewComponent:=Root.Components[a];
      if AvailCompsComboBox.Text=ComponentToString(NewComponent) then begin
        SetSelectedComponent(NewComponent);
        break;
      end;
    end;
  end;
end;

procedure TObjectInspector.OnBackgroundColPopupMenuItemClick(Sender :TObject);
var ColorDialog:TColorDialog;
begin
  ColorDialog:=TColorDialog.Create(Application);
  try
    with ColorDialog do begin
      Color:=PropertyGrid.BackgroundColor;
      if Execute then begin
        PropertyGrid.BackgroundColor:=Color;
        EventGrid.BackgroundColor:=Color;
        PropertyGrid.Invalidate;
        EventGrid.Invalidate;
      end;
    end;
  finally
    ColorDialog.Free;
  end;
end;

procedure TObjectInspector.OnGridModified(Sender: TObject);
begin
  if Assigned(FOnModified) then FOnModified(Self);
end;

procedure TObjectInspector.OnShowHintPopupMenuItemClick(Sender : TObject);
begin
  PropertyGrid.ShowHint:=not PropertyGrid.ShowHint;
  EventGrid.ShowHint:=not EventGrid.ShowHint;
end;

end.

