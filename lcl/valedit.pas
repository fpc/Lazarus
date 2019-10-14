unit ValEdit;

{$mode objfpc}{$H+}

interface

uses
  ContNrs, SysUtils, Classes, Variants,
  LazUtf8, Controls, StdCtrls, Grids, LResources, Dialogs, LCLType, Laz2_XMLCfg;

type

  TValueListEditor = class;    // Forward declaration
  TValueListStrings = class;

  TEditStyle = (esSimple, esEllipsis, esPickList);
  TVleSortCol = (colKey, colValue);

  { TItemProp }

  TItemProp = class(TPersistent)
  private
    FGrid: TValueListEditor;
    FEditMask: string;
    FEditStyle: TEditStyle;
    FPickList: TStrings;
    FMaxLength: Integer;
    FReadOnly: Boolean;
    FKeyDesc: string;
    function GetPickList: TStrings;
    procedure PickListChange(Sender: TObject);
    procedure SetEditMask(const AValue: string);
    procedure SetMaxLength(const AValue: Integer);
    procedure SetReadOnly(const AValue: Boolean);
    procedure SetEditStyle(const AValue: TEditStyle);
    procedure SetPickList(const AValue: TStrings);
    procedure SetKeyDesc(const AValue: string);
  protected
    procedure AssignTo(Dest: TPersistent); override;
  public
    constructor Create(AOwner: TValueListEditor);
    destructor Destroy; override;
//    function HasPickList: Boolean;
  published
    property EditMask: string read FEditMask write SetEditMask;
    property EditStyle: TEditStyle read FEditStyle write SetEditStyle;
    property KeyDesc: string read FKeyDesc write SetKeyDesc;
    property PickList: TStrings read GetPickList write SetPickList;
    property MaxLength: Integer read FMaxLength write SetMaxLength;
    property ReadOnly: Boolean read FReadOnly write SetReadOnly;
  end;

  { TItemPropList }

  TItemPropList = class
  private
    FList: TFPObjectList;
    FStrings: TValueListStrings;
    function GetCount: Integer;
    function GetItem(Index: Integer): TItemProp;
    procedure SetItem(Index: Integer; AValue: TItemProp);
  protected
  public
    procedure Add(AValue: TItemProp);
    procedure Assign(Source: TItemPropList);
    procedure Clear;
    procedure Delete(Index: Integer);
    procedure Exchange(Index1, Index2: Integer);
    procedure Insert(Index: Integer; AValue: TItemProp);
  public
    constructor Create(AOwner: TValueListStrings);
    destructor Destroy; override;
  public
    property Count: Integer read GetCount;
    property Items[Index: Integer]: TItemProp read GetItem write SetItem; default;
  end;

  { TValueListStrings }

  TValueListStrings = class(TStringList)
  private
    FGrid: TValueListEditor;
    FItemProps: TItemPropList;
    function GetItemProp(const AKeyOrIndex: Variant): TItemProp;
    procedure QuickSortStringsAndItemProps(L, R: Integer; CompareFn: TStringListSortCompare);
    function CanHideShowingEditorAtIndex(Index: Integer): Boolean;
  protected
    procedure InsertItem(Index: Integer; const S: string; AObject: TObject); override;
    procedure InsertItem(Index: Integer; const S: string); override;
    procedure Put(Index: Integer; const S: String); override;
  public
    constructor Create(AOwner: TValueListEditor);
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure Clear; override;
    procedure CustomSort(Compare: TStringListSortCompare); override;
    procedure Delete(Index: Integer); override;
    procedure Exchange(Index1, Index2: Integer); override;
  end;

  TKeyValuePair = record
    Key, Value: String;
  end;

  TDisplayOption = (doColumnTitles, doAutoColResize, doKeyColFixed);
  TDisplayOptions = set of TDisplayOption;

  TKeyOption = (keyEdit, keyAdd, keyDelete, keyUnique);
  TKeyOptions = set of TKeyOption;

  TGetPickListEvent = procedure(Sender: TObject; const KeyName: string;
    Values: TStrings) of object;

  TOnValidateEvent = procedure(Sender: TObject; ACol, ARow: Longint;
    const KeyName, KeyValue: string) of object;

  { TValueListEditor }

  TValueListEditor = class(TCustomStringGrid)
  private
    FTitleCaptions: TStrings;
    FStrings: TValueListStrings;
    FKeyOptions: TKeyOptions;
    FDisplayOptions: TDisplayOptions;
    FDropDownRows: Integer;
    FOnGetPickList: TGetPickListEvent;
    FOnStringsChange: TNotifyEvent;
    FOnStringsChanging: TNotifyEvent;
    FOnValidate: TOnValidateEvent;
    FRowTextOnEnter: TKeyValuePair;
    FLastEditedRow: Integer;
    FUpdatingKeyOptions: Boolean;
    function GetItemProp(const AKeyOrIndex: Variant): TItemProp;
    procedure SetItemProp(const AKeyOrIndex: Variant; AValue: TItemProp);
    procedure StringsChange(Sender: TObject);
    procedure StringsChanging(Sender: TObject);
    function GetOptions: TGridOptions;
    function GetKey(Index: Integer): string;
    function GetValue(const Key: string): string;
    procedure SetDisplayOptions(const AValue: TDisplayOptions);
    procedure SetDropDownRows(const AValue: Integer);
    procedure SetKeyOptions(AValue: TKeyOptions);
    procedure SetKey(Index: Integer; const Value: string);
    procedure SetValue(const Key: string; AValue: string);
    procedure SetOptions(AValue: TGridOptions);
    procedure SetStrings(const AValue: TValueListStrings);
    procedure SetTitleCaptions(const AValue: TStrings);
  protected
    class procedure WSRegisterClass; override;
    procedure SetFixedCols(const AValue: Integer); override;
    procedure ShowColumnTitles;
    procedure AdjustRowCount; virtual;
    procedure ColRowExchanged(IsColumn: Boolean; index, WithIndex: Integer); override;
    procedure ColRowDeleted(IsColumn: Boolean; index: Integer); override;
    procedure DefineCellsProperty(Filer: TFiler); override;
    procedure InvalidateCachedRow;
    procedure GetAutoFillColumnInfo(const Index: Integer; var aMin,aMax,aPriority: Integer); override;
    function GetEditText(ACol, ARow: Integer): string; override;
    function GetCells(ACol, ARow: Integer): string; override;
    function GetDefaultEditor(Column: Integer): TWinControl; override;
    function GetRowCount: Integer;
    procedure KeyDown(var Key : Word; Shift : TShiftState); override;
    procedure LoadContent(cfg: TXMLConfig; Version: Integer); override;
    procedure ResetDefaultColWidths; override;
    procedure SaveContent(cfg: TXMLConfig); override;
    procedure SetCells(ACol, ARow: Integer; const AValue: string); override;
    procedure SetEditText(ACol, ARow: Longint; const Value: string); override;
    procedure SetFixedRows(const AValue: Integer); override;
    procedure SetRowCount(AValue: Integer);
    procedure Sort(ColSorting: Boolean; index,IndxFrom,IndxTo:Integer); override;
    procedure TitlesChanged(Sender: TObject);
    function ValidateEntry(const ACol,ARow:Integer; const OldValue:string; var NewValue:string): boolean; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Clear;
    procedure DeleteColRow(IsColumn: Boolean; index: Integer);
    procedure DeleteRow(Index: Integer); override;
    procedure DeleteCol(Index: Integer); override;
    function FindRow(const KeyName: string; out aRow: Integer): Boolean;
    procedure InsertColRow(IsColumn: boolean; index: integer);
    function InsertRow(const KeyName, Value: string; Append: Boolean): Integer;
    procedure InsertRowWithValues(Index: Integer; Values: array of String);
    procedure ExchangeColRow(IsColumn: Boolean; index, WithIndex: Integer); override;
    function IsEmptyRow: Boolean; {Delphi compatible function}
    function IsEmptyRow(aRow: Integer): Boolean; {This for makes more sense to me}
    procedure MoveColRow(IsColumn: Boolean; FromIndex, ToIndex: Integer);
    function RestoreCurrentRow: Boolean;
    procedure Sort(Index, IndxFrom, IndxTo: Integer);
    procedure Sort(ACol: TVleSortCol = colKey);

    property Modified;
    property Keys[Index: Integer]: string read GetKey write SetKey;
    property Values[const Key: string]: string read GetValue write SetValue;
    property ItemProps[const AKeyOrIndex: Variant]: TItemProp read GetItemProp write SetItemProp;
  published
    // Same as in TStringGrid
    property Align;
    property AlternateColor;
    property Anchors;
    property AutoAdvance;
    property AutoEdit;
    property BiDiMode;
    property BorderSpacing;
    property BorderStyle;
    property Color;
    property Constraints;
    property DefaultColWidth;
    property DefaultDrawing;
    property DefaultRowHeight;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property ExtendedSelect;
    property FixedColor;
    property FixedCols;
    property Flat;
    property Font;
    property GridLineWidth;
    property HeaderHotZones;
    property HeaderPushZones;
    property MouseWheelOption;
    property ParentBiDiMode;
    property ParentColor default false;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property RowCount: Integer read GetRowCount write SetRowCount;
    property ScrollBars;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property TitleFont;
    property TitleImageList;
    property TitleStyle;
    property UseXORFeatures;
    property Visible;
    property VisibleColCount;
    property VisibleRowCount;

    property OnBeforeSelection;
    property OnButtonClick;
    property OnChangeBounds;
    property OnCheckboxToggled;
    property OnClick;
    property OnColRowDeleted;
    property OnColRowExchanged;
    property OnColRowInserted;
    property OnColRowMoved;
    property OnCompareCells;
    property OnContextPopup;
    property OnDragDrop;
    property OnDragOver;
    property OnDblClick;
    property OnDrawCell;
    property OnEditButtonClick; deprecated;
    property OnEditingDone;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnGetEditMask;
    property OnGetEditText;
    property OnHeaderClick;
    property OnHeaderSized;
    property OnHeaderSizing;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheel;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
    property OnMouseWheelHorz;
    property OnMouseWheelLeft;
    property OnMouseWheelRight;
    property OnPickListSelect;
    property OnPrepareCanvas;
    property OnResize;
    property OnSelectEditor;
    property OnSelection;
    property OnSelectCell;
    property OnSetEditText;
    property OnShowHint;
    property OnStartDock;
    property OnStartDrag;
    property OnTopLeftChanged;
    property OnUserCheckboxBitmap;
    property OnUTF8KeyPress;
    property OnValidateEntry;

    // Compatible with Delphi TValueListEditor:
    property DisplayOptions: TDisplayOptions read FDisplayOptions
      write SetDisplayOptions default [doColumnTitles, doAutoColResize, doKeyColFixed];
    property DoubleBuffered;
    property DropDownRows: Integer read FDropDownRows write SetDropDownRows default 8;
    property KeyOptions: TKeyOptions read FKeyOptions write SetKeyOptions default [];
    property Options: TGridOptions read GetOptions write SetOptions default
     [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goColSizing,
      goEditing, goAlwaysShowEditor, goThumbTracking];
    property Strings: TValueListStrings read FStrings write SetStrings;
    property TitleCaptions: TStrings read FTitleCaptions write SetTitleCaptions;

    property OnGetPickList: TGetPickListEvent read FOnGetPickList write FOnGetPickList;
    property OnStringsChange: TNotifyEvent read FOnStringsChange write FOnStringsChange;
    property OnStringsChanging: TNotifyEvent read FOnStringsChanging write FOnStringsChanging;
    property OnValidate: TOnValidateEvent read FOnValidate write FOnValidate;

  end;

const
  //ToDo: Make this a resourcestring in lclstrconsts unit, once we are satisfied with the implementation of validating
  rsVLEDuplicateKey = 'Duplicate Key:'+LineEnding+'A key with name "%s" already exists at column %d';
  //ToDo: Make this a resourcestring in lclstrconsts unit, once we are satisfied with ShowColumnTitles
  rsVLEKey = 'Key';
  rsVLEValue = 'Value';
  rsVLEInvalidRowColOperation = 'The operation %s is not allowed on a TValueListEditor%s.';
  //LoadContent errors
  rsVLENoRowCountFound = 'Error reading file "%s":'^m'No value for RowCount found.';
  rsVLERowIndexOutOfBounds = 'Error reading file "%s":'^m'Row index out of bounds (%d).';
  rsVLEColIndexOutOfBounds = 'Error reading file "%s":'^m'Column index out of bounds (%d).';

procedure Register;

implementation

type
  TCompositeCellEditorAccess = class(TCompositeCellEditor);

{ TItemProp }


constructor TItemProp.Create(AOwner: TValueListEditor);
begin
  inherited Create;
  FGrid := AOwner;
end;

destructor TItemProp.Destroy;
begin
  FPickList.Free;
  inherited Destroy;
end;

function TItemProp.GetPickList: TStrings;
begin
  if FPickList = Nil then
  begin
    FPickList := TStringList.Create;
    TStringList(FPickList).OnChange := @PickListChange;
  end;
  Result := FPickList;
end;

procedure TItemProp.PickListChange(Sender: TObject);
begin
  if PickList.Count > 0 then begin
    if EditStyle = esSimple then
      EditStyle := esPickList;
  end
  else begin
    if EditStyle = esPickList then
      EditStyle := esSimple;
  end;
end;

procedure TItemProp.SetEditMask(const AValue: string);
begin
  FEditMask := AValue;
  with FGrid do
    if EditorMode and (FStrings.UpdateCount = 0) then
      InvalidateCell(Col, Row);
end;

procedure TItemProp.SetMaxLength(const AValue: Integer);
begin
  FMaxLength := AValue;
  with FGrid do
    if EditorMode and (FStrings.UpdateCount = 0) then
      InvalidateCell(Col, Row);
end;

procedure TItemProp.SetReadOnly(const AValue: Boolean);
begin
  FReadOnly := AValue;
  with FGrid do
    if EditorMode and (FStrings.UpdateCount = 0) then
      InvalidateCell(Col, Row);
end;

procedure TItemProp.SetEditStyle(const AValue: TEditStyle);
begin
  FEditStyle := AValue;
  with FGrid do
    if EditorMode and (FStrings.UpdateCount = 0) then
      InvalidateCell(Col, Row);
end;

procedure TItemProp.SetPickList(const AValue: TStrings);
begin
  GetPickList.Assign(AValue);
  with FGrid do
    if EditorMode and (FStrings.UpdateCount = 0) then
      InvalidateCell(Col, Row);
end;

procedure TItemProp.SetKeyDesc(const AValue: string);
begin
  FKeyDesc := AValue;
end;

procedure TItemProp.AssignTo(Dest: TPersistent);
begin
  if not (Dest is TItemProp) then
    inherited AssignTo(Dest)
  else
  begin
    TItemProp(Dest).EditMask := Self.EditMask;
    TItemProp(Dest).EditStyle := Self.EditStyle;
    TItemProp(Dest).KeyDesc := Self.KeyDesc;
    TItemProp(Dest).PickList.Assign(Self.PickList);
    TItemProp(Dest).MaxLength := Self.MaxLength;
    TItemProp(Dest).ReadOnly := Self.ReadOnly;
  end;
end;


{ TItemPropList }

function TItemPropList.GetItem(Index: Integer): TItemProp;
begin
  Result := TItemProp(FList.Items[Index]);
end;

function TItemPropList.GetCount: Integer;
begin
  Result := FList.Count;
end;

procedure TItemPropList.SetItem(Index: Integer; AValue: TItemProp);
begin
  FList.Items[Index] := AValue;
end;

procedure TItemPropList.Insert(Index: Integer; AValue: TItemProp);
begin
  FList.Insert(Index, AValue);
end;

procedure TItemPropList.Add(AValue: TItemProp);
begin
  FList.Add(AValue);
end;

procedure TItemPropList.Assign(Source: TItemPropList);
var
  Index: Integer;
  Prop: TItemProp;
begin
  Clear;
  if not Assigned(Source) then Exit;
  for Index := 0 to Source.Count - 1 do
  begin
    Prop := TItemProp.Create(FStrings.FGrid);
    Prop.Assign(Source.Items[Index]);
    Add(Prop);
  end;
end;

procedure TItemPropList.Delete(Index: Integer);
begin
  FList.Delete(Index);
end;

procedure TItemPropList.Exchange(Index1, Index2: Integer);
begin
  FList.Exchange(Index1, index2);
end;

procedure TItemPropList.Clear;
begin
  FList.Clear;
end;

constructor TItemPropList.Create(AOwner: TValueListStrings);
begin
  FStrings := AOwner;
  FList := TFPObjectList.Create(True);
end;

destructor TItemPropList.Destroy;
begin
  FList.Free;
  inherited Destroy;
end;


{ TValueListStrings }

procedure TValueListStrings.InsertItem(Index: Integer; const S: string; AObject: TObject);
var
  MustHideShowingEditor: Boolean;
begin
  // ToDo: Check validity of key
  //debugln('TValueListStrings.InsertItem: Index = ',dbgs(index),' S = "',S,'" AObject = ',dbgs(aobject));
  FGrid.InvalidateCachedRow;
  MustHideShowingEditor := CanHideShowingEditorAtIndex(Index);
  if MustHideShowingEditor then FGrid.Options := FGrid.Options - [goAlwaysShowEditor];
  inherited InsertItem(Index, S, AObject);
  FItemProps.Insert(Index, TItemProp.Create(FGrid));
  //only restore this _after_ FItemProps is updated!
  if MustHideShowingEditor then FGrid.Options := FGrid.Options + [goAlwaysShowEditor];
end;

procedure TValueListStrings.InsertItem(Index: Integer; const S: string);
begin
  InsertItem(Index, S, nil);
end;

procedure TValueListStrings.Put(Index: Integer; const S: String);
var
  MustHideShowingEditor: Boolean;
begin
  // ToDo: Check validity of key
  MustHideShowingEditor := CanHideShowingEditorAtIndex(Index);
  //debugln('TValueListStrings.Put: MustHideShowingEditor=',DbgS(MustHideShowingEditor));
  if MustHideShowingEditor then FGrid.Options := FGrid.Options - [goAlwaysShowEditor];
  inherited Put(Index, S);
  if MustHideShowingEditor then FGrid.Options := FGrid.Options + [goAlwaysShowEditor];
end;

constructor TValueListStrings.Create(AOwner: TValueListEditor);
begin
  inherited Create;
  FGrid := AOwner;
  FItemProps := TItemPropList.Create(Self);
end;

destructor TValueListStrings.Destroy;
begin
  FItemProps.Free;
  inherited Destroy;
end;

procedure TValueListStrings.Assign(Source: TPersistent);
begin
  FGrid.InvalidateCachedRow;
  Clear;  //if this is not done, and a TValueListEditor.Sort() is done and then later a Strings.Assign, an exception will occur.
  inherited Assign(Source);
  if (Source is TValueListStrings) then
    FItemProps.Assign(TValueListStrings(Source).FItemProps);
end;

procedure TValueListStrings.Clear;
var
  IsShowingEditor: Boolean;
begin
  FGrid.InvalidateCachedRow;
  IsShowingEditor := goAlwaysShowEditor in FGrid.Options;
  if IsShowingEditor then FGrid.Options := FGrid.Options - [goAlwaysShowEditor];
  inherited Clear;
  FItemProps.Clear;
  if IsShowingEditor then FGrid.Options := FGrid.Options + [goAlwaysShowEditor];
end;


{
 Duplicates the functionality of TStringList.QuickSort, but also
 sorts the ItemProps.
}
procedure TValueListStrings.QuickSortStringsAndItemProps(L, R: Integer;
  CompareFn: TStringListSortCompare);
var
  Pivot, vL, vR: Integer;
begin
  if R - L <= 1 then
  begin // a little bit of time saver
    if L < R then
      if CompareFn(Self, L, R) > 0 then
        //Exchange also exchanges FItemProps
        Exchange(L, R);
    Exit;
  end;

  vL := L;
  vR := R;
  Pivot := L + Random(R - L); // they say random is best
  while vL < vR do
  begin
    while (vL < Pivot) and (CompareFn(Self, vL, Pivot) <= 0) do
      Inc(vL);
    while (vR > Pivot) and (CompareFn(Self, vR, Pivot) > 0) do
      Dec(vR);
    //Exchange also exchanges FItemProps
    Exchange(vL, vR);
    if Pivot = vL then // swap pivot if we just hit it from one side
      Pivot := vR
    else if Pivot = vR then
      Pivot := vL;
  end;

  if Pivot - 1 >= L then
    QuickSortStringsAndItemProps(L, Pivot - 1, CompareFn);
  if Pivot + 1 <= R then
    QuickSortStringsAndItemProps(Pivot + 1, R, CompareFn);
end;

function TValueListStrings.CanHideShowingEditorAtIndex(Index: Integer): Boolean;
var
  IndexToRow: Integer;
  WC: TWinControl;
  EditorHasFocus: Boolean;
begin
  IndexToRow := Index + FGrid.FixedRows;
  if (FGrid.Editor is TCompositeCellEditor) then
  begin
    WC := TCompositeCellEditorAccess(FGrid.Editor).GetActiveControl;
    if (WC is TCustomEdit) then
      EditorHasFocus := TCustomEdit(WC).Focused
    else
      EditorHasFocus := False;
  end
  else
    EditorHasFocus := Assigned(FGrid.Editor) and FGrid.Editor.Focused;

  //debugln('CanHideShowingEditor:');
  //debugln(' Assigned(FGrid.Editor) = ',DbgS(Assigned(FGrid.Editor)));
  //debugln(' (goAlwaysShowEditor in FGrid.Options) = ',DbgS(goAlwaysShowEditor in FGrid.Options));
  //if Assigned(FGrid.Editor) then
  //  debugln(' FGrid.Editor.Visible = ',DbgS(FGrid.Editor.Visible));
  //debugln(' IndexToRow = ',DbgS(IndextoRow));
  //debugln(' Count = ',DbgS(Count));
  //debugln(' EditorHasFocus = ',DbgS(EditorHasFocus));

  Result := Assigned(FGrid.Editor) and
            (goAlwaysShowEditor in FGrid.Options) and
            FGrid.Editor.Visible and
            ((IndexToRow = FGrid.Row) or (Count = 0)) and  //if Count = 0 we still have an editable row
            //if editor is Focussed, we are editing a cell, so we cannot hide!
            (not EditorHasFocus);
  //debugln('CanHideShowingEditor: Result = ',DbgS(Result));
end;

procedure TValueListStrings.CustomSort(Compare: TStringListSortCompare);
{
 Re-implement it, because we need it to call our own QuickSortStringsAndItemProps
 and so we cannot use inherited CustomSort
 Use BeginUpdate/EndUpdate to avoid numerous Changing/Changed calls
}
begin
  If not Sorted and (Count>1) then
  begin
    try
      BeginUpdate;
      FGrid.InvalidateCachedRow;
      QuickSortStringsAndItemProps(0,Count-1, Compare);
    finally
      EndUpdate;
    end;
  end;
end;

procedure TValueListStrings.Delete(Index: Integer);
var
  IsShowingEditor: Boolean;
begin
  FGrid.InvalidateCachedRow;
  IsShowingEditor := CanHideShowingEditorAtIndex(Index);
  if IsShowingEditor then FGrid.Options := FGrid.Options - [goAlwaysShowEditor];
  inherited Delete(Index);
  // Delete also ItemProps
  FItemProps.Delete(Index);
  //only restore this _after_ FItemProps is updated!
  if IsShowingEditor then FGrid.Options := FGrid.Options + [goAlwaysShowEditor];
end;

procedure TValueListStrings.Exchange(Index1, Index2: Integer);
var
  MustHideShowingEditor: Boolean;
begin
  FGrid.InvalidateCachedRow;
  MustHideShowingEditor := CanHideShowingEditorAtIndex(Index1) or CanHideShowingEditorAtIndex(Index2);
  if MustHideShowingEditor then FGrid.Options := FGrid.Options - [goAlwaysShowEditor];
  inherited Exchange(Index1, Index2);
  FItemProps.Exchange(Index1, Index2);
  if MustHideShowingEditor then FGrid.Options := FGrid.Options + [goAlwaysShowEditor];
end;

function TValueListStrings.GetItemProp(const AKeyOrIndex: Variant): TItemProp;
var
  i: Integer;
  s: string;
begin
  Result := Nil;
  if (Count > 0) and (UpdateCount = 0) then
  begin
    if VarIsOrdinal(AKeyOrIndex) then
      i := AKeyOrIndex
    else begin
      s := AKeyOrIndex;
      i := IndexOfName(s);
      if i = -1 then
        raise Exception.Create('TValueListStrings.GetItemProp: Key not found: '+s);
    end;
    if i < FItemProps.Count then
    begin
      Result := FItemProps.Items[i];
      if not Assigned(Result) then
        Raise Exception.Create(Format('TValueListStrings.GetItemProp: Index=%d Result=Nil',[i]));
    end;
  end;
end;


{ TValueListEditor }

constructor TValueListEditor.Create(AOwner: TComponent);
begin
  //need FStrings before inherited Create, because they are needed in overridden SelectEditor
  FStrings := TValueListStrings.Create(Self);
  FStrings.NameValueSeparator := '=';
  FTitleCaptions := TStringList.Create;
  inherited Create(AOwner);
  FStrings.OnChange := @StringsChange;
  FStrings.OnChanging := @StringsChanging;
  TStringList(FTitleCaptions).OnChange := @TitlesChanged;

  //Don't use Columns.Add, it interferes with setting FixedCols := 1 (it will then insert an extra column)
  {
  with Columns.Add do
    Title.Caption := 'Key';
  with Columns.Add do begin
    Title.Caption := 'Value';
    DropDownRows := 8;
  end;
  }

  ColCount:=2;
  {inherited} RowCount := 2;
  FixedCols := 0;
//  DefaultColWidth := 150;
//  DefaultRowHeight := 18;
//  Width := 306;
//  Height := 300;
  Options := [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine,
              goColSizing, goEditing, goAlwaysShowEditor, goThumbTracking];
  FDisplayOptions := [doColumnTitles, doAutoColResize, doKeyColFixed];
  Col := 1;
  FLastEditedRow := -1;
  FDropDownRows := 8;
  ShowColumnTitles;
  AutoFillColumns := true;
end;

destructor TValueListEditor.Destroy;
begin
  FTitleCaptions.Free;
  FStrings.Free;
  inherited Destroy;
end;

procedure TValueListEditor.Clear;
begin
  Strings.Clear;
end;

procedure TValueListEditor.DeleteColRow(IsColumn: Boolean; index: Integer);
begin
  if not IsColumn then
    DeleteRow(Index)
  else
    DeleteCol(Index);
end;

procedure TValueListEditor.DeleteRow(Index: Integer);
begin
  //If we have only one row, it may be empty and we cannot remove
  if not ((Index - FixedRows = 0) and (Strings.Count = 0)) then inherited DeleteRow(Index) ;
end;

procedure TValueListEditor.DeleteCol(Index: Integer);
begin
  Raise EGridException.CreateFmt(rsVLEInvalidRowColOperation,['DeleteCol','']);
end;

function TValueListEditor.FindRow(const KeyName: string; out aRow: Integer): Boolean;
var
  Index: Integer;
begin
  Index := Strings.IndexOfName(KeyName);
  Result := (Index > -1);
  if Result then aRow := Index + FixedRows;
end;

procedure TValueListEditor.InsertColRow(IsColumn: boolean; index: integer);
begin
  if not IsColumn then
    Strings.InsertItem(Index - FixedRows,'')
  else
    Raise EGridException.CreateFmt(rsVLEInvalidRowColOperation,['InsertColRow',' on columns']);
end;

function TValueListEditor.InsertRow(const KeyName, Value: string; Append: Boolean): Integer;
var
  NewInd, NewRow: Integer;
  Line: String;
begin
  if not ((KeyName = '') and (Value = '')) then
    Line := KeyName + Strings.NameValueSeparator + Value
  else
    Line := '';
  if (Row > Strings.Count) or ((Row - FixedRows) >= Strings.Count)
  or (Cells[0, Row] <> '') or (Cells[1, Row] <> '') then
  begin                                    // Add a new Key=Value pair
    Strings.BeginUpdate;
    try
      if Append then
      begin
        if (Strings.Count = 0) then   //empty grid
          NewInd := 0
        else
          NewInd := Row - FixedRows + 1 //append after current row
      end
      else
        NewInd := Row - FixedRows; //insert it at current row
      Strings.InsertItem(NewInd, Line, Nil);
    finally
      Strings.EndUpdate;
    end;
  end
  else begin   // Use an existing row, just update the Key and Value.
    Cells[0, Row] := KeyName;
    Cells[1, Row] := Value;
    NewInd := Row - FixedRows;
  end;
  Result := NewInd;
  NewRow := NewInd + FixedRows;
  if (NewRow <> Row) then Row := NewRow;
end;

procedure TValueListEditor.InsertRowWithValues(Index: Integer; Values: array of String);
var
  AKey, AValue: String;
begin
  AKey := '';
  AValue := '';
  if (Length(Values) > 1) then
  begin
    AKey := Values[0];
    AValue := Values[1];
  end
  else if (Length(Values) = 1) then
    AKey := Values[0];
  Strings.InsertItem(Index, AKey + Strings.NameValueSeparator + AValue);
end;

procedure TValueListEditor.ExchangeColRow(IsColumn: Boolean; index, WithIndex: Integer);
begin
  if not IsColumn then
    inherited ExchangeColRow(IsColumn, index, WithIndex)
  else
    Raise EGridException.CreateFmt(rsVLEInvalidRowColOperation,['ExchangeColRow',' on columns']);
end;

function TValueListEditor.IsEmptyRow: Boolean;
{As per help text on Embarcadero: the function does not have a parameter for row, so assume current one?}
begin
  Result := IsEmptyRow(Row);
end;

function TValueListEditor.IsEmptyRow(aRow: Integer): Boolean;
begin
  if (Strings.Count = 0) and (aRow - FixedRows = 0) then
    //special case: we have just one row, and it is empty
    Result := True
  else if (aRow = 0) and (FixedRows = 0) then
    Result := ((inherited GetCells(0,0)) = EmptyStr)  and ((inherited GetCells(1,0)) = EmptyStr)
  else
    Result := Strings.Strings[aRow - FixedRows] = EmptyStr;
end;

procedure TValueListEditor.MoveColRow(IsColumn: Boolean; FromIndex,
  ToIndex: Integer);
var
  Line: String;
begin
  if not IsColumn then
  begin
    try
      Strings.BeginUpdate;
      Line := Strings.Strings[FromIndex - FixedRows];
      Strings.Delete(FromIndex - FixedRows);
      Strings.InsertItem(ToIndex - FixedRows, Line);
    finally
      Strings.EndUpdate;
    end;
  end
  else
    Raise EGridException.CreateFmt(rsVLEInvalidRowColOperation,['MoveColRow',' on columns']);
end;


function TValueListEditor.RestoreCurrentRow: Boolean;
begin
  //DbgOut('RestoreCurrentRow: Row=',DbgS(Row),' FLastEditedRow=',DbgS(FLastEditedRow),' SavedKey=',FRowTextOnEnter.Key,' SavedValue=',FRowTextOnEnter.Value);
  Result := False;
  if (Row = FLastEditedRow) and Assigned(Editor) and Editor.Focused then
  begin
    if (Cells[0,Row] <> FRowTextOnEnter.Key) or (Cells[1,Row] <> FRowTextOnEnter.Value) then
    begin
      try
        EditorHide;
        if (Cells[0,Row] <> FRowTextOnEnter.Key) then Cells[0,Row] := FRowTextOnEnter.Key;
        if (Cells[1,Row] <> FRowTextOnEnter.Value) then Cells[1,Row] := FRowTextOnEnter.Value;
      finally
        EditorShow(True);
      end;
      Result := True;
    end;
  end;
end;


procedure TValueListEditor.Sort(ACol: TVleSortCol = colKey);
begin
  SortColRow(True, Ord(ACol));
end;

procedure TValueListEditor.Sort(Index, IndxFrom, IndxTo: Integer);
begin
  Sort(True, Index, IndxFrom, IndxTo);
end;

procedure TValueListEditor.StringsChange(Sender: TObject);
begin
  Modified := True;
  AdjustRowCount;
  Invalidate;
  if Assigned(OnStringsChange) then
    OnStringsChange(Self);
end;

procedure TValueListEditor.StringsChanging(Sender: TObject);
begin
  if Assigned(OnStringsChanging) then
    OnStringsChanging(Self);
end;

procedure TValueListEditor.SetFixedCols(const AValue: Integer);
begin
  if (AValue in [0,1]) then
    inherited SetFixedCols(AValue);
end;

procedure TValueListEditor.SetFixedRows(const AValue: Integer);
begin
  if AValue in [0,1] then begin  // No other values are allowed
    if AValue = 0 then           // Typically DisplayOptions are changed directly
      DisplayOptions := DisplayOptions - [doColumnTitles]
    else
      DisplayOptions := DisplayOptions + [doColumnTitles]
  end;
end;

function TValueListEditor.GetItemProp(const AKeyOrIndex: Variant): TItemProp;
begin
  Result := FStrings.GetItemProp(AKeyOrIndex);
end;

procedure TValueListEditor.SetItemProp(const AKeyOrIndex: Variant; AValue: TItemProp);
begin
  FStrings.GetItemProp(AKeyOrIndex).Assign(AValue);
end;

function TValueListEditor.GetOptions: TGridOptions;
begin
  Result := inherited Options;
end;

procedure TValueListEditor.SetDisplayOptions(const AValue: TDisplayOptions);
// Set number of fixed rows to 1 if titles are shown (based on DisplayOptions).
// Set the local options value, then Adjust Column Widths and Refresh the display.
begin
  BeginUpdate;
  if (doColumnTitles in DisplayOptions) <> (doColumnTitles in AValue) then
    if doColumnTitles in AValue then begin
      if RowCount < 2 then
        {inherited} RowCount := 2;
      inherited SetFixedRows(1);// don't do FixedRows := 1 here, it wil cause infinite recursion (Issue 0029993)
    end else
      inherited SetFixedRows(0);

  if (doAutoColResize in DisplayOptions) <> (doAutoColResize in AValue) then
    AutoFillColumns := (doAutoColResize in AValue);

  FDisplayOptions := AValue;
  ShowColumnTitles;
  AdjustRowCount;
  EndUpdate;
end;

procedure TValueListEditor.SetDropDownRows(const AValue: Integer);
begin
  FDropDownRows := AValue;
  // ToDo: If edit list for inplace editing is implemented, set its handler, too.
end;

procedure TValueListEditor.SetKeyOptions(AValue: TKeyOptions);
begin
  FUpdatingKeyOptions := True;
  // KeyAdd requires KeyEdit, KeyAdd oddly enough does not according to Delphi specs
  if KeyAdd in AValue then
    Include(AValue, keyEdit);
  FKeyOptions := AValue;
  if (KeyAdd in FKeyOptions) then
    Options := Options + [goAutoAddRows]
  else
    Options := Options - [goAutoAddRows];
  FUpdatingKeyOptions := False;
end;

procedure TValueListEditor.SetOptions(AValue: TGridOptions);
begin
  //cannot allow goColMoving
  if goColMoving in AValue then
    Exclude(AValue, goColMoving);
  //temporarily disable this, it causes crashes
  if (goAutoAddRowsSkipContentCheck in AValue) then
    Exclude(AValue, goAutoAddRowsSkipContentCheck);
  inherited Options := AValue;
  // Enable also the required KeyOptions for goAutoAddRows
  if not FUpdatingKeyOptions and not (csLoading in ComponentState)
  and (goAutoAddRows in AValue) then
    KeyOptions := KeyOptions + [keyEdit, keyAdd];
end;

procedure TValueListEditor.SetStrings(const AValue: TValueListStrings);
begin
  FStrings.Assign(AValue);
end;

procedure TValueListEditor.SetTitleCaptions(const AValue: TStrings);
begin
  FTitleCaptions.Assign(AValue);
end;

function TValueListEditor.GetKey(Index: Integer): string;
begin
  Result:=Cells[0,Index];
end;

procedure TValueListEditor.SetKey(Index: Integer; const Value: string);
begin
  Cells[0,Index]:=Value;
end;

function TValueListEditor.GetValue(const Key: string): string;
var
  I: Integer;
begin
  Result := '';
  I := Strings.IndexOfName(Key);
  if I > -1 then begin
    Inc(I, FixedRows);
    Result:=Cells[1,I];
  end;
end;

procedure TValueListEditor.SetValue(const Key: string; AValue: string);
var
  I: Integer;
begin
  I := Strings.IndexOfName(Key);
  if I > -1 then begin
    Inc(I, FixedRows);
    Cells[1,I]:=AValue;
  end
  else begin
    Insert(Strings.NameValueSeparator, AValue, 1);
    Insert(Key, AValue, 1);
    Strings.Add(AValue);
  end;
end;

procedure TValueListEditor.ShowColumnTitles;
var
  KeyCap, ValCap: String;
begin
  if (doColumnTitles in DisplayOptions) then
  begin
    KeyCap := rsVLEKey;
    ValCap := rsVLEValue;
    if (TitleCaptions.Count > 0) then KeyCap := TitleCaptions[0];
    if (TitleCaptions.Count > 1) then ValCap := TitleCaptions[1];
    //Columns[0].Title.Caption := KeyCap;
    //Columns[1].Title.Caption := ValCap;
    //or:
    Cells[0,0] := KeyCap;
    Cells[1,0] := ValCap;
  end;
end;

procedure TValueListEditor.AdjustRowCount;
// Change the number of rows based on the number of items in Strings collection.
// Sets Row and RowCount of parent TCustomDrawGrid class.
var
  NewC: Integer;
begin
  NewC:=FixedRows+1;
  if Strings.Count>0 then
    NewC:=Strings.Count+FixedRows;
  if NewC<>RowCount then
  begin
    if NewC<Row then
      Row:=NewC-1;
    if Row = 0 then
      if doColumnTitles in DisplayOptions then
        Row:=1;
    inherited RowCount:=NewC;
  end;
end;

procedure TValueListEditor.ColRowExchanged(IsColumn: Boolean; index,
  WithIndex: Integer);
begin
  Strings.Exchange(Index - FixedRows, WithIndex - FixedRows);
  inherited ColRowExchanged(IsColumn, index, WithIndex);
end;

procedure TValueListEditor.ColRowDeleted(IsColumn: Boolean; index: Integer);
begin
  EditorMode := False;
  Strings.Delete(Index-FixedRows);
  inherited ColRowDeleted(IsColumn, index);
end;

procedure TValueListEditor.DefineCellsProperty(Filer: TFiler);
begin
end;

procedure TValueListEditor.InvalidateCachedRow;
begin
  if (Strings.Count = 0) then
  begin
    FLastEditedRow := FixedRows;
    FRowTextOnEnter.Key := '';
    FRowTextOnEnter.Value := '';
  end
  else
    FLastEditedRow := -1;
end;

procedure TValueListEditor.GetAutoFillColumnInfo(const Index: Integer;
  var aMin, aMax, aPriority: Integer);
begin
  if Index=1 then
    aPriority := 1
  else
  begin
    if doKeyColFixed in FDisplayOptions then
      aPriority := 0
    else
      aPriority := 1;
  end;
end;

function TValueListEditor.GetCells(ACol, ARow: Integer): string;
var
  I: Integer;
begin
  Result:='';
  if (ARow=0) and (doColumnTitles in DisplayOptions) then
  begin
    Result := Inherited GetCells(ACol, ARow);
  end
  else
  begin
    I:=ARow-FixedRows;
    if (I >= Strings.Count) then
      //Either empty grid, or a row has been added and Strings hasn't been update yet
      //the latter happens when rows are auto-added (issue #0025166)
      Exit;
    if ACol=0 then
      Result:=Strings.Names[I]
    else if ACol=1 then
      Result:=Strings.ValueFromIndex[I];
  end;
end;

procedure SetGridEditorReadOnly(Ed: TwinControl; RO: Boolean);
begin
  //debugln('SetEditorReadOnly: Ed is ',DbgSName(Ed),' ReadOnly=',DbgS(RO));
  if (Ed is TCustomEdit) then
    TCustomEdit(Ed).ReadOnly := RO
  else if (Ed is TCustomComboBox) then
    if RO then
      TCustomComboBox(Ed).Style := csDropDownList
    else
      TCustomComboBox(Ed).Style := csDropDown;
end;

function TValueListEditor.GetDefaultEditor(Column: Integer): TWinControl;
var
  ItemProp: TItemProp;
begin
  if (Row <> FLastEditedRow) then
  //save current contents for RestoreCurrentRow
  begin
    FLastEditedRow := Row;
    FRowTextOnEnter.Key := Cells[0,Row];
    FRowTextOnEnter.Value := Cells[1,Row];
  end;
  Result:=inherited GetDefaultEditor(Column);
  //Need this to be able to intercept VK_Delete in the editor
  if (KeyDelete in KeyOptions) then
    EditorOptions := EditorOptions or EO_HOOKKEYDOWN
  else
    EditorOptions := EditorOptions and (not EO_HOOKKEYDOWN);
  if Column=1 then
  begin
    ItemProp := nil;
    //debugln('**** A Col=',dbgs(col),' Row=',dbgs(row),' (',dbgs(itemprop),')');
    ItemProp := Strings.GetItemProp(Row-FixedRows);
    if Assigned(ItemProp) then
    begin
      case ItemProp.EditStyle of
        esSimple: begin
          result := EditorByStyle(cbsAuto);
          SetGridEditorReadOnly(result, ItemProp.ReadOnly);
        end;
        esEllipsis: begin
          result := EditorByStyle(cbsEllipsis);
          SetGridEditorReadOnly(TCompositeCellEditorAccess(result).GetActiveControl, ItemProp.ReadOnly);
        end;
        esPickList: begin
          result := EditorByStyle(cbsPickList);
          (result as TCustomComboBox).Items.Assign(ItemProp.PickList);
          (result as TCustomComboBox).DropDownCount := DropDownRows;
          SetGridEditorReadOnly(result, ItemProp.ReadOnly);
          if Assigned(FOnGetPickList) then
            FOnGetPickList(Self, Strings.Names[Row - FixedRows], (result as TCustomComboBox).Items);
          //Style := csDropDown, default = csDropDownList;
        end;
      end; //case
    end
    else SetGridEditorReadOnly(result, False);
  end
  else
  begin
    //First column is only editable if KeyEdit is in KeyOptions
    if not (KeyEdit in KeyOptions) then
      Result := nil
    else
      SetGridEditorReadOnly(result, False);
  end;
end;

function TValueListEditor.GetRowCount: Integer;
begin
  Result := inherited RowCount;
end;

procedure TValueListEditor.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited KeyDown(Key, Shift);
  if (KeyAdd in KeyOptions) then
  begin
    if (Key = VK_INSERT) and (Shift = []) then
    begin
      //Insert a row in the current position
      InsertRow('', '', False);
      Key := 0;
    end;
  end;
  if (KeyDelete in KeyOptions) then
  begin
    //Although Delphi help says this happens if user presses Delete, testers report it only happens with Ctrl+Delete
    if (Key = VK_DELETE) and (Shift = [ssModifier]) then
    begin
      DeleteRow(Row);
      Key := 0;
    end;
  end;
  if (Key = VK_ESCAPE) and (Shift = []) then
    if RestoreCurrentRow then Key := 0;
end;

procedure TValueListEditor.LoadContent(cfg: TXMLConfig; Version: Integer);
var
  ContentSaved, HasColumnTitles, AlwaysShowEditor, HasSaveContent: Boolean;
  i,j,k, RC: Integer;
begin
  BeginUpdate;
  try
    AlwaysShowEditor := (goAlwaysShowEditor in Options);
    if AlwaysShowEditor then Options := Options - [goAlwaysShowEditor];
    HasSaveContent := soContent in SaveOptions;
    //no need to load content in inherited LoadContent, since we re-implemented that here.
    if HasSaveContent then
      SaveOptions := SaveOptions - [soContent];
    inherited LoadContent(Cfg, Version);
    if HasSaveContent then
      SaveOptions := SaveOptions + [soContent];
    if soContent in SaveOptions then
    begin
      ContentSaved:=Cfg.GetValue('grid/saveoptions/content', false);
      if ContentSaved then
      begin
        Clean(0,0,ColCount-1,RowCount-1,[]); //need if the to be loaded grid has no entries
        HasColumnTitles := cfg.getValue('grid/content/hascolumntitles', False);
        if HasColumnTitles then
          DisplayOptions := DisplayOptions + [doColumnTitles]
        else
          DisplayOptions := DisplayOptions - [doColumnTitles];

        //contrary to other grids we restore the entire saved content,
        //so we add/delete rows (not columns of course) as needed.
        RC := cfg.GetValue('grid/content/rowcount', -1);
        if (RC = -1) then
        begin
          raise EStreamError.CreateFmt(rsVLENoRowCountFound,[cfg.Filename]);
        end;
        if (RC < 1) then RC := 1;
        if HasColumnTitles and (RC = 1) then
          RC := 2;
        RowCount := RC;

        k:=cfg.getValue('grid/content/cells/cellcount', 0);
        while k>0 do
        begin
          i:=cfg.GetValue('grid/content/cells/cell'+IntToStr(k)+'/column', -1);
          j:=cfg.GetValue('grid/content/cells/cell'+IntTostr(k)+'/row',-1);
          if not IsRowIndexValid(j) then
            raise EStreamError.CreateFmt(rsVLERowIndexOutOfBounds,[cfg.Filename,j]);
          if not IsColumnIndexValid(i) then
            raise EStreamError.CreateFmt(rsVLEColIndexOutOfBounds,[cfg.Filename,i]);
            Cells[i,j]:=UTF8Encode(cfg.GetValue('grid/content/cells/cell'+IntToStr(k)+'/text',''));
          Dec(k);
        end;
      end;
    end;
  finally
    if AlwaysShowEditor then Options := Options + [goAlwaysShowEditor];
    if HasSaveContent then
      SaveOptions := SaveOptions + [soContent];
    EndUpdate(True);
  end;
end;

procedure TValueListEditor.ResetDefaultColWidths;
begin
  if not AutoFillColumns then
    inherited ResetDefaultColWidths
  else if doKeyColFixed in DisplayOptions then
  begin
    SetRawColWidths(0, -1);
    VisualChange;
  end;
end;

procedure TValueListEditor.SaveContent(cfg: TXMLConfig);
var
  i,j,k: Integer;
  Value: String;
  HasSaveContent: Boolean;
begin
  HasSaveContent := soContent in SaveOptions;
  //no need to save content in inherited SaveContent, since we re-implemented that here.
  if HasSaveContent then
    SaveOptions := SaveOptions - [soContent];
  try
    inherited SaveContent(cfg);
    if HasSaveContent then
      SaveOptions := SaveOptions + [soContent];
    cfg.SetValue('grid/saveoptions/content', soContent in SaveOptions);
    if soContent in SaveOptions then
    begin
      cfg.SetValue('grid/content/hascolumntitles',(doColumnTitles in FDisplayOptions));
      cfg.SetValue('grid/content/rowcount', RowCount);
      // Save Cell Contents
      k:=0;
      For i:=0 to ColCount-1 do
        For j:=0 to RowCount-1 do
        begin
          //fGrid.Celda is unassigned for cells other than the title row, so we neet to query GetCells here
          Value := GetCells(i,j);
          if (Value <> '') then
          begin
            Inc(k);
            //Cfg.SetValue('grid/content/cells/cellcount',k);
            cfg.SetValue('grid/content/cells/cell'+IntToStr(k)+'/column',i);
            cfg.SetValue('grid/content/cells/cell'+IntToStr(k)+'/row',j);
            cfg.SetValue('grid/content/cells/cell'+IntToStr(k)+'/text', Value);
          end;
          Cfg.SetValue('grid/content/cells/cellcount',k);
        end;
     end;
  finally
    if HasSaveContent then
      SaveOptions := SaveOptions + [soContent];
  end;
end;

procedure TValueListEditor.SetCells(ACol, ARow: Integer; const AValue: string);
var
  I: Integer;
  Key, KeyValue, Line: string;
begin
  if (ARow = 0) and (doColumnTitles in DisplayOptions) then
  begin
    Inherited SetCells(ACol, ARow, AValue);
  end
  else
  begin
    I:=ARow-FixedRows;
    if ACol=0 then
    begin
      Key := AValue;
      KeyValue := Cells[1,ARow]
    end
    else
    begin
      KeyValue := AValue;
      Key := Cells[0,ARow];
    end;
    //If cells are empty don't store '=' in Strings
    if (Key = '') and (KeyValue = '') then
      Line := ''
    else begin
      Line := KeyValue;
      system.Insert(Strings.NameValueSeparator, Line, 1);
      system.Insert(Key, Line, 1);
    end;
    // Empty grid: don't add a the line '' to Strings!
    if (Strings.Count = 0) and (Line = '') then Exit;
    if I>=Strings.Count then
      Strings.Insert(I,Line)
    else
      if (Line <> Strings[I]) then Strings[I]:=Line;
  end;
end;

function TValueListEditor.GetEditText(ACol, ARow: Integer): string;
begin
  Result:= Cells[ACol, ARow];
  if Assigned(OnGetEditText) then
    OnGetEditText(Self, ACol, ARow, Result);
end;

procedure TValueListEditor.SetEditText(ACol, ARow: Longint; const Value: string);
begin
  inherited SetEditText(ACol, ARow, Value);
  Cells[ACol, ARow] := Value;
end;

procedure TValueListEditor.SetRowCount(AValue: Integer);
var
  OldValue, NewCount: Integer;
begin
  //debugln('TValueListEditor.SetRowCount: AValue=',DbgS(AValue));
  OldValue := inherited RowCount;
  if OldValue = AValue then Exit;
  NewCount := AValue - FixedRows;
  if (NewCount > Strings.Count) then
  begin
    Strings.BeginUpdate;
    while (Strings.Count < NewCount) do Strings.Add('');
    Strings.EndUpdate;
  end
  else if (NewCount < Strings.Count) then
  begin
    Strings.BeginUpdate;
    while (NewCount < Strings.Count) do Strings.Delete(Strings.Count - 1);
    Strings.EndUpdate;
  end;
end;

procedure TValueListEditor.Sort(ColSorting: Boolean; index, IndxFrom,
  IndxTo: Integer);
var
  HideEditor: Boolean;
begin
  HideEditor := goAlwaysShowEditor in Options;
  if HideEditor then Options := Options - [goAlwaysShowEditor];
  Strings.BeginUpdate;
  try
    inherited Sort(True, index, IndxFrom, IndxTo);
  finally
    Strings.EndUpdate;
  end;
  if HideEditor then Options := Options + [goAlwaysShowEditor];
end;

procedure TValueListEditor.TitlesChanged(Sender: TObject);
begin
  // Refresh the display.
  ShowColumnTitles;
  AdjustRowCount;
  Invalidate;
end;

function TValueListEditor.ValidateEntry(const ACol, ARow: Integer;
  const OldValue: string; var NewValue: string): boolean;
var
  Index, i: Integer;
begin
  Result := inherited ValidateEntry(ACol, ARow, OldValue, NewValue);
  //Check for duplicate key names (only in "Key" column), if KeyUnique is set
  if ((ACol - FixedCols) = 0) and (KeyUnique in KeyOptions) then
  begin
    Index := ARow - FixedRows;
    for i := 0 to FStrings.Count - 1 do
    begin
      if (Index <> i) and (FStrings.Names[i] <> '') then
      begin
        if (Utf8CompareText(FStrings.Names[i], NewValue) = 0) then
        begin
          Result := False;
          ShowMessage(Format(rsVLEDuplicateKey,[NewValue, i + FixedRows]));
          if Editor is TStringCellEditor then TStringCelleditor(Editor).SelectAll;
          Break;
        end;
      end;
    end;
  end;
end;

class procedure TValueListEditor.WSRegisterClass;
begin
//  RegisterPropertyToSkip(Self, 'SomeProperty', 'VCL compatibility property', '');
  inherited WSRegisterClass;
end;

procedure Register;
begin
  RegisterComponents('Additional',[TValueListEditor]);
end;


end.

