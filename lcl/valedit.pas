unit ValEdit;

{$mode objfpc}{$H+}

interface

uses
  Classes, Controls, StdCtrls, SysUtils, Grids, LResources, Dialogs, LazUtf8, variants, LCLProc;

type

  TValueListEditor = class;    // Forward declaration

  TEditStyle = (esSimple, esEllipsis, esPickList);

  { TItemProp }

  TItemProp = class(TPersistent)
  private
    FOwner: TValueListEditor;
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
//    procedure AssignTo(Dest: TPersistent); override;
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

  TItemProps = array of TItemProp;

  { TValueListStrings }

  TValueListStrings = class(TStringList)
  private
    FOwner: TValueListEditor;
    FItemProps: TItemProps;
    function GetItemProp(const AKeyOrIndex: Variant): TItemProp;
    procedure FreeItemProps;
  protected
    procedure SetTextStr(const Value: string); override;
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
    FOnEditButtonClick: TNotifyEvent;
    FOnStringsChange: TNotifyEvent;
    FOnStringsChanging: TNotifyEvent;
    FOnValidate: TOnValidateEvent;
    function GetFixedRows: Integer;
    function GetItemProp(const AKeyOrIndex: Variant): TItemProp;
    procedure SetFixedRows(AValue: Integer);
    procedure SetItemProp(const AKeyOrIndex: Variant; AValue: TItemProp);
    procedure StringsChange(Sender: TObject);
    procedure StringsChanging(Sender: TObject);
    procedure SelectValueEditor(Sender: TObject; aCol, aRow: Integer; var aEditor: TWinControl);
//    procedure EditButtonClick(Sender: TObject);
    function GetOptions: TGridOptions;
    function GetKey(Index: Integer): string;
    function GetValue(const Key: string): string;
    procedure SetDisplayOptions(const AValue: TDisplayOptions);
    procedure SetDropDownRows(const AValue: Integer);
    procedure SetKeyOptions({const} AValue: TKeyOptions);
    procedure SetKey(Index: Integer; const Value: string);
    procedure SetValue(const Key: string; AValue: string);
    procedure SetOnEditButtonClick(const AValue: TNotifyEvent);
    procedure SetOptions(const AValue: TGridOptions);
    procedure SetStrings(const AValue: TValueListStrings);
    procedure SetTitleCaptions(const AValue: TStrings);
  protected
    class procedure WSRegisterClass; override;
    procedure DoOnResize; override;
    procedure SetFixedCols(const AValue: Integer); override;
    procedure ShowColumnTitles;
    procedure AdjustColumnWidths; virtual;
    procedure AdjustRowCount; virtual;
    procedure ColWidthsChanged; override;
    procedure DefineCellsProperty(Filer: TFiler); override;
    function GetEditText(ACol, ARow: Integer): string; override;
    function GetCells(ACol, ARow: Integer): string; override;
    procedure SetCells(ACol, ARow: Integer; const AValue: string); override;
    procedure SetEditText(ACol, ARow: Longint; const Value: string); override;
    procedure TitlesChanged(Sender: TObject);
    function ValidateEntry(const ACol,ARow:Integer; const OldValue:string; var NewValue:string): boolean; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function InsertRow(const KeyName, Value: string; Append: Boolean): Integer;
    property FixedRows: Integer read GetFixedRows write SetFixedRows default 1;
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
    property AutoFillColumns;
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
    property RowCount;
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
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheel;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
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

    property OnEditButtonClick: TNotifyEvent read FOnEditButtonClick write SetOnEditButtonClick;
    property OnGetPickList: TGetPickListEvent read FOnGetPickList write FOnGetPickList;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnStringsChange: TNotifyEvent read FOnStringsChange write FOnStringsChange;
    property OnStringsChanging: TNotifyEvent read FOnStringsChanging write FOnStringsChanging;
    property OnValidate: TOnValidateEvent read FOnValidate write FOnValidate;

  end;

const
  //ToDo: Make this a resourcestring in lclstrconsts unit, once we are satisfied with the implementation of validating
  rsVLEDuplicateKey = 'Duplicate Key:'+LineEnding+'A key with name "%s" already exists at column %d';
  //ToDo: Make this a resourcestring in lclstrconsts unit, once we are satisfied with ShowColumnTitles
  rsVLEKey = 'Key';
  rsVLEName = 'Name';

procedure Register;

implementation

{ TItemProp }

constructor TItemProp.Create(AOwner: TValueListEditor);
begin
  inherited Create;
  FOwner := AOwner;
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
  with FOwner do
    if EditorMode and (FStrings.UpdateCount = 0) then
      InvalidateCell(Col, Row);
end;

procedure TItemProp.SetMaxLength(const AValue: Integer);
begin
  FMaxLength := AValue;
  with FOwner do
    if EditorMode and (FStrings.UpdateCount = 0) then
      InvalidateCell(Col, Row);
end;

procedure TItemProp.SetReadOnly(const AValue: Boolean);
begin
  FReadOnly := AValue;
  with FOwner do
    if EditorMode and (FStrings.UpdateCount = 0) then
      InvalidateCell(Col, Row);
end;

procedure TItemProp.SetEditStyle(const AValue: TEditStyle);
begin
  FEditStyle := AValue;
  with FOwner do
    if EditorMode and (FStrings.UpdateCount = 0) then
      InvalidateCell(Col, Row);
end;

procedure TItemProp.SetPickList(const AValue: TStrings);
begin
  GetPickList.Assign(AValue);
  with FOwner do
    if EditorMode and (FStrings.UpdateCount = 0) then
      InvalidateCell(Col, Row);
end;

procedure TItemProp.SetKeyDesc(const AValue: string);
begin
  FKeyDesc := AValue;
end;

{ TValueListStrings }

procedure TValueListStrings.SetTextStr(const Value: string);
begin
  // ToDo: Assign also ItemProps ???
  inherited SetTextStr(Value);
end;

procedure TValueListStrings.InsertItem(Index: Integer; const S: string; AObject: TObject);
var
  i: Integer;
  IsShowingEditor: Boolean;
begin
  // ToDo: Check validity of key
  //debugln('TValueListStrings.InsertItem: Index=',dbgs(index),' S=',S,' AObject=',dbgs(aobject));
  Changing;
  IsShowingEditor := goAlwaysShowEditor in FOwner.Options;
  if IsShowingEditor then FOwner.Options := FOwner.Options - [goAlwaysShowEditor];
  inherited InsertItem(Index, S, AObject);
  if IsShowingEditor then FOwner.Options := FOwner.Options + [goAlwaysShowEditor];
  SetLength(FItemProps, Count);
  for i := Count-2 downto Index do
    FItemProps[i+1] := FItemProps[i];
  FItemProps[Index] := nil;
  Changed;
end;

procedure TValueListStrings.InsertItem(Index: Integer; const S: string);
var
  IsShowingEditor: Boolean;
begin
  // ToDo: Check validity of key
  //debugln('TValueListStrings.InsertItem: Index=',dbgs(index),' S=',S);
  IsShowingEditor := goAlwaysShowEditor in FOwner.Options;
  if IsShowingEditor then FOwner.Options := FOwner.Options - [goAlwaysShowEditor];
  inherited InsertItem(Index, S);
  if IsShowingEditor then FOwner.Options := FOwner.Options + [goAlwaysShowEditor];
end;

procedure TValueListStrings.Put(Index: Integer; const S: String);
var
  IsShowingEditor: Boolean;
begin
  // ToDo: Check validity of key
  IsShowingEditor := goAlwaysShowEditor in FOwner.Options;
  if IsShowingEditor then FOwner.Options := FOwner.Options - [goAlwaysShowEditor];
  inherited Put(Index, S);
  if IsShowingEditor then FOwner.Options := FOwner.Options + [goAlwaysShowEditor];
end;

constructor TValueListStrings.Create(AOwner: TValueListEditor);
begin
  inherited Create;
  FOwner := AOwner;
end;

destructor TValueListStrings.Destroy;
begin
  FreeItemProps;
  inherited Destroy;
end;

procedure TValueListStrings.Assign(Source: TPersistent);
begin
  // ToDo: Assign also ItemProps if Source is TValueListStrings
  inherited Assign(Source);
end;

procedure TValueListStrings.Clear;
var
  IsShowingEditor: Boolean;
begin
  IsShowingEditor := goAlwaysShowEditor in FOwner.Options;
  if IsShowingEditor then FOwner.Options := FOwner.Options - [goAlwaysShowEditor];
  inherited Clear;
  if IsShowingEditor then FOwner.Options := FOwner.Options + [goAlwaysShowEditor];
  FreeItemProps;
end;

procedure TValueListStrings.CustomSort(Compare: TStringListSortCompare);
begin
  inherited CustomSort(Compare);
  // ToDo: Sort also ItemProps using a copy of the orignal order
end;

procedure TValueListStrings.Delete(Index: Integer);
var
  i: Integer;
  IsShowingEditor: Boolean;
begin
  Changing;
  IsShowingEditor := goAlwaysShowEditor in FOwner.Options;
  if IsShowingEditor then FOwner.Options := FOwner.Options - [goAlwaysShowEditor];
  inherited Delete(Index);
  if IsShowingEditor then FOwner.Options := FOwner.Options + [goAlwaysShowEditor];
  // Delete also ItemProps
  if Index<=Count then begin
    FItemProps[Index].Free;
    for i := Index to Length(FItemProps)-1 do
      FItemProps[i] := FItemProps[i+1];
    SetLength(FItemProps, Count);
  end;
  Changed;
end;

procedure TValueListStrings.Exchange(Index1, Index2: Integer);
begin
  Changing;
  inherited Exchange(Index1, Index2);
  // ToDo: Exchange also ItemProps
  Changed;
end;

function TValueListStrings.GetItemProp(const AKeyOrIndex: Variant): TItemProp;
var
  i: Integer;
  s: string;
begin
  Result := Nil;
  if Count > 0 then
  begin
    if VarIsOrdinal(AKeyOrIndex) then
      i := AKeyOrIndex
    else begin
      s := AKeyOrIndex;
      i := IndexOfName(s);
      if i = -1 then
        raise Exception.Create('TValueListStrings.GetItemProp: Key not found: '+s);
    end;
    if i >= Length(FItemProps) then
      SetLength(FItemProps, i+1);
    Result := FItemProps[i];
    if not Assigned(Result) then begin
      Result := TItemProp.Create(FOwner);
      FItemProps[i] := Result;
    end;
  end;
end;

procedure TValueListStrings.FreeItemProps;
var
  i: Integer;
begin
  //{$R+}
  //debugln('TValueListStrings.Destroy: Length(FItemProps) = ',dbgs(Length(FItemProps)));
  for i := 0 to Length(FItemProps) - 1 do
  begin
    if Assigned(FItemProps[i]) then FItemProps[i].Free;
  end;
  SetLength(FItemProps, 0);
end;

{ TValueListEditor }

constructor TValueListEditor.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FStrings := TValueListStrings.Create(Self);
  FStrings.OnChange := @StringsChange;
  FStrings.OnChanging := @StringsChanging;
  FTitleCaptions := TStringList.Create;
  TStringList(FTitleCaptions).OnChange := @TitlesChanged;
  OnSelectEditor := @SelectValueEditor;
//  OnEditButtonClick := @EditButtonClick;

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
  inherited RowCount := 2;
  FixedCols := 0;
//  DefaultColWidth := 150;
//  DefaultRowHeight := 18;
//  Width := 306;
//  Height := 300;
  Options := [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine,
              goColSizing, goEditing, goAlwaysShowEditor, goThumbTracking];
  FDisplayOptions := [doColumnTitles, doAutoColResize, doKeyColFixed];
  Col := 1;
  FDropDownRows := 8;
  ShowColumnTitles;
end;

destructor TValueListEditor.Destroy;
begin
  FTitleCaptions.Free;
  FStrings.Free;
  inherited Destroy;
end;

function TValueListEditor.InsertRow(const KeyName, Value: string; Append: Boolean): Integer;
var
  NewInd: Integer;
begin
  Result := Row;
  if (Row > Strings.Count) or ((Row - FixedRows) >= Strings.Count)
  or (Cells[0, Row] <> '') or (Cells[1, Row] <> '') then
  begin                                    // Add a new Key=Value pair
    Strings.BeginUpdate;
    try
      if Append then
        NewInd := Strings.Count
      else
        NewInd := Result - FixedRows;
      Strings.InsertItem(NewInd, KeyName+'='+Value, Nil);
    finally
      Strings.EndUpdate;
    end;
  end
  else begin   // Use an existing row, just update the Key and Value.
    Cells[0, Result] := KeyName;
    Cells[1, Result] := Value;
  end;
end;

procedure TValueListEditor.StringsChange(Sender: TObject);
begin
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

procedure TValueListEditor.SelectValueEditor(Sender: TObject; aCol, aRow: Integer;
  var aEditor: TWinControl);
// Choose the cell editor based on ItemProp.EditStyle
var
  ItemProp: TItemProp;
begin
  if aCol <> 1 then Exit;     // Only for the Value column
  ItemProp := Strings.GetItemProp(aRow-FixedRows);
  if Assigned(ItemProp) then
    case ItemProp.EditStyle of
      esSimple: aEditor := EditorByStyle(cbsAuto);
      esEllipsis: aEditor := EditorByStyle(cbsEllipsis);
      esPickList: begin
        aEditor := EditorByStyle(cbsPickList);
        (aEditor as TCustomComboBox).Items.Assign(ItemProp.PickList);
        //Style := csDropDown, default = csDropDownList;
      end;
    end;
end;

// Triggering Action ...
//procedure TValueListEditor.EditButtonClick(Sender: TObject);
//begin
  // ToDo: support button clicks
//end;

function TValueListEditor.GetFixedRows: Integer;
begin
  Result := inherited FixedRows;
end;

procedure TValueListEditor.SetFixedCols(const AValue: Integer);
begin
  if (AValue in [0,1]) then
    inherited SetFixedCols(AValue);
end;

procedure TValueListEditor.SetFixedRows(AValue: Integer);
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
  if (doColumnTitles in DisplayOptions) <> (doColumnTitles in AValue) then
    if doColumnTitles in AValue then begin
      if RowCount < 2 then
        inherited RowCount := 2;
      inherited FixedRows := 1;
    end else
      inherited FixedRows := 0;
  FDisplayOptions := AValue;
  ShowColumnTitles;
  AdjustColumnWidths;
  AdjustRowCount;
  Invalidate;
end;

procedure TValueListEditor.SetDropDownRows(const AValue: Integer);
begin
  FDropDownRows := AValue;
  // ToDo: If edit list for inplace editing is implemented, set its handler, too.
end;

procedure TValueListEditor.SetKeyOptions({const} AValue: TKeyOptions);
begin
  // ToDo: Disable Add or enable Edit based on current value.
  // Enable Edit when Adding, disable Add when Editing.
  // Change Col if needed when editing keys is disabled.
  FKeyOptions := AValue;
end;

procedure TValueListEditor.SetOnEditButtonClick(const AValue: TNotifyEvent);
begin
  FOnEditButtonClick := AValue;
  // If edit list for inplace editing is implemented, set its handler, too.
end;

procedure TValueListEditor.SetOptions(const AValue: TGridOptions);
begin
  if not (goColMoving in Options) then
    inherited Options := AValue;
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
  else
    Strings.Add(Key+'='+AValue);
end;

procedure TValueListEditor.ShowColumnTitles;
var
  KeyCap, ValCap: String;
begin
  if (doColumnTitles in DisplayOptions) then
  begin
    KeyCap := rsVLEKey;
    ValCap := rsVLEName;
    if (TitleCaptions.Count > 0) then KeyCap := TitleCaptions[0];
    if (TitleCaptions.Count > 1) then ValCap := TitleCaptions[1];
    //Columns[0].Title.Caption := KeyCap;
    //Columns[1].Title.Caption := ValCap;
    //or:
    Cells[0,0] := KeyCap;
    Cells[1,0] := ValCap;
  end;
end;

procedure TValueListEditor.AdjustColumnWidths;
// If key column is fixed in width then adjust only the second column,
//  otherwise adjust both columns propertionally.
var
  CW: Integer;
begin
  CW := ClientWidth;
  if  (doKeyColFixed in DisplayOptions) then
  begin
    //AutoSizeColumn(0);
    ColWidths[1] := CW - ColWidths[0];
  end
  else
  begin
    ColWidths[0] := CW div 2;
    ColWidths[1] := CW div 2;
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

procedure TValueListEditor.ColWidthsChanged;
begin
  AdjustColumnWidths;
  inherited;
end;

procedure TValueListEditor.DefineCellsProperty(Filer: TFiler);
begin
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
    if Strings.Count<=I then exit;
    if ACol=0 then
      Result:=Strings.Names[I]
    else if ACol=1 then
      Result:=Strings.ValueFromIndex[I];
  end;
end;

procedure TValueListEditor.SetCells(ACol, ARow: Integer; const AValue: string);
var
  I: Integer;
  Line: string;
begin
  if (ARow = 0) and (doColumnTitles in DisplayOptions) then
  begin
    Inherited SetCells(ACol, ARow, AValue);
  end
  else
  begin
    I:=ARow-FixedRows;
    if ACol=0 then
      Line:=AValue+'='+Cells[1,ARow]
    else
      Line:=Cells[0,ARow]+'='+AValue;
    if I>=Strings.Count then
      Strings.Insert(I,Line)
    else
      Strings[I]:=Line;
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
  if ((ACol - FixedCols) = 0) then
  begin//Check for duplicate key names (only in "Key" column)
    Index := ARow - FixedRows;
    for i := 0 to FStrings.Count - 1 do
    begin
      if (Index <> i) then
      begin
        if (Utf8CompareText(FStrings.Names[i], NewValue) = 0) then
        begin
          Result := False;
          ShowMessage(Format(rsVLEDuplicateKey,[NewValue, i + FixedRows]));
          if Editor is TStringCellEditor then TStringCelleditor(Editor).SelectAll;
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

procedure TValueListEditor.DoOnResize;
begin
  inherited DoOnResize;
  if (doAutoColResize in DisplayOptions) then AdjustColumnWidths;
end;


procedure Register;
begin
  RegisterComponents('Additional',[TValueListEditor]);
end;


end.

