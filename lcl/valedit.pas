unit ValEdit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Grids, LResources, Dialogs, LazUtf8;

type

  { TValueListStrings }

  TValueListEditor = class;

  TValueListStrings = class(TStringList)
  private
    FOwner: TValueListEditor;
  protected
    procedure SetTextStr(const Value: string); override;
  public
    constructor Create(AOwner: TValueListEditor);
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  end;

  { TValueListEditor }

  TDisplayOption = (doColumnTitles, doAutoColResize, doKeyColFixed);
  TDisplayOptions = set of TDisplayOption;

  TKeyOption = (keyEdit, keyAdd, keyDelete, keyUnique);
  TKeyOptions = set of TKeyOption;

  TGetPickListEvent = procedure(Sender: TObject; const KeyName: string;
    Values: TStrings) of object;

  TOnValidateEvent = procedure(Sender: TObject; ACol, ARow: Longint;
    const KeyName, KeyValue: string) of object;

  TValueListEditor = class(TCustomStringGrid)
  private
    FTitleCaptions: TStrings;
    FStrings: TStrings;
    FKeyOptions: TKeyOptions;
    FDisplayOptions: TDisplayOptions;
    FDropDownRows: Integer;
    FOnGetPickList: TGetPickListEvent;
    FOnEditButtonClick: TNotifyEvent;
    FOnValidate: TOnValidateEvent;
    function GetFixedRows: Integer;
    procedure SetFixedRows(AValue: Integer);
    function GetOnStringsChange: TNotifyEvent;
    function GetOnStringsChanging: TNotifyEvent;
    function GetOptions: TGridOptions;
    function GetKey(Index: Integer): string;
    function GetValue(const Key: string): string;
    procedure SetDisplayOptions(const AValue: TDisplayOptions);
    procedure SetDropDownRows(const AValue: Integer);
    procedure SetKeyOptions({const} AValue: TKeyOptions);
    procedure SetKey(Index: Integer; const Value: string);
    procedure SetValue(const Key, Value: string);
    procedure SetOnEditButtonClick(const AValue: TNotifyEvent);
    procedure SetOnStringsChange(const AValue: TNotifyEvent);
    procedure SetOnStringsChanging(const AValue: TNotifyEvent);
    procedure SetOptions(const AValue: TGridOptions);
    procedure SetStrings(const AValue: TStrings);
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
    property FixedRows: Integer read GetFixedRows write SetFixedRows default 1;
    property Modified;
    property Keys[Index: Integer]: string read GetKey write SetKey;
    property Values[const Key: string]: string read GetValue write SetValue;
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
    property Strings: TStrings read FStrings write SetStrings;
    property TitleCaptions: TStrings read FTitleCaptions write SetTitleCaptions;

    property OnEditButtonClick: TNotifyEvent read FOnEditButtonClick write SetOnEditButtonClick;
    property OnGetPickList: TGetPickListEvent read FOnGetPickList write FOnGetPickList;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnStringsChange: TNotifyEvent read GetOnStringsChange
      write SetOnStringsChange;
    property OnStringsChanging: TNotifyEvent read GetOnStringsChanging
      write SetOnStringsChanging;
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

{ TValueListStrings }

procedure TValueListStrings.SetTextStr(const Value: string);
var
  IsShowingEditor: Boolean;
begin
  with FOwner do begin
    // Don't show editor while changing values. Edited cell would not be changed.
    IsShowingEditor := goAlwaysShowEditor in Options;
    Options := Options - [goAlwaysShowEditor];
    inherited SetTextStr(Value);
    if IsShowingEditor then
      Options := Options + [goAlwaysShowEditor];
  end;
end;

constructor TValueListStrings.Create(AOwner: TValueListEditor);
begin
  inherited Create;
  FOwner := AOwner;
end;

destructor TValueListStrings.Destroy;
begin
  inherited Destroy;
end;

procedure TValueListStrings.Assign(Source: TPersistent);
var
  IsShowingEditor: Boolean;
begin
  with FOwner do begin
    // Don't show editor while changing values. Edited cell would not be changed.
    IsShowingEditor := goAlwaysShowEditor in Options;
    Options := Options - [goAlwaysShowEditor];
    inherited Assign(Source);
    if IsShowingEditor then
      Options := Options + [goAlwaysShowEditor];
  end;
end;

{ TValueListEditor }

constructor TValueListEditor.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FStrings := TValueListStrings.Create(Self);
  // NOTE: here should be a handler for Strings.OnChange event
  //       so changing externally any value (or count) would be
  //       reflected in grid
  FTitleCaptions := TStringList.Create;
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

function TValueListEditor.GetOnStringsChange: TNotifyEvent;
begin
  Result := nil;       // Placeholder for Delphi compatibility.
end;

function TValueListEditor.GetOnStringsChanging: TNotifyEvent;
begin
  Result := nil;       // Placeholder for Delphi compatibility.
end;

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

procedure TValueListEditor.SetOnStringsChange(const AValue: TNotifyEvent);
begin
  ;                    // Placeholder for Delphi compatibility.
end;

procedure TValueListEditor.SetOnStringsChanging(const AValue: TNotifyEvent);
begin
  ;                    // Placeholder for Delphi compatibility.
end;

procedure TValueListEditor.SetOptions(const AValue: TGridOptions);
begin
  // ToDo: Check that column is not moving (goColMoving in Options).
  inherited Options := AValue;
end;

procedure TValueListEditor.SetStrings(const AValue: TStrings);
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
  if Row > -1 then begin
    Inc(I, FixedRows);
    Result:=Cells[1,I];
  end;
end;

procedure TValueListEditor.SetValue(const Key, Value: string);
var
  I: Integer;
begin
  I := Strings.IndexOfName(Key);
  if Row > -1 then begin
    Inc(I, FixedRows);
    Cells[1,I]:=Value;
  end
  else
    Strings.Add(Key+'='+Value);
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

