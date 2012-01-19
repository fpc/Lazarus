unit ValEdit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Grids, LResources;

type

  { TValueListStrings }

  TValueListEditor = class;

  TValueListStrings = class(TStringList)
  private
    FOwner: TValueListEditor;
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
    procedure ShowColumnTitles;
    procedure AdjustColumnWidths; virtual;
    procedure AdjustRowCount; virtual;
    procedure ColWidthsChanged; override;
    function GetEditText(ACol, ARow: Integer): string; override;
    function GetCells(ACol, ARow: Integer): string; override;
    procedure SetCells(ACol, ARow: Integer; const AValue: string); override;
    procedure SetEditText(ACol, ARow: Integer; const Value: string); override;
    procedure TitlesChanged(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
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
    //property ColCount;
    //property Columns;
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
    property FixedRows;
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

    property OnEditButtonClick: TNotifyEvent read FOnEditButtonClick
      write SetOnEditButtonClick;
    property OnGetPickList: TGetPickListEvent read FOnGetPickList write FOnGetPickList;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnStringsChange: TNotifyEvent read GetOnStringsChange
      write SetOnStringsChange;
    property OnStringsChanging: TNotifyEvent read GetOnStringsChanging
      write SetOnStringsChanging;
    property OnValidate: TOnValidateEvent read FOnValidate write FOnValidate;

  end;

procedure Register;

implementation

{ TValueListStrings }

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
  with Columns.Add do
    Title.Caption := 'Key';
  with Columns.Add do begin
    Title.Caption := 'Value';
    DropDownRows := 8;
  end;
  // or: ColCount:=2;
//  inherited RowCount := 2;
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
      FixedRows := 1;
    end else
      FixedRows := 0;
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
begin
  if (doColumnTitles in DisplayOptions) and (TitleCaptions.Count > 0) then begin
    Columns[0].Title.Caption := TitleCaptions[0];
    Columns[1].Title.Caption := TitleCaptions[1];
    // or:
    //Cells[0,0]:=TitleCaptions[0];
    //Cells[1,0]:=TitleCaptions[1];
  end;
end;

procedure TValueListEditor.AdjustColumnWidths;
begin
// ToDo: Change column widths only if they are not set automatically (DisplayOptions).
// If key column is fixed then adjust only the second column,
//  otherwise adjust both columns propertionally.
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

function TValueListEditor.GetCells(ACol, ARow: Integer): string;
var
  I: Integer;
begin
  Result:='';
  if ARow=0 then begin
    if doColumnTitles in DisplayOptions then
      if ACol<FTitleCaptions.Count then
        Result:=FTitleCaptions[ACol];
    exit;
  end;
  I:=ARow-FixedRows;
  if Strings.Count<=I then exit;
  if ACol=0 then
    Result:=Strings.Names[I]
  else if ACol=1 then
    Result:=Strings.ValueFromIndex[I];
end;

procedure TValueListEditor.SetCells(ACol, ARow: Integer; const AValue: string);
var
  I: Integer;
  Line: string;
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

function TValueListEditor.GetEditText(ACol, ARow: Longint): string;
begin
  Result:= Cells[ACol, ARow];
  if Assigned(OnGetEditText) then
    OnGetEditText(Self, ACol, ARow, Result);
end;

procedure TValueListEditor.SetEditText(ACol, ARow: Longint; const Value: string);
begin
  inherited SetEditText(ACol, ARow, Value);
  Cells[ACol, ARow] := Value;
  // ToDo: There must be a check for duplicate keys but it must
  //  not be triggered while the user is typing.
  // The error must be postponed until user moves to other cell.
end;

procedure TValueListEditor.TitlesChanged(Sender: TObject);
begin
  // Refresh the display.
  ShowColumnTitles;
  AdjustRowCount;
  Invalidate;
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

