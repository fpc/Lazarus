unit ModeMatrixCtrl;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, math, types, contnrs, Controls, LCLType, LCLIntf,
  Grids, Graphics, StdCtrls, Menus, LazLogger;

const
  DefaultGroupMatrixIndent = 10;
type
  TGroupedMatrix = class;
  TGroupedMatrixGroup = class;
  TGroupedMatrixControl = class;

  { TGroupedMatrixRow }

  TGroupedMatrixRow = class(TPersistent)
  private
    FMatrix: TGroupedMatrix;
    FGroup: TGroupedMatrixGroup;
    FRowInGrid: integer;
    procedure SetGroup(AValue: TGroupedMatrixGroup);
  public
    procedure Assign(Source: TPersistent); override;
    constructor Create(aMatrix: TGroupedMatrix); virtual;
    destructor Destroy; override;
    procedure Clear; virtual;
    function Equals(Obj: TObject): boolean; override;
    property Matrix: TGroupedMatrix read FMatrix;
    property Group: TGroupedMatrixGroup read FGroup write SetGroup;
    function GetGroupIndex: integer;
    property RowInGrid: integer read FRowInGrid; // in Grid, not in Group
    function Level: integer;
    function GetNextSibling: TGroupedMatrixRow;
    function GetNext: TGroupedMatrixRow; virtual;
    function GetNextSkipChildren: TGroupedMatrixRow;
    function GetPreviousSibling: TGroupedMatrixRow;
    function GetPrevious: TGroupedMatrixRow; // the reverse of GetNext
    function GetLastLeaf: TGroupedMatrixRow; virtual; // get last child of last child of ...
    function GetTopLvlItem: TGroupedMatrixRow;
    function AsString: string; virtual;
  end;

  { TGroupedMatrixGroup }

  TGroupedMatrixGroup = class(TGroupedMatrixRow)
  private
    FCaption: TCaption;
    FColor: TColor;
    FItems: TFPList; // list of TGroupedMatrixRow
    function GetCount: integer;
    function GetItems(Index: integer): TGroupedMatrixRow;
    procedure SetCaption(AValue: TCaption);
    procedure SetColor(AValue: TColor);
  public
    procedure Assign(Source: TPersistent); override;
    constructor Create(aControl: TGroupedMatrix); override;
    destructor Destroy; override;
    procedure Clear; override;
    function Equals(Obj: TObject): boolean; override;
    property Caption: TCaption read FCaption write SetCaption;
    property Count: integer read GetCount;
    property Items[Index: integer]: TGroupedMatrixRow read GetItems; default;
    function IndexOfRow(aRow: TGroupedMatrixRow): integer;
    function GetNext: TGroupedMatrixRow; override;
    function GetLastLeaf: TGroupedMatrixRow; override;
    property Color: TColor read FColor write SetColor;
    function GetEffectiveColor: TColor;
    function AsString: string; override;
  end;

  { TGroupedMatrixValue }

  TGroupedMatrixValue = class(TGroupedMatrixRow)
  private
    FModes: TStrings;
    FTyp: string;
    FValue: string;
    procedure SetModes(AValue: TStrings);
    procedure SetTyp(AValue: string);
    procedure SetValue(AValue: string);
  public
    procedure Assign(Source: TPersistent); override;
    constructor Create(aControl: TGroupedMatrix); override;
    destructor Destroy; override;
    function Equals(Obj: TObject): boolean; override;
    property Value: string read FValue write SetValue;
    property Typ: string read FTyp write SetTyp;
    property Modes: TStrings read FModes write SetModes;
    function AsString: string; override;
  end;

  { TGroupedMatrixMode }

  TGroupedMatrixMode = class(TPersistent)
  private
    FCaption: string;
    FColor: TColor;
  public
    procedure Assign(Source: TPersistent); override;
    constructor Create;
    function Equals(Obj: TObject): boolean; override;
    property Caption: string read FCaption write FCaption;
    property Color: TColor read FColor write FColor;
  end;

  { TGroupedMatrixModes }

  TGroupedMatrixModes = class(TPersistent)
  private
    FActive: integer;
    fItems: TFPList; // list of TGroupedMatrixMode
    function GetItems(Index: integer): TGroupedMatrixMode;
  public
    procedure Assign(Source: TPersistent); override;
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    function Equals(Obj: TObject): boolean; override;
    property Items[Index: integer]: TGroupedMatrixMode read GetItems; default;
    function Count: integer;
    function Add(aCaption: string; aColor: TColor = clDefault): TGroupedMatrixMode;
    property Active: integer read FActive write FActive;
  end;

  { TGroupedMatrix }

  TGroupedMatrix = class(TPersistent)
  private
    FControl: TGroupedMatrixControl;
    FModes: TGroupedMatrixModes;
    FRows: TFPList; // list of TGroupedMatrixRow
    FTopLvlRows: TFPList; // list of TGroupedMatrixRow with Level=0
    function GetRowCount: integer;
    function GetRows(Index: integer): TGroupedMatrixRow;
    function GetTopLvlCount: integer;
    function GetTopLvlItems(Index: integer): TGroupedMatrixRow;
    procedure InternalAdd(Group: TGroupedMatrixGroup; Row: TGroupedMatrixRow);
    procedure RebuildRows;
  public
    procedure Assign(Source: TPersistent); override;
    constructor Create(aControl: TGroupedMatrixControl);
    destructor Destroy; override;
    procedure Clear;
    function Equals(Obj: TObject): boolean; override;
    property RowCount: integer read GetRowCount;
    property Rows[Index: integer]: TGroupedMatrixRow read GetRows; default;
    function IndexOfRow(Row: TGroupedMatrixRow): integer;
    property TopLvlCount: integer read GetTopLvlCount;
    property TopLvlItems[Index: integer]: TGroupedMatrixRow read GetTopLvlItems;
    function IndexOfTopLvlItem(Row: TGroupedMatrixRow): integer;
    function IndexOfTopLvlGroup(aCaption: TCaption): integer;
    function GetTopLvlGroup(aCaption: TCaption): TGroupedMatrixGroup;
    function GetMaxLevel: integer;
    function AddGroup(ParentGroup: TGroupedMatrixGroup;
      aCaption: TCaption): TGroupedMatrixGroup;
    function AddValue(ParentGroup: TGroupedMatrixGroup;
      ModesAsText, aType, AValue: string): TGroupedMatrixValue;
    property Modes: TGroupedMatrixModes read FModes;
    property Control: TGroupedMatrixControl read FControl;
  end;

  { TGroupedMatrixControl }

  TGroupedMatrixControl = class(TCustomDrawGrid)
  private
    FActiveModeColor: TColor;
    FIndent: integer;
    FMatrix: TGroupedMatrix;
    FMaxUndo: integer;
    FTypeColumn: TGridColumn;
    FValueColumn: TGridColumn;
    fTypePopupMenu: TPopupMenu;
    fTypePopupMenuRow: integer; // grid row of fTypePopupMenu
    fUndoItems: TObjectList; // list of TGroupedMatrix, 0=oldest
    fRedoItems: TObjectList; // list of TGroupedMatrix, 0=oldest
    function GetModeColumns(Index: integer): TGridColumn;
    function GetModes: TGroupedMatrixModes;
    procedure SetActiveModeColor(AValue: TColor);
    procedure SetIndent(AValue: integer);
    procedure SetMaxUndo(AValue: integer);
    procedure ToggleModeValue(ValueRow: TGroupedMatrixValue; aRow, aCol: integer);
    procedure PopupTypes(aRow: integer);
    procedure OnTypePopupMenuClick(Sender: TObject);
  protected
    function GetCells(ACol, ARow: Integer): string; override;
    function GetEditText(aCol, aRow: Longint): string; override;
    procedure AutoLayout; virtual;
    procedure CreateWnd; override;
    procedure DrawCellGrid(aCol, aRow: Integer; aRect: TRect;
      aState: TGridDrawState); override;
    procedure DrawIndent(aRow: integer; aRect: TRect);
    procedure DrawRow(aRow: Integer); override;
    function EditingAllowed(ACol: Integer=-1): Boolean; override;
    procedure GetCheckBoxState(const aCol, aRow: Integer;
      var aState: TCheckboxState); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer
      ); override;
    procedure PrepareCanvas(aCol, aRow: Integer; aState: TGridDrawState);
      override;
    procedure PrepareGridCanvas;
    procedure SetEditText(ACol, ARow: Longint; const Value: string); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Clear;
  public
    property ActiveModeColor: TColor read FActiveModeColor write SetActiveModeColor;
    procedure DefaultDrawCell(aCol, aRow: Integer; var aRect: TRect;
      aState: TGridDrawState); override;
    property Matrix: TGroupedMatrix read FMatrix;
    property Modes: TGroupedMatrixModes read GetModes;
    procedure MatrixChanged;
    function ModeColFirst: integer;
    function ModeColLast: integer;
    property ModeColumns[Index: integer]: TGridColumn read GetModeColumns;
    property TypeColumn: TGridColumn read FTypeColumn;
    function TypeCol: integer;
    property ValueColumn: TGridColumn read FValueColumn;
    function ValueCol: integer;
    property Indent: integer read FIndent write SetIndent default DefaultGroupMatrixIndent;
  public
    // undo/redo
    function CanUndo: boolean;
    function CanRedo: boolean;
    procedure Undo;
    procedure Redo;
    property MaxUndo: integer read FMaxUndo write SetMaxUndo;
    procedure StoreUndo(EvenIfNothingChanged: boolean);
  end;

function VerticalIntersect(const aRect,bRect: TRect): boolean;
function HorizontalIntersect(const aRect,bRect: TRect): boolean;

implementation

function VerticalIntersect(const aRect,bRect: TRect): boolean;
begin
  result := (aRect.Top < bRect.Bottom) and (aRect.Bottom > bRect.Top);
end;

function HorizontalIntersect(const aRect,bRect: TRect): boolean;
begin
  result := (aRect.Left < bRect.Right) and (aRect.Right > bRect.Left);
end;

{ TGroupedMatrixMode }

procedure TGroupedMatrixMode.Assign(Source: TPersistent);
var
  aSource: TGroupedMatrixMode;
begin
  if Source is TGroupedMatrixMode then
  begin
    aSource:=TGroupedMatrixMode(Source);
    Color:=aSource.Color;
    Caption:=aSource.Caption;
  end else
    inherited Assign(Source);
end;

constructor TGroupedMatrixMode.Create;
begin
  FColor:=clDefault;
end;

function TGroupedMatrixMode.Equals(Obj: TObject): boolean;
var
  SrcMode: TGroupedMatrixMode;
begin
  Result:=false;
  if not (Obj is TGroupedMatrixMode) then exit;
  SrcMode:=TGroupedMatrixMode(Obj);
  if SrcMode.Caption<>Caption then exit;
  if SrcMode.Color<>Color then exit;
  Result:=true;
end;

{ TGroupedMatrixModes }

function TGroupedMatrixModes.GetItems(Index: integer): TGroupedMatrixMode;
begin
  Result:=TGroupedMatrixMode(fItems[Index]);
end;

procedure TGroupedMatrixModes.Assign(Source: TPersistent);
var
  SrcModes: TGroupedMatrixModes;
  i: Integer;
  SrcMode: TGroupedMatrixMode;
  NewMode: TGroupedMatrixMode;
begin
  if Source is TGroupedMatrixModes then
  begin
    SrcModes:=TGroupedMatrixModes(Source);
    Active:=SrcModes.Active;
    Clear;
    for i:=0 to SrcModes.Count-1 do begin
      SrcMode:=SrcModes[i];
      NewMode:=TGroupedMatrixMode(SrcMode.ClassType).Create;
      fItems.Add(NewMode);
      NewMode.Assign(SrcMode);
    end;
  end else
    inherited Assign(Source);
end;

constructor TGroupedMatrixModes.Create;
begin
  fItems:=TFPList.Create;
  FActive:=-1;
end;

destructor TGroupedMatrixModes.Destroy;
begin
  Clear;
  FreeAndNil(fItems);
  inherited Destroy;
end;

procedure TGroupedMatrixModes.Clear;
var
  i: Integer;
begin
  FActive:=-1;
  for i:=0 to fItems.Count-1 do
    Items[i].Free;
  fItems.Clear;
end;

function TGroupedMatrixModes.Equals(Obj: TObject): boolean;
var
  SrcModes: TGroupedMatrixModes;
  i: Integer;
begin
  Result:=false;
  if not (Obj is TGroupedMatrixModes) then exit;
  SrcModes:=TGroupedMatrixModes(Obj);
  if SrcModes.Count<>Count then exit;
  if SrcModes.Active<>Active then exit;
  for i:=0 to Count-1 do
    if not SrcModes[i].Equals(Items[i]) then exit;
  Result:=true;
end;

function TGroupedMatrixModes.Count: integer;
begin
  Result:=fItems.Count;
end;

function TGroupedMatrixModes.Add(aCaption: string; aColor: TColor
  ): TGroupedMatrixMode;
begin
  Result:=TGroupedMatrixMode.Create;
  Result.Caption:=aCaption;
  Result.Color:=aColor;
  fItems.Add(Result);
end;

{ TGroupedMatrix }

function TGroupedMatrix.GetRows(Index: integer): TGroupedMatrixRow;
begin
  Result:=TGroupedMatrixRow(FRows[Index]);
end;

function TGroupedMatrix.GetRowCount: integer;
begin
  Result:=FRows.Count;
end;

function TGroupedMatrix.GetTopLvlCount: integer;
begin
  Result:=FTopLvlRows.Count;
end;

function TGroupedMatrix.GetTopLvlItems(Index: integer): TGroupedMatrixRow;
begin
  Result:=TGroupedMatrixRow(FTopLvlRows[Index]);
end;

procedure TGroupedMatrix.InternalAdd(Group: TGroupedMatrixGroup;
  Row: TGroupedMatrixRow);
begin
  if Group=nil then begin
    FTopLvlRows.Add(Row);
    Row.FRowInGrid:=FRows.Count;
    FRows.Add(Row);
  end else begin
    Row.Group:=Group;
    RebuildRows;
  end;
end;

procedure TGroupedMatrix.RebuildRows;

  procedure Traverse(Row: TGroupedMatrixRow);
  var
    i: Integer;
  begin
    Row.FRowInGrid:=FRows.Count;
    FRows.Add(Row);
    if Row is TGroupedMatrixGroup then
      for i:=0 to TGroupedMatrixGroup(Row).Count-1 do
        Traverse(TGroupedMatrixGroup(Row)[i]);
  end;

var
  i: Integer;
begin
  FRows.Clear;
  for i:=0 to TopLvlCount-1 do
    Traverse(TopLvlItems[i]);
end;

procedure TGroupedMatrix.Assign(Source: TPersistent);
var
  SrcMatrix: TGroupedMatrix;
  i: Integer;
  SrcRow: TGroupedMatrixRow;
  NewRow: TGroupedMatrixRow;
begin
  if Source is TGroupedMatrix then
  begin
    SrcMatrix:=TGroupedMatrix(Source);
    Clear;
    Modes.Assign(SrcMatrix.Modes);
    for i:=0 to SrcMatrix.TopLvlCount-1 do begin
      SrcRow:=SrcMatrix.TopLvlItems[i];
      NewRow:=TGroupedMatrixRow(SrcRow.ClassType).Create(Self);
      FTopLvlRows.Add(NewRow);
      NewRow.Assign(SrcRow);
    end;
    RebuildRows;
  end else
    inherited Assign(Source);
end;

constructor TGroupedMatrix.Create(aControl: TGroupedMatrixControl);
begin
  FControl:=aControl;
  FRows:=TFPList.Create;
  FTopLvlRows:=TFPList.Create;
  FModes:=TGroupedMatrixModes.Create;
end;

destructor TGroupedMatrix.Destroy;
begin
  Clear;
  FreeAndNil(FModes);
  FreeAndNil(FRows);
  FreeAndNil(FTopLvlRows);
  inherited Destroy;
end;

procedure TGroupedMatrix.Clear;
var
  i: Integer;
begin
  for i:=TopLvlCount-1 downto 0 do
    TopLvlItems[i].Free;
  FModes.Clear;
end;

function TGroupedMatrix.Equals(Obj: TObject): boolean;
var
  SrcMatrix: TGroupedMatrix;
  i: Integer;
begin
  Result:=false;
  if not (Obj is TGroupedMatrix) then exit;
  SrcMatrix:=TGroupedMatrix(Obj);
  if SrcMatrix.RowCount<>RowCount then exit;
  if SrcMatrix.TopLvlCount<>TopLvlCount then exit;
  if not SrcMatrix.Modes.Equals(Modes) then exit;
  for i:=0 to TopLvlCount-1 do
    if not SrcMatrix.TopLvlItems[i].Equals(TopLvlItems[i]) then exit;
  Result:=true;
end;

function TGroupedMatrix.IndexOfRow(Row: TGroupedMatrixRow): integer;
begin
  for Result:=0 to RowCount-1 do
    if Row=Rows[Result] then exit;
  Result:=-1;
end;

function TGroupedMatrix.IndexOfTopLvlItem(Row: TGroupedMatrixRow): integer;
begin
  for Result:=0 to TopLvlCount-1 do
    if Row=TopLvlItems[Result] then exit;
  Result:=-1;
end;

function TGroupedMatrix.GetMaxLevel: integer;
var
  i: Integer;
begin
  Result:=0;
  for i:=0 to RowCount-1 do
    Result:=Max(Result,Rows[i].Level);
end;

function TGroupedMatrix.AddGroup(ParentGroup: TGroupedMatrixGroup;
  aCaption: TCaption): TGroupedMatrixGroup;
begin
  Result:=TGroupedMatrixGroup.Create(Self);
  Result.Caption:=aCaption;
  InternalAdd(ParentGroup,Result);
end;

function TGroupedMatrix.AddValue(ParentGroup: TGroupedMatrixGroup; ModesAsText,
  aType, AValue: string): TGroupedMatrixValue;
begin
  Result:=TGroupedMatrixValue.Create(Self);
  Result.Typ:=aType;
  Result.Value:=AValue;
  Result.Modes.Text:=ModesAsText;
  InternalAdd(ParentGroup,Result);
end;

function TGroupedMatrix.IndexOfTopLvlGroup(aCaption: TCaption): integer;
var
  i: Integer;
  Row: TGroupedMatrixRow;
begin
  Result:=0;
  for i:=0 to TopLvlCount-1 do begin
    Row:=TopLvlItems[i];
    if (Row is TGroupedMatrixGroup)
    and (TGroupedMatrixGroup(Row).Caption=aCaption) then
      exit(i);
  end;
end;

function TGroupedMatrix.GetTopLvlGroup(aCaption: TCaption): TGroupedMatrixGroup;
var
  i: Integer;
begin
  i:=IndexOfTopLvlGroup(aCaption);
  if i>=0 then
    Result:=TGroupedMatrixGroup(TopLvlItems[i])
  else
    Result:=nil;
end;

{ TGroupedMatrixValue }

procedure TGroupedMatrixValue.SetModes(AValue: TStrings);
begin
  if FModes=AValue then Exit;
  FModes.Assign(AValue);
end;

procedure TGroupedMatrixValue.SetTyp(AValue: string);
begin
  if FTyp=AValue then Exit;
  FTyp:=AValue;
end;

procedure TGroupedMatrixValue.SetValue(AValue: string);
begin
  if FValue=AValue then Exit;
  FValue:=AValue;
end;

procedure TGroupedMatrixValue.Assign(Source: TPersistent);
var
  aSource: TGroupedMatrixValue;
begin
  inherited Assign(Source);
  if Source is TGroupedMatrixValue then
  begin
    aSource:=TGroupedMatrixValue(Source);
    Value:=aSource.Value;
    Typ:=aSource.Typ;
    Modes:=aSource.Modes;
  end;
end;

constructor TGroupedMatrixValue.Create(aControl: TGroupedMatrix);
begin
  inherited Create(aControl);
  FModes:=TStringList.Create;
end;

destructor TGroupedMatrixValue.Destroy;
begin
  FreeAndNil(FModes);
  inherited Destroy;
end;

function TGroupedMatrixValue.Equals(Obj: TObject): boolean;
var
  SrcValue: TGroupedMatrixValue;
  i: Integer;
begin
  Result:=false;
  if not (Obj is TGroupedMatrixValue) then exit;
  if not (inherited Equals(Obj)) then exit;
  SrcValue:=TGroupedMatrixValue(Obj);
  if SrcValue.Modes.Count<>Modes.Count then exit;
  if SrcValue.Typ<>Typ then exit;
  if SrcValue.Value<>Value then exit;
  for i:=0 to Modes.Count-1 do
    if SrcValue.Modes[i]<>Modes[i] then exit;
  Result:=true;
end;

function TGroupedMatrixValue.AsString: string;
begin
  Result:=inherited AsString+Typ+':'+Value;
end;

{ TGroupedMatrixGroup }

function TGroupedMatrixGroup.GetCount: integer;
begin
  Result:=FItems.Count;
end;

function TGroupedMatrixGroup.GetItems(Index: integer): TGroupedMatrixRow;
begin
  Result:=TGroupedMatrixRow(FItems[Index]);
end;

procedure TGroupedMatrixGroup.SetCaption(AValue: TCaption);
begin
  if FCaption=AValue then Exit;
  FCaption:=AValue;
end;

procedure TGroupedMatrixGroup.SetColor(AValue: TColor);
begin
  if FColor=AValue then Exit;
  FColor:=AValue;
end;

procedure TGroupedMatrixGroup.Assign(Source: TPersistent);
var
  SrcGroup: TGroupedMatrixGroup;
  i: Integer;
  SrcItem: TGroupedMatrixRow;
  Item: TGroupedMatrixRow;
begin
  inherited Assign(Source);
  if Source is TGroupedMatrixGroup then
  begin
    SrcGroup:=TGroupedMatrixGroup(Source);
    FColor:=SrcGroup.FColor;
    FCaption:=SrcGroup.FCaption;
    Clear;
    for i:=0 to SrcGroup.Count-1 do begin
      SrcItem:=SrcGroup[i];
      Item:=TGroupedMatrixRow(SrcItem.ClassType).Create(Matrix);
      FItems.Add(Item);
      Item.FGroup:=Self;
      Item.Assign(SrcItem);
    end;
  end;
end;

constructor TGroupedMatrixGroup.Create(aControl: TGroupedMatrix);
begin
  inherited Create(aControl);
  FItems:=TFPList.Create;
  FColor:=clDefault;
end;

destructor TGroupedMatrixGroup.Destroy;
begin
  Clear;
  FreeAndNil(FItems);
  inherited Destroy;
end;

procedure TGroupedMatrixGroup.Clear;
var
  i: Integer;
begin
  if FItems=nil then exit;
  for i:=Count-1 downto 0 do
    Items[i].Free;
  inherited Clear;
end;

function TGroupedMatrixGroup.Equals(Obj: TObject): boolean;
var
  SrcGroup: TGroupedMatrixGroup;
  i: Integer;
begin
  Result:=false;
  if not (Obj is TGroupedMatrixGroup) then
    exit;
  if not (inherited Equals(Obj)) then exit;
  SrcGroup:=TGroupedMatrixGroup(Obj);
  if SrcGroup.Count<>Count then exit;
  if SrcGroup.Color<>Color then exit;
  if SrcGroup.Caption<>Caption then exit;
  for i:=0 to Count-1 do
    if not SrcGroup[i].Equals(Items[i]) then exit;
  Result:=true;
end;

function TGroupedMatrixGroup.IndexOfRow(aRow: TGroupedMatrixRow): integer;
begin
  for Result:=0 to Count-1 do
    if aRow=Items[Result] then exit;
  Result:=-1;
end;

function TGroupedMatrixGroup.GetNext: TGroupedMatrixRow;
begin
  if Count>0 then
    Result:=Items[0]
  else
    Result:=GetNextSibling;
end;

function TGroupedMatrixGroup.GetLastLeaf: TGroupedMatrixRow;
var
  aGroup: TGroupedMatrixGroup;
begin
  if Count=0 then exit(nil);
  Result:=Items[Count-1];
  while (Result is TGroupedMatrixGroup) do begin
    aGroup:=TGroupedMatrixGroup(Result);
    if aGroup.Count=0 then exit;
    Result:=aGroup[aGroup.Count-1];
  end;
end;

function TGroupedMatrixGroup.GetEffectiveColor: TColor;
var
  aGroup: TGroupedMatrixGroup;
begin
  aGroup:=Self;
  while aGroup<>nil do begin
    Result:=aGroup.Color;
    if Result<>clDefault then exit;
    aGroup:=aGroup.Group;
  end;
  Result:=Matrix.Control.Color;
end;

function TGroupedMatrixGroup.AsString: string;
begin
  Result:=inherited AsString+Caption;
end;

{ TGroupedMatrixRow }

procedure TGroupedMatrixRow.SetGroup(AValue: TGroupedMatrixGroup);
begin
  if FGroup=AValue then Exit;
  if FGroup<>nil then
    FGroup.FItems.Remove(Self);
  FGroup:=AValue;
  if FGroup<>nil then
    FGroup.FItems.Add(Self);
end;

procedure TGroupedMatrixRow.Assign(Source: TPersistent);
var
  aSource: TGroupedMatrixRow;
begin
  if Source is TGroupedMatrixRow then
  begin
    aSource:=TGroupedMatrixRow(Source);
    FRowInGrid:=aSource.FRowInGrid;
  end else
    inherited Assign(Source);
end;

constructor TGroupedMatrixRow.Create(aMatrix: TGroupedMatrix);
begin
  fMatrix:=aMatrix;
end;

destructor TGroupedMatrixRow.Destroy;
begin
  Clear;
  if Group=nil then
    Matrix.FTopLvlRows.Remove(Self)
  else begin
    Group.FItems.Remove(Self);
    FGroup:=nil;
  end;
  inherited Destroy;
end;

procedure TGroupedMatrixRow.Clear;
begin

end;

function TGroupedMatrixRow.Equals(Obj: TObject): boolean;
var
  SrcRow: TGroupedMatrixRow;
begin
  Result:=false;
  if not (Obj is TGroupedMatrixRow) then exit;
  SrcRow:=TGroupedMatrixRow(Obj);
  if SrcRow.RowInGrid<>RowInGrid then exit;
  Result:=true;
end;

function TGroupedMatrixRow.GetGroupIndex: integer;
begin
  if Group=nil then
    Result:=Matrix.IndexOfTopLvlItem(Self)
  else
    Result:=Group.IndexOfRow(Self);
end;

function TGroupedMatrixRow.Level: integer;
var
  aGroup: TGroupedMatrixGroup;
begin
  Result:=0;
  aGroup:=Group;
  while aGroup<>nil do begin
    inc(Result);
    aGroup:=aGroup.Group;
  end;
end;

function TGroupedMatrixRow.GetNextSibling: TGroupedMatrixRow;
var
  i: Integer;
begin
  i:=GetGroupIndex+1;
  if Group<>nil then begin
    if i>=Group.Count then exit(nil);
    Result:=Group[i];
  end else begin
    if i>=Matrix.TopLvlCount then exit(nil);
    Result:=Matrix.TopLvlItems[i];
  end;
end;

function TGroupedMatrixRow.GetNext: TGroupedMatrixRow;
begin
  Result:=GetNextSibling;
end;

function TGroupedMatrixRow.GetNextSkipChildren: TGroupedMatrixRow;
var
  aRow: TGroupedMatrixRow;
begin
  Result:=Self;
  repeat
    aRow:=Result.GetNextSibling;
    if aRow<>nil then exit(aRow);
    Result:=Result.Group;
  until Result=nil;
  Result:=nil;
end;

function TGroupedMatrixRow.GetPreviousSibling: TGroupedMatrixRow;
var
  i: Integer;
begin
  i:=GetGroupIndex-1;
  if i<0 then exit(nil);
  if Group<>nil then
    Result:=Group[i]
  else
    Result:=Matrix.TopLvlItems[i];
end;

function TGroupedMatrixRow.GetPrevious: TGroupedMatrixRow;
var
  aRow: TGroupedMatrixRow;
begin
  Result:=GetPreviousSibling;
  if Result=nil then
    exit(Group);
  aRow:=Result.GetLastLeaf;
  if aRow<>nil then
    Result:=aRow;
end;

function TGroupedMatrixRow.GetLastLeaf: TGroupedMatrixRow;
begin
  Result:=nil;
end;

function TGroupedMatrixRow.GetTopLvlItem: TGroupedMatrixRow;
begin
  Result:=Self;
  while Result.Group<>nil do
    Result:=Result.Group;
end;

function TGroupedMatrixRow.AsString: string;
begin
  Result:=Space(Level*2);
end;

{ TGroupedMatrixControl }

procedure TGroupedMatrixControl.ToggleModeValue(ValueRow: TGroupedMatrixValue;
  aRow, aCol: integer);
var
  i: Integer;
begin
  i:=ValueRow.Modes.IndexOf(Modes[aCol-1].Caption);
  if i>=0 then
    ValueRow.Modes.Delete(i)
  else
    ValueRow.Modes.Add(Modes[aCol-1].Caption);
  InvalidateCell(aCol, aRow);
end;

procedure TGroupedMatrixControl.PopupTypes(aRow: integer);
var
  i: Integer;
  Item: TMenuItem;
  XY: TPoint;
begin
  if aRow=0 then exit;
  // create popup menu
  if fTypePopupMenu=nil then
    fTypePopupMenu:=TPopupMenu.Create(Self);
  // fill popup menu with types from pick list
  for i:=0 to TypeColumn.PickList.Count-1 do begin
    if i>=fTypePopupMenu.Items.Count then
      fTypePopupMenu.Items.Add(TMenuItem.Create(Self));
    Item:=fTypePopupMenu.Items[i];
    Item.Caption:=TypeColumn.PickList.Names[i]+': '+TypeColumn.PickList.ValueFromIndex[i];
    Item.OnClick:=@OnTypePopupMenuClick;
  end;
  // delete not needed items
  while fTypePopupMenu.Items.Count>TypeColumn.PickList.Count do
    fTypePopupMenu.Items[fTypePopupMenu.Items.Count-1].Free;

  XY:=Point(0,0);
  ColRowToOffset(true,true,TypeCol,XY.X,i);
  ColRowToOffset(false,true,aRow,i,XY.Y);
  XY:=ClientToScreen(XY);
  fTypePopupMenuRow:=aRow;
  fTypePopupMenu.PopUp(XY.X,XY.Y);
end;

procedure TGroupedMatrixControl.OnTypePopupMenuClick(Sender: TObject);
var
  Item: TMenuItem;
  ValueRow: TGroupedMatrixValue;
  NewType: String;
begin
  Item:=Sender as TMenuItem;
  if fTypePopupMenuRow>=Matrix.RowCount then exit;
  if Matrix.Rows[fTypePopupMenuRow-1] is TGroupedMatrixValue then begin
    ValueRow:=TGroupedMatrixValue(Matrix[fTypePopupMenuRow-1]);
    NewType:=TypeColumn.PickList.Names[Item.MenuIndex];
    if NewType=ValueRow.Typ then exit;
    ValueRow.Typ:=NewType;
    InvalidateCell(TypeCol,fTypePopupMenuRow);
  end;
end;

function TGroupedMatrixControl.GetModeColumns(Index: integer): TGridColumn;
begin
  if (Index<0) or (Index>=Modes.Count) then
    raise Exception.Create('Index out of bounds '+dbgs(Index));
  Result:=Columns[Index];
end;

function TGroupedMatrixControl.GetModes: TGroupedMatrixModes;
begin
  Result:=Matrix.Modes;
end;

procedure TGroupedMatrixControl.SetActiveModeColor(AValue: TColor);
begin
  if FActiveModeColor=AValue then Exit;
  FActiveModeColor:=AValue;
  Invalidate;
end;

procedure TGroupedMatrixControl.SetIndent(AValue: integer);
begin
  if FIndent=AValue then Exit;
  FIndent:=AValue;
end;

procedure TGroupedMatrixControl.SetMaxUndo(AValue: integer);
begin
  AValue:=Max(AValue,1);
  if FMaxUndo=AValue then Exit;
  FMaxUndo:=AValue;
  while fUndoItems.Count+fRedoItems.Count>FMaxUndo do begin
    if fRedoItems.Count>0 then begin
      fRedoItems.Delete(0);
    end else begin
      fUndoItems.Delete(0);
    end;
  end;
end;

procedure TGroupedMatrixControl.PrepareCanvas(aCol, aRow: Integer;
  aState: TGridDrawState);
begin
  inherited PrepareCanvas(aCol, aRow, aState);
end;

procedure TGroupedMatrixControl.DrawRow(aRow: Integer);
var
  aRect: TRect;
  ClipArea: TRect;
  MatRow: TGroupedMatrixRow;
  Lvl: Integer;
  gds: TGridDrawState;
  i: Integer;
  GroupRow: TGroupedMatrixGroup;
  x: Integer;
begin
  aRect:=Rect(0,0,0,0);
  // Upper and Lower bounds for this row
  ColRowToOffSet(False, True, aRow, aRect.Top, aRect.Bottom);
  // is this row within the ClipRect?
  ClipArea := Canvas.ClipRect;
  if (aRect.Top>=aRect.Bottom) or not VerticalIntersect(aRect, ClipArea) then
    exit;

  if aRow>0 then begin
    MatRow:=Matrix.Rows[aRow-1];
    if MatRow is TGroupedMatrixGroup then begin
      GroupRow:=TGroupedMatrixGroup(MatRow);
      Lvl:=MatRow.Level;
      gds := GetGridDrawState(0, ARow);
      PrepareCanvas(0,aRow,gds);
      i:=0;
      ColRowToOffset(true,True,0,aRect.Left,i);
      ColRowToOffset(true,True,ColCount-1,i,aRect.Right);
      x:=aRect.Left+Lvl*Indent;
      // background
      //Canvas.Brush.Color:=GroupRow.GetEffectiveColor;
      Canvas.GradientFill(Rect(x,aRect.Top-1,x+2*Indent,aRect.Bottom),GroupRow.GetEffectiveColor,Color,gdHorizontal);
      // draw group caption
      Canvas.TextRect(aRect,constCellPadding+x,aRect.Top,GroupRow.Caption);
      // draw focus rect
      if aRow=Row then
        DrawFocusRect(0,aRow,Rect(x,aRect.Top,aRect.Right,aRect.Bottom));
      // draw grid
      PrepareGridCanvas;
      Canvas.MoveTo(x-1,aRect.Top-1);
      Canvas.LineTo(aRect.Right-1,aRect.Top-1);
      Canvas.LineTo(aRect.Right-1,aRect.Bottom-1);
      if GroupRow.Count>0 then
        Canvas.LineTo(x+Indent-1,aRect.Bottom-1)
      else
        Canvas.LineTo(x-1,aRect.Bottom-1);
      DrawIndent(aRow,aRect);
      exit;
    end;
  end;
  inherited DrawRow(aRow);
  if aRow>0 then
    DrawIndent(aRow,aRect);
end;

function TGroupedMatrixControl.EditingAllowed(ACol: Integer): Boolean;
var
  MatRow: TGroupedMatrixRow;
begin
  Result:=false;
  if (Row=0) or (Matrix=nil) or (Row>Matrix.RowCount) then exit;
  MatRow:=Matrix[Row-1];
  if MatRow is TGroupedMatrixValue then begin
    if ACol<>ValueCol then exit;
    Result:=true;
  end;
end;

procedure TGroupedMatrixControl.DrawCellGrid(aCol, aRow: Integer; aRect: TRect;
  aState: TGridDrawState);
begin
  if (aCol=0) then begin
    PrepareGridCanvas;
    if aRow=0 then exit;
    if goVertLine in Options then begin
      // right border is left border of mode checkbox
      Canvas.MoveTo(aRect.Right - 1, aRect.Top);
      Canvas.LineTo(aRect.Right - 1, aRect.Bottom);
    end;
    exit;
  end;
  inherited DrawCellGrid(aCol, aRow, aRect, aState);
end;

procedure TGroupedMatrixControl.GetCheckBoxState(const aCol, aRow: Integer;
  var aState: TCheckboxState);
var
  MatRow: TGroupedMatrixRow;
begin
  if (aCol>=1) and (aCol<=Modes.Count)
  and (aRow>0) then begin
    MatRow:=Matrix.Rows[aRow-1];
    if MatRow is TGroupedMatrixValue then begin
      //debugln(['TGroupedMatrixControl.GetCheckBoxState ',aCol,' ',aRow,' "',Modes[aCol-1],'" ',TGroupedMatrixValue(MatRow).Modes.Text]);
      if TGroupedMatrixValue(MatRow).Modes.IndexOf(Modes[aCol-1].Caption)>=0 then begin
        aState:=cbChecked;
        //debugln(['TGroupedMatrixControl.GetCheckBoxState ',aCol,' ',aRow,' "',Modes[aCol-1],'" ',TGroupedMatrixValue(MatRow).Modes.Text]);
      end
      else
        aState:=cbUnchecked;
    end;
  end;
  inherited GetCheckBoxState(aCol, aRow, aState);
end;

procedure TGroupedMatrixControl.AutoLayout;
var
  TitleHeight: Integer;
  i: Integer;
  MatRow: TGroupedMatrixRow;
  ValueRow: TGroupedMatrixValue;
  W: Integer;
begin
  if (not HandleAllocated) or (Parent=nil) then exit;

  // title row height
  TitleHeight:=20;
  for i:=0 to Modes.Count-1 do
    TitleHeight:=Max(TitleHeight,Canvas.TextWidth(Modes[i].Caption));
  RowHeights[0]:=TitleHeight+2*constCellPadding;

  // tree column width
  ColWidths[0]:=Matrix.GetMaxLevel*Indent;

  // type width
  W:=Canvas.TextWidth(TypeColumn.Title.Caption);
  for i:=0 to TypeColumn.PickList.Count-1 do
    W:=Max(W,Canvas.TextWidth(TypeColumn.PickList.Names[i]));
  for i:=0 to Matrix.RowCount-1 do begin
    MatRow:=Matrix.Rows[i];
    if MatRow is TGroupedMatrixValue then begin
      ValueRow:=TGroupedMatrixValue(MatRow);
      W:=Max(W,Canvas.TextWidth(ValueRow.Typ));
    end;
  end;
  TypeColumn.Width:=W+2*constCellPadding;

  // value width
  W:=0;
  for i:=0 to Matrix.RowCount-1 do begin
    MatRow:=Matrix.Rows[i];
    if MatRow is TGroupedMatrixValue then
      W:=Max(W,Canvas.TextWidth(TGroupedMatrixValue(MatRow).Value));
  end;
  ValueColumn.MinSize:=W;
end;

procedure TGroupedMatrixControl.CreateWnd;
begin
  inherited CreateWnd;
  AutoLayout;
end;

procedure TGroupedMatrixControl.MouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  aCol: Longint;
  aRow: Longint;
  MatRow: TGroupedMatrixRow;
  ValueRow: TGroupedMatrixValue;
begin
  inherited MouseDown(Button, Shift, X, Y);
  if (csDesigning in componentState) or not MouseButtonAllowed(Button) then
    Exit;
  aCol:=0;
  aRow:=0;
  MouseToCell(X,Y,aCol,aRow);
  if (aRow=RowCount-1) and (Y>CellRect(aCol,aRow).Bottom) then exit;
  if aRow>0 then begin
    MatRow:=Matrix[aRow-1];
    if MatRow is TGroupedMatrixValue then begin
      ValueRow:=TGroupedMatrixValue(MatRow);
      if (aCol>=ModeColFirst) and (aCol<=ModeColLast) then begin
        if Shift*[ssCtrl,ssShift,ssLeft]=[ssLeft] then begin
          // toggle a matrix cell
          ToggleModeValue(ValueRow, aRow, aCol);
        end;
      end else if aCol=TypeCol then begin
        if Shift*[ssCtrl,ssShift,ssLeft]=[ssLeft] then begin
          PopupTypes(aRow);
        end;
      end else if aCol=ValueCol then begin
        SelectEditor;
        EditorShow(False);
      end;
    end;
  end;
end;

procedure TGroupedMatrixControl.PrepareGridCanvas;
begin
  Canvas.Pen.Style := GridLineStyle;
  Canvas.Pen.Color := GridLineColor;
  Canvas.Pen.Width := GridLineWidth;
end;

procedure TGroupedMatrixControl.DrawIndent(aRow: integer; aRect: TRect);
var
  Group: TGroupedMatrixGroup;
  x: Integer;
begin
  Group:=Matrix[aRow-1].Group;
  while Group<>nil do begin
    x:=Indent*Group.Level;
    Canvas.GradientFill(Rect(x,aRect.Top-2,x+Indent,aRect.Bottom),
      Group.GetEffectiveColor,Color,gdHorizontal);
    PrepareGridCanvas;
    inc(x,Indent-1);
    Canvas.Line(x,aRect.Top-1,x,aRect.Bottom);
    Group:=Group.Group;
    if Group=nil then break;
  end;
  if aRow=RowCount-1 then begin
    // last row, draw line
    PrepareGridCanvas;
    Canvas.Line(0,aRect.Bottom-1,Indent*Matrix[aRow-1].Level,aRect.Bottom-1);
  end;
end;

function TGroupedMatrixControl.GetEditText(aCol, aRow: Longint): string;
begin
  if (aCol=ValueCol) and (aRow>0) and (Matrix[aRow-1] is TGroupedMatrixValue)
  then begin
    Result:=TGroupedMatrixValue(Matrix[aRow-1]).Value;
    exit;
  end;
  Result:=inherited GetEditText(aCol, aRow);
end;

procedure TGroupedMatrixControl.SetEditText(ACol, ARow: Longint;
  const Value: string);
begin
  if (aCol=ValueCol) and (aRow>0) and (Matrix[aRow-1] is TGroupedMatrixValue)
  then begin
    TGroupedMatrixValue(Matrix[aRow-1]).Value:=Value;
    exit;
  end;
  inherited SetEditText(ACol, ARow, Value);
end;

function TGroupedMatrixControl.GetCells(ACol, ARow: Integer): string;
begin
  if (aCol=ValueCol) and (aRow>0) and (Matrix[aRow-1] is TGroupedMatrixValue)
  then begin
    Result:=TGroupedMatrixValue(Matrix[aRow-1]).Value;
    exit;
  end;
  Result:=inherited GetCells(ACol, ARow);
end;

constructor TGroupedMatrixControl.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FMatrix:=TGroupedMatrix.Create(Self);
  fUndoItems:=TObjectList.Create(true);
  fRedoItems:=TObjectList.Create(true);

  Options:=Options+[goEditing]; // ToDo: change property default
  FixedCols:=1; // ToDo: change property default
  FixedRows:=1; // ToDo: change property default
  RowCount:=1;  // ToDo: change property default
  TitleStyle:=tsNative; // ToDo: change property default
  AutoFillColumns:=true;
  FIndent:=DefaultGroupMatrixIndent;
  FActiveModeColor:=RGBToColor(220,255,220);

  // type column
  FTypeColumn:=Columns.Add;
  FTypeColumn.Title.Caption:='Type';
  FTypeColumn.SizePriority:=0;

  // value column
  FValueColumn:=Columns.Add;
  FValueColumn.Title.Caption:='Value';
  FValueColumn.SizePriority:=1;
end;

destructor TGroupedMatrixControl.Destroy;
begin
  Clear;
  FreeAndNil(fUndoItems);
  FreeAndNil(fRedoItems);
  FreeAndNil(FMatrix);
  inherited Destroy;
end;

procedure TGroupedMatrixControl.Clear;
begin
  fUndoItems.Clear;
  fRedoItems.Clear;
  inherited Clear;
end;

procedure TGroupedMatrixControl.DefaultDrawCell(aCol, aRow: Integer; var aRect: TRect;
  aState: TGridDrawState);

  procedure DrawActiveModeRow(ValueRow: TGroupedMatrixValue);
  begin
    if Modes.Active<0 then exit;
    if ValueRow.Modes.IndexOf(Modes[Modes.Active].Caption)<0 then exit;
    Canvas.GradientFill(Rect(aRect.Left,(aRect.Top+aRect.Bottom) div 2,aRect.Right,aRect.Bottom),
      Color,ActiveModeColor,gdVertical);
  end;

var
  ts: TTextStyle;
  MatRow: TGroupedMatrixRow;
  ChkState: TCheckBoxState;
  Column: TGridColumn;
  ValueRow: TGroupedMatrixValue;
  ModeColor: TColor;
  StateColor: TColor;
begin
  //DebugLn(['TModeMatrixControl.DefaultDrawCell ']);
  if aRow=0 then begin
    // titles
    if aCol=0 then begin
      // title of tree
      DrawThemedCell(aCol, aRow, aRect, aState);
      exit;
    end else if (aCol>=1) and (aCol<=Modes.Count) then begin
      // mode names
      DrawThemedCell(aCol, aRow, aRect, aState);
      ts:=DefaultTextStyle;
      Canvas.TextStyle:=ts;
      Canvas.Font.Orientation:=900;
      Canvas.TextOut(aRect.Left+1,aRect.Bottom-2,Modes[aCol-1].Caption);
      exit;
    end;
  end else if aCol=0 then begin
    // first column of a non group
    exit;
  end else begin
    Column:=Columns[aCol-1];
    MatRow:=Matrix.Rows[aRow-1];
    if MatRow is TGroupedMatrixValue then begin
      ValueRow:=TGroupedMatrixValue(MatRow);
      if aCol<=ModeColLast then begin
        ModeColor:=Modes[aCol-ModeColFirst].Color;
        if ModeColor=clDefault then ModeColor:=Color;
        StateColor:=Color;
        if Modes.Active=aCol-ModeColFirst then
          StateColor:=ActiveModeColor;
        if (ModeColor<>Color) or (StateColor<>Color) then begin
          Canvas.GradientFill(aRect,ModeColor,StateColor,gdHorizontal);
        end;
        ChkState:=cbUnchecked;
        GetCheckBoxState(aCol,aRow,ChkState);
        DrawGridCheckboxBitmaps(aCol,aRow,aRect,ChkState);
      end else if Column=TypeColumn then begin
        DrawActiveModeRow(ValueRow);
        DrawCellText(aCol,aRow,aRect,aState,ValueRow.Typ);
      end else if Column=ValueColumn then begin
        DrawActiveModeRow(ValueRow);
        DrawCellText(aCol,aRow,aRect,aState,ValueRow.Value);
      end;
      exit;
    end;
  end;
  inherited DefaultDrawCell(aCol, aRow, aRect, aState);
end;

procedure TGroupedMatrixControl.MatrixChanged;
var
  i: Integer;
  aCol: TGridColumn;
begin
  for i:=0 to Modes.Count-1 do begin
    aCol:=Columns[i];
    if aCol=TypeColumn then begin
      // insert column
      Columns.Insert(i);
      aCol:=Columns[i];
      aCol.SizePriority:=0;
    end;
    aCol.Title.Caption:=Modes[i].Caption;
    {$IFDEF LCLcarbon}
    aCol.Alignment:=taLeftJustify;
    {$ELSE}
    aCol.Alignment:=taCenter;
    {$ENDIF}
    aCol.Width:=24;
  end;
  // free unneeded columns
  while Columns[Modes.Count]<>TypeColumn do
    Columns[Modes.Count].Free;

  RowCount:=Matrix.RowCount+1;

  AutoLayout;
end;

function TGroupedMatrixControl.ModeColFirst: integer;
begin
  Result:=1;
end;

function TGroupedMatrixControl.ModeColLast: integer;
begin
  Result:=Modes.Count;
end;

function TGroupedMatrixControl.TypeCol: integer;
begin
  Result:=Modes.Count+1;
end;

function TGroupedMatrixControl.ValueCol: integer;
begin
  Result:=Modes.Count+2;
end;

function TGroupedMatrixControl.CanUndo: boolean;
begin
  Result:=fUndoItems.Count>0;
end;

function TGroupedMatrixControl.CanRedo: boolean;
begin
  Result:=fRedoItems.Count>0;
end;

procedure TGroupedMatrixControl.Undo;
var
  DoMatrix: TGroupedMatrix;
begin
  if not CanUndo then exit;
  DoMatrix:=TGroupedMatrix(fUndoItems[fUndoItems.Count-1]);
  fRedoItems.Add(DoMatrix);
  fUndoItems.OwnsObjects:=false;
  fUndoItems.Delete(fUndoItems.Count-1);
  fUndoItems.OwnsObjects:=true;
  Matrix.Assign(DoMatrix);
  MatrixChanged;
end;

procedure TGroupedMatrixControl.Redo;
var
  DoMatrix: TGroupedMatrix;
begin
  if not CanRedo then exit;
  DoMatrix:=TGroupedMatrix(fRedoItems[fRedoItems.Count-1]);
  fUndoItems.Add(DoMatrix);
  fRedoItems.OwnsObjects:=false;
  fRedoItems.Delete(fRedoItems.Count-1);
  fRedoItems.OwnsObjects:=true;
  Matrix.Assign(DoMatrix);
  MatrixChanged;
end;

procedure TGroupedMatrixControl.StoreUndo(EvenIfNothingChanged: boolean);
var
  DoMatrix: TGroupedMatrix;
begin
  if (not EvenIfNothingChanged)
  and (fUndoItems.Count>0)
  and TGroupedMatrix(fUndoItems[fUndoItems.Count-1]).Equals(Matrix) then
    exit;
  fRedoItems.Clear;
  DoMatrix:=TGroupedMatrix.Create(nil);
  DoMatrix.Assign(Matrix);
  fUndoItems.Add(DoMatrix);
  if fUndoItems.Count>MaxUndo then
    fUndoItems.Delete(0);
end;

end.

