{
 ***************************************************************************
 *                                                                         *
 *   This source is free software; you can redistribute it and/or modify   *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 *   This code is distributed in the hope that it will be useful, but      *
 *   WITHOUT ANY WARRANTY; without even the implied warranty of            *
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU     *
 *   General Public License for more details.                              *
 *                                                                         *
 *   A copy of the GNU General Public License is available on the World    *
 *   Wide Web at <http://www.gnu.org/copyleft/gpl.html>. You can also      *
 *   obtain it by writing to the Free Software Foundation,                 *
 *   Inc., 51 Franklin Street - Fifth Floor, Boston, MA 02110-1335, USA.   *
 *                                                                         *
 ***************************************************************************

}
unit ModeMatrixCtrl;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, math, types, contnrs,
  // LCL
  Controls, LCLType, LCLIntf, Grids, Graphics, StdCtrls, Menus,
  // LazUtils
  LazLoggerBase,
  // Codetools
  FileProcs,
  // IdeIntf
  IDEUtils,
  // IDE
  LazarusIDEStrConsts;

const
  DefaultModeMatrixMaxUndo = 100;
  DefaultModeMatrixIndent = 10;
  DefaultModeMatrixOptions = DefaultGridOptions+[goEditing]-[goRangeSelect];
type
  TGroupedMatrix = class;
  TGroupedMatrixGroup = class;
  TGroupedMatrixControl = class;

  { TGroupedMatrixRow }

  TGroupedMatrixRow = class(TPersistent)
  private
    FLastDrawValueX: integer;
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
    function GetGroupIndex: integer;
    function Level: integer;
    function GetNextSibling: TGroupedMatrixRow;
    function GetNext: TGroupedMatrixRow; virtual;
    function GetNextSkipChildren: TGroupedMatrixRow;
    function GetPreviousSibling: TGroupedMatrixRow;
    function GetPrevious: TGroupedMatrixRow; // the reverse of GetNext
    function GetLastLeaf: TGroupedMatrixRow; virtual; // get last child of last child of ...
    function GetTopLvlItem: TGroupedMatrixRow;
    function AsString: string; virtual;
    property LastDrawValueX: integer read FLastDrawValueX write FLastDrawValueX;
    property Matrix: TGroupedMatrix read FMatrix;
    property Group: TGroupedMatrixGroup read FGroup write SetGroup;
    property RowInGrid: integer read FRowInGrid; // in Grid, not in Group
  end;
  TGroupedMatrixRowClass = class of TGroupedMatrixRow;

  { TGroupedMatrixGroup }

  TGroupedMatrixGroup = class(TGroupedMatrixRow)
  private
    FCaption: TCaption;
    FColor: TColor;
    FItems: TFPList; // list of TGroupedMatrixRow
    FValue: string;
    FWritable: boolean;
    function GetCount: integer;
    function GetItems(Index: integer): TGroupedMatrixRow;
  public
    procedure Assign(Source: TPersistent); override;
    constructor Create(aMatrix: TGroupedMatrix); override;
    destructor Destroy; override;
    procedure Clear; override;
    function Equals(Obj: TObject): boolean; override;
    function IndexOfRow(aRow: TGroupedMatrixRow): integer;
    procedure Move(CurIndex, NewIndex: integer);
    function GetNext: TGroupedMatrixRow; override;
    function GetLastLeaf: TGroupedMatrixRow; override;
    function GetEffectiveColor: TColor;
    function AsString: string; override;
    property Caption: TCaption read FCaption write FCaption;
    property Color: TColor read FColor write FColor;
    property Items[Index: integer]: TGroupedMatrixRow read GetItems; default;
    property Value: string read FValue write FValue;
    property Writable: boolean read FWritable write FWritable;
    property Count: integer read GetCount;
  end;

  { TGroupedMatrixValue }

  TGroupedMatrixValue = class(TGroupedMatrixRow)
  private
    FModeList: TStrings;
    FTyp: string;
    FValue: string;
    procedure SetModes(AValue: TStrings);
    procedure SetTyp(AValue: string);
    procedure SetValue(AValue: string);
  public
    ID: string;
    procedure Assign(Source: TPersistent); override;
    constructor Create(aControl: TGroupedMatrix); override;
    destructor Destroy; override;
    function Equals(Obj: TObject): boolean; override;
    property ModeList: TStrings read FModeList write SetModes;
    function GetNormalizedModes(IgnoreModes: TStrings = nil): string;
    function AsString: string; override;
    property Typ: string read FTyp write SetTyp;
    property Value: string read FValue write SetValue;
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
  TGroupedMatrixModeClass = class of TGroupedMatrixMode;

  { TGroupedMatrixModes }

  TGroupedMatrixModes = class(TPersistent)
  private
    fItems: TFPList; // list of TGroupedMatrixMode
    function GetItems(Index: integer): TGroupedMatrixMode;
  public
    procedure Assign(Source: TPersistent); override;
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    function Equals(Obj: TObject): boolean; override;
    function Count: integer;
    function Add(aCaption: string; aColor: TColor = clDefault): TGroupedMatrixMode;
    procedure Delete(Index: integer);
    property Items[Index: integer]: TGroupedMatrixMode read GetItems; default;
  end;
  TGroupedMatrixModesClass = class of TGroupedMatrixModes;

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
  public
    procedure Assign(Source: TPersistent); override;
    constructor Create(aControl: TGroupedMatrixControl);
    destructor Destroy; override;
    procedure Clear;
    function Equals(Obj: TObject): boolean; override;
    procedure RebuildRows;
    procedure DeleteRow(Index: integer);
    function IndexOfRow(Row: TGroupedMatrixRow): integer;
    function IndexOfTopLvlItem(Row: TGroupedMatrixRow): integer;
    function IndexOfTopLvlGroup(aCaption: TCaption): integer;
    function GetTopLvlGroup(aCaption: TCaption): TGroupedMatrixGroup;
    function GetMaxLevel: integer;
    function AddGroup(ParentGroup: TGroupedMatrixGroup;
      aCaption: TCaption; aValue: string = ''): TGroupedMatrixGroup;
    function AddValue(ParentGroup: TGroupedMatrixGroup;
      ModesAsText, aType, AValue, aID: string): TGroupedMatrixValue;
    procedure WriteDebugReport;
    property RowCount: integer read GetRowCount;
    property Rows[Index: integer]: TGroupedMatrixRow read GetRows; default;
    property TopLvlCount: integer read GetTopLvlCount;
    property TopLvlItems[Index: integer]: TGroupedMatrixRow read GetTopLvlItems;
    property Modes: TGroupedMatrixModes read FModes;
    property Control: TGroupedMatrixControl read FControl;
  end;

  TOnGetCellHightlightColor = procedure(Sender: TObject; aCol,aRow: integer;
    var aColor: TColor) of object;

  { TGroupedMatrixControl }

  TGroupedMatrixControl = class(TCustomDrawGrid)
  private
    FActiveModeIndex: integer;
    FActiveModeColor: TColor;
    FIndent: integer;
    FMatrix: TGroupedMatrix;
    FMaxUndo: integer;
    FOnGetCellHightlightColor: TOnGetCellHightlightColor;
    FTypeColumn: TGridColumn;
    FValueColumn: TGridColumn;
    fTypePopupMenu: TPopupMenu;
    fTypePopupMenuRow: integer; // grid row of fTypePopupMenu
    fUndoItems: TObjectList; // list of TGroupedMatrix, 0=oldest
    fRedoItems: TObjectList; // list of TGroupedMatrix, 0=oldest
    function GetModeColumns(Index: integer): TGridColumn;
    function GetModes: TGroupedMatrixModes;
    procedure InvalidateGroupedCells({%H-}aCol, aRow: Integer);
    procedure SetActiveModeIndex(AValue: integer);
    procedure SetActiveModeColor(AValue: TColor);
    procedure SetIndent(AValue: integer);
    procedure SetMaxUndo(AValue: integer);
    procedure ToggleModeValue(aCol, aRow: integer);
    procedure PopupTypes(aRow: integer);
    procedure OnTypePopupMenuClick(Sender: TObject);
  protected
    function EditingAllowed(ACol: Integer=-1): Boolean; override;
    function GetCells(ACol, ARow: Integer): string; override;
    function GetEditText(aCol, aRow: Longint): string; override;
    procedure AutoLayout; virtual;
    procedure BeforeMoveSelection(const DCol, DRow: Integer); override;
    procedure CreateWnd; override;
    procedure DoEditorShow; override;
    procedure DrawCellGrid(aCol, aRow: Integer; aRect: TRect;
      aState: TGridDrawState); override;
    procedure DrawIndent(aRow: integer; aRect: TRect);
    procedure DrawRow(aRow: Integer); override;
    procedure GetCheckBoxState(const aCol, aRow: Integer;
      var aState: TCheckboxState); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer
      ); override;
    procedure MoveSelection; override;
    procedure PrepareGridCanvas; // prepare canvas for drawing the lines of the grid
    procedure SetCheckboxState(const aCol, aRow: Integer;
      const aState: TCheckboxState); override;
    procedure SetEditText(ACol, ARow: Longint; const Value: string); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Clear;
    procedure DefaultDrawCell(aCol, aRow: Integer; var aRect: TRect;
      aState: TGridDrawState); override;
    procedure MatrixChanging;
    procedure MatrixChanged;
    procedure DeleteMatrixRow(aRow: integer);
    procedure RenameMode(aOldName, aNewName: string);
    function ModeColFirst: integer;
    function ModeColLast: integer;
    function TypeCol: integer;
    function ValueCol: integer;
    // undo/redo
    function CanUndo: boolean;
    function CanRedo: boolean;
    procedure Undo;
    procedure Redo;
    procedure StoreUndo(EvenIfNothingChanged: boolean = false);
  public
    property ActiveModeColor: TColor read FActiveModeColor write SetActiveModeColor;
    property Matrix: TGroupedMatrix read FMatrix;
    property Modes: TGroupedMatrixModes read GetModes;
    property ActiveModeIndex: integer read FActiveModeIndex write SetActiveModeIndex;
    property ModeColumns[Index: integer]: TGridColumn read GetModeColumns;
    property TypeColumn: TGridColumn read FTypeColumn;
    property ValueColumn: TGridColumn read FValueColumn;
    property Indent: integer read FIndent write SetIndent default DefaultModeMatrixIndent;
    property MaxUndo: integer read FMaxUndo write SetMaxUndo default DefaultModeMatrixMaxUndo;
    property Options default DefaultModeMatrixOptions;
    property TitleStyle default tsNative;
    property OnGetCellHightlightColor: TOnGetCellHightlightColor
                 read FOnGetCellHightlightColor write FOnGetCellHightlightColor;
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
  if Source=Self then exit;
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
  if Source=Self then exit;
  if Source is TGroupedMatrixModes then
  begin
    SrcModes:=TGroupedMatrixModes(Source);
    Clear;
    for i:=0 to SrcModes.Count-1 do begin
      SrcMode:=SrcModes[i];
      NewMode:=TGroupedMatrixModeClass(SrcMode.ClassType).Create;
      fItems.Add(NewMode);
      NewMode.Assign(SrcMode);
    end;
  end else
    inherited Assign(Source);
end;

constructor TGroupedMatrixModes.Create;
begin
  fItems:=TFPList.Create;
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
  for i:=0 to Count-1 do
    if not SrcModes[i].Equals(Items[i]) then exit;
  Result:=true;
end;

function TGroupedMatrixModes.Count: integer;
begin
  Result:=fItems.Count;
end;

function TGroupedMatrixModes.Add(aCaption: string; aColor: TColor): TGroupedMatrixMode;
begin
  Result:=TGroupedMatrixMode.Create;
  Result.Caption:=aCaption;
  Result.Color:=aColor;
  fItems.Add(Result);
end;

procedure TGroupedMatrixModes.Delete(Index: integer);
var
  Item: TGroupedMatrixMode;
begin
  Item:=Items[Index];
  fItems.Delete(Index);
  Item.Free;
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
  if Source=Self then exit;
  if Source is TGroupedMatrix then
  begin
    SrcMatrix:=TGroupedMatrix(Source);
    Clear;
    Modes.Assign(SrcMatrix.Modes);
    for i:=0 to SrcMatrix.TopLvlCount-1 do begin
      SrcRow:=SrcMatrix.TopLvlItems[i];
      NewRow:=TGroupedMatrixRowClass(SrcRow.ClassType).Create(Self);
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
  FTopLvlRows.Clear;
  FRows.Clear;
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

procedure TGroupedMatrix.DeleteRow(Index: integer);
begin
  Rows[Index].Free;
  RebuildRows;
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
  aCaption: TCaption; aValue: string): TGroupedMatrixGroup;
begin
  Result:=TGroupedMatrixGroup.Create(Self);
  Result.Caption:=aCaption;
  Result.Value:=aValue;
  Result.Writable:=aValue<>'';
  InternalAdd(ParentGroup,Result);
end;

function TGroupedMatrix.AddValue(ParentGroup: TGroupedMatrixGroup; ModesAsText,
  aType, AValue, aID: string): TGroupedMatrixValue;
begin
  Result:=TGroupedMatrixValue.Create(Self);
  Result.Typ:=aType;
  Result.Value:=AValue;
  Result.ModeList.Text:=ModesAsText;
  Result.ID:=aID;
  InternalAdd(ParentGroup,Result);
end;

procedure TGroupedMatrix.WriteDebugReport;
var
  i: Integer;
  MatRow: TGroupedMatrixRow;
begin
  for i:=0 to RowCount-1 do begin
    MatRow:=Rows[i];
    debugln(Space(MatRow.Level*2),MatRow.AsString);
  end;
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
  if FModeList=AValue then Exit;
  FModeList.Assign(AValue);
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
  if Source=Self then exit;
  inherited Assign(Source);
  if Source is TGroupedMatrixValue then
  begin
    aSource:=TGroupedMatrixValue(Source);
    Value:=aSource.Value;
    Typ:=aSource.Typ;
    ModeList:=aSource.ModeList;
  end;
end;

constructor TGroupedMatrixValue.Create(aControl: TGroupedMatrix);
begin
  inherited Create(aControl);
  FModeList:=TStringList.Create;
end;

destructor TGroupedMatrixValue.Destroy;
begin
  FreeAndNil(FModeList);
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
  if SrcValue.ModeList.Count<>ModeList.Count then exit;
  if SrcValue.Typ<>Typ then exit;
  if SrcValue.Value<>Value then exit;
  for i:=0 to ModeList.Count-1 do
    if SrcValue.ModeList[i]<>ModeList[i] then exit;
  Result:=true;
end;

function TGroupedMatrixValue.GetNormalizedModes(IgnoreModes: TStrings): string;
var
  i: Integer;
  m: String;
begin
  Result:='';
  for i:=0 to ModeList.Count-1 do begin
    m:=ModeList[i];
    if m='' then continue;
    if (IgnoreModes<>nil)
    and (IndexInStringList(IgnoreModes,cstCaseInsensitive,m)>=0) then
      continue;
    if Result<>'' then Result+=#10;
    Result+=m;
  end;
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

procedure TGroupedMatrixGroup.Assign(Source: TPersistent);
var
  SrcGroup: TGroupedMatrixGroup;
  i: Integer;
  SrcItem: TGroupedMatrixRow;
  Item: TGroupedMatrixRow;
begin
  if Source=Self then exit;
  inherited Assign(Source);
  if Source is TGroupedMatrixGroup then
  begin
    SrcGroup:=TGroupedMatrixGroup(Source);
    FColor:=SrcGroup.FColor;
    FCaption:=SrcGroup.FCaption;
    FValue:=SrcGroup.FValue;
    FWritable:=SrcGroup.FWritable;
    Clear;
    for i:=0 to SrcGroup.Count-1 do begin
      SrcItem:=SrcGroup[i];
      Item:=TGroupedMatrixRowClass(SrcItem.ClassType).Create(Matrix);
      FItems.Add(Item);
      Item.FGroup:=Self;
      Item.Assign(SrcItem);
    end;
  end;
end;

constructor TGroupedMatrixGroup.Create(aMatrix: TGroupedMatrix);
begin
  inherited Create(aMatrix);
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
  if SrcGroup.Value<>Value then exit;
  if SrcGroup.Writable<>Writable then exit;
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

procedure TGroupedMatrixGroup.Move(CurIndex, NewIndex: integer);
begin
  FItems.Move(CurIndex,NewIndex);
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
  if Source=Self then exit;
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

procedure TGroupedMatrixControl.ToggleModeValue(aCol, aRow: integer);
var
  aState: TCheckboxState;
begin
  aState:=cbUnchecked;
  GetCheckBoxState(aCol,aRow,aState);
  if aState=cbUnchecked then
    aState:=cbChecked
  else
    aState:=cbUnchecked;
  SetCheckboxState(aCol,aRow,aState);
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
  i:=TypeColumn.PickList.Count-1;
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
  if (fTypePopupMenuRow<=0) or (fTypePopupMenuRow>Matrix.RowCount) then exit;
  if Matrix.Rows[fTypePopupMenuRow-1] is TGroupedMatrixValue then begin
    ValueRow:=TGroupedMatrixValue(Matrix[fTypePopupMenuRow-1]);
    NewType:=TypeColumn.PickList.Names[Item.MenuIndex];
    if NewType=ValueRow.Typ then exit;
    StoreUndo;
    if Assigned(OnSetEditText) then
      OnSetEditText(Sender,TypeCol,fTypePopupMenuRow,NewType);
    ValueRow.Typ:=NewType;
    InvalidateCell(TypeCol,fTypePopupMenuRow);
    EditingDone;
  end;
end;

procedure TGroupedMatrixControl.BeforeMoveSelection(const DCol, DRow: Integer);
begin
  // invalidate old cells
  InvalidateGroupedCells(Col,Row);
  inherited BeforeMoveSelection(DCol, DRow);
end;

procedure TGroupedMatrixControl.MoveSelection;
begin
  // invalidate new cells
  InvalidateGroupedCells(Col,Row);
  inherited MoveSelection;
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

procedure TGroupedMatrixControl.InvalidateGroupedCells(aCol, aRow: Integer);
var
  MatRow: TGroupedMatrixRow;
begin
  if Matrix=nil then exit;
  if (aRow>=FixedRows) and (aRow<=Matrix.RowCount) then begin
    MatRow:=Matrix[aRow-FixedRows];
    if MatRow is TGroupedMatrixGroup then
      InvalidateRow(aRow);
  end;
end;

procedure TGroupedMatrixControl.SetActiveModeIndex(AValue: integer);
begin
  if FActiveModeIndex=AValue then Exit;
  FActiveModeIndex:=AValue;
  Invalidate;
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
  s: String;
  h: Integer;
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
      Canvas.FillRect(x+2*Indent,aRect.Top-1,aRect.Right,aRect.Bottom);
      // draw group caption
      s:=GroupRow.Caption;
      if (aRow<>Row) or (not EditorMode) then
        s+=GroupRow.Value;
      h:=Canvas.TextHeight(s);
      Canvas.TextRect(aRect,constCellPadding+x,(aRect.Top+aRect.Bottom-h) div 2,s);
      GroupRow.LastDrawValueX:=constCellPadding+x+Canvas.TextWidth(GroupRow.Caption);
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
  end else if MatRow is TGroupedMatrixGroup then begin
    if ACol<>ValueCol then exit;
    Result:=TGroupedMatrixGroup(MatRow).Writable;
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
  ValueRow: TGroupedMatrixValue;
  ModeName: String;
begin
  if (aCol>=1) and (aCol<=Modes.Count) and (aRow>0) then
  begin
    MatRow:=Matrix.Rows[aRow-1];
    if MatRow is TGroupedMatrixValue then
    begin
      ValueRow:=TGroupedMatrixValue(MatRow);
      ModeName:=Modes[aCol-1].Caption;
      if IndexInStringList(ValueRow.ModeList,cstCaseInsensitive,ModeName)>=0 then
        aState:=cbChecked
      else
        aState:=cbUnchecked;
    end;
  end;
  inherited GetCheckBoxState(aCol, aRow, aState);
end;

procedure TGroupedMatrixControl.SetCheckboxState(const aCol, aRow: Integer;
  const aState: TCheckboxState);
var
  MatRow: TGroupedMatrixRow;
  ValueRow: TGroupedMatrixValue;
  ModeName: String;
  i: Integer;
begin
  if (aCol>=1) and (aCol<=Modes.Count) and (aRow>0) then
  begin
    MatRow:=Matrix.Rows[aRow-1];
    if MatRow is TGroupedMatrixValue then begin
      ValueRow:=TGroupedMatrixValue(MatRow);
      if assigned(OnSetCheckboxState) then
        OnSetCheckboxState(Self, aCol, aRow, aState);
      ModeName:=Modes[aCol-1].Caption;
      i:=IndexInStringList(ValueRow.ModeList,cstCaseInsensitive,ModeName);
      if (i<0) = (aState=cbUnchecked) then exit;
      StoreUndo;
      if i>=0 then
        ValueRow.ModeList.Delete(i)
      else
        ValueRow.ModeList.Add(ModeName);
      InvalidateRow(aRow);
      EditingDone;
      exit;
    end;
  end;
  inherited SetCheckboxState(aCol, aRow, aState);
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
  GroupRow: TGroupedMatrixGroup;
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
      if (aCol>=ModeColFirst) and (aCol<=ModeColLast) then begin
        if Shift*[ssCtrl,ssShift,ssLeft]=[ssLeft] then begin
          ToggleModeValue(aCol, aRow);
        end;
      end else if aCol=TypeCol then begin
        if Shift*[ssCtrl,ssShift,ssLeft]=[ssLeft] then begin
          PopupTypes(aRow);
        end;
      end else if aCol=ValueCol then begin
        SelectEditor;
        EditorShow(False);
      end;
    end else if MatRow is TGroupedMatrixGroup then begin
      GroupRow:=TGroupedMatrixGroup(MatRow);
      if GroupRow.Writable and (X>GroupRow.LastDrawValueX) then begin
        Col:=ValueCol;
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
    Canvas.GradientFill(Rect(x,aRect.Top-1,x+Indent,aRect.Bottom),
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
var
  MatRow: TGroupedMatrixRow;
begin
  if (aCol=ValueCol) and (aRow>=FixedRows) then begin
    MatRow:=Matrix[aRow-FixedRows];
    if MatRow is TGroupedMatrixValue then
      Result:=TGroupedMatrixValue(MatRow).Value
    else
      Result:=TGroupedMatrixGroup(MatRow).Value;
    exit;
  end;
  Result:=inherited GetEditText(aCol, aRow);
end;

procedure TGroupedMatrixControl.SetEditText(ACol, ARow: Longint;
  const Value: string);
var
  ValueRow: TGroupedMatrixValue;
  MatRow: TGroupedMatrixRow;
  GroupRow: TGroupedMatrixGroup;
begin
  if (aCol=ValueCol) and (aRow>0) then begin
    MatRow:=Matrix[aRow-FixedRows];
    if MatRow is TGroupedMatrixValue then begin
      ValueRow:=TGroupedMatrixValue(MatRow);
      if ValueRow.Value=Value then exit;
      StoreUndo;
      ValueRow.Value:=Value;
    end else begin
      GroupRow:=TGroupedMatrixGroup(MatRow);
      if GroupRow.Value=Value then exit;
      StoreUndo;
      GroupRow.Value:=Value;
      InvalidateRow(ARow);
    end;
  end;
  inherited SetEditText(ACol, ARow, Value);
end;

procedure TGroupedMatrixControl.DoEditorShow;
begin
  inherited DoEditorShow;
  InvalidateGroupedCells(Col,Row);
end;

function TGroupedMatrixControl.GetCells(ACol, ARow: Integer): string;
var
  MatRow: TGroupedMatrixRow;
begin
  if (aCol=ValueCol) and (aRow>0) then begin
    MatRow:=Matrix[ARow-FixedRows];
    if MatRow is TGroupedMatrixValue then
      Result:=TGroupedMatrixValue(MatRow).Value
    else
      Result:=TGroupedMatrixGroup(MatRow).Value;
    exit;
  end;
  Result:=inherited GetCells(ACol, ARow);
end;

constructor TGroupedMatrixControl.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  MouseWheelOption := mwGrid;

  FMatrix:=TGroupedMatrix.Create(Self);
  fUndoItems:=TObjectList.Create(true);
  fRedoItems:=TObjectList.Create(true);
  FMaxUndo:=DefaultModeMatrixMaxUndo;

  Options:=DefaultModeMatrixOptions;
  RowCount:=1;
  TitleStyle:=tsNative;
  AutoFillColumns:=true;
  FIndent:=DefaultModeMatrixIndent;
  FActiveModeColor:=RGBToColor(220,255,220);

  // type column
  FTypeColumn:=Columns.Add;
  FTypeColumn.Title.Caption:=dlgEnvType;
  FTypeColumn.SizePriority:=0;

  // value column
  FValueColumn:=Columns.Add;
  FValueColumn.Title.Caption:=lisValue;
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
  var
    ModeName: String;
  begin
    if ActiveModeIndex<0 then exit;
    ModeName:=Modes[ActiveModeIndex].Caption;
    if IndexInStringList(ValueRow.ModeList,cstCaseInsensitive,ModeName)<0 then
      exit;
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
  aHighlightColor: TColor;
begin
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
      ts.Opaque:=false;
      Canvas.Brush.Style:=bsClear;
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
      Canvas.FillRect(aRect);
      ValueRow:=TGroupedMatrixValue(MatRow);
      if aCol<=ModeColLast then begin
        ModeColor:=Modes[aCol-ModeColFirst].Color;
        if ModeColor=clDefault then ModeColor:=Color;
        StateColor:=Color;
        if ActiveModeIndex=aCol-ModeColFirst then
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
        if Assigned(OnGetCellHightlightColor) then begin
          aHighlightColor:=clDefault;
          OnGetCellHightlightColor(Self,aCol,aRow,aHighlightColor);
          if aHighlightColor<>clDefault then
            Canvas.GradientFill(
              Rect(aRect.Left,aRect.Top,aRect.Right,(aRect.Top+aRect.Bottom) div 2),
              aHighlightColor,Color,gdVertical);
        end;
        DrawActiveModeRow(ValueRow);
        DrawCellText(aCol,aRow,aRect,aState,ValueRow.Value);
      end;
      exit;
    end;
  end;
  inherited DefaultDrawCell(aCol, aRow, aRect, aState);
end;

procedure TGroupedMatrixControl.MatrixChanging;
begin
  EditorHide;
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
  Invalidate;
end;

procedure TGroupedMatrixControl.DeleteMatrixRow(aRow: integer);
begin
  if (aRow<FixedRows) or (aRow>=RowCount) then exit;
  MatrixChanging;
  try
    StoreUndo;
    Matrix.DeleteRow(aRow-1);
  finally
    MatrixChanged;
  end;
end;

procedure TGroupedMatrixControl.RenameMode(aOldName, aNewName: string);
var
  iRow, iMod: Integer;
  MatRow: TGroupedMatrixRow;
  ValRow: TGroupedMatrixValue;
begin
  for iRow:=0 to Matrix.RowCount-1 do
  begin
    MatRow:=Matrix.Rows[iRow];
    if MatRow is TGroupedMatrixValue then
    begin
      ValRow:=TGroupedMatrixValue(MatRow);
      iMod:=IndexInStringList(ValRow.ModeList,cstCaseInsensitive,aOldName);
      if iMod>=0 then
        ValRow.ModeList[iMod]:=aNewName;
    end;
  end;
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
  MatrixChanging;
  try
    DoMatrix:=TGroupedMatrix.Create(nil);
    DoMatrix.Assign(Matrix);
    fRedoItems.Add(DoMatrix);
    if MaxUndo<fRedoItems.Count then
      fRedoItems.Delete(0);
    DoMatrix:=TGroupedMatrix(fUndoItems[fUndoItems.Count-1]);
    fUndoItems.OwnsObjects:=false;
    fUndoItems.Delete(fUndoItems.Count-1);
    fUndoItems.OwnsObjects:=true;
    Matrix.Assign(DoMatrix);
  finally
    MatrixChanged;
  end;
end;

procedure TGroupedMatrixControl.Redo;
var
  DoMatrix: TGroupedMatrix;
begin
  if not CanRedo then exit;
  MatrixChanging;
  try
    DoMatrix:=TGroupedMatrix.Create(nil);
    DoMatrix.Assign(Matrix);
    fUndoItems.Add(DoMatrix);
    if MaxUndo<fUndoItems.Count then
      fUndoItems.Delete(0);
    DoMatrix:=TGroupedMatrix(fRedoItems[fRedoItems.Count-1]);
    fRedoItems.OwnsObjects:=false;
    fRedoItems.Delete(fRedoItems.Count-1);
    fRedoItems.OwnsObjects:=true;
    Matrix.Assign(DoMatrix);
  finally
    MatrixChanged;
  end;
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

