{-------------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
the specific language governing rights and limitations under the License.

Alternatively, the contents of this file may be used under the terms of the
GNU General Public License Version 2 or later (the "GPL"), in which case
the provisions of the GPL are applicable instead of those above.
If you wish to allow use of your version of this file only under the terms
of the GPL and not to allow others to use your version of this file
under the MPL, indicate your decision by deleting the provisions above and
replace them with the notice and other provisions required by the GPL.
If you do not delete the provisions above, a recipient may use your version
of this file under either the MPL or the GPL.

-------------------------------------------------------------------------------}
unit SynPluginSyncronizedEditBase;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, math, Graphics,
  SynEditMiscClasses, SynEdit, SynEditMarkup, SynEditMiscProcs, SynEditTextBase,
  SynEditTextTrimmer;

type

  { TSynPluginSyncronizedEditCell }

  TSynPluginSyncronizedEditCell = class
  private
    FLogStart, FLogEnd: TPoint;
    FGroup: Integer;
  public
    procedure Assign(Src: TSynPluginSyncronizedEditCell); reintroduce;

    property LogStart: TPoint read FLogStart write FLogStart;
    property LogEnd: TPoint read FLogEnd write FLogEnd;
    property Group: Integer read FGroup write FGroup;
  end;

  TSynPluginSyncronizedEditCellChangedEvent = procedure(aIndex: Integer;
                            aOldVal, aNewVal: TSynPluginSyncronizedEditCell) of object;

  { TSynPluginSyncronizedEditList }

  TSynPluginSyncronizedEditList = class
  private
    FCells: Array of TSynPluginSyncronizedEditCell;
    FOnCellChange: TSynPluginSyncronizedEditCellChangedEvent;
    function GetCell(aIndex: Integer): TSynPluginSyncronizedEditCell;
    function GetGroupCell(aGroup, aIndex: Integer): TSynPluginSyncronizedEditCell;
    procedure SetCell(aIndex: Integer; const AValue: TSynPluginSyncronizedEditCell);
  protected
    property OnCellChange: TSynPluginSyncronizedEditCellChangedEvent // For Markup
             read FOnCellChange write FOnCellChange;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    function Add(aCell: TSynPluginSyncronizedEditCell): Integer;
    function AddNew: TSynPluginSyncronizedEditCell; virtual;
    procedure Delete(aIndex: Integer);
    function IndexOf(aCell: TSynPluginSyncronizedEditCell): Integer;
    function IndexOf(aX, aY: Integer; IncludeLast: Boolean = False): Integer;
    function Count: Integer;
    function GroupCount(aGroup: Integer): Integer;
    property Cell[aIndex: Integer]: TSynPluginSyncronizedEditCell
      read GetCell write SetCell; default;
    property GroupCell[aGroup, aIndex: Integer]: TSynPluginSyncronizedEditCell
      read GetGroupCell;
  end;

  { TSynPluginSyncronizedEditMarkupBase }

  TSynPluginSyncronizedEditMarkupBase = class(TSynEditMarkup)
  private
    FCells: TSynPluginSyncronizedEditList;
    procedure SetCells(const AValue: TSynPluginSyncronizedEditList);
  protected
    procedure CellChanged(aIndex: Integer; aOldVal, aNewVal: TSynPluginSyncronizedEditCell); virtual; abstract;
    function OwnedByMgr: Boolean; override;
    procedure DoEnabledChanged(Sender: TObject); override;
    property Cells: TSynPluginSyncronizedEditList read FCells write SetCells;
  public
    constructor Create(ASynEdit: TSynEditBase);
    destructor Destroy; override;
  end;

  { TSynPluginSyncronizedEditMarkup }

  TSynPluginSyncronizedEditMarkup = class(TSynPluginSyncronizedEditMarkupBase)
  private
    FCurrentCell: Integer;
    fMarkupInfoCurrent: TSynSelectedColor;
    fMarkupInfoSync: TSynSelectedColor;
    FPreparedRow: Integer;
    FPreparedCellFrom, FPreparedCellTo: Integer;
    FPreparedCellTop, FPreparedCellBottom: Integer;
    procedure SetCurrentCell(const AValue: Integer);
  protected
    procedure CellChanged(aIndex: Integer; aOldVal, aNewVal: TSynPluginSyncronizedEditCell); override;
    property CurrentCell: Integer read FCurrentCell write SetCurrentCell;
  public
    constructor Create(ASynEdit: TSynEditBase);
    destructor Destroy; override;
    function GetMarkupAttributeAtRowCol(const aRow, aCol: Integer): TSynSelectedColor; override;
    function GetNextMarkupColAfterRowCol(const aRow, aCol: Integer): Integer; override;
    Procedure PrepareMarkupForRow(aRow : Integer); override;
    Procedure EndMarkup; override;

    property MarkupInfoCurrent: TSynSelectedColor read fMarkupInfoCurrent;
    property MarkupInfoSync: TSynSelectedColor read fMarkupInfoSync;
  end;

  { TSynPluginSyncronizedEditMarkupArea }

  TSynPluginSyncronizedEditMarkupArea = class(TSynPluginSyncronizedEditMarkupBase)
  private
    FCellIdForArea: Integer;
  protected
    procedure CellChanged(aIndex: Integer; aOldVal, aNewVal: TSynPluginSyncronizedEditCell); override;
  public
    function GetMarkupAttributeAtRowCol(const aRow, aCol: Integer): TSynSelectedColor; override;
    function GetNextMarkupColAfterRowCol(const aRow, aCol: Integer): Integer; override;

    property CellGroupForArea: Integer read FCellIdForArea write FCellIdForArea;
  end;

  { TSynPluginSyncronizedEditBase }

  TSynPluginSyncronizedEditBase = class(TSynEditPlugin)
  private
    FActive: Boolean;
    FCells: TSynPluginSyncronizedEditList;
    FCurrentCell: Integer;
    FAreaMarkupEnabled: Boolean;
    FMarkupEnabled: Boolean;
    FEnabled: Boolean;
    FEditing: Boolean;

    fMarkupInfo: TSynSelectedColor;
    fMarkupInfoSync: TSynSelectedColor;
    fMarkupInfoCurrent: TSynSelectedColor;
    fMarkupInfoArea: TSynSelectedColor;

    function GetActive: Boolean;
    procedure SetActive(const AValue: Boolean);
    procedure SetCurrentCell(const AValue: Integer);
    procedure SetAreaMarkupEnabled(const AValue: Boolean);
    procedure SetEnabled(const AValue: Boolean);
    procedure SetMarkupEnabled(const AValue: Boolean);
  protected
    FMarkup: TSynPluginSyncronizedEditMarkup;
    FMarkupArea: TSynPluginSyncronizedEditMarkupArea;
    procedure MarkupChanged(AMarkup: TObject);
    function  CreateMarkup: TSynPluginSyncronizedEditMarkup; virtual;
    procedure SetEditor(const AValue: TCustomSynEdit); override;
    procedure DoLinesEdited(Sender: TSynEditStrings; aLinePos, aBytePos, aCount,
                            aLineBrkCnt: Integer; aText: String);
    procedure DoBeforeEdit(aX, aY: Integer; aUndoRedo: Boolean); virtual;
    procedure DoAfterEdit(aX, aY: Integer; aUndoRedo: Boolean); virtual;
    procedure DoClear; virtual;
    procedure DoOnActivate; virtual;
    procedure DoOnDeactivate; virtual;
    property CurrentCell: Integer read FCurrentCell write SetCurrentCell;
    property Cells: TSynPluginSyncronizedEditList read FCells;
    property Markup: TSynPluginSyncronizedEditMarkup read FMarkup;
    property MarkupArea: TSynPluginSyncronizedEditMarkupArea read FMarkupArea;
    property AreaMarkupEnabled: Boolean read FAreaMarkupEnabled write SetAreaMarkupEnabled;
    property MarkupEnabled: Boolean read FMarkupEnabled write SetMarkupEnabled;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Clear;
    property Enabled: Boolean read FEnabled write SetEnabled;
    property Active: Boolean read GetActive write SetActive;

    property MarkupInfo: TSynSelectedColor read FMarkupInfo;
    property MarkupInfoCurrent: TSynSelectedColor read FMarkupInfoCurrent;
    property MarkupInfoSync: TSynSelectedColor read FMarkupInfoSync;
    property MarkupInfoArea: TSynSelectedColor read FMarkupInfoArea;
  end;

  (* TSynPluginCustomSyncroEdit implements:
       - Locking of TrimTrailingSpace
       - CurrentCell follows Caret / LastCell
       - DeActivate if Edit outside Cell
       - DeActivate on undo/redo if needed
       - various helpers, to set the caret/block
  *)

  TSynPluginCustomSyncroEdit = class(TSynPluginSyncronizedEditBase)
  private
    FLastCell: Integer;
    FUndoRealCount, FRedoRealCount: Integer;
    FRedoList: TSynEditUndoList;
    FUndoList: TSynEditUndoList;
  protected
    procedure SetUndoStart; // Handle undo/redo stuff
    procedure SetEditor(const AValue: TCustomSynEdit); override;
    procedure DoOnActivate; override;
    procedure DoOnDeactivate; override;
    procedure DoBeforeEdit(aX, aY: Integer; aUndoRedo: Boolean); override;
    procedure UpdateCurrentCell;
    procedure DoCaretChanged(Sender: TObject);
    property LastCell: Integer read FLastCell;
  protected
    procedure SelectCurrentCell(Reverse: Boolean = False);
    procedure PreviousCell(SetSelect: Boolean = True; SkipSameIndex: Boolean = False);
    procedure NextCell(SetSelect: Boolean = True; SkipSameIndex: Boolean = False);
    procedure CellCaretHome;
    procedure CellCaretEnd;
  public
    //constructor Create(AOwner: TComponent); override;
    //destructor Destroy; override;
  end;

implementation

function CellsAreEqual(c1, c2: TSynPluginSyncronizedEditCell): boolean;
begin
  Result := (CompareCarets(c1.LogStart, c2.LogStart) = 0) and
            (CompareCarets(c1.LogEnd, c2.LogEnd) = 0) and
            (c1.Group = c2.Group);
end;

{ TSynPluginSyncronizedEditList }

constructor TSynPluginSyncronizedEditList.Create;
begin
  inherited;
  Clear;
end;

destructor TSynPluginSyncronizedEditList.Destroy;
begin
  Clear;
  inherited Destroy;
end;

procedure TSynPluginSyncronizedEditList.Clear;
var
  i: Integer;
begin
  for i := 0 to length(FCells) - 1 do begin
    if assigned(FOnCellChange) then
      FOnCellChange(i, FCells[i], nil);
    FCells[i].Free;
  end;
  SetLength(FCells, 0);
end;

function TSynPluginSyncronizedEditList.GetCell(aIndex: Integer): TSynPluginSyncronizedEditCell;
begin
  Result := FCells[aIndex];
end;

function TSynPluginSyncronizedEditList.GetGroupCell(aGroup,
  aIndex: Integer): TSynPluginSyncronizedEditCell;
var
  i: Integer;
begin
  i := 0;
  while i < length(FCells) do begin
    if FCells[i].Group = aGroup then begin
      dec(aIndex);
      if aIndex < 0 then exit(FCells[i]);
    end;
    inc(i);
  end;
  Result := nil;
end;

procedure TSynPluginSyncronizedEditList.SetCell(aIndex: Integer;
  const AValue: TSynPluginSyncronizedEditCell);
var
  OldVal: TSynPluginSyncronizedEditCell;
begin
  OldVal := FCells[aIndex];
  if CellsAreEqual(OldVal, AValue) then exit;
  FCells[aIndex] := AValue;
  if assigned(FOnCellChange) then
    FOnCellChange(aIndex, OldVal, AValue);
end;

function TSynPluginSyncronizedEditList.Add(aCell: TSynPluginSyncronizedEditCell): Integer;
var
  i: Integer;
begin
  i := length(FCells);
  SetLength(FCells, i + 1);
  FCells[i] := aCell;
  Result := i;
  if assigned(FOnCellChange) then
    FOnCellChange(i, nil, FCells[i]);
end;

function TSynPluginSyncronizedEditList.AddNew: TSynPluginSyncronizedEditCell;
begin
  Result := TSynPluginSyncronizedEditCell.Create;
  Add(Result);
end;

procedure TSynPluginSyncronizedEditList.Delete(aIndex: Integer);
var
  i: Integer;
begin
  FCells[aIndex].Free;
  i := length(FCells) - 1;
  if aIndex < i then
    System.Move(FCells[aIndex+1], FCells[aIndex], (i-aIndex) * SizeOf(TSynPluginSyncronizedEditCell));
  SetLength(FCells, i);
  if assigned(FOnCellChange) then
    FOnCellChange(aIndex, FCells[i], nil);
end;

function TSynPluginSyncronizedEditList.IndexOf(aCell: TSynPluginSyncronizedEditCell): Integer;
var
  i: Integer;
begin
  i := 0;
  while i < length(FCells) do
    if CellsAreEqual(FCells[i], aCell) then exit(i);
  Result := -1;
end;

function TSynPluginSyncronizedEditList.IndexOf(aX, aY: Integer; IncludeLast: Boolean): Integer;
var
  a, i: Integer;
begin
  if IncludeLast then
    a := 1
  else
    a := 0;;
  for i := 0 to Count -1 do begin
    if (FCells[i].Group >= 0) and
       ( (FCells[i].LogStart.Y < aY) or
         ((FCells[i].LogStart.Y = aY) and (FCells[i].LogStart.X <= aX)) ) and
       ( (FCells[i].LogEnd.Y > aY) or
         ((FCells[i].LogEnd.Y = aY) and (FCells[i].LogEnd.X + a > aX)) )
    then
      exit(i);
  end;
  Result := -1;
end;

function TSynPluginSyncronizedEditList.Count: Integer;
begin
  Result := length(FCells);
end;

function TSynPluginSyncronizedEditList.GroupCount(aGroup: Integer): Integer;
var
  i: Integer;
begin
  i := 0;
Result := 0;
  while i < length(FCells) do
    if FCells[i].Group = aGroup then
      inc(Result);
end;

{ TSynPluginSyncronizedEditMarkupBase }

procedure TSynPluginSyncronizedEditMarkupBase.SetCells(const AValue: TSynPluginSyncronizedEditList);
begin
  if FCells = AValue then exit;
  if FCells <> nil then
    FCells.OnCellChange := nil;
  FCells := AValue;
  if FCells <> nil then
    FCells.OnCellChange := @CellChanged;
end;

function TSynPluginSyncronizedEditMarkupBase.OwnedByMgr: Boolean;
begin
  Result := False;
end;

procedure TSynPluginSyncronizedEditMarkupBase.DoEnabledChanged(Sender: TObject);
var
  i: Integer;
begin
  if FCells.Count > 100 then
    InvalidateSynLines(-1, -1)
  else
    for i := 0 to FCells.Count - 1 do
      CellChanged(i, Cells[i], Cells[i]);
end;

constructor TSynPluginSyncronizedEditMarkupBase.Create(ASynEdit: TSynEditBase);
begin
  FCells := nil;
  inherited;
end;

destructor TSynPluginSyncronizedEditMarkupBase.Destroy;
begin
  Cells := nil;
  inherited Destroy;
end;

{ TSynPluginSyncronizedEditMarkup }

procedure TSynPluginSyncronizedEditMarkup.SetCurrentCell(const AValue: Integer);
var
  i, j: Integer;
begin
  if FCurrentCell = AValue then exit;
  if (FCurrentCell >= 0) and (FCurrentCell < Cells.Count) then begin
    j := Cells[FCurrentCell].Group;
    for i := 0 to Cells.Count -1 do
      if Cells[i].Group = j then
        InvalidateSynLines(Cells[i].LogStart.Y, Cells[i].LogEnd.Y);
  end;
  FCurrentCell := AValue;
  if (FCurrentCell >= 0) and (FCurrentCell < Cells.Count) then begin
    j := Cells[FCurrentCell].Group;
    for i := 0 to Cells.Count -1 do
      if Cells[i].Group = j then
        InvalidateSynLines(Cells[i].LogStart.Y, Cells[i].LogEnd.Y);
  end;
end;

procedure TSynPluginSyncronizedEditMarkup.CellChanged(aIndex: Integer; aOldVal,
  aNewVal: TSynPluginSyncronizedEditCell);
begin
  if (aOldVal <> nil) and (aOldVal.Group >= 0) then
    InvalidateSynLines(aOldVal.LogStart.Y, aOldVal.LogEnd.Y);
  if (aNewVal <> nil) and (aNewVal.Group >= 0) then
    InvalidateSynLines(aNewVal.LogStart.Y, aNewVal.LogEnd.Y);
end;

constructor TSynPluginSyncronizedEditMarkup.Create(ASynEdit: TSynEditBase);
begin
  inherited Create(ASynEdit);
  fMarkupInfoCurrent := TSynSelectedColor.Create;
  fMarkupInfoCurrent.OnChange := @MarkupChanged;
  fMarkupInfoSync := TSynSelectedColor.Create;
  fMarkupInfoSync.OnChange := @MarkupChanged;
  FPreparedRow := -1;
end;

destructor TSynPluginSyncronizedEditMarkup.Destroy;
begin
  FreeAndNil(fMarkupInfoCurrent);
  FreeAndNil(fMarkupInfoSync);
  inherited Destroy;
end;

function TSynPluginSyncronizedEditMarkup.GetMarkupAttributeAtRowCol(const aRow,
  aCol: Integer): TSynSelectedColor;
var
  i, col: Integer;
begin
  col := PhysicalToLogicalPos(Point(aCol, aRow)).x;
  Result := nil;
  for i := FPreparedCellFrom to FPreparedCellTo do begin
    if ( ((Cells[i].LogStart.y = aRow) and (Cells[i].LogStart.x <= Col)) or
         (Cells[i].LogStart.y < aRow) ) and
       ( ((Cells[i].LogEnd.y = aRow) and (Cells[i].LogEnd.x > Col)) or
         (Cells[i].LogEnd.y > aRow) ) and
       (Cells[i].Group >= 0) // dont't display negative groups
    then begin
      if i = CurrentCell then
        Result := MarkupInfoCurrent
      else
      if (CurrentCell >= 0) and (Cells[i].Group = Cells[CurrentCell].Group) then
        Result := MarkupInfoSync
      else
        Result := MarkupInfo;

      Result.StartX := LogicalToPhysicalPos(Cells[i].LogStart).x;
      if Cells[i].LogStart.y < aRow then
        Result.StartX := -1;
      Result.EndX := LogicalToPhysicalPos(Cells[i].LogEnd).x - 1;
      if Cells[i].LogEnd.y > aRow then
        Result.EndX := -1;
      break;
    end;
  end;
end;

function TSynPluginSyncronizedEditMarkup.GetNextMarkupColAfterRowCol(const aRow,
  aCol: Integer): Integer;
var
  i, col: Integer;
begin
  col := PhysicalToLogicalPos(Point(aCol, aRow)).x;
  Result := -1;
  for i := FPreparedCellFrom to FPreparedCellTo do begin
    if Cells[i].Group < 0 then continue;
    if (Cells[i].LogStart.y = aRow) and (Cells[i].LogStart.x > Col) and
       ( (Cells[i].LogStart.x < Result) or (Result < 0) )
    then
      Result := Cells[i].LogStart.x;
    if (Cells[i].LogEnd.y = aRow) and (Cells[i].LogEnd.x > Col) and
       ( (Cells[i].LogEnd.x < Result) or (Result < 0) )
    then
      Result := Cells[i].LogEnd.x;
  end;
  if Result >= 0 then
    Result := LogicalToPhysicalPos(Point(Result, aRow)).x;
end;

procedure TSynPluginSyncronizedEditMarkup.PrepareMarkupForRow(aRow: Integer);
var
  i, j, t, b: Integer;
begin
  inherited PrepareMarkupForRow(aRow);
  if FPreparedRow < 0 then begin
    i := 0;
    j := Cells.Count - 1;
    t := TopLine;
    b := ScreenRowToRow(LinesInWindow + 1) + 1;
    while (i <= j) and ((Cells[i].Group < 0 ) or ((Cells[i].LogStart.y < t) and (Cells[i].LogEnd.y < t))) do
      inc(i);
    FPreparedCellTop := i;
    while (i <= j) and ((Cells[j].Group < 0 ) or ((Cells[j].LogStart.y > b) and (Cells[j].LogEnd.y > b))) do
      dec(j);
    FPreparedCellBottom := j;
  end;;
  i := FPreparedCellTop;
  j := FPreparedCellBottom;
  if FPreparedRow >= 0 then begin
    if FPreparedRow < aRow then
      i := FPreparedCellFrom
    else
      j := FPreparedCellTo;
  end;
  FPreparedRow := aRow;
  while (i <= j) and ((Cells[j].Group < 0 ) or ((Cells[i].LogStart.y < aRow) and (Cells[i].LogEnd.y < aRow))) do
    inc(i);
  FPreparedCellFrom := i;
  while (i <= j) and ((Cells[j].Group < 0 ) or ((Cells[j].LogStart.y > aRow) and (Cells[j].LogEnd.y > aRow))) do
    dec(j);
  FPreparedCellTo := j;
end;

procedure TSynPluginSyncronizedEditMarkup.EndMarkup;
begin
  inherited EndMarkup;
  FPreparedRow := -1;
end;

{ TSynPluginSyncronizedEditMarkupArea }

procedure TSynPluginSyncronizedEditMarkupArea.CellChanged(aIndex: Integer; aOldVal,
  aNewVal: TSynPluginSyncronizedEditCell);
begin
  if (aOldVal <> nil) and (aOldVal.Group = CellGroupForArea) then
    InvalidateSynLines(aOldVal.LogStart.Y, aOldVal.LogEnd.Y);
  if (aNewVal <> nil) and (aNewVal.Group = CellGroupForArea) then
    InvalidateSynLines(aNewVal.LogStart.Y, aNewVal.LogEnd.Y);
end;

function TSynPluginSyncronizedEditMarkupArea.GetMarkupAttributeAtRowCol(const aRow,
  aCol: Integer): TSynSelectedColor;
var
  ac: TSynPluginSyncronizedEditCell;
begin
  Result := nil;
  if MarkupInfo.IsEnabled then begin
    ac := Cells.GroupCell[CellGroupForArea, 0];
    if (ac <> nil) and
       ( ((ac.LogStart.y = aRow) and (ac.LogStart.x <= aCol)) or (ac.LogStart.y < aRow)) and
       ( ((ac.LogEnd.y = aRow) and (ac.LogEnd.x > aCol)) or (ac.LogEnd.y > aRow))
    then
      Result := MarkupInfo;
  end;
end;

function TSynPluginSyncronizedEditMarkupArea.GetNextMarkupColAfterRowCol(const aRow,
  aCol: Integer): Integer;
var
  ac: TSynPluginSyncronizedEditCell;
begin
  Result := -1;
  if MarkupInfo.IsEnabled then begin
    ac := Cells.GroupCell[CellGroupForArea, 0];
    if ac <> nil then begin
      if (ac.LogStart.y = aRow) and (ac.LogStart.x > aCol) and
         ( (ac.LogStart.x < Result) or (Result < 0) )
      then
        Result := ac.LogStart.x;
      if (ac.LogEnd.y = aRow) and (ac.LogEnd.x > aCol) and
         ( (ac.LogEnd.x < Result) or (Result < 0) )
      then
        Result := ac.LogEnd.x;
    end;
  end;
end;

{ TSynPluginSyncronizedEditBase }

constructor TSynPluginSyncronizedEditBase.Create(AOwner: TComponent);
begin
  fMarkupInfo := TSynSelectedColor.Create;
  fMarkupInfo.OnChange := @MarkupChanged;
  fMarkupInfoSync := TSynSelectedColor.Create;
  fMarkupInfoSync.OnChange := @MarkupChanged;
  fMarkupInfoCurrent := TSynSelectedColor.Create;
  fMarkupInfoCurrent.OnChange := @MarkupChanged;
  fMarkupInfoArea := TSynSelectedColor.Create;
  fMarkupInfoArea.OnChange := @MarkupChanged;

  MarkupInfo.FrameColor := clMaroon;
  MarkupInfo.Background := clNone;
  MarkupInfo.Foreground := clNone;

  MarkupInfoCurrent.FrameColor := clAqua;
  MarkupInfoCurrent.Background := clNone;
  MarkupInfoCurrent.Foreground := clNone;

  MarkupInfoSync.FrameColor := clFuchsia;
  MarkupInfoSync.Background := clNone;
  MarkupInfoSync.Foreground := clNone;

  MarkupInfoArea.FrameColor := clNone;
  MarkupInfoArea.Background := clNone;
  MarkupInfoArea.Foreground := clNone;

  FCells := TSynPluginSyncronizedEditList.Create;
  CurrentCell := -1;
  AreaMarkupEnabled := False;
  MarkupEnabled := True;
  inherited Create(AOwner);
  FEnabled := True;
  Active := False;
  FEditing := False;
end;

destructor TSynPluginSyncronizedEditBase.Destroy;
begin
  Editor := nil;
  FreeAndNil(FMarkup);
  FreeAndNil(FMarkupArea);
  FreeAndNil(FCells);
  FreeAndNil(fMarkupInfo);
  FreeAndNil(fMarkupInfoSync);
  FreeAndNil(fMarkupInfoCurrent);
  FreeAndNil(fMarkupInfoArea);
  inherited;
end;

procedure TSynPluginSyncronizedEditBase.Clear;
begin
  FCells.Clear;
  CurrentCell := -1;
  Active := False;
  DoClear;
end;

procedure TSynPluginSyncronizedEditBase.SetEditor(const AValue: TCustomSynEdit);
begin
  if AValue = Editor then exit;
  Active := False;
  if Editor <> nil then begin
    ViewedTextBuffer.RemoveEditHandler(@DoLinesEdited);
    if FMarkup <> nil then begin
      TSynEditMarkupManager(MarkupMgr).RemoveMarkUp(FMarkup);
      FreeAndNil(FMarkup);
    end;
    if FMarkupArea <> nil then begin
      TSynEditMarkupManager(MarkupMgr).RemoveMarkUp(FMarkupArea);
      FreeAndNil(FMarkupArea);
    end;
  end;
  inherited SetEditor(AValue);
  if AValue <> nil then begin
    FMarkup := CreateMarkup;
    FMarkup.Cells := FCells;
    FMarkup.CurrentCell := FCurrentCell;
    FMarkup.Enabled := Active;
    TSynEditMarkupManager(MarkupMgr).AddMarkUp(FMarkup);
    FMarkupArea := TSynPluginSyncronizedEditMarkupArea.Create(Editor);
    FMarkupArea.Cells := FCells;
    FMarkupArea.Enabled := Active;
    TSynEditMarkupManager(MarkupMgr).AddMarkUp(FMarkupArea, True);
    MarkupChanged(nil);
    ViewedTextBuffer.AddEditHandler(@DoLinesEdited);
  end;
end;

procedure TSynPluginSyncronizedEditBase.SetCurrentCell(const AValue: Integer);
begin
  if FCurrentCell = AValue then exit;
  FCurrentCell := AValue;
  if FMarkup <> nil then
    FMarkup.CurrentCell := FCurrentCell;
end;

procedure TSynPluginSyncronizedEditBase.SetAreaMarkupEnabled(const AValue: Boolean);
begin
  if FAreaMarkupEnabled = AValue then exit;
  FAreaMarkupEnabled := AValue;
  if FMarkupArea <> nil then
    FMarkupArea.Enabled := Active and FAreaMarkupEnabled;
end;

procedure TSynPluginSyncronizedEditBase.SetEnabled(const AValue: Boolean);
var
  IsActive: Boolean;
begin
  IsActive := Active;
  FEnabled := AValue;
  if FMarkup <> nil then
    FMarkup.Enabled := Active and FMarkupEnabled;
  if FMarkupArea <> nil then
    FMarkupArea.Enabled := Active and FAreaMarkupEnabled;
  if IsActive <> Active then begin
    if Active
    then DoOnActivate
    else DoOnDeactivate;
  end;
end;

procedure TSynPluginSyncronizedEditBase.SetMarkupEnabled(const AValue: Boolean);
begin
  if FMarkupEnabled = AValue then exit;
  FMarkupEnabled := AValue;
  if FMarkup <> nil then
    FMarkup.Enabled := Active and FMarkupEnabled;
end;

procedure TSynPluginSyncronizedEditBase.MarkupChanged(AMarkup: TObject);
begin
  if FMarkup <> nil then begin
    FMarkup.MarkupInfo.Assign(fMarkupInfo);
    FMarkup.MarkupInfoSync.Assign(fMarkupInfoSync);
    FMarkup.MarkupInfoCurrent.Assign(fMarkupInfoCurrent);
  end;
  if FMarkupArea <> nil then
    FMarkupArea.MarkupInfo.Assign(fMarkupInfoArea);
end;

function TSynPluginSyncronizedEditBase.CreateMarkup: TSynPluginSyncronizedEditMarkup;
begin
  Result := TSynPluginSyncronizedEditMarkup.Create(Editor);
end;

function TSynPluginSyncronizedEditBase.GetActive: Boolean;
begin
  Result := FActive and FEnabled and (Editor <> nil);
end;

procedure TSynPluginSyncronizedEditBase.SetActive(const AValue: Boolean);
var
  IsActive: Boolean;
begin
  IsActive := Active;
  FActive := AValue;
  if FMarkup <> nil then
    FMarkup.Enabled := Active and FMarkupEnabled;
  if FMarkupArea <> nil then
    FMarkupArea.Enabled := Active and FAreaMarkupEnabled;
  if IsActive <> Active then begin
    if Active
    then DoOnActivate
    else DoOnDeactivate;
  end;
end;

procedure TSynPluginSyncronizedEditBase.DoLinesEdited(Sender: TSynEditStrings;
  aLinePos, aBytePos, aCount, aLineBrkCnt: Integer; aText: String);
var
  Pos, Pos2: TPoint;

  function AdjustPoint(aPoint: Tpoint; var Changed: Boolean): TPoint; inline;
  begin
    Result := aPoint;
    if aLineBrkCnt < 0 then begin
      (* Lines Deleted *)
      if aPoint.y > aLinePos then begin
        Result.y := Max(aLinePos, Result.y + aLineBrkCnt);
        if Result.y = aLinePos then
          Result.x := Result.x + Pos.x - 1;
      end;
    end
    else
    if aLineBrkCnt > 0 then begin
      (* Lines Inserted *)
      if aPoint.y >= aLinePos then begin
        if (aPoint.y = aLinePos) and (aPoint.x > Pos.x) then
          Result.x := Result.x - Pos.x + 1;
        Result.y := Result.y + aLineBrkCnt;
      end;
    end
    else
    if aCount <> 0 then begin
      (* Chars Insert/Deleted *)
      if (aPoint.y = aLinePos) and (aPoint.x >= Pos.x) then
        Result.x := Max(Pos.x, Result.x + aCount);
    end;
    Changed := Changed or (aPoint.x <> Result.x) or (aPoint.y <> Result.y);
  end;

var
  i, a: Integer;
  CurCell: TSynPluginSyncronizedEditCell;
  Y2, X2: Integer;
  chg: Boolean;
  edit: Boolean;
  CaretPos: TPoint;
begin
  if not Active then exit;
  Pos := Point(aBytePos, aLinePos);
  Pos2 := Pos;
  if not FEditing then
    DoBeforeEdit(Pos.x, Pos.y, IsUndoing or IsRedoing);

  // Todo: need do add undo info (start/stop flag),
  // so we know which group (if any) this applies to
  edit := FEditing or  IsUndoing or IsRedoing;
  for i := 0 to FCells.Count - 1 do begin
    CurCell := Cells[i];
    chg := False;
    a := CompareCarets(Pos, CurCell.LogStart);
    if (a > 0) then
      CurCell.LogStart := AdjustPoint(CurCell.LogStart, chg);
    a := CompareCarets(Pos, CurCell.LogEnd);
    if (a > 0) or ((a = 0) and ((i = FCurrentCell) or edit)) then
      CurCell.LogEnd := AdjustPoint(CurCell.LogEnd, chg);
    if chg then
      Cells[i] := CurCell;
  end;

  if (not (FEditing or IsUndoing or IsRedoing)) and
     (FCurrentCell >= 0) and (FCurrentCell < Cells.Count) and
     (CompareCarets(Pos, FCells[FCurrentCell].LogStart) <= 0) and
     (CompareCarets(Pos, FCells[FCurrentCell].LogEnd) >= 0)
  then begin
    ViewedTextBuffer.BeginUpdate;
    try
      FEditing := True;
      CaretPos := CaretObj.LineBytePos;
      CurCell := FCells[FCurrentCell];
      a := CurCell.Group;
      Pos.Y := Pos.Y - CurCell.LogStart.y;
      if Pos.y = 0
      then Pos.X := Pos.X - CurCell.LogStart.x
      else dec(Pos.x);
      for i := 0 to FCells.Count - 1 do begin
        if FCells[i].LogStart.Y = FCells[i].LogEnd.Y
        then x2 := FCells[i].LogStart.X + Pos.X
        else x2 := 1 + Pos.X;
        if (i <> FCurrentCell) and (FCells[i].Group = a) and
           ( (FCells[i].LogStart.Y + Pos.Y < FCells[i].LogEnd.Y) or
             ( (FCells[i].LogStart.Y + Pos.Y = FCells[i].LogEnd.Y) and
               (x2 <= FCells[i].LogEnd.X) )
           )
        then begin
          Y2 := FCells[i].LogStart.Y + Pos.Y;
          X2 := Pos.X;
          if Pos.Y = 0
          then X2 := X2 + FCells[i].LogStart.X
          else inc(X2);
          if aLineBrkCnt = -1 then begin
            ViewedTextBuffer.EditLineJoin(Y2);
            if (CaretPos.y > Y2) then begin
              dec(CaretPos.y);
              if (CaretPos.y = Y2) then
                inc(CaretPos.x, X2 - 1);
            end;
          end
          else if aLineBrkCnt < -1 then begin
            ViewedTextBuffer.EditLinesDelete(Y2, -aLineBrkCnt);
            if (CaretPos.y > Y2) then
              inc(CaretPos.y, aLineBrkCnt);
          end
          else if aLineBrkCnt = 1 then begin
            ViewedTextBuffer.EditLineBreak(X2, Y2);
            if (CaretPos.y > Y2) then
              inc(CaretPos.y);
            if (CaretPos.y = Y2) and (CaretPos.x > X2) then begin
              inc(CaretPos.y);
              dec(CaretPos.x, X2 - 1);
            end;
          end
          else if aLineBrkCnt > 1 then begin
            ViewedTextBuffer.EditLinesInsert(Y2, aLineBrkCnt);
            if (CaretPos.y > Y2) then
              inc(CaretPos.y, aLineBrkCnt);
          end
          else if aCount < 0 then begin
            ViewedTextBuffer.EditDelete(X2, Y2, -aCount);
            if (CaretPos.y = Y2) and (CaretPos.X > X2) then
              inc(CaretPos.X, aCount);
          end
          else if aCount > 0 then begin
            ViewedTextBuffer.EditInsert(X2, Y2, aText);
            if (CaretPos.y = Y2) and (CaretPos.X > X2) then
              inc(CaretPos.X, aCount);
          end;
        end;
      end
    finally
      ViewedTextBuffer.EndUpdate;
    end;
    CaretObj.LineBytePos := CaretPos;
    FEditing := False;
    if Pos.y = 0 then
      pos2.x := pos.x + CurCell.LogStart.x;
    Pos2.y := Pos.y + CurCell.LogStart.y;
  end;

  if not FEditing then
    DoAfterEdit(Pos2.x, Pos2.y, IsUndoing or IsRedoing);
end;

procedure TSynPluginSyncronizedEditBase.DoBeforeEdit(aX, aY: Integer; aUndoRedo: Boolean);
begin
  (* Do Nothing *);
end;

procedure TSynPluginSyncronizedEditBase.DoAfterEdit(aX, aY: Integer; aUndoRedo: Boolean);
begin
  (* Do Nothing *);
end;

procedure TSynPluginSyncronizedEditBase.DoClear;
begin
  (* Do Nothing *);
end;

procedure TSynPluginSyncronizedEditBase.DoOnActivate;
begin
  (* Do Nothing *);
end;

procedure TSynPluginSyncronizedEditBase.DoOnDeactivate;
begin
  (* Do Nothing *);
end;

{ TSynPluginSyncronizedEditCell }

procedure TSynPluginSyncronizedEditCell.Assign(Src: TSynPluginSyncronizedEditCell);
begin
  if Src = nil then exit;
  FLogStart := Src.FLogStart;
  FLogEnd := Src.FLogEnd;
  FGroup := Src.FGroup;
end;

{ TSynPluginCustomSyncroEdit }

procedure TSynPluginCustomSyncroEdit.SetUndoStart;
begin
  FUndoList.ForceGroupEnd;
  FUndoRealCount := FUndoList.RealCount;
  FRedoRealCount := FRedoList.RealCount;
end;

procedure TSynPluginCustomSyncroEdit.SetEditor(const AValue: TCustomSynEdit);
begin
  if Editor = AValue then exit;
  if Editor <> nil then begin
    CaretObj.RemoveChangeHandler(@DoCaretChanged);
    FRedoList := nil;
    FUndoList := nil;
  end;
  inherited SetEditor(AValue);
  if Editor <> nil then begin
    CaretObj.AddChangeHandler(@DoCaretChanged);
    FRedoList := ViewedTextBuffer.RedoList;
    FUndoList := ViewedTextBuffer.UndoList;
  end;
end;

procedure TSynPluginCustomSyncroEdit.DoOnActivate;
var
  b: TSynEditStrings;
begin
  b := ViewedTextBuffer;
  while b <> nil do begin
    if b is TSynEditStringTrimmingList then TSynEditStringTrimmingList(b).Lock;
    if b is TSynEditStringsLinked then
      b := TSynEditStringsLinked(b).NextLines
    else
      b := nil;
  end;
end;

procedure TSynPluginCustomSyncroEdit.DoOnDeactivate;
var
  b: TSynEditStrings;
begin
  FUndoRealCount := -1;
  FRedoRealCount := -1;
  b := ViewedTextBuffer;
  while b <> nil do begin
    if b is TSynEditStringTrimmingList then TSynEditStringTrimmingList(b).UnLock;
    if b is TSynEditStringsLinked then
      b := TSynEditStringsLinked(b).NextLines
    else
      b := nil;
  end;
end;

procedure TSynPluginCustomSyncroEdit.DoBeforeEdit(aX, aY: Integer; aUndoRedo: Boolean);
begin
  inherited;
  if IsUndoing and (FUndoRealCount >= 0) and (FUndoList.RealCount < FUndoRealCount)
  then
    Active := false;
  if IsRedoing and (FRedoRealCount >= 0) and (FRedoList.RealCount < FUndoRealCount)
  then
    Active := false;
  if aUndoRedo or not Active then exit;
  FRedoRealCount := -1;
  UpdateCurrentCell;
  if CurrentCell < 0 then begin
    Clear;
    Active := False;
  end;
end;

procedure TSynPluginCustomSyncroEdit.UpdateCurrentCell;
var
  i: Integer;
begin
  i := Cells.IndexOf(CaretObj.BytePos, CaretObj.LinePos, True);
  if (i <> CurrentCell) and (CurrentCell >= 0) then
    FLastCell := CurrentCell;
  CurrentCell := i;
end;

procedure TSynPluginCustomSyncroEdit.DoCaretChanged(Sender: TObject);
begin
  if not Active then exit;
  UpdateCurrentCell;
end;

procedure TSynPluginCustomSyncroEdit.SelectCurrentCell(Reverse: Boolean);
begin
  if (CurrentCell < 0) and (LastCell >= 0) then
    CurrentCell := LastCell;
  if (CurrentCell < 0) then
    exit;
  if Reverse then begin
    CaretObj.LineBytePos := Cells[CurrentCell].LogStart;
    Editor.BlockBegin := Cells[CurrentCell].LogEnd;
    Editor.BlockEnd := Cells[CurrentCell].LogStart;
  end else begin
    CaretObj.LineBytePos := Cells[CurrentCell].LogEnd;
    Editor.BlockBegin := Cells[CurrentCell].LogStart;
    Editor.BlockEnd := Cells[CurrentCell].LogEnd;
  end;
end;

procedure TSynPluginCustomSyncroEdit.PreviousCell(SetSelect: Boolean; SkipSameIndex: Boolean = False);
var
  i, j, x: Integer;
  Pos: TPoint;
begin
  Pos := CaretObj.LineBytePos;
  i := Cells.IndexOf(Pos.x, Pos.y, True);
  if i < 0 then begin
    x := -1;
    i := 0;
    while (i < Cells.Count) and
      ((Cells[i].Group < 0) or (CompareCarets(Cells[i].LogEnd, Pos) >= 0))
    do
      inc(i);
  end
  else
    x := Cells[i].Group;

  j := 0;
  Repeat
    dec(i);
    inc(j);
    if i < 0 then
      i := Cells.Count - 1;
  until (j > Cells.Count) or
        ((Cells[i].Group >= 0) and
         ((not SkipSameIndex) or (Cells[i].Group <> x)) );
  CurrentCell := i;

  if CurrentCell < 0 then
    exit;
  CaretObj.LineBytePos := Cells[CurrentCell].LogEnd;
  if SetSelect then
    SelectCurrentCell
  else
    Editor.BlockBegin := Cells[CurrentCell].LogEnd;
end;

procedure TSynPluginCustomSyncroEdit.NextCell(SetSelect: Boolean; SkipSameIndex: Boolean = False);
var
  Pos: TPoint;
  i, j, x: Integer;
begin
  Pos := CaretObj.LineBytePos;
  i := Cells.IndexOf(Pos.x, Pos.y, True);
  if i < 0 then begin
    x := -1;
    i := Cells.Count - 1;
    while (i >= 0) and
      ((Cells[i].Group < 0) or (CompareCarets(Cells[i].LogEnd, Pos) <= 0))
    do
      dec(i);
  end
  else
    x := Cells[i].Group;


  j := 0;
  Repeat
    inc(i);
    inc(j);
    if i >= Cells.Count then
      i := 0
  until (j > Cells.Count) or
        ((Cells[i].Group >= 0) and
         ((not SkipSameIndex) or (Cells[i].Group <> x)) );
  CurrentCell := i;
  if CurrentCell < 0 then
    exit;
  CaretObj.LineBytePos := Cells[CurrentCell].LogStart;
  if SetSelect then
    SelectCurrentCell(True)
  else
    Editor.BlockBegin := Cells[CurrentCell].LogStart;
end;

procedure TSynPluginCustomSyncroEdit.CellCaretHome;
begin
  if (CurrentCell < 0) and (LastCell >= 0) then
    CurrentCell := LastCell;
  if (CurrentCell < 0) then
    exit;
  CaretObj.LineBytePos := Cells[CurrentCell].LogStart;
  Editor.BlockBegin := Cells[CurrentCell].LogStart;
end;

procedure TSynPluginCustomSyncroEdit.CellCaretEnd;
begin
  if (CurrentCell < 0) and (LastCell >= 0) then
    CurrentCell := LastCell;
  if (CurrentCell < 0) then
    exit;
  CaretObj.LineBytePos := Cells[CurrentCell].LogEnd;
  Editor.BlockBegin := Cells[CurrentCell].LogEnd;
end;

end.

