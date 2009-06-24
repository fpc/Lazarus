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
unit SynPluginSyncEditBase;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, math, Graphics,
  SynEditMiscClasses, SynEdit, SynEditMarkup, SynEditMiscProcs, SynEditTextBase;

type

  { TSynPluginSyncEditCell }

  TSynPluginSyncEditCell = class
  private
    FLogStart, FLogEnd: TPoint;
    FGroup: Integer;
  public
    procedure Assign(Src: TSynPluginSyncEditCell); reintroduce;

    property LogStart: TPoint read FLogStart write FLogStart;
    property LogEnd: TPoint read FLogEnd write FLogEnd;
    property Group: Integer read FGroup write FGroup;
  end;

  TSynPluginSyncEditCellChangedEvent = procedure(aIndex: Integer;
                            aOldVal, aNewVal: TSynPluginSyncEditCell) of object;

  { TSynPluginSyncEditList }

  TSynPluginSyncEditList = class
  private
    FCells: Array of TSynPluginSyncEditCell;
    FOnCellChange: TSynPluginSyncEditCellChangedEvent;
    function GetCell(aIndex: Integer): TSynPluginSyncEditCell;
    function GetGroupCell(aGroup, aIndex: Integer): TSynPluginSyncEditCell;
    procedure SetCell(aIndex: Integer; const AValue: TSynPluginSyncEditCell);
  protected
    property OnCellChange: TSynPluginSyncEditCellChangedEvent // For Markup
             read FOnCellChange write FOnCellChange;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    function Add(aCell: TSynPluginSyncEditCell): Integer;
    function AddNew: TSynPluginSyncEditCell; virtual;
    procedure Delete(aIndex: Integer);
    function IndexOf(aCell: TSynPluginSyncEditCell): Integer;
    function IndexOf(aX, aY: Integer; IncludeLast: Boolean = False): Integer;
    function Count: Integer;
    function GroupCount(aGroup: Integer): Integer;
    property Cell[aIndex: Integer]: TSynPluginSyncEditCell
      read GetCell write SetCell; default;
    property GroupCell[aGroup, aIndex: Integer]: TSynPluginSyncEditCell
      read GetGroupCell;
  end;

  { TSynPluginSyncEditMarkup }

  TSynPluginSyncEditMarkup = class(TSynEditMarkup)
  private
    FCells: TSynPluginSyncEditList;
    FCurrentCell: Integer;
    fMarkupInfoCurrent: TSynSelectedColor;
    fMarkupInfoSync: TSynSelectedColor;
    procedure SetCells(const AValue: TSynPluginSyncEditList);
    procedure CellChanged(aIndex: Integer; aOldVal, aNewVal: TSynPluginSyncEditCell);
    procedure SetCurrentCell(const AValue: Integer);
  protected
    function OwnedByMgr: Boolean; override;
    procedure DoEnabledChanged(Sender: TObject); override;
    property CurrentCell: Integer read FCurrentCell write SetCurrentCell;
    property Cells: TSynPluginSyncEditList read FCells write SetCells;
  public
    constructor Create(ASynEdit: TSynEditBase);
    destructor Destroy; override;
    function GetMarkupAttributeAtRowCol(const aRow, aCol: Integer): TSynSelectedColor; override;
    function GetNextMarkupColAfterRowCol(const aRow, aCol: Integer): Integer; override;
    property MarkupInfoCurrent: TSynSelectedColor read fMarkupInfoCurrent;
    property MarkupInfoSync: TSynSelectedColor read fMarkupInfoSync;
  end;

  { TSynPluginSyncEditBase }

  TSynPluginSyncEditBase = class(TSynEditPlugin)
  private
    FActive: Boolean;
    FCells: TSynPluginSyncEditList;
    FCurrentCell: Integer;
    FEnabled: Boolean;
    FMarkup: TSynPluginSyncEditMarkup;
    FEditing: Boolean;
    function GetActive: Boolean;
    procedure SetActive(const AValue: Boolean);
    procedure SetCurrentCell(const AValue: Integer);
    procedure SetEnabled(const AValue: Boolean);
  protected
    procedure SetEditor(const AValue: TCustomSynEdit); override;
    procedure DoLinesEdited(Sender: TSynEditStrings; aLinePos, aBytePos, aCount,
                            aLineBrkCnt: Integer; aText: String);
    procedure DoBeforeEdit(aX, aY: Integer); virtual;
    procedure DoAfterEdit(aX, aY: Integer); virtual;
    procedure DoOnActivate; virtual;
    procedure DoOnDeactivate; virtual;
    property CurrentCell: Integer read FCurrentCell write SetCurrentCell;
    property Cells: TSynPluginSyncEditList read FCells;
    property Markup: TSynPluginSyncEditMarkup read FMarkup;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Clear;
    property Enabled: Boolean read FEnabled write SetEnabled;
    property Active: Boolean read GetActive write SetActive;
  end;

implementation

function CellsAreEqual(c1, c2: TSynPluginSyncEditCell): boolean;
begin
  Result := (CompareCarets(c1.LogStart, c2.LogStart) = 0) and
            (CompareCarets(c1.LogEnd, c2.LogEnd) = 0) and
            (c1.Group = c2.Group);
end;

{ TSynPluginSyncEditList }

constructor TSynPluginSyncEditList.Create;
begin
  inherited;
  Clear;
end;

destructor TSynPluginSyncEditList.Destroy;
begin
  Clear;
  inherited Destroy;
end;

procedure TSynPluginSyncEditList.Clear;
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

function TSynPluginSyncEditList.GetCell(aIndex: Integer): TSynPluginSyncEditCell;
begin
  Result := FCells[aIndex];
end;

function TSynPluginSyncEditList.GetGroupCell(aGroup,
  aIndex: Integer): TSynPluginSyncEditCell;
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

procedure TSynPluginSyncEditList.SetCell(aIndex: Integer;
  const AValue: TSynPluginSyncEditCell);
var
  OldVal: TSynPluginSyncEditCell;
begin
  OldVal := FCells[aIndex];
  if CellsAreEqual(OldVal, AValue) then exit;
  FCells[aIndex] := AValue;
  if assigned(FOnCellChange) then
    FOnCellChange(aIndex, OldVal, AValue);
end;

function TSynPluginSyncEditList.Add(aCell: TSynPluginSyncEditCell): Integer;
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

function TSynPluginSyncEditList.AddNew: TSynPluginSyncEditCell;
begin
  Result := TSynPluginSyncEditCell.Create;
  Add(Result);
end;

procedure TSynPluginSyncEditList.Delete(aIndex: Integer);
var
  i: Integer;
begin
  FCells[aIndex].Free;
  i := length(FCells) - 1;
  if aIndex < i then
    System.Move(FCells[aIndex+1], FCells[aIndex], (i-aIndex) * SizeOf(TSynPluginSyncEditCell));
  SetLength(FCells, i);
  if assigned(FOnCellChange) then
    FOnCellChange(aIndex, FCells[i], nil);
end;

function TSynPluginSyncEditList.IndexOf(aCell: TSynPluginSyncEditCell): Integer;
var
  i: Integer;
begin
  i := 0;
  while i < length(FCells) do
    if CellsAreEqual(FCells[i], aCell) then exit(i);
  Result := -1;
end;

function TSynPluginSyncEditList.IndexOf(aX, aY: Integer; IncludeLast: Boolean): Integer;
var
  a, i: Integer;
begin
  if IncludeLast then
    a := 1
  else
    a := 0;;
  for i := 0 to Count -1 do begin
    if ( (FCells[i].LogStart.Y < aY) or
         ((FCells[i].LogStart.Y = aY) and (FCells[i].LogStart.X <= aX)) ) and
       ( (FCells[i].LogEnd.Y > aY) or
         ((FCells[i].LogEnd.Y = aY) and (FCells[i].LogEnd.X + a > aX)) )
    then
      exit(i);
  end;
  Result := -1;
end;

function TSynPluginSyncEditList.Count: Integer;
begin
  Result := length(FCells);
end;

function TSynPluginSyncEditList.GroupCount(aGroup: Integer): Integer;
var
  i: Integer;
begin
  i := 0;
Result := 0;
  while i < length(FCells) do
    if FCells[i].Group = aGroup then
      inc(Result);
end;

{ TSynPluginSyncEditMarkup }

procedure TSynPluginSyncEditMarkup.SetCells(const AValue: TSynPluginSyncEditList);
begin
  if FCells = AValue then exit;
  if FCells <> nil then
    FCells.OnCellChange := nil;
  FCells := AValue;
  if FCells <> nil then
    FCells.OnCellChange := @CellChanged;
end;

procedure TSynPluginSyncEditMarkup.CellChanged(aIndex: Integer; aOldVal,
  aNewVal: TSynPluginSyncEditCell);
begin
  if aOldVal <> nil then
    InvalidateSynLines(aOldVal.LogStart.Y, aOldVal.LogEnd.Y);
  if aNewVal <> nil then
    InvalidateSynLines(aNewVal.LogStart.Y, aNewVal.LogEnd.Y);
end;

procedure TSynPluginSyncEditMarkup.SetCurrentCell(const AValue: Integer);
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

function TSynPluginSyncEditMarkup.OwnedByMgr: Boolean;
begin
  Result := False;
end;

procedure TSynPluginSyncEditMarkup.DoEnabledChanged(Sender: TObject);
var
  i: Integer;
begin
  for i := 0 to FCells.Count - 1 do
    CellChanged(i, Cells[i], Cells[i]);
end;

constructor TSynPluginSyncEditMarkup.Create(ASynEdit: TSynEditBase);
begin
  FCells := nil;
  inherited;
  fMarkupInfoCurrent := TSynSelectedColor.Create;
  fMarkupInfoCurrent.OnChange := @MarkupChanged;
  fMarkupInfoSync := TSynSelectedColor.Create;
  fMarkupInfoSync.OnChange := @MarkupChanged;

  MarkupInfo.FrameColor := clMaroon;
  MarkupInfo.Background := clNone;
  MarkupInfo.Foreground := clNone;

  MarkupInfoCurrent.FrameColor := clAqua;
  MarkupInfoCurrent.Background := clNone;
  MarkupInfoCurrent.Foreground := clNone;

  MarkupInfoSync.FrameColor := clFuchsia;
  MarkupInfoSync.Background := clNone;
  MarkupInfoSync.Foreground := clNone;
end;

destructor TSynPluginSyncEditMarkup.Destroy;
begin
  Cells := nil;
  FreeAndNil(fMarkupInfoCurrent);
  FreeAndNil(fMarkupInfoSync);
  inherited Destroy;
end;

function TSynPluginSyncEditMarkup.GetMarkupAttributeAtRowCol(const aRow,
  aCol: Integer): TSynSelectedColor;
var
  i: Integer;
  m : TSynSelectedColor;
begin
  Result := nil;
  for i := 0 to Cells.Count -1 do begin
    if ( ((Cells[i].LogStart.y = aRow) and (Cells[i].LogStart.x <= aCol)) or
         (Cells[i].LogStart.y < aRow) ) and
       ( ((Cells[i].LogEnd.y = aRow) and (Cells[i].LogEnd.x > aCol)) or
         (Cells[i].LogEnd.y > aRow) ) and
       (Cells[i].Group >= 0) // dont't display negative groups
    then begin
      if i = CurrentCell then
        m := MarkupInfoCurrent
      else
      if (CurrentCell >= 0) and (Cells[i].Group = Cells[CurrentCell].Group) then
        m := MarkupInfoSync
      else
        m := MarkupInfo;
      m.StartX := Cells[i].LogStart.x;
      if Cells[i].LogStart.y < aRow then
        m.StartX := -1;
      m.EndX := Cells[i].LogEnd.x - 1;
      if Cells[i].LogEnd.y > aRow then
        m.EndX := -1;
      exit(m);
    end;
  end;
end;

function TSynPluginSyncEditMarkup.GetNextMarkupColAfterRowCol(const aRow,
  aCol: Integer): Integer;
var
  i: Integer;
begin
  Result := -1;
  for i := 0 to Cells.Count -1 do begin
    if (Cells[i].LogStart.y = aRow) and (Cells[i].LogStart.x > aCol) and
       ( (Cells[i].LogStart.x < Result) or (Result < 0) )
    then
      Result := Cells[i].LogStart.x;
    if (Cells[i].LogEnd.y = aRow) and (Cells[i].LogEnd.x > aCol) and
       ( (Cells[i].LogEnd.x < Result) or (Result < 0) )
    then
      Result := Cells[i].LogEnd.x;
  end
end;

{ TSynPluginSyncEditBase }

constructor TSynPluginSyncEditBase.Create(AOwner: TComponent);
begin
  FCells := TSynPluginSyncEditList.Create;
  CurrentCell := -1;
  inherited Create(AOwner);
  FEnabled := True;
  Active := False;
  FEditing := False;
end;

destructor TSynPluginSyncEditBase.Destroy;
begin
  Editor := nil;
  FreeAndNil(FMarkup);
  FreeAndNil(FCells);
  inherited;
end;

procedure TSynPluginSyncEditBase.Clear;
begin
  FCells.Clear;
  CurrentCell := -1;
  Active := False;
end;

procedure TSynPluginSyncEditBase.SetEditor(const AValue: TCustomSynEdit);
begin
  if AValue = Editor then exit;
  Active := False;
  if Editor <> nil then begin
    ViewedTextBuffer.RemoveEditHandler(@DoLinesEdited);
    if FMarkup <> nil then begin
      TSynEditMarkupManager(MarkupMgr).RemoveMarkUp(FMarkup);
      FreeAndNil(FMarkup);
    end;
  end;
  inherited SetEditor(AValue);
  if AValue <> nil then begin
    FMarkup := TSynPluginSyncEditMarkup.Create(Editor);
    FMarkup.Cells := FCells;
    FMarkup.CurrentCell := FCurrentCell;
    FMarkup.Enabled := Active;
    TSynEditMarkupManager(MarkupMgr).AddMarkUp(FMarkup);
    ViewedTextBuffer.AddEditHandler(@DoLinesEdited);
  end;
end;

procedure TSynPluginSyncEditBase.SetCurrentCell(const AValue: Integer);
begin
  if FCurrentCell = AValue then exit;
  FCurrentCell := AValue;
  if FMarkup <> nil then
    FMarkup.CurrentCell := FCurrentCell;
end;

procedure TSynPluginSyncEditBase.SetEnabled(const AValue: Boolean);
var
  IsActive: Boolean;
begin
  IsActive := Active;
  FEnabled := AValue;
  if FMarkup <> nil then
    FMarkup.Enabled := Active;
  if IsActive <> Active then begin
    if Active
    then DoOnActivate
    else DoOnDeactivate;
  end;
end;

function TSynPluginSyncEditBase.GetActive: Boolean;
begin
  Result := FActive and FEnabled and (Editor <> nil);
end;

procedure TSynPluginSyncEditBase.SetActive(const AValue: Boolean);
var
  IsActive: Boolean;
begin
  IsActive := Active;
  FActive := AValue;
  if FMarkup <> nil then
    FMarkup.Enabled := Active;
  if IsActive <> Active then begin
    if Active
    then DoOnActivate
    else DoOnDeactivate;
  end;
end;

procedure TSynPluginSyncEditBase.DoLinesEdited(Sender: TSynEditStrings;
  aLinePos, aBytePos, aCount, aLineBrkCnt: Integer; aText: String);
var
  Pos, Pos2: TPoint;

  function AdjustPoint(aPoint: Tpoint): TPoint;
  begin
    Result := aPoint;
    if aLineBrkCnt < 0 then begin
      (* Lines Deleted *)
      if aPoint.y > aLinePos then
        Result.y := Max(aLinePos, Result.y + aLineBrkCnt);
      if Result.y = aLinePos then
        Result.x := Result.x + Pos.x - 1;
    end
    else
    if aLineBrkCnt > 0 then begin
      (* Lines Inserted *)
      if aPoint.y = aLinePos then
        Result.x := Result.x - Pos.x + 1;
      if aPoint.y >= aLinePos then
        Result.y := Result.y + aLineBrkCnt;
    end
    else
    if aCount <> 0 then begin
      (* Chars Insert/Deleted *)
      if (aPoint.y = aLinePos) and (aPoint.x >= Pos.x) then
        Result.x := Max(Pos.x, Result.x + aCount);
    end;
  end;

var
  i, a: Integer;
  CurCell: TSynPluginSyncEditCell;
  Y2, X2: Integer;
begin
  if (not Active) or (FCells.Count = 0) then exit;
  Pos := ViewedTextBuffer.LogicalToPhysicalPos(Point(aBytePos, aLinePos));
  Pos2 := Pos;
  if Not FEditing then
    DoBeforeEdit(Pos.x, Pos.y);
  for i := 0 to FCells.Count - 1 do begin
    CurCell := Cells[i];
    a := CompareCarets(Pos, CurCell.LogStart);
    if (a > 0) then
      CurCell.LogStart := AdjustPoint(CurCell.LogStart);
    a := CompareCarets(Pos, CurCell.LogEnd);
    if (a > 0) or ((a = 0) and ((i = FCurrentCell) or FEditing)) then
      CurCell.LogEnd := AdjustPoint(CurCell.LogEnd);
    Cells[i] := CurCell;
  end;

  if (not (FEditing or IsUndoing or IsRedoing)) and
     (FCurrentCell >= 0) and (FCurrentCell < Cells.Count) and
     (CompareCarets(Pos, FCells[FCurrentCell].LogStart) <= 0) and
     (CompareCarets(Pos, FCells[FCurrentCell].LogEnd) >= 0)
  then begin
    FEditing := True;
    CurCell := FCells[FCurrentCell];
    a := CurCell.Group;
    Pos.Y := Pos.Y - CurCell.LogStart.y;
    if Pos.y = 0 then
      Pos.X := Pos.X - CurCell.LogStart.x;
    for i := 0 to FCells.Count - 1 do
      if (i <> FCurrentCell) and (FCells[i].Group = a) and
         ( (FCells[i].LogStart.Y + Pos.Y < FCells[i].LogEnd.Y) or
           ((FCells[i].LogStart.Y + Pos.Y = FCells[i].LogEnd.Y) and
            (FCells[i].LogStart.X + Pos.X <= FCells[i].LogEnd.X))
         )
      then begin
        Y2 := FCells[i].LogStart.Y + Pos.Y;
        X2 := Pos.X;
        if Pos.Y = 0 then
          X2 := X2 + FCells[i].LogStart.X;
        if aLineBrkCnt = -1 then begin
          ViewedTextBuffer.EditLineJoin(Y2);
        end
        else if aLineBrkCnt < -1 then begin
          ViewedTextBuffer.EditLinesDelete(Y2, -aLineBrkCnt);
        end
        else if aLineBrkCnt = 1 then begin
          ViewedTextBuffer.EditLineBreak(X2, Y2);
        end
        else if aLineBrkCnt > 1 then begin
          ViewedTextBuffer.EditLinesInsert(Y2, aLineBrkCnt);
        end
        else if aCount < 0 then begin
          ViewedTextBuffer.EditDelete(X2, Y2, -aCount);
        end
        else if aCount > 0 then begin
          ViewedTextBuffer.EditInsert(X2, Y2, aText);
        end;
      end;
    FEditing := False;
    if Pos.y = 0 then
      pos2.x := pos.x + CurCell.LogStart.x;
    Pos2.y := Pos.y + CurCell.LogStart.y;
  end;
  if Not FEditing then
    DoAfterEdit(Pos2.x, Pos2.y);
end;

procedure TSynPluginSyncEditBase.DoBeforeEdit(aX, aY: Integer);
begin
  (* Do Nothing *);
end;

procedure TSynPluginSyncEditBase.DoAfterEdit(aX, aY: Integer);
begin
  (* Do Nothing *);
end;

procedure TSynPluginSyncEditBase.DoOnActivate;
begin
  (* Do Nothing *);
end;

procedure TSynPluginSyncEditBase.DoOnDeactivate;
begin
  (* Do Nothing *);
end;

{ TSynPluginSyncEditCell }

procedure TSynPluginSyncEditCell.Assign(Src: TSynPluginSyncEditCell);
begin
  if Src = nil then exit;
  FLogStart := Src.FLogStart;
  FLogEnd := Src.FLogEnd;
  FGroup := Src.FGroup;
end;

end.

