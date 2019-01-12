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
  Classes, SysUtils, Graphics, LCLProc,
  SynEditMiscClasses, SynEdit, SynEditMarkup, SynEditMiscProcs, LazSynEditText,
  SynEditTextTrimmer, SynEditKeyCmds;

type

  { TSynPluginSyncronizedEditCell }

  TSynPluginSyncronizedEditCell = class
  private
    FFirstInGroup: Boolean;
    FLogStart, FLogEnd: TPoint;
    FGroup: Integer;
  public
    constructor Create;
    procedure Assign(Src: TSynPluginSyncronizedEditCell); reintroduce;

    property LogStart: TPoint read FLogStart write FLogStart;
    property LogEnd: TPoint read FLogEnd write FLogEnd;
    property Group: Integer read FGroup write FGroup;
    property FirstInGroup: Boolean read FFirstInGroup write FFirstInGroup;
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
    procedure DoInvalidate;
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
    function GetMarkupAttributeAtRowCol(const aRow: Integer;
                                        const aStartCol: TLazSynDisplayTokenBound;
                                        const AnRtlInfo: TLazSynDisplayRtlInfo): TSynSelectedColor; override;
    procedure GetNextMarkupColAfterRowCol(const aRow: Integer;
                                         const aStartCol: TLazSynDisplayTokenBound;
                                         const AnRtlInfo: TLazSynDisplayRtlInfo;
                                         out   ANextPhys, ANextLog: Integer); override;
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
    function GetMarkupAttributeAtRowCol(const aRow: Integer;
                                        const aStartCol: TLazSynDisplayTokenBound;
                                        const AnRtlInfo: TLazSynDisplayRtlInfo): TSynSelectedColor; override;
    procedure GetNextMarkupColAfterRowCol(const aRow: Integer;
                                         const aStartCol: TLazSynDisplayTokenBound;
                                         const AnRtlInfo: TLazSynDisplayRtlInfo;
                                         out   ANextPhys, ANextLog: Integer); override;

    property CellGroupForArea: Integer read FCellIdForArea write FCellIdForArea;
  end;

  { TSynPluginSyncronizedEditChangeAction }

  TSynPluginSyncronizedEditChangeAction = record
    CellIndex: Integer;
    cLinePos, cBytePos, Count, LineBrkCount: Integer;
    Text: String;
  end;

  { TSynPluginSyncronizedEditChangeList }

  TSynPluginSyncronizedEditChangeList = class
  private
    FList: array of TSynPluginSyncronizedEditChangeAction;
    FCount: Integer;
    function GetItems(Index: Integer): TSynPluginSyncronizedEditChangeAction;
  public
    procedure Clear;
    procedure Add(aCellIndex, aLinePos, aBytePos, aCount, aLineBrkCnt: Integer;
      aText: String);
    property Count: Integer read FCount;
    property Items[Index: Integer]: TSynPluginSyncronizedEditChangeAction
             read GetItems; default;
  end;

  { TSynPluginSyncronizedEditBase }

  TSynPluginSyncronizedEditBase = class(TLazSynEditPlugin)
  private
    FActive: Boolean;
    FCells: TSynPluginSyncronizedEditList;
    FCurrentCell: Integer;
    FChangeList: TSynPluginSyncronizedEditChangeList;
    FAreaMarkupEnabled: Boolean;
    FMarkupEnabled: Boolean;
    FEnabled: Boolean;
    FEditing: Boolean;  // In ApplyChangeList, edit actions are caused by the plugin itself
    FPaintLock: Integer;
    FOwnPaintLock: Integer;
    FTextBufferChanging: Boolean;

    fMarkupInfo: TSynSelectedColor;
    fMarkupInfoSync: TSynSelectedColor;
    fMarkupInfoCurrent: TSynSelectedColor;
    fMarkupInfoArea: TSynSelectedColor;
    FOnActivate: TNotifyEvent;
    FOnDeactivate: TNotifyEvent;
    FPreActive: Boolean;

    function GetActive: Boolean;
    procedure SetActive(const AValue: Boolean);
    procedure SetCurrentCell(const AValue: Integer);
    procedure SetAreaMarkupEnabled(const AValue: Boolean);
    procedure SetEnabled(const AValue: Boolean);
    procedure SetMarkupEnabled(const AValue: Boolean);
    procedure SetMarkupInfo(AValue: TSynSelectedColor);
    procedure SetMarkupInfoArea(AValue: TSynSelectedColor);
    procedure SetMarkupInfoCurrent(AValue: TSynSelectedColor);
    procedure SetMarkupInfoSync(AValue: TSynSelectedColor);
  protected
    FMarkup: TSynPluginSyncronizedEditMarkup;
    FMarkupArea: TSynPluginSyncronizedEditMarkupArea;
    procedure MarkupChanged(AMarkup: TObject);
    function  CreateMarkup: TSynPluginSyncronizedEditMarkup; virtual;
    procedure DoEditorRemoving(AValue: TCustomSynEdit); override;
    procedure DoEditorAdded(AValue: TCustomSynEdit); override;
    procedure DoBufferChanging(Sender: TObject);
    procedure DoBufferChanged(Sender: TObject);
    procedure DoLinesEdited(Sender: TSynEditStrings; aLinePos, aBytePos, aCount,
                            aLineBrkCnt: Integer; aText: String);
    procedure ApplyChangeList;
    procedure DoPreActiveEdit(aX, aY, aCount, aLineBrkCnt: Integer; aUndoRedo: Boolean); virtual;
    procedure DoBeforeEdit(aX, aY, aCount, aLineBrkCnt: Integer; aUndoRedo: Boolean); virtual;
    procedure DoAfterEdit(aX, aY: Integer; aUndoRedo: Boolean); virtual;
    procedure DoPaintLockStarted; virtual;
    procedure DoPaintLockEnded; virtual;
    procedure DoClear; virtual;
    procedure DoOnActivate; virtual;
    procedure DoOnDeactivate; virtual;
    procedure DoIncPaintLock(Sender: TObject);
    procedure DoDecPaintLock(Sender: TObject);
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
    property PreActive: Boolean read FPreActive write FPreActive;           // e.g. collecting words => Reacts to LinesEdited

    property MarkupInfo: TSynSelectedColor read FMarkupInfo write SetMarkupInfo;
    property MarkupInfoCurrent: TSynSelectedColor read FMarkupInfoCurrent write SetMarkupInfoCurrent;
    property MarkupInfoSync: TSynSelectedColor read FMarkupInfoSync write SetMarkupInfoSync;
    property MarkupInfoArea: TSynSelectedColor read FMarkupInfoArea write SetMarkupInfoArea;
    property OnActivate: TNotifyEvent read FOnActivate write FOnActivate;
    property OnDeactivate: TNotifyEvent read FOnDeactivate write FOnDeactivate;
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
    FExternalEditLock: Integer;
  protected
    procedure SetUndoStart; // Handle undo/redo stuff
    procedure DoEditorRemoving(AValue: TCustomSynEdit); override;
    procedure DoEditorAdded(AValue: TCustomSynEdit); override;
    procedure DoOnActivate; override;
    procedure DoOnDeactivate; override;
    procedure DoBeforeEdit(aX, aY, aCount, aLineBrkCnt: Integer; aUndoRedo: Boolean); override;
    procedure DoAfterEdit(aX, aY: Integer; aUndoRedo: Boolean); override;
    procedure DoPaintLockStarted; override;
    procedure DoPaintLockEnded; override;
    procedure UpdateCurrentCell;
    procedure DoCaretChanged(Sender: TObject);
    property LastCell: Integer read FLastCell;
  protected
    procedure SelectCurrentCell(Reverse: Boolean = False);
    procedure PreviousCell(SetSelect: Boolean = True; SkipSameIndex: Boolean = False; FirstsOnly: Boolean = False);
    procedure NextCell(SetSelect: Boolean = True; SkipSameIndex: Boolean = False; FirstsOnly: Boolean = False);
    procedure CellCaretHome;
    procedure CellCaretEnd;
  public
    constructor Create(AOwner: TComponent); override;
    //destructor Destroy; override;
    procedure IncExternalEditLock;
    procedure DecExternalEditLock;
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

procedure TSynPluginSyncronizedEditMarkupBase.DoInvalidate;
begin
  DoEnabledChanged(nil);
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

function TSynPluginSyncronizedEditMarkup.GetMarkupAttributeAtRowCol(const aRow: Integer;
  const aStartCol: TLazSynDisplayTokenBound; const AnRtlInfo: TLazSynDisplayRtlInfo): TSynSelectedColor;
var
  i: Integer;
  s, e: Integer;
begin
  Result := nil;
  if not RealEnabled then exit;
  for i := FPreparedCellFrom to FPreparedCellTo do begin
    if ( ((Cells[i].LogStart.y = aRow) and (Cells[i].LogStart.x <= aStartCol.Logical)) or
         (Cells[i].LogStart.y < aRow) ) and
       ( ((Cells[i].LogEnd.y = aRow) and (Cells[i].LogEnd.x > aStartCol.Logical)) or
         (Cells[i].LogEnd.y > aRow) ) and
       (Cells[i].Group >= 0) // do not't display negative groups
    then begin
      if i = CurrentCell then
        Result := MarkupInfoCurrent
      else
      if (CurrentCell >= 0) and (Cells[i].Group = Cells[CurrentCell].Group) then
        Result := MarkupInfoSync
      else
        Result := MarkupInfo;

      s := Cells[i].LogStart.x;
      if Cells[i].LogStart.y < aRow then
        s := -1;
      e := Cells[i].LogEnd.x;
      if Cells[i].LogEnd.y > aRow then
        e := -1;
      Result.SetFrameBoundsLog(s, e);
      break;
    end;
  end;
end;

procedure TSynPluginSyncronizedEditMarkup.GetNextMarkupColAfterRowCol(const aRow: Integer;
  const aStartCol: TLazSynDisplayTokenBound; const AnRtlInfo: TLazSynDisplayRtlInfo; out ANextPhys,
  ANextLog: Integer);
var
  i: Integer;
begin
  ANextLog := -1;
  ANextPhys := -1;
  if not RealEnabled then exit;
  for i := FPreparedCellFrom to FPreparedCellTo do begin
    if Cells[i].Group < 0 then continue;
    if (Cells[i].LogStart.y = aRow) and (Cells[i].LogStart.x > aStartCol.Logical) and
       ( (Cells[i].LogStart.x < ANextLog) or (ANextLog < 0) )
    then
      ANextLog := Cells[i].LogStart.x;
    if (Cells[i].LogEnd.y = aRow) and (Cells[i].LogEnd.x > aStartCol.Logical) and
       ( (Cells[i].LogEnd.x < ANextLog) or (ANextLog < 0) )
    then
      ANextLog := Cells[i].LogEnd.x;
  end;
end;

procedure TSynPluginSyncronizedEditMarkup.PrepareMarkupForRow(aRow: Integer);
var
  i, j, t, b: Integer;
begin
  if not RealEnabled then exit;
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
  if not RealEnabled then exit;
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

function TSynPluginSyncronizedEditMarkupArea.GetMarkupAttributeAtRowCol(const aRow: Integer;
  const aStartCol: TLazSynDisplayTokenBound; const AnRtlInfo: TLazSynDisplayRtlInfo): TSynSelectedColor;
var
  ac: TSynPluginSyncronizedEditCell;
begin
  Result := nil;
  if MarkupInfo.IsEnabled then begin
    ac := Cells.GroupCell[CellGroupForArea, 0];
    if (ac <> nil) and
       ( ((ac.LogStart.y = aRow) and (ac.LogStart.x <= aStartCol.Logical)) or (ac.LogStart.y < aRow)) and
       ( ((ac.LogEnd.y = aRow) and (ac.LogEnd.x > aStartCol.Logical)) or (ac.LogEnd.y > aRow))
    then
      Result := MarkupInfo;
  end;
end;

procedure TSynPluginSyncronizedEditMarkupArea.GetNextMarkupColAfterRowCol(const aRow: Integer;
  const aStartCol: TLazSynDisplayTokenBound; const AnRtlInfo: TLazSynDisplayRtlInfo; out ANextPhys,
  ANextLog: Integer);
var
  ac: TSynPluginSyncronizedEditCell;
begin
  ANextLog := -1;
  ANextPhys := -1;
  if MarkupInfo.IsEnabled then begin
    ac := Cells.GroupCell[CellGroupForArea, 0];
    if ac <> nil then begin
      if (ac.LogStart.y = aRow) and (ac.LogStart.x > aStartCol.Logical) and
         ( (ac.LogStart.x < ANextLog) or (ANextLog < 0) )
      then
        ANextLog := ac.LogStart.x;
      if (ac.LogEnd.y = aRow) and (ac.LogEnd.x > aStartCol.Logical) and
         ( (ac.LogEnd.x < ANextLog) or (ANextLog < 0) )
      then
        ANextLog := ac.LogEnd.x;
    end;
  end;
end;

{ TSynPluginSyncronizedEditChangeList }

function TSynPluginSyncronizedEditChangeList.GetItems(Index: Integer): TSynPluginSyncronizedEditChangeAction;
begin
  Result := FList[Index];
end;

procedure TSynPluginSyncronizedEditChangeList.Clear;
begin
  FList := nil;
  FCount := 0;
end;

procedure TSynPluginSyncronizedEditChangeList.Add(aCellIndex, aLinePos, aBytePos,
  aCount, aLineBrkCnt: Integer; aText: String);
begin
  if length(FList) <= FCount then
    SetLength(FList, FCount + 4);

  FList[FCount].CellIndex := aCellIndex;
  FList[FCount].cLinePos := aLinePos;
  FList[FCount].cBytePos := aBytePos;
  FList[FCount].Count := aCount;
  FList[FCount].LineBrkCount := aLineBrkCnt;
  FList[FCount].Text := aText;
  inc(FCount);
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
  FChangeList := TSynPluginSyncronizedEditChangeList.Create;
  AreaMarkupEnabled := False;
  MarkupEnabled := True;
  inherited Create(AOwner);
  FEnabled := True;
  Active := False;
  FEditing := False;
  FTextBufferChanging := False;
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
  FreeAndNil(FChangeList);
  inherited;
end;

procedure TSynPluginSyncronizedEditBase.Clear;
begin
  if FMarkup <> nil then
    FMarkup.DoEnabledChanged(nil); // invalidate lines
  FCells.Clear;
  CurrentCell := -1;
  Active := False;
  DoClear;
end;

procedure TSynPluginSyncronizedEditBase.DoEditorRemoving(AValue: TCustomSynEdit);
begin
  Active := False;
  if Editor <> nil then begin
    if not FTextBufferChanging then begin
      ViewedTextBuffer.RemoveNotifyHandler(senrTextBufferChanging, @DoBufferChanging);
      ViewedTextBuffer.RemoveNotifyHandler(senrTextBufferChanged, @DoBufferChanged);
    end;
    ViewedTextBuffer.RemoveEditHandler(@DoLinesEdited);
    ViewedTextBuffer.RemoveNotifyHandler(senrAfterIncPaintLock, @DoIncPaintLock);
    ViewedTextBuffer.RemoveNotifyHandler(senrBeforeDecPaintLock, @DoDecPaintLock);
    if FMarkup <> nil then begin
      TSynEditMarkupManager(MarkupMgr).RemoveMarkUp(FMarkup);
      FreeAndNil(FMarkup);
    end;
    if FMarkupArea <> nil then begin
      TSynEditMarkupManager(MarkupMgr).RemoveMarkUp(FMarkupArea);
      FreeAndNil(FMarkupArea);
    end;
  end;
  inherited DoEditorRemoving(AValue);
end;

procedure TSynPluginSyncronizedEditBase.DoEditorAdded(AValue: TCustomSynEdit);
begin
  inherited DoEditorAdded(AValue);
  if Editor <> nil then begin
    FMarkup := CreateMarkup;
    FMarkup.Cells := FCells;
    FMarkup.CurrentCell := FCurrentCell;
    FMarkup.Enabled := (Active or PreActive) and FMarkupEnabled;
    TSynEditMarkupManager(MarkupMgr).AddMarkUp(FMarkup);
    FMarkupArea := TSynPluginSyncronizedEditMarkupArea.Create(Editor);
    FMarkupArea.Cells := FCells;
    FMarkupArea.Enabled := Active;
    TSynEditMarkupManager(MarkupMgr).AddMarkUp(FMarkupArea, True);
    MarkupChanged(nil);
    ViewedTextBuffer.AddEditHandler(@DoLinesEdited);
    ViewedTextBuffer.AddNotifyHandler(senrAfterIncPaintLock, @DoIncPaintLock);
    ViewedTextBuffer.AddNotifyHandler(senrBeforeDecPaintLock, @DoDecPaintLock);
    if not FTextBufferChanging then begin
      ViewedTextBuffer.AddNotifyHandler(senrTextBufferChanging, @DoBufferChanging);
      ViewedTextBuffer.AddNotifyHandler(senrTextBufferChanged, @DoBufferChanged);
    end;
  end;
end;

procedure TSynPluginSyncronizedEditBase.DoBufferChanging(Sender: TObject);
begin
  if FTextBufferChanging then
    exit;
  FTextBufferChanging := True;
  if Editor <> nil then
    DoEditorRemoving(Editor);
end;

procedure TSynPluginSyncronizedEditBase.DoBufferChanged(Sender: TObject);
begin
  if not FTextBufferChanging then
    exit;
  if Editor <> nil then
    DoEditorAdded(Editor);
  FTextBufferChanging := False;
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
    FMarkup.Enabled := (Active or PreActive) and FMarkupEnabled;
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
    FMarkup.Enabled := (Active or PreActive) and FMarkupEnabled;
end;

procedure TSynPluginSyncronizedEditBase.SetMarkupInfo(AValue: TSynSelectedColor
  );
begin
  if FMarkupInfo=AValue then Exit;
  FMarkupInfo.Assign(AValue);
end;

procedure TSynPluginSyncronizedEditBase.SetMarkupInfoArea(
  AValue: TSynSelectedColor);
begin
  if FMarkupInfoArea=AValue then Exit;
  FMarkupInfoArea.Assign(AValue);
end;

procedure TSynPluginSyncronizedEditBase.SetMarkupInfoCurrent(
  AValue: TSynSelectedColor);
begin
  if FMarkupInfoCurrent=AValue then Exit;
  FMarkupInfoCurrent.Assign(AValue);
end;

procedure TSynPluginSyncronizedEditBase.SetMarkupInfoSync(
  AValue: TSynSelectedColor);
begin
  if FMarkupInfoSync=AValue then Exit;
  FMarkupInfoSync.Assign(AValue);
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
  PreActive := False; // change in active, always disables PreActive
  if FMarkup <> nil then
    FMarkup.Enabled := (Active or PreActive) and FMarkupEnabled;
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
        if (aPoint.y = aLinePos) and (aPoint.x >= Pos.x) then
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
  chg: Boolean;
  edit: Boolean;
  CellAtPos: Integer;
begin
  if not Active then begin
    if PreActive then DoPreActiveEdit(aBytePos, aLinePos, aCount, aLineBrkCnt, IsUndoing or IsRedoing);
    exit;
  end;
  Pos := Point(aBytePos, aLinePos);
  Pos2 := Pos;
  if not FEditing then
    DoBeforeEdit(Pos.x, Pos.y, aCount, aLineBrkCnt, IsUndoing or IsRedoing);
  CellAtPos := Cells.IndexOf(Pos.x, Pos.y, True);

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
    if (a > 0) or ((a = 0) and ((i = FCurrentCell) or (CurCell.Group = -1) or edit)) then
      CurCell.LogEnd := AdjustPoint(CurCell.LogEnd, chg);
    if chg then
      Cells[i] := CurCell;
  end;

  if (not (FEditing or IsUndoing or IsRedoing)) and
     (CellAtPos >= 0) and (CellAtPos < Cells.Count) and
     (CompareCarets(Pos, FCells[CellAtPos].LogStart) <= 0) and
     (CompareCarets(Pos, FCells[CellAtPos].LogEnd) >= 0)
  then begin
    CurCell := FCells[CellAtPos];
    Pos.Y := Pos.Y - CurCell.LogStart.y;
    if Pos.y = 0 then
      Pos.X := Pos.X - CurCell.LogStart.x
    else
      dec(Pos.x);
    FChangeList.Add(CellAtPos, Pos.Y, Pos.X, aCount, aLineBrkCnt, aText);
  end;

  if not FEditing then
    DoAfterEdit(Pos2.x, Pos2.y, IsUndoing or IsRedoing);
  if (not FEditing) and (FPaintLock = 0) then
    DoPaintLockEnded;
end;

procedure TSynPluginSyncronizedEditBase.ApplyChangeList;
var
  Action: TSynPluginSyncronizedEditChangeAction;
  a, i: Integer;
  Group, Y2, X2: Integer;
  Cell: TSynPluginSyncronizedEditCell;
begin
  if FChangeList.Count = 0 then
    exit;
  FEditing := True;
  ViewedTextBuffer.BeginUpdate;
  CaretObj.IncAutoMoveOnEdit;
  try
    for a := 0 to FChangeList.Count - 1 do begin
      Action := FChangeList[a];
      Group := FCells[Action.CellIndex].Group;
      for i := 0 to FCells.Count - 1 do begin
        Cell := FCells[i];
        if (i = Action.CellIndex) or (Cell.Group <> Group) then
          continue;

        if Cell.LogStart.Y = Cell.LogEnd.Y then
          X2 := Cell.LogStart.X + Action.cBytePos
        else
          X2 := 1 + Action.cBytePos;
        if (Cell.LogStart.Y + Action.cLinePos < Cell.LogEnd.Y) or
           ( (Cell.LogStart.Y + Action.cLinePos = Cell.LogEnd.Y) and
             (X2 <= Cell.LogEnd.X) )
        then begin
          Y2 := Cell.LogStart.Y + Action.cLinePos;
          if Action.cLinePos = 0 then
            X2 := Cell.LogStart.X + Action.cBytePos
          else
            X2 := 1 + Action.cBytePos;

          if Action.LineBrkCount = -1 then
            ViewedTextBuffer.EditLineJoin(Y2)
          else
          if Action.LineBrkCount < -1 then
            ViewedTextBuffer.EditLinesDelete(Y2, -Action.LineBrkCount)
          else
          if Action.LineBrkCount = 1 then
            ViewedTextBuffer.EditLineBreak(X2, Y2)
          else
          if Action.LineBrkCount > 1 then
            ViewedTextBuffer.EditLinesInsert(Y2, Action.LineBrkCount)
          else
          if Action.Count < 0 then
            ViewedTextBuffer.EditDelete(X2, Y2, -Action.Count)
          else
          if Action.Count > 0 then
            ViewedTextBuffer.EditInsert(X2, Y2, Action.Text);
        end;
      end;
    end;
  finally
    FEditing := False;
    CaretObj.DecAutoMoveOnEdit;
    ViewedTextBuffer.EndUpdate;
  end;
  FChangeList.Clear;
end;

procedure TSynPluginSyncronizedEditBase.DoPreActiveEdit(aX, aY, aCount, aLineBrkCnt: Integer;
  aUndoRedo: Boolean);
begin
  //
end;

procedure TSynPluginSyncronizedEditBase.DoBeforeEdit(aX, aY, aCount, aLineBrkCnt: Integer; aUndoRedo: Boolean);
begin
  (* Do Nothing *);
end;

procedure TSynPluginSyncronizedEditBase.DoAfterEdit(aX, aY: Integer; aUndoRedo: Boolean);
begin
  (* Do Nothing *);
end;

procedure TSynPluginSyncronizedEditBase.DoPaintLockStarted;
begin
  (* Do Nothing *);
end;

procedure TSynPluginSyncronizedEditBase.DoPaintLockEnded;
begin
  (* Do Nothing *);
end;

procedure TSynPluginSyncronizedEditBase.DoClear;
begin
  (* Do Nothing *);
end;

procedure TSynPluginSyncronizedEditBase.DoOnActivate;
begin
  if assigned(FOnActivate) then
    FOnActivate(self);
end;

procedure TSynPluginSyncronizedEditBase.DoOnDeactivate;
begin
  if assigned(FOnDeactivate) then
    FOnDeactivate(self);
end;

procedure TSynPluginSyncronizedEditBase.DoIncPaintLock(Sender: TObject);
begin
  if FPaintLock = 0 then
    DoPaintLockStarted;
  inc(FPaintLock);
  if Sender = Editor then
    inc(FOwnPaintLock);
end;

procedure TSynPluginSyncronizedEditBase.DoDecPaintLock(Sender: TObject);
begin
  dec(FPaintLock);
  if Sender = Editor then
    dec(FOwnPaintLock);
  if FPaintLock = 0 then
    DoPaintLockEnded;
end;

{ TSynPluginSyncronizedEditCell }

constructor TSynPluginSyncronizedEditCell.Create;
begin
  FFirstInGroup := False;
end;

procedure TSynPluginSyncronizedEditCell.Assign(Src: TSynPluginSyncronizedEditCell);
begin
  if Src = nil then exit;
  FLogStart := Src.FLogStart;
  FLogEnd := Src.FLogEnd;
  FGroup := Src.FGroup;
  FFirstInGroup := Src.FFirstInGroup;
end;

{ TSynPluginCustomSyncroEdit }

procedure TSynPluginCustomSyncroEdit.SetUndoStart;
begin
  ViewedTextBuffer.UndoList.ForceGroupEnd;
  FUndoRealCount := ViewedTextBuffer.UndoList.RealCount;
  FRedoRealCount := ViewedTextBuffer.RedoList.RealCount;
end;

procedure TSynPluginCustomSyncroEdit.DoEditorRemoving(AValue: TCustomSynEdit);
begin
  if Editor <> nil then begin
    CaretObj.RemoveChangeHandler(@DoCaretChanged);
  end;
  inherited DoEditorRemoving(AValue);
end;

procedure TSynPluginCustomSyncroEdit.DoEditorAdded(AValue: TCustomSynEdit);
begin
  inherited DoEditorAdded(AValue);
  if Editor <> nil then begin
    CaretObj.AddChangeHandler(@DoCaretChanged);
  end;
end;

procedure TSynPluginCustomSyncroEdit.DoOnActivate;
var
  b: TSynEditStrings;
begin
  inherited;
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
  inherited;
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

procedure TSynPluginCustomSyncroEdit.DoBeforeEdit(aX, aY, aCount, aLineBrkCnt: Integer;
  aUndoRedo: Boolean);
var
  c1, c2: Integer;
begin
  inherited;
  if IsUndoing and (FUndoRealCount >= 0) and (ViewedTextBuffer.UndoList.RealCount < FUndoRealCount)
  then
    Active := false;
  if IsRedoing and (FRedoRealCount >= 0) and (ViewedTextBuffer.RedoList.RealCount < FUndoRealCount)
  then
    Active := false;
  if aUndoRedo or not Active then exit;
  FRedoRealCount := -1;
  (* TODO / Review
     - Caret may be outside Cell (eg IdentifierCompletion, TextBetweenPoints)
       But the edit happens inside a cell => ok
     - Caret may be in cell, but Codetools inserts text outside the cell => ok
     - User edit outside a cell (both locations will be outside the cell => deactivate
     TODO: Hook SynEdits Lock, and check Caret before locking only
  *)
  c1 := Cells.IndexOf(aX, aY, True);
  if aLineBrkCnt < 0 then
    c2 := Cells.IndexOf(1, aY-aLineBrkCnt, True)
  else if aCount < 0 then
    c2 := Cells.IndexOf(aX - aCount, aY, True)
  else
    c2 := c1;
  // allow edit outside cell? (only if not partly cell / part cell updates are not allowed at all)
  // Todo, could be just on the edge of a cell !
  if (c1 = c2) and (FExternalEditLock > 0) then begin
    exit;
  end;
  // shared editor, outside cells
  if (FPaintLock > 0) and (FOwnPaintLock = 0) then begin
    if (c1 < 0) and (c2 < 0) then
      exit;
    c1 := -1; // shared Eitor in cell => deactivate
  end;

  if (CurrentCell < 0) or (c1 < 0) or (c2 <> c1) then begin
    Clear;
    Active := False;
  end;
end;

procedure TSynPluginCustomSyncroEdit.DoAfterEdit(aX, aY: Integer; aUndoRedo: Boolean);
begin
  inherited DoAfterEdit(aX, aY, aUndoRedo);
  if FPaintLock = 0 then
    UpdateCurrentCell;
end;

procedure TSynPluginCustomSyncroEdit.DoPaintLockStarted;
begin
  inherited DoPaintLockStarted;
end;

procedure TSynPluginCustomSyncroEdit.DoPaintLockEnded;
begin
  inherited DoPaintLockEnded;
  if Active then begin
    ApplyChangeList;
    UpdateCurrentCell;
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

procedure TSynPluginCustomSyncroEdit.PreviousCell(SetSelect: Boolean; SkipSameIndex: Boolean;
  FirstsOnly: Boolean);
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
        ( (Cells[i].Group >= 0) and
          ((not SkipSameIndex) or (Cells[i].Group <> x)) and
          ((not FirstsOnly) or (Cells[i].FirstInGroup))
        );
  CurrentCell := i;

  if CurrentCell < 0 then
    exit;
  CaretObj.LineBytePos := Cells[CurrentCell].LogEnd;
  if SetSelect then
    SelectCurrentCell
  else
    Editor.BlockBegin := Cells[CurrentCell].LogEnd;
end;

procedure TSynPluginCustomSyncroEdit.NextCell(SetSelect: Boolean; SkipSameIndex: Boolean;
  FirstsOnly: Boolean);
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
        ( (Cells[i].Group >= 0) and
          ((not SkipSameIndex) or (Cells[i].Group <> x)) and
          ((not FirstsOnly) or (Cells[i].FirstInGroup))
        );
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

constructor TSynPluginCustomSyncroEdit.Create(AOwner: TComponent);
begin
  FPaintLock := 0;
  FExternalEditLock := 0;
  inherited Create(AOwner);
end;

procedure TSynPluginCustomSyncroEdit.IncExternalEditLock;
begin
  inc(FExternalEditLock);
end;

procedure TSynPluginCustomSyncroEdit.DecExternalEditLock;
begin
  dec(FExternalEditLock);
end;

end.

