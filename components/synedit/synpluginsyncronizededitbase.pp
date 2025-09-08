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
  Classes, SysUtils, Graphics, StrUtils,
  SynEditMiscClasses, SynEdit, SynEditMarkup, SynEditMiscProcs, LazSynEditText,
  SynEditTextTrimmer, SynEditKeyCmds, SynEditTextBase, LazUTF8, LazEditMiscProcs;

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
    function Add(aCell: TSynPluginSyncronizedEditCell; AnAddSorted: Boolean = False): Integer;
    function AddNew: TSynPluginSyncronizedEditCell; virtual; // only add sorted
    procedure Delete(aIndex: Integer);
    function IndexOf(aCell: TSynPluginSyncronizedEditCell): Integer;
    function IndexOf(aX, aY: Integer; IncludeLast: Boolean = False; aCurrentCellIdx: Integer = -1): Integer;
    function IndexOfNext(aX, aY: Integer; IncludeLast: Boolean = False; aCurrentCellIdx: Integer = -1): Integer;
    function IsInCell(aX, aY, ACellIdx: Integer; IncludeLast: Boolean = False): Boolean;
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

  TSynPluginSyncronizedEditBase = class;

  { TSynPluginSyncronizedEditUndoCurrentCell }

  TSynPluginSyncronizedEditUndoCurrentCell = class(TSynEditUndoItem)
  private
    FCurrentCell: Integer;
    FOwner: TSynPluginSyncronizedEditBase;
  protected
    function IsEqualContent(AnItem: TSynEditUndoItem): Boolean; override;
    function DebugString: String; override;
  public
    //function IsCaretInfo: Boolean; override; // TODO: maybe indicator for TSynEditUndoGroup.HasUndoInfo
    constructor Create(ACurrentCell: Integer; AOwner: TSynPluginSyncronizedEditBase);
    function PerformUndo(Caller: TObject): Boolean; override;
  end;

  { TSynPluginSyncronizedEditChangeAction }

  TSynPluginSyncronizedEditChangeAction = record
    CellIndex: Integer;
    cLinePos, cCharPos, Count, LineBrkCount: Integer;
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
    procedure Add(aCellIndex, aLinePos, aCharPos, aCount, aLineBrkCnt: Integer;
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
    FUndoingCurrentCell: Integer;
    FDependsOnCurrentCell, FJustStarted: Boolean;
    FActiveInUndo: Boolean;
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

    procedure DoUndoBegining(Sender: TObject);
    procedure DoUndoEnding(Sender: TObject);
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
    function IsCellFromCaretAmbigious: Boolean;
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

  TSynPluginSyncroScanMode = (spssNoCase, spssWithCase, spssCtxNoCase, spssCtxWithCase);
  TSynPluginSyncroScanModes = set of TSynPluginSyncroScanMode;

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

    function MovePointLeft(p: TPoint; out ARes: TPoint; AllowLineWrap: Boolean): Boolean;
    function MovePointRight(p: TPoint; out ARes: TPoint; AllowLineWrap: Boolean): Boolean;
    procedure RemoveSingleGroupCells;
    procedure ShrinkCell(AGroup: Integer; ARightSide: Boolean);
    procedure GrowCell(AGroup: Integer; ARightSide: Boolean);
    procedure ResizeCell(ARightSide: Boolean; AShrink: Boolean);
    procedure AddGroupFromSelection(AScanMode: TSynPluginSyncroScanMode);
    procedure RemoveCurrentCell;
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

{ TSynPluginSyncronizedEditUndoCurrentCell }

function TSynPluginSyncronizedEditUndoCurrentCell.IsEqualContent(
  AnItem: TSynEditUndoItem): Boolean;
begin
  Result := (FCurrentCell = TSynPluginSyncronizedEditUndoCurrentCell(AnItem).FCurrentCell) and
            (FOwner = TSynPluginSyncronizedEditUndoCurrentCell(AnItem).FOwner);
end;

function TSynPluginSyncronizedEditUndoCurrentCell.DebugString: String;
begin
  Result := ClassName + ' CurCell=' + IntToStr(FCurrentCell);
end;

constructor TSynPluginSyncronizedEditUndoCurrentCell.Create(
  ACurrentCell: Integer; AOwner: TSynPluginSyncronizedEditBase);
begin
  FCurrentCell := ACurrentCell;
  FOwner := AOwner;
end;

function TSynPluginSyncronizedEditUndoCurrentCell.PerformUndo(Caller: TObject
  ): Boolean;
var
  i: Integer;
begin
  Result := Caller is TCustomSynEdit;
  if not Result then
    exit;
  i := TCustomSynEdit(Caller).PluginCount;
  Result := False;
  while (i > 0) and (not Result) do begin
    dec(i);
    Result := (TCustomSynEdit(Caller).Plugin[i] = FOwner);
  end;
  if not (Result and
          (TObject(FOwner) is TSynPluginSyncronizedEditBase) and // in case a new plugin is at the same address
          (FOwner.Active)  // at the current point of undo-history only the real owner can be active
         )
  then
    exit;

  if (FOwner.FUndoingCurrentCell >= 0) and
     (FOwner.FUndoingCurrentCell <> FCurrentCell)
  then
    FOwner.ViewedTextBuffer.CurUndoList.AddChange(
      TSynPluginSyncronizedEditUndoCurrentCell.Create(FOwner.FUndoingCurrentCell, FOwner));

  FOwner.FUndoingCurrentCell := FCurrentCell;
  FOwner.FCurrentCell := FCurrentCell;
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

function TSynPluginSyncronizedEditList.Add(aCell: TSynPluginSyncronizedEditCell;
  AnAddSorted: Boolean): Integer;
var
  i, n: Integer;
begin
  n := -1;
  if AnAddSorted then
    n := IndexOfNext(aCell.LogStart.x, aCell.LogStart.Y);

  i := length(FCells);
  SetLength(FCells, i + 1);
  if (n >= 0) and (n < i) then begin
    move(FCells[n], FCells[n+1], SizeOf(FCells[0]) * (i-n));
    i := n;
  end;
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
  if assigned(FOnCellChange) then
    FOnCellChange(aIndex, FCells[aIndex], nil);
  FCells[aIndex].Free;
  i := length(FCells) - 1;
  if aIndex < i then
    System.Move(FCells[aIndex+1], FCells[aIndex], (i-aIndex) * SizeOf(TSynPluginSyncronizedEditCell));
  SetLength(FCells, i);
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

function TSynPluginSyncronizedEditList.IndexOf(aX, aY: Integer;
  IncludeLast: Boolean; aCurrentCellIdx: Integer): Integer;
var
  a, i: Integer;
begin
  // If 2 cells touch, prefer current cell
  if (aCurrentCellIdx >= 0) and IsInCell(aX, aY, aCurrentCellIdx, IncludeLast) then begin
    Result := aCurrentCellIdx;
    exit;
  end;

  if IncludeLast then
    a := 1
  else
    a := 0;
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

function TSynPluginSyncronizedEditList.IndexOfNext(aX, aY: Integer;
  IncludeLast: Boolean; aCurrentCellIdx: Integer): Integer;
var
  a, i: Integer;
begin
  // If 2 cells touch, prefer current cell
  if (aCurrentCellIdx >= 0) and IsInCell(aX, aY, aCurrentCellIdx, IncludeLast) then begin
    Result := aCurrentCellIdx;
    exit;
  end;

  if IncludeLast then
    a := 1
  else
    a := 0;
  for i := 0 to Count -1 do begin
    if (FCells[i].Group >= 0) and
       ( (FCells[i].LogEnd.Y > aY) or
         ((FCells[i].LogEnd.Y = aY) and (FCells[i].LogEnd.X + a > aX)) )
    then
      exit(i);
  end;
  Result := -1;
end;

function TSynPluginSyncronizedEditList.IsInCell(aX, aY, ACellIdx: Integer;
  IncludeLast: Boolean): Boolean;
var
  a: Integer;
begin
  if ACellIdx >= Length(FCells) then exit(False);
  if IncludeLast then
    a := 1
  else
    a := 0;;
  Result :=
     (FCells[ACellIdx].Group >= 0) and
     ( (FCells[ACellIdx].LogStart.Y < aY) or
       ((FCells[ACellIdx].LogStart.Y = aY) and (FCells[ACellIdx].LogStart.X <= aX)) ) and
     ( (FCells[ACellIdx].LogEnd.Y > aY) or
       ((FCells[ACellIdx].LogEnd.Y = aY) and (FCells[ACellIdx].LogEnd.X + a > aX)) );
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
  while i < length(FCells) do begin
    if FCells[i].Group = aGroup then
      inc(Result);
    inc(i);
  end;
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
  if (aOldVal <> nil) and (aOldVal.Group = CellGroupForArea) and (aOldVal.LogStart.Y <> high(Integer)) then
    InvalidateSynLines(aOldVal.LogStart.Y, aOldVal.LogEnd.Y);
  if (aNewVal <> nil) and (aNewVal.Group = CellGroupForArea) and (aNewVal.LogStart.Y <> high(Integer)) then
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

procedure TSynPluginSyncronizedEditChangeList.Add(aCellIndex, aLinePos, aCharPos, aCount,
  aLineBrkCnt: Integer; aText: String);
begin
  if length(FList) <= FCount then
    SetLength(FList, FCount + 4);

  FList[FCount].CellIndex := aCellIndex;
  FList[FCount].cLinePos := aLinePos;
  FList[FCount].cCharPos := aCharPos;
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
    ViewedTextBuffer.RemoveNotifyHandler(senrBeginUndoRedo, @DoUndoBegining);
    ViewedTextBuffer.RemoveNotifyHandler(senrEndUndoRedo, @DoUndoEnding);
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
    ViewedTextBuffer.AddNotifyHandler(senrBeginUndoRedo, @DoUndoBegining);
    ViewedTextBuffer.AddNotifyHandler(senrEndUndoRedo, @DoUndoEnding);
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

function TSynPluginSyncronizedEditBase.IsCellFromCaretAmbigious: Boolean;
var
  c: TSynPluginSyncronizedEditCell;
  p: TPoint;
begin
  p := CaretObj.LineBytePos;
  if (CurrentCell < 0) then
    Result := Cells.IndexOf(p.x, p.y, True) >= 0
  else begin
    c := Cells[CurrentCell];
    Result := (
      (CurrentCell > 0) and (CompareCarets(c.LogStart, p) = 0) and
      (CompareCarets(Cells[CurrentCell - 1].LogEnd, p) = 0)
    ) or (
      (CurrentCell < Cells.Count-1) and (CompareCarets(c.LogEnd, p) = 0) and
      (CompareCarets(Cells[CurrentCell + 1].LogStart, p) = 0)
    );
  end;
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

procedure TSynPluginSyncronizedEditBase.DoUndoBegining(Sender: TObject);
begin
  FActiveInUndo := Active;
  if not FActiveInUndo then
    exit;

  ViewedTextBuffer.CurUndoList.BeginBlock;
  FUndoingCurrentCell := -1;
end;

procedure TSynPluginSyncronizedEditBase.DoUndoEnding(Sender: TObject);
begin
  if not FActiveInUndo then
    exit;

  if FUndoingCurrentCell >= 0 then begin
    // AppendToLastChange: Add before the caret undo info, otherwise another caret info will be added (at this point the current undo group must have some items)
    ViewedTextBuffer.CurUndoList.AppendToLastChange(
      TSynPluginSyncronizedEditUndoCurrentCell.Create(FUndoingCurrentCell, Self));
  end;

  FDependsOnCurrentCell := False;  // Prevent ApplyChangeList from adding undo info
  FUndoingCurrentCell := -1;
  ViewedTextBuffer.CurUndoList.EndBlock;
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
  CellAtPos, CellCnt: Integer;
  LastCellEndPoint, CurCellStartPos: TPoint;
  Line: PChar;
begin
  if not Active then begin
    if PreActive then DoPreActiveEdit(aBytePos, aLinePos, aCount, aLineBrkCnt, IsUndoing or IsRedoing);
    exit;
  end;
  Pos := Point(aBytePos, aLinePos);
  Pos2 := Pos;
  if not FEditing then
    DoBeforeEdit(Pos.x, Pos.y, aCount, aLineBrkCnt, IsUndoing or IsRedoing);
  if not Active then
    exit;

  edit := FEditing or  IsUndoing or IsRedoing;
  if (not edit) then begin
    CellAtPos := Cells.IndexOf(Pos.x, Pos.y, True, CurrentCell);

    if FJustStarted and IsCellFromCaretAmbigious // and not IsUndoing
    then
      FDependsOnCurrentCell := True;
  end;
  FJustStarted := False;

  CurCellStartPos := Point(-1, -1);
  if FCurrentCell >= 0 then
    CurCellStartPos := Cells[FCurrentCell].LogStart;
  LastCellEndPoint := Point(-1, -1);
  CellCnt := FCells.Count - 1;
  for i := 0 to CellCnt do begin
    CurCell := Cells[i];
    chg := False;

    a := CompareCarets(Pos, CurCell.LogStart);
    if (a = 0) and (CompareCarets(LastCellEndPoint, CurCell.LogStart) = 0) then begin
      a := 1;
      FDependsOnCurrentCell := True;
    end;
    if (a > 0) then
      CurCell.LogStart := AdjustPoint(CurCell.LogStart, chg);

    a := CompareCarets(Pos, CurCell.LogEnd);
    if (a = 0) then begin
      if (CompareCarets(CurCell.LogEnd, CurCellStartPos) <> 0) or
         (CurCell.Group=-1)
      then
        inc(a)
      else
      begin
        FDependsOnCurrentCell := True;
        if (i >= CurrentCell) then
          inc(a);
      end;
    end;
    if (a > 0) //or
    then begin
      if CurCell.Group <> -1 then
        LastCellEndPoint := CurCell.LogEnd; // If this is adjusted, the start of the next cell (if equal to this) must be adjusted too
      CurCell.LogEnd := AdjustPoint(CurCell.LogEnd, chg);
    end;

    if chg then
      Cells[i] := CurCell;
  end;

  if (not edit) and
     (CellAtPos >= 0) and (CellAtPos < Cells.Count) and
     (CompareCarets(Pos, FCells[CellAtPos].LogStart) <= 0) and
     (CompareCarets(Pos, FCells[CellAtPos].LogEnd) >= 0)
  then begin
    CurCell := FCells[CellAtPos];
    Line := ViewedTextBuffer.GetPChar(Pos.Y-1, i);
    Pos.Y := Pos.Y - CurCell.LogStart.y;
    if Pos.y = 0 then begin
      Pos.X := Pos.X - CurCell.LogStart.x;
      Pos.X := CountChars(Line+CurCell.LogStart.x-1, Pos.X);
    end
    else begin
      dec(Pos.x);
      Pos.X := CountChars(Line, Pos.X);
    end;
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
  Group, Y2, X2, CurCell, LastUndoCurCell, Len: Integer;
  Cell: TSynPluginSyncronizedEditCell;
  Line: PChar;
  XStart: Integer;
begin
  LastUndoCurCell := -1;
  if FDependsOnCurrentCell then begin
    ViewedTextBuffer.CurUndoList.AddChange(
      TSynPluginSyncronizedEditUndoCurrentCell.Create(FCurrentCell, Self));
    LastUndoCurCell := FCurrentCell;
  end;

  if FChangeList.Count = 0 then
    exit;
  FEditing := True;
  ViewedTextBuffer.BeginUpdate;
  CaretObj.IncAutoMoveOnEdit;
  CurCell := FCurrentCell; // direct access / markup does not need to know

  try
    for i := 0 to FCells.Count - 1 do begin
      FDependsOnCurrentCell := False;
      Cell := FCells[i];

      for a := 0 to FChangeList.Count - 1 do begin
        Action := FChangeList[a];
        Group := FCells[Action.CellIndex].Group;
        if (i = Action.CellIndex) or (Cell.Group <> Group) then
          continue;

        FCurrentCell := i; // direct access / markup does not need to know
        Y2 := Cell.LogStart.Y + Action.cLinePos;
        if (Y2 <= Cell.LogEnd.Y) then begin
          Line := ViewedTextBuffer.GetPChar(Y2-1, Len);
          XStart := Cell.LogStart.X;
          if Action.cLinePos = 0 then
            X2 := XStart + CountBytes(Line + XStart - 1, Action.cCharPos, Len - XStart + 1)
          else
            X2 := 1 + CountBytes(Line, Action.cCharPos, Len);

          if (Y2 < Cell.LogEnd.Y) or (X2 <= Cell.LogEnd.X) then begin
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

      if FDependsOnCurrentCell then begin
        ViewedTextBuffer.CurUndoList.AddChange(
          TSynPluginSyncronizedEditUndoCurrentCell.Create(FCurrentCell, Self));
        LastUndoCurCell := FCurrentCell;
      end;
    end;
  finally
    FCurrentCell := CurCell; // direct access / markup does not need to know
    // for redo
    if IsCellFromCaretAmbigious and (FCurrentCell <> LastUndoCurCell) then
      ViewedTextBuffer.CurUndoList.AddChange(
        TSynPluginSyncronizedEditUndoCurrentCell.Create(FCurrentCell, Self));
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
  FDependsOnCurrentCell := False;
  FJustStarted := True;
end;

procedure TSynPluginSyncronizedEditBase.DoPaintLockEnded;
begin
  FJustStarted := False;
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
  c1 := Cells.IndexOf(aX, aY, True, CurrentCell);
  if aLineBrkCnt < 0 then
    c2 := Cells.IndexOf(1, aY-aLineBrkCnt, True, CurrentCell)
  else if aCount < 0 then
    c2 := Cells.IndexOf(aX - aCount, aY, True, CurrentCell)
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
  i := Cells.IndexOf(CaretObj.BytePos, CaretObj.LinePos, True, CurrentCell);
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
  i := Cells.IndexOf(Pos.x, Pos.y, True, CurrentCell);
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
  i := Cells.IndexOf(Pos.x, Pos.y, True, CurrentCell);
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

function TSynPluginCustomSyncroEdit.MovePointLeft(p: TPoint; out ARes: TPoint;
  AllowLineWrap: Boolean): Boolean;
begin
  Result := False;
  p := FriendEdit.LogicalToPhysicalPos(p);
  if p.X = 1 then begin
    if (not AllowLineWrap) or (p.Y = 1) then
      exit;

    ARes.Y := p.Y - 1;
    ARes.X := Length(FriendEdit.Lines[ToIdx(ARes.y)]) + 1;
    Result := True;
    exit;
  end;

  Dec(p.X);
  ARes := FriendEdit.PhysicalToLogicalPos(p);
  Result := True;
end;

function TSynPluginCustomSyncroEdit.MovePointRight(p: TPoint; out ARes: TPoint;
  AllowLineWrap: Boolean): Boolean;
begin
  Result := False;
  p := FriendEdit.LogicalToPhysicalPos(p);
  if p.X > Length(FriendEdit.Lines[ToIdx(p.Y)]) then begin
    if (not AllowLineWrap) or (p.Y >= FriendEdit.Lines.Count) then
      exit;

    ARes.Y := p.Y + 1;
    ARes.X := 1;
    Result := True;
    exit;
  end;

  Inc(p.X);
  ARes := FriendEdit.PhysicalToLogicalPos(p);
  Result := True;
end;

procedure TSynPluginCustomSyncroEdit.RemoveSingleGroupCells;
var
  i, j, g: Integer;
begin
  i := 0;
  while i < Cells.Count do begin
    g := Cells[i].Group;
    if g < 0 then begin
      inc(i);
      continue;
    end;
    j := i - 1;
    while (j >= 0) and (Cells[j].Group <> g) do
      dec(j);
    Cells[i].FFirstInGroup := j < 0;
    if j < 0 then begin
      j := Cells.Count - 1;
      while j > i do begin
        if Cells[j].Group = g then break;
        dec(j);
      end;
      if j = i then begin
        Cells.Delete(i);
        dec(i);
      end;
    end;
    inc(i);
  end;
end;

procedure TSynPluginCustomSyncroEdit.ShrinkCell(AGroup: Integer;
  ARightSide: Boolean);
var
  i2: Integer;
  c2: TSynPluginSyncronizedEditCell;
  NewPos: TPoint;
  DidDelete: Boolean;
begin
  DidDelete := False;
  for i2 := Cells.Count - 1 downto 0 do begin
    if i2 >= Cells.Count then Continue;
    c2 := Cells[i2];
    if c2.Group <> AGroup then Continue;
    if ARightSide then begin
      MovePointLeft(c2.LogEnd, NewPos, c2.LogEnd.y <> c2.LogStart.y);
      if CompareCarets(NewPos, c2.LogEnd) <= 0 then begin
        Cells.Delete(i2);
        DidDelete := True;
      end
      else
        Cells[i2].LogEnd := NewPos;
    end
    else begin
      MovePointRight(c2.LogStart, NewPos, c2.LogEnd.y <> c2.LogStart.y);
      if CompareCarets(c2.LogStart, NewPos) <= 0 then begin
        Cells.Delete(i2);
        DidDelete := True;
      end
      else
        Cells[i2].LogStart := NewPos;
    end;
  end;

  if DidDelete then
    RemoveSingleGroupCells;
  UpdateCurrentCell;
  FriendEdit.Invalidate;
end;

procedure TSynPluginCustomSyncroEdit.GrowCell(AGroup: Integer;
  ARightSide: Boolean);
var
  i2, i3: Integer;
  c2: TSynPluginSyncronizedEditCell;
  NewPos: TPoint;
  DidDelete: Boolean;
begin
  DidDelete := False;
  for i2 := Cells.Count - 1 downto 0 do begin
    if i2 >= Cells.Count then Continue;
    c2 := Cells[i2];
    if c2.Group <> AGroup then Continue;
    if ARightSide then begin
      MovePointRight(c2.LogEnd, NewPos, False);
      i3 := Cells.IndexOf(NewPos.X, NewPos.Y, True {touch});
      Cells[i2].LogEnd := NewPos;
    end
    else begin
      MovePointLeft(c2.LogStart, NewPos, False);
      i3 := Cells.IndexOf(NewPos.X, NewPos.Y, True {touch});
      Cells[i2].LogStart := NewPos;
    end;
    if (i3 >= 0) then begin
      Cells.Delete(i3);
      DidDelete := True;
    end;
  end;

  if DidDelete then
    RemoveSingleGroupCells;
  UpdateCurrentCell;
  FriendEdit.Invalidate;
end;

procedure TSynPluginCustomSyncroEdit.ResizeCell(ARightSide: Boolean;
  AShrink: Boolean);
var
  i, g, i2, i3: Integer;
  c, c2: TSynPluginSyncronizedEditCell;
  NewPos: TPoint;
  r: Boolean;
begin
  i := CurrentCell;
  if i < 0 then
    exit;

  c := Cells[i];
  g := c.Group;
  if g < 0 then exit;

  for i2 := 0 to Cells.Count - 1 do begin
    c2 := Cells[i2];
    if c2.Group <> g then Continue;
    if ARightSide then begin
      if AShrink
      then r := MovePointLeft(c2.LogEnd, NewPos, c2.LogEnd.y <> c2.LogStart.y)
      else r := MovePointRight(c2.LogEnd, NewPos, False);
    end
    else begin
      if AShrink
      then r := MovePointRight(c2.LogStart, NewPos, c2.LogEnd.y <> c2.LogStart.y)
      else r := MovePointLeft(c2.LogStart, NewPos, False);
    end;
    if not r then
      exit;

    if AShrink then begin
      if (i = i2) then begin
        // check current cell can shrink
        if ARightSide then begin
          if CompareCarets(NewPos, c2.LogEnd) <= 0 then exit;
        end
        else
          if CompareCarets(c2.LogStart, NewPos) <= 0 then exit;
      end;
    end
    else begin
      // growing
      i3 := Cells.IndexOf(NewPos.X, NewPos.Y, True {touch});
      if (i3 >= 0) and (Cells[i3].Group = g) then
        exit;
    end;
  end;

  if AShrink then
    ShrinkCell(g, ARightSide)
  else
    GrowCell(g, ARightSide);
end;

procedure TSynPluginCustomSyncroEdit.AddGroupFromSelection(AScanMode: TSynPluginSyncroScanMode);
type
  TPointEx = record Pnt: TPoint; Len: integer; end;

  function FindNextStart(AText: String; AStartPos, AMaxPos: TPoint): TPointEx;
  var
    l2, l: String;
    x2: LongInt;
  begin
    Result.Pnt.Y := -1;
    repeat
      l := TextBuffer[ToIdx(AStartPos.Y)];
      l2 := l;
      if AScanMode in [spssNoCase, spssCtxNoCase] then begin
        l := Utf8LowerCase(l);
        if AStartPos.X > 1 then
          AStartPos.X := CountBytes(PChar(l), CountChars(PChar(l2), AStartPos.X - 1), Length(l)) + 1;
      end;
      Result.Pnt.X := PosEx(AText, l, AStartPos.X);
      if (Result.Pnt.X > 0) and
         ( (AStartPos.Y < AMaxPos.Y) or
           ((AStartPos.y = AMaxPos.y) and (Result.Pnt.x + Length(AText) <= AMaxPos.X))
         )
      then begin
        Result.Pnt.Y   := AStartPos.Y;
        Result.Len := Length(AText);
        if AScanMode in [spssNoCase, spssCtxNoCase] then begin
          x2 := Result.Pnt.X;
          Result.Pnt.X := CountBytes(PChar(l2), CountChars(PChar(l), Result.Pnt.X - 1), Length(l2)) + 1;
          Result.Len := CountBytes(PChar(l2)+x2-1, CountChars(PChar(AText), Length(AText)), Length(l2)-x2+1);
        end;
        exit;
      end;
      AStartPos.X := 1;
      inc(AStartPos.Y);
    until AStartPos.y > AMaxPos.Y;
  end;

  function FindNextStart(AText: String; AStartPos, AMaxPos: TPoint; AScope: integer): TPointEx;
  var
    tt: Integer;
  begin
    repeat
      Result := FindNextStart(AText, AStartPos, AMaxPos);
      if not (AScanMode in [spssCtxNoCase, spssCtxWithCase]) then
        exit;
      if Result.Pnt.Y < 0 then
        exit;
      TSynEdit(FriendEdit).GetHighlighterAttriAtRowColEx(Result.Pnt, tt, True);
      if tt = AScope then
        exit;
      AStartPos := Result.Pnt;
      AStartPos.x := AStartPos.x + Result.Len;
    until False;
  end;

var
  Grp: Integer;
  DidDelete: boolean;

  procedure AddFndCell(p, p2: TPoint; AsFirst: boolean = False);
  var
    nc: TSynPluginSyncronizedEditCell;
    Existing: Integer;
  begin
    Existing := Cells.IndexOfNext(p.X, p.Y);
    if Existing >= 0 then
      while (Existing < Cells.Count) and (CompareCarets(Cells[Existing].LogStart, p2) > 0) do begin
        Cells.Delete(Existing);
        DidDelete := True;
      end;

    nc := TSynPluginSyncronizedEditCell.Create;
    nc.LogStart := p;
    nc.LogEnd := p2;
    nc.Group := Grp;
    nc.FirstInGroup := AsFirst;
    Cells.Add(nc, True);
  end;
var
  BndCell: TSynPluginSyncronizedEditCell;
  SelTxt: String;
  p: TPoint;
  Fnd, Fnd2: TPointEx;
  FndEnd, FndEnd2: TPoint;
  Ctx, i, SelTxtCharLen: Integer;
begin
  if (not FriendEdit.SelAvail) or (FriendEdit.BlockBegin.y <> FriendEdit.BlockEnd.Y) then
    exit;
  BndCell := Cells.GroupCell[-1, 0];
  if BndCell = nil then
    exit;

  DidDelete := False;
  Grp := 1;
  for i := 0 to Cells.Count - 1 do
    if Cells[i].Group >= Grp then
      Grp := Cells[i].Group + 1;

  Ctx := 0;
  if AScanMode in [spssCtxNoCase, spssCtxWithCase] then
    TSynEdit(FriendEdit).GetHighlighterAttriAtRowColEx(FriendEdit.BlockBegin, Ctx, False);

  SelTxt := FriendEdit.SelText;
//  SelTxtCharLen := CountChars(PChar(SelTxt), Length(SelTxt));
  if AScanMode in [spssNoCase, spssCtxNoCase] then
    SelTxt := UTF8LowerCase(SelTxt);

  p := BndCell.LogStart;
  Fnd := FindNextStart(SelTxt, p, BndCell.LogEnd, Ctx);
  FndEnd := Fnd.Pnt;
  inc(FndEnd.X, Fnd.Len);
  if (Fnd.Pnt.Y < 0)  then
    exit;

  p := FndEnd;
  Fnd2 := FindNextStart(SelTxt, p, BndCell.LogEnd, Ctx);
  FndEnd2 := Fnd2.Pnt;
  inc(FndEnd2.X, Fnd2.Len);
  if (Fnd2.Pnt.Y < 0) then
    exit;

  AddFndCell(Fnd.Pnt, FndEnd, True);
  while Fnd2.Pnt.y >= 0 do begin
    AddFndCell(Fnd2.Pnt, FndEnd2);

    p := FndEnd2;
    Fnd2 := FindNextStart(SelTxt, p, BndCell.LogEnd, Ctx);
    FndEnd2 := Fnd2.Pnt;
    inc(FndEnd2.X, Fnd2.Len);
  end;

  if DidDelete then
    RemoveSingleGroupCells;
  UpdateCurrentCell;
  FriendEdit.Invalidate;
end;

procedure TSynPluginCustomSyncroEdit.RemoveCurrentCell;
begin
  if (CurrentCell < 0) or (CurrentCell >= Cells.Count) then
    exit;

  Cells.Delete(CurrentCell);
  RemoveSingleGroupCells;
  CurrentCell := -1;
  FriendEdit.Invalidate;
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

