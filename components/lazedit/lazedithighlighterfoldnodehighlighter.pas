{
 *****************************************************************************
  This file is part of the LazEdit package from the Lazarus IDE.

  This content of this file is licensed: Modified LGPL-2
  Or at the users choice: Modified LGPL-3
  See the file COPYING.modifiedLGPL.txt, included in the Lazarus distribution,
  for details about the license.

  Alternatively, the contents of this file may be used under the terms of the
  Mozilla Public License Version 1.1 or 2.0 http://www.mozilla.org/MPL/

  A copy used under either License can have the other Licenses removed from this
  header. A note should be added that the original file is available with the
  above choice of License.
  Used units may have to be amended according to the choice.
 *****************************************************************************
}

unit LazEditHighlighterFoldNodeHighlighter;

{$mode objfpc}{$H+}

interface

uses
  Math, SysUtils,
  // LazUtils
  LazClasses,
  // LazEdit
  LazEditHighlighter, LazEditTypes;

type
  TLazEditFoldNodeInfoList = class;

  //TLazEditCustomFoldNodeHighlighter = class(TLazEditCustomHighlighter)
  TLazEditCustomFoldNodeHighlighter = class(TLazEditCustomRangesHighlighter)
  private
    FFoldNodeInfoList: TLazEditFoldNodeInfoList;
  protected
    function GetFoldNodeInfo(Line: TLineIdx): TLazEditFoldNodeInfoList;
    function CreateFoldNodeInfoList: TLazEditFoldNodeInfoList; virtual;
    procedure ClearFoldNodeList;
    procedure InitFoldNodeInfo(AList: TLazEditFoldNodeInfoList; Line: TLineIdx); virtual;
  public
    destructor Destroy; override;
    // All fold-nodes
    // FoldNodeInfo: Returns a shared object
    // Adding RefCount, will prevent others from getting further copies, but not from using copies they already have.
    // If not adding refcount, the object should not be stored/re-used
    // Not adding ref-count, should only be done for CountEx, NodeInfoEx
    property FoldNodeInfo[Line: TLineIdx]: TLazEditFoldNodeInfoList read GetFoldNodeInfo;
  end;

  TSynFoldAction = ( sfaOpen,         // Any Opening node
                     sfaClose,        // Any Closing node

                     sfaFold,         // Part of a fold- or hide-able block (FoldConf.Enabled = True)           - excludes one=liners for FoldFold, as they can not fold
                     sfaFoldFold,     // Part of a fold-able block (FoldConf.Enabled = True / smFold in Modes)  - excludes one=liners / only opening node, except ifdef/region (todo: maybe both?)
                     sfaFoldHide,     // Part of a hide-able block (FoldConf.Enabled = True / smHide in Modes)  - includes one=liners / only opening node, except ifdef/region (todo: maybe both?)

                     sfaMultiLine,    // The closing node is on an other line
                     sfaSingleLine,   // The closing node is on the same line (though the keyword may be on the next)
                     // //sfaSingleLineClosedByNext
                     sfaCloseForNextLine,  // Fold closes this line, but keyword is on the next (e.g. "var" block)
                     sfaLastLineClose,     // Fold is incomplete, and closed at last line of file
                     sfaCloseAndOpen,    // This node has the same location/type as the neighbouring opposite node.
                                         // eg an open node, matched exactly the previous node, which has to be a closing node of the same type and location (and vice versa for a closing node matching the next...)
                     sfaDefaultCollapsed,
                     sfaInvalid,  // Wrong Index

                     // TODO: deprecate
                     sfaOpenFold,     // At this node a new Fold can start // Actually, includes all,any multi-line node too.
                     sfaCloseFold,    // At this node a fold ends
                     sfaOneLineOpen,   // Open, but closes on same line; *only* if hide-able has [sfaOpenFold, sfaFold]; always has [sfaFoldFold, sfaFoldHide]
                     sfaOneLineClose,  // Open, but closes on same line;

                     sfaMarkup,             // This node can be highlighted by the matching Word-Pair Markup

                     sfaOutline,            // This node can be highlighted by nested color
                     sfaOutlineKeepLevel,   // Direct children should use same color as this.  e.g. "if","then" or "procedure", "begin"
                     sfaOutlineMergeParent, // Use same color as parent node.
                     sfaOutlineForceIndent  // Override/Ignore sfaOutlineKeepLevel, sfaOutlineMergeParent
                   );
  TSynFoldActions = set of TSynFoldAction;

  TLazEditFoldNodeInfo = record
    LineIndex: Integer;
    NodeIndex: Integer;          // Indicates the position within the list of info nodes (depends on search-Filter)
    AllNodeIndex: Integer;       // Indicates the position within the unfiltered list of info nodes
    LogXStart, LogXEnd: Integer; // -1 previous line ( 0-based)
    FoldLvlStart, FoldLvlEnd: Integer; // FoldLvl within each FoldGroup
    NestLvlStart, NestLvlEnd: Integer; // include disabled nodes, e.g markup (within each FoldGroup)
    FoldAction: TSynFoldActions;
    FoldType: Pointer;           // e.g.cfbtBeginEnd, cfbtProcedure ...
    FoldTypeCompatible: Pointer; // map outer and inner begin, and other exchangeable types
    FoldGroup: Integer;          // independent/overlapping folds, e.g begin/end; ifdef, region
  end;
  PLazEditFoldNodeInfo = ^TLazEditFoldNodeInfo;

  { TLazEditFoldNodeInfoList }

  TLazEditFoldNodeInfoList = class(TRefCountedObject)
  protected const
    DEFAULT_MIN_CAPACITY = 8;
  private
    FHighLighter: TLazEditCustomFoldNodeHighlighter;
    FValid: Boolean;
    FActionFilter: TSynFoldActions;
    FGroupFilter: Integer;
    FLine: TLineIdx;
    FNodeCount: Integer;
    FFilteredCount, FFilteredProgress: Integer;
    FNodeInfoList: Array of TLazEditFoldNodeInfo;
    FFilteredList: Array of TLazEditFoldNodeInfo;
    function  GetItem(Index: Integer): TLazEditFoldNodeInfo;
    procedure SetActionFilter(AValue: TSynFoldActions);
    procedure SetGroupFilter(AValue: Integer);
    function  GetItemPointer(AnIndex: Integer): PLazEditFoldNodeInfo;
    function  GetLastItemPointer: PLazEditFoldNodeInfo;
  protected
    procedure Invalidate;
    procedure ClearData;
    procedure ClearFilteredList;
    procedure DoFilter(MinIndex: Integer = -1);
    procedure SetLine(ALine: TLineIdx); // Does not clear anything, if line has not changed.
    property  HighLighter: TLazEditCustomFoldNodeHighlighter read FHighLighter;
  public
    constructor Create(AnHighLighter: TLazEditCustomFoldNodeHighlighter);
    // used by HighLighters to add data
    procedure Clear;
    procedure Add(const AnInfo: TLazEditFoldNodeInfo);
    procedure Delete(AnIndex: Integer = -1);
    function  CountAll: Integer;
    property  ItemPointer[AnIndex: Integer]: PLazEditFoldNodeInfo read GetItemPointer;
    property  LastItemPointer: PLazEditFoldNodeInfo read GetLastItemPointer;
  protected
    function  DefaultGroup: Integer; virtual;
    function  MinCapacity: Integer; virtual;
    function  Match(const AnInfo: TLazEditFoldNodeInfo;
                    AnActionFilter: TSynFoldActions; AGroupFilter: Integer = 0): Boolean; virtual;
  public
    // filtered items
    procedure ClearFilter;
    function Count: Integer;
    property Item[Index: Integer]: TLazEditFoldNodeInfo read GetItem; default;
    property ActionFilter: TSynFoldActions read FActionFilter write SetActionFilter;
    property GroupFilter: Integer read FGroupFilter write SetGroupFilter;
  public
    // all items / filtered on the fly
    function CountEx   (AnActionFilter: TSynFoldActions; AGroupFilter: Integer = 0): Integer;
    function NodeInfoEx(Index: Integer; AnActionFilter: TSynFoldActions; AGroupFilter: Integer = 0): TLazEditFoldNodeInfo; virtual;
  public
    // Only allowed to be set, if highlighter has CurrentLines (and is scanned)
    property Line: TLineIdx read FLine write SetLine;
    property Valid: boolean read FValid;
    property RefCount;
  end;



function dbgs(AFoldActions: TSynFoldActions): String; overload;
function dbgs(ANode: TLazEditFoldNodeInfo):string; overload;

implementation

function dbgs(AFoldActions: TSynFoldActions): String;
var
  i: TSynFoldAction;
  s: string;
begin
  Result:='';
  for i := low(TSynFoldAction) to high(TSynFoldAction) do
    if i in AFoldActions then begin
      WriteStr(s{%H-}, i);
      Result := Result + s + ',';
    end;
  if Result <> '' then Result := '[' + copy(Result, 1, Length(Result)-1) + ']';
end;

function dbgs(ANode: TLazEditFoldNodeInfo): string;
begin
  with ANode do
    if sfaInvalid in FoldAction then
      Result := Format('L=%3d I=%d  X=%2d-%2d  Fld=%d-%d Nst=%d-%d  FT=%2d FTC=%2d  Grp=%d  A=%s',
                       [LineIndex, NodeIndex, 0, 0, 0, 0, 0, 0, 0, 0, 0, dbgs(FoldAction)])
    else
      Result := Format('L=%3d I=%d  X=%2d-%2d  Fld=%d-%d Nst=%d-%d  FT=%2d FTC=%2d  Grp=%d  A=%s',
                       [LineIndex, NodeIndex, LogXStart, LogXEnd,
                        FoldLvlStart, FoldLvlEnd, NestLvlStart, NestLvlEnd,
                        PtrUInt(FoldType), PtrUInt(FoldTypeCompatible), FoldGroup,
                        dbgs(FoldAction)]);
end;

{ TLazEditCustomFoldNodeHighlighter }

procedure TLazEditCustomFoldNodeHighlighter.ClearFoldNodeList;
begin
  if FFoldNodeInfoList <> nil then begin
    if (FFoldNodeInfoList.RefCount > 1) then
      ReleaseRefAndNil(FFoldNodeInfoList)
    else
      FFoldNodeInfoList.Clear;
  end;
end;

function TLazEditCustomFoldNodeHighlighter.GetFoldNodeInfo(Line: TLineIdx
  ): TLazEditFoldNodeInfoList;
begin
  if (FFoldNodeInfoList <> nil) and (FFoldNodeInfoList.RefCount > 1) then
    ReleaseRefAndNil(FFoldNodeInfoList);

  if FFoldNodeInfoList = nil then begin
    FFoldNodeInfoList := CreateFoldNodeInfoList;
    FFoldNodeInfoList.AddReference;
  end
  else
  if (CurrentRanges <> nil) and (CurrentRanges.FirstInvalidLine >= 0) then
    ClearFoldNodeList;


  Result := FFoldNodeInfoList;
  if (Line >= 0) and
     ((Result.Line <> Line) or (not Result.valid))
  then
    Result.ClearFilter;
  Result.Line := Line;
end;

function TLazEditCustomFoldNodeHighlighter.CreateFoldNodeInfoList: TLazEditFoldNodeInfoList;
begin
  Result := TLazEditFoldNodeInfoList.Create(Self);
end;

procedure TLazEditCustomFoldNodeHighlighter.InitFoldNodeInfo(AList: TLazEditFoldNodeInfoList;
  Line: TLineIdx);
begin
  //
end;

destructor TLazEditCustomFoldNodeHighlighter.Destroy;
begin
  inherited Destroy;
  ReleaseRefAndNil(FFoldNodeInfoList);
end;

{ TLazEditFoldNodeInfoList }

function TLazEditFoldNodeInfoList.GetItem(Index: Integer): TLazEditFoldNodeInfo;
begin
  DoFilter(Index);
  if (Index >= FFilteredCount) or (Index < 0) or (not FValid) then begin
    Result.FoldAction := [sfaInvalid];
    Result.LineIndex := Line;
    Result.NodeIndex := -1;
  end
  else begin
    Result := FFilteredList[Index];
    Result.NodeIndex := Index; // only set copy on result
  end;
end;

procedure TLazEditFoldNodeInfoList.SetActionFilter(AValue: TSynFoldActions);
begin
  if FActionFilter=AValue then Exit;
  FActionFilter:=AValue;
  ClearFilteredList;
end;

procedure TLazEditFoldNodeInfoList.SetGroupFilter(AValue: Integer);
begin
  if FGroupFilter=AValue then Exit;
  FGroupFilter:=AValue;
  ClearFilteredList;
end;

procedure TLazEditFoldNodeInfoList.Clear;
begin
  ClearFilter;
  ClearData;
end;

procedure TLazEditFoldNodeInfoList.ClearData;
var
  c: Integer;
begin
  FValid := True;
  ClearFilteredList;
  FLine := -1;
  c := MinCapacity;
  FNodeCount := 0;
  if Length(FNodeInfoList) > c then
    SetLength(FNodeInfoList, 0);
end;

procedure TLazEditFoldNodeInfoList.ClearFilteredList;
begin
  if Length(FFilteredList) > Length(FNodeInfoList) * 2 then
    SetLength(FFilteredList, 0);
  FFilteredCount := 0;
  FFilteredProgress := 0; // next to be filtered
end;

procedure TLazEditFoldNodeInfoList.ClearFilter;
begin
  ClearFilteredList;
  FGroupFilter := 0;
  FActionFilter := [];
end;

procedure TLazEditFoldNodeInfoList.DoFilter(MinIndex: Integer = -1);
begin
  if FFilteredProgress = FNodeCount then exit;
  if (MinIndex >= 0) and (FFilteredCount > MinIndex) or (not FValid) then exit;

  if (FActionFilter = []) and (FGroupFilter = DefaultGroup) then begin
    FFilteredList := FNodeInfoList;
    FFilteredCount := FNodeCount;
    FFilteredProgress := FNodeCount;
    exit;
  end;

  if (Length(FFilteredList) < Length(FNodeInfoList)) or
     (FFilteredList = FNodeInfoList)
  then begin
    FFilteredList := nil; // don't copy content
    SetLength(FFilteredList, Length(FNodeInfoList));
  end;

  while FFilteredProgress < FNodeCount do begin
    if Match(FNodeInfoList[FFilteredProgress], FActionFilter, FGroupFilter)
    then begin
      FFilteredList[FFilteredCount] := FNodeInfoList[FFilteredProgress];
      inc(FFilteredCount);
    end;
    inc(FFilteredProgress);
    if (MinIndex >= 0) and (FFilteredCount > MinIndex) then break;
  end;
end;

procedure TLazEditFoldNodeInfoList.SetLine(ALine: TLineIdx);
begin
  if (FValid and (FLine = ALine)) or
     (ALine < 0)
  then
    exit;
  ClearData;
  FLine := ALine;
  FHighLighter.InitFoldNodeInfo(Self, FLine);
end;

constructor TLazEditFoldNodeInfoList.Create(AnHighLighter: TLazEditCustomFoldNodeHighlighter);
begin
  FHighLighter := AnHighLighter;
  inherited Create;
end;

function TLazEditFoldNodeInfoList.MinCapacity: Integer;
begin
  Result := DEFAULT_MIN_CAPACITY;
end;

procedure TLazEditFoldNodeInfoList.Add(const AnInfo: TLazEditFoldNodeInfo);
var
  c: Integer;
begin
  FFilteredCount := 0;
  if FNodeCount >= Length(FNodeInfoList) - 1 then begin
    c := MinCapacity;
    if c <= 0 then
      c := DEFAULT_MIN_CAPACITY;
    SetLength(FNodeInfoList, Max(Length(FNodeInfoList) * 2, c));
  end;
  FNodeInfoList[FNodeCount] := AnInfo;
  FNodeInfoList[FNodeCount].AllNodeIndex := FNodeCount;
  If (FNodeCount > 0) and (sfaOpen in AnInfo.FoldAction) then begin
    c := FNodeCount-1;
    if (sfaClose in FNodeInfoList[c].FoldAction) and
       //(AnInfo.FoldType = FNodeInfoList[c].FoldType) and  // cfbtIfDef <> cfbtIfElse
       (AnInfo.LogXStart = FNodeInfoList[c].LogXStart) and
       (AnInfo.LogXEnd = FNodeInfoList[c].LogXEnd)
    then begin
      include(FNodeInfoList[FNodeCount].FoldAction, sfaCloseAndOpen);
      include(FNodeInfoList[c].FoldAction, sfaCloseAndOpen);
    end;
  end;
  inc(FNodeCount);
end;

procedure TLazEditFoldNodeInfoList.Delete(AnIndex: Integer = -1);
begin
  FFilteredCount := 0;
  if FNodeCount > 0 then
    dec(FNodeCount);
  if AnIndex >= 0 then begin
    while (AnIndex < FNodeCount) do begin
      FNodeInfoList[AnIndex] := FNodeInfoList[AnIndex + 1];
      FNodeInfoList[AnIndex].AllNodeIndex := AnIndex;
    inc(AnIndex);
    end;
  end;
end;

function TLazEditFoldNodeInfoList.CountAll: Integer;
begin
  if FValid then
    Result := FNodeCount
  else
    Result := -1;
end;

function TLazEditFoldNodeInfoList.GetItemPointer(AnIndex: Integer
  ): PLazEditFoldNodeInfo;
begin
  if (AnIndex >= FNodeCount) or (AnIndex < 0) then
    Result := nil
  else
    Result := @FNodeInfoList[AnIndex];
end;

function TLazEditFoldNodeInfoList.GetLastItemPointer: PLazEditFoldNodeInfo;
begin
  if FNodeCount < 0 then
    Result := nil
  else
    Result := @FNodeInfoList[FNodeCount-1];
end;

procedure TLazEditFoldNodeInfoList.Invalidate;
begin
  Clear;
  FValid := False;
end;

function TLazEditFoldNodeInfoList.Match(const AnInfo: TLazEditFoldNodeInfo;
  AnActionFilter: TSynFoldActions; AGroupFilter: Integer): Boolean;
begin
  Result := (AnInfo.FoldAction * AnActionFilter = AnActionFilter) and
            ( (AGroupFilter = 0) or (AnInfo.FoldGroup = AGroupFilter) );
end;

function TLazEditFoldNodeInfoList.DefaultGroup: Integer;
begin
  Result := 0;
end;

function TLazEditFoldNodeInfoList.Count: Integer;
begin
  if not FValid then exit(-1);

  DoFilter(-1);
  Result := FFilteredCount;
end;

function TLazEditFoldNodeInfoList.CountEx(AnActionFilter: TSynFoldActions;
  AGroupFilter: Integer): Integer;
var
  i: Integer;
begin
  if not FValid then exit(-1);
  if (AnActionFilter = []) and (AGroupFilter = DefaultGroup) then begin
    Result := FNodeCount;
    exit;
  end;

  Result := 0;
  for i := 0 to FNodeCount - 1 do
    if Match(FNodeInfoList[i], AnActionFilter, AGroupFilter) then inc(Result);
end;

function TLazEditFoldNodeInfoList.NodeInfoEx(Index: Integer;
  AnActionFilter: TSynFoldActions; AGroupFilter: Integer): TLazEditFoldNodeInfo;
var
  i, j: Integer;
begin
  if FValid and (Index >= 0) and (Index < FNodeCount) then begin
    if (AnActionFilter = []) and (AGroupFilter = DefaultGroup) then begin
      Result := FNodeInfoList[Index];
      Result.NodeIndex := Index; // only set copy on result
      exit;
    end;

    i := 0;
    j := Index;
    while i < FNodeCount do begin
      if Match(FNodeInfoList[i], AnActionFilter, AGroupFilter) then dec(j);
      if j < 0 then begin
        Result := FNodeInfoList[i];
        Result.NodeIndex := Index; // only set copy on result
        exit;
      end;
      inc(i);
    end;
  end;

  Result.FoldAction := [sfaInvalid];
  Result.LineIndex := Line;
  Result.NodeIndex := -1;
end;


end.

