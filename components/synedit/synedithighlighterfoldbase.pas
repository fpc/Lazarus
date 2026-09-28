{-------------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/
Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: SynEditHighlighter.pas, released 2000-04-07.

The Original Code is based on mwHighlighter.pas by Martin Waldenburg, part of
the mwEdit component suite.
Portions created by Martin Waldenburg are Copyright (C) 1998 Martin Waldenburg.
All Rights Reserved.

Contributors to the SynEdit and mwEdit projects are listed in the
Contributors.txt file.

$Id: synedithighlighter.pp 19051 2009-03-21 00:47:33Z martin $

You may retrieve the latest version of this file at the SynEdit home page,
located at http://SynEdit.SourceForge.net

-------------------------------------------------------------------------------}

(* Naming Conventions:
   -  FoldBlock:
     A continuous range of lines, that can (optional) be folded.
     Which Foldblocks can be folded is decided by the Highlighter. It may be
     configurable.
     A Foldblock can contain other Foldbloccks (nested), but two Foldblocks can
     not overlap.
   -  FoldBlockLevel (FoldBlockNestLevel):
     The amount of FoldBlocks in which a line (or a point of text) is.
   -  FoldGroup:
     An independent set of FoldBlocks. FoldBlocks in different Groups may overlap.
     (e.g. IFDEF/REGION in the SynPasSyn allow for overlaps, rather than strict nesting)
     Some older code use "FoldType" instead
   -  FoldNode
     Start or End of a FoldBlock
*)

(* TODO : Workaround for bug #20850
   Remove when FPC 2.6.2 is out
*)
{$IFDEF CPU64}
{$IF (FPC_FULLVERSION = 20600) or (FPC_FULLVERSION = 20501)}
  {$DEFINE ISSUE_20850 }
{$ENDIF}
{$ENDIF}

unit SynEditHighlighterFoldBase;

{$I synedit.inc}

interface

uses
  SysUtils, Classes, Math, AVL_Tree,
  // LazUtils
  LazClasses, LazLoggerBase, LazTracer,
  // LazEdit
  LazEditHighlighterFoldNodeHighlighter, LazEditHighlighterUtils, LazEditFoldHighlighter,
  // SynEdit
  SynEditHighlighter, SynEditTypes, LazSynEditText;

const
  NullRange = TSynEditRange(nil);

type

  TSynFoldAction  = LazEditFoldHighlighter.TSynFoldAction;
  TSynFoldActions = LazEditFoldHighlighter.TSynFoldActions;

  TSynFoldBlockFilterFlag  = LazEditFoldHighlighter.TSynFoldBlockFilterFlag;
  TSynFoldBlockFilterFlags = LazEditFoldHighlighter.TSynFoldBlockFilterFlags;
  TSynFoldBlockFilter      = LazEditFoldHighlighter.TSynFoldBlockFilter;

procedure InitFoldBlockFilter(out AFilter: TSynFoldBlockFilter;
                              AFoldGroup: Integer = 0; AFlag: TSynFoldBlockFilterFlags = []);

type
  TSynFoldNodeInfo = LazEditHighlighterFoldNodeHighlighter.TLazEditFoldNodeInfo deprecated 'use TLazEditFoldNodeInfo // will be removed in 5.99';
  PSynFoldNodeInfo = LazEditHighlighterFoldNodeHighlighter.PLazEditFoldNodeInfo deprecated 'use PLazEditFoldNodeInfo // will be removed in 5.99';
  TLazSynFoldNodeInfoList = LazEditHighlighterFoldNodeHighlighter.TLazEditFoldNodeInfoList deprecated 'use TLazEditFoldNodeInfoList // will be removed in 5.99';

  TLazSynEditNestedFoldsListEntry = LazEditFoldHighlighter.TLazEditNestedFoldsListEntry deprecated 'use LazEditFoldHighlighter.TLazEditNestedFoldsListEntry // will be removed in 5.99';
  TLazSynEditNestedFoldsList = LazEditFoldHighlighter.TLazEditNestedFoldsList deprecated 'use LazEditFoldHighlighter.TLazEditNestedFoldsList // will be removed in 5.99';

const
  sfaOpen               = LazEditFoldHighlighter.TSynFoldAction.sfaOpen               deprecated 'use LazEditFoldHighlighter.TSynFoldAction.sfaOpen // will be removed in 5.99';
  sfaClose              = LazEditFoldHighlighter.TSynFoldAction.sfaClose              deprecated 'use LazEditFoldHighlighter.TSynFoldAction.sfaClose // will be removed in 5.99';
  sfaFold               = LazEditFoldHighlighter.TSynFoldAction.sfaFold               deprecated 'use LazEditFoldHighlighter.TSynFoldAction.sfaFold // will be removed in 5.99';
  sfaFoldFold           = LazEditFoldHighlighter.TSynFoldAction.sfaFoldFold           deprecated 'use LazEditFoldHighlighter.TSynFoldAction.sfaFoldFold // will be removed in 5.99';
  sfaFoldHide           = LazEditFoldHighlighter.TSynFoldAction.sfaFoldHide           deprecated 'use LazEditFoldHighlighter.TSynFoldAction.sfaFoldHide // will be removed in 5.99';
  sfaMultiLine          = LazEditFoldHighlighter.TSynFoldAction.sfaMultiLine          deprecated 'use LazEditFoldHighlighter.TSynFoldAction.sfaMultiLine // will be removed in 5.99';
  sfaSingleLine         = LazEditFoldHighlighter.TSynFoldAction.sfaSingleLine         deprecated 'use LazEditFoldHighlighter.TSynFoldAction.sfaSingleLine // will be removed in 5.99';
  sfaCloseForNextLine   = LazEditFoldHighlighter.TSynFoldAction.sfaCloseForNextLine   deprecated 'use LazEditFoldHighlighter.TSynFoldAction.sfaCloseForNextLine // will be removed in 5.99';
  sfaLastLineClose      = LazEditFoldHighlighter.TSynFoldAction.sfaLastLineClose      deprecated 'use LazEditFoldHighlighter.TSynFoldAction.sfaLastLineClose // will be removed in 5.99';
  sfaCloseAndOpen       = LazEditFoldHighlighter.TSynFoldAction.sfaCloseAndOpen       deprecated 'use LazEditFoldHighlighter.TSynFoldAction.sfaCloseAndOpen // will be removed in 5.99';
  sfaDefaultCollapsed   = LazEditFoldHighlighter.TSynFoldAction.sfaDefaultCollapsed   deprecated 'use LazEditFoldHighlighter.TSynFoldAction.sfaDefaultCollapsed // will be removed in 5.99';
  sfaMarkup             = LazEditFoldHighlighter.TSynFoldAction.sfaMarkup             deprecated 'use LazEditFoldHighlighter.TSynFoldAction.sfaMarkup // will be removed in 5.99';
  sfaOutline            = LazEditFoldHighlighter.TSynFoldAction.sfaOutline            deprecated 'use LazEditFoldHighlighter.TSynFoldAction.sfaOutline // will be removed in 5.99';
  sfaOutlineKeepLevel   = LazEditFoldHighlighter.TSynFoldAction.sfaOutlineKeepLevel   deprecated 'use LazEditFoldHighlighter.TSynFoldAction.sfaOutlineKeepLevel // will be removed in 5.99';
  sfaOutlineMergeParent = LazEditFoldHighlighter.TSynFoldAction.sfaOutlineMergeParent deprecated 'use LazEditFoldHighlighter.TSynFoldAction.sfaOutlineMergeParent // will be removed in 5.99';
  sfaOutlineForceIndent = LazEditFoldHighlighter.TSynFoldAction.sfaOutlineForceIndent deprecated 'use LazEditFoldHighlighter.TSynFoldAction.sfaOutlineForceIndent // will be removed in 5.99';
  sfaInvalid            = LazEditFoldHighlighter.TSynFoldAction.sfaInvalid            deprecated 'use LazEditFoldHighlighter.TSynFoldAction.sfaInvalid // will be removed in 5.99';
  sfaOpenFold           = LazEditFoldHighlighter.TSynFoldAction.sfaOpenFold           deprecated 'use LazEditFoldHighlighter.TSynFoldAction.sfaOpenFold // will be removed in 5.99';
  sfaCloseFold          = LazEditFoldHighlighter.TSynFoldAction.sfaCloseFold          deprecated 'use LazEditFoldHighlighter.TSynFoldAction.sfaCloseFold // will be removed in 5.99';
  sfaOneLineOpen        = LazEditFoldHighlighter.TSynFoldAction.sfaOneLineOpen        deprecated 'use LazEditFoldHighlighter.TSynFoldAction.sfaOneLineOpen // will be removed in 5.99';
  sfaOneLineClose       = LazEditFoldHighlighter.TSynFoldAction.sfaOneLineClose       deprecated 'use LazEditFoldHighlighter.TSynFoldAction.sfaOneLineClose // will be removed in 5.99';

type
  TSynCustomFoldConfigMode  = LazEditFoldHighlighter.TSynCustomFoldConfigMode;
  TSynCustomFoldConfigModes = LazEditFoldHighlighter.TSynCustomFoldConfigModes;

  { TSynCustomFoldConfig }

  TSynCustomFoldConfig = class(TLazEditCustomFoldConfig)
  private
    FIsEssential: boolean;
    FOnChange: TNotifyEvent;
  protected
    procedure DoOnChange; override;
  public
    constructor Create;
    constructor Create(ASupportedModes: TSynCustomFoldConfigModes; AnIsEssential: Boolean = False);

    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property IsEssential: boolean read FIsEssential;   // create node, even if disabled
    property SupportedModes: TSynCustomFoldConfigModes read FSupportedModes write SetSupportedModes;
  end;

  PSynCustomFoldConfig = ^TSynCustomFoldConfig;

  { TSynCustomCodeFoldBlock }

  TSynCustomCodeFoldBlock = class(TLazHighlighterRangeForDictionary)
  private
    FBlockType: Pointer;
    FParent: TSynCustomCodeFoldBlock;
  public
    constructor Create; overload;
    procedure Assign(Src: TLazHighlighterRange); override;
  public
    procedure InitRootBlockType(AType: Pointer);
    property BlockType: Pointer read FBlockType;
    property Parent: TSynCustomCodeFoldBlock read FParent;
  end;
  TSynCustomCodeFoldBlockClass = class of TSynCustomCodeFoldBlock;

  { TSynCustomHighlighterRange }

  TSynCustomHighlighterRange = class(TLazHighlighterRangeForDictionary)
  private
    // TODO: either reduce to one level, or create subclass for 2nd level
    FCodeFoldStackSize: integer; // EndLevel
    FNestFoldStackSize: integer; // EndLevel
    FMinimumCodeFoldBlockLevel: integer;
    FMinimumNestFoldBlockLevel: integer;
    FRangeType: Pointer;
    FTop: TSynCustomCodeFoldBlock;
  public
    constructor Create(Template: TLazHighlighterRange); override;
    destructor Destroy; override;
    function Compare(Range: TLazHighlighterRange): integer; override;
    function Push(ABlock: TSynCustomCodeFoldBlock; IncreaseLevel: Boolean = True):
        TSynCustomCodeFoldBlock; inline;
    procedure Pop(DecreaseLevel: Boolean = True); inline;
    function MaxFoldLevel: Integer; virtual;
    procedure Clear; virtual;
    procedure Assign(ASrc: TLazHighlighterRange); override;
    procedure WriteDebugReport;
    property FoldRoot: TSynCustomCodeFoldBlock read FTop write FTop;
  public
    property RangeType: Pointer read FRangeType write FRangeType;
    property CodeFoldStackSize: integer read FCodeFoldStackSize; // excl disabled, only IncreaseLevel
    property MinimumCodeFoldBlockLevel: integer
      read FMinimumCodeFoldBlockLevel write FMinimumCodeFoldBlockLevel;
    property NestFoldStackSize: integer read FNestFoldStackSize; // all, incl disabled (not IncreaseLevel)
    property MinimumNestFoldBlockLevel: integer
      read FMinimumNestFoldBlockLevel; // write FMinimumNestFoldBlockLevel;
    property Top: TSynCustomCodeFoldBlock read FTop;
  end;
  TSynCustomHighlighterRangeClass = class of TSynCustomHighlighterRange deprecated 'use TLazHighlighterRangeClass // will be removed in 5.99';

  { TSynCustomFoldHighlighter }

  TSynCustomFoldHighlighter = class(TSynCustomHighlighter)
  private const
    MAX_UFOLD_CAPACITY = 16;
  private type
    TUncommittedFold = record
      FBlockType: Pointer;
      FIncreaseLevel: Boolean;
    end;
  protected
    // Fold Config
    FFoldConfig: Array of TSynCustomFoldConfig;
    function GetFoldConfig(Index: Integer): TSynCustomFoldConfig; reintroduce; virtual;
    procedure SetFoldConfig(Index: Integer; const AValue: TSynCustomFoldConfig); reintroduce; virtual;
    function GetFoldConfigCount: Integer; override;
    function GetFoldConfigInternalCount: Integer; virtual;
    function CreateFoldConfigInstance(Index: Integer): TSynCustomFoldConfig; virtual;
    function GetFoldConfigInstance(Index: Integer): TSynCustomFoldConfig; virtual;
    procedure InitFoldConfig;
    procedure DestroyFoldConfig;
    procedure DoFoldConfigChanged(Sender: TObject); virtual;
  private
    FCodeFoldRange: TSynCustomHighlighterRange;
    FUncommittedFolds: array of TUncommittedFold;
    FUncommittedFoldStackCount, FUncommittedFoldNestCount: integer;
    FIsCollectingNodeInfo: boolean;
    FRanges: TLazHighlighterRangesDictionary;
    FFoldBlockRanges: TLazHighlighterRangesDictionary;
    FRootCodeFoldBlock, FTepmCodeFoldBlock: TSynCustomCodeFoldBlock;
    FCollectingNodeInfoList: TLazEditFoldNodeInfoList;
    procedure CommitUncommittedFolds; inline;
  protected
    // "Range"
    function GetRangeClass: TLazHighlighterRangeClass; virtual;
    procedure CreateRootCodeFoldBlock; virtual; // set RootCodeFoldBlock
    property CodeFoldRange: TSynCustomHighlighterRange read FCodeFoldRange;
    function TopCodeFoldBlockType(DownIndex: Integer = 0): Pointer;
    property RootCodeFoldBlock: TSynCustomCodeFoldBlock read FRootCodeFoldBlock
      write FRootCodeFoldBlock;

    // Open/Close Folds
    function StartCodeFoldBlock(ABlockType: Pointer = nil;
              IncreaseLevel: Boolean = true; ForceDisabled: Boolean = False): Boolean; virtual;
    procedure EndCodeFoldBlock(DecreaseLevel: Boolean = True); virtual;
    procedure CollectNodeInfo(FinishingABlock : Boolean; ABlockType: Pointer;
              LevelChanged: Boolean); virtual;
    procedure DoInitNode(var Node: TSynFoldNodeInfo;
                       FinishingABlock: Boolean;
                       ABlockType: Pointer; aActions: TSynFoldActions;
                       AIsFold: Boolean); virtual;
    procedure RepairSingleLineNode(var Node: TSynFoldNodeInfo); virtual;
    procedure GetTokenBounds(out LogX1,LogX2: Integer); virtual;

    // Info about Folds
    procedure ScanFoldNodeInfo(); virtual;
    procedure InitFoldNodeInfo(AList: TLazEditFoldNodeInfoList; Line: TLineIdx); override;

    // Info about Folds, on currently set line/range (simply forwarding to range
    function MinimumCodeFoldBlockLevel: integer; inline;
    function CurrentCodeFoldBlockLevel: integer; inline;
    function CurrentCodeNestBlockLevel: integer; inline;

    property IsCollectingNodeInfo : boolean read FIsCollectingNodeInfo;
    property CollectingNodeInfoList : TLazEditFoldNodeInfoList read FCollectingNodeInfoList;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    class function GetCapabilities: TSynHighlighterCapabilities; override;
    function GetRange: Pointer; override;

    // Info about Folds
    function FoldBlockEndLevel(ALineIndex: TLineIdx;
                               const AFilter: TSynFoldBlockFilter): integer; override; overload;
    function FoldBlockMinLevel(ALineIndex: TLineIdx;
                               const AFilter: TSynFoldBlockFilter): integer; override; overload;

    function FoldOpenCount(ALineIndex: Integer; AType: Integer = 0): integer;  deprecated;
    function FoldCloseCount(ALineIndex: Integer; AType: Integer = 0): integer; deprecated;
    function FoldNestCount(ALineIndex: Integer; AType: Integer = 0): integer; deprecated;

    function FoldEndLine(ALineIndex, FoldIndex: Integer): integer; virtual; overload; // deprecate // fix inherited classes

    procedure SetRange(Value: Pointer); override;
    procedure ResetRange; override;
    procedure InitForScanningLine; override;
    procedure DoCurrentLinesChanged; override;
    function DoPrepareLines(AFirstLineIdx: IntIdx; AMinimumRequiredLineIdx: IntIdx = - 1;
      AMaxTime: integer = 0): integer; override;

  public
    property FoldConfig[Index: Integer]: TSynCustomFoldConfig read GetFoldConfig write SetFoldConfig;
  end;

  { TSynCustomHighlighterRangeTree }

  TSynCustomHighlighterRangeTree = class(TLazHighlighterRanges)
  private
    FItems: TAvlTree;
  public
    constructor Create; override;
    destructor Destroy; override;
    function GetEqual(Range: TLazHighlighterRange
                      ): TLazHighlighterRange; override;
  end experimental; // replaced by TLazHighlighterRangesDictionary

function CompareSynHighlighterRanges(Data1, Data2: Pointer): integer;
function AllocateHighlighterRanges(
     HighlighterClass: TSynCustomHighlighterClass): TLazHighlighterRangesDictionary;
     deprecated 'use GetHighlighterRangesForHighlighter // will be removed in 5.99';

function dbgs(AMode: TSynCustomFoldConfigMode): String; overload;
function dbgs(AModes: TSynCustomFoldConfigModes): String; overload;

implementation

procedure InitFoldBlockFilter(out AFilter: TSynFoldBlockFilter; AFoldGroup: Integer;
  AFlag: TSynFoldBlockFilterFlags = []);
begin
  AFilter.FoldGroup := AFoldGroup;
  AFilter.Flags     := AFlag;
end;

function CompareSynHighlighterRanges(Data1, Data2: Pointer): integer;
var
  Range1: TSynCustomHighlighterRange;
  Range2: TSynCustomHighlighterRange;
begin
  Range1:=TSynCustomHighlighterRange(Data1);
  Range2:=TSynCustomHighlighterRange(Data2);
  Result:=Range1.Compare(Range2);
end;

function AllocateHighlighterRanges(
  HighlighterClass: TSynCustomHighlighterClass): TLazHighlighterRangesDictionary;
begin
  Result := TLazHighlighterRangesDictionary(GetHighlighterRangesForHighlighter(HighlighterClass, TLazHighlighterRangesDictionary));
  Result.AddReference;
end;

function dbgs(AMode: TSynCustomFoldConfigMode): String;
begin
  WriteStr(Result{%H-}, AMode);
end;

function dbgs(AModes: TSynCustomFoldConfigModes): String;
var
  i: TSynCustomFoldConfigMode;
  s: string;
begin
  Result:='';
  for i := low(TSynCustomFoldConfigMode) to high(TSynCustomFoldConfigMode) do
    if i in AModes then begin
      WriteStr(s{%H-}, i);
      Result := Result + s + ',';
    end;
  if Result <> '' then Result := '[' + copy(Result, 1, Length(Result)-1) + ']';
end;

{ TSynCustomFoldHighlighter }

constructor TSynCustomFoldHighlighter.Create(AOwner: TComponent);
begin
  SetLength(FFoldConfig, GetFoldConfigInternalCount);
  InitFoldConfig;
  fRanges := TLazHighlighterRangesDictionary(
    GetHighlighterRangesForHighlighter(TSynCustomHighlighterClass(ClassType), TLazHighlighterRangesDictionary)
  );
  fRanges.AddReference;

  FFoldBlockRanges := TLazHighlighterRangesDictionary(
    GetHighlighterRangesForHighlighter(TSynCustomHighlighterClass(pointer(ClassType)+1), TLazHighlighterRangesDictionary)
  );
  FFoldBlockRanges.AddReference;

  CreateRootCodeFoldBlock;
  FTepmCodeFoldBlock := FRootCodeFoldBlock;
  FRootCodeFoldBlock := TSynCustomCodeFoldBlock(FFoldBlockRanges.GetEqual(FRootCodeFoldBlock));
  inherited Create(AOwner);
  FCodeFoldRange:=TSynCustomHighlighterRange(GetRangeClass.Create(nil));
  FCodeFoldRange.FoldRoot := FRootCodeFoldBlock;
  FUncommittedFoldStackCount := 0;
  FUncommittedFoldNestCount := 0;
end;

destructor TSynCustomFoldHighlighter.Destroy;
begin
  inherited Destroy;
  DestroyFoldConfig;
  FreeAndNil(FCodeFoldRange);
  FreeAndNil(FTepmCodeFoldBlock);
  fRanges.ReleaseReference;
  FFoldBlockRanges.ReleaseReference;
  FFoldConfig := nil;
end;

class function TSynCustomFoldHighlighter.GetCapabilities: TSynHighlighterCapabilities;
begin
  Result := inherited GetCapabilities + [hcCodeFolding];
end;

function TSynCustomFoldHighlighter.GetRange: Pointer;
begin
  // FCodeFoldRange is the working range and changed steadily
  // => return a fixed copy of the current CodeFoldRange instance,
  //    that can be stored by other classes (e.g. TSynEdit)
  CommitUncommittedFolds;
  Result:=fRanges.GetEqual(FCodeFoldRange);
end;

function TSynCustomFoldHighlighter.FoldBlockEndLevel(ALineIndex: TLineIdx;
  const AFilter: TSynFoldBlockFilter): integer;
var
  r: Pointer;
begin
  Assert(CurrentRanges <> nil, 'TSynCustomFoldHighlighter.FoldBlockEndLevel requires CurrentRanges');
  if (ALineIndex < 0) or (ALineIndex >= CurrentLines.Count - 1) then
    exit(0);
  r := CurrentRanges[ALineIndex];
  if (r <> nil) and (r <> NullRange) then begin
    if sfbIncludeDisabled in AFilter.Flags then
      Result := TSynCustomHighlighterRange(r).NestFoldStackSize
    else
      Result := TSynCustomHighlighterRange(r).CodeFoldStackSize;
  end
  else
    Result:=0;
end;

function TSynCustomFoldHighlighter.FoldBlockMinLevel(ALineIndex: TLineIdx;
  const AFilter: TSynFoldBlockFilter): integer;
var
  r: Pointer;
begin
  Assert(CurrentRanges <> nil, 'TSynCustomFoldHighlighter.FoldBlockMinLevelrequires CurrentRanges');
  if (ALineIndex < 0) or (ALineIndex >= CurrentLines.Count - 1) then
    exit(0);
  r := CurrentRanges[ALineIndex];
  if (r <> nil) and (r <> NullRange) then begin
    if sfbIncludeDisabled in AFilter.Flags then
      Result := TSynCustomHighlighterRange(r).MinimumNestFoldBlockLevel
    else
      Result := TSynCustomHighlighterRange(r).MinimumCodeFoldBlockLevel
  end
  else
    Result:=0;
end;

procedure TSynCustomFoldHighlighter.ResetRange;
begin
  FCodeFoldRange.Clear;
  FCodeFoldRange.FoldRoot := FRootCodeFoldBlock;
  FUncommittedFoldStackCount := 0;
  FUncommittedFoldNestCount := 0;
end;

function TSynCustomFoldHighlighter.MinimumCodeFoldBlockLevel: integer;
begin
  assert(FCodeFoldRange <> nil, 'MinimumCodeFoldBlockLevel requires FCodeFoldRange');
  Result := FCodeFoldRange.MinimumCodeFoldBlockLevel;
end;

procedure TSynCustomFoldHighlighter.SetRange(Value: Pointer);
begin
  FUncommittedFoldStackCount := 0;
  FUncommittedFoldNestCount := 0;
  FCodeFoldRange.Assign(TSynCustomHighlighterRange(Value));
  // in case we asigned a null range
  if not assigned(FCodeFoldRange.FoldRoot) then
    FCodeFoldRange.FoldRoot := FRootCodeFoldBlock;
end;

procedure TSynCustomFoldHighlighter.InitForScanningLine;
begin
  inherited;
  FCodeFoldRange.MinimumCodeFoldBlockLevel := FCodeFoldRange.FCodeFoldStackSize;
  FCodeFoldRange.FMinimumNestFoldBlockLevel := FCodeFoldRange.NestFoldStackSize;
end;

procedure TSynCustomFoldHighlighter.DoCurrentLinesChanged;
begin
  inherited DoCurrentLinesChanged;
  ClearFoldNodeList;
end;

function TSynCustomFoldHighlighter.DoPrepareLines(AFirstLineIdx: IntIdx;
  AMinimumRequiredLineIdx: IntIdx; AMaxTime: integer): integer;
begin
  ClearFoldNodeList;
  Result := inherited DoPrepareLines(AFirstLineIdx, AMinimumRequiredLineIdx, AMaxTime);

  FUncommittedFoldStackCount := 0;
  FUncommittedFoldNestCount := 0;
  if Length(FUncommittedFolds) > MAX_UFOLD_CAPACITY then
    SetLength(FUncommittedFolds, MAX_UFOLD_CAPACITY);
end;

function TSynCustomFoldHighlighter.CurrentCodeFoldBlockLevel: integer;
begin
  assert(FCodeFoldRange <> nil, 'CurrentCodeFoldBlockLevel requires FCodeFoldRange');
  Result := FCodeFoldRange.CodeFoldStackSize + FUncommittedFoldStackCount;
end;

function TSynCustomFoldHighlighter.CurrentCodeNestBlockLevel: integer;
begin
  assert(FCodeFoldRange <> nil, 'CurrentCodeNestBlockLevel requires FCodeFoldRange');
  Result := FCodeFoldRange.NestFoldStackSize + FUncommittedFoldNestCount;
end;

function TSynCustomFoldHighlighter.FoldOpenCount(ALineIndex: Integer; AType: Integer = 0): integer;
begin
  result := FoldBlockOpeningCount(ALineIndex, AType);
end;

function TSynCustomFoldHighlighter.FoldCloseCount(ALineIndex: Integer; AType: Integer = 0): integer;
begin
  result := FoldBlockClosingCount(ALineIndex, AType);
end;

function TSynCustomFoldHighlighter.FoldNestCount(ALineIndex: Integer; AType: Integer = 0): integer;
begin
  Result := FoldBlockEndLevel(ALineIndex, AType);
end;

function TSynCustomFoldHighlighter.FoldEndLine(ALineIndex, FoldIndex: Integer): integer;
begin
  Result := FoldEndLine(ALineIndex, FoldIndex, 0, []);
end;

function TSynCustomFoldHighlighter.GetFoldConfig(Index: Integer): TSynCustomFoldConfig;
begin
  Result := FFoldConfig[Index];
end;

procedure TSynCustomFoldHighlighter.SetFoldConfig(Index: Integer; const AValue: TSynCustomFoldConfig);
begin
  BeginUpdate;
  FFoldConfig[Index].Assign(AValue);
  EndUpdate;
end;

function TSynCustomFoldHighlighter.GetFoldConfigCount: Integer;
begin
  Result := 0;
end;

function TSynCustomFoldHighlighter.GetFoldConfigInternalCount: Integer;
begin
  Result := 0;
end;

function TSynCustomFoldHighlighter.CreateFoldConfigInstance(Index: Integer
  ): TSynCustomFoldConfig;
begin
  Result := TSynCustomFoldConfig.Create;
end;

function TSynCustomFoldHighlighter.GetFoldConfigInstance(Index: Integer): TSynCustomFoldConfig;
begin
  Result := CreateFoldConfigInstance(Index);
  Result.Enabled := False;
end;

procedure TSynCustomFoldHighlighter.InitFoldConfig;
var
  i: Integer;
begin
  for i := 0 to high(FFoldConfig) do begin
    FFoldConfig[i] := GetFoldConfigInstance(i);
    if not assigned(FFoldConfig[i].OnChange) then
      FFoldConfig[i].OnChange := @DoFoldConfigChanged;
  end;
end;

procedure TSynCustomFoldHighlighter.DestroyFoldConfig;
var
  i: Integer;
begin
  for i := 0 to high(FFoldConfig) do
    FFoldConfig[i].Free;
end;

procedure TSynCustomFoldHighlighter.DoFoldConfigChanged(Sender: TObject);
begin
  RequestFullRescan;
end;

procedure TSynCustomFoldHighlighter.CommitUncommittedFolds;
var
  i: Integer;
  b, p: TSynCustomCodeFoldBlock;
begin
  p := FCodeFoldRange.FTop;
  for i := 0 to FUncommittedFoldNestCount - 1 do begin
    FTepmCodeFoldBlock.FBlockType := FUncommittedFolds[i].FBlockType;
    FTepmCodeFoldBlock.FParent := p;
    p := TSynCustomCodeFoldBlock(FFoldBlockRanges.GetEqual(FTepmCodeFoldBlock));
    FCodeFoldRange.Push(p, FUncommittedFolds[i].FIncreaseLevel);  // TODO: may fail
  end;

  FUncommittedFoldStackCount := 0;
  FUncommittedFoldNestCount := 0;
end;

procedure TSynCustomFoldHighlighter.ScanFoldNodeInfo;
begin
  NextToEol;
end;

procedure TSynCustomFoldHighlighter.InitFoldNodeInfo(AList: TLazEditFoldNodeInfoList; Line: TLineIdx);
begin
  FIsCollectingNodeInfo := True;
  try
    assert(FCollectingNodeInfoList = nil, 'TSynCustomFoldHighlighter.InitFoldNodeInfo: FCollectingNodeInfoList = nil');
    FCollectingNodeInfoList := AList;
    StartAtLineIndex(Line);
    ScanFoldNodeInfo();
  finally
    FIsCollectingNodeInfo := False;
    FCollectingNodeInfoList := nil;
  end;
end;

function TSynCustomFoldHighlighter.GetRangeClass: TLazHighlighterRangeClass;
begin
  Result:=TSynCustomHighlighterRange;
end;

function TSynCustomFoldHighlighter.TopCodeFoldBlockType(DownIndex: Integer = 0): Pointer;
var
  Fold: TSynCustomCodeFoldBlock;
begin
  Result:=nil;
  if DownIndex < FUncommittedFoldNestCount then begin
    Result := FUncommittedFolds[FUncommittedFoldNestCount-1 - DownIndex].FBlockType;
    exit;
  end;

  DownIndex := DownIndex - FUncommittedFoldNestCount;
  if (CodeFoldRange<>nil) then begin
    Fold := CodeFoldRange.Top;
    while (Fold <> nil) and (DownIndex > 0) do begin
      Fold := Fold.Parent;
      dec(DownIndex);
    end;
    if Fold <> nil then
      Result := Fold.BlockType
  end;
end;

procedure TSynCustomFoldHighlighter.GetTokenBounds(out LogX1, LogX2: Integer);
var p : pchar; L : integer;
begin
  GetTokenEx(p,L);
  LogX1 := GetTokenPos;
  LogX2 := LogX1 + L ;
end;

function TSynCustomFoldHighlighter.StartCodeFoldBlock(ABlockType: Pointer; IncreaseLevel: Boolean;
  ForceDisabled: Boolean): Boolean;
begin
  Result := False;
  if (PtrUInt(ABlockType) < FoldConfigCount) and (not ForceDisabled) and
     (not FoldConfig[PtrUInt(ABlockType)].Enabled) and
     (not FoldConfig[PtrUInt(ABlockType)].IsEssential)
  then
    exit;

  Result := True;

  if FIsCollectingNodeInfo then
    CollectNodeInfo(False, ABlockType, IncreaseLevel);

  if Length(FUncommittedFolds) <= FUncommittedFoldNestCount then
    SetLength(FUncommittedFolds, FUncommittedFoldNestCount + 4);
  FUncommittedFolds[FUncommittedFoldNestCount].FBlockType := ABlockType;
  FUncommittedFolds[FUncommittedFoldNestCount].FIncreaseLevel := IncreaseLevel;
  inc(FUncommittedFoldNestCount);
  if IncreaseLevel then
    inc(FUncommittedFoldStackCount);
//  CodeFoldRange.Add(ABlockType, IncreaseLevel);
end;

procedure TSynCustomFoldHighlighter.EndCodeFoldBlock(DecreaseLevel: Boolean = True);
var
  BlockType: Pointer;
begin
  //ABlockType required for detect whether singleline /multiline is being paired
  BlockType := TopCodeFoldBlockType;
  if FIsCollectingNodeInfo then
    CollectNodeInfo(True, BlockType, DecreaseLevel);

  if FUncommittedFoldNestCount > 0 then begin
    dec(FUncommittedFoldNestCount);
    if DecreaseLevel then
      dec(FUncommittedFoldStackCount);
  end
  else
    CodeFoldRange.Pop(DecreaseLevel);
end;

procedure TSynCustomFoldHighlighter.CollectNodeInfo(FinishingABlock: Boolean;
  ABlockType: Pointer; LevelChanged: Boolean);
var
  //DecreaseLevel,
  BlockTypeEnabled, BlockConfExists: Boolean;
  act: TSynFoldActions;
  nd: TSynFoldNodeInfo;
begin
  if not IsCollectingNodeInfo then exit;

  BlockConfExists := (PtrUInt(ABlockType) < FoldConfigCount);  // how about pascal that has blocktype > foldconfigcount?
  //BlockConfExists := HasFoldConfig(PtrUInt(ABlockType));
  BlockTypeEnabled := False;
  if BlockConfExists then
    BlockTypeEnabled := FoldConfig[PtrUInt(ABlockType)].Enabled;

  //Start
  if not FinishingABlock then
  begin
    act := [sfaOpen, sfaOpenFold]; // todo deprecate sfaOpenFold
    if BlockTypeEnabled then
      act := act + FoldConfig[PtrUInt(ABlockType)].FoldActions
    else
    if not BlockConfExists then
      act := act + [sfaFold,sfaFoldFold, sfaMarkup, sfaOutline];
  end
  else
  //Finish
  begin
    act := [sfaClose, sfaCloseFold]; // todo deprecate sfaCloseFold
    if BlockTypeEnabled then
      act := act + FoldConfig[PtrUInt(ABlockType)].FoldActions
    else
    if not BlockConfExists then
      act := act + [sfaFold, sfaFoldFold, sfaMarkup, sfaOutline];
    act := act - [sfaFoldFold, sfaFoldHide]; // it is closing tag
  end;

  DoInitNode(nd{%H-}, FinishingABlock, ABlockType, act, LevelChanged);
  FCollectingNodeInfoList.Add(nd);
end;

procedure TSynCustomFoldHighlighter.DoInitNode(var Node: TSynFoldNodeInfo;
  FinishingABlock: Boolean; ABlockType: Pointer;
  aActions: TSynFoldActions; AIsFold: Boolean);
var
  OneLine: Boolean;
  EndOffs: Integer;
  LogX1, LogX2: Integer;

begin
  GetTokenBounds(LogX1, LogX2);

  aActions := aActions + [sfaMultiLine];
  if FinishingABlock then
    EndOffs := -1
  else
    EndOffs := +1;
  Node.LineIndex := LineIndex;
  Node.LogXStart := LogX1;
  Node.LogXEnd := LogX2;
  Node.FoldType := ABlockType;
  Node.FoldTypeCompatible := ABlockType;
  Node.FoldAction := aActions;
  node.FoldGroup := 1;//FOLDGROUP_PASCAL;
  Node.FoldLvlStart := CodeFoldRange.CodeFoldStackSize + FUncommittedFoldStackCount; // If "not AIsFold" then the node has no foldlevel of its own
  Node.NestLvlStart := CodeFoldRange.NestFoldStackSize + FUncommittedFoldNestCount;
  OneLine := FinishingABlock and (Node.FoldLvlStart > CodeFoldRange.MinimumCodeFoldBlockLevel);
  Node.NestLvlEnd := Node.NestLvlStart + EndOffs;
  if not (sfaFold in aActions) then
    EndOffs := 0;
  Node.FoldLvlEnd := Node.FoldLvlStart + EndOffs;
  if OneLine then  // find opening node
    RepairSingleLineNode(Node);
end;

procedure TSynCustomFoldHighlighter.RepairSingleLineNode(var Node: TSynFoldNodeInfo);
var
  nd: PLazEditFoldNodeInfo;
  i : integer;
begin
    i := FCollectingNodeInfoList.CountAll - 1;
    nd := FCollectingNodeInfoList.ItemPointer[i];
    while (i >= 0) and
          ( (nd^.FoldType <> node.FoldType) or
            (nd^.FoldGroup <> node.FoldGroup) or
            (not (sfaOpenFold in nd^.FoldAction))
            or (nd^.FoldLvlEnd <> Node.FoldLvlStart)
          )
    do begin
      dec(i);
      nd := FCollectingNodeInfoList.ItemPointer[i];
    end;
    if i >= 0 then begin
      nd^.FoldAction  := nd^.FoldAction + [sfaOneLineOpen, sfaSingleLine] - [sfaMultiLine];
      Node.FoldAction := Node.FoldAction + [sfaOneLineClose, sfaSingleLine] - [sfaMultiLine];
      if (sfaFoldHide in nd^.FoldAction) then begin
        assert(sfaFold in nd^.FoldAction, 'sfaFoldHide without sfaFold');
        // one liner: hide-able / not fold-able
        nd^.FoldAction  := nd^.FoldAction - [sfaFoldFold];
        Node.FoldAction := Node.FoldAction - [sfaFoldFold];
      end else begin
        // one liner: nether hide-able nore fold-able
        nd^.FoldAction  := nd^.FoldAction - [sfaOpenFold, sfaFold, sfaFoldFold];
        Node.FoldAction := Node.FoldAction - [sfaCloseFold, sfaFold, sfaFoldFold];
      end;
    end;
end;

procedure TSynCustomFoldHighlighter.CreateRootCodeFoldBlock;
begin
  FRootCodeFoldBlock := TSynCustomCodeFoldBlock.Create;
end;

{ TSynCustomCodeFoldBlock }

constructor TSynCustomCodeFoldBlock.Create;
begin
  inherited Create(nil);
end;

procedure TSynCustomCodeFoldBlock.Assign(Src: TLazHighlighterRange);
var
  TheSrc: TSynCustomCodeFoldBlock absolute Src;
begin
  if Src is TSynCustomCodeFoldBlock then begin
    FBlockType := TheSrc.FBlockType;
    FParent := TheSrc.FParent;
  end;
end;

procedure TSynCustomCodeFoldBlock.InitRootBlockType(AType: Pointer);
begin
  if assigned(FParent) then
    raise Exception.Create('Attempt to modify a FoldBlock');
  FBlockType := AType;
end;

{ TSynCustomHighlighterRange }

constructor TSynCustomHighlighterRange.Create(
  Template: TLazHighlighterRange);
begin
  if (Template<>nil) and (ClassType<>Template.ClassType) then
    RaiseGDBException('');
  if Template<>nil then
    Assign(Template);
end;

destructor TSynCustomHighlighterRange.Destroy;
begin
  Clear;
  inherited Destroy;
end;

function TSynCustomHighlighterRange.Compare(Range: TLazHighlighterRange): integer;
begin
  Result := DoCompare(Range, TSynCustomHighlighterRange.InstanceSize);
end;

function TSynCustomHighlighterRange.Push(ABlock: TSynCustomCodeFoldBlock; IncreaseLevel: Boolean
  ): TSynCustomCodeFoldBlock;
var
  i: LongInt;
begin
  i := MaxFoldLevel;
  if (i > 0) and (FCodeFoldStackSize >= i) then begin
    //debugln('Reached MaxFoldLevel, ignoring folds');
    exit(nil);
  end;
  inc(FNestFoldStackSize);
  if IncreaseLevel then
    inc(FCodeFoldStackSize);
  FTop:=ABlock;
  Result := ABlock;
end;

procedure TSynCustomHighlighterRange.Pop(DecreaseLevel: Boolean = True);
// can be called, even if there is no stack
// because it's normal that sources under development have unclosed blocks
begin
  //debugln('TSynCustomHighlighterRange.Pop');
  if assigned(FTop.Parent) then begin
    FTop := FTop.Parent;
    dec(FNestFoldStackSize);
    if FMinimumNestFoldBlockLevel > FNestFoldStackSize then
      FMinimumNestFoldBlockLevel := FNestFoldStackSize;
    if DecreaseLevel then begin
      dec(FCodeFoldStackSize);
      if FMinimumCodeFoldBlockLevel > FCodeFoldStackSize then
        FMinimumCodeFoldBlockLevel := FCodeFoldStackSize;
    end;
  end;
end;

function TSynCustomHighlighterRange.MaxFoldLevel: Integer;
begin
  Result := -1;
end;

procedure TSynCustomHighlighterRange.Clear;
begin
  FRangeType:=nil;
  FCodeFoldStackSize := 0;
  FNestFoldStackSize := 0;
  FMinimumCodeFoldBlockLevel := 0;
  FMinimumNestFoldBlockLevel:= 0;
  FTop:=nil;
end;

procedure TSynCustomHighlighterRange.Assign(ASrc: TLazHighlighterRange);
var Src: TSynCustomHighlighterRange absolute ASrc;
begin
  if (Src<>nil) and (Src<>TSynCustomHighlighterRange(NullRange)) then begin
    FTop := Src.FTop;
    FCodeFoldStackSize := Src.FCodeFoldStackSize;
    FMinimumCodeFoldBlockLevel := Src.FMinimumCodeFoldBlockLevel;
    FNestFoldStackSize := Src.FNestFoldStackSize;
    FMinimumNestFoldBlockLevel := Src.FMinimumNestFoldBlockLevel;
    FRangeType := Src.FRangeType;
  end
  else begin
    FTop := nil;
    FCodeFoldStackSize := 0;
    FNestFoldStackSize := 0;
    FMinimumCodeFoldBlockLevel := 0;
    FMinimumNestFoldBlockLevel := 0;
    FRangeType := nil;
  end;
end;

procedure TSynCustomHighlighterRange.WriteDebugReport;
begin
  debugln('TSynCustomHighlighterRange.WriteDebugReport ',DbgSName(Self),
    ' RangeType=',dbgs(RangeType),' StackSize=',dbgs(CodeFoldStackSize));
  debugln(' Block=',dbgs(PtrInt(FTop)));
end;

{ TSynCustomHighlighterRangeTree }

constructor TSynCustomHighlighterRangeTree.Create;
begin
  FItems:=TAvlTree.Create(@CompareSynHighlighterRanges);
  inherited Create;
end;

destructor TSynCustomHighlighterRangeTree.Destroy;
begin
  FItems.FreeAndClear;
  FreeAndNil(FItems);
  inherited Destroy;
end;

function TSynCustomHighlighterRangeTree.GetEqual(Range: TLazHighlighterRange): TLazHighlighterRange;
var
  Node: TAvlTreeNode;
begin
  if Range=nil then exit(nil);
  Node:=FItems.Find(Range);
  if Node<>nil then begin
    Result:=TSynCustomHighlighterRange(Node.Data);
  end else begin
    // add a copy
    Result:=TLazHighlighterRangeClass(Range.ClassType).Create(Range);
    FItems.Add(Result);
    //if FItems.Count mod 32 = 0 then debugln(['FOLDRANGE Count=', FItems.Count]);
  end;
  //debugln('TSynCustomHighlighterRangeTree.GetEqual A ',dbgs(Node),' ',dbgs(Result.Compare(Range)),' ',dbgs(Result.CodeFoldStackSize));
end;

{ TSynCustomFoldConfig }

procedure TSynCustomFoldConfig.DoOnChange;
begin
  if assigned(FOnChange) then
    FOnChange(self);
end;

constructor TSynCustomFoldConfig.Create;
begin
  Inherited;
  FIsEssential := True;
end;

constructor TSynCustomFoldConfig.Create(ASupportedModes: TSynCustomFoldConfigModes; AnIsEssential: Boolean);
begin
  inherited Create(ASupportedModes);
  FSupportedModes := ASupportedModes;
  FIsEssential := AnIsEssential;
end;


end.

