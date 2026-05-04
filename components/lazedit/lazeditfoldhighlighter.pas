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
unit LazEditFoldHighlighter;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Math,
  // LazEdit
  LazEditHighlighter, LazEditTypes;

type

  (* TSynFoldBlockFilter
     used to specify which folds to include for:
     - FoldOpenCount, FoldCloseCount, FoldNestCount
     - maybe in future TLazSynFoldNodeInfoList
       TLazSynFoldNodeInfoList has additional filters
       TLazSynFoldNodeInfoList always uses the full set (sfbIncludeDisabled)

     A Highlighter is not required to implement this, or can choose to implement
     a subset only. For any field/value a Highlighter may simple assume default.
     - Highlighter that have only one "FoldGroup" do not require this.
     - Highlighter that do not store foldblocks that are unavailable (e.g. off by
       config) always return the same set

     Using a record, as argument is the virtual methods, allows one to add further
     fields/values, without breaking inheritance.
     New fields values are expected to be ignored (handled as default) by existing
     highlighter.

     Callers of the method can:
     - use InitFoldBlockFilter to make sure all fields are set to default
     - use (none virtual) wrapper methods
  *)
  TSynFoldBlockFilterFlag = (
    sfbIncludeDisabled // Foldable by config = off
  );
  TSynFoldBlockFilterFlags = set of TSynFoldBlockFilterFlag;
  TSynFoldBlockFilter = record
    FoldGroup: integer;
    Flags: TSynFoldBlockFilterFlags;
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
                                         // eg an open node, matche exactly the previous node, which has to be a closing node of the same type and location (and vice versa for a closing node matching the next...)

                     sfaDefaultCollapsed,
                     sfaMarkup,   // This node can be highlighted, by the matching Word-Pair Markup
                     sfaOutline,  // This node will be higlighted by nested color replacing the token color
                     sfaOutlineKeepLevel, // Direct children should not increase color dept. (But grandchild can.)  e.g. "if","then" any "procedure"
                     sfaOutlineMergeParent,// This node want to decrease current color depth. (But Previous sibling increased) e.g. "except", "finally"
                     sfaOutlineForceIndent, // Node will temporary ignore sfaOutlineKeep. (Next sibling can.) e.g in NESTED "procedure"
// TODO: review sfaOutlineNoColor / see issue 0034410
                     sfaOutlineNoColor,     // Node will not painted by nested-coloring, but may increase color (e.g. any "procedure")
                     sfaOutlineNoLine,      // Node doesn't want to have vertical line. (e.g. "then")
                     sfaInvalid,  // Wrong Index

                     // TODO: deprecate
                     sfaOpenFold,     // At this node a new Fold can start // Actually, includes all,any multiline node too.
                     sfaCloseFold,    // At this node a fold ends
                     sfaOneLineOpen,   // Open, but closes on same line; *only* if hide-able has [sfaOpenFold, sfaFold]; always has [sfaFoldFold, sfaFoldHide]
                     sfaOneLineClose  // Open, but closes on same line;
                   );
  TSynFoldActions = set of TSynFoldAction;

  TSynCustomFoldConfigMode = (fmFold, fmHide, fmMarkup, fmOutline);
  TSynCustomFoldConfigModes = set of TSynCustomFoldConfigMode;

const
  {$WriteableConst off}
  FOLD_MODE_TO_ACTION: array [TSynCustomFoldConfigMode] of TSynFoldActions = (
    { fmFold }    [sfaFold, sfaFoldFold],
    { fmHide }    [sfaFold, sfaFoldHide],
    { fmMarkup }  [sfaMarkup],
    { fmOutline } [sfaOutline]
  );

type

  { TLazEditCustomFoldConfig }

  TLazEditCustomFoldConfig = class(TPersistent)
  private
    FEnabled: Boolean;
    FModes: TSynCustomFoldConfigModes;

    function  GetFoldActions: TSynFoldActions; inline;
    procedure SetEnabled(const AValue: Boolean);
    procedure SetModes(AValue: TSynCustomFoldConfigModes);
  protected
    FSupportedModes: TSynCustomFoldConfigModes; // TODO: private, as soon as the inherited class no longer needs the setter
    procedure SetSupportedModes(AValue: TSynCustomFoldConfigModes); deprecated 'use create';
    procedure DoOnChange; virtual;
  public
    constructor Create;
    constructor Create(ASupportedModes: TSynCustomFoldConfigModes);
    procedure Assign(Src: TLazEditCustomFoldConfig); reintroduce; virtual; // TODO: do not copy supported modes

    property SupportedModes: TSynCustomFoldConfigModes read FSupportedModes;
    property FoldActions: TSynFoldActions read GetFoldActions;     // Actions representing the modes
  published
    property Enabled: Boolean read FEnabled write SetEnabled;
    property Modes: TSynCustomFoldConfigModes read FModes write SetModes default [fmFold];
  end;

  //TLazEditCustomFoldHighlighter = class(TLazEditCustomHighlighter)
  TLazEditCustomFoldHighlighter = class(TLazEditCustomRangesHighlighter)
  protected
    // Fold Config
    function GetFoldConfig(Index: Integer): TLazEditCustomFoldConfig; virtual;
    procedure SetFoldConfig(Index: Integer; const AValue: TLazEditCustomFoldConfig); virtual;
    function GetFoldConfigCount: Integer; virtual;
  public
    (* * FoldBlockOpeningCount:
         Count of folds opening on the given line;
         (not including "one line" blocks that also close on the same line)
       * FoldBlockClosingCount:
         Count of folds that close/end on this line
       * FoldBlockEndLevel:
         Count of folds (including nested from prior lines) that are open
         at the end of the line.
       * FoldBlockMinLevel:
         Count of folds (from prior lines) that "continue through" this line
    *)
    // Info about Folds
    function FoldBlockOpeningCount(ALineIndex: TLineIdx; const AFilter: TSynFoldBlockFilter): integer; virtual; overload;
    function FoldBlockClosingCount(ALineIndex: TLineIdx; const AFilter: TSynFoldBlockFilter): integer; virtual; overload;
    function FoldBlockEndLevel(ALineIndex: TLineIdx; const AFilter: TSynFoldBlockFilter): integer; virtual; overload;
    function FoldBlockMinLevel(ALineIndex: TLineIdx; const AFilter: TSynFoldBlockFilter): integer; virtual; overload;

    function FoldBlockOpeningCount(const ALineIndex: TLineIdx; const AFoldGroup: integer = 0;
                                   const AFlags: TSynFoldBlockFilterFlags = []): integer; inline; overload;
    function FoldBlockClosingCount(const ALineIndex: TLineIdx; const AFoldGroup: integer = 0;
                                   const AFlags: TSynFoldBlockFilterFlags = []): integer; inline; overload;
    function FoldBlockEndLevel(const ALineIndex: TLineIdx; const AFoldGroup: integer = 0;
                               const AFlags: TSynFoldBlockFilterFlags = []): integer; inline; overload;
    function FoldBlockMinLevel(const ALineIndex: TLineIdx; const AFoldGroup: integer = 0;
                               const AFlags: TSynFoldBlockFilterFlags = []): integer; inline; overload;

    function FindNextLineWithMinFoldLevel(ALineIndex: TLineIdx; ASearchLevel: Integer; const AFilter: TSynFoldBlockFilter): integer; virtual; overload;
    function FindNextLineWithMinFoldLevel(const ALineIndex: TLineIdx; const ASearchLevel: Integer;
                               const AFoldGroup: integer = 0; const AFlags: TSynFoldBlockFilterFlags = []): integer; inline; overload;

    function FoldEndLine(ALineIndex, FoldIndex: Integer; const AFilter: TSynFoldBlockFilter): integer; virtual; overload;
    function FoldEndLine(const ALineIndex, FoldIndex: Integer;
                         const AFoldGroup: integer; const AFlags: TSynFoldBlockFilterFlags=[]): integer; inline; overload;

    function FoldLineLength(ALineIndex, FoldIndex: Integer): integer; virtual;  // only for group 0 // may be one less than FoldEndLine, if end line is a mixed end-begin

    (* ***
       *** Scanning nodes in a line
       ***
     *)

    (* FoldBlockNestedTypes:
       All nested FoldType (cfbtBegin) if available. Similar to TopCodeFoldBlockType
       - Index=0 is most outer / Index=FoldBlockEndLevel is most inner (TopCodeFoldBlockType 0=inner)
       - False, if it can not be determined for the filter settings
    *)
    function FoldBlockNestedTypes(ALineIndex: TLineIdx; ANestIndex: Integer; out AType: Pointer;
                                  const AFilter: TSynFoldBlockFilter): boolean; virtual; overload;

    function FoldBlockNestedTypes(const ALineIndex: TLineIdx; const ANestIndex: Integer; out AType: Pointer;
                                  const AFoldGroup: integer = 0;
                                  const AFlags: TSynFoldBlockFilterFlags = []): boolean; inline; overload;


    function FoldGroupCount: integer; virtual;
    function FoldGroupAtNodeIndex(ALineIndex, FoldIndex: Integer;
             UseCloseNodes: boolean = false): integer; virtual; // TODO: could be deprecated ./ only child-classes
    function FoldTypeCount: integer; virtual; deprecated 'Misnomer: Use FoldGroupCount';
    function FoldTypeAtNodeIndex(ALineIndex, FoldIndex: Integer;
             UseCloseNodes: boolean = false): integer; virtual; // TODO: could be deprecated ./ only child-classes
             deprecated 'Misnomer: Use FoldGroupAtNodeIndex';


    (* ***
       *** Config
       ***
     *)
    property FoldConfig[Index: Integer]: TLazEditCustomFoldConfig read GetFoldConfig write SetFoldConfig;
    property FoldConfigCount: Integer read GetFoldConfigCount;
  end;

  TLazEditCustomFoldRangesHighlighter = class(TLazEditCustomFoldHighlighter)
  end;

procedure InitFoldBlockFilter(out AFilter: TSynFoldBlockFilter;
                              const AFoldGroup: Integer = 0;
                              const AFlag: TSynFoldBlockFilterFlags = []
                             ); inline;

implementation

procedure InitFoldBlockFilter(out AFilter: TSynFoldBlockFilter; const AFoldGroup: Integer;
  const AFlag: TSynFoldBlockFilterFlags);
begin
  AFilter := Default(TSynFoldBlockFilter);
end;

{ TLazEditCustomFoldConfig }

procedure TLazEditCustomFoldConfig.SetEnabled(const AValue: Boolean);
begin
  if FEnabled = AValue then
    exit;
  FEnabled := AValue;
  DoOnChange;
end;

function TLazEditCustomFoldConfig.GetFoldActions: TSynFoldActions;
begin
  Result := [];
  if fmFold    in FModes then Result := Result + FOLD_MODE_TO_ACTION[fmFold];
  if fmHide    in FModes then Result := Result + FOLD_MODE_TO_ACTION[fmHide];
  if fmMarkup  in FModes then Result := Result + FOLD_MODE_TO_ACTION[fmMarkup];
  if fmOutline in FModes then Result := Result + FOLD_MODE_TO_ACTION[fmOutline];
end;

procedure TLazEditCustomFoldConfig.SetModes(AValue: TSynCustomFoldConfigModes);
begin
   AValue := AValue * FSupportedModes;
  if FModes = AValue then
    exit;
  FModes := AValue;
  DoOnChange;
end;

procedure TLazEditCustomFoldConfig.SetSupportedModes(AValue: TSynCustomFoldConfigModes);
begin
  if FSupportedModes = AValue then
    exit;
  FSupportedModes := AValue;
  if Modes <> Modes * FSupportedModes then
    Modes := Modes * FSupportedModes
  else
    DoOnChange;
end;

procedure TLazEditCustomFoldConfig.DoOnChange;
begin
  //
end;

constructor TLazEditCustomFoldConfig.Create;
begin
  inherited Create;
  FSupportedModes := [fmFold];
  Modes := [fmFold];
end;

constructor TLazEditCustomFoldConfig.Create(ASupportedModes: TSynCustomFoldConfigModes);
begin
  Create;
  FSupportedModes := ASupportedModes;
  FModes := FModes * ASupportedModes;
end;

procedure TLazEditCustomFoldConfig.Assign(Src: TLazEditCustomFoldConfig);
begin
  FEnabled        := Src.Enabled;
  FModes          := Src.Modes;
  FSupportedModes := Src.SupportedModes;
  DoOnChange;
end;

{ TLazEditCustomFoldHighlighter }

function TLazEditCustomFoldHighlighter.GetFoldConfig(Index: Integer): TLazEditCustomFoldConfig;
begin
  Result := nil;
end;

procedure TLazEditCustomFoldHighlighter.SetFoldConfig(Index: Integer;
  const AValue: TLazEditCustomFoldConfig);
begin
  //
end;

function TLazEditCustomFoldHighlighter.GetFoldConfigCount: Integer;
begin
  Result := 0;
end;

function TLazEditCustomFoldHighlighter.FoldBlockOpeningCount(ALineIndex: TLineIdx;
  const AFilter: TSynFoldBlockFilter): integer;
begin
  if (ALineIndex < 0) or (ALineIndex >= CurrentLines.Count) then
    exit(0);
  Result := FoldBlockEndLevel(ALineIndex, AFilter) - FoldBlockMinLevel(ALineIndex, AFilter);
end;

function TLazEditCustomFoldHighlighter.FoldBlockClosingCount(ALineIndex: TLineIdx;
  const AFilter: TSynFoldBlockFilter): integer;
begin
  if (ALineIndex < 0) or (ALineIndex >= CurrentLines.Count) then
    exit(0);
  Result := FoldBlockEndLevel(ALineIndex - 1, AFilter) - FoldBlockMinLevel(ALineIndex, AFilter);
end;

function TLazEditCustomFoldHighlighter.FoldBlockEndLevel(ALineIndex: TLineIdx;
  const AFilter: TSynFoldBlockFilter): integer;
begin
  Result := 0;
end;

function TLazEditCustomFoldHighlighter.FoldBlockMinLevel(ALineIndex: TLineIdx;
  const AFilter: TSynFoldBlockFilter): integer;
begin
  Result := 0;
end;

function TLazEditCustomFoldHighlighter.FoldBlockOpeningCount(const ALineIndex: TLineIdx;
  const AFoldGroup: integer; const AFlags: TSynFoldBlockFilterFlags): integer;
var
  Filter: TSynFoldBlockFilter;
begin
  Filter.FoldGroup := AFoldGroup;
  Filter.Flags := AFlags;
  Result := FoldBlockOpeningCount(ALineIndex, Filter);
end;

function TLazEditCustomFoldHighlighter.FoldBlockClosingCount(const ALineIndex: TLineIdx;
  const AFoldGroup: integer; const AFlags: TSynFoldBlockFilterFlags): integer;
var
  Filter: TSynFoldBlockFilter;
begin
  Filter.FoldGroup := AFoldGroup;
  Filter.Flags := AFlags;
  Result := FoldBlockClosingCount(ALineIndex, Filter);
end;

function TLazEditCustomFoldHighlighter.FoldBlockEndLevel(const ALineIndex: TLineIdx;
  const AFoldGroup: integer; const AFlags: TSynFoldBlockFilterFlags): integer;
var
  Filter: TSynFoldBlockFilter;
begin
  Filter.FoldGroup := AFoldGroup;
  Filter.Flags := AFlags;
  Result := FoldBlockEndLevel(ALineIndex, Filter);
end;

function TLazEditCustomFoldHighlighter.FoldBlockMinLevel(const ALineIndex: TLineIdx;
  const AFoldGroup: integer; const AFlags: TSynFoldBlockFilterFlags): integer;
var
  Filter: TSynFoldBlockFilter;
begin
  Filter.FoldGroup := AFoldGroup;
  Filter.Flags := AFlags;
  Result := FoldBlockMinLevel(ALineIndex, Filter);
end;

function TLazEditCustomFoldHighlighter.FindNextLineWithMinFoldLevel(ALineIndex: TLineIdx;
  ASearchLevel: Integer; const AFilter: TSynFoldBlockFilter): integer;
var
  cnt: Integer;
begin
  cnt := CurrentLines.Count - 1;
  Result := ALineIndex; // Can return the original line
  while (Result < cnt) and (FoldBlockMinLevel(Result, AFilter) > ASearchLevel) do inc(Result);
end;

function TLazEditCustomFoldHighlighter.FindNextLineWithMinFoldLevel(const ALineIndex: TLineIdx;
  const ASearchLevel: Integer; const AFoldGroup: integer; const AFlags: TSynFoldBlockFilterFlags
  ): integer;
var
  Filter: TSynFoldBlockFilter;
begin
  Filter.FoldGroup := AFoldGroup;
  Filter.Flags := AFlags;
  Result := FindNextLineWithMinFoldLevel(ALineIndex, ASearchLevel, Filter);
end;

function TLazEditCustomFoldHighlighter.FoldEndLine(ALineIndex, FoldIndex: Integer;
  const AFilter: TSynFoldBlockFilter): integer;
var
  lvl: Integer;
begin
  // max count;
  lvl := min(FoldBlockMinLevel(ALineIndex, AFilter) + 1 + FoldIndex,
             FoldBlockEndLevel(ALineIndex, AFilter)
            );
  Result := FindNextLineWithMinFoldLevel(ALineIndex+1, lvl-1, AFilter);
end;

function TLazEditCustomFoldHighlighter.FoldEndLine(const ALineIndex, FoldIndex: Integer;
  const AFoldGroup: integer; const AFlags: TSynFoldBlockFilterFlags): integer;
var
  Filter: TSynFoldBlockFilter;
begin
  Filter.FoldGroup := AFoldGroup;
  Filter.Flags := AFlags;
  Result := FoldEndLine(ALineIndex, FoldIndex, Filter);
end;

function TLazEditCustomFoldHighlighter.FoldLineLength(ALineIndex, FoldIndex: Integer): integer;
begin
  Result := FoldEndLine(ALineIndex, FoldIndex, 0);
  // check if fold last line of block (not mixed "end begin")
  if (FoldBlockEndLevel(Result) > FoldBlockMinLevel(Result)) then
    dec(Result);
  // Amount of lines, that will become invisible (excludes the cfCollapsed line)
  Result := Result - ALineIndex;
end;

function TLazEditCustomFoldHighlighter.FoldBlockNestedTypes(ALineIndex: TLineIdx;
  ANestIndex: Integer; out AType: Pointer; const AFilter: TSynFoldBlockFilter): boolean;
begin
  AType := nil;
  Result := False;
end;

function TLazEditCustomFoldHighlighter.FoldBlockNestedTypes(const ALineIndex: TLineIdx;
  const ANestIndex: Integer; out AType: Pointer; const AFoldGroup: integer;
  const AFlags: TSynFoldBlockFilterFlags): boolean;
var
  Filter: TSynFoldBlockFilter;
begin
  Filter.FoldGroup := AFoldGroup;
  Filter.Flags := AFlags;
  Result := FoldBlockNestedTypes(ALineIndex, ANestIndex, AType, Filter);
end;

function TLazEditCustomFoldHighlighter.FoldGroupCount: integer;
begin
  Result := FoldTypeCount;
end;

function TLazEditCustomFoldHighlighter.FoldGroupAtNodeIndex(ALineIndex, FoldIndex: Integer;
  UseCloseNodes: boolean): integer;
begin
  Result := FoldTypeAtNodeIndex(ALineIndex, FoldIndex, UseCloseNodes);
end;

function TLazEditCustomFoldHighlighter.FoldTypeCount: integer;
begin
  Result := 1;
end;

function TLazEditCustomFoldHighlighter.FoldTypeAtNodeIndex(ALineIndex, FoldIndex: Integer;
  UseCloseNodes: boolean): integer;
begin
  Result := 0;
end;

end.

