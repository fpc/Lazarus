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

This file was added to the Lazarus branch of SynEdit.
The original Author is M Friebe
}

{ This unit provide Highlighting for diff files
  Currently supported formats: Context, Unified
  //Todo: support original diff
}

unit SynHighlighterDiff;

{$I SynEdit.inc}

interface

uses
  Classes, Graphics, math,
  SynEditHighlighter, SynEditHighlighterFoldBase;

type
  TtkTokenKind = (tkNull, tkUnknown, tkSpace,
                  // File Header
                  tkFileOrigMark, tkFileNewMark,
                  tkFileOrig, tkFileNew,
                  // Chunk Header
                  tkChunkSeparator,
                  tkChunkMixedMark, tkChunkOrigMark, tkChunkNewMark,
                  tkChunkMixed, tkChunkOrig, tkChunkNew,
                  // Lines
                  tkLineRemovedMark, tkLineAddedMark, tkLineChangedMark, tkLineContextMark,
                  tkLineRemoved, tkLineAdded, tkLineChanged, tkLineContext
                 );

  TRangeState = (rsUnknown,
                 // Context
                 rsCtxFirst,
                   rsCtxUnknown,
                   rsCtxFileOrig, rsCtxFileNew,
                   rsCtxChunkHeader, rsCtxChunkOrig, rsCtxChunkNew,
                   rsCtxLineRemoved, rsCtxLineAdded, rsCtxLineChanged,
                   rsCtxLineContext,
                 rsCtxLast,
                 // Unified
                 rsUniFirst,
                   rsUniUnknown,
                   rsUniFileOrig, rsUniFileNew,
                   rsUniChunkHeader,
                   rsUniLineRemoved, rsUniLineAdded,
                   rsUniLineContext,
                 rsUniLast
                );

  TDiffCodeFoldBlockType = (
    cfbtDiffFile,
    cfbtDiffChunk,
    cfbtDiffChunkSect,
    cfbtDiffNone
    );

  TProcTableProc = procedure of object;

const
  CountDiffCodeFoldBlockOffset: Pointer =
    Pointer(PtrInt(Integer(high(TDiffCodeFoldBlockType))+1));

type

  { TSynDiffSyn }

  TSynDiffSyn = class(TSynCustomFoldHighlighter)
  private
    fRange: TRangeState;
    fLine: PChar;
    fLineNumber: Integer;
    Run: LongInt;
    fTokenPos: Integer;
    FTokenID: TtkTokenKind;
  private
    fProcTable: array[#0..#255] of TProcTableProc;
    procedure MakeMethodTables;
    procedure CRProc;
    procedure LFProc;
    procedure NullProc;
    procedure LineProc;
    procedure UnknownProc;
    procedure AsteriskProc;
    procedure MinusProc;
    procedure PlusProc;
    procedure ExclamationProc;
    procedure AtProc;
    procedure SpaceProc;
  private
    FChunkMarkerAttri: TSynHighlighterAttributes;
    FChunkMixedAttri: TSynHighlighterAttributes;
    FChunkNewAttri: TSynHighlighterAttributes;
    FChunkOldAttri: TSynHighlighterAttributes;
    FLineAddedAttri: TSynHighlighterAttributes;
    FLineChangedAttri: TSynHighlighterAttributes;
    FLineContextAttri: TSynHighlighterAttributes;
    FLineRemovedAttri: TSynHighlighterAttributes;
    FNewFileAttri: TSynHighlighterAttributes;
    FOrigFileAttri: TSynHighlighterAttributes;
    FSpaceAttri: TSynHighlighterAttributes;
    FUnknownAttri: TSynHighlighterAttributes;
  protected
    function GetSampleSource : String; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;

    function  GetToken: String; override;
    procedure GetTokenEx(out TokenStart: PChar; out TokenLength: integer); override;
    function  GetTokenAttribute: TSynHighlighterAttributes; override;
    function  GetTokenKind: integer; override;
    function  GetTokenPos: Integer; override;
    function  GetTokenID: TtkTokenKind;

    procedure Next; override;
    function GetEol: Boolean; override;

    procedure SetLine({$IFDEF FPC}const {$ENDIF}NewValue: String;
      LineNumber: Integer); override;
    function GetRange: Pointer; override;
    procedure SetRange(Value: Pointer); override;
    procedure ResetRange; override;

    function GetDefaultAttribute(Index: integer): TSynHighlighterAttributes; override;
    class function GetLanguageName: string; override;
  protected
    // folding
    procedure CreateRootCodeFoldBlock; override;

    function StartDiffCodeFoldBlock(ABlockType: TDiffCodeFoldBlockType): TSynCustomCodeFoldBlock;
    procedure EndDiffCodeFoldBlock;
    function TopDiffCodeFoldBlockType(DownIndex: Integer = 0): TDiffCodeFoldBlockType;

    function GetFoldConfigInstance(Index: Integer): TSynCustomFoldConfig; override;
    function GetFoldConfigCount: Integer; override;
    function GetFoldConfigInternalCount: Integer; override;
  public
    // folding
    function FoldOpenCount(ALineIndex: Integer; AType: Integer = 0): integer; override;
    function FoldCloseCount(ALineIndex: Integer; AType: Integer = 0): integer; override;
    function FoldNestCount(ALineIndex: Integer; AType: Integer = 0): integer; override;
    // TODO: make private
    function MinimumFoldLevel(ALineIndex: Integer): integer; override;
    function EndFoldLevel(ALineIndex: Integer): integer; override;
  published
    property UnknownAttri: TSynHighlighterAttributes read FUnknownAttri write FUnknownAttri;
    property SpaceAttri: TSynHighlighterAttributes read FSpaceAttri write FSpaceAttri;
    property OrigFileAttri: TSynHighlighterAttributes read FOrigFileAttri write FOrigFileAttri;
    property NewFileAttri: TSynHighlighterAttributes read FNewFileAttri write FNewFileAttri;
    property ChunkMarkerAttri: TSynHighlighterAttributes read FChunkMarkerAttri write FChunkMarkerAttri;
    property ChunkNewAttri: TSynHighlighterAttributes read FChunkNewAttri write FChunkNewAttri;
    property ChunkOldAttri: TSynHighlighterAttributes read FChunkOldAttri write FChunkOldAttri;
    property ChunkMixedAttri: TSynHighlighterAttributes read FChunkMixedAttri write FChunkMixedAttri;
    property LineAddedAttri: TSynHighlighterAttributes read FLineAddedAttri write FLineAddedAttri;
    property LineRemovedAttri: TSynHighlighterAttributes read FLineRemovedAttri write FLineRemovedAttri;
    property LineChangedAttri: TSynHighlighterAttributes read FLineChangedAttri write FLineChangedAttri;
    property LineContextAttri: TSynHighlighterAttributes read FLineContextAttri write FLineContextAttri;
  end;

implementation

uses
  SynEditStrConst;


{ TSynDiffSyn }

procedure TSynDiffSyn.MakeMethodTables;
var
  I: Char;
begin
  for I := #0 to #255 do
    case I of
      #13: fProcTable[I] := @CRProc;
      #10: fProcTable[I] := @LFProc;
      #0: fProcTable[I] :=  @NullProc;
      '*': fProcTable[I] := @AsteriskProc;
      '-': fProcTable[I] := @MinusProc;
      '+': fProcTable[I] := @PlusProc;
      '!': fProcTable[I] := @ExclamationProc;
      '@': fProcTable[I] := @AtProc;
      ' ': fProcTable[I] := @SpaceProc;
    else fProcTable[I] := @UnknownProc;
    end;
end;

procedure TSynDiffSyn.CRProc;
begin
  fTokenID := tkSpace;
  Inc(Run);
  if (fLine[Run] = #10) then Inc(Run);
end;

procedure TSynDiffSyn.LFProc;
begin
  fTokenID := tkSpace;
  inc(Run);
end;

procedure TSynDiffSyn.NullProc;
begin
  fTokenID := tkNull;
end;

procedure TSynDiffSyn.LineProc;
var
  l: Integer;
begin
  case fRange of
    rsCtxFileOrig, rsUniFileOrig: FTokenID := tkFileOrig;
    rsCtxFileNew, rsUniFileNew:   FTokenID := tkFileNew;
    rsCtxChunkNew: begin
        l := length(fLine);
        if fLine[Run] = '-' then begin
          FTokenID := tkChunkNewMark;
          while (Run < l-1) and (fLine[Run] = '-') do inc(Run);
        end else begin
          FTokenID := tkChunkNew;
          while (Run < l-1) and (fLine[Run] <> '-') do inc(Run);
        end;
      end;
    rsCtxChunkOrig: begin
        l := length(fLine);
        if fLine[Run] = '*' then begin
          FTokenID := tkChunkOrigMark;
          while (Run < l-1) and (fLine[Run] = '*') do inc(Run);
        end else begin
          FTokenID := tkChunkOrig;
          while (Run < l-1) and (fLine[Run] <> '*') do inc(Run);
        end;
      end;
    rsUniChunkHeader: begin
        l := length(fLine);
        if fLine[Run] = '@' then begin
          FTokenID := tkChunkMixedMark;
          while (Run < l-1) and (fLine[Run] = '@') do inc(Run);
        end else begin
          FTokenID := tkChunkMixed;
          while (Run < l-1) and (fLine[Run] <> '@') do inc(Run);
        end;
      end;
    rsCtxLineRemoved, rsUniLineRemoved: FTokenID := tkLineRemoved;
    rsCtxLineAdded, rsUniLineAdded:     FTokenID := tkLineAdded;
    rsCtxLineChanged:                   FTokenID := tkLineChanged;
    rsCtxLineContext, rsUniLineContext: FTokenID := tkLineContext;
    else begin
      FTokenID := tkUnknown;
    end;
  end;
  Run := length(fLine); // #0 NullProc
end;

procedure TSynDiffSyn.UnknownProc;
begin
  if (fRange in [rsCtxFirst..rsCtxLast]) then
    fRange := rsCtxUnknown
  else if (fRange in [rsUniFirst..rsUniLast]) then
    fRange := rsUniUnknown
  else
    fRange := rsUnknown;
  FTokenID := tkUnknown;
  Run := length(fLine);
end;

procedure TSynDiffSyn.AsteriskProc;
var
  l: Integer;
begin
  (* Could be:
     *** /path/to/original   # orig file                     (Context Format)
     ***************         # start of chunk                (Context Format)
     *** 10,15 ****          # chunk: lines in orig file     (Context Format)
  *)
  l := length(fLine);
  if (not (fRange in [rsUnknown, rsCtxFirst..rsCtxLast])) or
     (l < 4) or (fLine[1] <> '*') or (fLine[2] <> '*')
  then begin
    UnknownProc;
    exit;
  end;

  if fLine[3] = '*' then begin
    // ***************
    fRange := rsCtxChunkHeader;
    FTokenID := tkChunkSeparator;
    Run := l;
    exit;
  end;

  if fLine[3] = ' ' then begin
    if fRange in [rsCtxChunkHeader..rsCtxChunkNew] then begin
      // *** 10,15 ****
      fRange := rsCtxChunkOrig;
      FTokenID := tkChunkOrigMark;
      Run := 3;
    end
    else begin
      // *** /path/to/original
      fRange := rsCtxFileOrig;
      FTokenID := tkFileOrigMark;
      Run := 3;
    end;
  end
  else
    UnknownProc;
end;

procedure TSynDiffSyn.MinusProc;
var
  l: Integer;
begin
  (* Could be:
     --- /path/to/new        # new file                      (Context) [usually after *** / path/to/orig]
     --- 10,15 ----          # chunk: lines in new file      (Context)
     - foo                   # Removed Line                  (Context)
     --- /path/to/original   # orig file                     (Unified) [usually before +++ / path/to/new]
     -foo                    # Removed Line                  (Unified)
  *)

  l := length(fLine);
  if (l > 4) and (fLine[1] = '-') and (fLine[2] = '-') and (fLine[3] = ' ') then begin
    // Todo: this check is unsufficent in Unified format; need to track amount of lines in Chunk
    if fRange in [rsUnknown, rsUniFirst..rsUniLast] then begin
      // --- /path/to/original   # orig file                   (Unified)
      fRange := rsUniFileOrig;
      FTokenID := tkFileOrigMark;
      Run := 3;
    end
    else if fRange in [rsCtxFileOrig] then begin
      // --- /path/to/new        # new file                    (Context)
      fRange := rsCtxFileNew;
      FTokenID := tkFileNewMark;
      Run := 3;
    end
    else begin
      // --- 10,15 ----          # new line                    (Context)
      fRange := rsCtxChunkNew;
      FTokenID := tkChunkNewMark;
      Run := 3;
    end;

    exit;
  end;

  if fRange = rsUnknown then begin
    FTokenID := tkUnknown;
    Run := l;
    exit;
  end;

  if (fRange in [rsCtxFirst..rsCtxLast]) then begin
    fRange := rsCtxLineRemoved;
    Run := Min(l, 2);
  end else begin
    fRange := rsUniLineRemoved;
    Run := 1;
  end;
  FTokenID := tkLineRemovedMark;
end;

procedure TSynDiffSyn.PlusProc;
var
  l: Integer;
begin
  (* Could be:
     + foo                   # Removed Line                  (Context)
     +++ /path/to/new        # orig file                     (Unified)
     +foo                    # Removed Line                  (Unified)
  *)

  l := length(fLine);
  if (l > 4) and (fLine[1] = '+') and (fLine[2] = '+') and (fLine[3] = ' ') then begin
    // Todo: this check is unsufficent in Unified format; need to track amount of lines in Chunk
    if fRange in [rsUnknown, rsUniFirst..rsUniLast] then begin
      // --- /path/to/new      # new file                    (Unified)
      fRange := rsUniFileNew;
      FTokenID := tkFileNewMark;
      Run := 3;
      exit;
    end;
  end;

  if fRange = rsUnknown then begin
    FTokenID := tkUnknown;
    Run := l - 1;
    exit;
  end;

  if (fRange in [rsCtxFirst..rsCtxLast]) then begin
    fRange := rsCtxLineAdded;
    Run := Min(l, 2);
  end else begin
    fRange := rsUniLineAdded;
    Run := 1;
  end;
  FTokenID := tkLineAddedMark;
end;

procedure TSynDiffSyn.ExclamationProc;
var
  l: Integer;
begin
  (* Could be:
     ! foo                   # Changed Line                  (Context)
  *)

  l := length(fLine);
  if (fRange in [rsCtxFirst..rsCtxLast]) and (l >= 2) and (fLine[1] = ' ')
  then begin
    fRange := rsCtxLineChanged;
    FTokenID := tkLineChangedMark;
    Run := Min(l, 2);
    exit;
  end;

  UnknownProc;
end;

procedure TSynDiffSyn.AtProc;
var
  l: Integer;
begin
  (* Could be:
     @@ -1,3 +1,9 @@         # Start of Chunk                (Unified)
  *)

  l := length(fLine);
  if (fRange in [rsCtxFirst..rsCtxLast]) or
     (l < 3) or (fLine[1] <> '@') or (fLine[2] <> ' ')
  then begin
    UnknownProc;
    exit;
  end;

  fRange := rsUniChunkHeader;
  FTokenID := tkChunkMixedMark;
    Run := Min(l, 2);
end;

procedure TSynDiffSyn.SpaceProc;
begin
  (* Could be:
     " foo"                  # Context Line                  (Context, Unified)
  *)

  if fRange = rsUnknown then
    UnknownProc;

  if (fRange in [rsCtxFirst..rsCtxLast]) then begin
    fRange := rsCtxLineContext;
    Run := Min(length(fLine), 2);
  end else begin
    fRange := rsUniLineContext;
    Run := 1;
  end;

  FTokenID := tkChunkMixedMark;
end;

function TSynDiffSyn.GetSampleSource: string;
begin
  Result := ''#13#10 +
            '';
end;

constructor TSynDiffSyn.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FUnknownAttri := TSynHighlighterAttributes.Create(SYNS_AttrUnknownWord, SYNS_XML_AttrUnknownWord);
  FUnknownAttri.Style := [fsItalic];
  AddAttribute(FUnknownAttri);

  FSpaceAttri := TSynHighlighterAttributes.Create(SYNS_AttrSpace, SYNS_XML_AttrSpace);
  AddAttribute(FSpaceAttri);

  FOrigFileAttri := TSynHighlighterAttributes.Create(SYNS_AttrOrigFile, SYNS_XML_AttrOrigFile);
  FOrigFileAttri.Style := [fsBold];
  FOrigFileAttri.Background := clRed;
  AddAttribute(FOrigFileAttri);

  FNewFileAttri := TSynHighlighterAttributes.Create(SYNS_AttrNewFile, SYNS_XML_AttrNewFile);
  FNewFileAttri.Style := [fsBold];
  FNewFileAttri.Background := clGreen;
  AddAttribute(FNewFileAttri);

  FChunkMarkerAttri := TSynHighlighterAttributes.Create(SYNS_AttrChunkMarker, SYNS_XML_AttrChunkMarker);
  FChunkMarkerAttri.Style := [fsBold];
  AddAttribute(FChunkMarkerAttri);

  FChunkNewAttri := TSynHighlighterAttributes.Create(SYNS_AttrChunkNew, SYNS_XML_AttrChunkNew);
  FChunkNewAttri.Style := [fsBold];
  FChunkNewAttri.Foreground := clGreen;
  AddAttribute(FChunkNewAttri);

  FChunkOldAttri := TSynHighlighterAttributes.Create(SYNS_AttrChunkOrig, SYNS_XML_AttrChunkOrig);
  FChunkOldAttri.Style := [fsBold];
  FChunkOldAttri.Foreground := clRed;
  AddAttribute(FChunkOldAttri);

  FChunkMixedAttri := TSynHighlighterAttributes.Create(SYNS_AttrChunkMixed, SYNS_XML_AttrChunkMixed);
  FChunkMixedAttri.Style := [fsBold];
  FChunkMixedAttri.Foreground := clPurple;
  AddAttribute(FChunkMixedAttri);

  FLineAddedAttri := TSynHighlighterAttributes.Create(SYNS_AttrLineAdded, SYNS_XML_AttrLineAdded);
  FLineAddedAttri.Foreground := clGreen;
  AddAttribute(FLineAddedAttri);

  FLineRemovedAttri := TSynHighlighterAttributes.Create(SYNS_AttrLineRemoved, SYNS_XML_AttrLineRemoved);
  FLineRemovedAttri.Foreground := clRed;
  AddAttribute(FLineRemovedAttri);

  FLineChangedAttri := TSynHighlighterAttributes.Create(SYNS_AttrLineChanged, SYNS_XML_AttrLineChanged);
  FLineChangedAttri.Foreground := clPurple;
  AddAttribute(FLineChangedAttri);

  FLineContextAttri := TSynHighlighterAttributes.Create(SYNS_AttrLineContext, SYNS_XML_AttrLineContext);
  AddAttribute(FLineContextAttri);

  SetAttributesOnChange(@DefHighlightChange);
  MakeMethodTables;
  fDefaultFilter := '';
  fRange := rsUnknown;
end;

destructor TSynDiffSyn.Destroy;
begin
  inherited Destroy;
end;

function TSynDiffSyn.GetToken: String;
var
  Len: LongInt;
begin
  Result := '';
  Len := Run - fTokenPos;
  SetString(Result, (FLine + fTokenPos), Len);
end;

procedure TSynDiffSyn.GetTokenEx(out TokenStart: PChar;
  out TokenLength: integer);
begin
  TokenLength:=Run-fTokenPos;
  TokenStart:=FLine + fTokenPos;
end;

function TSynDiffSyn.GetTokenAttribute: TSynHighlighterAttributes;
begin
  case fTokenID of
    tkNull:            Result := FUnknownAttri;
    tkUnknown:         Result := FUnknownAttri;
    tkSpace:           Result := FSpaceAttri;
    tkFileOrigMark:    Result := FOrigFileAttri;
    tkFileNewMark:     Result := FNewFileAttri;
    tkFileOrig:        Result := FOrigFileAttri;
    tkFileNew:         Result := FNewFileAttri;
    tkChunkSeparator:  Result := FChunkMarkerAttri;
    tkChunkMixedMark:  Result := FChunkMixedAttri;
    tkChunkOrigMark:   Result := FChunkOldAttri;
    tkChunkNewMark:    Result := FChunkNewAttri;
    tkChunkMixed:      Result := FChunkMixedAttri;
    tkChunkOrig:       Result := FChunkOldAttri;
    tkChunkNew:        Result := FChunkNewAttri;
    tkLineRemovedMark: Result := FLineRemovedAttri;
    tkLineAddedMark:   Result := FLineAddedAttri;
    tkLineChangedMark: Result := FLineChangedAttri;
    tkLineContextMark: Result := FLineContextAttri;
    tkLineRemoved:     Result := FLineRemovedAttri;
    tkLineAdded:       Result := FLineAddedAttri;
    tkLineChanged:     Result := FLineChangedAttri;
    tkLineContext:     Result := FLineContextAttri;
    else Result := nil;
  end;
end;

function TSynDiffSyn.GetTokenKind: integer;
begin
  Result := Ord(fTokenID);
end;

function TSynDiffSyn.GetTokenPos: Integer;
begin
  Result := fTokenPos;
end;

function TSynDiffSyn.GetTokenID: TtkTokenKind;
begin
  Result := fTokenId;
end;

procedure TSynDiffSyn.Next;
var
  OldRange: TRangeState;
begin
  fTokenPos := Run;
  if Run > 0 then begin
    if fLine[Run] = #0 then NullProc
                       else LineProc;
  end else begin
    OldRange := fRange;
    fProcTable[fLine[Run]]();

    if ((fRange in [rsCtxFileOrig, rsCtxFileNew, rsUniFileOrig..rsUniFileNew]) and
        not (OldRange in [rsCtxFileOrig, rsCtxFileNew, rsUniFileOrig..rsUniFileNew]))
    then begin
      while (TopDiffCodeFoldBlockType in [cfbtDiffChunkSect, cfbtDiffChunk, cfbtDiffFile]) do
        EndDiffCodeFoldBlock;
      StartDiffCodeFoldBlock(cfbtDiffFile);
    end;

    if (fRange in [rsCtxChunkHeader, rsUniChunkHeader])
    then begin
      while (TopDiffCodeFoldBlockType in [cfbtDiffChunkSect, cfbtDiffChunk]) do
        EndDiffCodeFoldBlock;
      StartDiffCodeFoldBlock(cfbtDiffChunk);
    end;

    if (fRange in [rsCtxChunkOrig, rsCtxChunkNew])
    then begin
      if (TopDiffCodeFoldBlockType = cfbtDiffChunkSect) then
        EndDiffCodeFoldBlock;
      StartDiffCodeFoldBlock(cfbtDiffChunkSect);
    end;


  end;
end;

function TSynDiffSyn.GetEol: Boolean;
begin
  Result := fTokenId = tkNull;
end;

procedure TSynDiffSyn.SetLine({$IFDEF FPC}const {$ENDIF}NewValue: String;
  LineNumber: Integer);
begin
  inherited;
  fLine := PChar(NewValue);
  Run := 0;
  fLineNumber := LineNumber;
  Next;
end;

function TSynDiffSyn.GetRange: Pointer;
begin
  CodeFoldRange.RangeType:=Pointer(PtrUInt(Integer(fRange)));
  Result := inherited;
end;

procedure TSynDiffSyn.SetRange(Value: Pointer);
begin
  inherited;
  fRange := TRangeState(Integer(PtrUInt(CodeFoldRange.RangeType)));
end;

procedure TSynDiffSyn.ResetRange;
begin
  inherited;
  fRange := rsUnknown;
end;

function TSynDiffSyn.GetDefaultAttribute(Index: integer): TSynHighlighterAttributes;
begin
  Result := nil;
end;

class function TSynDiffSyn.GetLanguageName: string;
begin
  Result := SYNS_LangDiff;
end;

procedure TSynDiffSyn.CreateRootCodeFoldBlock;
begin
  inherited CreateRootCodeFoldBlock;
  RootCodeFoldBlock.InitRootBlockType(Pointer(PtrInt(cfbtDiffNone)));
end;

function TSynDiffSyn.StartDiffCodeFoldBlock(ABlockType: TDiffCodeFoldBlockType): TSynCustomCodeFoldBlock;
var
  FoldBlock: Boolean;
  p: PtrInt;
begin
  FoldBlock :=  FFoldConfig[ord(ABlockType)].Enabled;
  p := 0;
  if not FoldBlock then
    p := PtrInt(CountDiffCodeFoldBlockOffset);
  Result := StartCodeFoldBlock(p + Pointer(PtrInt(ABlockType)), FoldBlock);
end;

procedure TSynDiffSyn.EndDiffCodeFoldBlock;
var
  DecreaseLevel: Boolean;
begin
  DecreaseLevel := TopCodeFoldBlockType < CountDiffCodeFoldBlockOffset;
  EndCodeFoldBlock(DecreaseLevel);
end;

function TSynDiffSyn.TopDiffCodeFoldBlockType(DownIndex: Integer): TDiffCodeFoldBlockType;
var
  p: Pointer;
begin
  p := TopCodeFoldBlockType(DownIndex);
  if p >= CountDiffCodeFoldBlockOffset then
    p := p - PtrUInt(CountDiffCodeFoldBlockOffset);
  Result := TDiffCodeFoldBlockType(PtrUInt(p));
end;

function TSynDiffSyn.GetFoldConfigInstance(Index: Integer): TSynCustomFoldConfig;
begin
  Result := inherited GetFoldConfigInstance(Index);
  Result.Enabled := True;
end;

function TSynDiffSyn.GetFoldConfigCount: Integer;
begin
  // excluded cfbtDiffnone;
  Result := ord(high(TDiffCodeFoldBlockType)) - ord(low(TDiffCodeFoldBlockType));
end;

function TSynDiffSyn.GetFoldConfigInternalCount: Integer;
begin
  // include cfbtDiffnone;
  Result := ord(high(TDiffCodeFoldBlockType)) - ord(low(TDiffCodeFoldBlockType)) + 1;
end;

// EndLvl = MinLvl(+1)
// MinLvl = Min(Min, Min(+1))

function TSynDiffSyn.FoldOpenCount(ALineIndex: Integer; AType: Integer): integer;
begin
  If AType <> 0 then exit(0);
  Result := Max(0, MinimumFoldLevel(ALineIndex+1) - MinimumFoldLevel(ALineIndex));
  //Result := EndFoldLevel(ALineIndex) - MinimumFoldLevel(ALineIndex);
end;

function TSynDiffSyn.FoldCloseCount(ALineIndex: Integer; AType: Integer): integer;
begin
  If AType <> 0 then exit(0);
  Result := Max(0, MinimumFoldLevel(ALineIndex) - MinimumFoldLevel(ALineIndex+1));
  //Result := EndFoldLevel(ALineIndex - 1) - MinimumFoldLevel(ALineIndex);
end;

function TSynDiffSyn.FoldNestCount(ALineIndex: Integer; AType: Integer): integer;
begin
  If AType <> 0 then exit(0);
  Result := MinimumFoldLevel(ALineIndex+1);
  //Result := EndFoldLevel(ALineIndex);
end;

function TSynDiffSyn.MinimumFoldLevel(ALineIndex: Integer): integer;
var
  r: TSynCustomHighlighterRange;
begin
  if (ALineIndex < 0) or (ALineIndex >= CurrentLines.Count) then
    exit(0);
  r := TSynCustomHighlighterRange(CurrentRanges[ALineIndex]);
  if (r <> nil) and (Pointer(r) <> NullRange) then
    Result := r.MinimumCodeFoldBlockLevel
  else
    Result := 0;
end;

function TSynDiffSyn.EndFoldLevel(ALineIndex: Integer): integer;
var
  r: TSynCustomHighlighterRange;
begin
  if (ALineIndex < 0) or (ALineIndex >= CurrentLines.Count) then
    exit(0);
  r := TSynCustomHighlighterRange(CurrentRanges[ALineIndex]);
  if (r <> nil) and (Pointer(r) <> NullRange) then
    Result := r.CodeFoldStackSize
  else
    Result := 0;
end;

initialization
  RegisterPlaceableHighlighter(TSynDiffSyn);

end.
