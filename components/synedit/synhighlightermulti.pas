{-------------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: SynHighlighterMulti.pas, released 2000-06-23.
The Original Code is based on mwMultiSyn.pas by Willo van der Merwe, part of the
mwEdit component suite.

Contributors to the SynEdit and mwEdit projects are listed in the
Contributors.txt file.

Alternatively, the contents of this file may be used under the terms of the
GNU General Public License Version 2 or later (the "GPL"), in which case
the provisions of the GPL are applicable instead of those above.
If you wish to allow use of your version of this file only under the terms
of the GPL and not to allow others to use your version of this file
under the MPL, indicate your decision by deleting the provisions above and
replace them with the notice and other provisions required by the GPL.
If you do not delete the provisions above, a recipient may use your version
of this file under either the MPL or the GPL.

You may retrieve the latest version of this file at the SynEdit home page,
located at http://SynEdit.SourceForge.net

-------------------------------------------------------------------------------}
{
@created(1999, converted to SynEdit 2000-06-23)
@author(Willo van der Merwe <willo@wack.co.za>
@converted to SynEdit by David Muir <dhm@dmsoftware.co.uk>)
@mostly rewritten for Lazarus by M. Friebe 04/2010

The SynHighlighterMulti unit provides SynEdit with a multiple-highlighter syntax highlighter.
This highlighter can be used to highlight text in which several languages are present, such as HTML.
For example, in HTML as well as HTML tags there can also be JavaScript and/or VBScript present.
}
unit SynHighlighterMulti;

{$I synedit.inc}

interface

uses
  Classes, Graphics, SysUtils, LCLProc, math,
  SynRegExpr, SynEditStrConst, SynEditTypes, SynEditTextBase,
  SynEditHighlighter;

type

  TSynHighlighterMultiScheme=class;
  TSynMultiSyn = class;

  TSynHLightMultiVirtualSection = record
    // X(Char): 1-based
    // Y(Line): 0-based
    StartPos, EndPos: TPoint;
    TokenStartPos, TokenEndPos: Integer;
    VirtualLine: Integer;
  end;

  PSynHLightMultiVirtualSection = ^TSynHLightMultiVirtualSection;

  { TSynHLightMultiSectionList }
  (* List of all parts of the original TextBuffer, which are to be scanned by one highlighter *)

  TSynHLightMultiSectionList=class(TSynEditStorageMem)
  private
    function  GetSection(Index: Integer): TSynHLightMultiVirtualSection;
    function GetSectionPointer(Index: Integer): PSynHLightMultiVirtualSection;
    procedure SetSection(Index: Integer; const AValue: TSynHLightMultiVirtualSection);
  public
    constructor Create;
    procedure Insert(AnIndex: Integer; AnSection: TSynHLightMultiVirtualSection);
    procedure Delete(AnIndex: Integer);
    property Sections[Index: Integer]: TSynHLightMultiVirtualSection
             read GetSection write SetSection; default;
    property PSections[Index: Integer]: PSynHLightMultiVirtualSection
             read GetSectionPointer;
    function IndexOfFirstSectionAtLineIdx(ALineIdx: Integer; ACharPos: Integer = -1;
                                          UseNext: Boolean = True): Integer;
    function IndexOfFirstSectionAtVirtualIdx(ALineIdx: Integer): Integer;
  end;

  { TSynHLightMultiVirtualLines }

  TSynHLightMultiVirtualLines=class(TSynEditStringsBase)
  private
    FFirstHLChangedLine: Integer;
    FLastHLChangedLine: Integer;
    FRangeList: TSynManagedStorageMemList;
    FRealLines: TSynEditStringsBase;
    FScheme: TSynHighlighterMultiScheme;
    FSectionList: TSynHLightMultiSectionList;
    FRScanStartedWithLineCount: Integer;
    FRScanStartedAtVLine: Integer;
    FRegionScanStartRangeIndex: Integer;
    FRegionScanRangeIndex: Integer;
    FLastPCharLine: String;
  protected
    function  GetRange(Index: Pointer): TSynManagedStorageMem; override;
    procedure PutRange(Index: Pointer; const ARange: TSynManagedStorageMem); override;
    function  Get(Index: integer): string; override;
    procedure Put(Index: integer; const S: string); override; // should not be called ever
    function  GetCount: integer; override;
  public
    constructor Create(ALines: TSynEditStringsBase);
    destructor Destroy; override;
    procedure Clear; override;                                   // should not be called ever
    procedure Delete(Index: Integer); override;                  // should not be called ever
    procedure Insert(Index: Integer; const S: string); override; // should not be called ever
    function  GetPChar(ALineIndex: Integer; out ALen: Integer): PChar; override; // experimental
    procedure SendHighlightChanged(aIndex, aCount: Integer); override;
    procedure PrepareRegionScan(AStartLineIdx: Integer);
    procedure FinishRegionScan(AEndLineIdx: Integer);
    procedure RegionScanUpdateFirstRegionEnd(AnEndPoint: TPoint; ATokenEndPos: Integer);
    procedure RegionScanUpdate0rInsertRegion(AStartPoint, AnEndPoint: TPoint;
                                  ATokenStartPos, ATokenEndPos: Integer);
    procedure RegionScanUpdateLastRegionStart(AStartPoint: TPoint;
                                  ATokenStartPos: Integer; ALineIndex: Integer);
    procedure RealLinesInserted(AIndex, ACount: Integer);
    procedure RealLinesDeleted(AIndex, ACount: Integer);
    procedure RealLinesChanged(AIndex, ACount: Integer);
    procedure ResetHLChangedLines;
    property  FirstHLChangedLine: Integer read FFirstHLChangedLine;
    property  LastHLChangedLine: Integer read FLastHLChangedLine;
    property  SectionList: TSynHLightMultiSectionList read FSectionList;
    property  Scheme: TSynHighlighterMultiScheme
              read FScheme write FScheme;
  end;

  { TSynHLightMultiVirtualLinesList }

  TSynHLightMultiVirtualLinesList=class(TFPList)
  private
    function GetVLines(Index: Integer): TSynHLightMultiVirtualLines;
    procedure PutVLines(Index: Integer; const AValue: TSynHLightMultiVirtualLines);
  public
    property Items[Index: Integer]: TSynHLightMultiVirtualLines
             read GetVLines write PutVLines; default;
  end;

  TOnCheckMarker=procedure(Sender: TObject; var StartPos, MarkerLen: Integer;
    var MarkerText: String) of object;

  { TSynHighlighterMultiScheme }

  TSynHighlighterMultiScheme = class(TCollectionItem)
  private
    FNeedHLScan: Boolean;
    FStartExpr, FEndExpr: string;
    FConvertedStartExpr, FConvertedEndExpr: String;
    FStartExprScanner, FEndExprScanner: TRegExpr;
    FStartLineSet, FEndLineSet: Boolean;
    FLastMatchLen: Integer;
    FHighlighter: TSynCustomHighLighter;
    fMarkerAttri: TSynHighlighterAttributes;
    fSchemeName: TComponentName;
    fCaseSensitive: Boolean;
    fOnCheckStartMarker: TOnCheckMarker;
    fOnCheckEndMarker: TOnCheckMarker;
    FVirtualLines: TSynHLightMultiVirtualLines;
    function  GetConvertedLine: String;
    function  GetConvertedEndExpr: String;
    function  GetConvertedStartExpr: String;
    procedure MarkerAttriChanged(Sender: TObject);
    procedure SetMarkerAttri(const Value: TSynHighlighterAttributes);
    procedure SetHighlighter(const Value: TSynCustomHighlighter);
    procedure SetEndExpr(const Value: string);
    procedure SetStartExpr(const Value: string);
    procedure SetCaseSensitive(const Value: Boolean);
    procedure SetVirtualLines(const AValue: TSynHLightMultiVirtualLines);
  protected
{$IFDEF SYN_COMPILER_3_UP}
    function GetDisplayName: String; override;
    procedure SetDisplayName(const Value: String); override;
{$ENDIF}
  public
    constructor Create(TheCollection: TCollection); override;
    destructor Destroy; override;
  public
    procedure ClearLinesSet;
    function  FindStartPosInLine(ASearchPos: Integer): Integer;
    function  FindEndPosInLine(ASearchPos: Integer): Integer;
    property  LastMatchLen: Integer read FLastMatchLen;
    property  NeedHLScan: Boolean read FNeedHLScan;
  public
    property VirtualLines: TSynHLightMultiVirtualLines
             read FVirtualLines write SetVirtualLines;
  published
    property CaseSensitive: Boolean read fCaseSensitive write SetCaseSensitive
      default True;
    property StartExpr: string read fStartExpr write SetStartExpr;
    property EndExpr: string read fEndExpr write SetEndExpr;
    property Highlighter: TSynCustomHighlighter read fHighlighter
             write SetHighlighter;
    property MarkerAttri: TSynHighlighterAttributes read fMarkerAttri
             write SetMarkerAttri;
    property SchemeName: TComponentName read fSchemeName write fSchemeName;
    property OnCheckStartMarker: TOnCheckMarker read fOnCheckStartMarker write fOnCheckStartMarker;
    property OnCheckEndMarker: TOnCheckMarker read fOnCheckEndMarker write fOnCheckEndMarker;
  end;

  { TSynHighlighterMultiSchemeList }

  TSynHighlighterMultiSchemeList = class(TCollection)
  private
    FCurrentLine, FConvertedCurrentLine: String;
    FOwner: TSynMultiSyn;
    function GetConvertedCurrentLine: String;
    function GetItems(Index: integer): TSynHighlighterMultiScheme;
    procedure SetCurrentLine(const AValue: String);
    procedure SetItems(Index: integer; const Value: TSynHighlighterMultiScheme);
  protected
{$IFDEF SYN_COMPILER_3_UP}
    function GetOwner: TPersistent; override;
    procedure Update(Item: TCollectionItem); override;
{$ENDIF}
    procedure Notify(Item: TCollectionItem;Action: TCollectionNotification); override;
  public
    constructor Create(aOwner: TSynMultiSyn);
    property Items[aIndex: integer]: TSynHighlighterMultiScheme read GetItems write SetItems;
      default;
    function IndexOf(AnItem: TSynHighlighterMultiScheme): Integer;
  public
    property ConvertedCurrentLine: String read GetConvertedCurrentLine;
    property CurrentLine: String read FCurrentLine write SetCurrentLine;
    property Owner: TSynMultiSyn read FOwner;
  end;

  { TSynHighlighterMultiRangeList }

  TSynHighlighterMultiRangeList = class(TSynHighlighterRangeList)
  private
    FLines: TSynEditStringsBase;
    FDefaultVirtualLines: TSynHLightMultiVirtualLines;
    FVirtualLines: TSynHLightMultiVirtualLinesList;
    function GetVirtualLines(Index: TSynHighlighterMultiScheme): TSynHLightMultiVirtualLines;
  protected
    procedure InsertedLines(AIndex, ACount: Integer); override;
    procedure DeletedLines(AIndex, ACount: Integer); override;
  public
    constructor Create(ALines: TSynEditStringsBase);
    destructor Destroy; override;
    procedure ClearVLines;
    procedure UpdateForScheme(AScheme: TSynHighlighterMultiSchemeList);
    procedure CleanUpForScheme(AScheme: TSynHighlighterMultiSchemeList);
    procedure CopyToScheme(AScheme: TSynHighlighterMultiSchemeList);
    property DefaultVirtualLines: TSynHLightMultiVirtualLines read FDefaultVirtualLines;
    property VirtualLines[Index: TSynHighlighterMultiScheme]: TSynHLightMultiVirtualLines
             read GetVirtualLines; // write SetVirtualLines;
  end;

  TRunSectionInfo = record
    SectionIdx: Integer;
    VirtualStartPos: Integer;     // Position in the Virtual line (without token)
    FirstChar, LastChar: Integer; // Position of the Real Line that is mapped
    TokenFirstChar, TokenLastChar: Integer;
  end;

  { TSynMultiSyn }

  TSynMultiSyn = class(TSynCustomHighLighter)
  private
    FDefaultLanguageName: String;
    FCurScheme: TSynHighlighterMultiScheme;
    function GetCurrentRanges: TSynHighlighterMultiRangeList;
    function GetDefaultVirtualLines: TSynHLightMultiVirtualLines;
    function GetKnownMultiRanges(Index: Integer): TSynHighlighterMultiRangeList;
    procedure SetDefaultHighlighter(const Value: TSynCustomHighLighter);
    procedure SetSchemes(const Value: TSynHighlighterMultiSchemeList);
    function CurrentVirtualLines: TSynHLightMultiVirtualLines;
  protected
    FSchemes: TSynHighlighterMultiSchemeList;
    FDefaultHighlighter: TSynCustomHighLighter;
    FLine: string;
    FCurLineIndex, FLineLen: Integer;
    FTokenPos: integer;
    FTokenKind: integer;
    FTokenAttr: TSynHighlighterAttributes;
    FRun: Integer;
    FRunSectionInfo: Array of TRunSectionInfo;
    FSampleSource: string;
    function GetIdentChars: TSynIdentChars; override;
    function GetDefaultAttribute(Index: integer): TSynHighlighterAttributes; override;
    function GetAttribCount: integer; override;
    function GetAttribute(idx: integer): TSynHighlighterAttributes; override;
    function GetSampleSource: string; override;
    procedure SetSampleSource(Value: string); override;

    procedure HookHighlighter(aHL: TSynCustomHighlighter);
    procedure UnhookHighlighter(aHL: TSynCustomHighlighter);
    procedure Notification(aComp: TComponent; aOp: TOperation); override;
    function  CreateRangeList(ALines: TSynEditStringsBase): TSynHighlighterRangeList; override;
    procedure BeforeDetachedFromRangeList(ARangeList: TSynHighlighterRangeList); override;
    procedure SetCurrentLines(const AValue: TSynEditStringsBase); override;
    procedure SchemeItemChanged(Item: TObject);
    procedure SchemeChanged;
    procedure DetachHighlighter(AHighlighter: TSynCustomHighlighter; AScheme: TSynHighlighterMultiScheme);
    procedure AttachHighlighter(AHighlighter: TSynCustomHighlighter; AScheme: TSynHighlighterMultiScheme);
    function  PerformScan(StartIndex, EndIndex: Integer; ForceEndIndex: Boolean = False): Integer; override;
    property CurrentRanges: TSynHighlighterMultiRangeList read GetCurrentRanges;
    property KnownRanges[Index: Integer]: TSynHighlighterMultiRangeList read GetKnownMultiRanges;
  public
    {$IFNDEF SYN_CPPB_1} class {$ENDIF}
    function GetLanguageName: string; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Next; override;
    function  GetEol: Boolean; override;
    function  GetToken: string; override;
    procedure GetTokenEx(out TokenStart: PChar; out TokenLength: integer); override;
    function  GetTokenAttribute: TSynHighlighterAttributes; override;
    function  GetTokenKind: integer; override;
    function  GetTokenPos: Integer; override; // 0-based
    procedure SetLine({$IFDEF FPC}const {$ENDIF}NewValue: string; LineNumber: Integer); override;
    function  GetRange: Pointer; override;
    procedure SetRange(Value: Pointer); override;
    procedure ResetRange; override;
  public
    property DefaultVirtualLines: TSynHLightMultiVirtualLines read GetDefaultVirtualLines;
  published
    property Schemes: TSynHighlighterMultiSchemeList read fSchemes write SetSchemes;
    property DefaultHighlighter: TSynCustomHighLighter read fDefaultHighlighter
      write SetDefaultHighlighter;
    property DefaultLanguageName: String read fDefaultLanguageName
      write fDefaultLanguageName;
  end;

implementation

const
  TokenKindPerHighlighter = 100;

operator > (p1, p2 : TPoint) b : boolean;
begin
  Result := (p1.y > p2.y) or ( (p1.y = p2.y) and (p1.x > p2.x) );
end;

operator >= (p1, p2 : TPoint) b : boolean;
begin
  Result := (p1.y > p2.y) or ( (p1.y = p2.y) and (p1.x >= p2.x) );
end;

operator < (p1, p2 : TPoint) b : boolean;
begin
  Result := (p1.y < p2.y) or ( (p1.y = p2.y) and (p1.x < p2.x) );
end;

{ TSynHLightMultiSectionList }

function TSynHLightMultiSectionList.GetSection(Index: Integer): TSynHLightMultiVirtualSection;
begin
  Result := PSynHLightMultiVirtualSection(ItemPointer[Index])^;
end;

function TSynHLightMultiSectionList.GetSectionPointer(Index: Integer): PSynHLightMultiVirtualSection;
begin
  Result := PSynHLightMultiVirtualSection(ItemPointer[Index]);
end;

procedure TSynHLightMultiSectionList.SetSection(Index: Integer;
  const AValue: TSynHLightMultiVirtualSection);
begin
  PSynHLightMultiVirtualSection(ItemPointer[Index])^ := AValue;
end;

constructor TSynHLightMultiSectionList.Create;
begin
  inherited;
  ItemSize := SizeOf(TSynHLightMultiVirtualSection);
end;

procedure TSynHLightMultiSectionList.Insert(AnIndex: Integer;
  AnSection: TSynHLightMultiVirtualSection);
begin
  InsertRows(AnIndex, 1);
  Sections[AnIndex] := AnSection;
end;

procedure TSynHLightMultiSectionList.Delete(AnIndex: Integer);
begin
  DeleteRows(AnIndex, 1);
  if (Capacity > 16) and (Capacity > (Count * 2)) then
    Capacity := Capacity - (Count div 2);
end;

function TSynHLightMultiSectionList.IndexOfFirstSectionAtLineIdx(ALineIdx: Integer;
  ACharPos: Integer = -1; UseNext: Boolean = True): Integer;
var
  p, p1, p2: Integer;
  s: PSynHLightMultiVirtualSection;
begin
  Result := -1;
  p2 := Count;
  if p2 = 0 then begin
    if UseNext then Result := 0;
    exit;
  end;
  p1 := p2 div 2;
  dec(p2);
  s := PSynHLightMultiVirtualSection(ItemPointer[p1]);
  if (ALineIdx < s^.StartPos.y) or ( (ALineIdx = s^.StartPos.y) and (ACharPos < s^.StartPos.x) )
  then begin          // target is in 0 .. p1-1
    p2 := p1 - 1;
    p1 := 0;
  end;

  while (p1 < p2) do begin
    p := (p1 + p2 + 1) div 2;
    s := PSynHLightMultiVirtualSection(ItemPointer[p]);
    if (ALineIdx < s^.StartPos.y) or
       ( (ALineIdx = s^.StartPos.y) and (ACharPos < s^.StartPos.x) )
    then
      p2 := p - 1     // target is in p1 .. p-1
    else
      p1 := p;        // target is in p .. p2
  end;

  s := PSynHLightMultiVirtualSection(ItemPointer[p1]);
  if ( (s^.StartPos.y > ALineIdx) or ((s^.StartPos.y = ALineIdx) and (s^.StartPos.x > ACharPos)) )
  then begin
    dec(p1);
    if p1 >= 0 then
      s := PSynHLightMultiVirtualSection(ItemPointer[p1]);
  end;

  if (p1 < 0) or (s^.EndPos.y < ALineIdx) or
    ( (s^.EndPos.y = ALineIdx) and (s^.EndPos.x < ACharPos) )
  then begin
    if UseNext then
      Result := p1 + 1 // Could be p1 = Count // behind end
    else
      Result := -1;
  end
  else begin
    Result := p1;
  end;
end;

function TSynHLightMultiSectionList.IndexOfFirstSectionAtVirtualIdx(ALineIdx: Integer): Integer;
var
  p, p1, p2: Integer;
  s: PSynHLightMultiVirtualSection;
begin
  Result := -1;
  p2 := Count;
  if p2 = 0 then
    exit;
  p1 := p2 div 2;
  dec(p2);
  s := PSynHLightMultiVirtualSection(ItemPointer[p1]);
  if (ALineIdx < s^.VirtualLine) then begin
    p2 := p1 - 1;          // target is in 0 .. p1-1
    p1 := 0;
  end;

  while (p1 < p2) do begin
    p := (p1 + p2 + 1) div 2;
    s := PSynHLightMultiVirtualSection(ItemPointer[p]);
    if (ALineIdx < s^.VirtualLine) then
      p2 := p - 1   // target is in p1 .. p-1
    else
      p1 := p;      // target is in p .. p2
  end;

  s := PSynHLightMultiVirtualSection(ItemPointer[p1]);
  if ALineIdx = s^.VirtualLine then begin
    while (p1 >= 0) and (s^.VirtualLine = ALineIdx) do begin
      dec(p1);
      if p1 >= 0 then
        s := PSynHLightMultiVirtualSection(ItemPointer[p1]);
    end;
    if (p1 < 0) or (s^.VirtualLine + s^.EndPos.y - s^.StartPos.y < ALineIdx) then
      inc(p1);
  end else begin
    p2 := Count;
    while (p1 < p2) and (s^.VirtualLine < ALineIdx) do begin
      inc(p1);
      if p1 < p2 then
        s := PSynHLightMultiVirtualSection(ItemPointer[p1]);
    end;
    if (p1 = p2) or (s^.VirtualLine > ALineIdx) then
      dec(p1);
  end;

  Result := p1;
end;

{ TSynHLightMultiVirtualLines }

function TSynHLightMultiVirtualLines.GetRange(Index: Pointer): TSynManagedStorageMem;
begin
  Result := FRangeList[Index];
end;

procedure TSynHLightMultiVirtualLines.PutRange(Index: Pointer; const ARange: TSynManagedStorageMem);
begin
  FRangeList[Index] := ARange;
  if ARange <> nil then begin
    ARange.Capacity := Count;
    ARange.Count := Count;
  end;
end;

function TSynHLightMultiVirtualLines.Get(Index: integer): string;
var
  i, i2, c1, c2: Integer;
  s: TSynHLightMultiVirtualSection;
  t: String;
begin
  i := FSectionList.IndexOfFirstSectionAtVirtualIdx(Index);
  if (i < 0) or (i >= FSectionList.Count) then
    exit('');
  s := FSectionList[i];
  i2 := s.StartPos.y + Index - s.VirtualLine;
  t := FRealLines[i2];
  c1 := 1;
  if Index = s.VirtualLine then c1 := s.StartPos.x;
  c2 := length(t);
  if Index = s.VirtualLine + s.EndPos.y - s.StartPos.y then c2 := s.EndPos.x;
  Result := copy(t, c1, c2 - c1 + 1);
  inc(i);
  while (i < FSectionList.Count) do begin
    s := FSectionList[i];
    if Index <> s.VirtualLine then break;
    t := FRealLines[s.StartPos.y];
    c1 := s.StartPos.x;
    c2 := length(t);
    if s.EndPos.y = s.StartPos.y then c2 := s.EndPos.x;
    Result := Result + copy(t, c1, c2 - c1 + 1);
    inc(i);
  end;
end;

procedure TSynHLightMultiVirtualLines.Put(Index: integer; const S: string);
begin
  raise Exception.Create('Not allowed');
end;

procedure TSynHLightMultiVirtualLines.Clear;
begin
  raise Exception.Create('Not allowed');
end;

procedure TSynHLightMultiVirtualLines.Delete(Index: Integer);
begin
  raise Exception.Create('Not allowed');
end;

procedure TSynHLightMultiVirtualLines.Insert(Index: Integer; const S: string);
begin
  raise Exception.Create('Not allowed');
end;

function TSynHLightMultiVirtualLines.GetPChar(ALineIndex: Integer; out ALen: Integer): PChar;
begin
  FLastPCharLine := Get(ALineIndex);
  ALen := length(FLastPCharLine);
  Result := PChar(FLastPCharLine);
end;

function TSynHLightMultiVirtualLines.GetCount: integer;
var
  s: TSynHLightMultiVirtualSection;
begin
  if FSectionList.Count = 0 then
    exit(0);
  s := FSectionList[FSectionList.Count - 1];
  Result := s.VirtualLine + 1 + s.EndPos.y - s.StartPos.y;
end;

procedure TSynHLightMultiVirtualLines.SendHighlightChanged(aIndex, aCount: Integer);
begin
  if (FFirstHLChangedLine < 0) or (FFirstHLChangedLine > aIndex) then
    FFirstHLChangedLine := aIndex;
  if (FLastHLChangedLine < aIndex + aCount - 1) then
    FFirstHLChangedLine := aIndex + aCount - 1;
end;

constructor TSynHLightMultiVirtualLines.Create(ALines: TSynEditStringsBase);
begin
  FRangeList := TSynManagedStorageMemList.Create;
  FSectionList := TSynHLightMultiSectionList.Create;
  FRealLines := ALines;
end;

destructor TSynHLightMultiVirtualLines.Destroy;
begin
  inherited Destroy;
  FreeAndNil(FSectionList);
  FreeAndNil(FRangeList);
end;

procedure TSynHLightMultiVirtualLines.PrepareRegionScan(AStartLineIdx: Integer);
begin
  FRegionScanRangeIndex := FSectionList.IndexOfFirstSectionAtLineIdx(AStartLineIdx, -1 ,True);
  FRegionScanStartRangeIndex := FRegionScanRangeIndex;
  FRScanStartedWithLineCount := Count;
  if FRegionScanRangeIndex < FSectionList.Count then
    FRScanStartedAtVLine := FSectionList[FRegionScanRangeIndex].VirtualLine
  else
    FRScanStartedAtVLine := FSectionList.Count;
end;

procedure TSynHLightMultiVirtualLines.FinishRegionScan(AEndLineIdx: Integer);
var
  i, NewVLine, LastVline, LastEnd: Integer;
  s: TSynHLightMultiVirtualSection;
  VDiff: Integer;
begin
  while (FRegionScanRangeIndex < FSectionList.Count) and
        (FSectionList.Sections[FRegionScanRangeIndex].StartPos.y <= AEndLineIdx)
  do
    FSectionList.Delete(FRegionScanRangeIndex);
  VDiff := 0;
  if FRegionScanStartRangeIndex < Count then begin
    // fix virtual lines on sections
    if (FRegionScanStartRangeIndex > 0) then begin
      s := FSectionList.Sections[FRegionScanStartRangeIndex-1];
      NewVLine := s.VirtualLine + s.EndPos.y - s.StartPos.y;
      LastEnd := s.EndPos.y;
    end
    else begin
      NewVLine := 0;
      LastEnd := FSectionList.Sections[FRegionScanStartRangeIndex].StartPos.y;
    end;
    LastVline := NewVLine;
    for i := FRegionScanStartRangeIndex to FSectionList.Count - 1 do begin
      s := FSectionList.Sections[i];
      if s.StartPos.y > LastEnd then
        inc(NewVLine);
      if i = FRegionScanRangeIndex then
        VDiff := NewVLine - s.VirtualLine;  // adjust ranges
      FSectionList.PSections[i]^.VirtualLine := NewVLine;
      NewVLine := NewVLine + s.EndPos.y - s.StartPos.y;
      LastEnd := s.EndPos.y;
    end;
  end;
  if VDiff = 0 then
    VDiff := Count - FRScanStartedWithLineCount;
  if VDiff < 0 then begin
    FRangeList.ChildDeleteRows(FRScanStartedAtVLine, -VDiff);
    FRangeList.CallDeletedLines(FRScanStartedAtVLine, -VDiff);
  end
  else if VDiff > 0 then begin
    FRangeList.ChildInsertRows(FRScanStartedAtVLine, VDiff);
    FRangeList.CallInsertedLines(FRScanStartedAtVLine, VDiff);
  end;
  FRangeList.CallLineTextChanged(FRScanStartedAtVLine, LastVline - FRScanStartedAtVLine + 1);
end;

procedure TSynHLightMultiVirtualLines.RegionScanUpdateFirstRegionEnd(AnEndPoint: TPoint;
  ATokenEndPos: Integer);
var
  p: PSynHLightMultiVirtualSection;
begin
  p := FSectionList.PSections[FRegionScanRangeIndex];
  p^.EndPos := AnEndPoint;
  p^.TokenEndPos := ATokenEndPos;
  inc(FRegionScanRangeIndex);
end;

procedure TSynHLightMultiVirtualLines.RegionScanUpdate0rInsertRegion(AStartPoint,
  AnEndPoint: TPoint; ATokenStartPos, ATokenEndPos: Integer);
var
  Sect: TSynHLightMultiVirtualSection;
  p: PSynHLightMultiVirtualSection;
begin
  if (FRegionScanRangeIndex = FSectionList.Count)
  or (FSectionList.Sections[FRegionScanRangeIndex].StartPos > AnEndPoint)
  then begin
    Sect.StartPos := AStartPoint;
    Sect.EndPos   := AnEndPoint;
    Sect.TokenStartPos := ATokenStartPos;
    Sect.TokenEndPos   := ATokenEndPos;
    FSectionList.Insert(FRegionScanRangeIndex, Sect);
  end else begin
    p := FSectionList.PSections[FRegionScanRangeIndex];
    p^.StartPos := AStartPoint;
    p^.EndPos   := AnEndPoint;
    p^.TokenStartPos := ATokenStartPos;
    p^.TokenEndPos   := ATokenEndPos;
  end;
  inc(FRegionScanRangeIndex);
end;

procedure TSynHLightMultiVirtualLines.RegionScanUpdateLastRegionStart(AStartPoint: TPoint;
  ATokenStartPos: Integer; ALineIndex: Integer);
var
  p: PSynHLightMultiVirtualSection;
begin
  while (FRegionScanRangeIndex < FSectionList.Count) and
        (FSectionList.Sections[FRegionScanRangeIndex].EndPos.y <= ALineIndex)
  do
    FSectionList.Delete(FRegionScanRangeIndex);
  p := FSectionList.PSections[FRegionScanRangeIndex];
  p^.StartPos := AStartPoint;
  p^.TokenStartPos := ATokenStartPos;
  inc(FRegionScanRangeIndex);
end;

procedure TSynHLightMultiVirtualLines.RealLinesInserted(AIndex, ACount: Integer);
var
  i, VLineDiff: Integer;
  s: TSynHLightMultiVirtualSection;
  p: PSynHLightMultiVirtualSection;
begin
  i := FSectionList.IndexOfFirstSectionAtLineIdx(AIndex, -1, True);
  if i = FSectionList.Count then exit;
  VLineDiff := 0;
  s := FSectionList[i];
  if AIndex > s.StartPos.y then begin
    p := FSectionList.PSections[i];
    FRangeList.ChildInsertRows(p^.VirtualLine + AIndex - p^.StartPos.y, ACount);
    FRangeList.CallInsertedLines(p^.VirtualLine + AIndex - p^.StartPos.y, ACount);
    p^.EndPos.y := p^.EndPos.y + ACount;
    inc(i);
    VLineDiff := ACount;
  end;
  while i < FSectionList.Count do begin
    p := FSectionList.PSections[i];
    p^.StartPos.y := p^.StartPos.y + ACount;
    p^.EndPos.y := p^.EndPos.y + ACount;
    p^.VirtualLine := p^.VirtualLine + VLineDiff;
    inc(i);
  end;
end;

procedure TSynHLightMultiVirtualLines.RealLinesDeleted(AIndex, ACount: Integer);
var
  i: Integer;
  PartCnt, VLineDiff: Integer;
  p: PSynHLightMultiVirtualSection;
begin
  i := FSectionList.IndexOfFirstSectionAtLineIdx(AIndex, -1, True);
  if i = FSectionList.Count then exit;

  VLineDiff := 0;
  p := FSectionList.PSections[i];
  if AIndex > p^.StartPos.y then begin
    PartCnt := p^.EndPos.y - AIndex + 1;
    FRangeList.ChildDeleteRows(p^.VirtualLine + AIndex - p^.StartPos.y, PartCnt);
    FRangeList.CallDeletedLines(p^.VirtualLine + AIndex - p^.StartPos.y, PartCnt);
    p^.EndPos.y := p^.EndPos.y - PartCnt;
    inc(i);
    p := FSectionList.PSections[i];
    VLineDiff := PartCnt;
  end;
  while p^.EndPos.y < AIndex + ACount do begin
    VLineDiff := VLineDiff + p^.EndPos.y - p^.StartPos.y + 1;
    FRangeList.ChildDeleteRows(p^.VirtualLine, p^.EndPos.y - p^.StartPos.y + 1);
    FRangeList.CallDeletedLines(p^.VirtualLine, p^.EndPos.y - p^.StartPos.y + 1);
    FSectionList.Delete(i);
    if i = FSectionList.Count then
      exit;
    p := FSectionList.PSections[i];
  end;
  if AIndex + ACount > p^.StartPos.y then begin
    PartCnt := ACount - (p^.StartPos.y - AIndex);
    FRangeList.ChildDeleteRows(p^.VirtualLine, PartCnt);
    FRangeList.CallDeletedLines(p^.VirtualLine, PartCnt);
    p^.EndPos.y := p^.EndPos.y - PartCnt;
    p^.VirtualLine := p^.VirtualLine - VLineDiff;
    VLineDiff := VLineDiff + PartCnt;
    inc(i);
  end;

  while i < FSectionList.Count do begin
    p := FSectionList.PSections[i];
    p^.StartPos.y := p^.StartPos.y - ACount;
    p^.EndPos.y := p^.EndPos.y - ACount;
    p^.VirtualLine := p^.VirtualLine - VLineDiff;
    inc(i);
  end;
end;

procedure TSynHLightMultiVirtualLines.RealLinesChanged(AIndex, ACount: Integer);
var
  i, VLine1, VLine2: Integer;
  s: TSynHLightMultiVirtualSection;
begin
  i := FSectionList.IndexOfFirstSectionAtLineIdx(AIndex, -1, True);
  if i = FSectionList.Count then exit;
  s := FSectionList[i];
  VLine1 := s.VirtualLine + AIndex - s.StartPos.y;
  i := FSectionList.IndexOfFirstSectionAtLineIdx(AIndex + ACount - 1, -1, True);
  if i = FSectionList.Count then
    VLine2 := Count-1
  else begin
    s := FSectionList[i];
    VLine2 := s.VirtualLine + AIndex + ACount - 1 - s.StartPos.y;
  end;
  FRangeList.CallLineTextChanged(VLine1, VLine2 - VLine1 + 1);
end;

procedure TSynHLightMultiVirtualLines.ResetHLChangedLines;
begin
  FFirstHLChangedLine := -1;
  FLastHLChangedLine :=  -1;
end;

{ TSynHLightMultiVirtualLinesList }

function TSynHLightMultiVirtualLinesList.GetVLines(Index: Integer): TSynHLightMultiVirtualLines;
begin
  Result := TSynHLightMultiVirtualLines(inherited Items[Index]);
end;

procedure TSynHLightMultiVirtualLinesList.PutVLines(Index: Integer;
  const AValue: TSynHLightMultiVirtualLines);
begin
  inherited Items[Index] := AValue;
end;

{ TSynHighlighterMultiRangeList }

function TSynHighlighterMultiRangeList.GetVirtualLines(Index: TSynHighlighterMultiScheme): TSynHLightMultiVirtualLines;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to FVirtualLines.Count - 1 do
    if FVirtualLines[i].Scheme = Index then
      exit(FVirtualLines[i]);
end;

procedure TSynHighlighterMultiRangeList.InsertedLines(AIndex, ACount: Integer);
var
  i: Integer;
begin
  inherited InsertedLines(AIndex, ACount);
  for i := 0 to FVirtualLines.Count - 1 do
    FVirtualLines[i].RealLinesInserted(AIndex, ACount);
  FDefaultVirtualLines.RealLinesInserted(AIndex, ACount);
end;

procedure TSynHighlighterMultiRangeList.DeletedLines(AIndex, ACount: Integer);
var
  i: Integer;
begin
  inherited DeletedLines(AIndex, ACount);
  for i := 0 to FVirtualLines.Count - 1 do
    FVirtualLines[i].RealLinesDeleted(AIndex, ACount);
  FDefaultVirtualLines.RealLinesDeleted(AIndex, ACount);
end;

constructor TSynHighlighterMultiRangeList.Create(ALines: TSynEditStringsBase);
begin
  inherited Create;
  FLines := ALines;
  FVirtualLines := TSynHLightMultiVirtualLinesList.Create;
end;

destructor TSynHighlighterMultiRangeList.Destroy;
begin
  inherited Destroy;
  ClearVLines;
  FreeAndNil(FVirtualLines);
end;

procedure TSynHighlighterMultiRangeList.ClearVLines;
begin
  FreeAndNil(FDefaultVirtualLines);
  while FVirtualLines.Count > 0 do begin
    FVirtualLines[0].Destroy;
    FVirtualLines.Delete(0);
  end;
  FVirtualLines.Clear;
end;

procedure TSynHighlighterMultiRangeList.UpdateForScheme(AScheme: TSynHighlighterMultiSchemeList);
var
  i: Integer;
  NewVline: TSynHLightMultiVirtualLines;
begin
  for i := FVirtualLines.Count - 1 downto 0 do
    if AScheme.IndexOf(FVirtualLines[i].Scheme) < 0 then begin
      FVirtualLines[i].Destroy;
      FVirtualLines.Delete(i);
    end;
  if FDefaultVirtualLines = nil then
    FDefaultVirtualLines := TSynHLightMultiVirtualLines.Create(FLines);
  for i := 0 to AScheme.Count - 1 do
    if VirtualLines[AScheme[i]] = nil then begin
      NewVline := TSynHLightMultiVirtualLines.Create(FLines);
      NewVline.Scheme := AScheme[i];
      FVirtualLines.Add(NewVline);
      if AScheme[i].Highlighter <> nil then
        AScheme[i].Highlighter.AttachToLines(NewVline);
    end;
end;

procedure TSynHighlighterMultiRangeList.CleanUpForScheme(AScheme: TSynHighlighterMultiSchemeList);
// Called before destruction / in detach
var
  i: Integer;
begin
  for i := 0 to AScheme.Count - 1 do
    if (VirtualLines[AScheme[i]] <> nil) and (AScheme[i].Highlighter <> nil) then
        AScheme[i].Highlighter.DetachFromLines(VirtualLines[AScheme[i]]);
end;

procedure TSynHighlighterMultiRangeList.CopyToScheme(AScheme: TSynHighlighterMultiSchemeList);
var
  i: Integer;
begin
  for i := 0 to AScheme.Count - 1 do
    AScheme[i].VirtualLines := FVirtualLines[i];
end;

{ TSynMultiSyn }

function TSynMultiSyn.CurrentVirtualLines: TSynHLightMultiVirtualLines;
begin
  if FCurScheme <> nil then
    Result := FCurScheme.VirtualLines
  else
    Result := DefaultVirtualLines;
end;

constructor TSynMultiSyn.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  fSchemes := TSynHighlighterMultiSchemeList.Create(Self);
  FCurScheme := nil;
end;

destructor TSynMultiSyn.Destroy;
var
  s: TSynHighlighterMultiSchemeList;
begin
  s := FSchemes;
  FSchemes := nil;
  s.Free;
  { unhook notification handlers }
  DefaultHighlighter := nil;
  inherited Destroy;
end;

function TSynMultiSyn.PerformScan(StartIndex, EndIndex: Integer; ForceEndIndex: Boolean = False): Integer;
var
  i, j, c: Integer;
  SearchPos, NewSearchPos, TmpSearchPos: Integer;
  CurRegStart: TPoint;
  CurRegTokenPos: Integer;
  LineText: string;

  procedure StartScheme(NewScheme: TSynHighlighterMultiScheme;
    StartAtLine, StartAtChar, TokenAtChar: Integer);
  var
    pt: TPoint;
  begin
    pt := Point(TokenAtChar-1, StartAtLine);
    if CurRegStart.y < 0 then
      DefaultVirtualLines.RegionScanUpdateFirstRegionEnd(pt, 0)
    else
    if pt >= CurRegStart then
      DefaultVirtualLines.RegionScanUpdate0rInsertRegion(CurRegStart, pt, 0, 0);

    FCurScheme := NewScheme;
    CurRegStart.y := StartAtLine;
    CurRegStart.x := StartAtChar;
    CurRegTokenPos := TokenAtChar;
  end;

  procedure EndScheme(EndAtLine, EndAtChar, TokenEndChar: Integer);
  var
    pt: TPoint;
  begin
    pt := Point(EndAtChar, EndAtLine);
    if CurRegStart.y < 0 then
      FCurScheme.VirtualLines.RegionScanUpdateFirstRegionEnd(pt, TokenEndChar)
    else
    if pt >= CurRegStart then
      FCurScheme.VirtualLines.RegionScanUpdate0rInsertRegion
        (CurRegStart, pt, CurRegTokenPos, TokenEndChar);

    FCurScheme := nil;
    CurRegStart.y := EndAtLine;
    CurRegStart.x := TokenEndChar + 1;
    CurRegTokenPos := 0;
  end;

begin
  (* Scan regions *)
  Result := StartIndex;
  c := CurrentLines.Count - 1;
  if c < 0 then begin
    // Clear ?
    exit;
  end;

  DefaultVirtualLines.PrepareRegionScan(Result);
  for i := 0 to Schemes.Count - 1 do
    Schemes[i].VirtualLines.PrepareRegionScan(Result);

  CurRegStart.y := -1;
  if Result = 0 then begin
    CurRegStart.y := 0;
    CurRegStart.x := 1;
    CurRegTokenPos := 1;
  end;
  StartAtLineIndex(Result);
  dec(Result);
  repeat
    inc(Result);
    if Result <> StartIndex then
      ContinueNextLine;

    LineText := CurrentLines[Result];
    FSchemes.CurrentLine := LineText;
    SearchPos := 1;
    while SearchPos <= length(LineText) do begin
      if FCurScheme <> nil then begin
        // Find Endpoint for CurScheme
        NewSearchPos := FCurScheme.FindEndPosInLine(SearchPos);
        if NewSearchPos <= 0 then
          break; // Ends in next line
        SearchPos := NewSearchPos + FCurScheme.LastMatchLen;
        EndScheme(Result, NewSearchPos - 1, SearchPos - 1);
      end
      else begin
        // Find new start of a Scheme
        NewSearchPos := -1;
        for i := 0 to Schemes.Count - 1 do begin
          TmpSearchPos := Schemes.Items[i].FindStartPosInLine(SearchPos);
          if (NewSearchPos < 0) or ((TmpSearchPos > 0) and (TmpSearchPos < NewSearchPos)) then begin
            j := i;
            NewSearchPos := TmpSearchPos;
          end;
        end;
        if NewSearchPos <= 0 then
          break; // Not in this line
        SearchPos := NewSearchPos + Schemes[j].LastMatchLen;
        StartScheme(Schemes[j], Result, SearchPos, NewSearchPos);
      end;
    end;

  until ((not UpdateRangeInfoAtLine(Result)) and (Result > EndIndex))
     or (Result = c);

  if Result = c then begin
    i := length(CurrentLines[c]) +  1;
    if FCurScheme = nil then
      StartScheme(nil, c, i, i)
    else
      EndScheme(c, i, i);
  end
  else if CurRegStart.y > 0 then begin
    if FCurScheme = nil
    then DefaultVirtualLines.RegionScanUpdateLastRegionStart(CurRegStart, 0, Result)
    else FCurScheme.VirtualLines.RegionScanUpdateLastRegionStart(CurRegStart, CurRegTokenPos, Result);
  end;
  DefaultVirtualLines.FinishRegionScan(Result);
  for i := 0 to Schemes.Count - 1 do
    Schemes[i].VirtualLines.FinishRegionScan(Result);

  (* Scan nested Highlighters *)
  for i := 0 to Schemes.Count - 1 do
    if Schemes[i].Highlighter <> nil then begin
      Schemes[i].Highlighter.ScanRanges;
      if Result < Schemes[i].VirtualLines.LastHLChangedLine then
        Result := Schemes[i].VirtualLines.LastHLChangedLine;
    end;
  if FDefaultHighlighter <> nil then begin
    FDefaultHighlighter.ScanRanges;
    if Result < DefaultVirtualLines.LastHLChangedLine then
      Result := DefaultVirtualLines.LastHLChangedLine;
  end;
end;

function TSynMultiSyn.GetAttribCount: integer;
var
  i: Integer;
begin
  Result := Schemes.Count;
  for i := 0 to Schemes.Count - 1 do
    if Schemes[i].Highlighter <> nil then
      inc(Result, Schemes[i].Highlighter.AttrCount);
  if DefaultHighlighter <> nil then
    Inc(Result, DefaultHighlighter.AttrCount);
end;

function TSynMultiSyn.GetAttribute(
  idx: integer): TSynHighlighterAttributes;
var
  i, j: Integer;
begin
  if DefaultHighlighter <> nil then begin
    j := DefaultHighlighter.AttrCount;
    if idx < j then
      exit(DefaultHighlighter.Attribute[idx]);
    dec(idx, j);
  end;

  for i := 0 to Schemes.Count - 1 do begin
    if idx = 0 then
      exit(Schemes[i].MarkerAttri);
    dec(idx);
    if Schemes[i].Highlighter <> nil then begin
      j := Schemes[i].Highlighter.AttrCount;
      if idx < j then
        exit(Schemes[i].Highlighter.Attribute[idx]);
      dec(idx, j);
    end;
  end;

  Result := nil;
  raise Exception.Create('bad attr idx');
end;

function TSynMultiSyn.GetDefaultAttribute(Index: integer): TSynHighlighterAttributes;
var
  iHL: TSynCustomHighlighter;
begin
  if (FCurScheme <> nil) and (FCurScheme.Highlighter <> nil) then
    iHL := FCurScheme.Highlighter
  else
    iHL := DefaultHighlighter;
  { the typecast to TSynMultiSyn is only necessary because the
  GetDefaultAttribute method is protected.
  And don't worry: this really works }
  if iHL <> nil then begin
    Result := TSynMultiSyn(iHL).GetDefaultAttribute(Index)
  end else
    Result := nil;
end;

function TSynMultiSyn.GetEol: Boolean;
begin
  Result := FTokenPos > FLineLen;
end;

function TSynMultiSyn.GetIdentChars: TSynIdentChars;
begin
  if FCurScheme <> nil then
    Result := FCurScheme.Highlighter.IdentChars
  else if DefaultHighlighter <> nil then
    Result := DefaultHighlighter.IdentChars
  else
    Result := inherited GetIdentChars;
end;

{$IFNDEF SYN_CPPB_1} class {$ENDIF}
function TSynMultiSyn.GetLanguageName: string;
begin
  Result := SYNS_LangGeneralMulti;
end;

function TSynMultiSyn.GetRange: Pointer;
begin
  Result := FCurScheme;
end;

function TSynMultiSyn.GetToken: string;
begin
  SetString(Result, (PChar(FLine) + FTokenPos - 1), FRun - FTokenPos);
end;

procedure TSynMultiSyn.GetTokenEx(out TokenStart: PChar;
  out TokenLength: integer);
begin
  TokenLength := FRun-FTokenPos;
  if TokenLength > 0 then begin
    TokenStart := @fLine[FTokenPos];
  end else begin
    TokenStart := nil;
  end;
end;

function TSynMultiSyn.GetTokenAttribute: TSynHighlighterAttributes;
begin
  Result := FTokenAttr;
end;

function TSynMultiSyn.GetTokenKind: integer;
begin
  Result := FTokenKind;
end;

function TSynMultiSyn.GetTokenPos: Integer;
begin
  Result := fTokenPos - 1;
end;

procedure TSynMultiSyn.HookHighlighter(aHL: TSynCustomHighlighter);
begin
  aHL.HookAttrChangeEvent( {$IFDEF FPC}@{$ENDIF}DefHighlightChange );
end;

procedure TSynMultiSyn.Next;
  procedure NextRunSection(ASchemeIdx: Integer);
  var
    VLines: TSynHLightMultiVirtualLines;
    idx: Integer;
    s: TSynHLightMultiVirtualSection;
    x1, x2, tx1, tx2: Integer;
  begin
    if ASchemeIdx > 0 then
      VLines := Schemes[ASchemeIdx-1].VirtualLines
    else
      VLines := DefaultVirtualLines;

    idx := FRunSectionInfo[ASchemeIdx].SectionIdx + 1;
    FRunSectionInfo[ASchemeIdx].SectionIdx := -1;
    if (idx < 0) or (idx >= VLines.SectionList.Count) then
      exit;
    s := VLines.SectionList[idx];
    if s.StartPos.y > FCurLineIndex then
      exit;

    FRunSectionInfo[ASchemeIdx].SectionIdx := idx;
    FRunSectionInfo[ASchemeIdx].VirtualStartPos :=
      FRunSectionInfo[ASchemeIdx].VirtualStartPos +
      FRunSectionInfo[ASchemeIdx].LastChar - FRunSectionInfo[ASchemeIdx].FirstChar + 1;
    if s.StartPos.y = FCurLineIndex then begin
      x1  := s.StartPos.x;
      tx1 := s.TokenStartPos;
      if tx1 = 0 then
        tx1 := x1;
    end else begin
      x1  := 1;
      tx1 := 1;
    end;
    if s.EndPos.y = FCurLineIndex then begin
      x2 := s.EndPos.x;
      tx2 := s.TokenEndPos;
      if tx2 = 0 then
        tx2 := x2;
    end else begin
      x2 := length(CurrentLines[FCurLineIndex]);
      tx2 := x2;
    end;
    FRunSectionInfo[ASchemeIdx].FirstChar := x1;
    FRunSectionInfo[ASchemeIdx].LastChar  := x2;
    FRunSectionInfo[ASchemeIdx].TokenFirstChar := tx1;
    FRunSectionInfo[ASchemeIdx].TokenLastChar  := tx2;
  end;

var
  idx: Integer;
  RSect: TRunSectionInfo;
  HL: TSynCustomHighlighter;
  dummy: PChar;
  tkpos, tklen: Integer;
begin
  //debugln(['--- Next at ',FRun]);
  FTokenPos := FRun;
  FTokenAttr := nil;
  FTokenKind := 0;
  if FRun > FLineLen then
    exit;

  idx := high(FRunSectionInfo);
  while (idx >= 0) and
    ( (FRunSectionInfo[idx].SectionIdx < 0) or
       not ( (FRun >= FRunSectionInfo[idx].TokenFirstChar) and
             (FRun <= FRunSectionInfo[idx].TokenLastChar) ) )
  do
    dec(idx);

  if idx < 0 then begin
    //debugln(['*** XXXXX No section found XXXXX ***']);
    FRun := FLineLen + 1;
    FTokenAttr := nil;
    FTokenKind := 0;
    exit;
  end;

  RSect := FRunSectionInfo[idx];
  //with RSect do debugln([' RSect ',idx,': SectIdx=', SectionIdx, ' Fc=',FirstChar,' LC=',LastChar,' TkFC=',TokenFirstChar, ' TkLC=',TokenLastChar]);
  if RSect.SectionIdx < 0 then begin
    //debugln(['*** XXXXX section missing XXXXX ***']);
    FRun := FLineLen + 1;
    FTokenAttr := nil;
    FTokenKind := 0;
    exit;
  end;

  if (idx > 0) and (FRun < RSect.FirstChar) then begin
    FTokenAttr := Schemes[idx-1].FMarkerAttri;
    FTokenKind := 1;
    FRun := RSect.FirstChar;
    //debugln(['  start-token ', FRun]);
  end
  else if (idx > 0) and (FRun > RSect.LastChar) then begin
    FTokenAttr := Schemes[idx-1].FMarkerAttri;
    FTokenKind := 1;
    FRun := RSect.TokenLastChar + 1;
    //debugln(['  end-token   ', FRun]);
  end
  else begin
    if idx = 0 then
      HL := DefaultHighlighter
    else
      HL := Schemes[idx-1].Highlighter;

    if HL <> nil then begin
      repeat
        HL.GetTokenEx(dummy, tklen);
        tkpos := HL.GetTokenPos + 1;
        if tkpos - RSect.VirtualStartPos + RSect.FirstChar + tklen - 1 < FRun then begin
          //debugln('>');
          HL.Next
        end else
          break;
      until HL.GetEol;
      if not HL.GetEol then begin
        FTokenAttr := HL.GetTokenAttribute;
        FTokenKind := idx * TokenKindPerHighlighter + HL.GetTokenKind;
        FRun := Min(tkpos - RSect.VirtualStartPos + RSect.FirstChar + tklen,
                    RSect.LastChar + 1);
        //debugln(['  FOUND-token   ', FRun, ' t=',copy(FLine, FTokenPos, 2),'... kind=',FTokenKind, '    subhl: tkpos=',tkpos,' tklen=',tklen, '  t=', copy(dummy,1,tklen) ]);
      end
      else
        HL := nil;
    end;

    if (HL = nil) then begin
      FTokenAttr := nil;
      FTokenKind := 0;
      FRun := RSect.LastChar + 1;
      //debugln(['  no HL ', FRun]);
    end;
  end;

  if (FRun > RSect.TokenLastChar) then
    NextRunSection(idx);
end;

procedure TSynMultiSyn.Notification(aComp: TComponent; aOp: TOperation);
var
  i: Integer;
begin
  inherited;
  if (aOp = opRemove) and (Schemes <> nil) then begin
    if (aComp = DefaultHighlighter) then
      DefaultHighlighter := nil;
    for i := 0 to Schemes.Count - 1 do
      if aComp = Schemes[i].Highlighter then
        Schemes[i].Highlighter := nil;
  end;
end;

function TSynMultiSyn.CreateRangeList(ALines: TSynEditStringsBase): TSynHighlighterRangeList;
var
  NewRangeList: TSynHighlighterMultiRangeList;
begin
  NewRangeList := TSynHighlighterMultiRangeList.Create(ALines);
  NewRangeList.UpdateForScheme(Schemes);
  NewRangeList.CopyToScheme(Schemes);
  if FDefaultHighlighter <> nil then
    FDefaultHighlighter.AttachToLines(NewRangeList.DefaultVirtualLines);
  Result := NewRangeList;
end;

procedure TSynMultiSyn.BeforeDetachedFromRangeList(ARangeList: TSynHighlighterRangeList);
begin
  inherited BeforeDetachedFromRangeList(ARangeList);
  if (Schemes <> nil) and (ARangeList.RefCount = 0) then begin
    TSynHighlighterMultiRangeList(ARangeList).CleanUpForScheme(Schemes);
    if (TSynHighlighterMultiRangeList(ARangeList).DefaultVirtualLines <> nil) and
       (DefaultHighlighter <> nil)
    then
      DefaultHighlighter.DetachFromLines(TSynHighlighterMultiRangeList(ARangeList).DefaultVirtualLines);
  end;
end;

procedure TSynMultiSyn.SetCurrentLines(const AValue: TSynEditStringsBase);
begin
  inherited SetCurrentLines(AValue);
  CurrentRanges.CopyToScheme(Schemes);
  if FDefaultHighlighter <> nil then
    FDefaultHighlighter.CurrentLines := CurrentRanges.DefaultVirtualLines;
end;

procedure TSynMultiSyn.ResetRange;
begin
  FCurScheme := nil;
  if DefaultHighlighter <> nil then begin
    DefaultHighlighter.ResetRange;
  end;
end;

procedure TSynMultiSyn.SetDefaultHighlighter(
  const Value: TSynCustomHighLighter);
const
  sDefaultHlSetToSelf = 'Not allowed';
var
  i: Integer;
begin
  if DefaultHighlighter = Value then exit;
  if Value = Self then
    raise Exception.Create( sDefaultHlSetToSelf );
  if DefaultHighlighter <> nil then begin
    DefaultHighlighter.RemoveFreeNotification(Self);
    UnhookHighlighter( DefaultHighlighter );
    for i := 0 to KnownLines.Count - 1 do
      DefaultHighlighter.DetachFromLines(KnownRanges[i].DefaultVirtualLines);
  end;
  fDefaultHighlighter := Value;
  if DefaultHighlighter <> nil then begin
    HookHighlighter( DefaultHighlighter );
    DefaultHighlighter.FreeNotification(Self);
    for i := 0 to KnownLines.Count - 1 do
      DefaultHighlighter.AttachToLines(KnownRanges[i].DefaultVirtualLines);
  end;
  { yes, it's necessary }
  if not( csDestroying in ComponentState ) then
    DefHighlightChange( Self );
end;

function TSynMultiSyn.GetDefaultVirtualLines: TSynHLightMultiVirtualLines;
begin
  Result := CurrentRanges.DefaultVirtualLines;
end;

function TSynMultiSyn.GetKnownMultiRanges(Index: Integer): TSynHighlighterMultiRangeList;
begin
  Result := TSynHighlighterMultiRangeList(inherited KnownRanges[Index])
end;

function TSynMultiSyn.GetCurrentRanges: TSynHighlighterMultiRangeList;
begin
  Result := TSynHighlighterMultiRangeList(inherited CurrentRanges)
end;

procedure TSynMultiSyn.SetLine({$IFDEF FPC}const {$ENDIF}NewValue: string;
  LineNumber: Integer);
  procedure InitRunSection(ASchemeIdx: Integer);
  var
    VLines: TSynHLightMultiVirtualLines;
    HL: TSynCustomHighlighter;
    s: TSynHLightMultiVirtualSection;
    idx, x1, x2, tx1, tx2: Integer;
  begin
    FRunSectionInfo[ASchemeIdx].SectionIdx := -1;
    if ASchemeIdx > 0 then begin
      VLines := Schemes[ASchemeIdx-1].VirtualLines;
      HL     := Schemes[ASchemeIdx-1].Highlighter;
    end else begin
      VLines := DefaultVirtualLines;
      HL     := DefaultHighlighter;
    end;
    idx := VLines.SectionList.IndexOfFirstSectionAtLineIdx(FCurLineIndex);
    if (idx < 0) or (idx >= VLines.SectionList.Count) then
      exit;
    s := VLines.SectionList[idx];
    if s.StartPos.y > FCurLineIndex then
      exit;

    FRunSectionInfo[ASchemeIdx].SectionIdx := idx;
    FRunSectionInfo[ASchemeIdx].VirtualStartPos := 1;
    if s.StartPos.y = FCurLineIndex then begin
      x1  := s.StartPos.x;
      tx1 := s.TokenStartPos;
      if tx1 = 0 then
        tx1 := x1;
    end else begin
      x1  := 1;
      tx1 := 1;
    end;
    if s.EndPos.y = FCurLineIndex then begin
      x2 := s.EndPos.x;
      tx2 := s.TokenEndPos;
      if tx2 = 0 then
        tx2 := x2;
    end else begin
      x2 := length(CurrentLines[FCurLineIndex]);
      tx2 := x2;
    end;
    FRunSectionInfo[ASchemeIdx].FirstChar := x1;
    FRunSectionInfo[ASchemeIdx].LastChar  := x2;
    FRunSectionInfo[ASchemeIdx].TokenFirstChar := tx1;
    FRunSectionInfo[ASchemeIdx].TokenLastChar  := tx2;

    if HL <> nil then
      HL.StartAtLineIndex(s.VirtualLine + FCurLineIndex - s.StartPos.y);
      //with FRunSectionInfo[ASchemeIdx] do debugln([' RunSection ',ASchemeIdx,': SectIdx=', SectionIdx, ' Fc=',FirstChar,' LC=',LastChar,' TkFC=',TokenFirstChar, ' TkLC=',TokenLastChar, '  VLine=',s.VirtualLine + FCurLineIndex - s.StartPos.y]);
  end;
var
  i: Integer;
begin
  if IsScanning then exit;
  inherited;

  FCurLineIndex := LineNumber;
  FLine := NewValue;
  FLineLen := length(FLine);
  fRun := 1;
  FTokenPos := 1;
  FTokenAttr := nil;
  FTokenKind := 0;
  //debugln(['>>>>> Setting Line ',FCurLineIndex,' = ',FLine]);
  for i := 0 to high(FRunSectionInfo) do
    InitRunSection(i);
  Next;
end;

procedure TSynMultiSyn.SetRange(Value: Pointer);
begin
  inherited;
  FCurScheme := TSynHighlighterMultiScheme(Value);
end;

procedure TSynMultiSyn.SetSchemes(const Value: TSynHighlighterMultiSchemeList);
begin
  fSchemes.Assign(Value);
end;

procedure TSynMultiSyn.UnhookHighlighter(aHL: TSynCustomHighlighter);
begin
  if csDestroying in aHL.ComponentState then
    Exit;
  aHL.UnhookAttrChangeEvent( {$IFDEF FPC}@{$ENDIF}DefHighlightChange );
end;

function TSynMultiSyn.GetSampleSource: string;
begin
  Result := fSampleSource;
end;

procedure TSynMultiSyn.SetSampleSource(Value: string);
begin
  fSampleSource := Value;
end;

procedure TSynMultiSyn.SchemeItemChanged(Item: TObject);
var
  i: Integer;
begin
  if Schemes = nil then exit;
  FAttributeChangeNeedScan := (Item <> nil) and (TSynHighlighterMultiScheme(Item).NeedHLScan);
  DefHighlightChange( Item );
  for i := 0 to KnownLines.Count - 1 do
    KnownRanges[i].InvalidateAll;
end;

procedure TSynMultiSyn.SchemeChanged;
var
  i: Integer;
begin
  if Schemes = nil then exit;
  SetLength(FRunSectionInfo, Schemes.Count + 1); // include default
  for i := 0 to KnownLines.Count - 1 do
    KnownRanges[i].UpdateForScheme(Schemes);
  if CurrentLines <> nil then
    CurrentRanges.CopyToScheme(Schemes);
  SchemeItemChanged(nil);
end;

procedure TSynMultiSyn.DetachHighlighter(AHighlighter: TSynCustomHighlighter;
  AScheme: TSynHighlighterMultiScheme);
var
  i: Integer;
begin
  for i := 0 to KnownLines.Count - 1 do
    AHighlighter.DetachFromLines(KnownRanges[i].VirtualLines[AScheme]);
end;

procedure TSynMultiSyn.AttachHighlighter(AHighlighter: TSynCustomHighlighter;
  AScheme: TSynHighlighterMultiScheme);
var
  i: Integer;
begin
  for i := 0 to KnownLines.Count - 1 do
    AHighlighter.AttachToLines(KnownRanges[i].VirtualLines[AScheme]);
end;

{ TSynHighlighterMultiSchemeList }

constructor TSynHighlighterMultiSchemeList.Create(aOwner: TSynMultiSyn);
begin
  inherited Create(TSynHighlighterMultiScheme);
  FOwner := aOwner;
end;

function TSynHighlighterMultiSchemeList.IndexOf(AnItem: TSynHighlighterMultiScheme): Integer;
begin
  Result := Count - 1;
  while (Result >= 0) and (Items[Result] <> AnItem) do
    dec(Result);
end;

function TSynHighlighterMultiSchemeList.GetItems(Index: integer): TSynHighlighterMultiScheme;
begin
  Result := inherited Items[Index] as TSynHighlighterMultiScheme;
end;

function TSynHighlighterMultiSchemeList.GetConvertedCurrentLine: String;
begin
  if FConvertedCurrentLine = '' then
    FConvertedCurrentLine := AnsiUpperCase(FCurrentLine);
  Result := FConvertedCurrentLine;
end;

procedure TSynHighlighterMultiSchemeList.SetCurrentLine(const AValue: String);
var
  i: Integer;
begin
  if FCurrentLine = AValue then exit;
  FCurrentLine := AValue;
  for i := 0 to Count - 1 do
    Items[i].ClearLinesSet;
end;

{$IFDEF SYN_COMPILER_3_UP}
function TSynHighlighterMultiSchemeList.GetOwner: TPersistent;
begin
  Result := Owner;
end;
{$ENDIF}

procedure TSynHighlighterMultiSchemeList.SetItems(Index: integer; const Value: TSynHighlighterMultiScheme);
begin
  inherited Items[Index] := Value;
end;

{$IFDEF SYN_COMPILER_3_UP}
procedure TSynHighlighterMultiSchemeList.Update(Item: TCollectionItem);
begin
  // property of an Item changed
  Owner.SchemeItemChanged(Item);
end;

procedure TSynHighlighterMultiSchemeList.Notify(Item: TCollectionItem;
  Action: TCollectionNotification);
begin
  // Item added/removed
  inherited Notify(Item, Action);
  Owner.SchemeChanged;
end;

{$ENDIF}

{ TSynHighlighterMultiScheme }

function TSynHighlighterMultiScheme.GetConvertedLine: String;
begin
  if FCaseSensitive then
    Result := TSynHighlighterMultiSchemeList(Collection).ConvertedCurrentLine
  else
    Result := TSynHighlighterMultiSchemeList(Collection).CurrentLine;
end;

function TSynHighlighterMultiScheme.GetConvertedEndExpr: String;
begin
  if not FCaseSensitive then
    Result := FEndExpr
  else begin
    if FConvertedEndExpr = '' then
      FConvertedEndExpr := AnsiUpperCase(FEndExpr);
    Result := FConvertedEndExpr
  end;
end;

function TSynHighlighterMultiScheme.GetConvertedStartExpr: String;
begin
  if not FCaseSensitive then
    Result := FStartExpr
  else begin
    if FConvertedStartExpr = '' then
      FConvertedStartExpr := AnsiUpperCase(FStartExpr);
    Result := FConvertedStartExpr
  end;
end;

constructor TSynHighlighterMultiScheme.Create(TheCollection: TCollection);
begin
  FStartExprScanner := TRegExpr.Create;
  FEndExprScanner := TRegExpr.Create;
  fCaseSensitive := True;
  fMarkerAttri := TSynHighlighterAttributes.Create(SYNS_AttrMarker, SYNS_XML_AttrMarker);
  fMarkerAttri.OnChange := {$IFDEF FPC}@{$ENDIF}MarkerAttriChanged;
  MarkerAttri.Background := clYellow;
  MarkerAttri.Style := [fsBold];
  MarkerAttri.InternalSaveDefaultValues;
  inherited Create(TheCollection); // Calls notify, all setup must be done
end;

destructor TSynHighlighterMultiScheme.Destroy;
begin
  { unhook notification handlers }
  Highlighter := nil;
  fMarkerAttri.Free;
  inherited Destroy;
  FreeAndNil(FStartExprScanner);
  FreeAndNil(FEndExprScanner);
end;

procedure TSynHighlighterMultiScheme.ClearLinesSet;
begin
  FStartLineSet := False;
  FEndLineSet := False;
end;

function TSynHighlighterMultiScheme.FindStartPosInLine(ASearchPos: Integer): Integer;
var
  t: String;
begin
  if (FStartExprScanner.Expression = '') or (FEndExprScanner.Expression = '') then
    exit(-1);

  if not FStartLineSet then begin
    FStartExprScanner.InputString := GetConvertedLine;
    FStartLineSet := True;
  end;

  Repeat
    if FStartExprScanner.Exec(ASearchPos) then begin
      Result := FStartExprScanner.MatchPos[0];
      FLastMatchLen := FStartExprScanner.MatchLen[0];

      if Assigned(OnCheckStartMarker) then begin
        t := FStartExprScanner.Match[0];
        OnCheckStartMarker(TSynHighlighterMultiSchemeList(Collection).Owner, Result, FLastMatchLen, t);
        if (t <> '') and (FLastMatchLen > 0) then
          exit;
        ASearchPos := FStartExprScanner.MatchPos[0] + 1;
      end
      else
        exit;
    end
    else begin
      Result := -1;
      FLastMatchLen := 0;
      exit;
    end;
  until False;
end;

function TSynHighlighterMultiScheme.FindEndPosInLine(ASearchPos: Integer): Integer;
var
  t: String;
begin
  if not FEndLineSet then begin
    FEndExprScanner.InputString := GetConvertedLine;
    FEndLineSet:= True;
  end;

  Repeat
    if FEndExprScanner.Exec(ASearchPos) then begin
      Result := FEndExprScanner.MatchPos[0];
      FLastMatchLen := FEndExprScanner.MatchLen[0];

      if Assigned(OnCheckEndMarker) then begin
        t := FEndExprScanner.Match[0];
        OnCheckEndMarker(TSynHighlighterMultiSchemeList(Collection).Owner, Result, FLastMatchLen, t);
        if (t <> '') and (FLastMatchLen > 0) then
          exit;
        ASearchPos := FEndExprScanner.MatchPos[0] + 1;
      end
      else
        exit;
    end
    else begin
      Result := -1;
      FLastMatchLen := 0;
      exit;
    end;
  until False;
end;

{$IFDEF SYN_COMPILER_3_UP}
function TSynHighlighterMultiScheme.GetDisplayName: String;
begin
  if SchemeName <> '' then
    Result := SchemeName
  else
    Result := inherited GetDisplayName;
end;
{$ENDIF SYN_COMPILER_3_UP}

procedure TSynHighlighterMultiScheme.MarkerAttriChanged(Sender: TObject);
begin
  Changed( False );
end;

procedure TSynHighlighterMultiScheme.SetCaseSensitive(const Value: Boolean);
begin
  if fCaseSensitive <> Value then
  begin
    fCaseSensitive := Value;
    FStartExprScanner.Expression := GetConvertedStartExpr;
    FEndExprScanner.Expression := GetConvertedEndExpr;
    FNeedHLScan := True;
    Changed( False );
    FNeedHLScan := False;
  end;
end;

procedure TSynHighlighterMultiScheme.SetVirtualLines(const AValue: TSynHLightMultiVirtualLines);
begin
  if FVirtualLines = AValue then exit;
  FVirtualLines := AValue;
  if FHighlighter <> nil then
    FHighlighter.CurrentLines := AValue;
end;

{$IFDEF SYN_COMPILER_3_UP}
procedure TSynHighlighterMultiScheme.SetDisplayName(const Value: String);
begin
  SchemeName := Value;
end;
{$ENDIF SYN_COMPILER_3_UP}

procedure TSynHighlighterMultiScheme.SetEndExpr(const Value: string);
var OldValue: String;
begin
  if fEndExpr <> Value then
  begin
    OldValue := GetConvertedEndExpr;
    FConvertedEndExpr := '';
    FEndExpr := Value;
    FEndExprScanner.Expression := GetConvertedEndExpr;
    FNeedHLScan := True;
    if GetConvertedEndExpr <> OldValue then
      Changed( False );
    FNeedHLScan := False;
  end;
end;

procedure TSynHighlighterMultiScheme.SetHighlighter(const Value: TSynCustomHighLighter);
var
  ParentHLighter: TSynMultiSyn;
begin
  if Highlighter <> Value then
  begin
    if (Value = TSynHighlighterMultiSchemeList(Collection).Owner) then
      raise Exception.Create('circular highlighter not allowed');

    ParentHLighter := TSynHighlighterMultiSchemeList(Collection).Owner;
    if Highlighter <> nil then begin
      Highlighter.RemoveFreeNotification(ParentHLighter);
      ParentHLighter.UnhookHighlighter(Highlighter);
      ParentHLighter.DetachHighlighter(Highlighter, Self);
    end;
    fHighlighter := Value;
    if Highlighter <> nil then begin
      ParentHLighter.AttachHighlighter(Highlighter, Self);
      Highlighter.FreeNotification(ParentHLighter);
      if FVirtualLines <> nil then
        FHighlighter.CurrentLines := FVirtualLines;
    end;
    FNeedHLScan := True;
    Changed(False);
    FNeedHLScan := False;
  end;
end;

procedure TSynHighlighterMultiScheme.SetMarkerAttri(const Value: TSynHighlighterAttributes);
begin
  fMarkerAttri.Assign(Value);
end;

procedure TSynHighlighterMultiScheme.SetStartExpr(const Value: string);
var OldValue: String;
begin
  if fStartExpr <> Value then
  begin
    OldValue := GetConvertedStartExpr;
    FConvertedStartExpr := '';
    FStartExpr := Value;
    FStartExprScanner.Expression := GetConvertedStartExpr;
    FNeedHLScan := True; // TODO: only if EndScanne.Expression <> '' ?
    if GetConvertedStartExpr <> OldValue then
      Changed( False );
    FNeedHLScan := False;
  end;
end;

end.

