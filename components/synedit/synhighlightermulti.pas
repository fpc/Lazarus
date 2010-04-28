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
  Classes, Graphics, SysUtils, LCLProc,
  SynRegExpr, SynEditStrConst, SynEditTypes, SynEditTextBase,
  SynEditHighlighter;

type

  TSynHLightMultiVirtualSection = record
    // X(Char): 1-based
    // Y(Line): 0-based
    StartPos, EndPos: TPoint;
    MarkerStartPos, MarkerEndPos: TPoint; // Todo: set them
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
  protected
    function ItemSize: Integer; override;
  public
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
    FRangeList: TSynManagedStorageMemList;
    FRealLines: TSynEditStrings;
    FSectionList: TSynHLightMultiSectionList;
    FRScanStartedWithLineCount: Integer;
    FRScanStartedAtVLine: Integer;
    FRegionScanStartRangeIndex: Integer;
    FRegionScanRangeIndex: Integer;
  protected
    function  GetRange(Index: Pointer): TSynManagedStorageMem; override;
    procedure PutRange(Index: Pointer; const ARange: TSynManagedStorageMem); override;
    function  Get(Index: integer): string; override;
    procedure Put(Index: integer; const S: string); override; // should not be called ever
    function  GetCount: integer; override;
    property SectionList: TSynHLightMultiSectionList read FSectionList;
    //procedure SendHighlightChanged(aIndex, aCount: Integer); override;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear; override;                                   // should not be called ever
    procedure Delete(Index: Integer); override;                  // should not be called ever
    procedure Insert(Index: Integer; const S: string); override; // should not be called ever
    procedure PrepareRegionScan(AStartLineIdx: Integer);
    procedure FinishRegionScan(AEndLineIdx: Integer);
    procedure RegionScanUpdateFirstRegionEnd(AnEndPoint: TPoint);
    procedure RegionScanUpdate0rInsertRegion(AStartPoint, AnEndPoint: TPoint);
    procedure RegionScanUpdateLastRegionStart(AStartPoint: TPoint; ALineIndex: Integer);
    procedure RealLinesInserted(AIndex, ACount: Integer);
    procedure RealLinesDeleted(AIndex, ACount: Integer);
    property  RealLines: TSynEditStrings read FRealLines write FRealLines;
  end;

  TOnCheckMarker=procedure(Sender: TObject; var StartPos, MarkerLen: Integer;
    var MarkerText: String) of object;

  { TSynHighlighterMultiScheme }

  TSynHighlighterMultiScheme = class(TCollectionItem)
  private
    FStartExpr, FEndExpr: string;
    FConvertedStartExpr, FConvertedEndExpr: String;
    FStartExprScanner, FEndExprScanner: TRegExpr;
    FStartLineSet, FEndLineSet: Boolean;
    FLastMatchLen: Integer;
    fHighlighter: TSynCustomHighLighter;
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
  public
    property VirtualLines: TSynHLightMultiVirtualLines read FVirtualLines;
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

  TSynMultiSyn = class;

  { TSynHighlighterMultiSchemeList }

  TSynHighlighterMultiSchemeList = class(TCollection)
  private
    FCurrentLine, FConvertedCurrentLine: String;
    fOwner: TSynMultiSyn;
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
  public
    property ConvertedCurrentLine: String read GetConvertedCurrentLine;
    property CurrentLine: String read FCurrentLine write SetCurrentLine;
  end;

  { TSynHighlighterMultiRangeList }

  TSynHighlighterMultiRangeList = class(TSynHighlighterRangeList)
  private
    FDefaultVirtualLines: TSynHLightMultiVirtualLines;
    FVirtualLines: TFPList;
  protected
    //procedure SetCapacity(const AValue: Integer); override;
    //procedure SetCount(const AValue: Integer); override;
    //procedure Move(AFrom, ATo, ALen: Integer); override; // should not be called ever
    //procedure LineTextChanged(AIndex: Integer); override;
    //procedure InsertedLines(AIndex, ACount: Integer); override;
    //procedure DeletedLines(AIndex, ACount: Integer); override;
  public
    constructor Create;
    destructor Destroy; override;
    procedure ClearVLines;
    procedure InitForScheme(AScheme: TSynHighlighterMultiSchemeList);
    procedure CopyToScheme(AScheme: TSynHighlighterMultiSchemeList);
    property DefaultVirtualLines: TSynHLightMultiVirtualLines read FDefaultVirtualLines;
    //property VirtualLines[Index: TSynHighlighterMultiScheme]: TSynHLightMultiVirtualLines
    //         read GetVirtualLines write SetVirtualLines;
  end;

  { TSynMultiSyn }

  TSynMultiSyn = class(TSynCustomHighLighter)
  private
    FDefaultLanguageName: String;
    FCurScheme: TSynHighlighterMultiScheme;
    function GetDefaultVirtualLines: TSynHLightMultiVirtualLines;
    procedure SetDefaultHighlighter(const Value: TSynCustomHighLighter);
    procedure SetSchemes(const Value: TSynHighlighterMultiSchemeList);
    function CurrentVirtualLines: TSynHLightMultiVirtualLines;
  protected
    FSchemes: TSynHighlighterMultiSchemeList;
    FDefaultHighlighter: TSynCustomHighLighter;
    FLine: string;
    FLineNumber: Integer;
    FTokenPos: integer;
    FRun: Integer;
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
    function  CreateRangeList: TSynHighlighterRangeList; override;
    procedure SetCurrentLines(const AValue: TSynEditStringsBase); override;
    procedure SchemeItemChanged(Item: TObject);
    procedure SchemeChanged;
    function  PerformScan(StartIndex, EndIndex: Integer): Integer; override;
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
    function  GetTokenPos: Integer; override;
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

operator > (p1, p2 : TPoint) b : boolean;
begin
  Result := (p1.y > p2.y) or ( (p1.y = p2.y) and (p1.x > p2.x) );
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

function TSynHLightMultiSectionList.ItemSize: Integer;
begin
  Result := SizeOf(TSynHLightMultiVirtualSection);
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
    if (p1 < 0) or (s^.VirtualLine < ALineIdx) then
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
  //if ARange <> nil then ;
  {$note  set count/capacity ?}
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
  t := RealLines[i2];
  c1 := 1;
  if Index = s.VirtualLine then c1 := s.StartPos.x;
  c2 := length(t);
  if Index = s.VirtualLine + s.EndPos.y - s.StartPos.y then c2 := s.EndPos.x;
  Result := copy(t, c1, c2 - c1 + 1);
  inc(i);
  while (i < FSectionList.Count) do begin
    s := FSectionList[i];
    if Index <> s.VirtualLine then break;
    t := RealLines[s.StartPos.y];
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

function TSynHLightMultiVirtualLines.GetCount: integer;
var
  s: TSynHLightMultiVirtualSection;
begin
  if FSectionList.Count = 0 then
    exit(0);
  s := FSectionList[FSectionList.Count - 1];
  Result := s.VirtualLine + 1 + s.EndPos.y - s.StartPos.y;
end;

constructor TSynHLightMultiVirtualLines.Create;
begin
  FRangeList := TSynManagedStorageMemList.Create;
  FSectionList := TSynHLightMultiSectionList.Create;
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
  i, NewVLine, StartVLine, LastEnd: Integer;
  s: TSynHLightMultiVirtualSection;
  VDiff: Integer;
begin
  while (FRegionScanRangeIndex < FSectionList.Count) and
        (FSectionList.Sections[FRegionScanRangeIndex].StartPos.y <= AEndLineIdx)
  do
    FSectionList.Delete(FRegionScanRangeIndex);
  // fix virtual lines on sections
  NewVLine := 0;
  StartVLine := 0;
  VDiff := 0;
  if (FRegionScanStartRangeIndex > 0) then begin
    s := FSectionList.Sections[FRegionScanStartRangeIndex-1];
    StartVLine := s.VirtualLine;
    NewVLine := s.VirtualLine + s.EndPos.y - s.StartPos.y;
    LastEnd := s.EndPos.y;
  end
  else
    LastEnd := FSectionList.Sections[FRegionScanStartRangeIndex].StartPos.y;
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
end;

procedure TSynHLightMultiVirtualLines.RegionScanUpdateFirstRegionEnd(AnEndPoint: TPoint);
var
  Sect: TSynHLightMultiVirtualSection;
begin
  //if FLineMap.Count = 0 then begin
  //  Sect.StartPos := Point(1,1);
  //  Sect.EndPos := AnEndPoint;
  //  FLineMap.Insert(0, Sect);
  //  FRegionScanRangeIndex := 1;
  //  exit;
  //end;
  FSectionList.PSections[FRegionScanRangeIndex]^.EndPos := AnEndPoint;
  inc(FRegionScanRangeIndex);
end;

procedure TSynHLightMultiVirtualLines.RegionScanUpdate0rInsertRegion(AStartPoint,
  AnEndPoint: TPoint);
var
  Sect: TSynHLightMultiVirtualSection;
begin
  if (FRegionScanRangeIndex = FSectionList.Count)
  or (FSectionList.Sections[FRegionScanRangeIndex].StartPos > AnEndPoint)
  then begin
    Sect.StartPos := AStartPoint;
    Sect.EndPos := AnEndPoint;
    FSectionList.Insert(FRegionScanRangeIndex, Sect);
  end else begin
    FSectionList.PSections[FRegionScanRangeIndex]^.StartPos := AStartPoint;
    FSectionList.PSections[FRegionScanRangeIndex]^.EndPos := AnEndPoint;
  end;
  inc(FRegionScanRangeIndex);
end;

procedure TSynHLightMultiVirtualLines.RegionScanUpdateLastRegionStart(AStartPoint: TPoint;
  ALineIndex: Integer);
begin
  while (FRegionScanRangeIndex < FSectionList.Count) and
        (FSectionList.Sections[FRegionScanRangeIndex].EndPos.y <= ALineIndex)
  do
    FSectionList.Delete(FRegionScanRangeIndex);
  FSectionList.PSections[FRegionScanRangeIndex]^.StartPos := AStartPoint;
  inc(FRegionScanRangeIndex);
end;

procedure TSynHLightMultiVirtualLines.RealLinesInserted(AIndex, ACount: Integer);
var
  i: Integer;
  s: TSynHLightMultiVirtualSection;
begin
  i := FSectionList.IndexOfFirstSectionAtLineIdx(AIndex, -1, True);
  s := FSectionList[i];
  if AIndex > s.StartPos.y then begin
  end
  while i < FSectionList.Count do begin
    FSectionList.PSections[i]^.StartPos. :=
  end;
end;

procedure TSynHLightMultiVirtualLines.RealLinesDeleted(AIndex, ACount: Integer);
var
  i: Integer;
begin
  for i := 0 to FSectionList.Count - 1 do begin
  end;
end;

{ TSynHighlighterMultiRangeList }

constructor TSynHighlighterMultiRangeList.Create;
begin
  inherited;
  FVirtualLines := TFPList.Create;
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
    TSynHLightMultiVirtualLines(FVirtualLines[0]).Destroy;
    FVirtualLines.Delete(0);
  end;
  FVirtualLines.Clear;
end;

procedure TSynHighlighterMultiRangeList.InitForScheme(AScheme: TSynHighlighterMultiSchemeList);
var
  i: Integer;
begin
  ClearVLines;
  FDefaultVirtualLines := TSynHLightMultiVirtualLines.Create;
  for i := 0 to AScheme.Count - 1 do
    FVirtualLines.Add(TSynHLightMultiVirtualLines.Create);
end;

procedure TSynHighlighterMultiRangeList.CopyToScheme(AScheme: TSynHighlighterMultiSchemeList);
var
  i: Integer;
begin
  for i := 0 to AScheme.Count - 1 do
    AScheme[i].FVirtualLines := TSynHLightMultiVirtualLines(FVirtualLines[i]);
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
begin
  FreeAndNil(fSchemes);
  { unhook notification handlers }
  DefaultHighlighter := nil;
  inherited Destroy;
end;

function TSynMultiSyn.PerformScan(StartIndex, EndIndex: Integer): Integer;
var
  i, j, c: Integer;
  SearchPos, NewSearchPos, TmpSearchPos: Integer;
  CurRegStart: TPoint;

  procedure ChangeScheme(NewScheme: TSynHighlighterMultiScheme;
    StartAtLine, StartAtChar: Integer);
  var
    VLines: TSynHLightMultiVirtualLines;
  begin
    if FCurScheme = nil
    then VLines := DefaultVirtualLines
    else VLines := FCurScheme.VirtualLines;
    if CurRegStart.y < 0 then
      VLines.RegionScanUpdateFirstRegionEnd(Point(StartAtChar-1, StartAtLine))
    else
      VLines.RegionScanUpdate0rInsertRegion(CurRegStart, Point(StartAtChar, StartAtLine));
    FCurScheme := NewScheme;
    CurRegStart.y := StartAtLine;
    CurRegStart.x := StartAtChar;
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
    CurRegStart.y := 1;
    CurRegStart.x := 1;
  end;
  StartAtLineIndex(Result);
  dec(Result);
  repeat
    inc(Result);
    if Result <> StartIndex then
      ContinueNextLine;

    FSchemes.CurrentLine := CurrentLines[Result];
    SearchPos := 1;
    while SearchPos <= length(FLine) do begin
      if FCurScheme <> nil then begin
        // Find Endpoint for CurScheme
        NewSearchPos := FCurScheme.FindEndPosInLine(SearchPos);
        if NewSearchPos <= 0 then
          break; // Ends in next line
        SearchPos := NewSearchPos + FCurScheme.LastMatchLen;
        ChangeScheme(nil, Result, SearchPos - FCurScheme.LastMatchLen);
      end
      else begin
        // Find new start of a Scheme
        NewSearchPos := -1;
        for i := 0 to Schemes.Count - 1 do begin
          TmpSearchPos := Schemes.Items[i].FindStartPosInLine(SearchPos);
          if (NewSearchPos < 0) or (TmpSearchPos < NewSearchPos) then begin
            j := i;
            NewSearchPos := TmpSearchPos;
          end;
        end;
        if NewSearchPos <= 0 then
          break; // Not in this line
        SearchPos := NewSearchPos + FCurScheme.LastMatchLen;
        ChangeScheme(Schemes[j], Result, SearchPos);
      end;
    end;

  until ((not UpdateRangeInfoAtLine(Result)) and (Result > EndIndex))
     or (Result = c);

  if Result = c then begin
    ChangeScheme(FCurScheme, c, length(CurrentLines[c-1]) +  1);
  end
  else if CurRegStart.y > 0 then begin
    if FCurScheme = nil
    then DefaultVirtualLines.RegionScanUpdateLastRegionStart(CurRegStart, Result)
    else FCurScheme.VirtualLines.RegionScanUpdateLastRegionStart(CurRegStart, Result);
  end;
  DefaultVirtualLines.FinishRegionScan(Result);
  for i := 0 to Schemes.Count - 1 do
    Schemes[i].VirtualLines.FinishRegionScan(Result);

  (* Scan nested Highlighters *)
end;

function TSynMultiSyn.GetAttribCount: integer;
begin
  Result := Schemes.Count;
  if DefaultHighlighter <> nil then
    Inc( Result, inherited GetAttribCount );
end;

function TSynMultiSyn.GetAttribute(
  idx: integer): TSynHighlighterAttributes;
begin
  if idx < Schemes.Count then
    Result := Schemes[ idx ].MarkerAttri
  else
    Result := inherited GetAttribute( idx + Schemes.Count );
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
  //if fMarker <> nil then
  //  Result := False
  //else
  if FCurScheme <> nil then
    Result := FCurScheme.Highlighter.GetEol
  else if DefaultHighlighter <> nil then
    Result := DefaultHighlighter.GetEol
  else
    Result := fRun > Length(fLine) + 2;
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
  if DefaultHighlighter = nil then
    Result := fLine
  else
    SetString(Result, (PChar(FLine) + fTokenPos), fRun - fTokenPos -1);
end;

procedure TSynMultiSyn.GetTokenEx(out TokenStart: PChar;
  out TokenLength: integer);
begin
  if DefaultHighlighter = nil then begin
    if fLine<>'' then begin
      TokenStart:=PChar(fLine);
      TokenLength:=length(fLine);
    end else begin
      TokenStart:=#0;
      TokenLength:=0;
    end;
  end else begin
    TokenLength:=fRun-fTokenPos-1;
    TokenStart:=PChar(FLine) + fTokenPos;
  end;
end;

function TSynMultiSyn.GetTokenAttribute: TSynHighlighterAttributes;
begin
  //if fMarker <> nil then
  //  Result := Schemes[fMarker.fScheme].MarkerAttri
  //else
  if FCurScheme <> nil then
    Result := FCurScheme.Highlighter.GetTokenAttribute
  else if DefaultHighlighter <> nil then
    Result := DefaultHighlighter.GetTokenAttribute
  else
    Result := nil;
end;

function TSynMultiSyn.GetTokenKind: integer;
begin
  //if fMarker <> nil then
  //  Result := 0
  //else
  if FCurScheme <> nil then
    Result := FCurScheme.Highlighter.GetTokenKind
  else if DefaultHighlighter <> nil then
    Result := DefaultHighlighter.GetTokenKind
  else
    Result := 0;
end;

function TSynMultiSyn.GetTokenPos: Integer;
begin
  Result := fTokenPos;
end;

procedure TSynMultiSyn.HookHighlighter(aHL: TSynCustomHighlighter);
begin
  aHL.FreeNotification( Self );
  aHL.HookAttrChangeEvent( {$IFDEF FPC}@{$ENDIF}DefHighlightChange );
end;

procedure TSynMultiSyn.Next;
begin
  if DefaultHighlighter = nil then begin
  end;

end;

procedure TSynMultiSyn.Notification(aComp: TComponent; aOp: TOperation);
var
  cScheme: integer;
begin
  inherited;
  if (aOp = opRemove) and (csDestroying in aComp.ComponentState) and
    (aComp is TSynCustomHighlighter)
  then begin
    if DefaultHighlighter = aComp then
      DefaultHighlighter := nil;
    if Schemes<>nil then
      for cScheme := 0 to Schemes.Count -1 do
        if Schemes[ cScheme ].Highlighter = aComp then
          Schemes[ cScheme ].Highlighter := nil;
  end;
end;

function TSynMultiSyn.CreateRangeList: TSynHighlighterRangeList;
begin
  Result := TSynHighlighterMultiRangeList.Create;
  TSynHighlighterMultiRangeList(Result).InitForScheme(Schemes);
  TSynHighlighterMultiRangeList(Result).CopyToScheme(Schemes);
end;

procedure TSynMultiSyn.SetCurrentLines(const AValue: TSynEditStringsBase);
begin
  inherited SetCurrentLines(AValue);
  TSynHighlighterMultiRangeList(CurrentRanges).CopyToScheme(Schemes);
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
  sDefaultHlSetToSelf = 'It is not good to set the DefaultHighlighter '+
    'of a SynMultiSyn to the SynMultiSyn itself. Please do not try this again';
begin
  if DefaultHighlighter <> Value then begin
    if Value = Self then
      raise Exception.Create( sDefaultHlSetToSelf );
    if DefaultHighlighter <> nil then
      UnhookHighlighter( DefaultHighlighter );
    fDefaultHighlighter := Value;
    if DefaultHighlighter <> nil then
      HookHighlighter( DefaultHighlighter );
    { yes, it's necessary }
    if not( csDestroying in ComponentState ) then
      DefHighlightChange( Self );
  end;
end;

function TSynMultiSyn.GetDefaultVirtualLines: TSynHLightMultiVirtualLines;
begin
  Result := TSynHighlighterMultiRangeList(CurrentRanges).DefaultVirtualLines;
end;

procedure TSynMultiSyn.SetLine({$IFDEF FPC}const {$ENDIF}NewValue: string;
  LineNumber: Integer);
begin
  if IsScanning then exit;

  FLineNumber := LineNumber;
  FLine := NewValue;
  //if LineNumber <> fLineNumber +1 then
  //  fTmpRange := nil;
  fRun := 1;
  fTokenPos := 0;
  //fNextMarker := 0;
  Next;
end;

procedure TSynMultiSyn.SetRange(Value: Pointer);
begin
  inherited;
  FCurScheme := TSynHighlighterMultiScheme(Value);
  {$note maybe default scheme if nil?}
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
{$IFDEF SYN_COMPILER_5_UP}
  aHL.RemoveFreeNotification( Self );
{$ENDIF}
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
begin
  DefHighlightChange( Item );
  {$note todo, rescan all}
end;

procedure TSynMultiSyn.SchemeChanged;
var
  i: Integer;
begin
  for i := 0 to KnownLines.Count - 1 do
    TSynHighlighterMultiRangeList(KnownRanges[i]).InitForScheme(Schemes);
  if CurrentLines <> nil then;
    TSynHighlighterMultiRangeList(CurrentRanges).CopyToScheme(Schemes);
  SchemeItemChanged(nil);
end;

{ TSynHighlighterMultiSchemeList }

constructor TSynHighlighterMultiSchemeList.Create(aOwner: TSynMultiSyn);
begin
  inherited Create(TSynHighlighterMultiScheme);
  fOwner := aOwner;
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
  Result := fOwner;
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
  fOwner.SchemeItemChanged(Item);
end;

procedure TSynHighlighterMultiSchemeList.Notify(Item: TCollectionItem;
  Action: TCollectionNotification);
begin
  // Item added/removed
  inherited Notify(Item, Action);
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
  inherited Create(TheCollection);
  fCaseSensitive := True;
  fMarkerAttri := TSynHighlighterAttributes.Create(SYNS_AttrMarker, SYNS_XML_AttrMarker);
  fMarkerAttri.OnChange := {$IFDEF FPC}@{$ENDIF}MarkerAttriChanged;
  MarkerAttri.Background := clYellow;
  MarkerAttri.Style := [fsBold];
  MarkerAttri.InternalSaveDefaultValues;
end;

destructor TSynHighlighterMultiScheme.Destroy;
begin
  fMarkerAttri.Free;
  { unhook notification handlers }
  Highlighter := nil;
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
        OnCheckStartMarker(TSynHighlighterMultiSchemeList(Collection).FOwner, Result, FLastMatchLen, t);
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
        OnCheckEndMarker(TSynHighlighterMultiSchemeList(Collection).FOwner, Result, FLastMatchLen, t);
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
    Changed( False );
  end;
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
    if GetConvertedEndExpr <> OldValue then
      Changed( False );
  end;
end;

procedure TSynHighlighterMultiScheme.SetHighlighter(const Value: TSynCustomHighLighter);
var
  iOwner: TSynMultiSyn;
begin
  if Highlighter <> Value then
  begin
    iOwner := TSynHighlighterMultiSchemeList(Collection).fOwner;
    if Highlighter <> nil then
      iOwner.UnhookHighlighter( Highlighter );
    fHighlighter := Value;
    if (Highlighter <> nil) and (Highlighter <> TSynHighlighterMultiSchemeList(Collection).fOwner) then
      iOwner.HookHighlighter( Highlighter );
    Changed(False);
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
    if GetConvertedStartExpr <> OldValue then
      Changed( False );
  end;
end;

end.

