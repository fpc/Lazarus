{-------------------------------------------------------------------------------
The contents of this file may be used under the terms of the
GNU General Public License Version 2 or later (the "GPL")

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: SynHighlighterPo.pp, released 2011-12-17.
Author: Bart Broersma
All Rights Reserved.

Contributors to the SynEdit and mwEdit projects are listed in the
Contributors.txt file.


$Id: SynHighlighterPo.pp,v 0.0.1 bbroersma Exp $


Known Issues:
-------------------------------------------------------------------------------}
{
@abstract(Provides a po-files highlighter for SynEdit)
@author(Bart Broersma)
@created(2011-12-17)
@lastmod(2011-12-18)
The SynHighlighterPo unit provides SynEdit with an po-files highlighter.
}
unit SynHighlighterPo;

{$I SynEdit.inc}

interface

uses
  Classes, SysUtils, Graphics, SynEditTypes, SynEditHighlighter, SynEditStrConst,
  LazEditTextAttributes, LazEditMiscProcs, LazEditHighlighter;

type
  TtkTokenKind = (tkComment, tkText, tkKey, tkNull, tkSpace, tkString,
                  tkIdentifier, tkPrevValue, tkFlags, tkUnknown);

  TProcTableProc = procedure of object;

type

  { TSynPoSyn }

  TSynPoSyn = class(TSynCustomHighlighter)
  private const
    PO_BRACKET_KIND_TOKEN_COUNT = 4;
    PO_BRACKET_KIND_TOKENS: array [Boolean, 0..PO_BRACKET_KIND_TOKEN_COUNT-1] of string =
      ( (')', ']', '}', '"'),
        ('(', '[', '{', '"')
      );
  private type
    TSynPasAttribute = (
    attribComment,
    attribText,
    attribKey,
    attribSpace
    );
  private
    fProcTable: array[#0..#255] of TProcTableProc;
    Run: LongInt;
    fTokenPos: Integer;
    FTokenID: TtkTokenKind;
    fCommentAttri: TSynHighlighterAttributes;
    fTextAttri: TSynHighlighterAttributes;
    fKeyAttri: TSynHighlighterAttributes;
    fSpaceAttri: TSynHighlighterAttributes;
    fStringAttri: TSynHighlighterAttributes;
    fIdentAttri: TSynHighlighterAttributes;
    fPrevAttri: TSynHighlighterAttributes;
    fFlagAttri: TSynHighlighterAttributes;
    procedure IdentProc;
    procedure KeyProc;
    procedure CRProc;
    procedure SetAttribute(AnIndex: TSynPasAttribute; AValue: TSynHighlighterAttributes);
    procedure TextProc;
    procedure LFProc;
    procedure NullProc;
    procedure HashProc;
    procedure SpaceProc;
    procedure StringProc;
    procedure MakeMethodTables;
  protected
    {General Stuff}
    function GetIdentChars: TSynIdentChars; override;
    function GetSampleSource: String; override;
    function GetBracketKinds(AnIndex: integer; AnOpeningToken: boolean): String; override;
  public
    class function GetLanguageName: string; override;
    function IsKeyword(const AKeyword: string): boolean; override;
  public
    constructor Create(AOwner: TComponent); override;
    function GetDefaultAttribute(Index: integer): TSynHighlighterAttributes;
      override;
    function GetEol: Boolean; override;
    function GetTokenID: TtkTokenKind;
    procedure InitForScanningLine; override;
    function GetToken: String; override;
    procedure GetTokenEx(out TokenStart: PChar; out TokenLength: integer); override;
    function GetTokenAttribute: TLazEditTextAttribute; override;
    function GetTokenKind: integer; override;
    function GetTokenPos: Integer; override;
    procedure Next; override;

    function BracketKindCount: integer; override;
    function GetBracketContextAt(const ALineIdx: TLineIdx; const ALogX: IntPos;
      const AByteLen: Integer; const AKind: integer; var AFlags: TLazEditBracketInfoFlags; out
      AContext, ANestLevel: Integer; var InternalInfo: PtrUInt): Boolean; override;
  published
    property CommentAttri: TSynHighlighterAttributes index attribComment read fCommentAttri write SetAttribute;
    property TextAttri   : TSynHighlighterAttributes index attribText read fTextAttri write SetAttribute;
    property KeyAttri    : TSynHighlighterAttributes index attribKey read fKeyAttri write SetAttribute;
    property SpaceAttri  : TSynHighlighterAttributes index attribSpace read fSpaceAttri write SetAttribute;
  end;

implementation



const
  PoKeysCount = 3;
  PoKeys: array[1..PoKeysCount] of string = (
    'msgid', 'msgstr', 'msgctxt');


procedure TSynPoSyn.MakeMethodTables;
var
  i: Char;
begin
  for i := #0 to #255 do
    case i of
      #0      : fProcTable[i] := @NullProc;
      #10 {LF}: fProcTable[i] := @LFProc;
      #13 {CR}: fProcTable[i] := @CRProc;
      #34 {"} : fProcTable[i] := @StringProc;
      'A'..'Z', 'a'..'z', '_': fProcTable[I] := @IdentProc;
      '#' {#} : fProcTable[i] := @HashProc;
      #1..#9, #11, #12, #14..#32: fProcTable[i] := @SpaceProc;
    else
      fProcTable[i] := @TextProc;
    end;
end;

constructor TSynPoSyn.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  fCommentAttri            := TSynHighlighterAttributes.Create(@SYNS_AttrComment);
  fCommentAttri.Style      := [fsItalic];
  fCommentAttri.Foreground := clGreen;
  AddAttribute(fCommentAttri);

  fTextAttri               := TSynHighlighterAttributes.Create(@SYNS_AttrText);
  AddAttribute(fTextAttri);

  fKeyAttri                := TSynHighlighterAttributes.Create(@SYNS_AttrKey);
  fKeyAttri.Foreground     := clBlue;
  fKeyAttri.Style          := [fsBold];
  AddAttribute(fKeyAttri);

  fIdentAttri := TSynHighlighterAttributes.Create(@SYNS_AttrIdentifier, SYNS_XML_AttrIdentifier);
  fIdentAttri.Foreground   := clGreen;
  fIdentAttri.Style        := [fsBold];
  AddAttribute(fIdentAttri);

  fPrevAttri  := TSynHighlighterAttributes.Create(@SYNS_AttrPrevValue, SYNS_XML_AttrPrevValue);
  fPrevAttri.Foreground    := clOlive;
  fPrevAttri.Style         := [fsItalic];
  AddAttribute(fPrevAttri);

  fFlagAttri  := TSynHighlighterAttributes.Create(@SYNS_AttrFlags, SYNS_XML_AttrFlags);
  fFlagAttri.Foreground    := clTeal;
  AddAttribute(fFlagAttri);

  fSpaceAttri              := TSynHighlighterAttributes.Create(@SYNS_AttrSpace, SYNS_XML_AttrSpace);
  AddAttribute(fSpaceAttri);

  fStringAttri             := TSynHighlighterAttributes.Create(@SYNS_AttrString, SYNS_XML_AttrString);
  fStringAttri.Foreground  := clFuchsia;
  AddAttribute(fStringAttri);

  SetAttributesOnChange(@DefHighlightChange);

  fDefaultFilter      := SYNS_FilterPo;
  MakeMethodTables;
end; { Create }

procedure TSynPoSyn.InitForScanningLine;
begin
  inherited;
  Run := 0;
  Next;
end;


procedure TSynPoSyn.IdentProc;
begin
  while LinePtr[Run] in GetIdentChars {['A'..'Z','a'..'z']} do inc(Run);
  if IsKeyWord(GetToken) then begin
    fTokenId := tkKey;
    Exit;
  end
  else fTokenId := tkUnknown;
end;

procedure TSynPoSyn.CRProc;
begin
  fTokenID := tkSpace;
  Case LinePtr[Run + 1] of
    #10: inc(Run, 2);
  else inc(Run);
  end;
end;

procedure TSynPoSyn.SetAttribute(AnIndex: TSynPasAttribute; AValue: TSynHighlighterAttributes);
begin
  case AnIndex of
    attribComment: FCommentAttri.Assign(AValue);
    attribText:    FTextAttri.Assign(AValue);
    attribKey:     FKeyAttri.Assign(AValue);
    attribSpace:   FSpaceAttri.Assign(AValue);
  end;
end;


procedure TSynPoSyn.KeyProc;
begin
  fTokenID := tkKey;
  inc(Run);
  while LinePtr[Run] <> #0 do
    case LinePtr[Run] of
      #32: break;
      #10: break;
      #13: break;
    else inc(Run);
    end;
end;

procedure TSynPoSyn.TextProc;
begin
  if Run = 0 then
    IdentProc
  else begin
    inc(Run);
    while (LinePtr[Run] in [#128..#191]) OR // continued utf8 subcode
     ((LinePtr[Run]<>#0) and (fProcTable[LinePtr[Run]] = @TextProc)) do inc(Run);
    fTokenID := tkText;
  end;
end;

procedure TSynPoSyn.LFProc;
begin
  fTokenID := tkSpace;
  inc(Run);
end;

procedure TSynPoSyn.NullProc;
begin
  fTokenID := tkNull;
end;



procedure TSynPoSyn.SpaceProc;
begin
  inc(Run);
  fTokenID := tkSpace;
  while LinePtr[Run] in [#1..#9, #11, #12, #14..#32] do inc(Run);
end;


procedure TSynPoSyn.StringProc;
var
  FirstQuotePos, LastQuotePos: longint;
begin
  FirstQuotePos := Run;
  LastQuotePos := FirstQuotePos;
  fTokenID := tkString;
  while LinePtr[Run] <> #0 do
  begin
    case LinePtr[Run] of
      #10, #13: break;
      #34: if (Run <= 0) or (LinePtr[Run - 1] <> '\') then LastQuotePos := Run;
    end;
    inc(Run);
  end;
  if FirstQuotePos <> LastQuotePos then
    Run := LastQuotePos;
  if LinePtr[Run] <> #0 then
    inc(Run);
end;


procedure TSynPoSyn.HashProc;
begin
  // if it is not column 0 mark as tkText and get out of here
  if Run > 0 then
  begin
    fTokenID := tkText;
    inc(Run);
    Exit;
  end;

  // this is column 0 --> ok
  fTokenID := tkComment;

  while LinePtr[Run] <> #0 do
    case LinePtr[Run] of
      #10: break;
      #13: break;
      ':': begin if (Run = 1) then fTokenId := tkIdentifier; Inc(Run) end;
      ',': begin if (Run = 1) then  fTokenId := tkFlags;  Inc(Run) end;
      '|': begin if (Run = 1) then  fTokenId := tkPrevValue; Inc(Run) end;
    else inc(Run);
    end;
end;

procedure TSynPoSyn.Next;
begin
  fTokenPos := Run;
  fProcTable[LinePtr[Run]];
end;

function TSynPoSyn.BracketKindCount: integer;
begin
  Result := PO_BRACKET_KIND_TOKEN_COUNT;
end;

function TSynPoSyn.GetBracketContextAt(const ALineIdx: TLineIdx; const ALogX: IntPos;
  const AByteLen: Integer; const AKind: integer; var AFlags: TLazEditBracketInfoFlags; out
  AContext, ANestLevel: Integer; var InternalInfo: PtrUInt): Boolean;
var
  LogIdx: Integer;
begin
  if LineIndex <> ALineIdx then
    StartAtLineIndex(ALineIdx);
  //IsInNextToEOL := True;
  NextToLogX(ALogX, True);
  //IsInNextToEOL := False;

  AContext := ord(FTokenID);
  ANestLevel := 0;
  AFlags := AFlags + [bfNoLanguageContext, bfUnknownNestLevel];
  Result := True;

  if AKind = 3 then begin // "
    LogIdx := ToIdx(ALogX);
    AFlags := AFlags + [bfSingleLine];
    if FTokenID <> tkString then
      AFlags := AFlags + [bfUniform, bfNotNestable] - [bfUnknownNestLevel, bfOpen]
    else
    if LogIdx = fTokenPos then // open
      AFlags := AFlags + [bfOpen, bfNotNestable] - [bfUnknownNestLevel]
    else
    if LogIdx = Run-1 then // close
      AFlags := AFlags + [bfNotNestable] - [bfOpen, bfUnknownNestLevel]
    else
      AFlags := AFlags + [bfUniform, bfNotNestable] - [bfUnknownNestLevel, bfOpen];
  end;

end;

function TSynPoSyn.GetDefaultAttribute(Index: integer): TSynHighlighterAttributes;
begin
  case Index of
    SYN_ATTR_COMMENT: Result := fCommentAttri;
    SYN_ATTR_KEYWORD: Result := fKeyAttri;
    SYN_ATTR_STRING: Result := fStringAttri;
    SYN_ATTR_WHITESPACE: Result := fSpaceAttri;
  else
    Result := nil;
  end;
end;

function TSynPoSyn.GetEol: Boolean;
begin
  Result := fTokenId = tkNull;
end;

function TSynPoSyn.GetToken: String;
var
  Len: LongInt;
begin
  Len := Run - fTokenPos;
  SetString(Result, (LinePtr + fTokenPos), Len);
end;

procedure TSynPoSyn.GetTokenEx(out TokenStart: PChar; out TokenLength: integer);
begin
  TokenLength := Run - fTokenPos;
  TokenStart := LinePtr + fTokenPos;
end;

function TSynPoSyn.GetTokenID: TtkTokenKind;
begin
  Result := fTokenId;
end;

function TSynPoSyn.GetTokenAttribute: TLazEditTextAttribute;
begin
  case fTokenID of
    tkComment: Result := fCommentAttri;
    tkText   : Result := fTextAttri;
    tkKey    : Result := fKeyAttri;
    tkSpace  : Result := fSpaceAttri;
    tkString : Result := fStringAttri;
    tkIdentifier: Result := fIdentAttri;
    tkFlags:       Result := fFlagAttri;
    tkPrevValue:  Result := fPrevAttri;
    tkUnknown: Result := fTextAttri;
    else Result := nil;
  end;
end;

function TSynPoSyn.GetTokenKind: integer;
begin
  Result := Ord(fTokenId);
end;

function TSynPoSyn.GetTokenPos: Integer;
begin
 Result := fTokenPos;
end;

function TSynPoSyn.GetIdentChars: TSynIdentChars;
begin
  Result := [#33..#255];
end;

class function TSynPoSyn.GetLanguageName: string;
begin
  Result := SYNS_LangPo;
end;

function TSynPoSyn.GetSampleSource: String;
begin
  Result := '"Project-Id-Version: \n"' + LineEnding +
            '"POT-Creation-Date: \n"' + LineEnding +
            '"MIME-Version: 1.0\n"' + LineEnding +
            '"Content-Type: text/plain; charset=UTF-8\n"' + LineEnding +
            '"Content-Transfer-Encoding: 8bit\n"' + LineEnding +
            LineEnding +
            '#: lazarusidestrconsts.dlgcochecks' + LineEnding +
            '#, fuzzy' + LineEnding +
            '#| msgid "Checks:"' + LineEnding +
            'msgid "Checks"' + LineEnding +
            'msgstr "Controleert:"' + LineEnding +
            LineEnding +
            '#: lazarusidestrconsts.listemplateeditparamcellhelp' + LineEnding +
            'msgid ""' + LineEnding +
            '"Inserts an editable Cell, with a default value\n"' + LineEnding +
            '"\"\",Sync=n (,S=n), to Sync with a previous cell (n=1 to highest prev cell\n"' + LineEnding +
            '"\"default\",Sync, to Sync with a previous cell of equal default\n"' + LineEnding +
            'msgstr ""';

end;

function TSynPoSyn.GetBracketKinds(AnIndex: integer; AnOpeningToken: boolean): String;
begin
  Result := PO_BRACKET_KIND_TOKENS[AnOpeningToken, AnIndex]
end;

function TSynPoSyn.IsKeyword(const AKeyword: string): boolean;
var
  i: Integer;
begin
  //There are only 3 keywords, so no need to make a hashtable
  for i := 1 to PoKeysCount do
    if CompareText(PoKeys[i], AKeyWord) = 0 then
      Exit(True);
  Result := False;
end;

initialization
  RegisterPlaceableHighlighter(TSynPoSyn);

end.
