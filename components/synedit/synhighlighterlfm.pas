{-------------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: SynHighlighterDfm.pas, released 2000-04-14.
The Original Code is based on the dmDfmSyn.pas file from the
mwEdit component suite by Martin Waldenburg and other developers, the Initial
Author of this file is David H. Muir.
All Rights Reserved.

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

$Id$

You may retrieve the latest version of this file at the SynEdit home page,
located at http://SynEdit.SourceForge.net

Known Issues:
-------------------------------------------------------------------------------}
{
@abstract(Provides a Delphi Form Source highlighter for SynEdit)
@author(David Muir <david@loanhead45.freeserve.co.uk>)
@created(April 13, 2000)
@lastmod(2000-06-23)
The SynHighlighterLFM unit provides SynEdit with a Delphi Form Source (.LFM) highlighter.
The highlighter formats form source code similar to when forms are viewed as text in the Delphi editor.
}
unit SynHighlighterLFM;

{$I SynEdit.inc}

interface

uses
  SysUtils, Classes,
  {$IFDEF SYN_CLX}
  Qt, QControls, QGraphics,
  {$ELSE}
  {$IFDEF SYN_LAZARUS}
  LCLIntf, LCLType,
  {$ELSE}
  Windows, Messages, Registry,
  {$ENDIF}
  Controls, Graphics,
  {$ENDIF}
  SynEditTypes, SynEditHighlighter;

type
  TtkTokenKind = (tkComment, tkIdentifier, tkKey, tkNull, tkNumber, tkSpace,
    tkString, tkSymbol, tkUnknown);

  TRangeState = (rsANil, rsComment, rsUnKnown);

  TProcTableProc = procedure of object;

type
  TSynLFMSyn = class(TSynCustomHighlighter)
  private
    fRange: TRangeState;
    fLine: PChar;
    fLineNumber: Integer;
    fProcTable: array[#0..#255] of TProcTableProc;
    Run: LongInt;
    fTokenPos: Integer;
    FTokenID: TtkTokenKind;
    fCommentAttri: TSynHighlighterAttributes;
    fIdentifierAttri: TSynHighlighterAttributes;
    fKeyAttri: TSynHighlighterAttributes;
    fNumberAttri: TSynHighlighterAttributes;
    fSpaceAttri: TSynHighlighterAttributes;
    fStringAttri: TSynHighlighterAttributes;
    fSymbolAttri: TSynHighlighterAttributes;
    procedure AltProc;
    procedure AsciiCharProc;
    procedure BraceCloseProc;
    procedure BraceOpenProc;
    procedure CommentProc;
    procedure CRProc;
    procedure EndProc;
    procedure IntegerProc;
    procedure LFProc;
    procedure NullProc;
    procedure NumberProc;
    procedure ObjectProc;
    procedure InheritedProc;
    procedure SpaceProc;
    procedure StringProc;
    procedure SymbolProc;
    procedure UnknownProc;
    procedure MakeMethodTables;
  protected
    function GetIdentChars: TSynIdentChars; override;
    function GetSampleSource: string; override;
  public
    {$IFNDEF SYN_CPPB_1} class {$ENDIF}                                         //mh 2000-07-14
    function GetLanguageName: string; override;
  public
    constructor Create(AOwner: TComponent); override;
    function GetDefaultAttribute(Index: integer): TSynHighlighterAttributes;
      override;
    function GetEol: Boolean; override;
    function GetRange: Pointer; override;
    function GetTokenID: TtkTokenKind;
    procedure SetLine({$IFDEF FPC}const {$ENDIF}NewValue: String;
      LineNumber: Integer); override;
    function GetToken: String; override;
    {$IFDEF SYN_LAZARUS}
    procedure GetTokenEx(var TokenStart: PChar; var TokenLength: integer); override;
    {$ENDIF}
    function GetTokenAttribute: TSynHighlighterAttributes; override;
    function GetTokenKind: integer; override;
    function GetTokenPos: Integer; override;
    procedure Next; override;
    procedure SetRange(Value: Pointer); override;
    procedure ResetRange; override;
    property IdentChars;
  published
    property CommentAttri: TSynHighlighterAttributes read fCommentAttri
      write fCommentAttri;
    property IdentifierAttri: TSynHighlighterAttributes read fIdentifierAttri
      write fIdentifierAttri;
    property KeyAttri: TSynHighlighterAttributes read fKeyAttri write fKeyAttri;
    property NumberAttri: TSynHighlighterAttributes read fNumberAttri
      write fNumberAttri;
    property SpaceAttri: TSynHighlighterAttributes read fSpaceAttri
      write fSpaceAttri;
    property StringAttri: TSynHighlighterAttributes read fStringAttri
      write fStringAttri;
  end;

function LoadLFMFile2Strings(const AFile: string; AStrings: TStrings;
  var WasText: boolean): integer;
function SaveStrings2LFMFile(AStrings: TStrings; const AFile: string): integer;

implementation

uses
  SynEditStrConst;

{ A couple of useful Delphi Form functions }

function LoadLFMFile2Strings(const AFile: string; AStrings: TStrings;
  var WasText: boolean): integer;
var
  Src, Dest: TStream;
{$IFDEF SYN_COMPILER_5_UP}
  origFormat: TStreamOriginalFormat;
{$ENDIF}
begin
  Result := 0;
  WasText := FALSE;
  AStrings.Clear;
  try
    Src := TFileStream.Create(AFile, fmOpenRead or fmShareDenyWrite);
    try
      Dest := TMemoryStream.Create;
      try
{$IFDEF SYN_COMPILER_5_UP}
        origFormat := sofUnknown;                                               //riceball 2000-11-17
        ObjectResourceToText(Src, Dest, origFormat);
        WasText := origFormat = sofText;
{$ELSE}
        ObjectResourceToText(Src, Dest);
{$ENDIF}
        Dest.Seek(0, soFromBeginning);
        AStrings.LoadFromStream(Dest);
      finally
        Dest.Free;
      end;
    finally
      Src.Free;
    end;
  except
    on E: EInOutError do Result := -E.ErrorCode;
    else Result := -1;
  end;
end;

function SaveStrings2LFMFile(AStrings: TStrings; const AFile: string): integer;
var
  Src, Dest: TStream;
begin
  Result := 0;
  try
    Src := TMemoryStream.Create;
    try
      AStrings.SaveToStream(Src);
      Src.Seek(0, soFromBeginning);
      Dest := TFileStream.Create(AFile, fmCreate);
      try
        ObjectTextToResource(Src, Dest);
      finally
        Dest.Free;
      end;
    finally
      Src.Free;
    end;
  except
    on E: EInOutError do Result := -E.ErrorCode;
    else Result := -1;
  end;
end;

{ TSynLFMSyn }

procedure TSynLFMSyn.MakeMethodTables;
var
  I: Char;
begin
  for I := #0 to #255 do
    case I of
      '#': fProcTable[I] := {$IFDEF FPC}@{$ENDIF}AsciiCharProc;
      '}': fProcTable[I] := {$IFDEF FPC}@{$ENDIF}BraceCloseProc;
      '{': fProcTable[I] := {$IFDEF FPC}@{$ENDIF}BraceOpenProc;
      #13: fProcTable[I] := {$IFDEF FPC}@{$ENDIF}CRProc;
      'A'..'Z', 'a'..'z', '_':
        if I in ['e', 'E'] then
          fProcTable[I] := {$IFDEF FPC}@{$ENDIF}EndProc
        else if I in ['o', 'O'] then
          fProcTable[I] := {$IFDEF FPC}@{$ENDIF}ObjectProc
        else if I in ['i', 'I'] then
          fProcTable[I] := {$IFDEF FPC}@{$ENDIF}InheritedProc
        else
          fProcTable[I] := {$IFDEF FPC}@{$ENDIF}AltProc;
      '$': fProcTable[I] := {$IFDEF FPC}@{$ENDIF}IntegerProc;
      #10: fProcTable[I] := {$IFDEF FPC}@{$ENDIF}LFProc;
      #0: fProcTable[I] := {$IFDEF FPC}@{$ENDIF}NullProc;
      '0'..'9': fProcTable[I] := {$IFDEF FPC}@{$ENDIF}NumberProc;
      '(', ')', '/', '=', '<', '>', '.', ',', '[', ']':
        fProcTable[I] := {$IFDEF FPC}@{$ENDIF}SymbolProc;
      #1..#9, #11, #12, #14..#32: fProcTable[I] := {$IFDEF FPC}@{$ENDIF}SpaceProc;
      #39: fProcTable[I] := {$IFDEF FPC}@{$ENDIF}StringProc;
    else fProcTable[I] := {$IFDEF FPC}@{$ENDIF}UnknownProc;
    end;
end;

constructor TSynLFMSyn.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  fCommentAttri := TSynHighlighterAttributes.Create(SYNS_AttrComment);
  fCommentAttri.Style := [fsItalic];
  AddAttribute(fCommentAttri);
  fIdentifierAttri := TSynHighlighterAttributes.Create(SYNS_AttrIdentifier);
  AddAttribute(fIdentifierAttri);
  fKeyAttri := TSynHighlighterAttributes.Create(SYNS_AttrKey);
  fKeyAttri.Style := [fsBold];
  AddAttribute(fKeyAttri);
  fNumberAttri := TSynHighlighterAttributes.Create(SYNS_AttrNumber);
  AddAttribute(fNumberAttri);
  fSpaceAttri := TSynHighlighterAttributes.Create(SYNS_AttrSpace);
  AddAttribute(fSpaceAttri);
  fStringAttri := TSynHighlighterAttributes.Create(SYNS_AttrString);
  AddAttribute(fStringAttri);
  fSymbolAttri := TSynHighlighterAttributes.Create(SYNS_AttrSymbol);
  AddAttribute(fSymbolAttri);
  SetAttributesOnChange({$IFDEF FPC}@{$ENDIF}DefHighlightChange);
  MakeMethodTables;
  fDefaultFilter := SYNS_FilterLFM;
  fRange := rsUnknown;
end;

procedure TSynLFMSyn.SetLine({$IFDEF FPC}const {$ENDIF}NewValue: String;
  LineNumber: Integer);
begin
  fLine := PChar(NewValue);
  Run := 0;
  fLineNumber := LineNumber;
  Next;
end;

procedure TSynLFMSyn.AltProc;
begin
  fTokenID := tkIdentifier;
  repeat
    Inc(Run);
  until not (fLine[Run] in ['_', '0'..'9', 'a'..'z', 'A'..'Z']);
end;

procedure TSynLFMSyn.AsciiCharProc;
begin
  fTokenID := tkString;
  repeat
    Inc(Run);
  until not (fLine[Run] in ['0'..'9']);
end;

procedure TSynLFMSyn.BraceCloseProc;
begin
  inc(Run);
  fRange := rsUnknown;
  fTokenId := tkIdentifier;
end;

procedure TSynLFMSyn.BraceOpenProc;
begin
  fRange := rsComment;
  CommentProc;
end;

procedure TSynLFMSyn.CommentProc;
begin
  fTokenID := tkComment;
  repeat
    inc(Run);
    if fLine[Run] = '}' then begin
      Inc(Run);
      fRange := rsUnknown;
      break;
    end;
  until fLine[Run] in [#0, #10, #13];
end;

procedure TSynLFMSyn.CRProc;
begin
  fTokenID := tkSpace;
  Inc(Run);
  if (fLine[Run] = #10) then Inc(Run);
end;

procedure TSynLFMSyn.EndProc;
begin
  if (fLine[Run + 1] in ['n', 'N']) and
     (fLine[Run + 2] in ['d', 'D']) and
     not (fLine[Run + 3] in ['_', '0'..'9', 'a'..'z', 'A'..'Z'])
  then begin
    fTokenID := tkKey;
    Inc(Run, 3);
  end else
    AltProc;
end;

procedure TSynLFMSyn.IntegerProc;
begin
  fTokenID := tkNumber;
  repeat
    inc(Run);
  until not (fLine[Run] in ['0'..'9', 'A'..'F', 'a'..'f']);
end;

procedure TSynLFMSyn.LFProc;
begin
  fTokenID := tkSpace;
  inc(Run);
end;

procedure TSynLFMSyn.NullProc;
begin
  fTokenID := tkNull;
end;

procedure TSynLFMSyn.NumberProc;
begin
  fTokenID := tkNumber;
  repeat
    Inc(Run);
    if fLine[Run] = '.' then begin
      if fLine[Run + 1] <> '.' then Inc(Run);
      break;
    end;
  until not (fLine[Run] in ['0'..'9', 'e', 'E']);
end;

procedure TSynLFMSyn.ObjectProc;
begin
  if (fLine[Run + 1] in ['b', 'B']) and
     (fLine[Run + 2] in ['j', 'J']) and
     (fLine[Run + 3] in ['e', 'E']) and
     (fLine[Run + 4] in ['c', 'C']) and
     (fLine[Run + 5] in ['t', 'T']) and
     not (fLine[Run + 6] in ['_', '0'..'9', 'a'..'z', 'A'..'Z'])
  then
  begin
    fTokenID := tkKey;
    Inc(Run, 6);
  end
  else
    AltProc;
end;

procedure TSynLFMSyn.InheritedProc;
begin
  if (fLine[Run + 1] in ['n', 'N']) and
     (fLine[Run + 2] in ['h', 'H']) and
     (fLine[Run + 3] in ['e', 'E']) and
     (fLine[Run + 4] in ['r', 'R']) and
     (fLine[Run + 5] in ['i', 'I']) and
     (fLine[Run + 6] in ['t', 'T']) and
     (fLine[Run + 7] in ['e', 'E']) and
     (fLine[Run + 8] in ['d', 'D']) and
     not (fLine[Run + 9] in ['_', '0'..'9', 'a'..'z', 'A'..'Z'])
  then
  begin
    fTokenID := tkKey;
    Inc(Run, 9);
  end
  else
    AltProc;
end;

procedure TSynLFMSyn.SpaceProc;
begin
  fTokenID := tkSpace;
  repeat
    Inc(Run);
  until (fLine[Run] > #32) or (fLine[Run] in [#0, #10, #13]);
end;

procedure TSynLFMSyn.StringProc;
begin
  fTokenID := tkString;
  repeat
    Inc(Run);
    if fLine[Run] = '''' then begin
      Inc(Run);
      if fLine[Run] <> '''' then break
    end;
  until fLine[Run] in [#0, #10, #13];
end;

procedure TSynLFMSyn.SymbolProc;
begin
  inc(Run);
  fTokenID := tkSymbol;
end;

procedure TSynLFMSyn.UnknownProc;
begin
{$IFDEF SYN_MBCSSUPPORT}
  if FLine[Run] in LeadBytes then
    Inc(Run,2)
  else
{$ENDIF}
  inc(Run);
  fTokenID := tkUnknown;
end;

procedure TSynLFMSyn.Next;
begin
  fTokenPos := Run;
  if fRange = rsComment then begin
    if fLine[Run] = #0 then NullProc
                       else CommentProc;
  end else
    fProcTable[fLine[Run]]{$IFDEF FPC}(){$ENDIF};
end;

function TSynLFMSyn.GetDefaultAttribute(Index: integer): TSynHighlighterAttributes;
begin
  case Index of
    SYN_ATTR_COMMENT: Result := fCommentAttri;
    SYN_ATTR_IDENTIFIER: Result := fIdentifierAttri;
    SYN_ATTR_KEYWORD: Result := fKeyAttri;
    SYN_ATTR_STRING: Result := fStringAttri;
    SYN_ATTR_WHITESPACE: Result := fSpaceAttri;
    SYN_ATTR_SYMBOL: Result := fSymbolAttri;
  else
    Result := nil;
  end;
end;

function TSynLFMSyn.GetEol: Boolean;
begin
  Result := fTokenId = tkNull;
end;

function TSynLFMSyn.GetRange: Pointer;
begin
  Result := Pointer(PtrInt(fRange));
end;

function TSynLFMSyn.GetTokenID: TtkTokenKind;
begin
  Result := fTokenId;
end;

function TSynLFMSyn.GetToken: String;
var
  Len: LongInt;
begin
  Result := '';
  Len := Run - fTokenPos;
  SetString(Result, (FLine + fTokenPos), Len);
end;

{$IFDEF SYN_LAZARUS}
procedure TSynLFMSyn.GetTokenEx(var TokenStart: PChar;
  var TokenLength: integer);
begin
  TokenLength:=Run-fTokenPos;
  TokenStart:=FLine + fTokenPos;
end;
{$ENDIF}

function TSynLFMSyn.GetTokenAttribute: TSynHighlighterAttributes;
begin
  case fTokenID of
    tkComment: Result := fCommentAttri;
    tkIdentifier: Result := fIdentifierAttri;
    tkKey: Result := fKeyAttri;
    tkNumber: Result := fNumberAttri;
    tkSpace: Result := fSpaceAttri;
    tkString: Result := fStringAttri;
    tkSymbol: Result := fSymbolAttri;
    tkUnknown: Result := fIdentifierAttri;
    else Result := nil;
  end;
end;

function TSynLFMSyn.GetTokenKind: integer;
begin
  Result := Ord(fTokenID);
end;

function TSynLFMSyn.GetTokenPos: Integer;
begin
  Result := fTokenPos;
end;

procedure TSynLFMSyn.ResetRange;
begin
  fRange := rsUnknown;
end;

procedure TSynLFMSyn.SetRange(Value: Pointer);
begin
  fRange := TRangeState(PtrInt(Value));
end;

function TSynLFMSyn.GetIdentChars: TSynIdentChars;
begin
  Result := TSynValidStringChars;
end;

{$IFNDEF SYN_CPPB_1} class {$ENDIF}                                             //mh 2000-07-14
function TSynLFMSyn.GetLanguageName: string;
begin
  Result := SYNS_LangLFM;
end;

function TSynLFMSyn.GetSampleSource: string;
begin
  Result := '{ Delphi/C++ Builder Form Definitions }'#13#10 +
            'object TestForm: TTestForm'#13#10 +
            '  Left = 273'#13#10 +
            '  Top = 103'#13#10 +
            '  Caption = ''SynEdit sample source'''#13#10 +
            'end';
end; { GetSampleSource }

{$IFNDEF SYN_CPPB_1}                                                            //mh 2000-07-14
initialization
  RegisterPlaceableHighlighter(TSynLFMSyn);
{$ENDIF}
end.
