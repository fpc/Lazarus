{-------------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: SynHighlighterGeneral.pas, released 2000-04-07.
The Original Code is based on the mwGeneralSyn.pas file from the
mwEdit component suite by Martin Waldenburg and other developers, the Initial
Author of this file is Martin Waldenburg.
Portions written by Martin Waldenburg are copyright 1999 Martin Waldenburg.
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

$Id: SynHighlighterGeneral.pas,v 1.3 2000/11/08 22:09:59 mghie Exp $

You may retrieve the latest version of this file at the SynEdit home page,
located at http://SynEdit.SourceForge.net

Known Issues:
-------------------------------------------------------------------------------

@abstract(Provides a customizable highlighter for SynEdit)
@author(Martin Waldenburg, converted to SynEdit by Michael Hieke)
@created(1999)
@lastmod(2000-06-23)
The SynHighlighterGeneral unit provides a customizable highlighter for SynEdit.

Notes: March 21, 2006

 -SynHighlighterAny.pas converted for use with Lazarus by Lars (L505)
 -See $IFDEF FPC and $IFDEF SYN_LAZARUS defines for changes and additions.
 
}



unit SynHighlighterAny;

{$I SynEdit.inc}

interface

uses
  SysUtils, Classes, FileUtil, Controls, Graphics, Registry,
  SynEditTypes, SynEditHighlighter;

type
  TtkTokenKind = (tkComment, tkIdentifier, tkKey, tkNull, tkNumber,
    tkPreprocessor, tkSpace, tkString, tkSymbol, tkUnknown, tkConstant, tkObject, tkEntity, tkDollarVariable, tkDot);

  TCommentStyle = (csAnsiStyle, csPasStyle, csCStyle, csAsmStyle, csBasStyle, csVBStyle);
  CommentStyles = set of TCommentStyle;

  TRangeState = (rsANil, rsAnsi, rsPasStyle, rsCStyle, rsUnKnown);

  TStringDelim = (sdSingleQuote, sdDoubleQuote);

  TProcTableProc = procedure of object;

type
  TIniList= class(TStringList)
  private
    function GetKeyIndex(asection,akey:string):integer;
    function GetKeyValue(asection,akey:string):string;    
  public
    function ReadString(asection,akey,adefault:string):string;
    function ReadInteger(asection,akey:string; adefault:integer):integer;
    function ReadBool(asection,akey:string; adefault:boolean):boolean;
    procedure ReadSectionNames(asection:string;alist:TStrings);
  end;

  { TSynAnySyn }

  TSynAnySyn = class(TSynCustomHighlighter)
  private
    fUserData:TIniList;
    fMarkupOn:boolean;
    fRange: TRangeState;
    fLine: PChar;
    fProcTable: array[#0..#255] of TProcTableProc;
    Run: LongInt;
    fTokenPos: Integer;
    fTokenID: TtkTokenKind;
    fLineNumber : Integer;
    fCommentAttri: TSynHighlighterAttributes;
    fIdentifierAttri: TSynHighlighterAttributes;
    fKeyAttri: TSynHighlighterAttributes;
    fConstantAttri: TSynHighlighterAttributes;
    fObjectAttri: TSynHighlighterAttributes;
    fNumberAttri: TSynHighlighterAttributes;
    fPreprocessorAttri: TSynHighlighterAttributes;
    fSpaceAttri: TSynHighlighterAttributes;
    fStringAttri: TSynHighlighterAttributes;
    fSymbolAttri: TSynHighlighterAttributes;
    fKeyWords: TStrings;
    fConstants:TStrings;
    fObjects:TStrings;
    fComments: CommentStyles;
    fStringDelimCh: char;
    fIdentChars: TSynIdentChars;
    fDetectPreprocessor: boolean;
    FMarkup: boolean;
    FEntity: boolean;
    fEntityAttri: TSynHighlighterAttributes;
    FDollarVariables: boolean;
    fVariableAttri: TSynHighlighterAttributes;
    FActiveDot: boolean;
    FDotAttri: TSynHighlighterAttributes;
    procedure ApostropheProc;
    procedure AmpersandProc;
    procedure AsciiCharProc;
    procedure BraceOpenProc;
    procedure PointCommaProc;
    procedure CRProc;
    procedure DotProc;    
    procedure IdentProc;
    procedure IntegerProc;
    procedure DollarProc;
    procedure LFProc;
    procedure NullProc;
    procedure NumberProc;
    procedure RoundOpenProc;
    procedure RoundCloseProc;
    procedure SlashProc;
    procedure SpaceProc;
    procedure StringProc;
    procedure UnknownProc;
    procedure GreaterThan;
    procedure SmallerThan;
    procedure MakeMethodTables;
    procedure AnsiProc;
    procedure PasStyleProc;
    procedure CStyleProc;
    procedure SetKeyWords(const Value: TStrings);
    procedure SetComments(Value: CommentStyles);
    function GetStringDelim: TStringDelim;
    procedure SetStringDelim(const Value: TStringDelim);
    function GetIdentifierChars: string;
    procedure SetIdentifierChars(const Value: string);
    procedure SetDetectPreprocessor(Value: boolean);
    procedure SetConstants(const Value: TStrings);
    procedure SetObjects(const Value: TStrings);
    function IsObject(const AObject: string): boolean;
    procedure SetMarkup(const Value: boolean);
    procedure SetEntity(const Value: boolean);
    procedure SetDollarVariables(const Value: boolean);
    procedure SetActiveDot(const Value: boolean);
    procedure SetDotAttri(const Value: TSynHighlighterAttributes);
  protected
    function GetIdentChars: TSynIdentChars; override;
  public
    class function GetLanguageName: string; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function GetDefaultAttribute(Index: integer): TSynHighlighterAttributes;
      override;
    function GetEol: Boolean; override;
    function GetRange: Pointer; override;
    function GetTokenID: TtkTokenKind;
    function GetToken: String; override;
    {$IFDEF SYN_LAZARUS}
    procedure GetTokenEx(out TokenStart: PChar; out TokenLength: integer); override;
    {$ENDIF}
    function GetTokenAttribute: TSynHighlighterAttributes; override;
    function GetTokenKind: integer; override;
    function GetTokenPos: Integer; override;
    function IsKeyword(const AKeyword: string): boolean; override;              //mh 2000-11-08
    function IsConstant(const AConstant: string): boolean;
    procedure Next; override;
    procedure ResetRange; override;
    procedure SetRange(Value: Pointer); override;
    procedure SetLine(const NewValue: String; LineNumber: Integer); override;
    function SaveToRegistry(RootKey: HKEY; Key: string): boolean; override;
    function LoadFromRegistry(RootKey: HKEY; Key: string): boolean; override;
    procedure LoadHighLighter(aFile: string);
  published
    property CommentAttri: TSynHighlighterAttributes read fCommentAttri
      write fCommentAttri;
    property Comments: CommentStyles read fComments write SetComments;
    property DetectPreprocessor: boolean read fDetectPreprocessor
      write SetDetectPreprocessor;
    property IdentifierAttri: TSynHighlighterAttributes read fIdentifierAttri
      write fIdentifierAttri;
    property IdentifierChars: string read GetIdentifierChars
      write SetIdentifierChars;
    property KeyAttri: TSynHighlighterAttributes read fKeyAttri write fKeyAttri;
    property ConstantAttri: TSynHighlighterAttributes read fConstantAttri write fConstantAttri;
    property ObjectAttri: TSynHighlighterAttributes read fObjectAttri write fObjectAttri;
    property EntityAttri: TSynHighlighterAttributes read fEntityAttri write fEntityAttri;
    property VariableAttri: TSynHighlighterAttributes read fVariableAttri write fVariableAttri;
    property DotAttri: TSynHighlighterAttributes read FDotAttri write SetDotAttri;
    property KeyWords: TStrings read fKeyWords write SetKeyWords;
    property Constants: TStrings read fConstants write SetConstants;
    property Objects: TStrings read fObjects write SetObjects;
    property NumberAttri: TSynHighlighterAttributes read fNumberAttri
      write fNumberAttri;
    property PreprocessorAttri: TSynHighlighterAttributes
      read fPreprocessorAttri write fPreprocessorAttri;
    property SpaceAttri: TSynHighlighterAttributes read fSpaceAttri
      write fSpaceAttri;
    property StringAttri: TSynHighlighterAttributes read fStringAttri
      write fStringAttri;
    property SymbolAttri: TSynHighlighterAttributes read fSymbolAttri
      write fSymbolAttri;
    property StringDelim: TStringDelim read GetStringDelim write SetStringDelim
      default sdSingleQuote;
    property Markup:boolean read FMarkup write SetMarkup;
    property Entity:boolean read FEntity write SetEntity;
    property DollarVariables:boolean read FDollarVariables write SetDollarVariables;
    property ActiveDot:boolean read FActiveDot write SetActiveDot;
  end;

implementation

uses
  SynEditStrConst;

var
  Identifiers: array[#0..#255] of ByteBool;
{$IFNDEF SYN_LAZARUS}
  mHashTable: array[#0..#255] of Integer;
{$ENDIF}

procedure MakeIdentTable;
var
  I: Char;
  {$IFNDEF SYN_LAZARUS}
  J: Char;
  {$ENDIF}
  idents:string;
begin
  idents:='_0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ-?!';
  for I := #0 to #255 do
  begin
    if pos(i,idents)>0 then identifiers[i]:=true
    else identifiers[i]:=false;
//    case I in ['_', '0'..'9', 'a'..'z', 'A'..'Z','-','?','!'] of true: Identifiers[I] := True;
//    else Identifiers[I] := False;
//    end;
    {$IFNDEF SYN_LAZARUS}
    J := UpCase(I);
    Case I in ['_', 'a'..'z', 'A'..'Z'] of
      True: mHashTable[I] := Ord(J) - 64
    else mHashTable[I] := 0;
    end;
    {$ENDIF}
  end;
end;

function TSynAnySyn.IsKeyword(const AKeyword: string): boolean;             //mh 2000-11-08
var
  First, Last, I, Compare: Integer;
  Token: String;
begin
  First := 0;
  Last := fKeywords.Count - 1;
  Result := False;
  Token := UpperCase(AKeyword);
  while First <= Last do
  begin
    I := (First + Last) shr 1;
    {$IFDEF SYN_LAZARUS}
    Compare := AnsiCompareStr(fKeywords[i], Token);
    {$ELSE}
    Compare := CompareStr(fKeywords[i], Token);
    {$ENDIF}
    if Compare = 0 then
    begin
      Result := True;
      break;
    end else
      if Compare < 0 then First := I + 1 else Last := I - 1;
  end;
end; { IsKeyWord }


function TSynAnySyn.IsConstant(const AConstant: string): boolean;             //mh 2000-11-08
var
  First, Last, I, Compare: Integer;
  Token: String;
begin
  First := 0;
  Last := fConstants.Count - 1;
  Result := False;
  Token := UpperCase(AConstant);
  while First <= Last do
  begin
    I := (First + Last) shr 1;
    {$IFDEF SYN_LAZARUS}
    Compare := AnsiCompareStr(fConstants[i], Token);
    {$ELSE}
    Compare := CompareStr(fConstants[i], Token);
    {$ENDIF}
    if Compare = 0 then
    begin
      Result := True;
      break;
    end else
      if Compare < 0 then First := I + 1 else Last := I - 1;
  end;
end; { IsConstant }

function TSynAnySyn.IsObject(const AObject: string): boolean;             //mh 2000-11-08
var
  First, Last, I, Compare: Integer;
  Token: String;
begin
  First := 0;
  Last := fObjects.Count - 1;
  Result := False;
  Token := UpperCase(AObject);
  while First <= Last do
  begin
    I := (First + Last) shr 1;
    {$IFDEF SYN_LAZARUS}
    Compare := AnsiCompareStr(fObjects[i], Token);
    {$ELSE}
    Compare := CompareStr(fObjects[i], Token);
    {$ENDIF}
    if Compare = 0 then
    begin
      Result := True;
      break;
    end else
      if Compare < 0 then First := I + 1 else Last := I - 1;
  end;
end; { IsObject }


procedure TSynAnySyn.MakeMethodTables;
var
  I: Char;
begin
  for I := #0 to #255 do
    case I of
      #39: begin
             if csVBStyle in comments then begin
               fProcTable[I] := {$ifdef FPC}@{$endif}ApostropheProc;
               fStringDelimch:= #34;
             end
             else
               fProcTable[I] := {$ifdef FPC}@{$endif}UnknownProc;
           end;
      '<': begin
             if markup then
               fProcTable[i]:= {$ifdef FPC}@{$endif}SmallerThan
             else
               fProcTable[I] := {$ifdef FPC}@{$endif}UnknownProc;
           end;
      '>': begin
             if markup then
               fProcTable[i]:= {$ifdef FPC}@{$endif}GreaterThan
             else
               fProcTable[I] := {$ifdef FPC}@{$endif}UnknownProc;
           end;
      '&': begin
             if Entity then
               fProcTable[i]:= {$ifdef FPC}@{$endif}AmpersandProc
             else
               fProcTable[I] := {$ifdef FPC}@{$endif}UnknownProc;
           end;
      '#': fProcTable[I] := {$ifdef FPC}@{$endif}AsciiCharProc;
      '{': fProcTable[I] := {$ifdef FPC}@{$endif}BraceOpenProc;
      ';': fProcTable[I] := {$ifdef FPC}@{$endif}PointCommaProc;
      #13: fProcTable[I] := {$ifdef FPC}@{$endif}CRProc;
      'A'..'Z', 'a'..'z', '_': fProcTable[I] := {$ifdef FPC}@{$endif}IdentProc;
      '$': begin
             if dollarvariables then
               fProcTable[I] := {$ifdef FPC}@{$endif}DollarProc
             else
               fProcTable[I] := {$ifdef FPC}@{$endif}IntegerProc;
           end;
      '.': fProcTable[i] := {$ifdef FPC}@{$endif}DotProc;
      #10: fProcTable[I] := {$ifdef FPC}@{$endif}LFProc;
      #0: fProcTable[I] := {$ifdef FPC}@{$endif}NullProc;
      '0'..'9': fProcTable[I] := {$ifdef FPC}@{$endif}NumberProc;
      '(': fProcTable[I] := {$ifdef FPC}@{$endif}RoundOpenProc;
      ')': fProcTable[I] := {$ifdef FPC}@{$endif}RoundCloseProc;
      '/': fProcTable[I] := {$ifdef FPC}@{$endif}SlashProc;
      #1..#9, #11, #12, #14..#32: fProcTable[I] := {$ifdef FPC}@{$endif}SpaceProc;
      else fProcTable[I] := {$ifdef FPC}@{$endif}UnknownProc;
    end;
    fProcTable[fStringDelimCh] := {$ifdef FPC}@{$endif}StringProc;
end;

constructor TSynAnySyn.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  fUserData:= TIniList.Create;
  fKeyWords := TStringList.Create;
  TStringList(fKeyWords).Sorted := True;
  TStringList(fKeyWords).Duplicates := dupIgnore;
  fConstants := TStringList.Create;
  TStringList(fConstants).Sorted := True;
  TStringList(fConstants).Duplicates := dupIgnore;
  fObjects := TStringList.Create;
  TStringList(fObjects).Sorted := True;
  TStringList(fObjects).Duplicates := dupIgnore;
  fCommentAttri := TSynHighlighterAttributes.Create(SYNS_AttrComment, SYNS_XML_AttrComment);
  fCommentAttri.Style := [fsItalic];
  AddAttribute(fCommentAttri);
  fIdentifierAttri := TSynHighlighterAttributes.Create(SYNS_AttrIdentifier, SYNS_XML_AttrIdentifier);
  AddAttribute(fIdentifierAttri);
  fKeyAttri := TSynHighlighterAttributes.Create(SYNS_AttrReservedWord, SYNS_XML_AttrReservedWord);
  fKeyAttri.Style := [fsBold];
  AddAttribute(fKeyAttri);
  fConstantAttri := TSynHighlighterAttributes.Create('jan_constant');
  fConstantAttri.Style := [fsBold];
  fConstantAttri.Foreground:=clfuchsia;
  AddAttribute(fConstantAttri);
  fObjectAttri := TSynHighlighterAttributes.Create('jan_object');
  fObjectAttri.Style := [fsBold];
  fObjectAttri.Foreground:=clmaroon;
  AddAttribute(fObjectAttri);
  fEntityAttri := TSynHighlighterAttributes.Create('jan_entity');
  fEntityAttri.Style := [fsBold];
  fEntityAttri.Foreground:=cllime;
  AddAttribute(fEntityAttri);
  fDotAttri := TSynHighlighterAttributes.Create('jan_dot');
  fDotAttri.Style := [fsBold];
  fDotAttri.Foreground:=clgreen;
  AddAttribute(fDotAttri);
  fNumberAttri := TSynHighlighterAttributes.Create(SYNS_AttrNumber, SYNS_XML_AttrNumber);
  AddAttribute(fNumberAttri);
  fSpaceAttri := TSynHighlighterAttributes.Create(SYNS_AttrSpace, SYNS_XML_AttrSpace);
  AddAttribute(fSpaceAttri);
  fStringAttri := TSynHighlighterAttributes.Create(SYNS_AttrString, SYNS_XML_AttrString);
  AddAttribute(fStringAttri);
  fSymbolAttri := TSynHighlighterAttributes.Create(SYNS_AttrSymbol, SYNS_XML_AttrSymbol);
  AddAttribute(fSymbolAttri);
  fVariableAttri := TSynHighlighterAttributes.Create('jan_Variable');
  fVariableAttri.Style := [fsBold];
  fVariableAttri.Foreground:=clpurple;
  AddAttribute(fVariableAttri);
  fPreprocessorAttri := TSynHighlighterAttributes.Create(SYNS_AttrPreprocessor, SYNS_XML_AttrPreprocessor);
  AddAttribute(fPreprocessorAttri);
  SetAttributesOnChange({$IFDEF FPC}@{$ENDIF}DefHighlightChange);

  fStringDelimCh := '''';
  fIdentChars := inherited GetIdentChars;
  MakeMethodTables;
  fRange := rsUnknown;
end; { Create }

destructor TSynAnySyn.Destroy;
begin
  fUserData.free;
  fKeyWords.Free;
  FConstants.Free;
  FObjects.Free;
  inherited Destroy;
end; { Destroy }

procedure TSynAnySyn.SetLine(const NewValue: String; LineNumber:Integer);
begin
  inherited;
  fLine := PChar(NewValue);
  Run := 0;
  fLineNumber := LineNumber;
  Next;
end; { SetLine }

procedure TSynAnySyn.AnsiProc;
begin
  case fLine[Run] of
     #0: NullProc;
    #10: LFProc;
    #13: CRProc;
  else
    fTokenID := tkComment;
    repeat
      if (fLine[Run] = '*') and (fLine[Run + 1] = ')') then begin
        fRange := rsUnKnown;
        Inc(Run, 2);
        break;
      end;
      Inc(Run);
    until fLine[Run] in [#0, #10, #13];
  end;
end;

procedure TSynAnySyn.PasStyleProc;
begin
  case fLine[Run] of
     #0: NullProc;
    #10: LFProc;
    #13: CRProc;
  else
    fTokenID := tkComment;
    repeat
      if fLine[Run] = '}' then begin
        fRange := rsUnKnown;
        Inc(Run);
        break;
      end;
      Inc(Run);
    until fLine[Run] in [#0, #10, #13];
  end;
end;

procedure TSynAnySyn.CStyleProc;
begin
  case fLine[Run] of
     #0: NullProc;
    #10: LFProc;
    #13: CRProc;
  else
    fTokenID := tkComment;
    repeat
      if (fLine[Run] = '*') and (fLine[Run + 1] = '/') then begin
        fRange := rsUnKnown;
        Inc(Run, 2);
        break;
      end;
      Inc(Run);
    until fLine[Run] in [#0, #10, #13];
  end;
end;

procedure TSynAnySyn.AsciiCharProc;
begin
  if fDetectPreprocessor then begin
    fTokenID := tkPreprocessor;
    repeat
      inc(Run);
    until fLine[Run] in [#0, #10, #13];
  end else begin
    fTokenID := tkString;
    repeat
      inc(Run);
    until not (fLine[Run] in ['0'..'9']);
  end;
end;

procedure TSynAnySyn.BraceOpenProc;
begin
  if csPasStyle in fComments then
  begin
    fTokenID := tkComment;
    fRange := rsPasStyle;
    inc(Run);
    while FLine[Run] <> #0 do
      case FLine[Run] of
        '}':
          begin
            fRange := rsUnKnown;
            inc(Run);
            break;
          end;
        #10: break;

        #13: break;
      else inc(Run);
      end;
  end else
  begin
    inc(Run);
    fTokenID := tkSymbol;
  end;
end;

procedure TSynAnySyn.PointCommaProc;
begin
  if (csASmStyle in fComments) or (csBasStyle in fComments) then
  begin
    fTokenID := tkComment;
    fRange := rsUnknown;
    inc(Run);
    while FLine[Run] <> #0 do
      begin
        fTokenID := tkComment;
        inc(Run);
      end;
  end else
  begin
    inc(Run);
    fTokenID := tkSymbol;
  end;
end;

procedure TSynAnySyn.CRProc;
begin
  fTokenID := tkSpace;
  Inc(Run);
  if fLine[Run] = #10 then Inc(Run);
end;

procedure TSynAnySyn.IdentProc;
var
  aToken:string;
begin
  while Identifiers[fLine[Run]] do inc(Run);
  aToken:=GetToken;
  if IsKeyWord(aToken) then begin
    if not Markup then
      fTokenId:= tkKey
    else begin
      if fMarkupOn then
        fTokenId := tkKey
      else
        fTokenId:=tkIdentifier;
    end
  end
  else if IsConstant(aToken) then fTokenId:=tkConstant
  else if IsObject(aToken) then fTokenId:=tkObject
  else fTokenId := tkIdentifier;
end;

procedure TSynAnySyn.IntegerProc;
begin
  inc(Run);
  fTokenID := tkNumber;
  while FLine[Run] in ['0'..'9', 'A'..'F', 'a'..'f'] do inc(Run);
end;

procedure TSynAnySyn.LFProc;
begin
  fTokenID := tkSpace;
  inc(Run);
end;

procedure TSynAnySyn.NullProc;
begin
  fTokenID := tkNull;
end;

procedure TSynAnySyn.NumberProc;
begin
  inc(Run);
  fTokenID := tkNumber;
  while FLine[Run] in ['0'..'9', '.', 'e', 'E', 'x'] do
  begin
    case FLine[Run] of
      'x': begin // handle C style hex numbers
             IntegerProc;
             break;
           end;
      '.':
        if FLine[Run + 1] = '.' then break;
    end;
    inc(Run);
  end;
end;

procedure TSynAnySyn.RoundOpenProc;
begin
  inc(Run);
  if csAnsiStyle in fComments then
  begin
    case fLine[Run] of
      '*':
        begin
          fTokenID := tkComment;
          fRange := rsAnsi;
          inc(Run);
          while fLine[Run] <> #0 do
            case fLine[Run] of
              '*':
                if fLine[Run + 1] = ')' then
                begin
                  fRange := rsUnKnown;
                  inc(Run, 2);
                  break;
                end else inc(Run);
              #10: break;
              #13: break;
            else inc(Run);
            end;
        end;
      '.':
        begin
          inc(Run);
          fTokenID := tkSymbol;
        end;
    else
      begin
        FTokenID := tkSymbol;
      end;
    end;
  end else fTokenId := tkSymbol;
end;

procedure TSynAnySyn.RoundCloseProc;
begin
  inc(Run);
  FTokenID := tkSymbol;
end;

procedure TSynAnySyn.SlashProc;
begin
  case FLine[Run + 1] of
    '/':
      begin
        inc(Run, 2);
        fTokenID := tkComment;
        while FLine[Run] <> #0 do
        begin
          case FLine[Run] of
            #10, #13: break;
          end;
          inc(Run);
        end;
      end;
    '*':
      begin
        if csCStyle in fComments then
        begin
          fTokenID := tkComment;
          fRange := rsCStyle;
          inc(Run);
          while fLine[Run] <> #0 do
            case fLine[Run] of
              '*':
                if fLine[Run + 1] = '/' then
                begin
                  fRange := rsUnKnown;
                  inc(Run, 2);
                  break;
                end else inc(Run);
              #10: break;
              #13: break;
            else inc(Run);
            end;
        end
        else
          begin
            inc(Run);
            fTokenId := tkSymbol;
          end;
      end;
  else
    begin
      inc(Run);
      if markup and fmarkupon then
        fTokenID:=tkKey
      else
        fTokenID := tkSymbol;
    end;
  end;
end;

procedure TSynAnySyn.SpaceProc;
begin
  inc(Run);
  fTokenID := tkSpace;
  while FLine[Run] in [#1..#9, #11, #12, #14..#32] do inc(Run);
end;

procedure TSynAnySyn.StringProc;
begin
  fTokenID := tkString;
  if (fLine[Run + 1] = fStringDelimCh) and (fLine[Run + 2] = fStringDelimCh) then
    Inc(Run, 2);
  repeat
    case FLine[Run] of
      #0, #10, #13: break;
    end;
    inc(Run);
  until FLine[Run] = fStringDelimCh;
  if FLine[Run] <> #0 then inc(Run);
end;

procedure TSynAnySyn.UnknownProc;
begin
  inc(Run);
  {$IFDEF SYN_LAZARUS}
  while (fLine[Run] in [#128..#191]) OR // continued utf8 subcode
   ((fLine[Run]<>#0) and (fProcTable[fLine[Run]] = @UnknownProc)) do inc(Run);
  {$ENDIF}
  fTokenID := tkUnKnown;
end;

procedure TSynAnySyn.Next;
begin
  fTokenPos := Run;
  case fRange of
    rsAnsi: AnsiProc;
    rsPasStyle: PasStyleProc;
    rsCStyle: CStyleProc;
  else
    fProcTable[fLine[Run]];
  end;
end;

function TSynAnySyn.GetDefaultAttribute(Index: integer): TSynHighlighterAttributes;
begin
  case Index of
    SYN_ATTR_COMMENT: Result := fCommentAttri;
    SYN_ATTR_IDENTIFIER: Result := fIdentifierAttri;
    SYN_ATTR_KEYWORD: Result := fKeyAttri;
    SYN_ATTR_STRING: Result := fStringAttri;
    SYN_ATTR_WHITESPACE: Result := fSpaceAttri;
  else
    Result := nil;
  end;
end;

function TSynAnySyn.GetEol: Boolean;
begin
  Result := fTokenId = tkNull;
end;

function TSynAnySyn.GetRange: Pointer;
begin
  Result := Pointer(PtrUInt(fRange));
end;

function TSynAnySyn.GetToken: String;
var
  Len: LongInt;
begin
  Len := Run - fTokenPos;
  Result:='';
  SetString(Result, (FLine + fTokenPos), Len);
end;

{$IFDEF SYN_LAZARUS}
procedure TSynAnySyn.GetTokenEx(out TokenStart: PChar;
  out TokenLength: integer);
begin
  TokenLength:=Run-fTokenPos;
  TokenStart:=FLine + fTokenPos;
end;
{$ENDIF}


function TSynAnySyn.GetTokenID: TtkTokenKind;
begin
  Result := fTokenId;
end;

function TSynAnySyn.GetTokenAttribute: TSynHighlighterAttributes;
begin
  case fTokenID of
    tkComment: Result := fCommentAttri;
    tkIdentifier: Result := fIdentifierAttri;
    tkEntity: result:= fEntityAttri;
    tkKey: Result := fKeyAttri;
    tkConstant: Result := fConstantAttri;
    tkObject: Result := fObjectAttri;
    tkNumber: Result := fNumberAttri;
    tkPreprocessor: Result := fPreprocessorAttri;
    tkSpace: Result := fSpaceAttri;
    tkString: Result := fStringAttri;
    tkSymbol: Result := fSymbolAttri;
    tkDollarVariable: Result := fVariableAttri;
    tkDot: result:=fDotAttri;
    tkUnknown: Result := fSymbolAttri;
  else
    Result := nil;
  end;
end;

function TSynAnySyn.GetTokenKind: integer;
begin
  Result := Ord(fTokenId);
end;

function TSynAnySyn.GetTokenPos: Integer;
begin
  Result := fTokenPos;
end;

procedure TSynAnySyn.ReSetRange;
begin
  fRange := rsUnknown;
end;

procedure TSynAnySyn.SetRange(Value: Pointer);
begin
  fRange := TRangeState(PtrUInt(Value));
end;

procedure TSynAnySyn.SetKeyWords(const Value: TStrings);
var
  i: Integer;
begin
  if Value <> nil then
    begin
      Value.BeginUpdate;
      for i := 0 to Value.Count - 1 do
        Value[i] := UpperCase(Value[i]);
      Value.EndUpdate;
    end;
  fKeyWords.Assign(Value);
  DefHighLightChange(nil);
end;

procedure TSynAnySyn.SetComments(Value: CommentStyles);
begin
  if fComments = Value then exit;
  fComments := Value;
  MakeMethodTables;
  DefHighLightChange(nil);
end;

class function TSynAnySyn.GetLanguageName: string;
begin
  Result := SYNS_LangGeneral;
end;

function TSynAnySyn.LoadFromRegistry(RootKey: HKEY; Key: string): boolean;
var
  r: TBetterRegistry;
begin
  r:= TBetterRegistry.Create;
  try
    r.RootKey := RootKey;
    if r.OpenKeyReadOnly(Key) then begin
      if r.ValueExists('KeyWords') then KeyWords.Text:= r.ReadString('KeyWords');
      Result := inherited LoadFromRegistry(RootKey, Key);
    end
    else Result := false;
  finally r.Free; end;
end;

function TSynAnySyn.SaveToRegistry(RootKey: HKEY; Key: string): boolean;
var
  r: TBetterRegistry;
begin
  r:= TBetterRegistry.Create;
  try
    r.RootKey := RootKey;
    if r.OpenKey(Key,true) then begin
      Result := true;
      r.WriteString('KeyWords', KeyWords.Text);
      Result := inherited SaveToRegistry(RootKey, Key);
    end
    else Result := false;
  finally r.Free; end;
end;

function TSynAnySyn.GetStringDelim: TStringDelim;
begin
  if fStringDelimCh = '''' then
    Result := sdSingleQuote
  else
    Result := sdDoubleQuote;
end;

procedure TSynAnySyn.SetStringDelim(const Value: TStringDelim);
var
  newCh: char;
begin
  case Value of
    sdSingleQuote: newCh := '''';
    else newCh := '"';
  end; //case
  if newCh <> fStringDelimCh then begin
    fStringDelimCh := newCh;
    MakeMethodTables;
  end;
end;

function TSynAnySyn.GetIdentifierChars: string;
var
  ch: char;
  s: shortstring;
begin
  s := '';
  for ch := #0 to #255 do
    if ch in fIdentChars then s := s + ch;
  Result := s;
end;

procedure TSynAnySyn.SetIdentifierChars(const Value: string);
var
  i: integer;
begin
  fIdentChars := [];
  for i := 1 to Length(Value) do begin
    fIdentChars := fIdentChars + [Value[i]];
  end; //for
end;

function TSynAnySyn.GetIdentChars: TSynIdentChars;
begin
  Result := fIdentChars;
end;

procedure TSynAnySyn.SetDetectPreprocessor(Value: boolean);
begin
  if Value <> fDetectPreprocessor then begin
    fDetectPreprocessor := Value;
    DefHighlightChange(Self);
  end;
end;


procedure TSynAnySyn.ApostropheProc;
begin
  fTokenID := tkComment;
  repeat
    inc(Run);
  until fLine[Run] in [#0, #10, #13];
end;

procedure TSynAnySyn.SetConstants(const Value: TStrings);
var
  i: Integer;
begin
  if Value <> nil then
    begin
      Value.BeginUpdate;
      for i := 0 to Value.Count - 1 do
        Value[i] := UpperCase(Value[i]);
      Value.EndUpdate;
    end;
  fConstants.Assign(Value);
  DefHighLightChange(nil);
end;

procedure TSynAnySyn.SetObjects(const Value: TStrings);
var
  i: Integer;
begin
  if Value <> nil then
    begin
      Value.BeginUpdate;
      for i := 0 to Value.Count - 1 do
        Value[i] := UpperCase(Value[i]);
      Value.EndUpdate;
    end;
  fObjects.Assign(Value);
  DefHighLightChange(nil);
end;

procedure TSynAnySyn.SetMarkup(const Value: boolean);
begin
  if value<>FMarkup then begin
    FMarkup := Value;
    DefHighLightChange(nil);
  end;
end;

procedure TSynAnySyn.GreaterThan;
begin
  inc(Run);
  if markup then begin
    fMarkupOn:=false;
    fTokenId:=tkKey;
  end
  else
   fTokenID := tkUnKnown;
end;

procedure TSynAnySyn.SmallerThan;
begin
  inc(Run);
  if markup then begin
    fMarkupOn:=true;
    fTokenId:=tkKey;
  end
  else
    fTokenID := tkUnKnown;
end;


procedure TSynAnySyn.LoadHighLighter(aFile: string);
var
  hini:TIniList;
  s:string;
  b:boolean;
  HL:TSynAnySyn;
  i:integer;
  genlist:TStringlist;

  function StyleToStr(aStyle:TFontStyles):string;
  begin
    Result:='';
    if (fsbold in aStyle) then
      result:=result+'bold,';
    if (fsitalic in aStyle) then
      result:=result+'italic,';
    if (fsunderline in aStyle) then
      result:=result+'underline';
  end;

  procedure ReadAttribute(attr:TSynHighlighterAttributes;attrname:string);
  begin
    s:='';
    if (fsbold in attr.Style) then
      s:='bold,';
    if (fsitalic in attr.Style) then
      s:=s+'italic,';
    if (fsunderline in attr.Style) then
      s:=s+'underline';
    s:=hini.ReadString(AttrName,'Style',s);
    if (s='normal') or (s='') then
      attr.style:=[]
    else begin
      if pos('bold',s)>0 then attr.style:=attr.style+[fsbold] else attr.style:=attr.style-[fsbold];
      if pos('italic',s)>0 then attr.style:=attr.style+[fsitalic] else attr.style:=attr.style-[fsitalic];
      if pos('underline',s)>0 then attr.style:=attr.style+[fsunderline] else attr.style:=attr.style-[fsunderline];
    end;
    s:=colortostring(Attr.Background);
    s:=hini.ReadString(AttrName,'Background',s);
    Attr.Background:=stringtocolor(s);
    s:=colortostring(Attr.Foreground);
    s:=hini.ReadString(AttrName,'Foreground',s);
    Attr.Foreground:=stringtocolor(s);
  end;

begin
  if FileExistsUTF8(aFile) then
  begin
    HL:=self;
    fUserData.LoadFromFile(UTF8ToSys(aFile));
    hini:=fUserData;
    // attributes
    ReadAttribute(HL.Commentattri,'Comment');
    ReadAttribute(HL.Identifierattri,'Identifier');
    ReadAttribute(HL.Keyattri,'Key');
    ReadAttribute(HL.Constantattri,'Constant');
    ReadAttribute(HL.Objectattri,'Object');
    ReadAttribute(HL.Numberattri,'Number');
    ReadAttribute(HL.Spaceattri,'Space');
    ReadAttribute(HL.Stringattri,'String');
    ReadAttribute(HL.Symbolattri,'Symbol');
    ReadAttribute(HL.Entityattri,'Entity');
    ReadAttribute(HL.Variableattri,'Variables');
    ReadAttribute(HL.DotAttri,'Dot');
    // comment style
    HL.Comments:=[];
    if hini.ReadBool('CommentStyle','ansi',false) then
      HL.comments:=HL.Comments+[csAnsiStyle];
    if hini.ReadBool('CommentStyle','pas',false) then
      HL.comments:=HL.Comments+[csPasStyle];
    if hini.ReadBool('CommentStyle','c',false) then
      HL.comments:=HL.Comments+[csCStyle];
    if hini.ReadBool('CommentStyle','asm',false) then
      HL.comments:=HL.Comments+[csAsmStyle];
    if hini.ReadBool('CommentStyle','bas',false) then
      HL.comments:=HL.Comments+[csBasStyle];
    if hini.ReadBool('CommentStyle','vb',false) then
      HL.comments:=HL.Comments+[csVBStyle];
    // markup switch for html, xml, xsl etc.
    HL.markup:= hini.ReadBool('Switches','markup',false);
    // entity switch for html, xml, xsl etc.
    HL.entity:= hini.ReadBool('Switches','entity',false);
    // $variable switch for languages like perl and php
    HL.dollarvariables:= hini.ReadBool('Switches','dollarvariables',false);
    // .dot switch for object methods and properties
    HL.ActiveDot:= hini.ReadBool('Switches','activedot',false);
    // string delimiter
    b:=hini.ReadBool('String Delimiter','Double Quotes',false);
    if b then
      HL.StringDelim:=sdDoubleQuote
    else
      HL.StringDelim:=sdSingleQuote;
    genlist:=TStringList.create;
    // read keywords
    hini.ReadSectionNames('Keywords',genlist);
    if genlist.count>0 then
      for i:=0 to genlist.count-1 do
        genlist[i]:=uppercase(genlist[i]);
    HL.KeyWords.assign(genlist);
    hini.ReadSectionNames('Constants',genlist);
    if genlist.count>0 then
      for i:=0 to genlist.count-1 do
        genlist[i]:=uppercase(genlist[i]);
    HL.Constants.assign(genlist);
    hini.ReadSectionNames('Objects',genlist);
    if genlist.count>0 then
      for i:=0 to genlist.count-1 do
        genlist[i]:=uppercase(genlist[i]);
    HL.Objects.assign(genlist);
    genlist.free;
    makemethodtables;
  end;
end;



{ TIniList }

function TIniList.GetKeyIndex(asection, akey: string): integer;
var
  i,c:integer;
begin
  result:=-1;
  i:=self.IndexOf('['+asection+']');
  if i=-1 then exit;
  c:=self.Count;
  inc(i);
  while (i<c) and (pos('=',strings[i])<>0) do begin
    if comparetext(names[i],akey)=0 then begin
      result:=i;
      exit;
    end;
    inc(i);
  end;
end;

function TIniList.GetKeyValue(asection, akey: string): string;
var
  i,p:integer;
begin
  result:='';
  i:=getkeyindex(asection,akey);
  if i=-1 then exit;
  p:=pos('=',strings[i]);
  result:=copy(strings[i],p+1,maxint);
end;

function TIniList.ReadBool(asection, akey: string;
  adefault: boolean): boolean;
var
  keyvalue:string;
begin
  result:=adefault;
  keyvalue:=GetKeyValue(asection,akey);
  result:=comparetext('true',keyvalue)=0;
end;

function TIniList.ReadInteger(asection, akey: string;
  adefault: integer): integer;
var
  keyvalue:string;
begin
  result:=adefault;
  keyvalue:=GetKeyValue(asection,akey);
  try
    result:=strtoint(keyvalue);
  except
  end;
end;

procedure TIniList.ReadSectionNames(asection: string; alist: TStrings);
var
  i,c,p:integer;
  s:string;
begin
  alist.Clear;
  i:=self.IndexOf('['+asection+']');
  if i=-1 then exit;
  inc(i);
  c:=count;
  while (i<c) and (pos('[',strings[i])=0) and (strings[i]<>'') do begin
    s:=strings[i];
    p:=pos('=',s);
    if p=0 then
      alist.Append(s)
    else
      alist.append(copy(s,1,p-1));
    inc(i);
  end;
end;

function TIniList.ReadString(asection, akey, adefault: string): string;
var
  keyvalue:string;
begin
  result:=adefault;
  keyvalue:=GetKeyValue(asection,akey);
  if keyvalue<>'' then result:=keyvalue;
end;

procedure TSynAnySyn.SetEntity(const Value: boolean);
begin
  if value<>FEntity then begin
    FEntity := Value;
    DefHighLightChange(nil);
  end;
end;

procedure TSynAnySyn.AmpersandProc;
  function testentity:boolean;
  var i:integer;
  begin
    result:=false;
    i:=run;
    inc(i);
    while FLine[i] <> #0 do
      case FLine[i] of
        ';':
          begin
            fRange := rsUnKnown;
            inc(i);
            result:=true;
            break;
          end;
        #10: break;
        ' ': break;
        #13: break;
      else inc(i);
      end;
    if result then run:=i;
  end;
begin
  if Entity then
  begin
    if testentity then
      fTokenID := tkEntity
    else begin
      inc(Run);
      fTokenID := tkSymbol;
    end;
  end else
  begin
    inc(Run);
    fTokenID := tkSymbol;
  end;
end;

procedure TSynAnySyn.SetDollarVariables(const Value: boolean);
begin
  if Value<>FDollarVariables then begin
    FDollarVariables := Value;
    MakeMethodTables;
    DefHighLightChange(nil);
  end;
end;

procedure TSynAnySyn.DollarProc;
begin
  inc(Run);
  fTokenID := tkDollarVariable;
  while FLine[Run] in ['0'..'9', 'A'..'Z', 'a'..'z','_'] do inc(Run);
end;

procedure TSynAnySyn.DotProc;
  function testdot:boolean;
  var i:integer;
  begin
    result:=false;
    i:=run;
    inc(i);
    while (FLine[i] in ['a'..'z','A'..'Z']) do
      inc(i);
    if i>(run+1) then result:=true;
    if result then run:=i;
  end;
begin
  if not FActiveDot then
  begin
    inc(Run);
    fTokenID := tkSymbol;
  end
  else if testDot then
     fTokenID := tkDot
  else begin
      inc(Run);
      fTokenID := tkSymbol;
  end;
end;

procedure TSynAnySyn.SetActiveDot(const Value: boolean);
begin
  FActiveDot := Value;
end;

procedure TSynAnySyn.SetDotAttri(const Value: TSynHighlighterAttributes);
begin
  FDotAttri := Value;
end;

initialization
  MakeIdentTable;
{$IFNDEF FPC}
  RegisterPlaceableHighlighter(TSynAnySyn);
{$ENDIF}
end.

