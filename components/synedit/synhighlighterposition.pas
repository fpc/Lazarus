{-------------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/
Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
the specific language governing rights and limitations under the License.


$Id$

This is a basic synedit highlighter that does not parse text to create the
attributes, but stores a list of ranges.

-------------------------------------------------------------------------------}
unit SynHighlighterPosition;

{$I synedit.inc}

interface

uses
  Classes, SysUtils, Graphics, SynEditStrConst, SynEditTypes,
  SynEditHighlighter;

const
  tkNone   = 0;
  tkText   = 1;
  tkCustom = 2;
  
  MaxColumns = 100000;

type
  TtkTokenKind = integer;

  TPositionToken = record
    Kind: TtkTokenKind;
    Column: integer;
  end;
  PPositionToken = ^TPositionToken;

  TPositionTokens = record
    Count: integer;
    Tokens: array[0..MaxColumns] of TPositionToken;
  end;
  PPositionTokens = ^TPositionTokens;

  { TSynPositionHighlighter }

  TSynPositionHighlighter = class(TSynCustomHighlighter)
  private
    fCopiedAttributes: TList;
    fLine: string;
    fLineLen: integer;
    fLineNumber: Integer;
    fTokenEnd: LongInt; // end of current token
    fTextAttri: TSynHighlighterAttributes;
    fTokenPos: Integer;
    fTokenArrayPos: integer;
    FTokenKind: TtkTokenKind;
    fTokens: TList; // list of PPositionTokens
    function GetTokens(TheLineNumber: integer): PPositionTokens;
  protected
    function GetIdentChars: TSynIdentChars; override;
    function IsFilterStored: boolean; override;                                 //mh 2000-10-08
    function GetPositionTokensSize(ItemCount: integer): integer;
    function GetDefaultAttribute(Index: integer): TSynHighlighterAttributes;
      override;
  public
    class function GetLanguageName: string; override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    function GetEol: Boolean; override;
    function GetRange: Pointer; override;
    function GetToken: string; override;
    {$IFDEF SYN_LAZARUS}
    procedure GetTokenEx(out TokenStart: PChar; out TokenLength: integer); override;
    {$ENDIF}
    function GetTokenAttribute: TSynHighlighterAttributes; override;
    function GetTokenKind: integer; override;
    function GetTokenPos: Integer; override;
    procedure Next; override;
    procedure ResetRange; override;
    procedure SetLine({$IFDEF FPC}const {$ENDIF}NewValue: string;
      LineNumber:Integer); override;
    procedure SetRange(Value: Pointer); override;
    function UseUserSettings(settingIndex: integer): boolean; override;
    procedure EnumUserSettings(settings: TStrings); override;
    property IdentChars;
  public
    procedure AddToken(Line, // 0 based
                       Col: integer; // 1 based
                       TokenKind: TtkTokenKind);
    procedure ClearTokens(Line: integer); // 0 based
    procedure ClearAllCopiedAttributes;
    procedure ClearAllTokens;
    procedure InsertTokens(Lines: TStringList;
      Highlighter: TSynCustomHighlighter;
      Line, //0 based
      Col: integer // 1 based
      );
    function CreateTokenID(const aName: string; Foreground, BackGround: TColor;
                           Style: TFontStyles): TtkTokenKind;
    function GetCopiedTokenID(Attr: TSynHighlighterAttributes): TtkTokenKind;
    function GetCopiedAttribute(TokenID: TtkTokenKind): TSynHighlighterAttributes;
    property Tokens[TheLineNumber: integer]: PPositionTokens read GetTokens;
  published
    property TextAttri: TSynHighlighterAttributes read fTextAttri write fTextAttri;
  end;

implementation

{ TSynPositionHighlighter }

function TSynPositionHighlighter.GetTokens(TheLineNumber: integer
  ): PPositionTokens;
begin
  if (TheLineNumber>=0) and (TheLineNumber<fTokens.Count) then
    Result:=PPositionTokens(fTokens[TheLineNumber])
  else
    Result:=nil;
end;

function TSynPositionHighlighter.GetIdentChars: TSynIdentChars;
begin
  Result := ['_', '0'..'9', 'a'..'z', 'A'..'Z'];
end;

function TSynPositionHighlighter.IsFilterStored: boolean;
begin
  Result:=true;
end;

function TSynPositionHighlighter.GetPositionTokensSize(ItemCount: integer
  ): integer;
begin
  Result:=SizeOf(integer)+SizeOf(TPositionToken)*ItemCount;
end;

function TSynPositionHighlighter.GetDefaultAttribute(Index: integer
  ): TSynHighlighterAttributes;
begin
  Result:=nil;
end;

class function TSynPositionHighlighter.GetLanguageName: string;
begin
  Result:='Position based highlighter';
end;

constructor TSynPositionHighlighter.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  fTokens:=TList.Create;
  fCopiedAttributes:=TList.Create;
  fTextAttri := TSynHighlighterAttributes.Create(SYNS_AttrText, SYNS_XML_AttrText);
  AddAttribute(fTextAttri);
  SetAttributesOnChange({$IFDEF FPC}@{$ENDIF}DefHighlightChange);

  fDefaultFilter := '';
end;

destructor TSynPositionHighlighter.Destroy;
begin
  ClearAllTokens;
  fTokens.Free;
  ClearAllCopiedAttributes;
  fCopiedAttributes.Free;
  inherited Destroy;
end;

function TSynPositionHighlighter.GetEol: Boolean;
begin
  Result := fTokenKind = tkNone;
end;

function TSynPositionHighlighter.GetRange: Pointer;
begin
  Result := Pointer(PtrInt(fLineNumber));
end;

function TSynPositionHighlighter.GetToken: string;
var
  Len: LongInt;
begin
  Len := fTokenEnd - fTokenPos;
  SetLength(Result,Len);
  System.Move(fLine[fTokenPos],Result[1],Len);
end;

{$IFDEF SYN_LAZARUS}
procedure TSynPositionHighlighter.GetTokenEx(out TokenStart: PChar;
  out TokenLength: integer);
begin
  TokenLength:=fTokenEnd-fTokenPos;
  if TokenLength>0 then begin
    TokenStart:=@fLine[fTokenPos];
  end else begin
    TokenStart:=nil;
  end;
end;
{$ENDIF}

function TSynPositionHighlighter.GetTokenAttribute: TSynHighlighterAttributes;
var
  t: TtkTokenKind;
begin
  t:=GetTokenKind;
  if t=tkText then
    Result:=fTextAttri
  else if (t<0) then
    // this is a copied attribute
    Result:=GetCopiedAttribute(t)
  else
    Result:=nil;
end;

function TSynPositionHighlighter.GetTokenKind: integer;
begin
  Result:=fTokenKind;
end;

function TSynPositionHighlighter.GetTokenPos: Integer;
begin
  Result:=fTokenPos-1;
end;

procedure TSynPositionHighlighter.Next;
var
  p: PPositionTokens;
begin
  fTokenPos := fTokenEnd;
  if fTokenEnd>fLineLen then begin
    fTokenKind := tkNone;
    exit;
  end;
  inc(fTokenArrayPos);
  p:=Tokens[fLineNumber];
  if (p<>nil) and (p^.Count>fTokenArrayPos) then begin
    fTokenKind := p^.Tokens[fTokenArrayPos].Kind;
    fTokenEnd := p^.Tokens[fTokenArrayPos].Column+1;
    if fTokenEnd>fLineLen+1 then
      fTokenEnd := fLineLen+1;
    if fTokenEnd=fTokenPos then
      Next;
  end else begin
    fTokenEnd := fLineLen+1;
    fTokenKind := tkText;
  end;
end;

procedure TSynPositionHighlighter.ResetRange;
begin
  inherited ResetRange;
end;

procedure TSynPositionHighlighter.SetLine(const NewValue: string;
  LineNumber: Integer);
var
  p: PPositionTokens;
begin
  inherited;
  fLine := NewValue;
  fLineLen := length(fLine);
  fLineNumber := LineNumber;
  fTokenArrayPos := 0;
  fTokenPos := 1;
  p:=Tokens[fLineNumber];
  if p<>nil then begin
    fTokenEnd := p^.Tokens[0].Column+1;
    if fTokenEnd>fLineLen+1 then
      fTokenEnd:=fLineLen+1;
    FTokenKind := p^.Tokens[0].Kind;
    if fTokenEnd=fTokenPos then Next;
  end else begin
    fTokenEnd := fLineLen+1;
    FTokenKind := tkText;
  end;
end;

procedure TSynPositionHighlighter.SetRange(Value: Pointer);
begin
  inherited SetRange(Value);
end;

function TSynPositionHighlighter.UseUserSettings(settingIndex: integer
  ): boolean;
begin
  Result:=inherited UseUserSettings(settingIndex);
end;

procedure TSynPositionHighlighter.EnumUserSettings(settings: TStrings);
begin
  inherited EnumUserSettings(settings);
end;

procedure TSynPositionHighlighter.AddToken(Line, Col: integer;
  TokenKind: TtkTokenKind);
var
  p: PPositionTokens;
  TokenIndex, TokenCount: integer;
begin
  if Col<1 then exit;
  // fill up lines
  while (Line>=fTokens.Count) do fTokens.Add(nil);
  // resize Token array
  p:=Tokens[Line];
  TokenIndex:=0;
  if p<>nil then
    TokenCount:=p^.Count+1
  else
    TokenCount:=1;
  ReAllocMem(p,GetPositionTokensSize(TokenCount));
  fTokens[Line]:=p;
  p^.Count:=TokenCount;
  // insert Token
  TokenIndex:=TokenCount-1;
  while (TokenIndex>0)
  and (p^.Tokens[TokenIndex-1].Column>Col) do
    dec(TokenIndex);
  if TokenIndex<TokenCount-1 then begin
    System.Move(p^.Tokens[TokenIndex],p^.Tokens[TokenIndex+1],
      SizeOf(TPositionToken)*(TokenCount-TokenIndex-1));
  end;
  with p^.Tokens[TokenIndex] do begin
    Kind:=TokenKind;
    Column:=Col;
  end;
  DefHighlightChange(Self);
end;

procedure TSynPositionHighlighter.ClearTokens(Line: integer);
var
  p: pointer;
begin
  if (Line>=0) and (Line<fTokens.Count) then begin
    p:=fTokens[Line];
    if p<>nil then begin
      FreeMem(p);
      fTokens[Line]:=nil;
    end;
  end;
end;

procedure TSynPositionHighlighter.ClearAllCopiedAttributes;
var
  i: Integer;
begin
  for i:=0 to fCopiedAttributes.Count-1 do
    TObject(fCopiedAttributes[i]).Free;
  fCopiedAttributes.Clear;
end;

procedure TSynPositionHighlighter.ClearAllTokens;
var
  i: Integer;
begin
  for i:=0 to fTokens.Count-1 do
    ClearTokens(i);
  fTokens.Clear;
end;

procedure TSynPositionHighlighter.InsertTokens(Lines: TStringList;
  Highlighter: TSynCustomHighlighter; Line, Col: integer);
var
  RelLine: integer;
  nTokenLen: integer;
  sToken: PChar;
  Attr: TSynHighlighterAttributes;
  TokenID: integer;
begin
  if (Lines=nil) or (Lines.Count=0) or (Highlighter=nil) then exit;
  RelLine:=0;
  while RelLine<Lines.Count do begin
    HighLighter.SetLine(Lines[RelLine], RelLine);
    while not Highlighter.GetEol do begin
      Attr:=Highlighter.GetTokenAttribute;
      TokenID:=GetCopiedTokenID(Attr);
      Highlighter.GetTokenEx(sToken,nTokenLen);
      inc(Col,nTokenLen);
      AddToken(Line,Col-1,TokenID);
      // next Token
      Highlighter.Next;
    end;
    // next line
    inc(Line);
    inc(RelLine);
    Col:=1;
  end;
end;

function TSynPositionHighlighter.CreateTokenID(const aName: string;
  Foreground, BackGround: TColor;
  Style: TFontStyles): TtkTokenKind;
var
  Attr: TSynHighlighterAttributes;
begin
  Attr:=TSynHighlighterAttributes.Create(aName);
  Attr.Foreground:=Foreground;
  Attr.Background:=BackGround;
  Attr.Style:=Style;
  Result:=GetCopiedTokenID(Attr);
  Attr.Free;
end;

function TSynPositionHighlighter.GetCopiedTokenID(
  Attr: TSynHighlighterAttributes): TtkTokenKind;
var
  i: Integer;
  CurAttr: TSynHighlighterAttributes;
begin
  i:=fCopiedAttributes.Count-1;
  while i>=0 do begin
    CurAttr:=TSynHighlighterAttributes(fCopiedAttributes[i]);
    if (Attr.ForeGround=CurAttr.ForeGround)
    and (Attr.BackGround=CurAttr.BackGround)
    and (Attr.Style=CurAttr.Style) then begin
      // attribute already exists
      Result:=(-i-1);
      exit;
    end;
    dec(i);
  end;
  // create new attribute
  CurAttr:=TSynHighlighterAttributes.Create('');
  CurAttr.Assign(Attr);
  fCopiedAttributes.Add(CurAttr);
  Result:= -fCopiedAttributes.Count;
end;

function TSynPositionHighlighter.GetCopiedAttribute(TokenID: TtkTokenKind
  ): TSynHighlighterAttributes;
begin
  if (TokenID<0) and (fCopiedAttributes.Count>=-TokenID) then
    Result:=TSynHighlighterAttributes(fCopiedAttributes[-TokenID-1])
  else
    Result:=nil;
end;

end.

