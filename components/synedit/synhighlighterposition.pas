{-------------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/
Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
the specific language governing rights and limitations under the License.


$Id$

This is a basic synedit highlighter that does not pasre text to create the
attributes, but stores a list of ranges.

-------------------------------------------------------------------------------}
unit SynHighlighterPosition;

{$I synedit.inc}

interface

uses
  Classes, SysUtils, SynEditStrConst, SynEditTypes, SynEditHighlighter;

const
  tkNone = 0;
  tkText = 1;
  
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

  TSynPositionHighlighter = class(TSynCustomHighlighter)
  private
    fLine: string;
    fLineLen: integer;
    fLineNumber: Integer;
    Run: LongInt; // end of current token
    fTextAttri: TSynHighlighterAttributes;
    fTokenPos: Integer;
    FTokenID: integer;
    FTokenKind: TtkTokenKind;
    fTokens: TList; // list of PPositionTokens
    function GetTokens(TheLineNumber: integer): PPositionTokens;
  protected
    function GetIdentChars: TSynIdentChars; override;
    function IsFilterStored: boolean; override;                                 //mh 2000-10-08
    function GetPositionTokensSize(ItemCount: integer): integer;
  public
    {$IFNDEF SYN_CPPB_1} class {$ENDIF}
    function GetLanguageName: string; override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    function GetEol: Boolean; override;
    function GetRange: Pointer; override;
    function GetToken: string; override;
    {$IFDEF SYN_LAZARUS}
    procedure GetTokenEx(var TokenStart: PChar; var TokenLength: integer); override;
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
    procedure AddToken(Line, Col: integer; TokenKind: TtkTokenKind);
    procedure ClearTokens(Line: integer);
    procedure ClearAllTokens;
    property Tokens[TheLineNumber: integer]: PPositionTokens read GetTokens;
  published
    property TextAttri: TSynHighlighterAttributes read fTextAttri write fTextAttri;
  end;

implementation

{ TSynPositionHighlighter }

function TSynPositionHighlighter.GetTokens(TheLineNumber: integer
  ): PPositionTokens;
begin
  if (TheLineNumber>=1) and (TheLineNumber<=fTokens.Count) then
    Result:=PPositionTokens(fTokens[TheLineNumber-1])
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

function TSynPositionHighlighter.GetLanguageName: string;
begin
  Result:='Position based highlighter';
end;

constructor TSynPositionHighlighter.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  fTokens:=TList.Create;
  fTextAttri := TSynHighlighterAttributes.Create(SYNS_AttrText);
  AddAttribute(fTextAttri);
  SetAttributesOnChange({$IFDEF FPC}@{$ENDIF}DefHighlightChange);

  fDefaultFilter := '';
end;

destructor TSynPositionHighlighter.Destroy;
begin
  ClearAllTokens;
  fTokens.Free;
  inherited Destroy;
end;

function TSynPositionHighlighter.GetEol: Boolean;
begin
  Result := fTokenKind = tkNone;
end;

function TSynPositionHighlighter.GetRange: Pointer;
begin
  Result := Pointer(fLineNumber);
end;

function TSynPositionHighlighter.GetToken: string;
var
  Len: LongInt;
begin
  Len := Run - fTokenPos;
  SetLength(Result,Len);
  System.Move(fLine[fTokenPos],Result[1],Len);
end;

{$IFDEF SYN_LAZARUS}
procedure TSynPositionHighlighter.GetTokenEx(var TokenStart: PChar;
  var TokenLength: integer);
begin
  TokenLength:=Run-fTokenPos;
  if TokenLength>0 then begin
    TokenStart:=@fLine[fTokenPos];
  end else begin
    TokenStart:=nil;
  end;
end;
{$ENDIF}

function TSynPositionHighlighter.GetTokenAttribute: TSynHighlighterAttributes;
begin
  if GetTokenKind=tkText then
    Result:=fTextAttri
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
  fTokenPos := Run;
  if Run>fLineLen then begin
    fTokenKind := tkNone;
    exit;
  end;
  inc(fTokenID);
  p:=Tokens[fLineNumber];
  if (p<>nil) and (p^.Count>fTokenID) then begin
    Run := p^.Tokens[fTokenID].Column;
    if Run>fLineLen+1 then
      Run := fLineLen+1;
    if Run>fTokenPos then
      fTokenKind := p^.Tokens[fTokenID].Kind
    else
      fTokenKind := tkText;
  end else begin
    Run := fLineLen+1;
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
  fLine := NewValue;
  fLineLen := length(fLine);
  fLineNumber := LineNumber;
  FTokenID := 0;
  fTokenPos := 1;
  p:=Tokens[fLineNumber];
  if p<>nil then begin
    Run := p^.Tokens[0].Column;
    FTokenKind := p^.Tokens[0].Kind;
  end else begin
    Run := fLineLen+1;
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
  // fill up lines
  while (Line>fTokens.Count) do fTokens.Add(nil);
  // resize Token array
  p:=Tokens[Line];
  TokenIndex:=0;
  if p<>nil then
    TokenCount:=p^.Count+1
  else
    TokenCount:=1;
  ReAllocMem(p,GetPositionTokensSize(TokenCount));
  fTokens[Line-1]:=p;
  p^.Count:=TokenCount;
  // insert Token
  TokenIndex:=TokenCount-1;
  while (TokenIndex>0)
  and (p^.Tokens[TokenIndex].Column>Col) do
    dec(TokenIndex);
  if TokenIndex<TokenCount-1 then begin
    System.Move(p^.Tokens[TokenIndex],p^.Tokens[TokenIndex+1],
      SizeOf(TPositionToken)*(TokenCount-TokenIndex-1));
  end;
  with p^.Tokens[TokenIndex] do begin
    Kind:=TokenKind;
    Column:=Col;
  end;
end;

procedure TSynPositionHighlighter.ClearTokens(Line: integer);
var
  p: pointer;
begin
  if (Line>=1) and (Line<=fTokens.Count) then begin
    p:=fTokens[Line-1];
    FreeMem(p);
    fTokens[Line-1]:=nil;
  end;
end;

procedure TSynPositionHighlighter.ClearAllTokens;
var
  i: Integer;
begin
  for i:=0 to fTokens.Count-1 do
    ClearTokens(i);
  fTokens.Clear;
end;

end.

