unit PreProcessorExpressionTokenise;

{(*}
(*------------------------------------------------------------------------------
 Delphi Code formatter source code 

The Original Code is PreProcessorExpressionTokenise, released August 2003.
The Initial Developer of the Original Code is Anthony Steele. 
Portions created by Anthony Steele are Copyright (C) 2003 Anthony Steele.
All Rights Reserved. 
Contributor(s): Anthony Steele. 

The contents of this file are subject to the Mozilla Public License Version 1.1
(the "License"). you may not use this file except in compliance with the License.
You may obtain a copy of the License at http://www.mozilla.org/NPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied.
See the License for the specific language governing rights and limitations 
under the License.

Alternatively, the contents of this file may be used under the terms of
the GNU General Public License Version 2 or later (the "GPL") 
See http://www.gnu.org/licenses/gpl.html
------------------------------------------------------------------------------*)
{*)}

{$I JcfGlobal.inc}

interface

{
  AFS 26 Aug 2003

 lexer for preprocessor $IF expressions
 Turns text into a list of tokens
 The tokens are defined in PreProcessorTokens
 Whitespace is discarded
}
uses
  SysUtils,
  PreProcessorExpressionTokens;

type
  TPreProcessorExpressionTokeniser = class
  private
    fsExpr: String;
    fiCurrentIndex: integer;
    fbHasError: boolean;

    fcTokens: TPreProcessorExpressionTokenList;

    function Rest: string;
    function StartsWith(const ps: string): boolean;

    function TryConsumeFixedSymbol: boolean;
    function TryConsumeIdentifier: boolean;
    procedure ConsumeWhiteSpace;

  public
    constructor Create;
    destructor Destroy; override;

    procedure Tokenise;

    property Expression: String Read fsExpr Write fsExpr;
    property Tokens: TPreProcessorExpressionTokenList Read fcTokens;
    property HasError: boolean Read fbHasError;
  end;

implementation

uses
  { local }
  JcfStringUtils;


constructor TPreProcessorExpressionTokeniser.Create;
begin
  inherited;
  fcTokens := TPreProcessorExpressionTokenList.Create;
end;

destructor TPreProcessorExpressionTokeniser.Destroy;
begin
  FreeAndNil(fcTokens);
  inherited;
end;

function TPreProcessorExpressionTokeniser.Rest: string;
begin
  Result := string(StrRestOf(fsExpr, fiCurrentIndex));
end;

function TPreProcessorExpressionTokeniser.StartsWith(const ps: string): boolean;
begin
  Result := AnsiSameText(StrLeft(Rest, Length(ps)), ps);
end;

procedure TPreProcessorExpressionTokeniser.Tokenise;
begin
  fcTokens.Clear;
  fiCurrentIndex := 1;
  fbHasError     := False;

  while fiCurrentIndex <= Length(fsExpr) do
  begin
    if not TryConsumeFixedSymbol then
      if not TryConsumeIdentifier then
      begin
        // unknown/unsupported Syntax. :(
        fbHasError := True;
        break;
      end;

    ConsumeWhiteSpace;
  end;

end;

function TPreProcessorExpressionTokeniser.TryConsumeFixedSymbol: boolean;
var
  leLoop:  TPreProcessorSymbol;
  lbFound: boolean;
begin
  Result := False;

  for leLoop := low(SYMBOL_DATA) to high(SYMBOL_DATA) do
  begin
    lbFound := StartsWith(SYMBOL_DATA[leLoop]);

    if lbFound then
    begin
      fcTokens.Add(leLoop, SYMBOL_DATA[leLoop]);

      fiCurrentIndex := fiCurrentIndex + Length(SYMBOL_DATA[leLoop]);
      Result := True;
      break;
    end;
  end;
end;


function TPreProcessorExpressionTokeniser.TryConsumeIdentifier: boolean;
var
  liStart: integer;
  lsIdentifierText: string;
begin
  Result := False;

  if CharIsAlpha(fsExpr[fiCurrentIndex]) then
  begin
    liStart := fiCurrentIndex;
    while CharIsAlphaNum(fsExpr[fiCurrentIndex]) do
      Inc(fiCurrentIndex);

    Result := True;

    lsIdentifierText := string(copy(fsExpr, liStart, fiCurrentIndex - liStart));
    fcTokens.Add(eIdentifier, lsIdentifierText);
  end;
end;


procedure TPreProcessorExpressionTokeniser.ConsumeWhiteSpace;
begin
  // this lexer can ignore the white space
  while (fiCurrentIndex < Length(fsExpr)) and CharIsWhiteSpace(fsExpr[fiCurrentIndex]) do
    Inc(fiCurrentIndex);
end;

end.
