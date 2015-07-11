unit PreProcessorExpressionParser;

{(*}
(*------------------------------------------------------------------------------
 Delphi Code formatter source code 

The Original Code is PreProcessorParse, released October 2003.
The Initial Developer of the Original Code is Anthony Steele. 
Portions created by Anthony Steele are Copyright (C) 1999-2008 Anthony Steele.
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

  Delphi preprocessor $IF expression parsing

  an 'immediate' parser that evalutes the expression,
  does not produce a parse tree except in the ephemera of the call stack

  All these expression are in $IF preprocessor statements
  and thus have boolean results

  The entire expression is contained in the text string of a single token

 Grammar:

 expr -> term
 expr -> term and expr
 expr -> term or expr

 term -> (expr)
 term -> defined(identifier)
 term -> not term
 term -> true
 term -> false

 and that's all for now. Just "defined" checks, with brackets, negation and conjunctions
 More will come later, such as '=' '<' etc.
 But that will necessitate symbols with values and some kind of type inference
}


uses
  { celphi }
  Classes, SysUtils,
  { local }
  PreProcessorExpressionTokens;

type
  { distinguish these exceptions from others }
  PreProcessorParseFailedException = class(Exception);

  TPreProcessorExpressionParser = class(TObject)
  private
    fiCurrentIndex: integer;

    // referenced data
    fcTokens: TPreProcessorExpressionTokenList;
    fcDefinedSymbols: TStrings;

    function ParseExpr: boolean;
    function ParseTerm: boolean;

    function CurrentTokenType: TPreProcessorSymbol;
    function MoreTokens: boolean;

    procedure Consume(const peType: TPreProcessorSymbol);
    function SymbolIsDefined(const psSymbol: string): boolean;
    function SymbolIsDeclared(const psSymbol: string): boolean;
  public
    function Parse: boolean;

    property Tokens: TPreProcessorExpressionTokenList Read fcTokens Write fcTokens;
    property DefinedSymbols: TStrings Read fcDefinedSymbols Write fcDefinedSymbols;

  end;


implementation

{ TPreProcessorExpressionParser }

procedure TPreProcessorExpressionParser.Consume(const peType: TPreProcessorSymbol);
begin
  Assert(CurrentTokenType = peType,
    'expected token ' + PreProcessorSymbolToString(peType) +
    ' got ' + PreProcessorSymbolToString(CurrentTokenType) +
    ' at position ' + IntToStr(fiCurrentIndex));
  Inc(fiCurrentIndex)
end;

function TPreProcessorExpressionParser.CurrentTokenType: TPreProcessorSymbol;
begin
  if fiCurrentIndex < fcTokens.Count then
    Result := fcTokens.Items[fiCurrentIndex].Symbol
  else
    Result := eNone;
end;

function TPreProcessorExpressionParser.MoreTokens: boolean;
begin
  Result := fcTokens.Count > fiCurrentIndex;
end;

function TPreProcessorExpressionParser.Parse: boolean;
begin
  Assert(fcTokens <> nil);
  Assert(fcTokens.Count > 0);
  fiCurrentIndex := 0;

  Result := ParseExpr;

  if MoreTokens then
    raise PreProcessorParseFailedException.Create('Expression has trailing tokens');
end;

function TPreProcessorExpressionParser.ParseExpr: boolean;
var
  lbExprResult: boolean;
begin
  Result := ParseTerm;

  if MoreTokens then
  begin
    case CurrentTokenType of
      eAnd:
      begin
        Consume(eAnd);
        // always evaluate this
        lbExprResult := ParseExpr;
        Result := Result and lbExprResult;
      end;
      eOr:
      begin
        Consume(eOr);
        // always evaluate this
        lbExprResult := ParseExpr;
        Result := Result or lbExprResult;
      end;
      eCloseBracket:
      begin
        // do nothing, should be matched to open bracket below
      end;
      else
      begin
        raise PreProcessorParseFailedException.Create(
          'Preprocessor expression could not be parsed');
      end;
    end;
  end;
end;

function TPreProcessorExpressionParser.ParseTerm: boolean;
begin

  case CurrentTokenType of
    eOpenBracket:
    begin
      Consume(eOpenBracket);
      Result := ParseExpr;
      Consume(eCloseBracket);
    end;
    eDefined:
    begin
      Consume(eDefined);
      Consume(eOpenBracket);
      Result := SymbolIsDefined(Tokens.Items[fiCurrentIndex].SourceCode);
      Consume(eIdentifier);
      Consume(eCloseBracket);
    end;
    eDeclared:
    begin
      Consume(eDeclared);
      Consume(eOpenBracket);
      Result := SymbolIsDeclared(Tokens.Items[fiCurrentIndex].SourceCode);
      Consume(eIdentifier);
      Consume(eCloseBracket);
    end;

    eNot:
    begin
      Consume(eNot);
      Result := not ParseTerm;
    end;
    eTrue:
    begin
      Consume(eTrue);
      Result := True;
    end;
    eFalse:
    begin
      Consume(eFalse);
      Result := False;
    end;
    eIdentifier:
    begin
      Consume(eIdentifier);
      // we don't know, so guess. One of us can't be wrong
      Result := True;
    end;
    else
    begin
      CurrentTokenType;
      raise PreProcessorParseFailedException.Create(
        'Preprocessor term could not be parsed');
    end;
  end;
end;


function TPreProcessorExpressionParser.SymbolIsDefined(const psSymbol: string): boolean;
begin
  Result := DefinedSymbols.IndexOf(psSymbol) >= 0;
end;

function TPreProcessorExpressionParser.SymbolIsDeclared(const psSymbol: string): boolean;
begin
  { 'Declared returns true if the argument passed to it
    is a valid declared Delphi identifier visible within the current scope.'
   we are faking it }
  Result := (psSymbol <> '');
end;

end.
