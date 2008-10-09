unit PreProcessorExpressionTokens;

{(*}
(*------------------------------------------------------------------------------
 Delphi Code formatter source code 

The Original Code is PreProcessorExpressionTokens, released August 2003.
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

  Delphi preprocessor $IF expression parsing

  tokens defined via an enum, an item class
  and a list class
}

uses Contnrs;

type

  TPreProcessorSymbol =
    (eNone, eIdentifier,
    // symbols with fixed text
    eOpenBracket, eCloseBracket, eDefined, eDeclared,
    eAnd, eOr, eNot, eTrue, eFalse);

{ used to recognise tokens - all expect identifiers have fixed text }
const
  SYMBOL_DATA: array[eOpenBracket .. eFalse] of string =
    ('(', ')', 'defined', 'declared', 'and', 'or', 'not', 'true', 'false');

type

  TPreProcessorExpressionToken = class(TObject)
  private
    feSymbol: TPreProcessorSymbol;
    fsSourceCode: string;
  public
    property Symbol: TPreProcessorSymbol Read feSymbol Write feSymbol;
    property SourceCode: string Read fsSOurceCode Write fsSourceCode;
  end;

  TPreProcessorExpressionTokenList = class(TObject)
  private
    fcList: TObjectList;

    function GetItems(const piIndex: integer): TPreProcessorExpressionToken;
    function GetCount: integer;
  public
    constructor Create;
    destructor Destroy; override;

    function Add(const peSymbol: TPreProcessorSymbol;
      psText: string): TPreProcessorExpressionToken;
    procedure Clear;

    property Items[const piIndex: integer]: TPreProcessorExpressionToken Read GetItems;
    property Count: integer Read GetCount;
  end;

function PreProcessorSymbolToString(const peSymbol: TPreProcessorSymbol): string;

implementation

uses SysUtils;


function PreProcessorSymbolToString(const peSymbol: TPreProcessorSymbol): string;
begin
  case peSymbol of
    eNone:
      Result := 'No symbol';
    eIdentifier:
      Result := 'identifier';
    eOpenBracket:
      Result := '(';
    eCloseBracket:
      Result := ')';
    eDefined:
      Result := 'defined';
    eAnd:
      Result := 'and';
    eOr:
      Result := 'or';
    eNot:
      Result := 'not';
    eTrue:
      Result := 'true';
    eFalse:
      Result := 'false';
    else
      Assert(False);
  end;

end;

{ TPreProcessorExpressionTokenList }

function TPreProcessorExpressionTokenList.Add(const peSymbol: TPreProcessorSymbol;
  psText: string): TPreProcessorExpressionToken;
begin
  Result := TPreProcessorExpressionToken.Create;
  Result.Symbol := peSymbol;
  Result.SourceCode := psText;

  fcList.Add(Result);
end;

procedure TPreProcessorExpressionTokenList.Clear;
begin
  fcList.Clear;
end;

constructor TPreProcessorExpressionTokenList.Create;
begin
  inherited;
  // thiws is an owning list
  fcList := TObjectList.Create;
end;

destructor TPreProcessorExpressionTokenList.Destroy;
begin
  FreeAndNil(fcList);
  inherited;

end;

function TPreProcessorExpressionTokenList.GetCount: integer;
begin
  Result := fcList.Count;
end;

function TPreProcessorExpressionTokenList.GetItems(
  const piIndex: integer): TPreProcessorExpressionToken;
begin
  if piIndex < fcList.Count then
    Result := TPreProcessorExpressionToken(fcList[piIndex])
  else
    Result := nil;
end;


end.
