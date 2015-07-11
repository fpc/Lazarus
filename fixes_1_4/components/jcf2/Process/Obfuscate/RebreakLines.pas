unit RebreakLines;

{(*}
(*------------------------------------------------------------------------------
 Delphi Code formatter source code 

The Original Code is RebreakLines, released May 2003.
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

{ AFS 29 December 2002

  Obfuscate process
  break lines at regular intervals
}

uses SwitchableVisitor;

type
  TRebreakLines = class(TSwitchableVisitor)
  private
    xPos: integer;
  protected
    function EnabledVisitSourceToken(const pcNode: TObject): Boolean; override;
  public
    constructor Create; override;
  end;

implementation

uses
  JcfUnicode,
  SourceToken, Tokens, FormatFlags, TokenUtils, ParseTreeNodeType;

function CanBreakHere(const pt: TSourceToken): boolean;
var
  lbInString: Boolean;
  lcNext: TSourceToken;
begin
  lbInString := pt.HasParentNode(nLiteralString);
  if lbInString then
  begin
    lcNext := pt.NextToken;
    lbInString := (lcNext <> nil) and lcNext.HasParentNode(nLiteralString);
  end;

  Result := not lbInString;
end;

constructor TRebreakLines.Create;
begin
  inherited;
  FormatFlags := FormatFlags + [eObfuscate];
  xPos := 1;
end;

function TRebreakLines.EnabledVisitSourceToken(const pcNode: TObject): Boolean;
const
  LINE_LENGTH = 80;
var
  lcToken: TSourceToken;
  lcNext, lcNew: TSourceToken;
  liLen:   integer;
begin
  Result := False;
  lcToken := TSourceToken(pcNode);

  if lcToken.TokenType = ttReturn then
  begin
    xPos := 0
  end
  else
  begin
    liLen := Length(lcToken.SourceCode);

    if ((XPos + liLen) > LINE_LENGTH) and CanBreakHere(lcToken) then
    begin
      { no space directly after the new return }
      lcNext := lcToken.NextToken;
      if (lcNext <> nil) and (lcNext.TokenType = ttWhiteSpace) and
        (lcNext.SourceCode <> '') then
        BlankToken(lcNext);

      { need a return? }
      if (lcNext <> nil) and (lcNext.TokenType <> ttReturn) then
      begin

        lcNew := TSourceToken.Create;
        lcNew.TokenType := ttReturn;
        lcNew.SourceCode := WideLineBreak;
        XPos  := 0;

        InsertTokenAfter(lcToken, lcNew);
      end;
    end
    else
      // not at enhd of line yet 
      xPos := xPos + liLen;
  end;
end;

end.
