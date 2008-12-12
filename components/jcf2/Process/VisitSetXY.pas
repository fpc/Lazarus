unit VisitSetXY;

{ A visitor to set the X and Y coordinates of each token
  based on keeping a running count of the text length and number of newlines }

{(*}
(*------------------------------------------------------------------------------
 Delphi Code formatter source code 

The Original Code is VisitSetXY, released May 2003.
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

uses BaseVisitor;

type
  TVisitSetXY = class(TBaseTreeNodeVisitor)
  private
    // running totals of x and Y pos, and count of solid tokens on the line
    fiX, fiY, fiSolidTokenOnLineIndex: integer;
    fsFileName: string;
  public
    constructor Create; override;

    function VisitSourceToken(const pcToken: TObject): Boolean; override;
  end;

implementation

uses
  { local }
  JcfStringUtils,
  JcfMiscFunctions, SourceToken, Tokens;

constructor TVisitSetXY.Create;
begin
  inherited;
  
  // text coords start at 1,1
  fiX := 1;
  fiY := 1;
  fiSolidTokenOnLineIndex := 0;
  fsFileName := '';
end;

function TVisitSetXY.VisitSourceToken(const pcToken: TObject): boolean;
var
  lcToken: TSourceToken;
begin
  Result := False;
  lcToken := TSourceToken(pcToken);

    // track the file name
  if (fsFileName = '') and (lcToken.FileName <> '') then
  begin
    fsFileName := lcToken.FileName;
  end;

  // apply file name to all tokens that don't have it
  if lcToken.FileName = '' then
  begin
    lcToken.FileName := fsFileName;
  end;
  


  // track position
  lcToken.XPosition := fiX;
  lcToken.YPosition := fiY;
  lcToken.SolidTokenOnLineIndex := fiSolidTokenOnLineIndex;

  if lcToken.TokenType = ttReturn then
    fiSolidTokenOnLineIndex := 0
  else if (lcToken.TokenType = ttComment) and
    (Pos(NativeLineBreak, string(lcToken.SourceCode)) > 0) then
    fiSolidTokenOnLineIndex := 0
  else if lcToken.IsSolid then
    Inc(fiSolidTokenOnLineIndex);

  // keep count
  AdvanceTextPos(lcToken.SourceCode, fiX, fiY);
end;

end.
