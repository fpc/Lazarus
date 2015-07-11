unit AlignTypedef;

{(*}
(*------------------------------------------------------------------------------
 Delphi Code formatter source code 

The Original Code is AlignTypedef, released May 2003.
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

uses SourceToken, AlignBase;

type

  TAlignTypedef = class(TAlignBase)
  protected
    { TokenProcessor overrides }
    function IsTokenInContext(const pt: TSourceToken): boolean; override;

      { AlignStatements overrides }
    function TokenIsAligned(const pt: TSourceToken): boolean; override;
    function TokenEndsStatement(const pt: TSourceToken): boolean; override;
    function TokenEndsAlignment(const pt: TSourceToken): boolean; override;

  public
    constructor Create; override;

    function IsIncludedInSettings: boolean; override;
  end;

implementation

uses FormatFlags, JcfSettings, ParseTreeNodeType, Tokens, TokenUtils;

constructor TAlignTypedef.Create;
begin
  inherited;
  FormatFlags := FormatFlags + [eAlignTypedef];
end;

function TAlignTypedef.IsIncludedInSettings: boolean;
begin
  Result := ( not FormatSettings.Obfuscate.Enabled) and
    FormatSettings.Align.AlignTypedef;
end;

function TAlignTypedef.IsTokenInContext(const pt: TSourceToken): boolean;
begin
  Result := pt.HasParentNode(nTypeDecl) and ( not pt.HasParentNode(ObjectTypes)) and
    (( not FormatSettings.Align.InterfaceOnly) or (pt.HasParentNode(nInterfaceSection)));
end;

function TAlignTypedef.TokenEndsAlignment(const pt: TSourceToken): boolean;
begin
  // alignment block ended by a blank line
  Result := IsBlankLineEnd(pt);
end;

function InStructuredTypeBody(const pt: TSourceToken): boolean;
begin
  Result := pt.HasParentNode(ObjectTypes + [nRecordType]);
  if Result then
  begin
    // exclude the starting word
    Result := not (pt.TokenType in StructuredTypeWords + [ttObject]);
  end;
end;

function TAlignTypedef.TokenEndsStatement(const pt: TSourceToken): boolean;
begin
  if pt = nil then
    Result := True
  { only look at solid tokens }
  else if (pt.TokenType in [ttReturn, ttWhiteSpace]) then
  begin
    Result := False;
  end
  else
  begin
    Result := ( not pt.HasParentNode(nTypeDecl)) or
      (pt.TokenType in [ttSemiColon]) or InStructuredTypeBody(pt);
  end;
end;

function TAlignTypedef.TokenIsAligned(const pt: TSourceToken): boolean;
begin
  Result := (pt.TokenType = ttEquals);
end;

end.
