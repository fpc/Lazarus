unit AlignField;

{(*}
(*------------------------------------------------------------------------------
 Delphi Code formatter source code 

The Original Code is AlignField, released June 2004.
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

{ the purpose of this process is to align the type names
  of fields in class and record declarations

  e.g.
  type
   TFoo = class
   private
     fiField1:     integer;
     fsSomeSTring: string;
     ff:           float;
   end;

  This is  similar to aligning a "var" section,
  so this class overrides TAlignVars
  and just changes the rules for where it kicks in
}

uses SourceToken, AlignVars;

type

  TAlignField = class(TAlignVars)
  private
  protected

    { TokenProcessor overrides }
    function AlignedToken(const pt: TSourceToken): boolean; override;

    function IsTokenInContext(const pt: TSourceToken): boolean; override;

    function TokenIsAligned(const pt: TSourceToken): boolean; override;
    function TokenEndsStatement(const pt: TSourceToken): boolean; override;
    function TokenEndsAlignment(const pt: TSourceToken): boolean; override;

  public
    constructor Create; override;

    function IsIncludedInSettings: boolean; override;
  end;

implementation

uses
  FormatFlags, 
  JcfSettings, ParseTreeNodeType, Tokens, TokenUtils;

{ Adding 'nClassDeclarations' causes the top part of a form
(ie without a visiblity specifier) to be aligned.
  This is probably the desired behaviour. 
}
const
  FORMATTED_SECTIONS: TParseTreeNodeTypeSet =
    [nClassVisibility, nClassDeclarations, nFieldDeclaration];

constructor TAlignField.Create;
begin
  inherited;
  FormatFlags := FormatFlags + [eAlignField];
end;

function TAlignField.AlignedToken(const pt: TSourceToken): boolean;
const
  NOT_ALIGNED: TTokenTypeSet = [ttWhiteSpace, ttReturn, ttComment, ttSemiColon, ttColon, ttRecord];
begin
  Result := not (pt.TokenType in NOT_ALIGNED);

  if Result then
    Result := pt.HasParentNode(FORMATTED_SECTIONS);

  if Result then
    Result := pt.IsOnRightOf([nVarDecl, nFieldDeclaration], [ttColon]);
end;

function TAlignField.IsIncludedInSettings: boolean;
begin
  Result := ( not FormatSettings.Obfuscate.Enabled) and FormatSettings.Align.AlignField;
end;

function TAlignField.IsTokenInContext(const pt: TSourceToken): boolean;
begin
  Result := pt.HasParentNode(FORMATTED_SECTIONS) and
    ((not FormatSettings.Align.InterfaceOnly) or (pt.HasParentNode(nInterfaceSection)));
end;

function TAlignField.TokenEndsStatement(const pt: TSourceToken): boolean;
begin
  if pt = nil then
    Result := True
  { only look at solid tokens }
  else if (pt.TokenType in [ttReturn, ttWhiteSpace]) then
  begin
    // ended by a blank line
    Result := IsBlankLineEnd(pt);
  end
  else
  begin
    Result := (pt.TokenType = ttSemiColon) or (not pt.HasParentNode(FORMATTED_SECTIONS));
  end;
end;

function TAlignField.TokenIsAligned(const pt: TSourceToken): boolean;
begin
  { the local var feFoundTokenState is used to recognise the first token
    in the type after the colon }

  Result := (feFoundTokenState in [eOn, eUnknown]) and AlignedToken(pt);

  if Result and (RoundBracketLevel(pt) > 0) then
    Result := False;
end;

function TAlignField.TokenEndsAlignment(const pt: TSourceToken): boolean;
begin
  // ended by a blank line
  Result := IsBlankLineEnd(pt);

  { ended by change in class visiblity section }
  if not Result then
    Result := pt.TokenType in ClassVisibility;

  { ended by a change in record nesting level }
  if not Result then
    Result := (pt.TokenType = ttCase) and (pt.HasParentNode([nRecordVariantSection], 1));

  if not Result then
    Result := (pt.TokenType = ttRecord) and (pt.HasParentNode([nRecordType], 1));
end;

end.
