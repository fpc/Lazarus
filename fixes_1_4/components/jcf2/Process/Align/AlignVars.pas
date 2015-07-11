{(*}
(*------------------------------------------------------------------------------
 Delphi Code formatter source code 

The Original Code is AlignVars.pas, released April 2000.
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

unit AlignVars;

{ AFS 12 Feb 2K
 Align the RHS of var types
}

{$I JcfGlobal.inc}

interface

uses SourceToken, AlignBase;

type

  TFoundTokenState = (eBefore, eOn, eAfter, eUnknown);

  TAlignVars = class(TAlignBase)
  protected
    feFoundTokenState: TFoundTokenState;

    function AlignedToken(const pt: TSourceToken): boolean; virtual;

    { TokenProcessor overrides }
    function IsTokenInContext(const pt: TSourceToken): boolean; override;

      { AlignStatements overrides }
    function TokenIsAligned(const pt: TSourceToken): boolean; override;
    function TokenEndsStatement(const pt: TSourceToken): boolean; override;

    procedure OnTokenRead(const pt: TSourceToken); override;
    procedure ResetState; override;


  public
    constructor Create; override;

    function IsIncludedInSettings: boolean; override;
  end;

implementation

uses
  { local}Tokens, FormatFlags, JcfSettings,
  ParseTreeNodeType,
  TokenUtils, Nesting;

{ TAlignVars }

constructor TAlignVars.Create;
begin
  inherited;
  FormatFlags := FormatFlags + [eAlignVars];
  feFoundTokenState := eUnknown;
end;

function TAlignVars.AlignedToken(const pt: TSourceToken): boolean;
const
  NOT_ALIGNED: TTokenTypeSet = [ttWhiteSpace, ttReturn, ttComment, ttSemiColon, ttColon];
begin
  Result := ( not (pt.TokenType in NOT_ALIGNED)) and
    pt.HasParentNode(nVarSection) and pt.IsOnRightOf(nVarDecl, ttColon);
end;

procedure TAlignVars.OnTokenRead(const pt: TSourceToken);
begin
  inherited;

  { when we get the colon, we are now *before* the target token
   on the next on-whitespace token we are *on* it,
   any token after that, we are *after* it
    unknown status is there to match the very first token in the block }

  if pt.TokenType = ttColon then
    feFoundTokenState := eBefore
  else if AlignedToken(pt) and
    (feFoundTokenState in [eUnknown, eBefore]) then
    feFoundTokenState := eOn
  else if feFoundTokenState = eOn then
    feFoundTokenState := eAfter
  else if feFoundTokenState = eUnknown then
    feFoundTokenState := eBefore;
end;

procedure TAlignVars.ResetState;
begin
  inherited;
  feFoundTokenState := eUnknown;
end;

function TAlignVars.TokenEndsStatement(const pt: TSourceToken): boolean;
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
    Result := (pt.TokenType = ttSemiColon) or ( not pt.HasParentNode(nVarSection));
  end;
end;

function TAlignVars.IsTokenInContext(const pt: TSourceToken): boolean;
begin
  Result := (pt.HasParentNode(nVarSection)) and (RoundBracketLevel(pt) < 1) and
    (( not FormatSettings.Align.InterfaceOnly) or (pt.HasParentNode(nInterfaceSection)));
end;

function TAlignVars.TokenIsAligned(const pt: TSourceToken): boolean;
begin
  { the local var feFoundTokenState is used to recognise the first token
    in the type after the colon }

  Result := (feFoundTokenState in [eOn, eUnknown]) and AlignedToken(pt);

  if Result and (pt.Nestings.GetLevel(nlRecordType) > 0) then
    Result := False;

  if Result and (RoundBracketLevel(pt) > 0) then
    Result := False;
end;

function TAlignVars.IsIncludedInSettings: boolean;
begin
  Result := ( not FormatSettings.Obfuscate.Enabled) and FormatSettings.Align.AlignVar;
end;


end.
