unit Warning;

{(*}
(*------------------------------------------------------------------------------
 Delphi Code formatter source code 

The Original Code is Warning, released May 2003.
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

uses SwitchableVisitor, ConvertTypes;

type
  TWarning = class(TSwitchableVisitor)
  private
    fOnWarning: TStatusMessageProc;

  protected
    procedure SendWarning(const pcNode: TObject; const psMessage: string);

  public
    constructor Create; override;

    function IsIncludedInSettings: boolean; override;

    property OnWarning: TStatusMessageProc Read fOnWarning Write fOnWarning;
  end;


implementation

uses ParseTreeNode, SourceToken, TokenUtils, FormatFlags, JcfSettings;

constructor TWarning.Create;
begin
  inherited;
  FormatFlags := FormatFlags + [eWarning];
end;

function TWarning.IsIncludedInSettings: boolean;
begin
  // included if warnings are turned on 
  Result := FormattingSettings.Clarify.Warnings;
end;

procedure TWarning.SendWarning(const pcNode: TObject; const psMessage: string);
var
  lsMessage, lsProc: string;
  lcToken: TSourceToken;
begin
  { don't bother with the rest }
  if not Assigned(fOnWarning) then
    exit;

  lsMessage := psMessage;
  if (pcNode is TSourceToken) then
  begin
    lcToken := TSourceToken(pcNode);
  end
  else if (pcNode is TParseTreeNode) then
  begin
    // use first token under this node for pos
    lcToken := TParseTreeNode(pcNode).FirstSolidLeaf as TSourceToken;
  end
  else
    lcToken := nil;

  if lcToken <> nil then
  begin
    lsMessage := lsMessage + ' near ' + lcToken.Describe;
    lsProc    := GetProcedureName(lcToken, True, False);
    if lsProc <> '' then
      lsMessage := lsMessage + ' in ' + GetBlockType(lcToken) + ' ' + lsProc;
  end;

  fOnWarning('', lsMessage, mtCodeWarning, lcToken.YPosition, lcToken.XPosition);
end;



end.
