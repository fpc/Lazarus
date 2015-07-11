{(*}
(*------------------------------------------------------------------------------
 Delphi Code formatter source code 

The Original Code is MozComment.pas, released April 2000.
The Initial Developer of the Original Code is Anthony Steele. 
Portions created by Anthony Steele are Copyright (C) 1999-2000 Anthony Steele.
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

unit MozComment;

{ AFS 24 march 2K
 The Mozilla public licence requires that all files include a header comment
 that specifies the licence.

 Right now, my files don't
 The easiest way to fix that, is this code below:
}

{$I JcfGlobal.inc}

interface

uses BaseVisitor, SourceToken;

type
  TMozComment = class(TBaseTreeNodeVisitor)
  private
    fbWorkIsDone: boolean;
  protected

  public
    constructor Create; override;

    function VisitSourceToken(const pcToken: TObject): Boolean; override;
    function IsIncludedInSettings: boolean; override;
  end;


implementation

uses
  { delphi }
  SysUtils,
  { local }
  Tokens, TokenUtils, JcfSettings, JcfStringUtils,
  SettingsTypes, ParseTreeNodeType, SetClarify;


const
  { this comment will be inserted in all files above the unit header }
  MozURL = 'http://www.mozilla.org/NPL/';

  MozCommentString: string =
    NOFORMAT_ON + NativeLineBreak + // so this program can't easily obfuscate it out
    '(*------------------------------------------------------------------------------' +
    NativeLineBreak +
    ' Delphi Code formatter source code ' + NativeLineBreak + NativeLineBreak +
    'The Original Code is <FileName>, released <Date>.' + NativeLineBreak +
    'The Initial Developer of the Original Code is Anthony Steele. ' + NativeLineBreak +
    'Portions created by Anthony Steele are Copyright (C) 1999-2000 Anthony Steele.' +
    NativeLineBreak +
    'All Rights Reserved. ' + NativeLineBreak +
    'Contributor(s): Anthony Steele. ' + NativeLineBreak + NativeLineBreak +
    'The contents of this file are subject to the Mozilla Public License Version 1.1' +
    NativeLineBreak +
    '(the "License"). you may not use this file except in compliance with the License.' +
    NativeLineBreak +
    'You may obtain a copy of the License at ' + MozURL + NativeLineBreak + NativeLineBreak +
    'Software distributed under the License is distributed on an "AS IS" basis,' +
    NativeLineBreak +
    'WITHOUT WARRANTY OF ANY KIND, either express or implied.' + NativeLineBreak +
    'See the License for the specific language governing rights and limitations ' +
    NativeLineBreak +
    'under the License.' + NativeLineBreak +
    '------------------------------------------------------------------------------*)' +
    NativeLineBreak + NOFORMAT_OFF + NativeLineBreak + NativeLineBreak;

function FirstOpportunityForMozInsert(const pt: TSourceToken): boolean;
begin
  Result := False;

  if (pt.TokenType = ttUses) and pt.HasParentNode(nUses, 2) and
    pt.HasParentNode(TopOfProgramSections, 2) then
  begin
    { just before first uses clause in program, etc }
    Result := True;
  end
  else if (pt.TokenType = ttInterface) and pt.HasParentNode(nUnit, 2) then
  begin
    // before interface in unit 
    Result := True;
  end;
end;


constructor TMozComment.Create;
begin
  inherited;
  fbWorkIsDone := False;
end;

function TMozComment.IsIncludedInSettings: boolean;
begin
  Result := ( not FormatSettings.Obfuscate.Enabled) and
    (FormatSettings.Clarify.OnceOffs <> eDoNotRun)
end;


function TMozComment.VisitSourceToken(const pcToken: TObject): Boolean;
var
  lsFile:      string;
  lsComment:   string;
  lcToken, lcNewComment: TSourceToken;
  lbInContext: boolean;
begin
  Result := False;
  if fbWorkIsDone then
    exit;

  lcToken := TSourceToken(pcToken);

  if (lcToken.TokenType = ttComment) and (Pos(WideString(MozURL), lcToken.SourceCode) > 0) then
  begin
    fbWorkIsDone := True;
    exit;
  end;

  lbInContext := FirstOpportunityForMozInsert(lcToken);
  if not lbInContext then
    exit;

  { get the file name but remove the path
    This will be inserted into the standard comment string
  }

  lsFile := JCFUnitName(lcToken);

  lsComment := MozCommentString;
  lsComment := StringReplace(lsComment, '<FileName>', lsFile, [rfReplaceAll]);
  lsComment := StringReplace(lsComment, '<Date>', FormatDateTime('mmmm yyyy', Date),
    [rfReplaceAll]);

  // put the comment in front of the unit start word
  lcNewComment := TSourceToken.Create;
  lcNewComment.TokenType := ttComment;
  lcNewComment.SourceCode := lsComment;

  lcToken.Parent.InsertChild(lcToken.IndexOfSelf, lcNewComment);

  fbWorkIsDone := True;
end;

end.
