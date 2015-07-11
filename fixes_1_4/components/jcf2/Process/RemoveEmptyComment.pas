unit RemoveEmptyComment;

{(*}
(*------------------------------------------------------------------------------
 Delphi Code formatter source code 

The Original Code is RemoveEmptyComment, released Nov 2003.
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

{ AFS 9 Nov 2003
  Remove empty comments
}

uses SwitchableVisitor;

type
  TRemoveEmptyComment = class(TSwitchableVisitor)
  private
  protected
    function EnabledVisitSourceToken(const pcNode: TObject): Boolean; override;
  public
    constructor Create; override;

    function IsIncludedInSettings: boolean; override;
  end;


implementation

uses
  { system }
  SysUtils,
  { local }
  JcfStringUtils,
  FormatFlags, SourceToken, Tokens, TokenUtils, JcfSettings;


constructor TRemoveEmptyComment.Create;
begin
  inherited;
  FormatFlags := FormatFlags + [eRemoveComments];
end;

function TRemoveEmptyComment.EnabledVisitSourceToken(const pcNode: TObject): Boolean;
var
  lcSourceToken: TSourceToken;
  lsCommentText: string;
begin
  Result := False;
  lcSourceToken := TSourceToken(pcNode);

  case lcSourceToken.CommentStyle of
    eDoubleSlash:
    begin
      if FormatSettings.Comments.RemoveEmptyDoubleSlashComments then
      begin
        lsCommentText := StrAfter('//', lcSourceToken.SourceCode);
        lsCommentText := Trim(lsCommentText);
        if lsCommentText = '' then
          BlankToken(lcSourceToken);
      end;
    end;
    eCurlyBrace:
    begin
      if FormatSettings.Comments.RemoveEmptyCurlyBraceComments then
      begin
        lsCommentText := StrAfter('{', lcSourceToken.SourceCode);
        lsCommentText := StrBefore('}', lsCommentText);
        lsCommentText := Trim(lsCommentText);
        if lsCommentText = '' then
          BlankToken(lcSourceToken);
      end;
    end;
    eBracketStar, eCompilerDirective: ; // always leave these
    eNotAComment: ; // this is not a comment
    else
      // should not be here
      Assert(False);
  end;
end;

function TRemoveEmptyComment.IsIncludedInSettings: boolean;
begin
  Result := FormatSettings.Comments.RemoveEmptyDoubleSlashComments or
    FormatSettings.Comments.RemoveEmptyCurlyBraceComments;
end;

end.
