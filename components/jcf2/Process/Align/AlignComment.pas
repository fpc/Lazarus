unit AlignComment;

{(*}
(*------------------------------------------------------------------------------
 Delphi Code formatter source code 

The Original Code is AlignComment, released May 2003.
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
  TAlignComment = class(TAlignBase)
  private
    // don't align across block nexting levels
    fiStartBlockLevel: integer;

  protected
    { TokenProcessor overrides }
    function IsTokenInContext(const pt: TSourceToken): boolean; override;

      { AlignStatements overrides }
    function TokenIsAligned(const pt: TSourceToken): boolean; override;
    function TokenEndsStatement(const pt: TSourceToken): boolean; override;

    procedure ResetState; override;
  public
    constructor Create; override;

    function IsIncludedInSettings: boolean; override;

  end;


implementation

uses FormatFlags, JcfSettings, ParseTreeNodeType, Tokens, TokenUtils;

constructor TAlignComment.Create;
begin
  inherited;
  FormatFlags := FormatFlags + [eAlignComment];
  fiStartBlockLevel := -1;
end;

function TAlignComment.IsIncludedInSettings: boolean;
begin
  Result := (not FormattingSettings.Obfuscate.Enabled) and
    FormattingSettings.Align.AlignComment;
end;

function TAlignComment.IsTokenInContext(const pt: TSourceToken): boolean;
begin
  Result := (not FormattingSettings.Align.InterfaceOnly) or
    (pt.HasParentNode(nInterfaceSection));
end;

procedure TAlignComment.ResetState;
begin
  inherited;
  fiStartBlockLevel := -1;
end;

function TAlignComment.TokenEndsStatement(const pt: TSourceToken): boolean;
begin
  if pt = nil then
    Result := True
  { only look at solid tokens and returns }
  else if (pt.TokenType in [ttWhiteSpace]) then
  begin
    Result := False;
  end
  else
  begin
    Result := (pt.TokenType in [ttReturn]);
  end;
end;

function ShortEnoughToMove(const pt: TSourceToken): boolean;
begin
  // don't further indent long lines
  Result := (pt.XPosition + Length(pt.SourceCode)) <= FormattingSettings.Returns.MaxLineLength;
end;

function TAlignComment.TokenIsAligned(const pt: TSourceToken): boolean;
var
  ltNext: TSourceToken;
begin
  // must be a comment on one line
  Result := IsSingleLineComment(pt);

  { don't align compiler directives like other comments }
  if Result then
    Result := (pt.CommentStyle <> eCompilerDirective);

  if Result then
  begin

    // must be the last thing on the line
    ltNext := pt.NextTokenWithExclusions([ttWhitespace]);
    Result := (ltNext <> nil) and (ltNext.TokenType = ttReturn) and
      ShortEnoughToMove(pt) and
      ((fiStartBlockLevel < 0) or (fiStartBlockLevel = BlockLevel(pt)));

    if Result and (fiStartBlockLevel < 0) then
      fiStartBlockLevel := BlockLevel(pt);
  end;

end;

end.
