unit BlockStyles;

{(*}
(*------------------------------------------------------------------------------
 Delphi Code formatter source code 

The Original Code is BlockStyles, released May 2003.
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

{ AFS 22 April 2003
  This unit handles the different styles of line breaking & spacing after the constructs
  if <expression> then
    statement;

  if <expression> then
  begin
     ..statements
  end;

  The styles are
  - never break line (subject to approval by the linebreaker.
    If the resulting line is too long, just after the then
    is a very good place to break and may be chosen. )
  - Leave as is
  - Always break line. This is the official style
    http://www.borland.com/techvoyage/articles/DelphiStyle/StyleGuide.html

  This style also applies to
    for <expression> do
  and
    while <expression> do
  and
    else
      <statement>

 to do:
 apply to

 case exp of
  value: statement;
  value: begin statements end;

 end;
}

uses SwitchableVisitor, ParseTreeNodeType;

type
  TBlockStyles = class(TSwitchableVisitor)
  private

  protected
    function EnabledVisitSourceToken(const pcNode: TObject): Boolean; override;
  public
    constructor Create; override;

    function IsIncludedInSettings: boolean; override;
  end;

implementation

uses
  { local }
  JcfStringUtils,
  Tokens, SourceToken, TokenUtils, JcfSettings,
  FormatFlags, SettingsTypes;

const
  BreakWords: TTokenTypeSet = [ttThen, ttDo, ttElse, ttEnd];

function GetStyle(const pt: TSourceToken): TTriOptionStyle;
var
  lcNextToken: TSourceToken;
begin
  Result := eLeave;

  lcNextToken := pt.NextSolidToken;
  if lcNextToken = nil then
    exit;

  { only do anything to an end if it is followed by an else }
  if pt.TokenType = ttEnd then
  begin
    if lcNextToken.TokenType = ttElse then
      Result := FormattingSettings.Returns.EndElseStyle;
  end
  else if pt.TokenType = ttColon then
  begin
    if IsCaseColon(pt) then
    begin
      if lcNextToken.TokenType = ttBegin then
        Result := FormattingSettings.Returns.CaseBeginStyle
      else
        Result := FormattingSettings.Returns.CaseLabelStyle;
    end
    else if IsLabelColon(pt) then
    begin
      { otherwise, is there a begin next? }
      if lcNextToken.TokenType = ttBegin then
        Result := FormattingSettings.Returns.LabelBeginStyle
      else
        Result := FormattingSettings.Returns.LabelStyle;
    end;
  end
  else if (pt.TokenType = ttElse) then
  begin

    if pt.HasParentNode(nElseCase, 1) then
    begin
      if lcNextToken.TokenType = ttBegin then
        Result := FormattingSettings.Returns.CaseElseBeginStyle
      else
        Result := FormattingSettings.Returns.CaseElseStyle;
    end
    else if (lcNextToken.TokenType = ttIf) then
    begin
      { though else normally starts a block,
       according to standards, there is never a return in "else if" (!!! check!)
       But we have a config setting for Marcus F

        **NB** rare exception: this does not apply when the if is not related to the else
        ie
         case (foo) of
           1:
            DoSomething1;
           2:
            SoSomething2;
           else
             // this is the else case, not part of an if.
             // All statements from the 'else' to the 'end' form a block
             if (SomeCond) then // though the 'if' is directly after the else, this is not an else-if
               DoSomethingElse;
             if (SomeOtherCond) then
               DoSomeOtherThing;
         end;

         end;

       }
      Result := FormattingSettings.Returns.ElseIfStyle;
    end
    else if (lcNextToken.TokenType = ttBegin) then
    begin
      Result := FormattingSettings.Returns.ElseBeginStyle;
    end;
  end
  else
  begin
    { otherwise, is there a begin next? }
    if lcNextToken.TokenType = ttBegin then
      Result := FormattingSettings.Returns.BlockBeginStyle
    else
      Result := FormattingSettings.Returns.BlockStyle;
  end;
end;


constructor TBlockStyles.Create;
begin
  inherited;
  FormatFlags := FormatFlags + [eBlockStyle, eAddReturn, eRemoveReturn];
end;

function TBlockStyles.EnabledVisitSourceToken(const pcNode: TObject): Boolean;
var
  leStyle: TTriOptionStyle;
  lcSourceToken, lcNextReturn, lcNextComment, lcNextSpace: TSourceToken;
begin
  Result := False;
  lcSourceToken := TSourceToken(pcNode);

  if (lcSourceToken.TokenType in BreakWords) or IsLabelColon(lcSourceToken) or
    IsCaseColon(lcSourceToken) then
  begin
    leStyle := GetStyle(lcSourceToken);

    case leStyle of
      eLeave: ; // do nothing, leave as is
      eAlways:
      begin
        lcNextReturn := lcSourceToken.NextTokenWithExclusions([ttWhiteSpace, ttComment]);
        if (lcNextReturn <> nil) and (lcNextReturn.TokenType <> ttReturn) then
        begin
          { if there's a white space at heand, turn it into a return. Else make a return }
          lcNextSpace := lcSourceToken.NextToken;
          if lcNextSpace.TokenType = ttWhiteSpace then
          begin
            lcNextSpace.TokenType := ttReturn;
            lcNextSpace.SourceCode := NativeLineBreak;
          end
          else
            InsertTokenAfter(lcSourceToken, NewReturn);
        end;
      end;
      eNever:
      begin
        lcNextReturn := lcSourceToken.NextTokenWithExclusions([ttWhiteSpace, ttComment]);
        if (lcNextReturn <> nil) and (lcNextReturn.TokenType = ttReturn) then
        begin
          { if there is a '//' comment before the return, don't remove the return :) }
          lcNextComment := lcSourceToken.NextTokenWithExclusions([ttWhiteSpace]);
          if lcNextComment.CommentStyle <> eDoubleSlash then
          begin
            // turn to space
            lcNextReturn.TokenType := ttWhiteSpace;
            // need some space here - don't leave nothing between tokens
            if NextToWhiteSpace(lcNextReturn) then
            begin
              // null space as it's next to space already
              lcNextReturn.SourceCode := '';

              // make the folling space into a single space
              lcNextSpace := lcNextReturn.NextToken;
              if (lcNextSpace <> nil) and (lcNextSpace.TokenType = ttWhiteSpace) then
              begin
                lcNextSpace.SourceCode := ' ';
              end;

            end
            else
              // keep a single space
              lcNextReturn.SourceCode := ' ';
          end;
        end;
      end;
      else
        Assert(False);
    end;
  end;
end;

function TBlockStyles.IsIncludedInSettings: boolean;
begin
  Result := True;
end;

end.
