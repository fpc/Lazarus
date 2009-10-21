{(*}
(*------------------------------------------------------------------------------
 Delphi Code formatter source code 

The Original Code is SetTransform.pas, released June 2004.
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

unit SetTransform;

{ settings to do with Code transformation
  AFS 5 June 2004 
}

{$I JcfGlobal.inc}

interface

uses JcfSetBase, SettingsTypes, SettingsStream;

type
  TUsesSortOrder = (eAlpha, eReverseAlpha, eShortToLong, eLongToShort);

  TSetTransform = class(TSetBase)
  private
    { add/remove begin/end from single statement blocks }
    feBeginEndStyle: TTriOptionStyle;
    { add a semicolon at the last statement of a block }
    fbAddBlockEndSemicolon: Boolean;

    { sort uses clauses }

    { when to sort }
    fbSortInterfaceUses: Boolean;
    fbSortImplementationUses: Boolean;
    fbSortProgramUses: boolean;

    fbSortUsesNoComments: boolean;

    { how to sort}
    fbBreakUsesSortOnReturn: Boolean;
    fbBreakUsesSortOnComment: Boolean;
    feUsesSortOrder: TUsesSortOrder;
  public
    constructor Create;

    procedure WriteToStream(const pcOut: TSettingsOutput); override;
    procedure ReadFromStream(const pcStream: TSettingsInput); override;

    property BeginEndStyle: TTriOptionStyle read feBeginEndStyle write feBeginEndStyle;
    property AddBlockEndSemicolon: boolean read fbAddBlockEndSemicolon write fbAddBlockEndSemicolon;

    property SortInterfaceUses: Boolean read fbSortInterfaceUses write fbSortInterfaceUses;
    property SortImplementationUses: Boolean read fbSortImplementationUses write fbSortImplementationUses;
    property SortProgramUses: Boolean read fbSortProgramUses write fbSortProgramUses;

    property SortUsesNoComments: Boolean read fbSortUsesNoComments write fbSortUsesNoComments;

    property BreakUsesSortOnReturn: Boolean read fbBreakUsesSortOnReturn write fbBreakUsesSortOnReturn;
    property BreakUsesSortOnComment: Boolean read fbBreakUsesSortOnComment write fbBreakUsesSortOnComment;
    property UsesSortOrder: TUsesSortOrder read feUsesSortOrder write feUsesSortOrder;

  end;

implementation

const
  REG_BEGIN_END_STYLE = 'BeginEndStyle';
  REG_ADD_BLOCK_END_SEMICOLON = 'AddBlockEndSemicolon';
  REG_SORT_USES_INTERFACE = 'SortUsesInterface';
  REG_SORT_USES_IMPLEMENTATION = 'SortUsesImplmentation';
  REG_SORT_USES_PROGRAM = 'SortUsesProgram';
  REG_SORT_USES_BREAK_ON_RETURN = 'SortUsesBreakOnReturn';
  REG_SORT_USES_BREAK_ON_COMMENT = 'SortUsesBreakOnComment';
  REG_SORT_USES_SORT_ORDER = 'SortUsesSortOrder';
  REG_SORT_USES_NO_COMMENTS = 'SortUsesNoComments';

constructor TSetTransform.Create;
begin
  inherited;
  SetSection('Transform');
end;

procedure TSetTransform.ReadFromStream(const pcStream: TSettingsInput);
begin
  Assert(pcStream <> nil);
  feBeginEndStyle := TTriOptionStyle(pcStream.Read(REG_BEGIN_END_STYLE, Ord(eLeave)));
  fbAddBlockEndSemicolon := pcStream.Read(REG_ADD_BLOCK_END_SEMICOLON, True);

  fbSortInterfaceUses := pcStream.Read(REG_SORT_USES_INTERFACE, True);
  fbSortImplementationUses := pcStream.Read(REG_SORT_USES_IMPLEMENTATION, True);
  fbSortProgramUses := pcStream.Read(REG_SORT_USES_PROGRAM, False);

  fbBreakUsesSortOnReturn := pcStream.Read(REG_SORT_USES_BREAK_ON_RETURN, True);
  fbBreakUsesSortOnComment := pcStream.Read(REG_SORT_USES_BREAK_ON_COMMENT, True);
  feUsesSortOrder := TUsesSortOrder(pcStream.Read(REG_SORT_USES_SORT_ORDER, Ord(eAlpha)));
  fbSortUsesNoComments := pcStream.Read(REG_SORT_USES_NO_COMMENTS, True);
end;

procedure TSetTransform.WriteToStream(const pcOut: TSettingsOutput);
begin
  Assert(pcOut <> nil);

  pcOut.Write(REG_BEGIN_END_STYLE, Ord(feBeginEndStyle));
  pcOut.Write(REG_ADD_BLOCK_END_SEMICOLON, fbAddBlockEndSemicolon);

  pcOut.Write(REG_SORT_USES_INTERFACE, fbSortInterfaceUses);
  pcOut.Write(REG_SORT_USES_IMPLEMENTATION, fbSortImplementationUses);
  pcOut.Write(REG_SORT_USES_PROGRAM, fbSortProgramUses);

  pcOut.Write(REG_SORT_USES_BREAK_ON_RETURN, fbBreakUsesSortOnReturn);
  pcOut.Write(REG_SORT_USES_BREAK_ON_COMMENT, fbBreakUsesSortOnComment);
  pcOut.Write(REG_SORT_USES_SORT_ORDER, Ord(feUsesSortOrder));
  pcOut.Write(REG_SORT_USES_NO_COMMENTS, fbSortUsesNoComments);
end;

end.
 