{
 ***************************************************************************
 *                                                                         *
 *   This source is free software; you can redistribute it and/or modify   *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 *   This code is distributed in the hope that it will be useful, but      *
 *   WITHOUT ANY WARRANTY; without even the implied warranty of            *
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU     *
 *   General Public License for more details.                              *
 *                                                                         *
 *   A copy of the GNU General Public License is available on the World    *
 *   Wide Web at <http://www.gnu.org/copyleft/gpl.html>. You can also      *
 *   obtain it by writing to the Free Software Foundation,                 *
 *   Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.        *
 *                                                                         *
 ***************************************************************************

  Author: Mattias Gaertner

  Abstract:
    TFindDeclarationTool enhances the TPascalParserTool with the ability
    to find the source position or code tree node of a declaration.


  ToDo:
}
unit FindDeclarationTool;

{$ifdef FPC}{$mode objfpc}{$endif}{$H+}

interface

{$I codetools.inc}

uses
  {$IFDEF MEM_CHECK}
  MemCheck,
  {$ENDIF}
  Classes, SysUtils, CodeTree, CodeAtom, CustomCodeTool, SourceLog,
  KeywordFuncLists, BasicCodeTools, LinkScanner, CodeCache, AVL_Tree, TypInfo,
  PascalParserTool;

type
  TOnGetSearchPath = function(Sender: TObject): string;

  TFindDeclarationTool = class(TPascalParserTool)
  private
    FOnGetUnitSourceSearchPath: TOnGetSearchPath;
    function FindDeclarationInUsesSection(UsesNode: TCodeTreeNode;
      CleanPos: integer;
      var NewPos: TCodeXYPosition; var NewTopLine: integer): boolean;
  public
    function FindDeclaration(CursorPos: TCodeXYPosition;
      var NewPos: TCodeXYPosition; var NewTopLine: integer): boolean;
    function FindUnit(const AnUnitName, AnUnitInFilename: string): TCodeBuffer;
    property OnGetUnitSourceSearchPath: TOnGetSearchPath
      read FOnGetUnitSourceSearchPath write FOnGetUnitSourceSearchPath;
  end;

implementation



{ TFindDeclarationTool }

function TFindDeclarationTool.FindDeclaration(CursorPos: TCodeXYPosition;
  var NewPos: TCodeXYPosition; var NewTopLine: integer): boolean;
var r, CleanCursorPos: integer;
  CursorNode: TCodeTreeNode;
begin
  Result:=false;
  // build code tree
{$IFDEF CTDEBUG}
writeln('TFindDeclarationTool.FindDeclaration A CursorPos=',CursorPos.X,',',CursorPos.Y);
{$ENDIF}
  BuildTree(false);
  if not EndOfSourceFound then
    RaiseException('End Of Source not found');
{$IFDEF CTDEBUG}
writeln('TFindDeclarationTool.FindDeclaration B');
{$ENDIF}
  // find the CursorPos in cleaned source
  r:=CaretToCleanPos(CursorPos, CleanCursorPos);
  if (r<>0) and (r<>-1) then
    RaiseException('Cursor outside of code');
  // find CodeTreeNode at cursor
  CursorNode:=FindDeepestNodeAtPos(CleanCursorPos);
  if CursorNode=nil then
    RaiseException('no node found at cursor');
  if CursorNode.Desc=ctnUsesSection then begin
    Result:=FindDeclarationInUsesSection(CursorNode,CleanCursorPos,
                                         NewPos,NewTopLine);
  end else begin

  end;
end;

function TFindDeclarationTool.FindDeclarationInUsesSection(
  UsesNode: TCodeTreeNode; CleanPos: integer;
  var NewPos: TCodeXYPosition; var NewTopLine: integer): boolean;
var UnitName, UnitInFilename: string;
  UnitNamePos, UnitInFilePos: TAtomPosition;
begin
  Result:=false;
{$IFDEF CTDEBUG}
writeln('TFindDeclarationTool.FindDeclarationInUsesSection A');
{$ENDIF}
  // reparse uses section
  MoveCursorToNodeStart(UsesNode);
  repeat
    ReadNextAtom;  // read name
    if CurPos.StartPos>CleanPos then break;
    if AtomIsChar(';') then break;
    AtomIsIdentifier(true);
    UnitNamePos:=CurPos;
    ReadNextAtom;
    if UpAtomIs('IN') then begin
      ReadNextAtom;
      if not AtomIsStringConstant then
        RaiseException(
          'syntax error: string constant expected, but '+GetAtom+' found');
      UnitInFilePos:=CurPos;
      ReadNextAtom;
    end else
      UnitInFilePos.StartPos:=-1;
    if CleanPos<UnitNamePos.EndPos then begin
      // cursor is on a unitname -> try to locate it
      UnitName:=copy(Src,UnitNamePos.StartPos,
                     UnitNamePos.EndPos-UnitNamePos.StartPos);
      if UnitInFilePos.StartPos>=1 then
        UnitInFilename:=copy(Src,UnitInFilePos.StartPos,
                     UnitInFilePos.EndPos-UnitInFilePos.StartPos)
      else
        UnitInFilename:='';
      NewPos.Code:=FindUnit(UnitName,UnitInFilename);
      if NewPos.Code=nil then
        RaiseException('unit not found: '+UnitName);
      NewPos.X:=1;
      NewPos.Y:=1;
      NewTopLine:=1;
      Result:=true;
      exit;
    end;
    if AtomIsChar(';') then break;
    if not AtomIsChar(',') then
      RaiseException(
        'syntax error: ; expected, but '+GetAtom+' found')
  until (CurPos.StartPos>SrcLen);
{$IFDEF CTDEBUG}
writeln('TFindDeclarationTool.FindDeclarationInUsesSection END cursor not on unitname');
{$ENDIF}
end;

function TFindDeclarationTool.FindUnit(const AnUnitName,
  AnUnitInFilename: string): TCodeBuffer;
begin
{$IFDEF CTDEBUG}
writeln('TFindDeclarationTool.FindUnit A AnUnitName=',AnUnitName,' AnUnitInFilename=',AnUnitInFilename);
{$ENDIF}
  Result:=nil;

  
end;

end.




