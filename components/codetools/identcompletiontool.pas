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
    TIdentCompletionTool enhances the TFindDeclarationTool with the ability
    to create lists of valid identifiers at a specific code position.

}
unit IdentCompletionTool;

{$mode objfpc}{$H+}

{$ifdef FPC}{$mode objfpc}{$endif}{$H+}

interface

{$I codetools.inc}

// activate for debug:

// mem check
{ $DEFINE MEM_CHECK}

// verbosity
{ $DEFINE CTDEBUG}

uses
  {$IFDEF MEM_CHECK}
  MemCheck,
  {$ENDIF}
  Classes, SysUtils, CodeToolsStrConsts, CodeTree, CodeAtom, CustomCodeTool,
  SourceLog, KeywordFuncLists, BasicCodeTools, LinkScanner, CodeCache, AVL_Tree,
  FindDeclarationTool, PascalParserTool;
  

type
  TIdentCompletionTool = class;

  // history, level, alphabetically, compatibility, prefix
  TIdentifierCategory = (
    icatVar,
    icatConst,
    icatFunc,
    icatType,
    icatEnum
    );
  TIdentifierCategories = set of TIdentifierCategory;
    
  TIdentifierCompatibility = (
    icompExact,
    icompCompatible,
    icompUnknown,
    icompIncompatible
    );
  TIdentifierCompatibilities = set of TIdentifierCompatibility;
  
  TIdentifierListItem = class
  private
    AVLNode: TAVLTreeNode;
  public
    Category: TIdentifierCategory;
    Compatibility: TIdentifierCompatibility;
    HasChilds: boolean;
    HistoryIndex: integer;
    Identifier: PChar;
    Node: TCodeTreeNode;
    Tool: TIdentCompletionTool;
  end;
  
  TIdentifierList = class
  private
    FCount: integer;
    FPrefix: string;
    Items: TAVLTree;
    procedure SetPrefix(const AValue: string);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
  public
    property Count: integer read FCount;
    property Prefix: string read FPrefix write SetPrefix;
  end;

  TIdentCompletionTool = class(TFindDeclarationTool)
  public
  
  end;

implementation

{ TIdentifierList }

procedure TIdentifierList.SetPrefix(const AValue: string);
begin
  if FPrefix=AValue then exit;
  FPrefix:=AValue;
end;

constructor TIdentifierList.Create;
begin

end;

destructor TIdentifierList.Destroy;
begin
  inherited Destroy;
end;

procedure TIdentifierList.Clear;
begin

end;

end.

