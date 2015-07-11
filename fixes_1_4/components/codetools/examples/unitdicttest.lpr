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
    Demonstrating, how to read, update and use the unitdictionary.
}
program unitdicttest;

{$mode objfpc}{$H+}

uses
  Classes, SysUtils, AVL_Tree, CodeCache, CodeToolManager, FileProcs,
  BasicCodeTools, SourceChanger, CodeTree, DefineTemplates, unitdictionary,
  CodeToolsStructs, zstream;
  
const
  ConfigFilename = 'codetools.config';
  DictionaryFilename = 'dictionary.txt';
var
  Filename: string;
  Dictionary: TUnitDictionary;
  UnitSet: TFPCUnitSetCache;
  CfgCache: TFPCTargetConfigCache;
  AVLNode: TAVLTreeNode;
  Item: PStringToStringTreeItem;
  Directory: String;
  D2: TUnitDictionary;
begin
  CodeToolBoss.SimpleInit(ConfigFilename);

  Dictionary:=TUnitDictionary.Create;
  try
    UnitSet:=CodeToolBoss.GetUnitSetForDirectory('');
    CfgCache:=UnitSet.GetConfigCache(true);
    AVLNode:=CfgCache.Units.Tree.FindLowest;
    Directory:=AppendPathDelim(UnitSet.FPCSourceDirectory)+'packages';
    while AVLNode<>nil do begin
      Item:=PStringToStringTreeItem(AVLNode.Data);
      FileName:=UnitSet.GetUnitSrcFile(Item^.Name,false);
      //writeln('UnitName=',Item^.Name,' Source=',Filename);
      if FilenameIsPascalUnit(Filename)
      and FileIsInPath(Filename,Directory) then begin
        //writeln('Filename: ',Filename);
        // parse the unit
        try
          Dictionary.ParseUnit(Filename);
        except
          on E: Exception do begin
            writeln(Filename,' Error: ',E.Message);
          end;
        end;
      end;
      AVLNode:=CfgCache.Units.Tree.FindSuccessor(AVLNode);
    end;

    Dictionary.ConsistencyCheck;
    Dictionary.SaveToFile(DictionaryFilename);

    D2:=TUnitDictionary.Create;
    D2.LoadFromFile(DictionaryFilename,false);
    D2.ConsistencyCheck;

    if not D2.Equals(Dictionary) then
      raise Exception.Create('SaveToFile/LoadFromFile difference');

    D2.Free;
  finally
    Dictionary.Free;
  end;
end.

