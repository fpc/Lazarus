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
    The TResourceCodeTool provides functions to find, add and delete resources
    in resource files.
}
unit ResourceCodeTool;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, KeywordFuncLists, MultiKeyWordListTool, CodeCache,
  CodeAtom, BasicCodeTools;

type
  TResourceCodeTool = class(TMultiKeyWordListCodeTool)
  protected
    procedure SetSource(ACode: TCodeBuffer);
  public
    // lazarus resources
    function FindLazarusResource(ResourceCode: TCodeBuffer;
          const ResourceName: string): TAtomPosition;
    function AddLazarusResource(ResourceCode: TCodeBuffer;
          const ResourceName, ResourceData: string): boolean;
    function RemoveLazarusResource(ResourceCode: TCodeBuffer;
          const ResourceName: string): boolean;
  end;
  
  TResourceCodeToolError = class(Exception)
  end;

implementation

{ TResourceCodeTool }

procedure TResourceCodeTool.SetSource(ACode: TCodeBuffer);
begin
  ClearLastError;
  Src:=ACode.Source;
  UpperSrc:=UpperCaseStr(Src);
  SrcLen:=length(Src);
  CurPos:=StartAtomPosition;
  LastAtoms.Clear;
  NextPos.StartPos:=-1;
  CurNode:=nil;
  DoDeleteNodes;
end;

function TResourceCodeTool.FindLazarusResource(
  ResourceCode: TCodeBuffer; const ResourceName: string): TAtomPosition;
var
  ResourceNameInPascal: string;
  ResStartPos: integer;
begin
  Result.StartPos:=-1;
  Result.EndPos:=-1;
  SetSource(ResourceCode);
  
  // search "LAZARUSRESOURCES.ADD('ResourceName',"
  ResourceNameInPascal:=''''+UpperCaseStr(ResourceName)+'''';
  repeat
    ReadNextAtom;
    if UpAtomIs('LAZARUSRESOURCES') then begin
      ResStartPos:=CurPos.StartPos;
      ReadNextAtom;
      if CurPos.Flag<>cafPoint then continue;
      ReadNextAtom;
      if not UpAtomIs('ADD') then continue;
      ReadNextAtom;
      if CurPos.Flag<>cafRoundBracketOpen then continue;
      ReadNextAtom;
      if UpAtomIs(ResourceNameInPascal) then begin
        // resource found
        Result.StartPos:=ResStartPos;
      end;
      UndoReadNextAtom;
      ReadTilBracketClose(false);
      if CurPos.Flag<>cafRoundBracketClose then begin
        // syntax error
        Result.StartPos:=-1;
        exit;
      end;
      if (Result.StartPos>0) then begin
        Result.EndPos:=CurPos.EndPos;
        ReadNextAtom;
        if CurPos.Flag=cafSemicolon then
          Result.EndPos:=CurPos.EndPos;
        exit;
      end;
    end;
  until CurPos.StartPos>SrcLen;
end;

function TResourceCodeTool.AddLazarusResource(ResourceCode: TCodeBuffer;
  const ResourceName, ResourceData: string): boolean;
var
  InsertAtom: TAtomPosition;
  NeededLineEnds, i: integer;
  NewResData: string;
begin
  Result:=false;
  // first try to find an old resource
  InsertAtom:=FindLazarusResource(ResourceCode,ResourceName);
  if InsertAtom.StartPos<1 then begin
    // not found -> add at end of file
    InsertAtom.StartPos:=ResourceCode.SourceLength+1;
    InsertAtom.EndPos:=ResourceCode.SourceLength+1;
  end;
  InsertAtom.StartPos:=BasicCodeTools.FindLineEndOrCodeInFrontOfPosition(Src,
                                   InsertAtom.StartPos,1,false,true)+1;
  InsertAtom.EndPos:=BasicCodeTools.FindLineEndOrCodeAfterPosition(Src,
                                   InsertAtom.EndPos,SrcLen,false);
  NewResData:=ResourceData;
  i:=length(NewResData);
  while (i>1) and (NewResData[i] in [' ',#10,#13]) do
    dec(i);
  SetLength(NewResData,i);
  // add front gap
  NeededLineEnds:=CountNeededLineEndsToAddForward(ResourceData,1,2);
  NeededLineEnds:=CountNeededLineEndsToAddBackward(Src,InsertAtom.StartPos-1,
                                                   NeededLineEnds);
  for i:=1 to NeededLineEnds do
    NewResData:=EndOfLine+NewResData;
  // add start gap
  NeededLineEnds:=CountNeededLineEndsToAddBackward(ResourceData,
                                                   length(ResourceData),2);
  NeededLineEnds:=CountNeededLineEndsToAddForward(Src,InsertAtom.EndPos,
                                                  NeededLineEnds);
  for i:=1 to NeededLineEnds do
    NewResData:=NewResData+EndOfLine;
  // replace
  ResourceCode.Replace(InsertAtom.StartPos,
                       InsertAtom.EndPos-InsertAtom.StartPos,
                       NewResData);
  Result:=true;
end;

function TResourceCodeTool.RemoveLazarusResource(ResourceCode: TCodeBuffer;
  const ResourceName: string): boolean;
var OldAtom: TAtomPosition;
begin
  Result:=true;
  OldAtom:=FindLazarusResource(ResourceCode,ResourceName);
  if (OldAtom.StartPos<1) then exit;
  OldAtom.EndPos:=BasicCodeTools.FindLineEndOrCodeAfterPosition(Src,
                         OldAtom.EndPos,SrcLen,false);
  ResourceCode.Delete(OldAtom.StartPos,OldAtom.EndPos);
end;

end.

