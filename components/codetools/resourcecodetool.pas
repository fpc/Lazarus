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
    function FindLazarusResourceHeaderComment(ResourceCode: TCodeBuffer
          ): TAtomPosition;
    function AddLazarusResourceHeaderComment(ResourceCode: TCodeBuffer;
          const Comment: string): boolean;
    function FindLazarusResource(ResourceCode: TCodeBuffer;
          const ResourceName: string; StartPos: integer): TAtomPosition;
    function FindAllLazarusResource(ResourceCode: TCodeBuffer;
          const ResourceName: string; StartPos: integer): TAtomList;
    function AddLazarusResource(ResourceCode: TCodeBuffer;
          const ResourceName, ResourceData: string): boolean;
    function RemoveLazarusResource(ResourceCode: TCodeBuffer;
          const ResourceName: string): boolean;
    function RemoveLazarusResourceEx(ResourceCode: TCodeBuffer;
          const ResourceName: string; AllExceptFirst: boolean;
          out First: TAtomPosition): boolean;
  end;
  
  TResourceCodeToolError = class(Exception)
  end;

implementation

{ TResourceCodeTool }

procedure TResourceCodeTool.SetSource(ACode: TCodeBuffer);
begin
  ClearLastError;
  Src:=ACode.Source;
  SrcLen:=length(Src);
  CurPos:=StartAtomPosition;
  LastAtoms.Clear;
  NextPos.StartPos:=-1;
  CurNode:=nil;
  DoDeleteNodes;
end;

function TResourceCodeTool.FindLazarusResourceHeaderComment(
  ResourceCode: TCodeBuffer): TAtomPosition;
begin
  Result.StartPos:=-1;
  Result.EndPos:=-1;
  Result.Flag:=cafNone;
  SetSource(ResourceCode);

  Result.StartPos:=FindNextNonSpace(Src,1);
  if (Result.StartPos<=SrcLen) and (Src[Result.StartPos]='{') then
    Result.EndPos:=FindCommentEnd(Src,Result.StartPos,false)
  else
    Result.StartPos:=-1;
end;

function TResourceCodeTool.AddLazarusResourceHeaderComment(
  ResourceCode: TCodeBuffer; const Comment: string): boolean;
var
  InsertPos: TAtomPosition;
begin
  Result:=true;
  
  // find existing one
  InsertPos:=FindLazarusResourceHeaderComment(ResourceCode);
  if InsertPos.StartPos>0 then begin
    // there is already a comment
    // -> don't touch it
  end else
    ResourceCode.Insert(1,Comment);
end;

function TResourceCodeTool.FindLazarusResource(
  ResourceCode: TCodeBuffer; const ResourceName: string;
  StartPos: integer): TAtomPosition;
var
  ResourceNameInPascal: string;
  ResStartPos: integer;
begin
  Result.StartPos:=-1;
  Result.EndPos:=-1;
  SetSource(ResourceCode);
  if StartPos>=1 then begin
    CurPos.StartPos:=StartPos;
    CurPos.EndPos:=StartPos;
  end;

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
        // resource start found
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
        // resource end found
        Result.EndPos:=CurPos.EndPos;
        ReadNextAtom;
        if CurPos.Flag=cafSemicolon then
          Result.EndPos:=CurPos.EndPos;
        exit;
      end;
    end;
  until CurPos.StartPos>SrcLen;
end;

function TResourceCodeTool.FindAllLazarusResource(ResourceCode: TCodeBuffer;
  const ResourceName: string; StartPos: integer): TAtomList;
var
  ResourcePos: TAtomPosition;
begin
  Result:=TAtomList.Create;
  repeat
    ResourcePos:=FindLazarusResource(ResourceCode,ResourceName,StartPos);
    if ResourcePos.StartPos<1 then break;
    Result.Add(ResourcePos);
    StartPos:=ResourcePos.EndPos;
  until false;
end;

function TResourceCodeTool.AddLazarusResource(ResourceCode: TCodeBuffer;
  const ResourceName, ResourceData: string): boolean;
var
  InsertAtom: TAtomPosition;
  NeededLineEnds, i: integer;
  NewResData: string;
begin
  Result:=false;
  // try to find an old resource and delete all doubles
  Result:=RemoveLazarusResourceEx(ResourceCode,ResourceName,true,InsertAtom);
  if InsertAtom.StartPos<1 then begin
    // not found -> add at end of file
    InsertAtom.StartPos:=ResourceCode.SourceLength+1;
    InsertAtom.EndPos:=ResourceCode.SourceLength+1;
  end else begin
    InsertAtom.StartPos:=BasicCodeTools.FindLineEndOrCodeInFrontOfPosition(Src,
                                     InsertAtom.StartPos,1,false,true);
    InsertAtom.EndPos:=BasicCodeTools.FindLineEndOrCodeAfterPosition(Src,
                                     InsertAtom.EndPos,SrcLen,false);
  end;
  if CodeIsOnlySpace(Src,1,InsertAtom.StartPos-1) then
    InsertAtom.StartPos:=1;
  if CodeIsOnlySpace(Src,InsertAtom.EndPos+1,SrcLen) then
    InsertAtom.EndPos:=SrcLen+1;

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
    NewResData:=LineEnding+NewResData;
  // add start gap
  NeededLineEnds:=CountNeededLineEndsToAddBackward(ResourceData,
                                                   length(ResourceData),2);
  NeededLineEnds:=CountNeededLineEndsToAddForward(Src,InsertAtom.EndPos,
                                                  NeededLineEnds);
  for i:=1 to NeededLineEnds do
    NewResData:=NewResData+LineEnding;
  // replace
  ResourceCode.Replace(InsertAtom.StartPos,
                       InsertAtom.EndPos-InsertAtom.StartPos,
                       NewResData);
  Result:=true;
end;

function TResourceCodeTool.RemoveLazarusResource(ResourceCode: TCodeBuffer;
  const ResourceName: string): boolean;
var
  FirstResPos: TAtomPosition;
begin
  Result:=RemoveLazarusResourceEx(ResourceCode,ResourceName,false,FirstResPos);
end;

function TResourceCodeTool.RemoveLazarusResourceEx(ResourceCode: TCodeBuffer;
  const ResourceName: string; AllExceptFirst: boolean; out First: TAtomPosition
  ): boolean;
var
  ResourcePositions: TAtomList;
  CurResPos: TAtomPosition;
  i, FirstIndex: integer;
begin
  Result:=true;
  ResourcePositions:=FindAllLazarusResource(ResourceCode,ResourceName,-1);
  try
    if AllExceptFirst then
      FirstIndex:=1
    else
      FirstIndex:=0;
    for i:=ResourcePositions.Count-1 downto FirstIndex do begin
      CurResPos:=ResourcePositions[i];
      CurResPos.EndPos:=BasicCodeTools.FindLineEndOrCodeAfterPosition(Src,
                             CurResPos.EndPos,SrcLen,false);
      ResourceCode.Delete(CurResPos.StartPos,
                          CurResPos.EndPos-CurResPos.StartPos);
    end;
    if ResourcePositions.Count>0 then begin
      First:=ResourcePositions[0];
    end else begin
      First.StartPos:=-1;
      First.EndPos:=-1;
    end;
  finally
    ResourcePositions.Free;
  end;
end;

end.

