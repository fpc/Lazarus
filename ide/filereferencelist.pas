{  $Id$  }
{
 /***************************************************************************
                            filereferencelist.pas
                            ---------------------


 ***************************************************************************/

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
    TFileReferenceList stores reference counters for a set of filenames and
    can create a search path of all files.
}
unit FileReferenceList;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, AVL_Tree, FileCtrl;
  
type
  { TFileReference }
  
  TFileReference = class
  private
    fFilename: string;
    fReferenceCount: integer;
  public
    property Filename: string read fFilename;
    property ReferenceCount: integer read FReferenceCount;
  end;

  
  { TFileReferenceList }
  
  TFileReferenceFlag = (
    frfSearchPathValid
    );
  TFileReferenceFlags = set of TFileReferenceFlag;
  
  TFileReferenceList = class
  private
    FTree: TAVLTree; // tree of TFileReference sorted for filename
    FFlags: TFileReferenceFlags;
    FSearchPath: string;
    procedure UpdateSearchPath;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    procedure AddFilename(const Filename: string);
    procedure RemoveFilename(const Filename: string);
    function GetFileReference(const Filename: string): TFileReference;
    function CreateSearchPathFromAllFiles: string;
  end;

implementation

function CompareFileReferences(Data1, Data2: Pointer): integer;
var
  FileRef1: TFileReference;
  FileRef2: TFileReference;
begin
  FileRef1:=TFileReference(Data1);
  FileRef2:=TFileReference(Data2);
  Result:=CompareFilenames(FileRef1.Filename,FileRef2.Filename);
end;

function CompareFileNameAndReference(Key, Data: Pointer): integer;
var
  Filename: String;
  FileRef: TFileReference;
begin
  Filename:=String(Key);
  FileRef:=TFileReference(Data);
  Result:=CompareFilenames(Filename,FileRef.Filename);
end;

{ TFileReferenceList }

procedure TFileReferenceList.UpdateSearchPath;
var
  SearchPathLen: Integer;
  ANode: TAVLTreeNode;
  StartPos: Integer;
  CurFileLen: Integer;
  CurFileName: String;
begin
  if (frfSearchPathValid in FFlags) then exit;
  
  FSearchPath:='';
  if FTree<>nil then begin
    // count length
    SearchPathLen:=0;
    ANode:=FTree.FindLowest;
    while ANode<>nil do begin
      CurFileName:=TFileReference(ANode.Data).Filename;
      CurFileLen:=length(CurFileName);
      if CurFileLen>0 then begin
        // add semicolon
        if SearchPathLen>0 then inc(SearchPathLen);
        // add path
        inc(SearchPathLen,CurFileLen);
      end;
      ANode:=FTree.FindSuccessor(ANode);
    end;
    // create search path
    SetLength(FSearchPath,SearchPathLen);
    StartPos:=1;
    ANode:=FTree.FindLowest;
    while ANode<>nil do begin
      CurFileName:=TFileReference(ANode.Data).Filename;
      CurFileLen:=length(CurFileName);
      if CurFileLen>0 then begin
        // add semicolon
        FSearchPath[StartPos]:=';';
        inc(StartPos);
        // add path
        Move(CurFileName[1],FSearchPath[StartPos],CurFileLen);
        inc(StartPos,CurFileLen);
      end;
      ANode:=FTree.FindSuccessor(ANode);
    end;
  end;
  Include(FFlags,frfSearchPathValid);
end;

constructor TFileReferenceList.Create;
begin

end;

destructor TFileReferenceList.Destroy;
begin
  Clear;
  FTree.Free;
  FTree:=nil;
  inherited Destroy;
end;

procedure TFileReferenceList.Clear;
begin
  if (FTree<>nil) and (FTree.Count>0) then begin
    FTree.FreeAndClear;
    Exclude(FFlags,frfSearchPathValid);
  end;
end;

procedure TFileReferenceList.AddFilename(const Filename: string);
var
  ANode: TAVLTreeNode;
  NewFileRef: TFileReference;
begin
  if Filename='' then exit;
  if FTree<>nil then begin
    ANode:=FTree.FindKey(PChar(Filename),@CompareFileNameAndReference);
    if ANode<>nil then begin
      inc(TFileReference(ANode.Data).fReferenceCount);
      exit;
    end;
  end;
  NewFileRef:=TFileReference.Create;
  NewFileRef.fFilename:=Filename;
  inc(NewFileRef.fReferenceCount);
  if FTree=nil then FTree:=TAVLTree.Create(@CompareFileReferences);
  FTree.Add(NewFileRef);
  Exclude(FFlags,frfSearchPathValid);
end;

procedure TFileReferenceList.RemoveFilename(const Filename: string);
var
  ANode: TAVLTreeNode;
  CurFileRef: TFileReference;
begin
  if Filename='' then exit;
  if FTree=nil then exit;
  ANode:=FTree.FindKey(PChar(Filename),@CompareFileNameAndReference);
  if ANode=nil then exit;
  CurFileRef:=TFileReference(ANode.Data);
  dec(CurFileRef.fReferenceCount);
  if CurFileRef.fReferenceCount=0 then begin
    FTree.Remove(CurFileRef);
    CurFileRef.Free;
    Exclude(FFlags,frfSearchPathValid);
  end;
end;

function TFileReferenceList.GetFileReference(const Filename: string
  ): TFileReference;
var
  ANode: TAVLTreeNode;
begin
  Result:=nil;
  if FTree=nil then exit;
  ANode:=FTree.FindKey(PChar(Filename),@CompareFileNameAndReference);
  if ANode=nil then exit;
  Result:=TFileReference(ANode.Data);
end;

function TFileReferenceList.CreateSearchPathFromAllFiles: string;
begin
  UpdateSearchPath;
  Result:=FSearchPath;
end;

end.

