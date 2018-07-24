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
 *   Inc., 51 Franklin Street - Fifth Floor, Boston, MA 02110-1335, USA.   *
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
  Classes, SysUtils, Laz_AVL_Tree,
  // Codetools
  FileProcs,
  // LazUtils
  LazFileUtils, LazTracer;

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
    frfSearchPathValid,
    frfChanged
    );
  TFileReferenceFlags = set of TFileReferenceFlag;
  
  TFileReferenceList = class
  private
    FOnChanged: TNotifyEvent;
    FTimeStamp: integer;
    FTree: TAvlTree; // tree of TFileReference sorted for filename
    FFlags: TFileReferenceFlags;
    FSearchPath: string;
    FUpdateLock: integer;
    procedure UpdateSearchPath;
    procedure IncreaseTimeStamp;
    procedure Invalidate;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    procedure BeginUpdate;
    procedure EndUpdate;
    procedure AddFilename(const Filename: string);
    procedure RemoveFilename(const Filename: string);
    function GetFileReference(const Filename: string): TFileReference;
    function CreateSearchPathFromAllFiles: string;
    function CreateFileList: TStringList;
    property TimeStamp: integer read FTimeStamp;
    property OnChanged: TNotifyEvent read FOnChanged write FOnChanged;
    property UpdateLock: integer read FUpdateLock;
  end;
  
implementation

function CompareFileReferences(Data1, Data2: Pointer): integer;
var
  FileRef1: TFileReference absolute Data1;
  FileRef2: TFileReference absolute Data2;
begin
  Result:=CompareFilenames(FileRef1.Filename,FileRef2.Filename);
end;

function CompareFileNameAndReference(Key, Data: Pointer): integer;
var
  FileRef: TFileReference absolute Data;
begin
  Result:=CompareFilenames(AnsiString(Key),FileRef.Filename);
end;

{ TFileReferenceList }

procedure TFileReferenceList.UpdateSearchPath;
var
  SearchPathLen: Integer;
  ANode: TAvlTreeNode;
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
        if StartPos>1 then begin
          FSearchPath[StartPos]:=';';
          inc(StartPos);
        end;
        // add path
        Move(CurFileName[1],FSearchPath[StartPos],CurFileLen);
        inc(StartPos,CurFileLen);
      end;
      ANode:=FTree.FindSuccessor(ANode);
    end;
    if StartPos<>length(FSearchPath)+1 then
      RaiseGDBException('TFileReferenceList.UpdateSearchPath');
  end;
  Include(FFlags,frfSearchPathValid);
end;

procedure TFileReferenceList.IncreaseTimeStamp;
begin
  CTIncreaseChangeStamp(FTimeStamp);
  //DebugLn('TFileReferenceList.IncreaseTimeStamp ',dbgs(FTimeStamp));
end;

procedure TFileReferenceList.Invalidate;
begin
  IncreaseTimeStamp;
  Exclude(FFlags,frfSearchPathValid);
  if FUpdateLock>0 then
    Include(FFlags,frfChanged)
  else if Assigned(OnChanged) then
    OnChanged(Self);
end;

constructor TFileReferenceList.Create;
begin
  FTimeStamp:=CTInvalidChangeStamp;
end;

destructor TFileReferenceList.Destroy;
begin
  Clear;
  FreeAndNil(FTree);
  inherited Destroy;
end;

procedure TFileReferenceList.Clear;
begin
  if (FTree<>nil) and (FTree.Count>0) then begin
    FTree.FreeAndClear;
    Invalidate;
  end;
end;

procedure TFileReferenceList.BeginUpdate;
begin
  inc(FUpdateLock);
end;

procedure TFileReferenceList.EndUpdate;
begin
  if FUpdateLock<=0 then RaiseGDBException('TFileReferenceList.EndUpdate');
  dec(FUpdateLock);
  if (frfChanged in FFlags) then begin
    Exclude(FFlags,frfChanged);
    if Assigned(OnChanged) then
      OnChanged(Self);
  end;
end;

procedure TFileReferenceList.AddFilename(const Filename: string);
var
  ANode: TAvlTreeNode;
  NewFileRef: TFileReference;
begin
  if Filename='' then exit;
  if FTree<>nil then begin
    ANode:=FTree.FindKey(Pointer(Filename),@CompareFileNameAndReference);
    if ANode<>nil then begin
      inc(TFileReference(ANode.Data).fReferenceCount);
      exit;
    end;
  end;
  NewFileRef:=TFileReference.Create;
  NewFileRef.fFilename:=Filename;
  inc(NewFileRef.fReferenceCount);
  if FTree=nil then FTree:=TAvlTree.Create(@CompareFileReferences);
  FTree.Add(NewFileRef);
  Invalidate;
end;

procedure TFileReferenceList.RemoveFilename(const Filename: string);
var
  ANode: TAvlTreeNode;
  CurFileRef: TFileReference;
begin
  if Filename='' then exit;
  if FTree=nil then exit;
  ANode:=FTree.FindKey(Pointer(Filename),@CompareFileNameAndReference);
  if ANode=nil then exit;
  CurFileRef:=TFileReference(ANode.Data);
  dec(CurFileRef.fReferenceCount);
  if CurFileRef.fReferenceCount=0 then begin
    FTree.Delete(ANode);
    CurFileRef.Free;
    Invalidate;
  end;
end;

function TFileReferenceList.GetFileReference(const Filename: string): TFileReference;
var
  ANode: TAvlTreeNode;
begin
  Result:=nil;
  if FTree=nil then exit;
  ANode:=FTree.FindKey(Pointer(Filename),@CompareFileNameAndReference);
  if ANode=nil then exit;
  Result:=TFileReference(ANode.Data);
end;

function TFileReferenceList.CreateSearchPathFromAllFiles: string;
begin
  UpdateSearchPath;
  Result:=FSearchPath;
end;

function TFileReferenceList.CreateFileList: TStringList;
var
  ANode: TAvlTreeNode;
begin
  Result:=TStringList.Create;
  if FTree=nil then exit;
  ANode:=FTree.FindLowest;
  while ANode<>nil do begin
    Result.Add(TFileReference(ANode.Data).Filename);
    ANode:=FTree.FindSuccessor(ANode);
  end;
end;

end.

