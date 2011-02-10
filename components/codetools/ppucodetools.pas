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
    Tools to handle ppu files.
}
unit PPUCodeTools;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, PPUParser, CodeCache, AVL_Tree, FileProcs;

type

  { TPPUTool }

  TPPUTool = class
  public
    PPU: TPPU;
    Code: TCodeBuffer;
    CodeChangeStep: integer;
    ErrorMsg: string;
    constructor Create(aCode: TCodeBuffer);
    destructor Destroy; override;
  end;

  { TPPUTools }

  TPPUTools = class
  private
    fItems: TAVLTree; // tree of TPPUTool sorted for Code
    FSourceCache: TCodeCache;
  public
    constructor Create(SrcCache: TCodeCache);
    destructor Destroy; override;
    procedure ClearCaches;

    property SourceCache: TCodeCache read FSourceCache;
    function FindFile(Code: TCodeBuffer): TPPUTool;
    function FindFile(const ExpandedFilename: string): TPPUTool;
    function LoadFile(const ExpandedFilename: string;
                      UpdateFromDisk, Revert: boolean): TPPUTool;

    // uses section
    procedure GetMainUsesSectionNames(Code: TCodeBuffer; var List: TStrings);
    procedure GetImplementationUsesSectionNames(Code: TCodeBuffer; var List: TStrings);
  end;

function ComparePPUTools(Tool1, Tool2: Pointer): integer;
function CompareCodeWithPPUTool(Code, Tool: Pointer): integer;

implementation

function ComparePPUTools(Tool1, Tool2: Pointer): integer;
begin
  Result:=ComparePointers(TPPUTool(Tool1).Code,TPPUTool(Tool2).Code);
end;

function CompareCodeWithPPUTool(Code, Tool: Pointer): integer;
begin
  Result:=ComparePointers(Code,TPPUTool(Tool).Code);
end;

{ TPPUTools }

constructor TPPUTools.Create(SrcCache: TCodeCache);
begin
  FSourceCache:=SrcCache;
  fItems:=TAVLTree.Create(@ComparePPUTools);
end;

destructor TPPUTools.Destroy;
begin
  fItems.FreeAndClear;
  FreeAndNil(fItems);
  FSourceCache:=nil;
  inherited Destroy;
end;

procedure TPPUTools.ClearCaches;
var
  Node: TAVLTreeNode;
  Tool: TPPUTool;
begin
  Node:=fItems.FindLowest;
  while Node<>nil do begin
    Tool:=TPPUTool(Node.Data);
    FreeAndNil(Tool.PPU);
    Tool.ErrorMsg:='';
    Node:=fItems.FindSuccessor(Node);
  end;
end;

function TPPUTools.FindFile(Code: TCodeBuffer): TPPUTool;
var
  Node: TAVLTreeNode;
begin
  Result:=nil;
  if Code=nil then exit;
  Node:=fItems.FindKey(Code,@CompareCodeWithPPUTool);
  if Node<>nil then
    Result:=TPPUTool(Node.Data);
end;

function TPPUTools.FindFile(const ExpandedFilename: string): TPPUTool;
var
  Code: TCodeBuffer;
begin
  Code:=SourceCache.FindFile(ExpandedFilename);
  if Code<>nil then
    Result:=FindFile(Code)
  else
    Result:=nil;
end;

function TPPUTools.LoadFile(const ExpandedFilename: string; UpdateFromDisk,
  Revert: boolean): TPPUTool;
var
  Code: TCodeBuffer;
  ss: TStringStream;
begin
  Result:=FindFile(ExpandedFilename);
  if (not UpdateFromDisk) and (not Revert) then begin
    // no update needed
    if Result<>nil then exit;
    Code:=SourceCache.FindFile(ExpandedFilename);
    if (Code=nil) or Code.IsDeleted then exit(nil);
  end;

  // load file
  Code:=SourceCache.LoadFile(ExpandedFilename);
  if Code=nil then exit(nil);
  if Revert then begin
    if not Code.Revert then
      exit(nil);
  end else if UpdateFromDisk and Code.AutoRevertFromDisk
  and Code.FileNeedsUpdate then begin
    //debugln(['TPPUTools.LoadFile ',ExpandedFilename,' AutoRevert=',Result.AutoRevertFromDisk,' Modified=',Result.Modified,' NeedLoad=',Result.FileNeedsUpdate]);
    Code.Reload;
  end;

  // check if tool needs update
  if Result=nil then begin
    Result:=TPPUTool.Create(Code);
    fItems.Add(Result);
  end;
  Result.Code:=Code;
  if (Result.PPU<>nil) and (Result.CodeChangeStep=Code.ChangeStep) then
    exit;
  //debugln(['TPPUTools.LoadFile parsing ppu ',Code.Filename,' ...']);
  Result.ErrorMsg:='';
  if Result.PPU=nil then
    Result.PPU:=TPPU.Create;
  ss:=TStringStream.Create(Code.Source);
  try
    try
      Result.PPU.LoadFromStream(ss);
    except
      on E: Exception do begin
        Result.ErrorMsg:=E.Message;
        debugln(['TPPUTools.LoadFile ',Code.Filename,' ERROR: ', Result.ErrorMsg]);
      end;
    end;
  finally
    ss.Free;
  end;
end;

procedure TPPUTools.GetMainUsesSectionNames(Code: TCodeBuffer;
  var List: TStrings);
begin

end;

procedure TPPUTools.GetImplementationUsesSectionNames(Code: TCodeBuffer;
  var List: TStrings);
begin

end;

{ TPPUTool }

constructor TPPUTool.Create(aCode: TCodeBuffer);
begin
  Code:=aCode;
  CodeChangeStep:=Code.ChangeStep;
end;

destructor TPPUTool.Destroy;
begin
  FreeAndNil(PPU);
  inherited Destroy;
end;

end.

