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
    Filename: string;
    LoadDate: longint;
    LoadedParts: TPPUParts;
    ErrorMsg: string;
    constructor Create(aFilename: string);
    destructor Destroy; override;
    function FileDateOnDisk: longint;
    function NeedsUpdate(const Parts: TPPUParts = PPUPartsAll): boolean;
    function Load(const Parts: TPPUParts = PPUPartsAll): boolean;
    procedure Clear;
  end;

  { TPPUTools }

  TPPUTools = class
  private
    fItems: TAVLTree; // tree of TPPUTool sorted for Code
  public
    constructor Create;
    destructor Destroy; override;
    procedure ClearCaches;

    function FindFile(const NormalizedFilename: string): TPPUTool;
    function LoadFile(const NormalizedFilename: string;
                      const Parts: TPPUParts = PPUPartsAll): TPPUTool;

    // uses section
    function GetMainUsesSectionNames(NormalizedFilename: string; var List: TStrings): boolean;
    function GetImplementationUsesSectionNames(NormalizedFilename: string; var List: TStrings): boolean;
  end;

function ComparePPUTools(Tool1, Tool2: Pointer): integer;
function CompareFilenameWithPPUTool(Filename, Tool: Pointer): integer;

implementation

function ComparePPUTools(Tool1, Tool2: Pointer): integer;
begin
  Result:=CompareFilenames(TPPUTool(Tool1).Filename,TPPUTool(Tool2).Filename);
end;

function CompareFilenameWithPPUTool(Filename, Tool: Pointer): integer;
begin
  Result:=CompareFilenames(AnsiString(Filename),TPPUTool(Tool).Filename);
end;

{ TPPUTools }

constructor TPPUTools.Create;
begin
  fItems:=TAVLTree.Create(@ComparePPUTools);
end;

destructor TPPUTools.Destroy;
begin
  fItems.FreeAndClear;
  FreeAndNil(fItems);
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

function TPPUTools.FindFile(const NormalizedFilename: string): TPPUTool;
var
  Node: TAVLTreeNode;
begin
  Node:=fItems.FindKey(Pointer(NormalizedFilename),@CompareFilenameWithPPUTool);
  if Node<>nil then
    Result:=TPPUTool(Node.Data)
  else
    Result:=nil;
end;

function TPPUTools.LoadFile(const NormalizedFilename: string;
  const Parts: TPPUParts): TPPUTool;
var
  Tool: TPPUTool;
begin
  Result:=nil;
  if Parts=[] then exit;
  Tool:=FindFile(NormalizedFilename);
  if Tool=nil then begin
    Tool:=TPPUTool.Create(NormalizedFilename);
    fItems.Add(Tool);
  end;
  if not Tool.NeedsUpdate(Parts) then exit;
  if not Tool.Load(Parts) then exit;
  Result:=Tool;
end;

function TPPUTools.GetMainUsesSectionNames(NormalizedFilename: string;
  var List: TStrings): boolean;
var
  Tool: TPPUTool;
begin
  Result:=false;
  Tool:=LoadFile(NormalizedFilename,[ppInterfaceHeader]);
  if Tool=nil then exit;
  Tool.PPU.GetMainUsesSectionNames(List);
  Result:=true;
end;

function TPPUTools.GetImplementationUsesSectionNames(
  NormalizedFilename: string; var List: TStrings): boolean;
var
  Tool: TPPUTool;
begin
  Result:=false;
  Tool:=LoadFile(NormalizedFilename,[ppImplementationHeader]);
  if Tool=nil then exit;
  Tool.PPU.GetImplementationUsesSectionNames(List);
  Result:=true;
end;

{ TPPUTool }

constructor TPPUTool.Create(aFilename: string);
begin
  Filename:=aFilename;
end;

destructor TPPUTool.Destroy;
begin
  FreeAndNil(PPU);
  inherited Destroy;
end;

function TPPUTool.FileDateOnDisk: longint;
begin
  Result:=FileAgeCached(Filename);
end;

function TPPUTool.NeedsUpdate(const Parts: TPPUParts): boolean;
begin
  Result:=(Parts-LoadedParts<>[]) or (FileDateOnDisk<>LoadDate);
end;

function TPPUTool.Load(const Parts: TPPUParts): boolean;
begin
  Result:=false;
  ErrorMsg:='';
  if PPU=nil then
    PPU:=TPPU.Create;
  try
    LoadDate:=FileDateOnDisk;
    LoadedParts:=Parts;
    PPU.LoadFromFile(Filename,Parts);
    Result:=true;
  except
    on E: Exception do begin
      ErrorMsg:=E.Message;
      debugln(['TPPUTool.Load ',Filename,' ERROR: ',ErrorMsg]);
    end;
  end;
end;

procedure TPPUTool.Clear;
begin
  FreeAndNil(PPU);
  LoadedParts:=[];
end;

end.

