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
 *   Inc., 51 Franklin Street - Fifth Floor, Boston, MA 02110-1335, USA.   *
 *                                                                         *
 ***************************************************************************

  Author: Mattias Gaertner

  Abstract:
    Help for IDE windows (controls).
}
unit IDEWindowHelp;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  // LCL
  LCLProc, Controls, Dialogs, HelpIntfs,
  // LazUtils
  LazFileUtils, LazConfigStorage,
  // IdeIntf
  IDEDialogs,
  // IDE
  EnvironmentOpts, IDEOptionDefs, LazarusIDEStrConsts;
  
type

  { TIWHelpNode }
  
  TIWHelpNode = class
  private
    FIsRoot: boolean;
    FItems: TFPList;// list of TIWHelpNode
    FHasHelp: Boolean;
    FName: string;
    FParent: TIWHelpNode;
    FPath: string;
    function GetChilds(Index: integer): TIWHelpNode;
    function GetCount: integer;
    procedure SetHasHelp(const AValue: Boolean);
    procedure SetIsRoot(const AValue: boolean);
    procedure SetName(const AValue: string);
    procedure SetPath(const AValue: string);
    procedure DoRemove(AChild: TIWHelpNode);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    procedure Assign(Source: TIWHelpNode);
    function AddChild(const ChildName: string = '';
                      const ChildPath: string = ''): TIWHelpNode;
    procedure Load(Config: TConfigStorage; const CfgPath: string);
    procedure Save(Config: TConfigStorage; const CfgPath: string);
    function FindByName(const ChildName: string): TIWHelpNode;
    procedure DeleteLeavesWithoutHelp;
    function GetFullPath: string;
  public
    property HasHelp: Boolean read FHasHelp write SetHasHelp;
    property IsRoot: boolean read FIsRoot write SetIsRoot;// skip parent paths, except path of the top node
    property Name: string read FName write SetName;
    property Path: string read FPath write SetPath;
    property Parent: TIWHelpNode read FParent;
    property Count: integer read GetCount;
    property Children[Index: integer]: TIWHelpNode read GetChilds; default;
  end;
  
  { TIWHelpTree }

  TIWHelpTree = class
  private
    FRoot: TIWHelpNode;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    procedure Assign(Source: TIWHelpTree);
    procedure Load(Config: TConfigStorage; const Path: string);
    procedure Save(Config: TConfigStorage; const Path: string);
    function ControlHasValidNamePath(AControl: TControl): Boolean;
    function FindNodeForControl(AControl: TControl;
                               CreateIfNotExists: Boolean = false): TIWHelpNode;
    procedure WriteDebugReport;
    procedure DeleteLeavesWithoutHelp;
    procedure InvokeHelp(AControl: TControl);
    function CreateURL(AControl: TControl): string;
  public
    property Root: TIWHelpNode read FRoot;
  end;
  
const
  IDEWindowHelpTreeFile = 'docs/IDEWindowHelpTree.xml';

var
  IDEWindowHelpNodes: TIWHelpTree = nil;
  
function GetIDEWindowHelpFilename: string;
procedure LoadIDEWindowHelp;
procedure SaveIDEWindowHelp;

implementation

function GetIDEWindowHelpFilename: string;
begin
  Result:=AppendPathDelim(EnvironmentOptions.GetParsedLazarusDirectory)
           +GetForcedPathDelims(IDEWindowHelpTreeFile);
end;

procedure LoadIDEWindowHelp;
var
  Filename: String;
  Config: TXMLOptionsStorage;
begin
  if IDEWindowHelpNodes=nil then
    IDEWindowHelpNodes:=TIWHelpTree.Create;
  Filename:=GetIDEWindowHelpFilename;
  try
    Config:=TXMLOptionsStorage.Create(Filename,true);
    if Config=nil then exit;
    try
      IDEWindowHelpNodes.Load(Config,'');
    finally
      Config.Free;
    end;
  except
    on E: Exception do begin
      IDEMessageDialog('Read error','Error reading file '+Filename+LineEnding+E.Message,
        mtError,[mbOk]);
    end;
  end;
end;

procedure SaveIDEWindowHelp;
var
  Filename: String;
  Config: TConfigStorage;
begin
  if IDEWindowHelpNodes=nil then exit;
  Filename:=GetIDEWindowHelpFilename;
  try
    Config:=TXMLOptionsStorage.Create(Filename,false);
    if Config=nil then exit;
    try
      IDEWindowHelpNodes.Save(Config,'');
      Config.WriteToDisk;
    finally
      Config.Free;
    end;
  except
    on E: Exception do begin
      IDEMessageDialog(lisCodeToolsDefsWriteError,
        Format(lisErrorWritingFile, [Filename+LineEnding+E.Message]),
        mtError,[mbOk]);
    end;
  end;
end;

{ TIWHelpNode }

procedure TIWHelpNode.SetHasHelp(const AValue: Boolean);
begin
  if FHasHelp=AValue then exit;
  FHasHelp:=AValue;
end;

procedure TIWHelpNode.SetIsRoot(const AValue: boolean);
begin
  if FIsRoot=AValue then exit;
  FIsRoot:=AValue;
end;

procedure TIWHelpNode.SetName(const AValue: string);
begin
  if FName=AValue then exit;
  FName:=AValue;
end;

function TIWHelpNode.GetChilds(Index: integer): TIWHelpNode;
begin
  Result:=TIWHelpNode(FItems[Index]);
end;

function TIWHelpNode.GetCount: integer;
begin
  if FItems<>nil then
    Result:=FItems.Count
  else
    Result:=0;
end;

procedure TIWHelpNode.SetPath(const AValue: string);
begin
  if FPath=AValue then exit;
  FPath:=AValue;
end;

procedure TIWHelpNode.DoRemove(AChild: TIWHelpNode);
begin
  FItems.Remove(AChild);
  AChild.FParent:=nil;
end;

constructor TIWHelpNode.Create;
begin

end;

destructor TIWHelpNode.Destroy;
begin
  Clear;
  if FParent<>nil then
    FParent.DoRemove(Self);
  FreeAndNil(FItems);
  inherited Destroy;
end;

procedure TIWHelpNode.Clear;
var
  i: Integer;
  CurChild: TIWHelpNode;
begin
  if FItems<>nil then begin
    for i:=FItems.Count-1 downto 0 do begin
      CurChild:=Children[i];
      CurChild.FParent:=nil;
      CurChild.Free;
    end;
    FreeAndNil(FItems);
  end;
end;

procedure TIWHelpNode.Assign(Source: TIWHelpNode);
var
  i: Integer;
  SrcNode: TIWHelpNode;
  NewNode: TIWHelpNode;
begin
  Clear;
  Name:=Source.Name;
  Path:=Source.Path;
  HasHelp:=Source.HasHelp;
  IsRoot:=Source.IsRoot;
  for i:=0 to Source.Count-1 do begin
    SrcNode:=Source[i];
    NewNode:=AddChild;
    NewNode.Assign(SrcNode);
  end;
end;

function TIWHelpNode.AddChild(const ChildName: string;
  const ChildPath: string): TIWHelpNode;
begin
  Result:=TIWHelpNode.Create;
  Result.FParent:=Self;
  Result.Name:=ChildName;
  Result.Path:=ChildPath;
  if FItems=nil then
    FItems:=TFPList.Create;
  FItems.Add(Result);
end;

procedure TIWHelpNode.Load(Config: TConfigStorage; const CfgPath: string);
var
  NewChildCount: LongInt;
  i: Integer;
  NewChild: TIWHelpNode;
  NewName: String;
begin
  Clear;
  NewName:=Config.GetValue(CfgPath+'Name','');
  if NewName='' then exit;
  Name:=NewName;
  Path:=Config.GetValue(CfgPath+'Path','');
  HasHelp:=Config.GetValue(CfgPath+'HasHelp',false);
  IsRoot:=Config.GetValue(CfgPath+'IsRoot',false);
  NewChildCount:=Config.GetValue(CfgPath+'ChildCount',0);
  for i:=0 to NewChildCount-1 do begin
    NewChild:=AddChild('');
    NewChild.Load(Config,CfgPath+'Node'+IntToStr(i+1)+'/');
  end;
end;

procedure TIWHelpNode.Save(Config: TConfigStorage; const CfgPath: string);
var
  i: Integer;
begin
  Config.SetDeleteValue(CfgPath+'Name',Name,'');
  Config.SetDeleteValue(CfgPath+'Path',Path,'');
  Config.SetDeleteValue(CfgPath+'HasHelp',HasHelp,false);
  Config.SetDeleteValue(CfgPath+'IsRoot',IsRoot,false);
  Config.SetDeleteValue(CfgPath+'ChildCount',Count,0);
  for i:=0 to Count-1 do
    Children[i].Save(Config,CfgPath+'Node'+IntToStr(i+1)+'/');
end;

function TIWHelpNode.FindByName(const ChildName: string): TIWHelpNode;
var
  i: Integer;
begin
  for i := 0 to Count-1 do begin
    Result:=Children[i];
    if CompareText(Result.Name,ChildName)=0 then exit;
  end;
  Result:=nil;
end;

procedure TIWHelpNode.DeleteLeavesWithoutHelp;
var
  CurChild: TIWHelpNode;
  i: Integer;
begin
  for i:=Count-1 downto 0 do begin
    CurChild:=Children[i];
    CurChild.DeleteLeavesWithoutHelp;
    if (CurChild.Count=0) and (not CurChild.HasHelp) then
      CurChild.Free;
  end;
end;

function TIWHelpNode.GetFullPath: string;
var
  Node: TIWHelpNode;
  SkipTillRoot: Boolean;
begin
  Result:='';
  Node:=Self;
  SkipTillRoot:=false;
  while Node<>nil do begin
    if (Node.Parent=nil) or (not SkipTillRoot) then
      Result:=Node.Path+Result;
    if Node.IsRoot then
      SkipTillRoot:=true;
    Node:=Node.Parent;
  end;
end;

{ TIWHelpTree }

constructor TIWHelpTree.Create;
begin
  Clear;
end;

destructor TIWHelpTree.Destroy;
begin
  Clear;
  FreeAndNil(FRoot);
  inherited Destroy;
end;

procedure TIWHelpTree.Clear;
begin
  FreeAndNil(FRoot);
  FRoot:=TIWHelpNode.Create;
  Root.Name:='IDE windows and dialogs';
  Root.Path:='IDE_Window:_';
end;

procedure TIWHelpTree.Assign(Source: TIWHelpTree);
begin
  Clear;
  Root.Assign(Source.Root);
end;

procedure TIWHelpTree.Load(Config: TConfigStorage; const Path: string);
begin
  Clear;
  FRoot.Load(Config,Path);
end;

procedure TIWHelpTree.Save(Config: TConfigStorage; const Path: string);
begin
  FRoot.Save(Config,Path);
end;

function TIWHelpTree.ControlHasValidNamePath(AControl: TControl): Boolean;
begin
  if (AControl=nil) then exit(false);
  if AControl.Name='' then exit(false);
  if AControl.Parent=nil then begin
    Result:=true;
  end else begin
    Result:=ControlHasValidNamePath(AControl.Parent);
  end;
end;

function TIWHelpTree.FindNodeForControl(AControl: TControl;
  CreateIfNotExists: Boolean): TIWHelpNode;

  function Find(TheControl: TControl): TIWHelpNode;
  var
    NextParent: TWinControl;
    ParentHelpNode: TIWHelpNode;
    CurName: String;
  begin
    Result:=nil;
    //DebugLn('TIWHelpTree.FindNodeForControl.Find ',dbgsName(TheControl));
    NextParent:=TheControl.Parent;
    if NextParent=nil then begin
      CurName:=TheControl.ClassName;
      ParentHelpNode:=Root;
    end else begin
      CurName:=TheControl.Name;
      if CurName='' then exit;
      ParentHelpNode:=Find(NextParent);
      if ParentHelpNode=nil then exit;
    end;
    Result:=ParentHelpNode.FindByName(CurName);
    if (Result=nil) and CreateIfNotExists then begin
      Result:=ParentHelpNode.AddChild(CurName,CurName);
      //DebugLn('Find Create: ParentHelpNode=',ParentHelpNode.Name,' Result=',Result.Name);
    end;
  end;

begin
  Result:=Find(AControl);
end;

procedure TIWHelpTree.WriteDebugReport;

  procedure WriteNode(const Prefix: string; Node: TIWHelpNode);
  var
    i: Integer;
  begin
    if Node=nil then exit;
    DebugLn(Prefix,'Name="',Node.Name,'" Path="',Node.Path,'" HashHelp=',dbgs(Node.HasHelp));
    for i:=0 to Node.Count-1 do
      WriteNode(Prefix+'  ',Node[i]);
  end;

begin
  DebugLn('TIWHelpTree.WriteDebugReport =====================================');
  WriteNode('',Root);
end;

procedure TIWHelpTree.DeleteLeavesWithoutHelp;
begin
  Root.DeleteLeavesWithoutHelp;
end;

procedure TIWHelpTree.InvokeHelp(AControl: TControl);
var
  URL: String;
begin
  URL:=CreateURL(AControl);
  if URL='' then exit;
  ShowHelpOrError(URL,'Help for '+dbgsName(AControl),'text/html');
end;

function TIWHelpTree.CreateURL(AControl: TControl): string;
var
  HelpNode: TIWHelpNode;

  function Find(TheControl: TControl): TIWHelpNode;
  var
    NextParent: TWinControl;
    ParentHelpNode: TIWHelpNode;
    CurName: String;
  begin
    Result:=nil;
    NextParent:=TheControl.Parent;
    if NextParent=nil then begin
      CurName:=TheControl.ClassName;
      ParentHelpNode:=Root;
    end else begin
      CurName:=TheControl.Name;
      ParentHelpNode:=Find(NextParent);
      if ParentHelpNode=nil then exit;
    end;
    Result:=ParentHelpNode.FindByName(CurName);
    if (Result<>nil) and Result.HasHelp then
      HelpNode:=Result;
  end;

begin
  HelpNode:=nil;
  // search a help for this control
  Find(AControl);
  if HelpNode=nil then begin
    Result:='';
  end else begin
    Result:='http://wiki.lazarus.freepascal.org/'+HelpNode.GetFullPath;
  end;
end;

finalization
  FreeAndNil(IDEWindowHelpNodes);

end.

