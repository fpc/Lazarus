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
    Help for IDE windows (controls).
}
unit IDEWindowHelp;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Dialogs, HelpIntf, ConfigStorage,
  EnvironmentOpts;
  
type

  { TIWHelpNode }
  
  TIWHelpNode = class
  private
    FItems: TFPList;// list of TIWHelpNode
    FHasHelp: Boolean;
    FName: string;
    FParent: TIWHelpNode;
    FPath: string;
    function GetChilds(Index: integer): TIWHelpNode;
    function GetCount: integer;
    procedure SetHasHelp(const AValue: Boolean);
    procedure SetName(const AValue: string);
    procedure SetPath(const AValue: string);
    procedure DoRemove(AChild: TIWHelpNode);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    function AddChild(const ChildName: string = '';
                      const ChildPath: string = ''): TIWHelpNode;
    procedure Load(Config: TConfigStorage; const CfgPath: string);
    procedure Save(Config: TConfigStorage; const CfgPath: string);
    property HasHelp: Boolean read FHasHelp write SetHasHelp;
    property Name: string read FName write SetName;
    property Path: string read FPath write SetPath;
    property Parent: TIWHelpNode read FParent;
    property Count: integer read GetCount;
    property Childs[Index: integer]: TIWHelpNode read GetChilds; default;
  end;
  
  { TIWHelpTree }

  TIWHelpTree = class
  private
    FRoot: TIWHelpNode;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Load(Config: TConfigStorage; const Path: string);
    procedure Save(Config: TConfigStorage; const Path: string);
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
  Result:=AppendPathDelim(EnvironmentOptions.LazarusDirectory)
           +SetDirSeparators(IDEWindowHelpTreeFile);
end;

procedure LoadIDEWindowHelp;
var
  Filename: String;
  Config: TConfigStorage;
begin
  if IDEWindowHelpNodes=nil then
    IDEWindowHelpNodes:=TIWHelpTree.Create;
  Filename:=GetIDEWindowHelpFilename;
  try
    Config:=GetIDEConfigStorage(Filename,true);
    if Config=nil then exit;
    try
      IDEWindowHelpNodes.Load(Config,'');
    finally
      Config.Free;
    end;
  except
    on E: Exception do begin
      MessageDlg('Read error','Error reading file '+Filename+#13+E.Message,
        mtError,[mbOk],0);
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
    Config:=GetIDEConfigStorage(Filename,false);
    if Config=nil then exit;
    try
      IDEWindowHelpNodes.Save(Config,'');
      Config.WriteToDisk;
    finally
      Config.Free;
    end;
  except
    on E: Exception do begin
      MessageDlg('Write error','Error writing file '+Filename+#13+E.Message,
        mtError,[mbOk],0);
    end;
  end;
end;

{ TIWHelpNode }

procedure TIWHelpNode.SetHasHelp(const AValue: Boolean);
begin
  if FHasHelp=AValue then exit;
  FHasHelp:=AValue;
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
      CurChild:=Childs[i];
      CurChild.FParent:=nil;
      CurChild.Free;
    end;
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
begin
  Clear;
  Name:=Config.GetValue(CfgPath+'Name','');
  Path:=Config.GetValue(CfgPath+'Path','');
  HasHelp:=Config.GetValue(CfgPath+'HasHelp',true);
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
  Config.SetDeleteValue(CfgPath+'HasHelp',HasHelp,true);
  Config.SetDeleteValue(CfgPath+'ChildCount',Count,0);
  for i:=0 to Count-1 do
    Childs[i].Save(Config,CfgPath+'Node'+IntToStr(i+1)+'/');
end;

{ TIWHelpTree }

constructor TIWHelpTree.Create;
begin
  FRoot:=TIWHelpNode.Create;
  FRoot.Name:='IDE windows and dialogs';
  FRoot.Path:='IDEWindowsAndDialogs';
end;

destructor TIWHelpTree.Destroy;
begin
  inherited Destroy;
end;

procedure TIWHelpTree.Load(Config: TConfigStorage; const Path: string);
begin
  FRoot.Load(Config,Path);
end;

procedure TIWHelpTree.Save(Config: TConfigStorage; const Path: string);
begin
  FRoot.Save(Config,Path);
end;

end.

