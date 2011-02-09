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
    An IDE dialog showing all used ppus of a project.
}
unit PPUListDlg;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LCLProc, LResources, Forms, Controls, Graphics, Dialogs,
  ButtonPanel, Grids, StdCtrls, AvgLvlTree,
  // IDEIntf
  IDECommands, MenuIntf, ProjectIntf, LazIDEIntf, IDEDialogs, IDEWindowIntf,
  // codetools
  BasicCodeTools, FileProcs, CodyStrConsts;

const
  PPUFileNotFound = ' ';
type
  TPPUListType = (
    pltUsedBy,
    pltUses
    );

  { TPPUListItem }

  TPPUListItem = class
  public
    TheUnitName: string;
    SrcFile: string;
    PPUFile: string; // = '' means not searched, = PPUFileNotFound means not found
    OFile: string;
    PPUFileSize: int64;
    OFileSize: int64;
    First, Last, Next, Prev: array[TPPUListType] of TPPUListItem;
    procedure AppendToList(var aFirst, aLast: TPPUListItem; ListType: TPPUListType);
    procedure RemoveFromList(var aFirst, aLast: TPPUListItem; ListType: TPPUListType);
  end;

  { TPPUListDialog }

  TPPUListDialog = class(TForm)
    ButtonPanel1: TButtonPanel;
    ScopeLabel: TLabel;
    UnitsStringGrid: TStringGrid;
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    FAProject: TLazProject;
    FIdleConnected: boolean;
    FSearchingItems: TAvgLvlTree; // tree of TPPUListItem sorted for TheUnitName
    FItems: TAvgLvlTree; // tree of TPPUListItem sorted for TheUnitName
    procedure SetAProject(const AValue: TLazProject);
    procedure SetIdleConnected(const AValue: boolean);
    procedure OnIdle(Sender: TObject; var Done: Boolean);
    procedure UpdateAll;
    procedure UpdateUnitsGrid;
  public
    property AProject: TLazProject read FAProject write SetAProject;
    property IdleConnected: boolean read FIdleConnected write SetIdleConnected;
  end;

procedure ShowPPUList(Sender: TObject);
function ComparePPUListItems(Item1, Item2: Pointer): integer;
function CompareUnitNameWithPPUListItem(TheUnitName, Item: Pointer): integer;

procedure Register;

implementation

procedure ShowPPUList(Sender: TObject);
var
  Dlg: TPPUListDialog;
begin
  if LazarusIDE.ActiveProject=nil then begin
    IDEMessageDialog(crsNoProject, crsPleaseOpenAProjectFirst, mtError, [mbCancel]);
    exit;
  end;
  Dlg:=TPPUListDialog.Create(nil);
  try
    Dlg.AProject:=LazarusIDE.ActiveProject;
    Dlg.ShowModal;
  finally
    Dlg.Free;
  end;
end;

function ComparePPUListItems(Item1, Item2: Pointer): integer;
var
  li1: TPPUListItem absolute Item1;
  li2: TPPUListItem absolute Item2;
begin
  Result:=CompareIdentifiers(PChar(li1.TheUnitName),PChar(li2.TheUnitName));
end;

function CompareUnitNameWithPPUListItem(TheUnitName, Item: Pointer): integer;
var
  li: TPPUListItem absolute Item;
  un: PChar;
begin
  un:=PChar(AnsiString(TheUnitName));
  Result:=CompareIdentifiers(un,PChar(li.TheUnitName));
end;

procedure Register;
var
  CmdCategory: TIDECommandCategory;
  PPUListCommand: TIDECommand;
begin
  CmdCategory:=IDECommandList.FindCategoryByName('ProjectMenu');
  if CmdCategory=nil then
    raise Exception.Create('cody: PPUListDlg.Register: command category ProjectMenu not found');
  PPUListCommand:=RegisterIDECommand(CmdCategory,'ShowPPUList','Show used .ppu files',
    CleanIDEShortCut,CleanIDEShortCut,nil,@ShowPPUList);
  RegisterIDEMenuCommand(itmProjectWindowSection,'PPUList','Show used .ppu files',
    nil,nil,PPUListCommand);
end;

{ TPPUListItem }

procedure TPPUListItem.AppendToList(var aFirst, aLast: TPPUListItem;
  ListType: TPPUListType);
begin
  if (Next[ListType]<>nil) or (Prev[ListType]<>nil) then
    RaiseCatchableException('TPPUListItem.AppendToList');
  if aFirst=nil then
    aFirst:=Self;
  Prev[ListType]:=aLast;
  if Prev[ListType]<>nil then Prev[ListType].Next[ListType]:=Self;
  aLast:=Self;
end;

procedure TPPUListItem.RemoveFromList(var aFirst, aLast: TPPUListItem;
  ListType: TPPUListType);
begin
  if aFirst=Self then aFirst:=Next[ListType];
  if aLast=Self then aLast:=Prev[ListType];
  if Prev[ListType]<>nil then Prev[ListType].Next[ListType]:=Next[ListType];
  if Next[ListType]<>nil then Next[ListType].Prev[ListType]:=Prev[ListType];
  Prev[ListType]:=nil;
  Next[ListType]:=nil;
end;

{ TPPUListDialog }

procedure TPPUListDialog.FormCreate(Sender: TObject);
begin
  FSearchingItems:=TAvgLvlTree.Create(@ComparePPUListItems);
  FItems:=TAvgLvlTree.Create(@ComparePPUListItems);

  Caption:='Used ppu files of project';
  IDEDialogLayoutList.ApplyLayout(Self);
end;

procedure TPPUListDialog.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FSearchingItems);
  FItems.FreeAndClear;
  FreeAndNil(FItems);
end;

procedure TPPUListDialog.FormClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  IDEDialogLayoutList.SaveLayout(Self);
end;

procedure TPPUListDialog.SetAProject(const AValue: TLazProject);
begin
  if FAProject=AValue then exit;
  FAProject:=AValue;
  UpdateAll;
end;

procedure TPPUListDialog.SetIdleConnected(const AValue: boolean);
begin
  if FIdleConnected=AValue then exit;
  FIdleConnected:=AValue;
  if IdleConnected then
    Application.AddOnIdleHandler(@OnIdle)
  else
    Application.RemoveOnIdleHandler(@OnIdle);
end;

procedure TPPUListDialog.UpdateAll;
var
  s: String;
  MainUnit: TLazProjectFile;
begin
  if AProject=nil then exit;

  FSearchingItems.Clear;
  FItems.FreeAndClear;

  // caption
  s:='';
  if AProject.Title<>'' then
    s:=AProject.Title
  else
    s:=ExtractFileNameOnly(AProject.ProjectInfoFile);
  Caption:='PPU files of project "'+dbgstr(s)+'"';

  // ScopeLabel
  MainUnit:=AProject.MainFile;
  if MainUnit=nil then begin
    ScopeLabel.Caption:='Project has no main source file.';
  end else begin
    ScopeLabel.Caption:='Main source file: '+MainUnit.Filename;
  end;

  IdleConnected:=true;
end;

procedure TPPUListDialog.UpdateUnitsGrid;
var
  Grid: TStringGrid;
  SortedItems: TFPList;
  Node: TAvgLvlTreeNode;
  Item: TPPUListItem;
  i: Integer;
begin
  Grid:=UnitsStringGrid;
  Grid.BeginUpdate;
  Grid.RowCount:=2+FItems.Count;

  // header
  Grid.Cells[0,0]:='Unit';
  Grid.Cells[1,0]:='Size of .ppu file';
  Grid.Cells[2,0]:='Size of .o file';

  // total
  Grid.Cells[0,0]:='Unit';
  Grid.Cells[1,0]:='0 bytes / 100%';
  Grid.Cells[2,0]:='0 bytes / 100%';

  SortedItems:=TFPList.Create;
  try
    Node:=FItems.FindLowest;
    while Node<>nil do begin
      SortedItems.Add(TPPUListItem(Node.Data));
      Node:=FItems.FindSuccessor(Node);
    end;

    // ToDo: sort

    for i:=0 to SortedItems.Count-1 do begin
      Item:=TPPUListItem(SortedItems[i]);

    end;

  finally
    SortedItems.Free;
  end;

  Grid.EndUpdate;
end;

procedure TPPUListDialog.OnIdle(Sender: TObject; var Done: Boolean);
begin
  UpdateUnitsGrid;
  IdleConnected:=false;
end;

initialization
  {$I ppulistdlg.lrs}

end.

