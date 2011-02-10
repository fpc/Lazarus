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
  Classes, SysUtils, LCLProc, FileUtil, LResources, Forms, Controls, Graphics,
  Dialogs, ButtonPanel, Grids, StdCtrls, AvgLvlTree,
  // IDEIntf
  IDECommands, MenuIntf, ProjectIntf, LazIDEIntf, IDEDialogs, IDEWindowIntf,
  // codetools
  BasicCodeTools, FileProcs, CodyStrConsts, CodeToolManager, CodeCache;

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
    UsesUnits: TStrings; // =nil means uses section not yet scanned
    UsedByUnits: TStrings;
    destructor Destroy; override;
  end;

  { TPPUListDialog }

  TPPUListDialog = class(TForm)
    ButtonPanel1: TButtonPanel;
    ScopeLabel: TLabel;
    UnitsStringGrid: TStringGrid;
    procedure FormClose(Sender: TObject; var {%H-}CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    FAProject: TLazProject;
    FIdleConnected: boolean;
    FSearchingItems: TAvgLvlTree; // tree of TPPUListItem sorted for TheUnitName
    FItems: TAvgLvlTree; // tree of TPPUListItem sorted for TheUnitName
    procedure SetAProject(const AValue: TLazProject);
    procedure SetIdleConnected(const AValue: boolean);
    procedure OnIdle(Sender: TObject; var {%H-}Done: Boolean);
    procedure AddUses(SrcItem: TPPUListItem; UsedUnits: TStrings);
    function FindUnit(AnUnitName: string): TPPUListItem;
    procedure UpdateAll;
    procedure UpdateUnitsGrid;
    function DoubleAsPercentage(const d: double): string;
    function FindUnitInList(AnUnitName: string; List: TStrings): integer;
  public
    property AProject: TLazProject read FAProject write SetAProject;
    property IdleConnected: boolean read FIdleConnected write SetIdleConnected;
  end;

procedure ShowPPUList(Sender: TObject);
function ComparePPUListItems(Item1, Item2: Pointer): integer;
function CompareUnitNameWithPPUListItem(TheUnitName, Item: Pointer): integer;

procedure Register;

implementation

{$R *.lfm}

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

destructor TPPUListItem.Destroy;
begin
  FreeAndNil(UsesUnits);
  FreeAndNil(UsedByUnits);
  inherited Destroy;
end;

{ TPPUListDialog }

procedure TPPUListDialog.FormCreate(Sender: TObject);
begin
  IdleConnected:=false;
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
  Item: TPPUListItem;
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
    Item:=TPPUListItem.Create;
    Item.TheUnitName:=ExtractFileName(MainUnit.Filename);
    Item.SrcFile:=MainUnit.Filename;
    Item.PPUFile:=AProject.LazCompilerOptions.CreatePPUFilename(Item.SrcFile);
    //debugln(['TPPUListDialog.UpdateAll Item.SrcFile=',Item.SrcFile,' Item.PPUFile=',Item.PPUFile,' ',FileExistsCached(Item.PPUFile)]);
    Item.OFile:=ChangeFileExt(Item.PPUFile,'.o');
    if not FileExistsCached(Item.PPUFile) then
      Item.PPUFile:=PPUFileNotFound
    else
      Item.PPUFileSize:=FileSize(Item.PPUFile);
    if not FileExistsCached(Item.OFile) then
      Item.OFile:=PPUFileNotFound
    else
      Item.OFileSize:=FileSize(Item.OFile);
    FItems.Add(Item);
    FSearchingItems.Add(Item);
  end;

  IdleConnected:=true;
end;

procedure TPPUListDialog.UpdateUnitsGrid;

  function SizeToStr(TheBytes: int64; ThePercent: double): string;
  begin
    Result:=IntToStr(TheBytes)+' bytes / '+DoubleAsPercentage(ThePercent);
  end;

var
  Grid: TStringGrid;
  SortedItems: TFPList;
  Node: TAvgLvlTreeNode;
  Item: TPPUListItem;
  i: Integer;
  Row: Integer;
  s: String;
  TotalPPUBytes, TotalOBytes: int64;
begin
  Grid:=UnitsStringGrid;
  Grid.BeginUpdate;
  Grid.RowCount:=2+FItems.Count;

  // header
  Grid.Cells[0,0]:='Unit';
  Grid.Cells[1,0]:='Size of .ppu file';
  Grid.Cells[2,0]:='Size of .o file';


  SortedItems:=TFPList.Create;
  try
    Node:=FItems.FindLowest;
    TotalPPUBytes:=0;
    TotalOBytes:=0;
    while Node<>nil do begin
      Item:=TPPUListItem(Node.Data);
      if Item.PPUFileSize>0 then
        inc(TotalPPUBytes,Item.PPUFileSize);
      if Item.OFileSize>0 then
        inc(TotalOBytes,Item.OFileSize);
      SortedItems.Add(Item);
      Node:=FItems.FindSuccessor(Node);
    end;

    // total
    Grid.Cells[0,1]:='Total';
    Grid.Cells[1,1]:=SizeToStr(TotalPPUBytes,1.0);
    Grid.Cells[2,1]:=SizeToStr(TotalOBytes,1.0);

    // ToDo: sort

    Row:=2;
    for i:=0 to SortedItems.Count-1 do begin
      Item:=TPPUListItem(SortedItems[i]);
      Grid.Cells[0,Row]:=Item.TheUnitName;

      // .ppu size
      s:='';
      if Item.PPUFile='' then
        s:='searching ...'
      else if Item.PPUFile=PPUFileNotFound then
        s:='missing ...'
      else
        s:=IntToStr(Item.PPUFileSize)+' bytes / '
          +DoubleAsPercentage(double(Item.PPUFileSize)/TotalPPUBytes);
      Grid.Cells[1,Row]:=s;

      // .o size
      s:='';
      if Item.OFile='' then
        s:='searching ...'
      else if Item.OFile=PPUFileNotFound then
        s:='missing ...'
      else
        s:=IntToStr(Item.OFileSize)+' bytes / '
          +DoubleAsPercentage(double(Item.OFileSize)/TotalOBytes);
      Grid.Cells[2,Row]:=s;

      inc(Row);
    end;

  finally
    SortedItems.Free;
  end;

  Grid.EndUpdate;
end;

function TPPUListDialog.DoubleAsPercentage(const d: double): string;
begin
  Result:=IntToStr(round(d*10000));
  while length(Result)<3 do Result:='0'+Result;
  Result:=copy(Result,1,length(Result)-2)
          +DefaultFormatSettings.ThousandSeparator+RightStr(Result,2)+'%';
end;

function TPPUListDialog.FindUnitInList(AnUnitName: string; List: TStrings
  ): integer;
begin
  if List=nil then exit(-1);
  Result:=List.Count-1;
  while (Result>=0) and (SysUtils.CompareText(AnUnitName,List[Result])<>0) do
    dec(Result);
end;

procedure TPPUListDialog.OnIdle(Sender: TObject; var Done: Boolean);
const
  MaxNonIdleTime = (1/86400)/2;
var
  StartTime: TDateTime;
  Node: TAvgLvlTreeNode;
  Item: TPPUListItem;
  AnUnitName: String;
  InFilename: String;
  Code: TCodeBuffer;
  MainUsesSection: TStrings;
  ImplementationUsesSection: TStrings;
  ProjectDir: String;
begin
  StartTime:=Now;
  ProjectDir:=ExtractFilePath(AProject.ProjectInfoFile);
  while FSearchingItems.Count>0 do begin
    Node:=FSearchingItems.Root;
    Item:=TPPUListItem(Node.Data);
    FSearchingItems.Delete(Node);
    AnUnitName:=Item.TheUnitName;

    if Item.SrcFile='' then begin
      // search source
      debugln(['TPPUListDialog.OnIdle search source of ',AnUnitName]);
      InFilename:='';
      Item.SrcFile:=CodeToolBoss.DirectoryCachePool.FindUnitSourceInCompletePath(
        '',AnUnitName,InFilename);
    end;

    if Item.PPUFile='' then begin
      // search ppu file
      debugln(['TPPUListDialog.OnIdle search ppu of ',AnUnitName]);
      Item.PPUFile:=CodeToolBoss.DirectoryCachePool.FindCompiledUnitInCompletePath(
                                                         ProjectDir,AnUnitName);
      Item.OFile:=ChangeFileExt(Item.PPUFile,'.o');
      if not FileExistsCached(Item.PPUFile) then
        Item.PPUFile:=PPUFileNotFound
      else
        Item.PPUFileSize:=FileSize(Item.PPUFile);
      if not FileExistsCached(Item.OFile) then
        Item.OFile:=PPUFileNotFound
      else
        Item.OFileSize:=FileSize(Item.OFile);
    end;

    if Item.UsesUnits=nil then begin
      Item.UsesUnits:=TStringList.Create;
      Item.UsedByUnits:=TStringList.Create;
      debugln(['TPPUListDialog.OnIdle search used units of ',AnUnitName]);
      // scan for used units
      if Item.PPUFile<>PPUFileNotFound then begin
        debugln(['TPPUListDialog.OnIdle search used units of ppu "',Item.PPUFile,'"']);

      end else if Item.SrcFile<>'' then begin
        debugln(['TPPUListDialog.OnIdle search used units of source "',Item.SrcFile,'"']);
        Code:=CodeToolBoss.LoadFile(Item.SrcFile,true,false);
        if Code<>nil then begin
          MainUsesSection:=nil;
          ImplementationUsesSection:=nil;
          try
            if CodeToolBoss.FindUsedUnitNames(Code,MainUsesSection,ImplementationUsesSection)
            then begin
              AddUses(Item,MainUsesSection);
              AddUses(Item,ImplementationUsesSection);
            end;
          finally
            MainUsesSection.Free;
            ImplementationUsesSection.Free;
          end;
        end;
      end;
    end;

    if Now-StartTime>MaxNonIdleTime then break;
  end;

  UpdateUnitsGrid;

  if FSearchingItems.Count=0 then
    IdleConnected:=false;
end;

procedure TPPUListDialog.AddUses(SrcItem: TPPUListItem; UsedUnits: TStrings);
var
  i: Integer;
  AnUnitName: string;
  UsedUnit: TPPUListItem;
begin
  if UsedUnits=nil then exit;
  debugln(['TPPUListDialog.AddUses Src=',SrcItem.TheUnitName,' UsedUnits="',UsedUnits.DelimitedText,'"']);
  for i:=0 to UsedUnits.Count-1 do begin
    AnUnitName:=UsedUnits[i];
    debugln(['TPPUListDialog.AddUses ',SrcItem.TheUnitName,' uses ',AnUnitName]);
    UsedUnit:=FindUnit(AnUnitName);
    if UsedUnit=nil then begin
      // new unit
      UsedUnit:=TPPUListItem.Create;
      UsedUnit.TheUnitName:=AnUnitName;
      FItems.Add(UsedUnit);
      FSearchingItems.Add(UsedUnit);
      UsedUnit.UsedByUnits:=TStringList.Create;
    end;

    if FindUnitInList(AnUnitName,SrcItem.UsesUnits)<0 then
      SrcItem.UsesUnits.Add(AnUnitName);
    if FindUnitInList(SrcItem.TheUnitName,UsedUnit.UsedByUnits)<0 then
      UsedUnit.UsedByUnits.Add(SrcItem.TheUnitName);
  end;
end;

function TPPUListDialog.FindUnit(AnUnitName: string): TPPUListItem;
var
  Node: TAvgLvlTreeNode;
begin
  Node:=FItems.FindKey(Pointer(AnUnitName),@CompareUnitNameWithPPUListItem);
  if Node=nil then
    Result:=nil
  else
    Result:=TPPUListItem(Node.Data);
end;

end.

