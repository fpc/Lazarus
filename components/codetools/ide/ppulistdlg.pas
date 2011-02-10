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
  Classes, SysUtils, math, LCLProc, FileUtil, LResources, Forms, Controls,
  Graphics, Dialogs, ButtonPanel, Grids, StdCtrls, AvgLvlTree, ExtCtrls,
  ComCtrls,
  // IDEIntf
  IDECommands, MenuIntf, ProjectIntf, LazIDEIntf, IDEDialogs, IDEWindowIntf,
  // codetools
  BasicCodeTools, FileProcs, CodyStrConsts, CodeToolManager, CodeCache,
  PPUCodeTools;

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
    Splitter1: TSplitter;
    UnitUsedByStringGrid: TStringGrid;
    UnitUsesStringGrid: TStringGrid;
    UnitUsesTabSheet: TTabSheet;
    UnitUsedByTabSheet: TTabSheet;
    UnitGroupBox: TGroupBox;
    UnitPageControl: TPageControl;
    UnitsStringGrid: TStringGrid;
    procedure FormClose(Sender: TObject; var {%H-}CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure UnitsStringGridSelectCell(Sender: TObject; {%H-}aCol, aRow: Integer;
      var {%H-}CanSelect: Boolean);
  private
    FProject: TLazProject;
    FIdleConnected: boolean;
    FSearchingItems: TAvgLvlTree; // tree of TPPUListItem sorted for TheUnitName
    FItems: TAvgLvlTree; // tree of TPPUListItem sorted for TheUnitName
    procedure SetProject(const AValue: TLazProject);
    procedure SetIdleConnected(const AValue: boolean);
    procedure OnIdle(Sender: TObject; var {%H-}Done: Boolean);
    procedure AddUses(SrcItem: TPPUListItem; UsedUnits: TStrings);
    function FindUnit(AnUnitName: string): TPPUListItem;
    procedure UpdateAll;
    procedure UpdateUnitsGrid;
    function DoubleAsPercentage(const d: double): string;
    function BytesToStr(b: int64): string;
    function FindUnitInList(AnUnitName: string; List: TStrings): integer;
    function CompareUnits({%H-}Tree: TAvgLvlTree; Data1, Data2: Pointer): integer;
    procedure FillUnitsInfo(AnUnitName: string);
  public
    property AProject: TLazProject read FProject write SetProject;
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
  UnitUsesTabSheet.Caption:=crsUses;
  UnitUsedByTabSheet.Caption:=crsUsedBy;
  UnitPageControl.PageIndex:=0;

  IDEDialogLayoutList.ApplyLayout(Self);
end;

procedure TPPUListDialog.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FSearchingItems);
  FItems.FreeAndClear;
  FreeAndNil(FItems);
end;

procedure TPPUListDialog.UnitsStringGridSelectCell(Sender: TObject; aCol,
  aRow: Integer; var CanSelect: Boolean);
var
  AnUnitName: String;
begin
  if (aRow<2) or (aRow>=UnitsStringGrid.RowCount) then
    AnUnitName:=''
  else
    AnUnitName:=UnitsStringGrid.Cells[0,aRow];
  FillUnitsInfo(AnUnitName);
end;

procedure TPPUListDialog.FormClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  IDEDialogLayoutList.SaveLayout(Self);
end;

procedure TPPUListDialog.SetProject(const AValue: TLazProject);
begin
  if FProject=AValue then exit;
  FProject:=AValue;
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
  Caption:=Format(crsPPUFilesOfProject, [dbgstr(s)]);

  // ScopeLabel
  MainUnit:=AProject.MainFile;
  if MainUnit=nil then begin
    ScopeLabel.Caption:=crsProjectHasNoMainSourceFile;
  end else begin
    ScopeLabel.Caption:=Format(crsMainSourceFile, [MainUnit.Filename]);
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
    Result:=BytesToStr(TheBytes)+' / '+DoubleAsPercentage(ThePercent);
  end;

var
  Grid: TStringGrid;
  Node: TAvgLvlTreeNode;
  Item: TPPUListItem;
  Row: Integer;
  s: String;
  TotalPPUBytes, TotalOBytes: int64;
  SortedItems: TAvgLvlTree;
begin
  Grid:=UnitsStringGrid;
  Grid.BeginUpdate;

  // header
  Grid.Cells[0,0]:='Unit';
  Grid.Cells[1, 0]:=crsSizeOfPpuFile;
  Grid.Cells[2, 0]:=crsSizeOfOFile;


  SortedItems:=TAvgLvlTree.CreateObjectCompare(@CompareUnits);
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

    Grid.RowCount:=2+SortedItems.Count;

    // total
    Grid.Cells[0,1]:=crsTotal;
    Grid.Cells[1,1]:=SizeToStr(TotalPPUBytes,1.0);
    Grid.Cells[2,1]:=SizeToStr(TotalOBytes,1.0);

    // fill grid
    Row:=2;
    Node:=SortedItems.FindLowest;
    while Node<>nil do begin
      Item:=TPPUListItem(Node.Data);
      Grid.Cells[0,Row]:=Item.TheUnitName;

      // .ppu size
      s:='';
      if Item.PPUFile='' then
        s:=crsSearching
      else if Item.PPUFile=PPUFileNotFound then
        s:=crsMissing
      else
        s:=SizeToStr(Item.PPUFileSize,double(Item.PPUFileSize)/TotalPPUBytes);
      Grid.Cells[1,Row]:=s;

      // .o size
      s:='';
      if Item.OFile='' then
        s:=crsSearching
      else if Item.OFile=PPUFileNotFound then
        s:=crsMissing
      else
        s:=SizeToStr(Item.OFileSize,double(Item.OFileSize)/TotalOBytes);
      Grid.Cells[2,Row]:=s;

      inc(Row);
      Node:=SortedItems.FindSuccessor(Node);
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

function TPPUListDialog.BytesToStr(b: int64): string;
begin
  Result:='';
  if b>80000 then begin
    Result:='k';
    b:=b div 1000;
  end;
  if b>80000 then begin
    Result:='m';
    b:=b div 1000;
  end;
  if b>80000 then begin
    Result:='g';
    b:=b div 1000;
  end;
  Result:=IntToStr(b)+' '+Result+'bytes';
end;

function TPPUListDialog.FindUnitInList(AnUnitName: string; List: TStrings
  ): integer;
begin
  if List=nil then exit(-1);
  Result:=List.Count-1;
  while (Result>=0) and (SysUtils.CompareText(AnUnitName,List[Result])<>0) do
    dec(Result);
end;

function TPPUListDialog.CompareUnits(Tree: TAvgLvlTree; Data1, Data2: Pointer
  ): integer;
var
  Item1: TPPUListItem absolute Data1;
  Item2: TPPUListItem absolute Data2;
  Size1: Int64;
  Size2: Int64;
begin
  // compare size of .o file
  Size1:=Max(0,Item1.OFileSize);
  Size2:=Max(0,Item2.OFileSize);
  if Size1>Size2 then exit(-1)
  else if Size1<Size2 then exit(1);
  // compare unit name
  Result:=-SysUtils.CompareText(Item1.TheUnitName,Item2.TheUnitName);
end;

procedure TPPUListDialog.FillUnitsInfo(AnUnitName: string);
var
  Item: TPPUListItem;
  i: Integer;
  UsesUnitName: string;
  UsedByUnitName: string;
begin
  Item:=FindUnit(AnUnitName);
  if Item=nil then begin
    UnitGroupBox.Caption:='No unit selected';
    UnitGroupBox.Enabled:=false;
  end else begin
    UnitGroupBox.Caption:='Unit: '+AnUnitName;
    UnitGroupBox.Enabled:=true;
    // uses
    if Item.UsesUnits<>nil then begin
      UnitUsesStringGrid.RowCount:=1+Item.UsesUnits.Count;
      for i:=0 to Item.UsesUnits.Count-1 do begin
        UsesUnitName:=Item.UsesUnits[i];
        UnitUsesStringGrid.Cells[0,i+1]:=UsesUnitName;
      end;
    end else begin
      UnitUsesStringGrid.RowCount:=1;
    end;
    // used by
    if Item.UsedByUnits<>nil then begin
      UnitUsedByStringGrid.RowCount:=1+Item.UsedByUnits.Count;
      for i:=0 to Item.UsedByUnits.Count-1 do begin
        UsedByUnitName:=Item.UsedByUnits[i];
        UnitUsedByStringGrid.Cells[0,i+1]:=UsedByUnitName;
      end;
    end else begin
      UnitUsedByStringGrid.RowCount:=1;
    end;
  end;
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
  BaseDir: String;
  Scanned: Boolean;
  PPUTool: TPPUTool;
  OutputDir: String;
begin
  StartTime:=Now;

  BaseDir:=ExtractFilePath(AProject.ProjectInfoFile);
  OutputDir:=AProject.LazCompilerOptions.GetUnitOutputDirectory(false);

  while FSearchingItems.Count>0 do begin
    Node:=FSearchingItems.Root;
    Item:=TPPUListItem(Node.Data);
    FSearchingItems.Delete(Node);
    AnUnitName:=Item.TheUnitName;

    if Item.SrcFile='' then begin
      // search source
      //debugln(['TPPUListDialog.OnIdle search source of ',AnUnitName]);
      InFilename:='';
      Item.SrcFile:=CodeToolBoss.DirectoryCachePool.FindUnitSourceInCompletePath(
        BaseDir,AnUnitName,InFilename);
    end;

    if Item.PPUFile='' then begin
      // search ppu file
      //debugln(['TPPUListDialog.OnIdle search ppu of ',AnUnitName]);
      Item.PPUFile:=CodeToolBoss.DirectoryCachePool.FindCompiledUnitInCompletePath(
                                                         BaseDir,AnUnitName);
      if (Item.PPUFile='') and (OutputDir<>'') then begin
        // fallback: search in output directory
        Item.PPUFile:=SearchPascalFileInDir(AnUnitName+'.ppu',OutputDir,
                                            ctsfcLoUpCase);
      end;
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
      if Item.UsedByUnits=nil then
        Item.UsedByUnits:=TStringList.Create;
      //debugln(['TPPUListDialog.OnIdle search used units of ',AnUnitName]);
      // scan for used units
      Scanned:=false;
      if Item.PPUFile<>PPUFileNotFound then begin
        //debugln(['TPPUListDialog.OnIdle search used units of ppu "',Item.PPUFile,'" ...']);
        PPUTool:=CodeToolBoss.PPUCache.LoadFile(Item.PPUFile,true,false);
        if (PPUTool<>nil) and (PPUTool.ErrorMsg='') then begin
          //debugln(['TPPUListDialog.OnIdle parsed ppu "',Item.PPUFile,'"']);
          MainUsesSection:=nil;
          ImplementationUsesSection:=nil;
          try
            PPUTool.PPU.GetMainUsesSectionNames(MainUsesSection);
            AddUses(Item,MainUsesSection);
            PPUTool.PPU.GetImplementationUsesSectionNames(ImplementationUsesSection);
            AddUses(Item,ImplementationUsesSection);
            Scanned:=true;
          finally
            MainUsesSection.Free;
            ImplementationUsesSection.Free;
          end;
        end;
      end;
      if (not Scanned) and (Item.SrcFile<>'') then begin
        //debugln(['TPPUListDialog.OnIdle search used units of source "',Item.SrcFile,'"']);
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
  //debugln(['TPPUListDialog.AddUses Src=',SrcItem.TheUnitName,' UsedUnits="',UsedUnits.DelimitedText,'"']);
  for i:=0 to UsedUnits.Count-1 do begin
    AnUnitName:=UsedUnits[i];
    //debugln(['TPPUListDialog.AddUses ',SrcItem.TheUnitName,' uses ',AnUnitName]);
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

