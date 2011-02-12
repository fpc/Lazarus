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
  PPUParser, PPUCodeTools;

const
  PPUFileNotFound = ' ';
type
  TPPUListSort = (
    plsName,
    plsOSize,
    plsPPUSize,
    plsUsesCount,
    plsUsedByCount
    );
  TPPUListSortRec = record
    Category: TPPUListSort;
    Reverse: boolean;
  end;

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
    function UsesCount: integer;
    function UsedByCount: integer;
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
    procedure UnitsStringGridMouseDown(Sender: TObject; {%H-}Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure UnitsStringGridSelectCell(Sender: TObject; {%H-}aCol, aRow: Integer;
      var {%H-}CanSelect: Boolean);
  private
    FProject: TLazProject;
    FIdleConnected: boolean;
    FSearchingItems: TAvgLvlTree; // tree of TPPUListItem sorted for TheUnitName
    FItems: TAvgLvlTree; // tree of TPPUListItem sorted for TheUnitName
    FSort: array[1..3] of TPPUListSortRec;
    procedure SetProject(const AValue: TLazProject);
    procedure SetIdleConnected(const AValue: boolean);

    // scan
    procedure OnIdle(Sender: TObject; var {%H-}Done: Boolean);
    procedure AddUses(SrcItem: TPPUListItem; UsedUnits: TStrings);

    function FindUnit(AnUnitName: string): TPPUListItem;
    function FindUnitInList(AnUnitName: string; List: TStrings): integer;

    procedure UpdateAll;

    // grid
    procedure UpdateUnitsGrid;
    function CompareUnits({%H-}Tree: TAvgLvlTree; Data1, Data2: Pointer): integer;

    // units info
    procedure FillUnitsInfo(AnUnitName: string);

    function DoubleAsPercentage(const d: double): string;
    function BytesToStr(b: int64): string;
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

function TPPUListItem.UsesCount: integer;
begin
  if UsesUnits=nil then
    Result:=0
  else
    Result:=UsesUnits.Count;
end;

function TPPUListItem.UsedByCount: integer;
begin
  if UsedByUnits=nil then
    Result:=0
  else
    Result:=UsedByUnits.Count;
end;

{ TPPUListDialog }

procedure TPPUListDialog.FormCreate(Sender: TObject);
begin
  FSearchingItems:=TAvgLvlTree.Create(@ComparePPUListItems);
  FItems:=TAvgLvlTree.Create(@ComparePPUListItems);
  FSort[1].Category:=plsOSize;
  FSort[2].Category:=plsName;
  FSort[3].Category:=plsPPUSize;

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

procedure TPPUListDialog.UnitsStringGridMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  Col: Longint;
  Row: Longint;
  s: TPPUListSort;
  i: Integer;
  l: Integer;
begin
  Col:=-1;
  Row:=-1;
  UnitsStringGrid.MouseToCell(X,Y,Col,Row);
  if (Row<=1) and (Shift=[ssLeft,ssDouble]) then begin
    // double left click => sort
    case Col of
    0: s:=plsName;
    1: s:=plsPPUSize;
    2: s:=plsOSize;
    3: s:=plsUsesCount;
    4: s:=plsUsedByCount;
    else exit;
    end;
    l:=low(FSort);
    if FSort[l].Category=s then begin
      // reverse direction
      FSort[l].Reverse:=not FSort[l].Reverse;
    end else begin
      // new primary sort
      i:=l;
      while (i<=High(FSort)) and (FSort[i].Category<>s) do inc(i);
      System.Move(FSort[l],FSort[succ(l)],(i-l)*SizeOf(FSort[l]));
      FSort[l].Category:=s;
      FSort[l].Reverse:=false;
    end;
    UpdateUnitsGrid;
  end;
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
  Grid.Cells[3, 0]:=crsUses;
  Grid.Cells[4, 0]:=crsUsedBy;


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
    Grid.Cells[3,1]:=IntToStr(SortedItems.Count);
    Grid.Cells[4,1]:='';

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

      // uses
      Grid.Cells[3,Row]:=IntToStr(Item.UsesCount);

      // used by
      Grid.Cells[4,Row]:=IntToStr(Item.UsedByCount);

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

  function CompareInt(const a,b: int64; Reverse: boolean): integer;
  begin
    if a=b then exit(0);
    if (a>b) xor Reverse then
      Result:=-1
    else
      Result:=1;
  end;

var
  Item1: TPPUListItem absolute Data1;
  Item2: TPPUListItem absolute Data2;
  i: Integer;
begin
  Result:=0;
  for i:=low(FSort) to High(FSort) do begin
    case FSort[i].Category of
    plsName:
      begin
        Result:=SysUtils.CompareText(Item1.TheUnitName,Item2.TheUnitName);
        if not FSort[i].Reverse then
          Result:=-Result;
        if Result<>0 then exit;
      end;
    plsOSize:
      begin
        Result:=CompareInt(Max(0,Item1.OFileSize),Max(0,Item2.OFileSize),
                           FSort[i].Reverse);
        if Result<>0 then exit;
      end;
    plsPPUSize:
      begin
        Result:=CompareInt(Max(0,Item1.PPUFileSize),Max(0,Item2.PPUFileSize),
                           FSort[i].Reverse);
        if Result<>0 then exit;
      end;
    plsUsesCount:
      begin
        Result:=CompareInt(Item1.UsesCount,Item2.UsesCount,FSort[i].Reverse);
        if Result<>0 then exit;
      end;
    plsUsedByCount:
      begin
        Result:=CompareInt(Item1.UsedByCount,Item2.UsedByCount,FSort[i].Reverse);
        if Result<>0 then exit;
      end;
    end;
  end;
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
      debugln(['TPPUListDialog.OnIdle search used units of ',AnUnitName]);
      // scan for used units
      Scanned:=false;
      if Item.PPUFile<>PPUFileNotFound then begin
        debugln(['TPPUListDialog.OnIdle search used units of ppu "',Item.PPUFile,'" ...']);
        PPUTool:=CodeToolBoss.PPUCache.LoadFile(Item.PPUFile,
                                    [ppInterfaceHeader,ppImplementationHeader]);
        if (PPUTool<>nil) and (PPUTool.ErrorMsg='') then begin
          debugln(['TPPUListDialog.OnIdle parsed ppu "',Item.PPUFile,'"']);
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
        end else begin
          debugln(['TPPUListDialog.OnIdle failed loading ',Item.PPUFile]);
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

