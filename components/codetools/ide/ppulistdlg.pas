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
  Classes, SysUtils, contnrs, math, LCLProc, FileUtil, LResources, Forms,
  Controls, Graphics, Dialogs, ButtonPanel, Grids, StdCtrls, AvgLvlTree,
  ExtCtrls, ComCtrls,
  // IDEIntf
  ProjectIntf, LazIDEIntf, IDEDialogs, IDEWindowIntf,
  PackageIntf,
  // codetools
  BasicCodeTools, FileProcs, CodyStrConsts, CodeToolManager, CodeCache,
  PPUParser, PPUCodeTools, DefineTemplates,
  CodyUtils;

const
  PPUFileNotFound = ' ';
type
  TPPUListSort = (
    plsName,
    plsOSize,
    plsPPUSize,
    plsUsesCount,
    plsUsedByCount,
    plsPackage
    );
  TPPUListSortRec = record
    Category: TPPUListSort;
    Reverse: boolean;
  end;

  TPPUListType = (
    pltUsedBy,
    pltUses
    );

  { TPPUDlgListItem }

  TPPUDlgListItem = class
  public
    TheUnitName: string;
    SrcFile: string;
    PPUFile: string; // = '' means not searched, = PPUFileNotFound means not found
    OFile: string;
    PPUFileSize: int64;
    OFileSize: int64;
    UsesUnits: TStrings; // =nil means uses section not yet scanned
    UsedByUnits: TStrings;
    LinkedFiles: TObjectList; // list of TPPULinkedFile
    PackageName: string;
    destructor Destroy; override;
    function UsesCount: integer;
    function UsedByCount: integer;
  end;

  { TPPUDlgLinkedFile }

  TPPUDlgLinkedFile = class(TPPULinkedFile)
  public
    Units: TStrings;
    constructor Create;
    destructor Destroy; override;
  end;

  { TPPUListDialog }

  TPPUListDialog = class(TForm)
    ButtonPanel1: TButtonPanel;
    LinkedFilesTreeView: TTreeView;
    PageControl1: TPageControl;
    UnitsTabSheet: TTabSheet;
    LinkedFilesTabSheet: TTabSheet;
    ScopeLabel: TLabel;
    Splitter1: TSplitter;
    InfoTabSheet: TTabSheet;
    PPUFileLabel: TLabel;
    SourceFileLabel: TLabel;
    UnitLinkedFilesTabSheet: TTabSheet;
    UnitLinkedFilesStringGrid: TStringGrid;
    UsesPathStringGrid: TStringGrid;
    UsesPathTabSheet: TTabSheet;
    UsedByStringGrid: TStringGrid;
    UsesStringGrid: TStringGrid;
    UsesTabSheet: TTabSheet;
    UsedByTabSheet: TTabSheet;
    UnitGroupBox: TGroupBox;
    UnitPageControl: TPageControl;
    UnitsStringGrid: TStringGrid;
    procedure FormClose(Sender: TObject; var {%H-}CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure LinkedFilesTreeViewDblClick(Sender: TObject);
    procedure UnitsStringGridMouseDown(Sender: TObject; {%H-}Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure UnitsStringGridSelectCell(Sender: TObject; {%H-}aCol, aRow: Integer;
      var {%H-}CanSelect: Boolean);
    procedure UnitStringGridMouseDown(Sender: TObject;
      {%H-}Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure HelpButtonClick(Sender: TObject);
  private
    FMainItem: TPPUDlgListItem;
    FProject: TLazProject;
    FIdleConnected: boolean;
    FSearchingItems: TAvgLvlTree; // tree of TPPUDlgListItem sorted for TheUnitName
    FItems: TAvgLvlTree; // tree of TPPUDlgListItem sorted for TheUnitName
    FSort: array[1..3] of TPPUListSortRec;
    FDlgLinkedFiles: TAvgLvlTree; // tree of TPPUDlgLinkedFile sorted for ID, file, flags
    procedure SetProject(const AValue: TLazProject);
    procedure SetIdleConnected(const AValue: boolean);

    // scan
    procedure OnIdle(Sender: TObject; var {%H-}Done: Boolean);
    procedure AddUses(SrcItem: TPPUDlgListItem; UsedUnits: TStrings);

    function FindUnit(AnUnitName: string): TPPUDlgListItem;
    function FindUnitInList(AnUnitName: string; List: TStrings): integer;
    function FindUnitOfListitem(List: TStrings; Index: integer): TPPUDlgListItem;
    function FindPackageOfUnit(Item: TPPUDlgListItem): string;

    procedure UpdateAll;

    // units grid
    procedure UpdateUnitsGrid;
    function CompareUnits({%H-}Tree: TAvgLvlTree; Data1, Data2: Pointer): integer;
    procedure JumpToUnit(TheUnitName: string);

    // units info
    procedure UpdateUnitsInfo;
    procedure FillUnitsInfo(AnUnitName: string);
    function FindUsesPath(UsingUnit, UsedUnit: TPPUDlgListItem): TFPList;

    // linked files
    procedure UpdateLinkedFilesTreeView;

    function DoubleAsPercentage(const d: double): string;
    function BytesToStr(b: int64): string;
  public
    property AProject: TLazProject read FProject write SetProject;
    property IdleConnected: boolean read FIdleConnected write SetIdleConnected;
    property MainItem: TPPUDlgListItem read FMainItem;
  end;

procedure ShowPPUList(Sender: TObject);
function ComparePPUListItems(Item1, Item2: Pointer): integer;
function CompareUnitNameWithPPUListItem(TheUnitName, Item: Pointer): integer;

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
  li1: TPPUDlgListItem absolute Item1;
  li2: TPPUDlgListItem absolute Item2;
begin
  Result:=CompareIdentifiers(PChar(li1.TheUnitName),PChar(li2.TheUnitName));
end;

function CompareUnitNameWithPPUListItem(TheUnitName, Item: Pointer): integer;
var
  li: TPPUDlgListItem absolute Item;
  un: PChar;
begin
  un:=PChar(AnsiString(TheUnitName));
  Result:=CompareIdentifiers(un,PChar(li.TheUnitName));
end;

{ TPPUDlgLinkedFile }

constructor TPPUDlgLinkedFile.Create;
begin
  inherited Create;
  Units:=TStringList.Create;
end;

destructor TPPUDlgLinkedFile.Destroy;
begin
  FreeAndNil(Units);
  inherited Destroy;
end;

{ TPPUDlgListItem }

destructor TPPUDlgListItem.Destroy;
begin
  FreeAndNil(UsesUnits);
  FreeAndNil(UsedByUnits);
  FreeAndNil(LinkedFiles);
  inherited Destroy;
end;

function TPPUDlgListItem.UsesCount: integer;
begin
  if UsesUnits=nil then
    Result:=0
  else
    Result:=UsesUnits.Count;
end;

function TPPUDlgListItem.UsedByCount: integer;
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
  FDlgLinkedFiles:=TAvgLvlTree.Create(@ComparePPULinkedFiles);

  FSort[1].Category:=plsOSize;
  FSort[2].Category:=plsName;
  FSort[3].Category:=plsPPUSize;

  PageControl1.PageIndex:=0;

  UnitsTabSheet.Caption:=crsUnits;

  // UnitsStringGrid header
  UnitsStringGrid.Columns[0].Title.Caption:=crsUnit;
  UnitsStringGrid.Columns[1].Title.Caption:=crsSizeOfPpuFile;
  UnitsStringGrid.Columns[2].Title.Caption:=crsSizeOfOFile;
  UnitsStringGrid.Columns[3].Title.Caption:=crsUses;
  UnitsStringGrid.Columns[4].Title.Caption:=crsUsedBy;
  UnitsStringGrid.Columns[5].Title.Caption:=crsPackage;

  InfoTabSheet.Caption:=lisCOGeneral;

  UsesTabSheet.Caption:=crsUses;
  UsesStringGrid.Columns[0].Title.Caption:=crsUnit;

  UsedByTabSheet.Caption:=crsUsedBy;
  UsedByStringGrid.Columns[0].Title.Caption:=crsUnit;

  UsesPathTabSheet.Caption:=lisCOUsesPath;
  UsesPathStringGrid.Columns[0].Title.Caption:=crsUnit;

  UnitLinkedFilesTabSheet.Caption:=crsLinkedFiles;
  UnitLinkedFilesStringGrid.Columns[0].Title.Caption:=crsType;
  UnitLinkedFilesStringGrid.Columns[1].Title.Caption:=crsFile;
  UnitLinkedFilesStringGrid.Columns[2].Title.Caption:=crsFlags;

  UnitPageControl.PageIndex:=0;

  LinkedFilesTabSheet.Caption:=crsLinkedFiles;

  ButtonPanel1.HelpButton.Caption:=crsHelp;
  ButtonPanel1.CloseButton.Caption:=crsClose;

  IDEDialogLayoutList.ApplyLayout(Self);
end;

procedure TPPUListDialog.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FSearchingItems);
  FItems.FreeAndClear;
  FreeAndNil(FItems);
  FDlgLinkedFiles.FreeAndClear;
  FreeAndNil(FDlgLinkedFiles);
end;

procedure TPPUListDialog.LinkedFilesTreeViewDblClick(Sender: TObject);
var
  Node: TTreeNode;
  TheUnitName: string;
begin
  Node:=LinkedFilesTreeView.Selected;
  if Node=nil then exit;
  if Node.Data=nil then begin
    TheUnitName:=Node.Text;
    JumpToUnit(TheUnitName);
  end;
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
  if FItems=nil then exit;
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
    5: s:=plsPackage;
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
  if FItems=nil then exit;
  if (aRow<2) or (aRow>=UnitsStringGrid.RowCount) then
    AnUnitName:=''
  else
    AnUnitName:=UnitsStringGrid.Cells[0,aRow];
  FillUnitsInfo(AnUnitName);
end;

procedure TPPUListDialog.UnitStringGridMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  Grid: TStringGrid;
  Col: Longint;
  Row: Longint;
  AnUnitName: string;
begin
  if FItems=nil then exit;
  Grid:=TStringGrid(Sender);
  if Shift=[ssLeft,ssDouble] then begin
    Col:=0;
    Row:=0;
    Grid.MouseToCell(X,Y,Col,Row);
    if (Row<1) or (Row>=Grid.RowCount) then exit;
    if (Col=0) then begin
      AnUnitName:=Grid.Cells[0,Row];
      JumpToUnit(AnUnitName);
    end;
  end;
end;

procedure TPPUListDialog.HelpButtonClick(Sender: TObject);
begin
  OpenCodyHelp('#PPU_files_of_project');
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
  FMainItem:=nil;
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

function TPPUListDialog.FindUnitOfListitem(List: TStrings; Index: integer
  ): TPPUDlgListItem;
begin
  Result:=TPPUDlgListItem(List.Objects[Index]);
  if Result<>nil then exit;
  Result:=FindUnit(List[Index]);
  if Result<>nil then
    List.Objects[Index]:=Result;
end;

function TPPUListDialog.FindPackageOfUnit(Item: TPPUDlgListItem): string;
var
  BaseDir: String;
  PPUDir: String;

  procedure CheckIfFPCUnit;
  var
    BaseDir: String;
    UnitSetID: String;
    Cache: TFPCUnitSetCache;
    CfgCache: TFPCTargetConfigCache;
    HasChanged: boolean;
  begin
    UnitSetID:=CodeToolBoss.GetUnitSetIDForDirectory(BaseDir{%H-});
    if UnitSetID='' then exit;
    Cache:=CodeToolBoss.FPCDefinesCache.FindUnitSetWithID(UnitSetID,HasChanged,false);
    if Cache=nil then exit;
    CfgCache:=Cache.GetConfigCache(false);
    if CfgCache=nil then exit;
    if CfgCache.Units.Contains(Item.TheUnitName) then
      Item.PackageName:=crsByFpcCfg;
  end;

var
  i: Integer;
  Pkg: TIDEPackage;
  OutDir: String;
begin
  BaseDir:='';
  if Item.PackageName='' then begin
    BaseDir:=ExtractFilePath(AProject.ProjectInfoFile);
    if not FilenameIsAbsolute(BaseDir) then BaseDir:='';

    if Item.PPUFile<>'' then
      PPUDir:=ExtractFilePath(Item.PPUFile)
    else
      PPUDir:='';

    // check if virtual unit
    if (Item.SrcFile<>'') and (not FilenameIsAbsolute(Item.SrcFile)) then
      Item.PackageName:=crsVirtualUnit;

    // check if in output directory of project
    if PPUDir<>'' then begin
      OutDir:=AppendPathDelim(AProject.LazCompilerOptions.GetUnitOutputDirectory(false));
      if CompareFilenames(OutDir,PPUDir)=0 then
        Item.PackageName:=crsProjectOutput;
    end;

    if (Item.PackageName='') and (PPUDir<>'') then begin
      // search in output directories of packages
      for i:=0 to PackageEditingInterface.GetPackageCount-1 do begin
        Pkg:=PackageEditingInterface.GetPackages(i);
        OutDir:=Pkg.LazCompilerOptions.GetUnitOutputDirectory(false);
        if (OutDir<>'') and FilenameIsAbsolute(OutDir)
        and (CompareFilenames(AppendPathDelim(OutDir),PPUDir)=0) then begin
          Item.PackageName:=Pkg.Name;
          break;
        end;
      end;
    end;

    // search in FPC unit paths
    if Item.PackageName=''then
      CheckIfFPCUnit;

    if Item.PackageName='' then
      Item.PackageName:='?';
  end;
  Result:=Item.PackageName;
end;

procedure TPPUListDialog.UpdateAll;
var
  s: String;
  MainUnit: TLazProjectFile;
  Item: TPPUDlgListItem;
begin
  if AProject=nil then exit;

  FSearchingItems.Clear;
  FItems.FreeAndClear;

  // caption
  s:=AProject.GetDefaultTitle;
  Caption:=Format(crsPPUFilesOfProject, [s]);

  // ScopeLabel
  MainUnit:=AProject.MainFile;
  if MainUnit=nil then begin
    ScopeLabel.Caption:=crsProjectHasNoMainSourceFile;
  end else begin
    ScopeLabel.Caption:=Format(crsMainSourceFile, [MainUnit.Filename]);
    Item:=TPPUDlgListItem.Create;
    FMainItem:=Item;
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
  Item: TPPUDlgListItem;
  Row: Integer;
  s: String;
  TotalPPUBytes, TotalOBytes: int64;
  SortedItems: TAvgLvlTree;
begin
  Grid:=UnitsStringGrid;
  Grid.BeginUpdate;

  SortedItems:=TAvgLvlTree.CreateObjectCompare(@CompareUnits);
  try
    Node:=FItems.FindLowest;
    TotalPPUBytes:=0;
    TotalOBytes:=0;
    while Node<>nil do begin
      Item:=TPPUDlgListItem(Node.Data);
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
    Grid.Cells[5,1]:='';

    // fill grid
    Row:=2;
    Node:=SortedItems.FindLowest;
    while Node<>nil do begin
      Item:=TPPUDlgListItem(Node.Data);
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

      // used by
      Grid.Cells[5,Row]:=Item.PackageName;

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
          +DefaultFormatSettings.DecimalSeparator+RightStr(Result,2)+'%';
end;

function TPPUListDialog.BytesToStr(b: int64): string;
begin
  Result:='';
  if b>80000 then begin
    Result:=crsKbytes;
    b:=b div 1000;
  end;
  if b>80000 then begin
    Result:=crsMbytes;
    b:=b div 1000;
  end;
  if b>80000 then begin
    Result:=crsGbytes;
    b:=b div 1000;
  end;
  Result:=IntToStr(b)+' '+Result;
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
  Item1: TPPUDlgListItem absolute Data1;
  Item2: TPPUDlgListItem absolute Data2;
  i: Integer;
begin
  Result:=0;
  for i:=low(FSort) to High(FSort) do begin
    case FSort[i].Category of
    plsName:
      begin
        Result:=SysUtils.CompareText(Item1.TheUnitName,Item2.TheUnitName);
        if FSort[i].Reverse then
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
    plsPackage:
      begin
        Result:=SysUtils.CompareText(Item1.PackageName,Item2.PackageName);
        if FSort[i].Reverse then
          Result:=-Result;
        if Result<>0 then exit;
      end;
    end;
  end;
end;

procedure TPPUListDialog.JumpToUnit(TheUnitName: string);
var
  i: Integer;
begin
  for i:=2 to UnitsStringGrid.RowCount-1 do begin
    if SysUtils.CompareText(UnitsStringGrid.Cells[0,i],TheUnitName)<>0 then
      continue;
    PageControl1.PageIndex:=0;
    UnitsStringGrid.Row:=i;
    UnitsStringGrid.Col:=0;
    exit;
  end;
end;

procedure TPPUListDialog.UpdateUnitsInfo;
var
  AnUnitName: String;
begin
  if (UnitsStringGrid.Row<2) or (UnitsStringGrid.Row>=UnitsStringGrid.RowCount) then
    AnUnitName:=''
  else
    AnUnitName:=UnitsStringGrid.Cells[0,UnitsStringGrid.Row];
  FillUnitsInfo(AnUnitName);
end;

procedure TPPUListDialog.FillUnitsInfo(AnUnitName: string);
var
  Item: TPPUDlgListItem;
  i: Integer;
  UsesUnitName: string;
  UsedByUnitName: string;
  UsesPath: TFPList;
  LinkedFile: TPPULinkedFile;
  Grid: TStringGrid;
begin
  Item:=FindUnit(AnUnitName);
  if Item=nil then begin
    UnitGroupBox.Caption:=crsNoUnitSelected;
    UnitGroupBox.Enabled:=false;
    SourceFileLabel.Caption:='';
    PPUFileLabel.Caption:='';
  end else begin
    UnitGroupBox.Caption:=Format(crsUnit2, [AnUnitName]);
    UnitGroupBox.Enabled:=true;
    // info
    SourceFileLabel.Caption:=Format(crsSource, [Item.SrcFile]);
    PPUFileLabel.Caption:=Format(crsPPU, [Item.PPUFile]);
    // uses
    if Item.UsesUnits<>nil then begin
      UsesStringGrid.RowCount:=1+Item.UsesUnits.Count;
      for i:=0 to Item.UsesUnits.Count-1 do begin
        UsesUnitName:=Item.UsesUnits[i];
        UsesStringGrid.Cells[0,i+1]:=UsesUnitName;
      end;
    end else begin
      UsesStringGrid.RowCount:=1;
    end;
    // used by
    if Item.UsedByUnits<>nil then begin
      UsedByStringGrid.RowCount:=1+Item.UsedByUnits.Count;
      for i:=0 to Item.UsedByUnits.Count-1 do begin
        UsedByUnitName:=Item.UsedByUnits[i];
        UsedByStringGrid.Cells[0,i+1]:=UsedByUnitName;
      end;
    end else begin
      UsedByStringGrid.RowCount:=1;
    end;
    // uses path
    UsesPath:=FindUsesPath(MainItem,Item);
    try
      UsesPathStringGrid.RowCount:=UsesPath.Count+1;
      for i:=0 to UsesPath.Count-1 do begin
        UsesPathStringGrid.Cells[0,i+1]:=TPPUDlgListItem(UsesPath[i]).TheUnitName;
      end;
    finally
      UsesPath.Free;
    end;

    // linked files
    Grid:=UnitLinkedFilesStringGrid;
    if Item.LinkedFiles<>nil then begin
      Grid.RowCount:=1+Item.LinkedFiles.Count;
      for i:=0 to Item.LinkedFiles.Count-1 do begin
        LinkedFile:=TPPULinkedFile(Item.LinkedFiles[i]);
        Grid.Cells[0,i+1]:=PPUEntryName(LinkedFile.ID);
        Grid.Cells[1,i+1]:=LinkedFile.Filename;
        Grid.Cells[2,i+1]:=PPULinkContainerFlagToStr(LinkedFile.Flags);
      end;
    end else begin
      Grid.RowCount:=1;
    end;
  end;
end;

function TPPUListDialog.FindUsesPath(UsingUnit, UsedUnit: TPPUDlgListItem): TFPList;
{ Search a path from UsingUnit to UsedUnit
  Result is a list of TPPUDlgListItem
}
var
  Visited: TAvgLvlTree;

  function Search(Item: TPPUDlgListItem; Path: TFPList): boolean;
  var
    i: Integer;
    ParentUnit: TPPUDlgListItem;
  begin
    Result:=false;
    if Visited.Find(Item)<>nil then exit;
    Visited.Add(Item);
    if Item.UsedByUnits<>nil then begin
      for i:=0 to Item.UsedByUnits.Count-1 do begin
        ParentUnit:=FindUnitOfListitem(Item.UsedByUnits,i);
        if (ParentUnit=nil) or (Visited.Find(ParentUnit)<>nil) then continue;
        if (ParentUnit=UsingUnit) or Search(ParentUnit,Path) then begin
          // path found
          Path.Add(ParentUnit);
          exit(true);
        end;
      end;
    end;
  end;

begin
  Result:=TFPList.Create;
  if (UsingUnit=nil) or (UsedUnit=nil) then exit;
  Visited:=TAvgLvlTree.Create(@ComparePPUListItems);
  try
    if Search(UsedUnit,Result) then
      Result.Add(UsedUnit);
  finally
    Visited.Free;
  end;
end;

procedure TPPUListDialog.UpdateLinkedFilesTreeView;

  function GetLinkedFilesCategoryNode(ID: byte): TTreeNode;
  var
    i: Integer;
  begin
    for i:=0 to LinkedFilesTreeView.Items.TopLvlCount-1 do begin
      Result:=LinkedFilesTreeView.Items.TopLvlItems[i];
      if {%H-}PtrUInt(Result.Data)=ID then exit;
    end;
    Result:=nil;
  end;

  function CreateCategoryNode(ID: byte): TTreeNode;
  var
    Desc: String;
  begin
    case ID of
    iblinkunitofiles:
      Desc:=crsUnitObjectFiles;
    iblinkunitstaticlibs :
      Desc:=crsUnitStaticLibraries;
    iblinkunitsharedlibs :
      Desc:=crsUnitSharedLibraries;
    iblinkotherofiles :
      Desc:=crsOtherObjectFiles;
    iblinkotherstaticlibs :
      Desc:=crsOtherStaticLibraries;
    iblinkothersharedlibs :
      Desc:=crsOtherSharedLibraries;
    iblinkotherframeworks:
      Desc:=crsFrameworks;
    else
      Desc:=PPUEntryName(ID);
    end;
    Result:=LinkedFilesTreeView.Items.AddObject(nil,Desc,{%H-}Pointer(ID));
  end;

var
  PPUNode, DlgLinkedFileNode: TAvgLvlTreeNode;
  Item: TPPUDlgListItem;
  PPULinkedFile: TPPULinkedFile;
  DlgLinkedFile: TPPUDlgLinkedFile;
  CategoryNode: TTreeNode;
  s: String;
  i: Integer;
  TVNode: TTreeNode;
begin
  LinkedFilesTreeView.BeginUpdate;
  try
    LinkedFilesTreeView.Items.Clear;
    FDlgLinkedFiles.FreeAndClear;

    // collect all linked files
    PPUNode:=FItems.FindLowest;
    while PPUNode<>nil do begin
      Item:=TPPUDlgListItem(PPUNode.Data);
      if Item.LinkedFiles<>nil then begin
        for i:=0 to Item.LinkedFiles.Count-1 do begin
          PPULinkedFile:=TPPULinkedFile(Item.LinkedFiles[i]);
          DlgLinkedFileNode:=FDlgLinkedFiles.Find(PPULinkedFile);
          if DlgLinkedFileNode<>nil then
            DlgLinkedFile:=TPPUDlgLinkedFile(DlgLinkedFileNode.Data)
          else begin
            DlgLinkedFile:=TPPUDlgLinkedFile.Create;
            DlgLinkedFile.ID:=PPULinkedFile.ID;
            DlgLinkedFile.Filename:=PPULinkedFile.Filename;
            DlgLinkedFile.Flags:=PPULinkedFile.Flags;
            FDlgLinkedFiles.Add(DlgLinkedFile);
          end;
          if DlgLinkedFile.Units.IndexOf(Item.TheUnitName)<0 then
            DlgLinkedFile.Units.Add(Item.TheUnitName);
        end;
      end;
      PPUNode:=FItems.FindSuccessor(PPUNode);
    end;

    // create category nodes
    for i:=iblinkunitofiles to iblinkothersharedlibs do
      CreateCategoryNode(i);
    CreateCategoryNode(iblinkotherframeworks);

    DlgLinkedFileNode:=FDlgLinkedFiles.FindLowest;
    while DlgLinkedFileNode<>nil do begin
      DlgLinkedFile:=TPPUDlgLinkedFile(DlgLinkedFileNode.Data);
      CategoryNode:=GetLinkedFilesCategoryNode(DlgLinkedFile.ID);
      s:=DlgLinkedFile.Filename+' ['+PPULinkContainerFlagToStr(DlgLinkedFile.Flags)+']';
      TVNode:=LinkedFilesTreeView.Items.AddChildObject(CategoryNode,s,DlgLinkedFile);
      for i:=0 to DlgLinkedFile.Units.Count-1 do
        LinkedFilesTreeView.Items.AddChild(TVNode,DlgLinkedFile.Units[i]);
      DlgLinkedFileNode:=FDlgLinkedFiles.FindSuccessor(DlgLinkedFileNode);
    end;

  finally
    LinkedFilesTreeView.EndUpdate;
  end;
end;

procedure TPPUListDialog.OnIdle(Sender: TObject; var Done: Boolean);
const
  MaxNonIdleTime = (1/86400)/2;
var
  StartTime: TDateTime;
  Node: TAvgLvlTreeNode;
  Item: TPPUDlgListItem;
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
    Item:=TPPUDlgListItem(Node.Data);
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
        Item.PPUFile:=CodeToolBoss.DirectoryCachePool.FindCompiledUnitInPath(
          OutputDir,'.',AnUnitName,false);
      end;
      Item.OFile:=ChangeFileExt(Item.PPUFile,'.o');
      if not FileExistsCached(Item.PPUFile) then begin
        if Item.PPUFile<>'' then begin
          debugln(['TPPUListDialog.OnIdle warning: ppu file gone from disk: ',Item.PPUFile]);
        end;
        Item.PPUFile:=PPUFileNotFound;
      end else
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
        PPUTool:=CodeToolBoss.PPUCache.LoadFile(Item.PPUFile,
                                    [ppInterfaceHeader,ppImplementationHeader]);
        if (PPUTool<>nil) and (PPUTool.ErrorMsg='') then begin
          //debugln(['TPPUListDialog.OnIdle parsed ppu "',Item.PPUFile,'"']);
          MainUsesSection:=nil;
          ImplementationUsesSection:=nil;
          FreeAndNil(Item.LinkedFiles);
          try
            PPUTool.PPU.GetMainUsesSectionNames(MainUsesSection);
            AddUses(Item,MainUsesSection);
            PPUTool.PPU.GetImplementationUsesSectionNames(ImplementationUsesSection);
            AddUses(Item,ImplementationUsesSection);
            PPUTool.PPU.GetLinkedFiles(Item.LinkedFiles);
            Scanned:=true;
          finally
            MainUsesSection.Free;
            ImplementationUsesSection.Free;
          end;
        end else begin
          debugln(['TPPUListDialog.OnIdle failed loading ',Item.PPUFile]);
        end;
      end else begin
        //debugln(['TPPUListDialog.OnIdle PPU not found of ',AnUnitName]);
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

    Item.PackageName:='';
    FindPackageOfUnit(Item);

    if Now-StartTime>MaxNonIdleTime then break;
  end;

  UpdateUnitsGrid;
  UpdateLinkedFilesTreeView;

  if FSearchingItems.Count=0 then begin
    IdleConnected:=false;
    UpdateUnitsInfo;
  end;
end;

procedure TPPUListDialog.AddUses(SrcItem: TPPUDlgListItem; UsedUnits: TStrings);
var
  i: Integer;
  AnUnitName: string;
  UsedUnit: TPPUDlgListItem;
begin
  if UsedUnits=nil then exit;
  //debugln(['TPPUListDialog.AddUses Src=',SrcItem.TheUnitName,' UsedUnits="',UsedUnits.DelimitedText,'"']);
  for i:=0 to UsedUnits.Count-1 do begin
    AnUnitName:=UsedUnits[i];
    //debugln(['TPPUListDialog.AddUses ',SrcItem.TheUnitName,' uses ',AnUnitName]);
    UsedUnit:=FindUnitOfListitem(UsedUnits,i);
    if UsedUnit=nil then begin
      // new unit
      UsedUnit:=TPPUDlgListItem.Create;
      UsedUnit.TheUnitName:=AnUnitName;
      FItems.Add(UsedUnit);
      FSearchingItems.Add(UsedUnit);
      UsedUnits.Objects[i]:=UsedUnit;
      UsedUnit.UsedByUnits:=TStringList.Create;
    end;

    if FindUnitInList(AnUnitName,SrcItem.UsesUnits)<0 then
      SrcItem.UsesUnits.Add(AnUnitName);
    if FindUnitInList(SrcItem.TheUnitName,UsedUnit.UsedByUnits)<0 then
      UsedUnit.UsedByUnits.Add(SrcItem.TheUnitName);
  end;
end;

function TPPUListDialog.FindUnit(AnUnitName: string): TPPUDlgListItem;
var
  Node: TAvgLvlTreeNode;
begin
  Node:=FItems.FindKey(Pointer(AnUnitName),@CompareUnitNameWithPPUListItem);
  if Node=nil then
    Result:=nil
  else
    Result:=TPPUDlgListItem(Node.Data);
end;

end.

