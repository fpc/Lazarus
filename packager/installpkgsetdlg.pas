{  $Id$  }
{
 /***************************************************************************
                         installpkgsetdlg.pas
                         --------------------


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
 *   Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.        *
 *                                                                         *
 ***************************************************************************

  Author: Mattias Gaertner

  Abstract:
    Dialog to edit the package set installed in the IDE.
}
unit InstallPkgSetDlg;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, contnrs, LCLProc, Forms, Controls, Graphics, Dialogs,
  KeywordFuncLists, StdCtrls, Buttons, FileUtil, ExtCtrls, ComCtrls, EditBtn,
  AVL_Tree, Laz_XMLCfg, TreeFilterEdit, PackageIntf, IDEImagesIntf, IDEHelpIntf,
  LazarusIDEStrConsts, EnvironmentOpts, InputHistory, LazConf, IDEProcs,
  PackageDefs, PackageSystem, PackageLinks, IDEContextHelpEdit;

type
  TOnCheckInstallPackageList =
                      procedure(PkgIDs: TObjectList; out Ok: boolean) of object;

  { TInstallPkgSetDialog }

  TInstallPkgSetDialog = class(TForm)
    AddToInstallButton: TBitBtn;
    AvailableTreeView: TTreeView;
    AvailablePkgGroupBox: TGroupBox;
    HelpButton: TBitBtn;
    CancelButton: TBitBtn;
    ExportButton: TButton;
    BtnPanel: TPanel;
    InstallTreeView: TTreeView;
    lblMiddle: TLabel;
    AvailableFilterEdit: TTreeFilterEdit;
    NoteLabel: TLabel;
    PkgInfoMemo: TMemo;
    PkgInfoGroupBox: TGroupBox;
    ImportButton: TButton;
    SaveAndExitButton: TBitBtn;
    InstallPkgGroupBox: TGroupBox;
    SaveAndRebuildButton: TBitBtn;
    UninstallButton: TBitBtn;
    procedure AddToInstallButtonClick(Sender: TObject);
    procedure AvailableTreeViewDblClick(Sender: TObject);
    procedure AvailableTreeViewSelectionChanged(Sender: TObject);
    procedure ExportButtonClick(Sender: TObject);
    procedure HelpButtonClick(Sender: TObject);
    procedure ImportButtonClick(Sender: TObject);
    procedure InstallButtonClick(Sender: TObject);
    procedure InstallTreeViewDblClick(Sender: TObject);
    procedure InstallPkgSetDialogCreate(Sender: TObject);
    procedure InstallPkgSetDialogDestroy(Sender: TObject);
    procedure InstallPkgSetDialogResize(Sender: TObject);
    procedure InstallTreeViewSelectionChanged(Sender: TObject);
    procedure SaveAndExitButtonClick(Sender: TObject);
    procedure UninstallButtonClick(Sender: TObject);
  private
    FNewInstalledPackages: TObjectList;
    FOldInstalledPackages: TPkgDependency;
    FOnCheckInstallPackageList: TOnCheckInstallPackageList;
    fAvailablePackages: TAVLTree;// tree of TLazPackageID (all available packages and links)
    FRebuildIDE: boolean;
    FSelectedPkg: TLazPackage;
    ImgIndexPackage: integer;
    ImgIndexInstallPackage: integer;
    ImgIndexInstalledPackage: integer;
    ImgIndexUninstallPackage: integer;
    ImgIndexCirclePackage: integer;
    ImgIndexMissingPackage: integer;
    procedure SetOldInstalledPackages(const AValue: TPkgDependency);
    procedure AssignOldInstalledPackagesToList;
    function PackageInInstallList(PkgName: string): boolean;
    function ChooseImageIndex(Str: String; Data: TObject; var AIsEnabled: Boolean): Integer;
    procedure UpdateAvailablePackages(Immediately: boolean = false);
    procedure UpdateNewInstalledPackages;
    procedure OnIteratePackages(APackageID: TLazPackageID);
    function DependencyToStr(Dependency: TPkgDependency): string;
    procedure ClearNewInstalledPackages;
    function CheckSelection: boolean;
    procedure UpdateButtonStates;
    procedure UpdatePackageInfo(Tree: TTreeView);
    function NewInstalledPackagesContains(APackageID: TLazPackageID): boolean;
    function IndexOfNewInstalledPackageID(APackageID: TLazPackageID): integer;
    function IndexOfNewInstalledPkgByName(const APackageName: string): integer;
    procedure SavePackageListToFile(const AFilename: string);
    procedure LoadPackageListFromFile(const AFilename: string);
    function ExtractNameFromPkgID(ID: string): string;
    procedure AddToInstall;
    procedure AddToUninstall;
  public
    function GetNewInstalledPackages: TObjectList;
    property OldInstalledPackages: TPkgDependency read FOldInstalledPackages
                                                  write SetOldInstalledPackages;
    property NewInstalledPackages: TObjectList read FNewInstalledPackages;
    property RebuildIDE: boolean read FRebuildIDE write FRebuildIDE;
    property OnCheckInstallPackageList: TOnCheckInstallPackageList
               read FOnCheckInstallPackageList write FOnCheckInstallPackageList;
  end;

function ShowEditInstallPkgsDialog(OldInstalledPackages: TPkgDependency;
  CheckInstallPackageList: TOnCheckInstallPackageList;
  var NewInstalledPackages: TObjectList; // list of TLazPackageID (must be freed)
  var RebuildIDE: boolean): TModalResult;

implementation

{$R *.lfm}

function ShowEditInstallPkgsDialog(OldInstalledPackages: TPkgDependency;
  CheckInstallPackageList: TOnCheckInstallPackageList;
  var NewInstalledPackages: TObjectList; // list of TLazPackageID
  var RebuildIDE: boolean): TModalResult;
var
  InstallPkgSetDialog: TInstallPkgSetDialog;
begin
  NewInstalledPackages:=nil;
  InstallPkgSetDialog:=TInstallPkgSetDialog.Create(nil);
  try
    InstallPkgSetDialog.OldInstalledPackages:=OldInstalledPackages;
//    InstallPkgSetDialog.UpdateAvailablePackages;
    InstallPkgSetDialog.UpdateButtonStates;
    InstallPkgSetDialog.OnCheckInstallPackageList:=CheckInstallPackageList;
    Result:=InstallPkgSetDialog.ShowModal;
    NewInstalledPackages:=InstallPkgSetDialog.GetNewInstalledPackages;
    RebuildIDE:=InstallPkgSetDialog.RebuildIDE;
  finally
    InstallPkgSetDialog.Free;
  end;
end;

{ TInstallPkgSetDialog }

procedure TInstallPkgSetDialog.InstallPkgSetDialogCreate(Sender: TObject);
begin
  InstallTreeView.Images := IDEImages.Images_16;
  AvailableTreeView.Images := IDEImages.Images_16;
  ImgIndexPackage := IDEImages.LoadImage(16, 'item_package');
  ImgIndexInstalledPackage := IDEImages.LoadImage(16, 'pkg_installed');
  ImgIndexInstallPackage := IDEImages.LoadImage(16, 'pkg_package_autoinstall');
  ImgIndexUninstallPackage := IDEImages.LoadImage(16, 'pkg_package_uninstall');
  ImgIndexCirclePackage := IDEImages.LoadImage(16, 'pkg_package_circle');
  ImgIndexMissingPackage := IDEImages.LoadImage(16, 'pkg_conflict');

  Caption:=lisInstallUninstallPackages;
  NoteLabel.Caption:=lisToInstallYouMustCompileAndRestartTheIDE;

  AvailablePkgGroupBox.Caption:=lisDoNotInstall;
  AvailableFilterEdit.OnGetImageIndex:=@ChooseImageIndex;

  ExportButton.Caption:=lisExportList;
  ImportButton.Caption:=lisImportList;
  UninstallButton.Caption:=lisUninstallSelection;
  UninstallButton.LoadGlyphFromLazarusResource('arrow_right');
  InstallPkgGroupBox.Caption:=lisPckEditInstall;
  AddToInstallButton.Caption:=lisInstallSelection;
  AddToInstallButton.LoadGlyphFromLazarusResource('arrow_left');
  PkgInfoGroupBox.Caption := lisPackageInfo;
  SaveAndRebuildButton.Caption:=lisSaveAndRebuildIDE;
  SaveAndExitButton.Caption:=lisSaveAndExitDialog;
  HelpButton.Caption:=lisMenuHelp;
  CancelButton.Caption:=dlgCancel;

  fAvailablePackages:=TAVLTree.Create(@CompareLazPackageIDNames);
  FNewInstalledPackages:=TObjectList.Create(true);
  ActiveControl:=AvailableFilterEdit;
  PkgInfoMemo.Clear;
end;

procedure TInstallPkgSetDialog.InstallButtonClick(Sender: TObject);
begin
  if not CheckSelection then exit;
  RebuildIDE:=true;
  ModalResult:=mrOk;
end;

procedure TInstallPkgSetDialog.InstallTreeViewDblClick(Sender: TObject);
begin
  AddToUninstall;
end;

procedure TInstallPkgSetDialog.AvailableTreeViewSelectionChanged(Sender: TObject);
begin
  UpdateButtonStates;
  UpdatePackageInfo(AvailableTreeView);
end;

procedure TInstallPkgSetDialog.ExportButtonClick(Sender: TObject);
var
  SaveDialog: TSaveDialog;
  AFilename: string;
begin
  SaveDialog:=TSaveDialog.Create(nil);
  try
    InputHistories.ApplyFileDialogSettings(SaveDialog);
    SaveDialog.InitialDir:=GetPrimaryConfigPath;
    SaveDialog.Title:='Export package list (*.xml)';
    SaveDialog.Options:=SaveDialog.Options+[ofPathMustExist];
    if SaveDialog.Execute then begin
      AFilename:=CleanAndExpandFilename(SaveDialog.Filename);
      if ExtractFileExt(AFilename)='' then
        AFilename:=AFilename+'.xml';
      SavePackageListToFile(AFilename);
    end;
    InputHistories.StoreFileDialogSettings(SaveDialog);
  finally
    SaveDialog.Free;
  end;
end;

procedure TInstallPkgSetDialog.HelpButtonClick(Sender: TObject);
begin
  LazarusHelp.ShowHelpForIDEControl(Self);
end;

procedure TInstallPkgSetDialog.ImportButtonClick(Sender: TObject);
var
  OpenDialog: TOpenDialog;
  AFilename: string;
begin
  OpenDialog:=TSaveDialog.Create(nil);
  try
    InputHistories.ApplyFileDialogSettings(OpenDialog);
    OpenDialog.InitialDir:=GetPrimaryConfigPath;
    OpenDialog.Title:='Import package list (*.xml)';
    OpenDialog.Options:=OpenDialog.Options+[ofPathMustExist,ofFileMustExist];
    if OpenDialog.Execute then begin
      AFilename:=CleanAndExpandFilename(OpenDialog.Filename);
      LoadPackageListFromFile(AFilename);
    end;
    InputHistories.StoreFileDialogSettings(OpenDialog);
  finally
    OpenDialog.Free;
  end;
end;

procedure TInstallPkgSetDialog.AddToInstallButtonClick(Sender: TObject);
begin
  AddToInstall;
end;

procedure TInstallPkgSetDialog.AvailableTreeViewDblClick(Sender: TObject);
begin
  AddToInstall;
end;

procedure TInstallPkgSetDialog.InstallPkgSetDialogDestroy(Sender: TObject);
begin
  ClearNewInstalledPackages;
  FNewInstalledPackages.Free;
  fAvailablePackages.Free;
end;

procedure TInstallPkgSetDialog.InstallPkgSetDialogResize(Sender: TObject);
var
  w: Integer;
begin
  w:=ClientWidth div 2-InstallPkgGroupBox.BorderSpacing.Left*3;
  if w<1 then w:=1;
  InstallPkgGroupBox.Width:=w;
end;

procedure TInstallPkgSetDialog.InstallTreeViewSelectionChanged(Sender: TObject);
begin
  UpdateButtonStates;
  UpdatePackageInfo(InstallTreeView);
end;

procedure TInstallPkgSetDialog.SaveAndExitButtonClick(Sender: TObject);
begin
  if not CheckSelection then exit;
  RebuildIDE:=false;
  ModalResult:=mrOk;
end;

procedure TInstallPkgSetDialog.UninstallButtonClick(Sender: TObject);
begin
  AddToUninstall;
end;

procedure TInstallPkgSetDialog.SetOldInstalledPackages(const AValue: TPkgDependency);
begin
  if FOldInstalledPackages=AValue then exit;
  FOldInstalledPackages:=AValue;
  AssignOldInstalledPackagesToList;
end;

procedure TInstallPkgSetDialog.AssignOldInstalledPackagesToList;
var
  Dependency: TPkgDependency;
  Cnt: Integer;
  NewPackageID: TLazPackageID;
begin
  ClearNewInstalledPackages;
  Cnt:=0;
  Dependency:=OldInstalledPackages;
  while Dependency<>nil do begin
    NewPackageID:=TLazPackageID.Create;
    if (Dependency.LoadPackageResult=lprSuccess)
    and (Dependency.RequiredPackage<>nil) then begin
      // packages can be freed while the dialog runs => use packageid instead
      NewPackageID.AssignID(Dependency.RequiredPackage);
    end else begin
      NewPackageID.Name:=Dependency.PackageName;
    end;
    FNewInstalledPackages.Add(NewPackageID);
    Dependency:=Dependency.NextRequiresDependency;
    inc(Cnt);
  end;
  UpdateNewInstalledPackages;
end;

function TInstallPkgSetDialog.PackageInInstallList(PkgName: string): boolean;
var
  i: Integer;
begin
  //for i:=0 to InstallTreeView.Items.TopLvlCount-1 do
    //debugln(['TInstallPkgSetDialog.PackageInInstallList ',i,' ',ExtractNameFromPkgID(InstallTreeView.Items.TopLvlItems[i].Text),' ',PkgName]);
  for i:=0 to InstallTreeView.Items.TopLvlCount-1 do
    if SysUtils.CompareText(
      ExtractNameFromPkgID(InstallTreeView.Items.TopLvlItems[i].Text),PkgName)=0
    then
      exit(true);
  Result:=false;
end;

function TInstallPkgSetDialog.ChooseImageIndex(Str: String; Data: TObject;
                                               var AIsEnabled: Boolean): Integer;
var
  Pkg: TLazPackageID;
  APackage: TLazPackage;
begin
  Pkg:=TLazPackageID(Data);
  Result:=ImgIndexPackage;
  if (Pkg is TLazPackage) then begin
    APackage:=TLazPackage(Pkg);
    if APackage.Installed<>pitNope then
      Result:=ImgIndexUninstallPackage; // is installed and will be uninstalled
  end;
end;

procedure TInstallPkgSetDialog.UpdateAvailablePackages(Immediately: boolean);
var
  ANode: TAVLTreeNode;
  Pkg: TLazPackageID;
  PkgName: String;
  DuplCheck: TStringList;  // Add pkg names also here to filter out duplicates.
  FilteredBranch: TTreeFilterBranch;
begin
  DuplCheck:=TStringList.Create;
  try
    if fAvailablePackages.Count=0 then
      PackageGraph.IteratePackages(fpfSearchAllExisting,@OnIteratePackages);
    FilteredBranch := AvailableFilterEdit.GetBranch(Nil); // All items are top level.
    ANode:=fAvailablePackages.FindLowest;
    while ANode<>nil do begin
      Pkg:=TLazPackageID(ANode.Data);
      if (not (Pkg is TLazPackage))
      or (TLazPackage(Pkg).PackageType in [lptDesignTime,lptRunAndDesignTime])
      then begin
        if (not PackageInInstallList(Pkg.Name)) then begin
          PkgName:=Pkg.IDAsString;
          if (DuplCheck.IndexOf(PkgName)<0) then begin
            DuplCheck.Add(PkgName);
            FilteredBranch.AddNodeData(PkgName, Pkg);
          end;
        end;
      end;
      ANode:=fAvailablePackages.FindSuccessor(ANode);
    end;
    AvailableFilterEdit.InvalidateFilter;
  finally
    DuplCheck.Free;
  end;
end;

procedure TInstallPkgSetDialog.UpdateNewInstalledPackages;
var
  s: String;
  NewPackageID: TLazPackageID;
  i: Integer;
  sl: TStringList;
  TVNode: TTreeNode;
  APackage: TLazPackage;
  ImgIndex: LongInt;
begin
  sl:=TStringList.Create;
  for i:=0 to FNewInstalledPackages.Count-1 do begin
    NewPackageID:=TLazPackageID(FNewInstalledPackages[i]);
    APackage:=PackageGraph.FindPackageWithName(NewPackageID.Name,nil);
    if APackage<>nil then
      NewPackageID:=APackage;
    s:=NewPackageID.IDAsString;
    sl.AddObject(s,NewPackageID);
  end;
  sl.Sort;
  InstallTreeView.BeginUpdate;
  InstallTreeView.Items.Clear;
  for i:=0 to sl.Count-1 do begin
    TVNode:=InstallTreeView.Items.Add(nil,sl[i]);
    NewPackageID:=TLazPackageID(sl.Objects[i]);
    ImgIndex:=ImgIndexInstallPackage;
    if NewPackageID is TLazPackage then begin
      APackage:=TLazPackage(NewPackageID);
      if APackage.Installed<>pitNope then
        ImgIndex:=ImgIndexInstalledPackage;    // stay installed
    end;
    TVNode.ImageIndex:=ImgIndex;
    TVNode.SelectedIndex:=ImgIndex;
  end;
  InstallTreeView.EndUpdate;
  sl.Free;
  UpdateAvailablePackages;
end;

procedure TInstallPkgSetDialog.OnIteratePackages(APackageID: TLazPackageID);
begin
  //debugln('TInstallPkgSetDialog.OnIteratePackages ',APackageID.IDAsString);
  if (fAvailablePackages.Find(APackageID)=nil) then
    fAvailablePackages.Add(APackageID);
end;

function TInstallPkgSetDialog.DependencyToStr(Dependency: TPkgDependency): string;
begin
  Result:='';
  if Dependency=nil then exit;
  if (Dependency.LoadPackageResult=lprSuccess)
  and (Dependency.RequiredPackage<>nil) then
    Result:=Dependency.RequiredPackage.IDAsString
  else
    Result:=Dependency.PackageName;
end;

procedure TInstallPkgSetDialog.ClearNewInstalledPackages;
begin
  FNewInstalledPackages.Clear;
end;

function TInstallPkgSetDialog.CheckSelection: boolean;
begin
  OnCheckInstallPackageList(FNewInstalledPackages,Result);
end;

procedure TInstallPkgSetDialog.UpdateButtonStates;
var
  Cnt: Integer;
  Dependency: TPkgDependency;
  s: String;
  ListChanged: Boolean;
begin
  UninstallButton.Enabled:=InstallTreeView.Selected<>nil;
  AddToInstallButton.Enabled:=AvailableTreeView.Selected<>nil;
  // check for changes
  ListChanged:=false;
  Cnt:=0;
  Dependency:=OldInstalledPackages;
  while Dependency<>nil do begin
    s:=Dependency.PackageName;
    if not PackageInInstallList(s) then begin
      ListChanged:=true;
      break;
    end;
    Dependency:=Dependency.NextRequiresDependency;
    inc(Cnt);
  end;
  if InstallTreeView.Items.TopLvlCount<>Cnt then
    ListChanged:=true;
  SaveAndExitButton.Enabled:=ListChanged;
  SaveAndRebuildButton.Enabled:=ListChanged;
end;

procedure TInstallPkgSetDialog.UpdatePackageInfo(Tree: TTreeView);
var
  PkgName: String;
  PkgID: TLazPackageID;
  Author: String;
  Description: String;
  PkgLink: TPackageLink;
  XMLConfig: TXMLConfig;
begin
  if Tree = nil then Exit;
  PkgName := '';
  if Tree.Selected <> nil then
    PkgName := Tree.Selected.Text;

  if PkgName = '' then Exit;
  if Assigned(FSelectedPkg) and (PkgName = FSelectedPkg.IDAsString) then Exit;
    
  PkgInfoMemo.Clear;
  PkgID := TLazPackageID.Create;
  try
    PkgID.StringToID(PkgName);
    FSelectedPkg := PackageGraph.FindPackageWithID(PkgID);

    Author:='';
    Description:='';
    if FSelectedPkg <> nil then begin
      Author:=FSelectedPkg.Author;
      Description:=FSelectedPkg.Description;
    end else begin
      // package not loaded -> read values from .lpk
      PkgLink:=PkgLinks.FindLinkWithPackageID(PkgID);
      if (PkgLink<>nil) and FileExistsCached(PkgLink.GetEffectiveFilename) then begin
        // load the package file
        try
          XMLConfig:=TXMLConfig.Create(PkgLink.GetEffectiveFilename);
          try
            Author:=XMLConfig.GetValue('Package/Author/Value','');
            Description:=XMLConfig.GetValue('Package/Description/Value','');
          finally
            XMLConfig.Free;
          end;
        except
          on E: Exception do begin
            DebugLn('TInstallPkgSetDialog.UpdatePackageInfo ERROR: ',E.Message);
          end;
        end;
      end;
    end;
    if Author<>'' then
      PkgInfoMemo.Lines.Add(lisPckOptsAuthor + ' ' + Author);
    if Description<>'' then
      PkgInfoMemo.Lines.Add(lisPckOptsDescriptionAbstract
                            + ': ' + Description);
  finally
    PkgId.Free;
  end;
end;

function TInstallPkgSetDialog.NewInstalledPackagesContains(
  APackageID: TLazPackageID): boolean;
begin
  Result:=IndexOfNewInstalledPackageID(APackageID)>=0;
end;

function TInstallPkgSetDialog.IndexOfNewInstalledPackageID(
  APackageID: TLazPackageID): integer;
begin
  Result:=FNewInstalledPackages.Count-1;
  while (Result>=0)
  and (TLazPackageID(FNewInstalledPackages[Result]).Compare(APackageID)<>0) do
    dec(Result);
end;

function TInstallPkgSetDialog.IndexOfNewInstalledPkgByName(
  const APackageName: string): integer;
begin
  Result:=FNewInstalledPackages.Count-1;
  while (Result>=0)
  and (CompareText(TLazPackageID(FNewInstalledPackages[Result]).Name,
       APackageName)<>0)
  do
    dec(Result);
end;

procedure TInstallPkgSetDialog.SavePackageListToFile(const AFilename: string);
var
  XMLConfig: TXMLConfig;
  i: Integer;
  LazPackageID: TLazPackageID;
begin
  try
    XMLConfig:=TXMLConfig.CreateClean(AFilename);
    try
      XMLConfig.SetDeleteValue('Packages/Count',FNewInstalledPackages.Count,0);
      for i:=0 to FNewInstalledPackages.Count-1 do begin
        LazPackageID:=TLazPackageID(FNewInstalledPackages[i]);
        XMLConfig.SetDeleteValue('Packages/Item'+IntToStr(i)+'/ID',
                                 LazPackageID.IDAsString,'');
      end;
      InvalidateFileStateCache;
      XMLConfig.Flush;
    finally
      XMLConfig.Free;
    end;
  except
    on E: Exception do begin
      MessageDlg(lisCodeToolsDefsWriteError,
        Format(lisErrorWritingPackageListToFile, [#13, AFilename, #13, E.Message
          ]), mtError, [mbCancel], 0);
    end;
  end;
end;

procedure TInstallPkgSetDialog.LoadPackageListFromFile(const AFilename: string);
  
  function PkgNameExists(List: TObjectList; ID: TLazPackageID): boolean;
  var
    i: Integer;
    LazPackageID: TLazPackageID;
  begin
    if List<>nil then
      for i:=0 to List.Count-1 do begin
        LazPackageID:=TLazPackageID(List[i]);
        if CompareText(LazPackageID.Name,ID.Name)=0 then begin
          Result:=true;
          exit;
        end;
      end;
    Result:=false;
  end;
  
var
  XMLConfig: TXMLConfig;
  i: Integer;
  LazPackageID: TLazPackageID;
  NewCount: LongInt;
  NewList: TObjectList;
  ID: String;
begin
  NewList:=nil;
  LazPackageID:=nil;
  try
    XMLConfig:=TXMLConfig.Create(AFilename);
    try
      NewCount:=XMLConfig.GetValue('Packages/Count',0);
      LazPackageID:=TLazPackageID.Create;
      for i:=0 to NewCount-1 do begin
        // get ID
        ID:=XMLConfig.GetValue('Packages/Item'+IntToStr(i)+'/ID','');
        if ID='' then continue;
        // parse ID
        if not LazPackageID.StringToID(ID) then continue;
        // ignore doubles
        if PkgNameExists(NewList,LazPackageID) then continue;
        // add
        if NewList=nil then NewList:=TObjectList.Create(true);
        NewList.Add(LazPackageID);
        LazPackageID:=TLazPackageID.Create;
      end;
      // clean up old list
      ClearNewInstalledPackages;
      FNewInstalledPackages.Free;
      // assign new list
      FNewInstalledPackages:=NewList;
      NewList:=nil;
      UpdateNewInstalledPackages;
      UpdateButtonStates;
    finally
      XMLConfig.Free;
      LazPackageID.Free;
      NewList.Free;
    end;
  except
    on E: Exception do begin
      MessageDlg(lisCodeToolsDefsReadError,
        Format(lisErrorReadingPackageListFromFile, [#13, AFilename, #13,
          E.Message]), mtError, [mbCancel], 0);
    end;
  end;
end;

function TInstallPkgSetDialog.ExtractNameFromPkgID(ID: string): string;
var
  p: Integer;
begin
  p:=1;
  while (p<=length(ID)) and IsIdentChar[ID[p]] do inc(p);
  Result:=copy(ID,1,p-1);
end;

procedure TInstallPkgSetDialog.AddToInstall;
var
  i: Integer;
  NewPackageID: TLazPackageID;
  j: LongInt;
  APackage: TLazPackage;
  Additions: TObjectList;
  TVNode: TTreeNode;
  PkgName: String;
begin
  Additions:=TObjectList.Create(false);
  NewPackageID:=TLazPackageID.Create;
  try
    for i:=0 to AvailableTreeView.Items.TopLvlCount-1 do begin
      TVNode:=AvailableTreeView.Items.TopLvlItems[i];
      if not TVNode.MultiSelected then continue;
      PkgName:=TVNode.Text;
      // check string
      if not NewPackageID.StringToID(PkgName) then begin
        TVNode.Selected:=false;
        debugln('TInstallPkgSetDialog.AddToInstallButtonClick invalid ID: ',
                PkgName);
        continue;
      end;
      // check if already in list
      if NewInstalledPackagesContains(NewPackageID) then begin
        MessageDlg('Double',
          'The package '+NewPackageID.Name+' is already in the list',mtError,
          [mbCancel],0);
        TVNode.Selected:=false;
        exit;
      end;
      // check if a package with same name is already in the list
      j:=IndexOfNewInstalledPkgByName(NewPackageID.Name);
      if j>=0 then begin
        MessageDlg('Conflict',
          'There is already a package '+NewPackageID.Name+' in the list',
          mtError,[mbCancel],0);
        TVNode.Selected:=false;
        exit;
      end;
      // check if package is loaded and has some attributes that prevents
      // installation in the IDE
      APackage:=PackageGraph.FindPackageWithID(NewPackageID);
      if APackage<>nil then begin
        if APackage.PackageType=lptRunTime then begin
          MessageDlg('Not a designtime package',
            'The package '+APackage.IDAsString+' is not a design time package.'
            +' It can not be installed in the IDE',mtError,
            [mbCancel],0);
          TVNode.Selected:=false;
          exit;
        end;
      end;
      // ok => add to list
      Additions.Add(NewPackageID);
      NewPackageID:=TLazPackageID.Create;
    end;
    // all ok => add to list
    for i:=0 to Additions.Count-1 do
      FNewInstalledPackages.Add(Additions[i]);
    Additions.Clear;
    UpdateNewInstalledPackages;
    UpdateButtonStates;
  finally
    // clean up
    NewPackageID.Free;
    for i:=0 to Additions.Count-1 do
      TObject(Additions[i]).Free;
    Additions.Free;
  end;
end;

procedure TInstallPkgSetDialog.AddToUninstall;
var
  i: Integer;
  OldPackageID: TLazPackageID;
  APackage: TLazPackage;
  Deletions: TObjectList;
  DelPackageID: TLazPackageID;
  j: LongInt;
  TVNode: TTreeNode;
  PkgName: String;
begin
  OldPackageID := nil;
  Deletions:=TObjectList.Create(true);
  try
    for i:=0 to InstallTreeView.Items.TopLvlCount-1 do begin
      TVNode:=InstallTreeView.Items.TopLvlItems[i];
      if not TVNode.MultiSelected then continue;
      if OldPackageID = nil then
        OldPackageID:=TLazPackageID.Create;
      PkgName:=TVNode.Text;
      if not OldPackageID.StringToID(PkgName) then begin
        TVNode.Selected:=false;
        debugln('TInstallPkgSetDialog.AddToUninstallButtonClick invalid ID: ',
                PkgName);
        continue;
      end;
      // ok => add to deletions
      Deletions.Add(OldPackageID);
      DelPackageID:=OldPackageID;
      OldPackageID := nil;
      // get package
      APackage:=PackageGraph.FindPackageWithID(DelPackageID);
      if APackage<>nil then begin
        // check if package is a base package
        if APackage.AutoCreated
        or PackageGraph.IsStaticBasePackage(APackage.Name) then begin
          TVNode.Selected:=false;
          MessageDlg(lisUninstallImpossible,
            Format(lisThePackageCanNotBeUninstalledBecauseItIsNeededByTh, [
              APackage.Name]), mtError, [mbCancel], 0);
          exit;
        end;
      end;
    end;

    // ok => remove from list
    InstallTreeView.Selected:=nil;
    for i:=0 to Deletions.Count-1 do begin
      DelPackageID:=TLazPackageID(Deletions[i]);
      j:=IndexOfNewInstalledPackageID(DelPackageID);
      FNewInstalledPackages.Delete(j);
    end;

    UpdateNewInstalledPackages;
    UpdateButtonStates;
  finally
    OldPackageID.Free;
    Deletions.Free;
  end;
end;

function TInstallPkgSetDialog.GetNewInstalledPackages: TObjectList;
var
  i: Integer;
  NewPackageID: TLazPackageID;
begin
  Result:=TObjectList.Create(true);
  for i:=0 to FNewInstalledPackages.Count-1 do begin
    NewPackageID:=TLazPackageID.Create;
    NewPackageID.AssignID(TLazPackageID(FNewInstalledPackages[i]));
    Result.Add(NewPackageID);
  end;
end;

end.

