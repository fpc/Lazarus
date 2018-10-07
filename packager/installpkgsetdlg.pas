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
 *   Inc., 51 Franklin Street - Fifth Floor, Boston, MA 02110-1335, USA.   *
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
  Classes, SysUtils, contnrs, Laz_AVL_Tree,
  // LCL
  LCLType, LCLProc, Forms, Controls, Graphics, Dialogs, StdCtrls, Buttons,
  ExtCtrls, ComCtrls, ImgList,
  // LazControls
  TreeFilterEdit,
  // Codetools
  BasicCodeTools,
  // LazUtils
  LazFileUtils, Laz2_XMLCfg,
  // IdeIntf
  PackageDependencyIntf, PackageIntf, IDEImagesIntf, IDEHelpIntf, IDEDialogs, IDEWindowIntf, PackageLinkIntf,
  // IDE
  LazarusIDEStrConsts, InputHistory, LazConf, PackageDefs, PackageSystem, LPKCache, PackageLinks;

type
  TOnCheckInstallPackageList =
    procedure(PkgIDs: TObjectList; RemoveConflicts: boolean; out Ok: boolean) of object;

  { TInstallPkgSetDialog }

  TInstallPkgSetDialog = class(TForm)
    AddToInstallButton: TBitBtn;
    AvailableTreeView: TTreeView;
    AvailablePkgGroupBox: TGroupBox;
    MiddleBevel: TBevel;
    HelpButton: TBitBtn;
    CancelButton: TBitBtn;
    ExportButton: TButton;
    BtnPanel: TPanel;
    InstallTreeView: TTreeView;
    AvailableFilterEdit: TTreeFilterEdit;
    LPKParsingTimer: TTimer;
    NoteLabel: TLabel;
    Panel1: TPanel;
    PkgInfoMemo: TMemo;
    PkgInfoGroupBox: TGroupBox;
    ImportButton: TButton;
    SaveAndExitButton: TBitBtn;
    InstallPkgGroupBox: TGroupBox;
    SaveAndRebuildButton: TBitBtn;
    InstalledFilterEdit: TTreeFilterEdit;
    Splitter1: TSplitter;
    UninstallButton: TBitBtn;
    procedure AddToInstallButtonClick(Sender: TObject);
    function FilterEditGetImageIndex({%H-}Str: String; {%H-}Data: TObject;
      var {%H-}AIsEnabled: Boolean): Integer;
    procedure FormClose(Sender: TObject; var {%H-}CloseAction: TCloseAction);
    procedure InstallTreeViewKeyPress(Sender: TObject; var Key: char);
    procedure LPKParsingTimerTimer(Sender: TObject);
    procedure OnAllLPKParsed(Sender: TObject);
    procedure OnIdle(Sender: TObject; var {%H-}Done: Boolean);
    procedure TreeViewAdvancedCustomDrawItem(Sender: TCustomTreeView;
      Node: TTreeNode; {%H-}State: TCustomDrawState; Stage: TCustomDrawStage;
      var PaintImages, {%H-}DefaultDraw: Boolean);
    procedure AvailableTreeViewDblClick(Sender: TObject);
    procedure AvailableTreeViewKeyPress(Sender: TObject; var Key: char);
    procedure AvailableTreeViewSelectionChanged(Sender: TObject);
    procedure ExportButtonClick(Sender: TObject);
    procedure HelpButtonClick(Sender: TObject);
    procedure ImportButtonClick(Sender: TObject);
    procedure SaveAndRebuildButtonClick(Sender: TObject);
    procedure InstallTreeViewDblClick(Sender: TObject);
    procedure InstallPkgSetDialogCreate(Sender: TObject);
    procedure InstallPkgSetDialogDestroy(Sender: TObject);
    procedure InstallPkgSetDialogShow(Sender: TObject);
    procedure InstallPkgSetDialogResize(Sender: TObject);
    procedure InstallTreeViewSelectionChanged(Sender: TObject);
    procedure SaveAndExitButtonClick(Sender: TObject);
    procedure UninstallButtonClick(Sender: TObject);
  private
    FIdleConnected: boolean;
    FNewInstalledPackages: TObjectList; // list of TLazPackageID (not TLazPackage)
    FOldInstalledPackages: TPkgDependency;
    FOnCheckInstallPackageList: TOnCheckInstallPackageList;
    FRebuildIDE: boolean;
    FSelectedPkgState: TLPKInfoState;
    FSelectedPkgID: string;
    fAvailablePkgsNeedUpdate: boolean;
    ImgIndexPackage: integer;
    ImgIndexInstallPackage: integer;
    ImgIndexInstalledPackage: integer;
    ImgIndexUninstallPackage: integer;
    ImgIndexCirclePackage: integer;
    ImgIndexMissingPackage: integer;
    ImgIndexAvailableOnline: integer;
    ImgIndexOverlayUnknown: integer;
    ImgIndexOverlayBasePackage: integer;
    ImgIndexOverlayFPCPackage: integer;
    ImgIndexOverlayLazarusPackage: integer;
    ImgIndexOverlayDesigntimePackage: integer;
    ImgIndexOverlayRuntimePackage: integer;
    procedure SetIdleConnected(AValue: boolean);
    procedure SetOldInstalledPackages(const AValue: TPkgDependency);
    procedure AssignOldInstalledPackagesToList;
    function PackageInInstallList(PkgName: string): boolean;
    function GetPkgImgIndex(Installed: TPackageInstallType; InInstallList,
      IsOnline: boolean): integer;
    procedure UpdateAvailablePackages(Immediately: boolean = false);
    procedure UpdateNewInstalledPackages;
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
    procedure PkgInfosChanged;
    procedure ChangePkgVersion(PkgInfo: TLPKInfo; NewVersion: TPkgVersion);
    function FindOnlinePackageLink(const AName: String): TPackageLink;
  public
    function GetNewInstalledPackages: TObjectList;
    property OldInstalledPackages: TPkgDependency read FOldInstalledPackages
                                                  write SetOldInstalledPackages;
    property NewInstalledPackages: TObjectList read FNewInstalledPackages; // list of TLazPackageID
    property RebuildIDE: boolean read FRebuildIDE write FRebuildIDE;
    property OnCheckInstallPackageList: TOnCheckInstallPackageList
               read FOnCheckInstallPackageList write FOnCheckInstallPackageList;
    property IdleConnected: boolean read FIdleConnected write SetIdleConnected;
  end;

function ShowEditInstallPkgsDialog(OldInstalledPackages: TPkgDependency;
  CheckInstallPackageList: TOnCheckInstallPackageList;
  var NewInstalledPackages: TObjectList; // list of TLazPackageID (must be freed)
  var RebuildIDE: boolean): TModalResult;

implementation

{$R *.lfm}

procedure SetControlsWidthOnMax(AControls: array of TControl);
var
  i, MaxWidth: Integer;
begin
  MaxWidth:=0;
  for i:=Low(AControls) to High(AControls) do
    if AControls[i].Width>MaxWidth then
      MaxWidth:=AControls[i].Width;
  for i:=Low(AControls) to High(AControls) do
    AControls[i].Constraints.MinWidth:=MaxWidth;  // AutoSize=True
end;

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
  IDEDialogLayoutList.ApplyLayout(Self);

  InstallTreeView.Images := IDEImages.Images_16;
  AvailableTreeView.Images := IDEImages.Images_16;
  ImgIndexPackage := IDEImages.LoadImage('item_package');
  ImgIndexInstalledPackage := IDEImages.LoadImage('pkg_installed');
  ImgIndexInstallPackage := IDEImages.LoadImage('pkg_package_autoinstall');
  ImgIndexUninstallPackage := IDEImages.LoadImage('pkg_package_uninstall');
  ImgIndexCirclePackage := IDEImages.LoadImage('pkg_package_circle');
  ImgIndexMissingPackage := IDEImages.LoadImage('pkg_conflict');
  ImgIndexAvailableOnline := IDEImages.LoadImage('pkg_install');
  ImgIndexOverlayUnknown := IDEImages.LoadImage('state_unknown');
  ImgIndexOverlayBasePackage := IDEImages.LoadImage('pkg_core_overlay');
  ImgIndexOverlayFPCPackage := IDEImages.LoadImage('pkg_fpc_overlay');
  ImgIndexOverlayLazarusPackage := IDEImages.LoadImage('pkg_lazarus_overlay');
  ImgIndexOverlayDesignTimePackage := IDEImages.LoadImage('pkg_design_overlay');
  ImgIndexOverlayRunTimePackage := IDEImages.LoadImage('pkg_runtime_overlay');

  Caption:=lisInstallUninstallPackages;
  NoteLabel.Caption:=lisIDECompileAndRestart;

  AvailablePkgGroupBox.Caption:=lisAvailableForInstallation;

  ExportButton.Caption:=lisExportList;
  ImportButton.Caption:=lisImportList;
  UninstallButton.Caption:=lisUninstallSelection;
  IDEImages.AssignImage(UninstallButton, 'arrow__darkred_right');
  InstallPkgGroupBox.Caption:=lisPckEditInstall;
  AddToInstallButton.Caption:=lisInstallSelection;
  IDEImages.AssignImage(AddToInstallButton, 'arrow__darkgreen_left');
  PkgInfoGroupBox.Caption := lisPackageInfo;
  SaveAndRebuildButton.Caption:=lisSaveAndRebuildIDE;
  SaveAndExitButton.Caption:=lisSaveAndExitDialog;
  HelpButton.Caption:=lisMenuHelp;
  CancelButton.Caption:=lisCancel;

  FNewInstalledPackages:=TObjectList.Create(true);
  PkgInfoMemo.Clear;
  LPKInfoCache.AddOnQueueEmpty(@OnAllLPKParsed);
  LPKInfoCache.StartLPKReaderWithAllAvailable;

  UpdateButtonStates;
end;

procedure TInstallPkgSetDialog.InstallPkgSetDialogDestroy(Sender: TObject);
begin
  LPKInfoCache.EndLPKReader;
  LPKInfoCache.RemoveOnQueueEmpty(@OnAllLPKParsed);
  ClearNewInstalledPackages;
  FreeAndNil(FNewInstalledPackages);
  IdleConnected:=false;
end;

procedure TInstallPkgSetDialog.InstallPkgSetDialogShow(Sender: TObject);
begin
  InstalledFilterEdit.Filter:='';    // (filter) - text is shown after this.
  AvailableFilterEdit.Filter:='';
  SetControlsWidthOnMax([UninstallButton, ImportButton, ExportButton, AddToInstallButton]);
end;

procedure TInstallPkgSetDialog.SaveAndRebuildButtonClick(Sender: TObject);
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
    SaveDialog.Title:=lisExportPackageListXml;
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
  OpenDialog:=TOpenDialog.Create(nil);
  try
    InputHistories.ApplyFileDialogSettings(OpenDialog);
    OpenDialog.InitialDir:=GetPrimaryConfigPath;
    OpenDialog.Title:=lisImportPackageListXml;
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

function TInstallPkgSetDialog.FilterEditGetImageIndex(Str: String;
  Data: TObject; var AIsEnabled: Boolean): Integer;
begin
  Result:=0;
end;

procedure TInstallPkgSetDialog.FormClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  IDEDialogLayoutList.SaveLayout(Self);
end;

procedure TInstallPkgSetDialog.InstallTreeViewKeyPress(Sender: TObject; var Key: char);
begin
  if Key = char(VK_RETURN) then
    AddToUninstall;
end;

procedure TInstallPkgSetDialog.LPKParsingTimerTimer(Sender: TObject);
begin
  UpdateNewInstalledPackages;
  UpdateAvailablePackages;
end;

procedure TInstallPkgSetDialog.OnAllLPKParsed(Sender: TObject);
begin
  LPKParsingTimer.Enabled:=false;
  UpdateNewInstalledPackages;
  UpdateAvailablePackages;
end;

procedure TInstallPkgSetDialog.OnIdle(Sender: TObject; var Done: Boolean);
begin
  if fAvailablePkgsNeedUpdate then
    UpdateAvailablePackages(true);
  IdleConnected:=false;
end;

procedure TInstallPkgSetDialog.TreeViewAdvancedCustomDrawItem(
  Sender: TCustomTreeView; Node: TTreeNode; State: TCustomDrawState;
  Stage: TCustomDrawStage; var PaintImages, DefaultDraw: Boolean);
var
  Info: TLPKInfo;
  NodeRect: TRect;
  x: Integer;
  Images: TCustomImageList;
  CurCanvas: TCanvas;
  y: Integer;
  Tree: TTreeView;
  InLazSrc: Boolean;
  IsBase: Boolean;
  PkgType: TLazPackageType;
  Installed: TPackageInstallType;
  PkgName: String;
  ImgIndex: Integer;
  Unknown: Boolean;
  PackageLink: TPackageLink;
  ImagesRes: TScaledImageListResolution;
begin
  Tree:=Sender as TTreeView;
  if Stage=cdPostPaint then begin
    LPKInfoCache.EnterCritSection;
    try
      Info:=LPKInfoCache.FindPkgInfoWithIDAsString(Node.Text);
      if Info=nil then exit;
      PkgName:=Info.ID.Name;
      Unknown:=not (Info.LPKParsed in [lpkiParsed,lpkiParsedError]);
      InLazSrc:=Info.InLazSrc;
      IsBase:=Info.Base;
      PkgType:=Info.PkgType;
      Installed:=Info.Installed;
    finally
      LPKInfoCache.LeaveCritSection;
    end;
    if Sender = InstallTreeView then
      PackageLink := nil
    else
      PackageLink := FindOnlinePackageLink(Info.ID.Name);
    Images:=Tree.Images;
    if Images = nil then exit;
    ImagesRes := Images.ResolutionForPPI[Tree.ImagesWidth, Font.PixelsPerInch, GetCanvasScaleFactor];
    CurCanvas:=Tree.Canvas;

    NodeRect:=Node.DisplayRect(False);
    x:=Node.DisplayIconLeft+1;
    y:=(NodeRect.Top+NodeRect.Bottom-ImagesRes.Height) div 2;
    // draw image
    ImgIndex:=GetPkgImgIndex(Installed,PackageInInstallList(PkgName), PackageLink <> nil);
    ImagesRes.Draw(CurCanvas,x,y,ImgIndex);
    // draw overlays
    if InLazSrc then
      ImagesRes.Draw(CurCanvas,x,y,ImgIndexOverlayLazarusPackage);
    if IsBase then
      ImagesRes.Draw(CurCanvas,x,y,ImgIndexOverlayBasePackage);
    if PkgType=lptRunTimeOnly then
      ImagesRes.Draw(CurCanvas,x,y,ImgIndexOverlayRuntimePackage);
    if PkgType=lptDesignTime then
      ImagesRes.Draw(CurCanvas,x,y,ImgIndexOverlayDesigntimePackage);
    if Unknown then
      ImagesRes.Draw(CurCanvas,x,y,ImgIndexOverlayUnknown);
  end;
  PaintImages:=false;
end;

procedure TInstallPkgSetDialog.AvailableTreeViewDblClick(Sender: TObject);
begin
  AddToInstall;
end;

procedure TInstallPkgSetDialog.AvailableTreeViewKeyPress(Sender: TObject; var Key: char);
begin
  if Key = char(VK_RETURN) then
    AddToInstall;
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

procedure TInstallPkgSetDialog.SetIdleConnected(AValue: boolean);
begin
  if FIdleConnected=AValue then Exit;
  FIdleConnected:=AValue;
  if IdleConnected then
    Application.AddOnIdleHandler(@OnIdle)
  else
    Application.RemoveOnIdleHandler(@OnIdle);
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
  UpdateAvailablePackages;
end;

function TInstallPkgSetDialog.PackageInInstallList(PkgName: string): boolean;
var
  i: Integer;
begin
  for i:=0 to NewInstalledPackages.Count-1 do
    if SysUtils.CompareText(TLazPackageID(NewInstalledPackages[i]).Name,PkgName)=0 then
      exit(true);
  Result:=false;
end;

function TInstallPkgSetDialog.GetPkgImgIndex(Installed: TPackageInstallType;
  InInstallList, IsOnline: boolean): integer;
begin
  if Installed<>pitNope then begin
    // is not currently installed
    if InInstallList then begin
      // is installed and will be installed
      Result:=ImgIndexPackage;
    end
    else begin
      // is installed and will be uninstalled
      Result:=ImgIndexUninstallPackage;
    end;
  end else begin
    // is currently installed
    if InInstallList then begin
      // is not installed and will be installed
      Result:=ImgIndexInstallPackage;
    end
    else begin
      // is not installed and will be not be installed
      if IsOnline then
        Result := ImgIndexAvailableOnline
      else
        Result:=ImgIndexPackage;
    end;
  end;
end;

function TInstallPkgSetDialog.FindOnlinePackageLink(const AName: String): TPackageLink;
var
  PackageLink: TPackageLink;
  PkgName: String;
  P: Integer;
begin
  Result := nil;
  Exit;
  if OPMInterface = nil then
    Exit;
  PkgName := Trim(AName);
  P := Pos(' ', PkgName);
  if P > 0 then
    PkgName := Copy(PkgName, 1, P - 1);
  PackageLink := OPMInterface.FindOnlineLink(PkgName);
  if PackageLink <> nil then
    Result := PackageLink;
end;


procedure TInstallPkgSetDialog.UpdateAvailablePackages(Immediately: boolean);
var
  ANode: TAvlTreeNode;
  FilteredBranch: TTreeFilterBranch;
  Info: TLPKInfo;
  List: TStringList;
  i: Integer;
  PackageLink: TPackageLink;
begin
  if not Immediately then begin
    fAvailablePkgsNeedUpdate:=true;
    IdleConnected:=true;
    exit;
  end;
  fAvailablePkgsNeedUpdate:=false;
  List:=TStringList.Create;
  try
    // collect available packages, not yet installed
    LPKInfoCache.EnterCritSection;
    try
      ANode:=LPKInfoCache.LPKByID.FindLowest;
      while ANode<>nil do begin
        Info:=TLPKInfo(ANode.Data);
        ANode:=LPKInfoCache.LPKByID.FindSuccessor(ANode);
        PackageLink := FindOnlinePackageLink(Info.ID.Name);
        if (PackageLink <> nil) and (PackageLink.PackageType in [lptDesignTime,lptRunAndDesignTime]) then begin
          if (not PackageInInstallList(Info.ID.Name)) then begin
            Info.PkgType := PackageLink.PackageType;
            Info.ID.Version.Assign(PackageLink.Version);
            List.Add(Info.ID.IDAsString);
            Continue;
          end;
        end;
        if Info.LPKParsed=lpkiParsedError then continue;
        if (Info.LPKParsed in [lpkiNotParsed,lpkiParsing])
        or (Info.PkgType in [lptDesignTime,lptRunAndDesignTime])
        then begin
          if (not PackageInInstallList(Info.ID.Name)) then
            List.Add(Info.ID.IDAsString);
        end;
      end;
    finally
      LPKInfoCache.LeaveCritSection;
    end;
    // fill tree view through FilterEdit
    FilteredBranch := AvailableFilterEdit.GetCleanBranch(Nil); // All items are top level.
    for i:=0 to List.Count-1 do
      FilteredBranch.AddNodeData(List[i],nil);
  finally
    List.Free;
  end;
  AvailableFilterEdit.InvalidateFilter;
end;

procedure TInstallPkgSetDialog.UpdateNewInstalledPackages;
var
  NewPackageID: TLazPackageID;
  APackage: TLazPackage;
  FilteredBranch: TTreeFilterBranch;
  List: TStringList;
  i: Integer;
begin
  List:=TStringList.Create;
  try
    for i:=0 to FNewInstalledPackages.Count-1 do begin
      NewPackageID:=TLazPackageID(FNewInstalledPackages[i]);
      APackage:=PackageGraph.FindPackageWithName(NewPackageID.Name,nil);
      if APackage<>nil then
        NewPackageID:=APackage;
      List.Add(NewPackageID.IDAsString);
    end;
    List.Sort;
    // fill tree view through FilterEdit
    FilteredBranch := InstalledFilterEdit.GetCleanBranch(Nil); // All items are top level.
    for i:=0 to List.Count-1 do
      FilteredBranch.AddNodeData(List[i],nil);
  finally
    List.Free;
  end;
  InstalledFilterEdit.InvalidateFilter;
end;

procedure TInstallPkgSetDialog.PkgInfosChanged;
// called in mainthread after package parser helper thread finished
begin
  UpdateAvailablePackages;
end;

procedure TInstallPkgSetDialog.ChangePkgVersion(PkgInfo: TLPKInfo;
  NewVersion: TPkgVersion);
// called by LPKInfoCache when a lpk has a different version than the IDE list
var
  OldID, NewID: String;

  procedure ChangeTV(TV: TTreeView);
  var
    i: Integer;
    Node: TTreeNode;
  begin
    for i:=0 to TV.Items.TopLvlCount-1 do begin
      Node:=TV.Items.TopLvlItems[i];
      if Node.Text=OldID then
        Node.Text:=NewID;
    end;
  end;

begin
  OldID:=PkgInfo.ID.IDAsString;
  NewID:=PkgInfo.ID.Name+' '+NewVersion.AsString;
  ChangeTV(AvailableTreeView);
  ChangeTV(InstallTreeView);
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
  OnCheckInstallPackageList(FNewInstalledPackages,true,Result);
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
  InfoStr: string;

  procedure AddState(const NewState: string);
  begin
    if (InfoStr<>'') and (InfoStr[length(InfoStr)]<>' ') then
      InfoStr:=InfoStr+', ';
    InfoStr:=InfoStr+NewState;
  end;

var
  PkgID: String;
  Info: TLPKInfo;
  PackageLink: TPackageLink;
  Author, Description, License: String;
begin
  if Tree = nil then Exit;
  PkgID := '';
  if Tree.Selected <> nil then
    PkgID := Tree.Selected.Text;
  if PkgID = '' then Exit;
  if Tree = InstallTreeView then
    PackageLink := nil
  else
    PackageLink := FindOnlinePackageLink(PkgID);
  LPKInfoCache.EnterCritSection;
  try
    Info:=LPKInfoCache.FindPkgInfoWithIDAsString(PkgID);
    if ((Info=nil) and (FSelectedPkgID=''))
    or ((Info<>nil) and (Info.ID.IDAsString=FSelectedPkgID)
                    and (Info.LPKParsed=FSelectedPkgState))
    then
      exit; // no change
    PkgInfoMemo.Clear;
    if (Info=nil) then begin
      FSelectedPkgID:='';
      exit;
    end;
    FSelectedPkgID:=PkgID;

    if Info.LPKParsed=lpkiNotParsed then begin
      LPKInfoCache.ParseLPKInfoInMainThread(Info);
      if FSelectedPkgID='' then begin
        // version has changed
        // => has already triggered an update
        exit;
      end;
    end;

    if PackageLink = nil then
    begin
      Author := Info.Author;
      Description := Info.Description;
      License := Info.License;
    end
    else
    begin
      Author := PackageLink.Author;
      Description := PackageLink.Description;
      License := PackageLink.License;
    end;

    if Author<>'' then
      PkgInfoMemo.Lines.Add(lisPckOptsAuthor + ': ' + Author);
    if Description<>'' then
      PkgInfoMemo.Lines.Add(lisPckOptsDescriptionAbstract + ': ' + Description);
    if License<>'' then
      PkgInfoMemo.Lines.Add(lisPckOptsLicense + ': ' + License);

    PkgInfoMemo.Lines.Add(Format(lisOIPFilename, [Info.LPKFilename]));

    InfoStr:=lisCurrentState;
    if Info.Installed<>pitNope then
    begin
      if PackageInInstallList(Info.ID.Name)=false then
        AddState(lisSelectedForUninstallation);
      AddState(lisInstalled);
    end
    else
    begin
      if PackageInInstallList(Info.ID.Name)=true then
        AddState(lisSelectedForInstallation);
      AddState(lisNotInstalled);
      if PackageLink <> nil then
        AddState(lisOnlinePackage);
    end;
    if Info.Base then
      AddState(lisPckExplBase);
    AddState(LazPackageTypeIdents[Info.PkgType]);
    PkgInfoMemo.Lines.Add(InfoStr);
  finally
    LPKInfoCache.LeaveCritSection;
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
        Format(lisErrorWritingPackageListToFile,[LineEnding,AFilename,LineEnding,E.Message]),
        mtError, [mbCancel], 0);
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
      UpdateAvailablePackages;
      UpdateButtonStates;
    finally
      XMLConfig.Free;
      LazPackageID.Free;
      NewList.Free;
    end;
  except
    on E: Exception do begin
      MessageDlg(lisCodeToolsDefsReadError,
        Format(lisErrorReadingPackageListFromFile,[LineEnding,AFilename,LineEnding,E.Message]),
        mtError, [mbCancel], 0);
    end;
  end;
end;

function TInstallPkgSetDialog.ExtractNameFromPkgID(ID: string): string;
begin
  if ID='' then
    Result:=''
  else
    Result:=GetIdentifier(PChar(ID));
end;

procedure TInstallPkgSetDialog.AddToInstall;

  function SelectionOk(aPackageID: TLazPackageID): Boolean;
  var
    APackage: TLazPackage;
    ConflictDep: TPkgDependency;
    i: Integer;
  begin
    // check if already in list
    if NewInstalledPackagesContains(aPackageID) then begin
      MessageDlg(lisDuplicate,
        Format(lisThePackageIsAlreadyInTheList, [aPackageID.Name]),
        mtError, [mbCancel],0);
      exit(false);
    end;
    // check if a package with same name is already in the list
    i:=IndexOfNewInstalledPkgByName(aPackageID.Name);
    if i>=0 then begin
      MessageDlg(lisConflict,
        Format(lisThereIsAlreadyAPackageInTheList, [aPackageID.Name]),
        mtError,[mbCancel],0);
      exit(false);
    end;
    // check if package is loaded and has some attributes that prevents
    // installation in the IDE
    APackage:=PackageGraph.FindPackageWithID(aPackageID);
    if APackage<>nil then begin
      if APackage.PackageType in [lptRunTime,lptRunTimeOnly] then begin
        IDEMessageDialog(lisNotADesigntimePackage,
          Format(lisThePackageIsNotADesignTimePackageItCanNotBeInstall,
                 [APackage.IDAsString]),
          mtError, [mbCancel]);
        exit(false);
      end;
      ConflictDep:=PackageGraph.FindRuntimePkgOnlyRecursively(
        APackage.FirstRequiredDependency);
      if ConflictDep<>nil then begin
        IDEMessageDialog(lisNotADesigntimePackage,
          Format(lisThePackageCanNotBeInstalledBecauseItRequiresWhichI,
            [APackage.Name, ConflictDep.AsString]),
          mtError, [mbCancel]);
        exit(false);
      end;
    end;
    Result:=true;
  end;

var
  i, j: Integer;
  NewSelectedIndex, LastNonSelectedIndex: Integer;
  NewPackageID: TLazPackageID;
  Additions: TObjectList;
  AddedPkgNames: TStringList;
  TVNode: TTreeNode;
  PkgName: String;
  FilteredBranch: TTreeFilterBranch;
  PkgLinks: TList;
  PkgLinksStr: String;
  PkgLink: TPackageLink;
begin
  NewSelectedIndex:=-1;
  LastNonSelectedIndex:=-1;
  Additions:=TObjectList.Create(false);
  AddedPkgNames:=TStringList.Create;
  PkgLinks := TList.Create;
  NewPackageID:=TLazPackageID.Create;
  FilteredBranch := AvailableFilterEdit.GetExistingBranch(Nil); // All items are top level.
  try
    for i:=0 to AvailableTreeView.Items.TopLvlCount-1 do
    begin
      TVNode:=AvailableTreeView.Items.TopLvlItems[i];
      if not TVNode.MultiSelected then begin
        LastNonSelectedIndex:=i;
        continue;
      end;
      NewSelectedIndex:=i+1; // Will have the next index after the selected one.
      PkgName:=TVNode.Text;
      // Convert package name to ID and check it
      if not NewPackageID.StringToID(PkgName) then begin
        TVNode.Selected:=false;
        DebugLn('TInstallPkgSetDialog.AddToInstall invalid ID: ', PkgName);
        continue;
      end;
      if not SelectionOk(NewPackageID) then begin
        TVNode.Selected:=false;
        exit;
      end;
      // ok => add to list
      PkgLink := FindOnlinePackageLink(NewPackageID.Name);
      if PkgLink <> nil then begin
        if not FileExists(PkgLink.OPMFileName) then
          PkgLinks.Add(PkgLink)
        else
        begin
          PkgLink := LazPackageLinks.AddUserLink(PkgLink.OPMFileName, PkgLink.Name);
          if PkgLink <> nil then
            LazPackageLinks.SaveUserLinks;
          Additions.Add(NewPackageID);
          NewPackageID:=TLazPackageID.Create;
          AddedPkgNames.Add(PkgName);
        end;
      end else begin
        Additions.Add(NewPackageID);
        NewPackageID:=TLazPackageID.Create;
        AddedPkgNames.Add(PkgName);
      end;
    end;
    //download online packages
    if (OPMInterface <> nil) and (PkgLinks.Count > 0) then
    begin
      PkgLinksStr := '';
      for I := 0 to PkgLinks.Count - 1 do begin
        if PkgLinksStr = '' then
          PkgLinksStr := '"' + TPackageLink(PkgLinks.Items[I]).Name + '"'
        else
          PkgLinksStr := PkgLinksStr + ', ' + '"' + TPackageLink(PkgLinks.Items[I]).Name + '"';
      end;
      if IDEMessageDialog(lisDownload, Format(lisDonwloadOnlinePackages, [PkgLinksStr]), mtConfirmation, [mbYes, mbNo]) = mrYes then begin
        if OPMInterface.DownloadPackages(PkgLinks) = mrOK then begin
          for I := PkgLinks.Count - 1 downto 0 do begin
            if OPMInterface.IsPackageAvailable(TPackageLink(PkgLinks.Items[I]), 1) then begin
              Additions.Add(NewPackageID);
              NewPackageID:=TLazPackageID.Create;
              AddedPkgNames.Add(PkgName);
              PkgLink := LazPackageLinks.AddUserLink(TPackageLink(PkgLinks.Items[I]).OPMFileName, TPackageLink(PkgLinks.Items[I]).Name);
              if PkgLink <> nil then
                LazPackageLinks.SaveUserLinks;
            end;
          end;
        end
        else
          Dec(NewSelectedIndex);
      end
      else
        Dec(NewSelectedIndex);
    end;
    // all ok => add to installed packages
    for i:=0 to Additions.Count-1 do
      FNewInstalledPackages.Add(Additions[i]);
    for i:=0 to AddedPkgNames.Count-1 do begin
      j:=FilteredBranch.Items.IndexOf(AddedPkgNames[i]);
      Assert(j<>-1, 'TInstallPkgSetDialog.AddToInstall: '+AddedPkgNames[i]+' not found in Filter items.');
      FilteredBranch.Items.Delete(j);
    end;
    // Don't call UpdateAvailablePackages here, only the selected nodes were removed.
    UpdateNewInstalledPackages;
    UpdateButtonStates;
    if ((NewSelectedIndex=-1) or (NewSelectedIndex=AvailableTreeView.Items.TopLvlCount)) then
      NewSelectedIndex:=LastNonSelectedIndex;
    if NewSelectedIndex<>-1 then
      AvailableTreeView.Items.TopLvlItems[NewSelectedIndex].Selected:=True;
    AvailableFilterEdit.InvalidateFilter;
  finally
    NewPackageID.Free;
    AddedPkgNames.Free;
    Additions.Free;
    PkgLinks.Free;
  end;
end;

procedure TInstallPkgSetDialog.AddToUninstall;

  function SelectionOk(aPackageID: TLazPackageID): Boolean;
  var
    APackage: TLazPackage;
  begin
    APackage:=PackageGraph.FindPackageWithID(aPackageID);
    if APackage<>nil then begin
      // check if package is a base package
      if PackageGraph.IsStaticBasePackage(APackage.Name) then begin
        MessageDlg(lisUninstallImpossible,
          Format(lisThePackageCanNotBeUninstalledBecauseItIsNeededByTh, [
            APackage.Name]), mtError, [mbCancel], 0);
        exit(false);
      end;
    end;
    Result:=true;
  end;

var
  i, j: Integer;
  NewSelectedIndex, LastNonSelectedIndex: Integer;
  DelPackageID, PackID: TLazPackageID;
  Deletions: TObjectList;
  DeletedPkgNames: TStringList;
  TVNode: TTreeNode;
  PkgName: String;
  FilteredBranch: TTreeFilterBranch;
begin
  NewSelectedIndex:=-1;
  LastNonSelectedIndex:=-1;
  Deletions:=TObjectList.Create(true);
  DeletedPkgNames:=TStringList.Create;
  DelPackageID:=TLazPackageID.Create;
  FilteredBranch := InstalledFilterEdit.GetExistingBranch(Nil); // All items are top level.
  try
    for i:=0 to InstallTreeView.Items.TopLvlCount-1 do begin
      TVNode:=InstallTreeView.Items.TopLvlItems[i];
      if not TVNode.MultiSelected then begin
        LastNonSelectedIndex:=i;
        continue;
      end;
      NewSelectedIndex:=i+1; // Will have the next index after the selected one.
      PkgName:=TVNode.Text;
      if not DelPackageID.StringToID(PkgName) then begin
        TVNode.Selected:=false;
        debugln('TInstallPkgSetDialog.AddToUninstall invalid ID: ', PkgName);
        continue;
      end;
      if not SelectionOk(DelPackageID) then begin
        TVNode.Selected:=false;
        exit;
      end;
      // ok => add to deletions
      Deletions.Add(DelPackageID);
      DelPackageID:=TLazPackageID.Create;
      DeletedPkgNames.Add(PkgName);
    end;

    // ok => remove from installed packages
    InstallTreeView.Selected:=nil;
    for i:=0 to Deletions.Count-1 do begin
      PackID:=TLazPackageID(Deletions[i]);
      j:=IndexOfNewInstalledPackageID(PackID);
      FNewInstalledPackages.Delete(j);
    end;
    for i:=0 to DeletedPkgNames.Count-1 do begin
      j:=FilteredBranch.Items.IndexOf(DeletedPkgNames[i]);
      Assert(j<>-1, 'TInstallPkgSetDialog.AddToUninstall: '+DeletedPkgNames[i]+' not found in Filter items.');
      FilteredBranch.Items.Delete(j);
    end;

    // Don't call UpdateNewInstalledPackages here, only the selected nodes were removed.
    UpdateAvailablePackages;
    UpdateButtonStates;
    if ((NewSelectedIndex=-1) or (NewSelectedIndex=InstallTreeView.Items.TopLvlCount)) then
      NewSelectedIndex:=LastNonSelectedIndex;
    if NewSelectedIndex<>-1 then
      InstallTreeView.Items.TopLvlItems[NewSelectedIndex].Selected:=True;
    InstalledFilterEdit.InvalidateFilter;
  finally
    DelPackageID.Free;
    DeletedPkgNames.Free;
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

