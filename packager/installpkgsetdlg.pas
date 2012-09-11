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
  LCLType, ImgList, AvgLvlTree, Laz2_XMLCfg, LazUTF8, TreeFilterEdit,
  PackageIntf, IDEImagesIntf, IDEHelpIntf, IDEDialogs, LazarusIDEStrConsts,
  EnvironmentOpts, InputHistory, LazConf, IDEProcs, PackageDefs, PackageSystem,
  PackageLinks, IDEContextHelpEdit;

type
  TOnCheckInstallPackageList =
    procedure(PkgIDs: TObjectList; RemoveConflicts: boolean; out Ok: boolean) of object;

  TIPSInfoState = (
    ipisNotParsed,
    ipisParsingMainThread,
    ipisParsingHelperThread,
    ipisParsedError,
    ipisParsed
    );

  { TIPSPkgInfo }

  TIPSPkgInfo = class
  public
    ID: TLazPackageID;
    LPKFilename: string;
    InLazSrc: boolean; // lpk is in lazarus source directory
    Installed: TPackageInstallType;
    Base: boolean; // is base package, can not be uninstalled

    LPKParsed: TIPSInfoState;
    LPKError: string;

    // the below is only valid if TIPSInfoState=ipisParsed
    Author: string;
    Description: string;
    License: string;
    PkgType: TLazPackageType; // design, runtime

    constructor Create(TheID: TLazPackageID);
    destructor Destroy; override;
  end;

  TInstallPkgSetDialog = class;

  { TIPSLPKReader }

  TIPSLPKReader = class(TThread)
  protected
    procedure SynChangePkgVersion;
    procedure SynPkgInfosChanged;
    procedure Log(Msg: string);
    procedure Execute; override;
  public
    Dialog: TInstallPkgSetDialog;
    NewVersion: TPkgVersion;
    Info: TIPSPkgInfo; // currently processed info
    Abort: boolean;
    PkgIDQueue: TStrings; // list of package ids to parse by the lpkreader thread
    destructor Destroy; override;
  end;


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
    procedure TreeViewAdvancedCustomDrawItem(Sender: TCustomTreeView;
      Node: TTreeNode; State: TCustomDrawState; Stage: TCustomDrawStage;
      var PaintImages, DefaultDraw: Boolean);
    procedure AvailableTreeViewDblClick(Sender: TObject);
    procedure AvailableTreeViewKeyPress(Sender: TObject; var Key: char);
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
    // multi threaded stuff
    FCritSec: TRTLCriticalSection;
    FLPKReader: TIPSLPKReader;
    fAvailablePackages: TAvgLvlTree; // tree of TIPSPkgInfo (all available packages and links)
    FSelectedPkg: TIPSPkgInfo;
    procedure StartLPKReader;
    procedure EndLPKReader;
    procedure ParseLPK(LPKFilename: string;
      out ErrorMsg, Author, License, Description: string;
      out PkgType: TLazPackageType;
      var Version: TPkgVersion); // called by main and helper thread
    function FindPkgInfo(ID: string): TIPSPkgInfo; // requires crit sec
    function FindPkgInfo(PkgID: TLazPackageID): TIPSPkgInfo; // requires crit sec
  private
    FNewInstalledPackages: TObjectList; // list of TLazPackageID (not TLazPackage)
    FOldInstalledPackages: TPkgDependency;
    FOnCheckInstallPackageList: TOnCheckInstallPackageList;
    FRebuildIDE: boolean;
    ImgIndexPackage: integer;
    ImgIndexInstallPackage: integer;
    ImgIndexInstalledPackage: integer;
    ImgIndexUninstallPackage: integer;
    ImgIndexCirclePackage: integer;
    ImgIndexMissingPackage: integer;
    ImgIndexOverlayBasePackage: integer;
    ImgIndexOverlayFPCPackage: integer;
    ImgIndexOverlayLazarusPackage: integer;
    ImgIndexOverlayDesigntimePackage: integer;
    ImgIndexOverlayRuntimePackage: integer;
    procedure SetOldInstalledPackages(const AValue: TPkgDependency);
    procedure AssignOldInstalledPackagesToList;
    function PackageInInstallList(PkgName: string): boolean;
    function GetPkgImgIndex(Installed: TPackageInstallType; InInstallList: boolean): integer;
    procedure UpdateAvailablePackages(Immediately: boolean = false);
    procedure UpdateNewInstalledPackages;
    procedure OnIterateAvailablePackages(APackageID: TLazPackageID);
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
    procedure ParseLPKMainThread(PkgInfo: TIPSPkgInfo);
    procedure PkgInfosChanged;
    procedure ChangePkgVersion(PkgInfo: TIPSPkgInfo; NewVersion: TPkgVersion);
  public
    function GetNewInstalledPackages: TObjectList;
    procedure EnterCritSection;
    procedure LeaveCritSection;
    property OldInstalledPackages: TPkgDependency read FOldInstalledPackages
                                                  write SetOldInstalledPackages;
    property NewInstalledPackages: TObjectList read FNewInstalledPackages; // list of TLazPackageID
    property RebuildIDE: boolean read FRebuildIDE write FRebuildIDE;
    property OnCheckInstallPackageList: TOnCheckInstallPackageList
               read FOnCheckInstallPackageList write FOnCheckInstallPackageList;
  end;

function ShowEditInstallPkgsDialog(OldInstalledPackages: TPkgDependency;
  CheckInstallPackageList: TOnCheckInstallPackageList;
  var NewInstalledPackages: TObjectList; // list of TLazPackageID (must be freed)
  var RebuildIDE: boolean): TModalResult;

function CompareIPSPkgInfos(PkgInfo1, PkgInfo2: Pointer): integer;
function ComparePkgIDWithIPSPkgInfo(PkgID, PkgInfo: Pointer): integer;

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

function CompareIPSPkgInfos(PkgInfo1, PkgInfo2: Pointer): integer;
var
  Info1: TIPSPkgInfo absolute PkgInfo1;
  Info2: TIPSPkgInfo absolute PkgInfo2;
begin
  Result:=CompareLazPackageIDNames(Info1.ID,Info2.ID);
end;

function ComparePkgIDWithIPSPkgInfo(PkgID, PkgInfo: Pointer): integer;
var
  ID: TLazPackageID absolute PkgID;
  Info: TIPSPkgInfo absolute PkgInfo;
begin
  Result:=CompareLazPackageIDNames(ID,Info.ID);
end;

{ TIPSLPKReader }

procedure TIPSLPKReader.Execute;
var
  ErrorMsg: string;
  Author, License, Description: string;
  PkgType: TLazPackageType;
begin
  try
    while not Abort do begin
      // get next lpk to parse
      Dialog.EnterCritSection;
      try
        Info:=nil;
        while PkgIDQueue.Count>0 do begin
          Info:=Dialog.FindPkgInfo(PkgIDQueue[PkgIDQueue.Count-1]);
          PkgIDQueue.Delete(PkgIDQueue.Count-1);
          if Info=nil then continue;
          if Info.LPKParsed=ipisNotParsed then begin
            Info.LPKParsed:=ipisParsingHelperThread;
            break;
          end else
            Info:=nil;
        end;
        if Info=nil then break;
      finally
        Dialog.LeaveCritSection;
      end;
      // parse lpk
      NewVersion:=TPkgVersion.Create;
      try
        Dialog.ParseLPK(Info.LPKFilename,ErrorMsg,Author,License,Description,PkgType,NewVersion);
        Dialog.EnterCritSection;
        try
          if ErrorMsg<>'' then begin
            Info.LPKError:=ErrorMsg;
            Info.LPKParsed:=ipisParsedError;
          end else begin
            Info.Author:=Author;
            Info.Description:=Description;
            Info.License:=License;
            Info.PkgType:=PkgType;
            Info.LPKParsed:=ipisParsed;
          end;
          if Dialog.FSelectedPkg=Info then
            Dialog.FSelectedPkg:=nil;
        finally
          Dialog.LeaveCritSection;
        end;
        if NewVersion.Compare(Info.ID.Version)<>0 then begin
          Synchronize(@SynChangePkgVersion);
        end;
      finally
        FreeAndNil(NewVersion);
      end;
      Info:=nil;
    end;
  except
    on E: Exception do begin
      Log('ERROR: TIPSLPKReader.Execute: '+E.Message);
    end;
  end;

  Synchronize(@SynPkgInfosChanged);

  Dialog.EnterCritSection;
  try
    Dialog.FLPKReader:=nil;
  finally
    Dialog.LeaveCritSection;
  end;
end;

procedure TIPSLPKReader.SynChangePkgVersion;
begin
  Dialog.ChangePkgVersion(Info,NewVersion);
end;

procedure TIPSLPKReader.SynPkgInfosChanged;
begin
  Dialog.PkgInfosChanged;
end;

procedure TIPSLPKReader.Log(Msg: string);
begin
  debugln(['TIPSLPKReader.Log: ',Msg]);
end;

destructor TIPSLPKReader.Destroy;
begin
  FreeAndNil(PkgIDQueue);
  inherited Destroy;
end;

{ TIPSPkgInfo }

constructor TIPSPkgInfo.Create(TheID: TLazPackageID);
begin
  ID:=TLazPackageID.Create;
  ID.AssignID(TheID);
end;

destructor TIPSPkgInfo.Destroy;
begin
  FreeAndNil(ID);
  inherited Destroy;
end;

{ TInstallPkgSetDialog }

procedure TInstallPkgSetDialog.InstallPkgSetDialogCreate(Sender: TObject);
begin
  InitCriticalSection(FCritSec);

  InstallTreeView.Images := IDEImages.Images_16;
  AvailableTreeView.Images := IDEImages.Images_16;
  ImgIndexPackage := IDEImages.LoadImage(16, 'item_package');
  ImgIndexInstalledPackage := IDEImages.LoadImage(16, 'pkg_installed');
  ImgIndexInstallPackage := IDEImages.LoadImage(16, 'pkg_package_autoinstall');
  ImgIndexUninstallPackage := IDEImages.LoadImage(16, 'pkg_package_uninstall');
  ImgIndexCirclePackage := IDEImages.LoadImage(16, 'pkg_package_circle');
  ImgIndexMissingPackage := IDEImages.LoadImage(16, 'pkg_conflict');
  ImgIndexOverlayBasePackage := IDEImages.LoadImage(16, 'pkg_core_overlay');
  ImgIndexOverlayFPCPackage := IDEImages.LoadImage(16, 'pkg_fpc_overlay');
  ImgIndexOverlayLazarusPackage := IDEImages.LoadImage(16, 'pkg_lazarus_overlay');
  ImgIndexOverlayDesignTimePackage := IDEImages.LoadImage(16, 'pkg_design_overlay');
  ImgIndexOverlayRunTimePackage := IDEImages.LoadImage(16, 'pkg_runtime_overlay');

  Caption:=lisInstallUninstallPackages;
  NoteLabel.Caption:=lisToInstallYouMustCompileAndRestartTheIDE;

  AvailablePkgGroupBox.Caption:=lisDoNotInstall;

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
  CancelButton.Caption:=lisCancel;

  fAvailablePackages:=TAvgLvlTree.Create(@CompareIPSPkgInfos);
  PackageGraph.IteratePackages(fpfSearchAllExisting,@OnIterateAvailablePackages);
  FNewInstalledPackages:=TObjectList.Create(true);
  PkgInfoMemo.Clear;

  UpdateButtonStates;

  ActiveControl:=AvailableFilterEdit;
  StartLPKReader;
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

procedure TInstallPkgSetDialog.TreeViewAdvancedCustomDrawItem(
  Sender: TCustomTreeView; Node: TTreeNode; State: TCustomDrawState;
  Stage: TCustomDrawStage; var PaintImages, DefaultDraw: Boolean);
var
  Info: TIPSPkgInfo;
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
begin
  Tree:=Sender as TTreeView;
  if Stage=cdPostPaint then begin
    EnterCritSection;
    try
      Info:=FindPkgInfo(Node.Text);
      if Info=nil then exit;
      PkgName:=Info.ID.Name;
      InLazSrc:=Info.InLazSrc;
      IsBase:=Info.Base;
      PkgType:=Info.PkgType;
      Installed:=Info.Installed;
    finally
      LeaveCritSection;
    end;
    Images:=Tree.Images;
    CurCanvas:=Tree.Canvas;

    NodeRect:=Node.DisplayRect(False);
    x:=Node.DisplayIconLeft+1;
    y:=(NodeRect.Top+NodeRect.Bottom-Images.Height) div 2;
    // draw image
    ImgIndex:=GetPkgImgIndex(Installed,PackageInInstallList(PkgName));
    Images.Draw(CurCanvas,x,y,ImgIndex);
    // draw overlays
    if InLazSrc then
      Images.Draw(CurCanvas,x,y,ImgIndexOverlayLazarusPackage);
    if IsBase then
      Images.Draw(CurCanvas,x,y,ImgIndexOverlayBasePackage);
    if PkgType=lptRunTimeOnly then
      Images.Draw(CurCanvas,x,y,ImgIndexOverlayRuntimePackage);
    if PkgType=lptDesignTime then
      Images.Draw(CurCanvas,x,y,ImgIndexOverlayDesigntimePackage);
  end;
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

procedure TInstallPkgSetDialog.InstallPkgSetDialogDestroy(Sender: TObject);
begin
  EndLPKReader;
  ClearNewInstalledPackages;
  FreeAndNil(FNewInstalledPackages);
  fAvailablePackages.FreeAndClear;
  FreeAndNil(fAvailablePackages);

  DoneCriticalsection(FCritSec);
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

procedure TInstallPkgSetDialog.StartLPKReader;
var
  Node: TAvgLvlTreeNode;
  Info: TIPSPkgInfo;
begin
  if FLPKReader<>nil then exit;
  FLPKReader:=TIPSLPKReader.Create(true);
  FLPKReader.Dialog:=Self;
  FLPKReader.FreeOnTerminate:=true;
  FLPKReader.PkgIDQueue:=TStringList.Create;
  for Node in fAvailablePackages.GetEnumeratorHighToLow do begin
    Info:=TIPSPkgInfo(Node.Data);
    if Info.LPKParsed<>ipisNotParsed then continue;
    FLPKReader.PkgIDQueue.Add(Info.ID.IDAsString);
  end;
  FLPKReader.Start;
end;

procedure TInstallPkgSetDialog.EndLPKReader;
begin
  EnterCritSection;
  try
    if FLPKReader=nil then exit;
    FLPKReader.Abort:=true;
  finally
    LeaveCritSection;
  end;
  while FLPKReader<>nil do
    Sleep(10);
end;

procedure TInstallPkgSetDialog.ParseLPK(LPKFilename: string; out ErrorMsg,
  Author, License, Description: string; out PkgType: TLazPackageType; var
  Version: TPkgVersion);
var
  Path: String;
  XMLConfig: TXMLConfig;
  FileVersion: Integer;
begin
  ErrorMsg:='';
  Author:='';
  License:='';
  Description:='';
  PkgType:=lptRunAndDesignTime;
  if FilenameIsAbsolute(LPKFilename) and FileExistsUTF8(LPKFilename) then begin
    // load the package file
    try
      XMLConfig:=TXMLConfig.Create(LPKFilename);
      try
        Path:='Package/';
        FileVersion:=XMLConfig.GetValue(Path+'Version',0);
        Author:=XMLConfig.GetValue(Path+'Author/Value','');
        Description:=XMLConfig.GetValue(Path+'Description/Value','');
        License:=XMLConfig.GetValue(Path+'License/Value','');
        PkgType:=LazPackageTypeIdentToType(XMLConfig.GetValue(Path+'Type/Value',
                                                LazPackageTypeIdents[lptRunTime]));
        PkgVersionLoadFromXMLConfig(Version,XMLConfig,Path+'Version/',FileVersion);
      finally
        XMLConfig.Free;
      end;
    except
      on E: Exception do begin
        ErrorMsg:='file="'+LPKFilename+'": '+E.Message;
        debugln('TInstallPkgSetDialog.ParseLPK ERROR: '+ErrorMsg);
      end;
    end;
  end else begin
    ErrorMsg:='file not found "'+LPKFilename+'"';
  end;
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
  for i:=0 to NewInstalledPackages.Count-1 do
    if SysUtils.CompareText(TLazPackageID(NewInstalledPackages[i]).Name,PkgName)=0
    then
      exit(true);
  Result:=false;
end;

function TInstallPkgSetDialog.GetPkgImgIndex(Installed: TPackageInstallType;
  InInstallList: boolean): integer;
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
      Result:=ImgIndexPackage;
    end;
  end;
end;

procedure TInstallPkgSetDialog.UpdateAvailablePackages(Immediately: boolean);
var
  ANode: TAvgLvlTreeNode;
  FilteredBranch: TTreeFilterBranch;
  Info: TIPSPkgInfo;
  List: TStringList;
  i: Integer;
begin
  List:=TStringList.Create;
  try
    // collect available packages, not yet installed
    EnterCritSection;
    try
      ANode:=fAvailablePackages.FindLowest;
      while ANode<>nil do begin
        Info:=TIPSPkgInfo(ANode.Data);
        if (Info.LPKParsed in [ipisNotParsed,ipisParsingHelperThread])
        or (Info.PkgType in [lptDesignTime,lptRunAndDesignTime])
        then begin
          if (not PackageInInstallList(Info.ID.Name)) then begin
            List.Add(Info.ID.IDAsString);
          end;
        end;
        ANode:=fAvailablePackages.FindSuccessor(ANode);
      end;
    finally
      LeaveCritSection;
    end;
    // fill tree view
    FilteredBranch := AvailableFilterEdit.GetBranch(Nil); // All items are top level.
    for i:=0 to List.Count-1 do
      FilteredBranch.AddNodeData(List[i],nil);
  finally
    List.Free;
  end;
  AvailableFilterEdit.InvalidateFilter;
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
      ImgIndex:=GetPkgImgIndex(APackage.Installed,true);
    end;
    TVNode.ImageIndex:=ImgIndex;
    TVNode.SelectedIndex:=ImgIndex;
  end;
  InstallTreeView.EndUpdate;
  sl.Free;
  UpdateAvailablePackages;
end;

procedure TInstallPkgSetDialog.PkgInfosChanged;
// called in mainthread after helper thread finished
begin
  AvailableTreeView.Invalidate;
  InstallTreeView.Invalidate;
end;

procedure TInstallPkgSetDialog.ChangePkgVersion(PkgInfo: TIPSPkgInfo;
  NewVersion: TPkgVersion);
// called by helper thread via synchronize or by main thread
// => only main thread is active at the moment
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
  if PkgInfo.ID.Version.Compare(NewVersion)=0 then exit;
  OldID:=PkgInfo.ID.IDAsString;
  fAvailablePackages.Remove(PkgInfo);
  PkgInfo.ID.Version.Assign(NewVersion);
  fAvailablePackages.Add(PkgInfo);
  NewID:=PkgInfo.ID.IDAsString;
  ChangeTV(AvailableTreeView);
  ChangeTV(InstallTreeView);
end;

procedure TInstallPkgSetDialog.OnIterateAvailablePackages(APackageID: TLazPackageID);
// called in FormCreate, helper thread not yet running, so no crit sec needed
var
  Info: TIPSPkgInfo;
  Pkg: TLazPackage;
  OldInfo: TIPSPkgInfo;
  Link: TPackageLink;
begin
  //debugln('TInstallPkgSetDialog.OnIteratePackages ',APackageID.IDAsString);
  if APackageID=nil then exit;
  OldInfo:=FindPkgInfo(APackageID);
  if (OldInfo<>nil) and (OldInfo.LPKParsed=ipisParsed) then begin
    // old is good enough => ignore duplicate
    exit;
  end;

  if APackageID is TLazPackage then begin
    // a loaded package
    Pkg:=TLazPackage(APackageID);
    Info:=TIPSPkgInfo.Create(APackageID);
    Info.LPKFilename:=Pkg.Filename;
    Info.Installed:=Pkg.Installed;
    Info.PkgType:=Pkg.PackageType;
    Info.Author:=Pkg.Author;
    Info.Description:=Pkg.Description;
    Info.License:=Pkg.License;
    Info.LPKParsed:=ipisParsed;
  end else if APackageID is TPackageLink then begin
    // only a link to a package
    Link:=TPackageLink(APackageID);
    Info:=TIPSPkgInfo.Create(APackageID);
    Info.LPKFilename:=Link.GetEffectiveFilename;
  end else
    exit;

  if OldInfo<>nil then begin
    if (Info.LPKParsed=ipisParsed)
    or (not FileExistsCached(OldInfo.LPKFilename)) then begin
      // the new info is better => remove old
      fAvailablePackages.Remove(OldInfo);
      OldInfo.Free;
    end else begin
      Info.Free;
      exit;
    end;
  end;

  Info.InLazSrc:=FileIsInPath(Info.LPKFilename,EnvironmentOptions.GetParsedLazarusDirectory);
  Info.Base:=PackageGraph.IsStaticBasePackage(Info.ID.Name);

  fAvailablePackages.Add(Info);
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
begin
  if Tree = nil then Exit;
  PkgID := '';
  if Tree.Selected <> nil then
    PkgID := Tree.Selected.Text;
  if PkgID = '' then Exit;

  EnterCritSection;
  try
    if Assigned(FSelectedPkg) and (PkgID = FSelectedPkg.ID.IDAsString) then
      exit;
    PkgInfoMemo.Clear;

    FSelectedPkg:=FindPkgInfo(PkgID);
    if FSelectedPkg=nil then exit;

    if FSelectedPkg.LPKParsed=ipisNotParsed then begin
      ParseLPKMainThread(FSelectedPkg);
      if FSelectedPkg=nil then exit;
    end;

    if FSelectedPkg.Author<>'' then
      PkgInfoMemo.Lines.Add(lisPckOptsAuthor + ': ' + FSelectedPkg.Author);
    if FSelectedPkg.Description<>'' then
      PkgInfoMemo.Lines.Add(lisPckOptsDescriptionAbstract
                            + ': ' + FSelectedPkg.Description);
    PkgInfoMemo.Lines.Add(Format(lisOIPFilename, [FSelectedPkg.LPKFilename]));

    InfoStr:=lisCurrentState;
    if FSelectedPkg.Installed<>pitNope then
      AddState(lisInstalled)
    else
      AddState(lisNotInstalled);
    if FSelectedPkg.Base then
      AddState(lisPckExplBase);
    AddState(LazPackageTypeIdents[FSelectedPkg.PkgType]);
    PkgInfoMemo.Lines.Add(InfoStr);
  finally
    LeaveCritSection;
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
  ConflictDep: TPkgDependency;
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
        MessageDlg(lisDuplicate,
          Format(lisThePackageIsAlreadyInTheList, [NewPackageID.Name]), mtError,
          [mbCancel],0);
        TVNode.Selected:=false;
        exit;
      end;
      // check if a package with same name is already in the list
      j:=IndexOfNewInstalledPkgByName(NewPackageID.Name);
      if j>=0 then begin
        MessageDlg(lisConflict,
          Format(lisThereIsAlreadyAPackageInTheList, [NewPackageID.Name]),
          mtError,[mbCancel],0);
        TVNode.Selected:=false;
        exit;
      end;
      // check if package is loaded and has some attributes that prevents
      // installation in the IDE
      APackage:=PackageGraph.FindPackageWithID(NewPackageID);
      if APackage<>nil then begin
        if APackage.PackageType in [lptRunTime,lptRunTimeOnly] then begin
          IDEMessageDialog(lisNotADesigntimePackage,
            Format(lisThePackageIsNotADesignTimePackageItCanNotBeInstall, [
              APackage.IDAsString]), mtError,
            [mbCancel]);
          TVNode.Selected:=false;
          exit;
        end;
        ConflictDep:=PackageGraph.FindRuntimePkgOnlyRecursively(
          APackage.FirstRequiredDependency);
        if ConflictDep<>nil then begin
          IDEMessageDialog(lisNotADesigntimePackage,
            Format(lisThePackageCanNotBeInstalledBecauseItRequiresWhichI, [
              APackage.Name, ConflictDep.AsString]),
            mtError,[mbCancel]);
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

procedure TInstallPkgSetDialog.ParseLPKMainThread(PkgInfo: TIPSPkgInfo);
var
  NewVersion: TPkgVersion;
begin
  EnterCritSection;
  try
    if PkgInfo.LPKParsed<>ipisNotParsed then exit;
    PkgInfo.LPKParsed:=ipisParsingMainThread;
  finally
    LeaveCritSection;
  end;
  NewVersion:=TPkgVersion.Create;
  try
    ParseLPK(PkgInfo.LPKFilename,PkgInfo.LPKError,PkgInfo.Author,PkgInfo.License,
      PkgInfo.Description,PkgInfo.PkgType,NewVersion);
    if PkgInfo.LPKError<>'' then begin
      PkgInfo.LPKParsed:=ipisParsedError;
    end else begin
      PkgInfo.LPKParsed:=ipisParsed;
    end;
    if NewVersion.Compare(PkgInfo.ID.Version)<>0 then
      ChangePkgVersion(PkgInfo,NewVersion);
  finally
    NewVersion.Free;
  end;
end;

function TInstallPkgSetDialog.FindPkgInfo(ID: string): TIPSPkgInfo;
var
  PkgID: TLazPackageID;
begin
  Result:=nil;
  PkgID:=TLazPackageID.Create;
  try
    if not PkgID.StringToID(ID) then exit;
    Result:=FindPkgInfo(PkgID);
  finally
    PkgID.Free;
  end;
end;

function TInstallPkgSetDialog.FindPkgInfo(PkgID: TLazPackageID): TIPSPkgInfo;
var
  Node: TAvgLvlTreeNode;
begin
  Node:=fAvailablePackages.FindKey(PkgID,@ComparePkgIDWithIPSPkgInfo);
  if Node=nil then exit(nil);
  Result:=TIPSPkgInfo(Node.Data);
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

procedure TInstallPkgSetDialog.EnterCritSection;
begin
  EnterCriticalsection(FCritSec);
end;

procedure TInstallPkgSetDialog.LeaveCritSection;
begin
  LeaveCriticalsection(FCritSec);
end;

end.

