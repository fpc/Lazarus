{  $Id$  }
{
 /***************************************************************************
                            pkgoptionsdlg.pas
                            -----------------


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
    TPackageOptionsDialog is the form for the general options of a package.
}
unit PkgOptionsDlg;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LCLProc, Forms, Controls, Buttons, LResources,
  ExtCtrls, StdCtrls, Spin, Dialogs, PathEditorDlg, IDEProcs, IDEWindowIntf,
  LazarusIDEStrConsts, BrokenDependenciesDlg, PackageDefs, PackageSystem,
  CompilerOptions;

type

  { TPackageOptionsDialog }

  TPackageOptionsDialog = class(TForm)
    Notebook: TNotebook;
    // Description page
    DescriptionPage: TPage;
    DescriptionGroupBox: TGroupBox;
    DescriptionMemo: TMemo;
    AuthorGroupBox: TGroupBox;
    AuthorEdit: TEdit;
    LicenseGroupBox: TGroupBox;
    LicenseMemo: TMemo;
    VersionGroupBox: TGroupBox;
    VersionMajorLabel: TLabel;
    VersionMajorSpinEdit: TSpinEdit;
    VersionMinorLabel: TLabel;
    VersionMinorSpinEdit: TSpinEdit;
    VersionReleaseLabel: TLabel;
    VersionReleaseSpinEdit: TSpinEdit;
    VersionBuildLabel: TLabel;
    VersionBuildSpinEdit: TSpinEdit;
    AutoIncrementOnBuildCheckBox: TCheckBox;
    // Usage page
    UsagePage: TPage;
    AddPathsGroupBox: TGroupBox;
    UnitPathLabel: TLabel;
    UnitPathEdit: TEdit;
    UnitPathButton: TPathEditorButton;
    IncludePathLabel: TLabel;
    IncludePathEdit: TEdit;
    IncludePathButton: TPathEditorButton;
    ObjectPathLabel: TLabel;
    ObjectPathEdit: TEdit;
    ObjectPathButton: TPathEditorButton;
    LibraryPathLabel: TLabel;
    LibraryPathEdit: TEdit;
    LibraryPathButton: TPathEditorButton;
    AddOptionsGroupBox: TGroupBox;
    LinkerOptionsLabel: TLabel;
    LinkerOptionsMemo: TMemo;
    CustomOptionsLabel: TLabel;
    CustomOptionsMemo: TMemo;
    // IDE integration page
    IDEPage: TPage;
    PkgTypeRadioGroup: TRadioGroup;
    UpdateRadioGroup: TRadioGroup;
    LazDocGroupBox: TGroupBox;
    LazDocPathEdit: TEdit;
    LazDocPathButton: TPathEditorButton;
    // buttons
    OkButton: TButton;
    CancelButton: TButton;
    procedure AddOptionsGroupBoxResize(Sender: TObject);
    procedure AddPathsGroupBoxResize(Sender: TObject);
    procedure DescriptionPageResize(Sender: TObject);
    procedure IDEPageResize(Sender: TObject);
    procedure OkButtonClick(Sender: TObject);
    procedure PackageOptionsDialogClose(Sender: TObject;
      var CloseAction: TCloseAction);
    procedure PackageOptionsDialogResize(Sender: TObject);
    procedure PathEditBtnClick(Sender: TObject);
    procedure PathEditBtnExecuted(Sender: TObject);
    procedure PkgTypeRadioGroupClick(Sender: TObject);
    procedure UsagePageResize(Sender: TObject);
    procedure VersionGroupBoxResize(Sender: TObject);
  private
    FLazPackage: TLazPackage;
    procedure SetLazPackage(const AValue: TLazPackage);
    procedure SetupComponents;
    procedure SetupUsagePage(PageIndex: integer);
    procedure SetupDescriptionPage(PageIndex: integer);
    procedure SetupIDEPage(PageIndex: integer);
    procedure ReadOptionsFromPackage;
    procedure ReadPkgTypeFromPackage;
    function GetEditForPathButton(AButton: TPathEditorButton): TEdit;
    function ShowMsgPackageTypeMustBeDesign: Boolean;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
  public
    property LazPackage: TLazPackage read FLazPackage write SetLazPackage;
  end;
  
function ShowPackageOptionsDlg(APackage: TLazPackage): TModalResult;


implementation


uses Math;

function ShowPackageOptionsDlg(APackage: TLazPackage): TModalResult;
var
  PkgOptsDlg: TPackageOptionsDialog;
begin
  PkgOptsDlg:=TPackageOptionsDialog.Create(nil);
  PkgOptsDlg.LazPackage:=APackage;
  Result:=PkgOptsDlg.ShowModal;
  PkgOptsDlg.Free;
end;

{ TPackageOptionsDialog }

procedure TPackageOptionsDialog.PackageOptionsDialogResize(Sender: TObject);
begin
  with Notebook do
    SetBounds(0,0,Parent.ClientWidth,Parent.ClientHeight-40);
    
  with OkButton do
    SetBounds(Parent.ClientWidth-200,Parent.ClientHeight-30,80,Height);
    
  with CancelButton do
    SetBounds(OkButton.Left+OkButton.Width+20,OkButton.Top,
              OkButton.Width,OkButton.Height);
end;

procedure TPackageOptionsDialog.PathEditBtnClick(Sender: TObject);
var
  AButton: TPathEditorButton;
  OldPath: String;
  AnEdit: TEdit;
  Templates: String;
begin
  if not (Sender is TPathEditorButton) then exit;
  AButton:=TPathEditorButton(Sender);
  AnEdit:=GetEditForPathButton(AButton);
  OldPath:=AnEdit.Text;
  if AButton=UnitPathButton then begin
    Templates:=SetDirSeparators(
           '$(PkgOutDir)'
          +'$(LazarusDir)/lcl/units/$(TargetCPU)-$(TargetOS)'
          +';$(LazarusDir)/lcl/units/$(TargetCPU)-$(TargetOS)/$(LCLWidgetType)'
          +';$(LazarusDir)/components/codetools/units/$(TargetCPU)-$(TargetOS)'
          +';$(LazarusDir)/components/custom'
          +';$(LazarusDir)/packager/units/$(TargetCPU)-$(TargetOS)'
          );
  end
  else if AButton=IncludePathButton then begin
    Templates:='include';
  end else
  if AButton=ObjectPathButton then begin
    Templates:='objects';
  end else
  if AButton=LibraryPathButton then begin
    Templates:='';
  end else
  if AButton=LazDocPathButton then begin
    Templates:='docs';
  end;
  AButton.CurrentPathEditor.Path:=OldPath;
  AButton.CurrentPathEditor.Templates:=SetDirSeparators(Templates);
end;

procedure TPackageOptionsDialog.PathEditBtnExecuted(Sender: TObject);
var
  AButton: TPathEditorButton;
  NewPath: String;
  AnEdit: TEdit;
  OldPath: String;
  //CurDir: string;
  //StartPos: Integer;
begin
  if not (Sender is TPathEditorButton) then exit;
  AButton:=TPathEditorButton(Sender);
  if AButton.CurrentPathEditor.ModalResult<>mrOk then exit;
  NewPath:=AButton.CurrentPathEditor.Path;
  AnEdit:=GetEditForPathButton(AButton);
  OldPath:=AnEdit.Text;
  if OldPath<>NewPath then begin
    // check NewPath
    {StartPos:=1;
    repeat
      CurDir:=GetNextDirectoryInSearchPath(NewPath,StartPos);
      if CurDir<>'' then begin
        IDEMacros.SubstituteMacros(SearchPath);
        CurDir:=LazPackage.LongenFilename(CurDir);
        if not FileExists(CurDir) then begin
          // TODO:
          
        end;
      end;
    until StartPos>length(NewPath);}
  end;
  AnEdit.Text:=NewPath;
end;

procedure TPackageOptionsDialog.PkgTypeRadioGroupClick(Sender: TObject);
begin
  if LazPackage=nil then exit;
  if (PkgTypeRadioGroup.ItemIndex=1) and (LazPackage.PackageType<>lptRunTime)
  then begin
    // user sets to runtime only
    if (LazPackage.AutoInstall<>pitNope) then begin
      ShowMsgPackageTypeMustBeDesign;
    end;
  end;
end;

procedure TPackageOptionsDialog.UsagePageResize(Sender: TObject);
var
  x: Integer;
  y: Integer;
  w: Integer;
  h: Integer;
begin
  x:=3;
  y:=3;
  w:=UsagePage.ClientWidth-2*x;
  h:=130;
  with AddPathsGroupBox do
    SetBounds(x,y,w,h);
  inc(y,h+3);

  h:=Max(70,UsagePage.ClientHeight-y);
  with AddOptionsGroupBox do
    SetBounds(x,y,w,h);
end;

procedure TPackageOptionsDialog.VersionGroupBoxResize(Sender: TObject);
var
  x: Integer;
  y: Integer;
  w: Integer;
begin
  x:=2;
  y:=22;
  w:=VersionGroupBox.ClientWidth div 4;

  with VersionMajorLabel do
    SetBounds(x,3,w,Height);
  with VersionMajorSpinEdit do
    SetBounds(x,y,Max(10,w-5),Height);
  inc(x,w);
    
  with VersionMinorLabel do
    SetBounds(x,3,w,Height);
  with VersionMinorSpinEdit do
    SetBounds(x,y,Max(10,w-5),Height);
  inc(x,w);

  with VersionReleaseLabel do
    SetBounds(x,3,w,Height);
  with VersionReleaseSpinEdit do
    SetBounds(x,y,Max(10,w-5),Height);
  inc(x,w);

  with VersionBuildLabel do
    SetBounds(x,3,w,Height);
  with VersionBuildSpinEdit do
    SetBounds(x,y,Max(10,w-5),Height);

  inc(y,VersionMinorSpinEdit.Height+5);
  with AutoIncrementOnBuildCheckBox do
    SetBounds(0,y,Parent.ClientWidth,Height);
end;

procedure TPackageOptionsDialog.DescriptionPageResize(Sender: TObject);
var
  x: Integer;
  y: Integer;
  w: Integer;
begin
  x:=3;
  y:=3;
  w:=DescriptionPage.ClientWidth-2*x;
  with DescriptionGroupBox do begin
    SetBounds(x,y,w,80);
    inc(y,Height+5);
  end;

  with AuthorGroupBox do begin
    SetBounds(x,y+3,w,50);
    inc(y,Height+5);
  end;
    
  with LicenseGroupBox do begin
    SetBounds(x,y+3,w,70);
    inc(y,Height+5);
  end;

  with VersionGroupBox do
    SetBounds(x,y,w,90);
end;

procedure TPackageOptionsDialog.IDEPageResize(Sender: TObject);
var
  x: Integer;
  y: Integer;
  w: Integer;
  h: Integer;
begin
  x:=3;
  y:=3;
  w:=(IDEPage.ClientWidth-2*x);
  h:=90;
  with PkgTypeRadioGroup do begin
    SetBounds(x,y,w,h);
  end;
end;

procedure TPackageOptionsDialog.OkButtonClick(Sender: TObject);
var
  NewPackageType: TLazPackageType;
  NewVersion: TPkgVersion;
  BrokenDependencies: TFPList;
  RenameDependencies: Boolean;
  MsgResult: TModalResult;
begin
  if LazPackage.ReadOnly then exit;
  
  // check changes
  
  // package type
  case PkgTypeRadioGroup.ItemIndex of
  0:   NewPackageType:=lptDesignTime;
  1:   NewPackageType:=lptRunTime;
  else NewPackageType:=lptRunAndDesignTime;
  end;
  if NewPackageType<>LazPackage.PackageType then begin
    if (NewPackageType=lptRunTime) and (LazPackage.AutoInstall<>pitNope) then
    begin
      if ShowMsgPackageTypeMustBeDesign then exit;
    end;
  end;

  // version
  NewVersion:=TPkgVersion.Create;
  try
    NewVersion.Major:=RoundToInt(VersionMajorSpinEdit.Value);
    NewVersion.Minor:=RoundToInt(VersionMinorSpinEdit.Value);
    NewVersion.Release:=RoundToInt(VersionReleaseSpinEdit.Value);
    NewVersion.Build:=RoundToInt(VersionBuildSpinEdit.Value);

    // check for broken dependencies
    BrokenDependencies:=PackageGraph.GetBrokenDependenciesWhenChangingPkgID(
      LazPackage,LazPackage.Name,NewVersion);
    RenameDependencies:=false;
    try
      if BrokenDependencies.Count>0 then begin
        MsgResult:=ShowBrokenDependencies(BrokenDependencies,
                                       DefaultBrokenDepButtons);
        if MsgResult=mrYes then
          RenameDependencies:=true
        else if MsgResult=mrNo then
          RenameDependencies:=false
        else
          exit;
      end;
    finally
      BrokenDependencies.Free;
    end;

    PackageGraph.ChangePackageID(LazPackage,LazPackage.Name,NewVersion,
                                  RenameDependencies);
  finally
    NewVersion.Free;
  end;

  // Description page
  LazPackage.Description:=DescriptionMemo.Text;
  LazPackage.Author:=AuthorEdit.Text;
  LazPackage.License:=LicenseMemo.Text;
  LazPackage.AutoIncrementVersionOnBuild:=AutoIncrementOnBuildCheckBox.Checked;

  // Usage page
  LazPackage.PackageType:=NewPackageType;
  case UpdateRadioGroup.ItemIndex of
  2: LazPackage.AutoUpdate:=pupManually;
  1: LazPackage.AutoUpdate:=pupOnRebuildingAll;
  else LazPackage.AutoUpdate:=pupAsNeeded;
  end;
  with LazPackage.UsageOptions do begin
    UnitPath:=TrimSearchPath(UnitPathEdit.Text,'');
    IncludePath:=TrimSearchPath(IncludePathEdit.Text,'');
    ObjectPath:=TrimSearchPath(ObjectPathEdit.Text,'');
    LibraryPath:=TrimSearchPath(LibraryPathEdit.Text,'');
    LinkerOptions:=LinkerOptionsMemo.Text;
    CustomOptions:=CustomOptionsMemo.Text;
  end;
  LazPackage.LazDocPaths:=LazDocPathEdit.Text;
  
  ModalResult:=mrOk;
end;

procedure TPackageOptionsDialog.PackageOptionsDialogClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  IDEDialogLayoutList.SaveLayout(Self);
end;

procedure TPackageOptionsDialog.AddPathsGroupBoxResize(Sender: TObject);
var
  LabelLeft: Integer;
  LabelWidth: Integer;
  ButtonWidth: Integer;
  EditLeft: Integer;
  EditWidth: Integer;
  ButtonLeft: Integer;
  EditHeight: Integer;
  y: Integer;
begin
  LabelLeft:=3;
  LabelWidth:=70;
  ButtonWidth:=25;
  EditLeft:=LabelWidth+LabelLeft;
  EditWidth:=AddPathsGroupBox.ClientWidth-2*LabelLeft-LabelWidth-ButtonWidth;
  ButtonLeft:=EditLeft+EditWidth;
  EditHeight:=UnitPathEdit.Height;
  y:=0;
  
  UnitPathLabel.SetBounds(LabelLeft,y+3,LabelWidth,Height);
  UnitPathEdit.SetBounds(EditLeft,y,EditWidth,EditHeight);
  UnitPathButton.SetBounds(ButtonLeft,y,ButtonWidth,EditHeight);
  inc(y,EditHeight+3);
  
  IncludePathLabel.SetBounds(LabelLeft,y+3,LabelWidth,Height);
  IncludePathEdit.SetBounds(EditLeft,y,EditWidth,EditHeight);
  IncludePathButton.SetBounds(ButtonLeft,y,ButtonWidth,EditHeight);
  inc(y,EditHeight+3);

  ObjectPathLabel.SetBounds(LabelLeft,y+3,LabelWidth,Height);
  ObjectPathEdit.SetBounds(EditLeft,y,EditWidth,EditHeight);
  ObjectPathButton.SetBounds(ButtonLeft,y,ButtonWidth,EditHeight);
  inc(y,EditHeight+3);

  LibraryPathLabel.SetBounds(LabelLeft,y+3,LabelWidth,Height);
  LibraryPathEdit.SetBounds(EditLeft,y,EditWidth,EditHeight);
  LibraryPathButton.SetBounds(ButtonLeft,y,ButtonWidth,EditHeight);
end;

procedure TPackageOptionsDialog.AddOptionsGroupBoxResize(Sender: TObject);
var
  LabelLeft: Integer;
  LabelWidth: Integer;
  MemoLeft: Integer;
  MemoWidth: Integer;
  MemoHeight: Integer;
  y: Integer;
begin
  y:=3;
  LabelLeft:=3;
  LabelWidth:=70;
  MemoLeft:=LabelLeft+LabelWidth;
  MemoWidth:=AddOptionsGroupBox.ClientWidth-LabelLeft-MemoLeft;
  MemoHeight:=(AddOptionsGroupBox.ClientHeight-3*y) div 2;
  
  LinkerOptionsLabel.SetBounds(LabelLeft,y+3,LabelWidth,Height);
  LinkerOptionsMemo.SetBounds(MemoLeft,y,MemoWidth,MemoHeight);
  inc(y,y+MemoHeight);
  CustomOptionsLabel.SetBounds(LabelLeft,y+3,LabelWidth,Height);
  CustomOptionsMemo.SetBounds(MemoLeft,y,MemoWidth,MemoHeight);
end;

procedure TPackageOptionsDialog.SetLazPackage(const AValue: TLazPackage);
begin
  if FLazPackage=AValue then exit;
  FLazPackage:=AValue;
  ReadOptionsFromPackage;
end;

procedure TPackageOptionsDialog.SetupComponents;
begin
  Notebook:=TNotebook.Create(Self);
  with Notebook do begin
    Name:='Notebook';
    Parent:=Self;
    Pages.Add(lisPckOptsUsage);
    Pages.Add(lisToDoLDescription);
    Pages.Add(lisPckOptsIDEIntegration);
    PageIndex:=0;
  end;
  
  SetupUsagePage(0);
  SetupDescriptionPage(1);
  SetupIDEPage(2);

  OkButton:=TButton.Create(Self);
  with OkButton do begin
    Name:='OkButton';
    Caption:=lisLazBuildOk;
    Parent:=Self;
    OnClick:=@OkButtonClick;
    Default:=true;
  end;
  
  CancelButton:=TButton.Create(Self);
  with CancelButton do begin
    Name:='CancelButton';
    Parent:=Self;
    Caption:=dlgCancel;
    ModalResult:=mrCancel;
    Cancel:=true;
  end;
end;

procedure TPackageOptionsDialog.SetupDescriptionPage(PageIndex: integer);
begin
  // Description page
  DescriptionPage:=Notebook.Page[PageIndex];
  DescriptionPage.OnResize:=@DescriptionPageResize;

  DescriptionGroupBox:=TGroupBox.Create(Self);
  with DescriptionGroupBox do begin
    Name:='DescriptionGroupBox';
    Parent:=DescriptionPage;
    Caption:=lisPckOptsDescriptionAbstract;
  end;
  
  DescriptionMemo:=TMemo.Create(Self);
  with DescriptionMemo do begin
    Name:='DescriptionMemo';
    Parent:=DescriptionGroupBox;
    Align:=alClient;
  end;

  AuthorGroupBox:=TGroupBox.Create(Self);
  with AuthorGroupBox do begin
    Name:='AuthorGroupBox';
    Parent:=DescriptionPage;
    Caption:=lisPckOptsAuthor;
  end;

  AuthorEdit:=TEdit.Create(Self);
  with AuthorEdit do begin
    Name:='AuthorEdit';
    Parent:=AuthorGroupBox;
    Align:=alTop;
    Text:='';
  end;

  LicenseGroupBox:=TGroupBox.Create(Self);
  with LicenseGroupBox do begin
    Name:='LicenseGroupBox';
    Parent:=DescriptionPage;
    Caption:=lisPckOptsLicense;
  end;

  LicenseMemo:=TMemo.Create(Self);
  with LicenseMemo do begin
    Name:='LicenseMemo';
    Parent:=LicenseGroupBox;
    Align:=alClient;
    ScrollBars:=ssAutoVertical;
    Text:='';
  end;

  VersionGroupBox:=TGroupBox.Create(Self);
  with VersionGroupBox do begin
    Name:='VersionGroupBox';
    Parent:=DescriptionPage;
    Caption:=lisVersion;
    OnResize:=@VersionGroupBoxResize;
  end;

  VersionMajorLabel:=TLabel.Create(Self);
  with VersionMajorLabel do begin
    Name:='VersionMajorLabel';
    Parent:=VersionGroupBox;
    Caption:=lisPckOptsMajor;
  end;

  VersionMajorSpinEdit:=TSpinEdit.Create(Self);
  with VersionMajorSpinEdit do begin
    Name:='VersionMajorSpinEdit';
    Parent:=VersionGroupBox;
    MinValue:=0;
    MaxValue:=9999;
  end;

  VersionMinorLabel:=TLabel.Create(Self);
  with VersionMinorLabel do begin
    Name:='VersionMinorLabel';
    Parent:=VersionGroupBox;
    Caption:=lisPckOptsMinor;
  end;

  VersionMinorSpinEdit:=TSpinEdit.Create(Self);
  with VersionMinorSpinEdit do begin
    Name:='VersionMinorSpinEdit';
    Parent:=VersionGroupBox;
    MinValue:=0;
    MaxValue:=9999;
  end;

  VersionReleaseLabel:=TLabel.Create(Self);
  with VersionReleaseLabel do begin
    Name:='VersionReleaseLabel';
    Parent:=VersionGroupBox;
    Caption:=lisPckOptsRelease;
  end;

  VersionReleaseSpinEdit:=TSpinEdit.Create(Self);
  with VersionReleaseSpinEdit do begin
    Name:='VersionReleaseSpinEdit';
    Parent:=VersionGroupBox;
    MinValue:=0;
    MaxValue:=9999;
  end;

  VersionBuildLabel:=TLabel.Create(Self);
  with VersionBuildLabel do begin
    Name:='VersionBuildLabel';
    Parent:=VersionGroupBox;
    Caption:=lisBuildNumber;
  end;

  VersionBuildSpinEdit:=TSpinEdit.Create(Self);
  with VersionBuildSpinEdit do begin
    Name:='VersionBuildSpinEdit';
    Parent:=VersionGroupBox;
    MinValue:=0;
    MaxValue:=9999;
  end;

  AutoIncrementOnBuildCheckBox:=TCheckBox.Create(Self);
  with AutoIncrementOnBuildCheckBox do begin
    Name:='AutoIncrementOnBuildCheckBox';
    Parent:=VersionGroupBox;
    Caption:=lisPckOptsAutomaticallyIncrementVersionOnBuild;
    Enabled:=false;
  end;
end;

procedure TPackageOptionsDialog.SetupIDEPage(PageIndex: integer);
begin
  // Usage page
  IDEPage:=Notebook.Page[PageIndex];
  IDEPage.OnResize:=@IDEPageResize;

  PkgTypeRadioGroup:=TRadioGroup.Create(Self);
  with PkgTypeRadioGroup do begin
    Name:='UsageRadioGroup';
    Caption:=lisPckOptsPackageType;
    with Items do begin
      BeginUpdate;
      Add(lisPckOptsDesigntimeOnly);
      Add(lisPckOptsRuntimeOnly);
      Add(lisPckOptsDesigntimeAndRuntime);
      EndUpdate;
    end;
    ItemIndex:=2;
    OnClick:=@PkgTypeRadioGroupClick;
    Parent:=IDEPage;
  end;

  UpdateRadioGroup:=TRadioGroup.Create(Self);
  with UpdateRadioGroup do begin
    Name:='UpdateRadioGroup';
    Caption:=lisPckOptsUpdateRebuild;
    with Items do begin
      BeginUpdate;
      Add(lisPckOptsAutomaticallyRebuildAsNeeded);
      Add(lisPckOptsAutoRebuildWhenRebuildingAll);
      Add(lisPckOptsManualCompilationNeverAutomatically);
      EndUpdate;
    end;
    ItemIndex:=0;
    Parent:=IDEPage;
    Height:=90;
    AnchorToCompanion(akTop,6,PkgTypeRadioGroup);
  end;

  // lazdoc
  LazDocGroupBox:=TGroupBox.Create(Self);
  with LazDocGroupBox do begin
    Name:='LazDocGroupBox';
    Caption:='LazDoc - Lazarus documentation';
    AnchorToCompanion(akTop,6,UpdateRadioGroup);
    AutoSize:=true;
    Parent:=IDEPage;
  end;

  LazDocPathEdit:=TEdit.Create(Self);
  with LazDocPathEdit do begin
    Name:='LazDocPathEdit';
    SetBounds(6,0,Width,Height);
    Parent:=LazDocGroupBox;
  end;

  LazDocPathButton:=TPathEditorButton.Create(Self);
  with LazDocPathButton do begin
    Name:='LazDocPathButton';
    Caption:='...';
    AutoSize:=true;
    Anchors:=[akTop,akRight,akBottom];
    AnchorParallel(akRight,6,LazDocGroupBox);
    Top:=0;
    AnchorParallel(akBottom,0,LazDocPathEdit);
    OnClick:=@PathEditBtnClick;
    OnExecuted:=@PathEditBtnExecuted;
    Parent:=LazDocGroupBox;
  end;
  LazDocPathEdit.AnchorToNeighbour(akRight,0,LazDocPathButton);
end;

procedure TPackageOptionsDialog.SetupUsagePage(PageIndex: integer);
begin
  // Usage page
  UsagePage:=Notebook.Page[PageIndex];
  UsagePage.OnResize:=@UsagePageResize;

  AddPathsGroupBox:=TGroupBox.Create(Self);
  with AddPathsGroupBox do begin
    Name:='AddPathsGroupBox';
    Parent:=UsagePage;
    Caption:=lisPckOptsAddPathsToDependentPackagesProjects;
    OnResize:=@AddPathsGroupBoxResize;
  end;

  UnitPathLabel:=TLabel.Create(Self);
  with UnitPathLabel do begin
    Name:='UnitPathLabel';
    Parent:=AddPathsGroupBox;
    Caption:=lisPkgFileTypeUnit;
  end;

  UnitPathEdit:=TEdit.Create(Self);
  with UnitPathEdit do begin
    Name:='UnitPathEdit';
    Parent:=AddPathsGroupBox;
    Text:='';
  end;

  UnitPathButton:=TPathEditorButton.Create(Self);
  with UnitPathButton do begin
    Name:='UnitPathButton';
    Parent:=AddPathsGroupBox;
    Caption:='...';
    OnClick:=@PathEditBtnClick;
    OnExecuted:=@PathEditBtnExecuted;
  end;

  IncludePathLabel:=TLabel.Create(Self);
  with IncludePathLabel do begin
    Name:='IncludePathLabel';
    Parent:=AddPathsGroupBox;
    Caption:=lisPckOptsInclude;
  end;

  IncludePathEdit:=TEdit.Create(Self);
  with IncludePathEdit do begin
    Name:='IncludePathEdit';
    Parent:=AddPathsGroupBox;
    Text:='';
  end;

  IncludePathButton:=TPathEditorButton.Create(Self);
  with IncludePathButton do begin
    Name:='IncludePathButton';
    Parent:=AddPathsGroupBox;
    Caption:='...';
    OnClick:=@PathEditBtnClick;
    OnExecuted:=@PathEditBtnExecuted;
  end;

  ObjectPathLabel:=TLabel.Create(Self);
  with ObjectPathLabel do begin
    Name:='ObjectPathLabel';
    Parent:=AddPathsGroupBox;
    Caption:=lisPckOptsObject;
  end;

  ObjectPathEdit:=TEdit.Create(Self);
  with ObjectPathEdit do begin
    Name:='ObjectPathEdit';
    Parent:=AddPathsGroupBox;
    Text:='';
  end;

  ObjectPathButton:=TPathEditorButton.Create(Self);
  with ObjectPathButton do begin
    Name:='ObjectPathButton';
    Parent:=AddPathsGroupBox;
    Caption:='...';
    OnClick:=@PathEditBtnClick;
    OnExecuted:=@PathEditBtnExecuted;
  end;

  LibraryPathLabel:=TLabel.Create(Self);
  with LibraryPathLabel do begin
    Name:='LibraryPathLabel';
    Parent:=AddPathsGroupBox;
    Caption:=lisPckOptsLibrary;
  end;

  LibraryPathEdit:=TEdit.Create(Self);
  with LibraryPathEdit do begin
    Name:='LibraryPathEdit';
    Parent:=AddPathsGroupBox;
    Text:='';
  end;

  LibraryPathButton:=TPathEditorButton.Create(Self);
  with LibraryPathButton do begin
    Name:='LibraryPathButton';
    Parent:=AddPathsGroupBox;
    Caption:='...';
    OnClick:=@PathEditBtnClick;
    OnExecuted:=@PathEditBtnExecuted;
  end;

  AddOptionsGroupBox:=TGroupBox.Create(Self);
  with AddOptionsGroupBox do begin
    Name:='AddOptionsGroupBox';
    Parent:=UsagePage;
    Caption:=lisPckOptsAddOptionsToDependentPackagesAndProjects;
    OnResize:=@AddOptionsGroupBoxResize;
  end;

  LinkerOptionsLabel:=TLabel.Create(Self);
  with LinkerOptionsLabel do begin
    Name:='LinkerOptionsLabel';
    Parent:=AddOptionsGroupBox;
    Caption:=lisPckOptsLinker;
  end;

  LinkerOptionsMemo:=TMemo.Create(Self);
  with LinkerOptionsMemo do begin
    Name:='LinkerOptionsMemo';
    Parent:=AddOptionsGroupBox;
    ScrollBars:=ssAutoVertical;
  end;

  CustomOptionsLabel:=TLabel.Create(Self);
  with CustomOptionsLabel do begin
    Name:='CustomOptionsLabel';
    Parent:=AddOptionsGroupBox;
    Caption:=lisPckOptsCustom;
  end;

  CustomOptionsMemo:=TMemo.Create(Self);
  with CustomOptionsMemo do begin
    Name:='CustomOptionsMemo';
    Parent:=AddOptionsGroupBox;
    ScrollBars:=ssAutoVertical;
  end;
end;

procedure TPackageOptionsDialog.ReadOptionsFromPackage;
begin
  if LazPackage=nil then exit;
  
  OkButton.Enabled:=not LazPackage.ReadOnly;

  // Description page
  DescriptionMemo.Text:=LazPackage.Description;
  AuthorEdit.Text:=LazPackage.Author;
  LicenseMemo.Text:=LazPackage.License;

  VersionMajorSpinEdit.Value:=LazPackage.Version.Major;
  VersionMinorSpinEdit.Value:=LazPackage.Version.Minor;
  VersionReleaseSpinEdit.Value:=LazPackage.Version.Release;
  VersionBuildSpinEdit.Value:=LazPackage.Version.Build;
  AutoIncrementOnBuildCheckBox.Checked:=LazPackage.AutoIncrementVersionOnBuild;

  // Usage page
  ReadPkgTypeFromPackage;

  case LazPackage.AutoUpdate of
  pupAsNeeded: UpdateRadioGroup.ItemIndex:=0;
  pupOnRebuildingAll: UpdateRadioGroup.ItemIndex:=1;
  else UpdateRadioGroup.ItemIndex:=2;
  end;
    
  with LazPackage.UsageOptions do begin
    UnitPathEdit.Text:=UnitPath;
    IncludePathEdit.Text:=IncludePath;
    ObjectPathEdit.Text:=ObjectPath;
    LibraryPathEdit.Text:=LibraryPath;
    LinkerOptionsMemo.Text:=LinkerOptions;
    CustomOptionsMemo.Text:=CustomOptions;
  end;
  
  LazDocPathEdit.Text:=LazPackage.LazDocPaths;
end;

procedure TPackageOptionsDialog.ReadPkgTypeFromPackage;
begin
  case LazPackage.PackageType of
  lptDesignTime: PkgTypeRadioGroup.ItemIndex:=0;
  lptRunTime:    PkgTypeRadioGroup.ItemIndex:=1;
  else           PkgTypeRadioGroup.ItemIndex:=2;
  end;
end;

function TPackageOptionsDialog.GetEditForPathButton(AButton: TPathEditorButton
  ): TEdit;
begin
  if AButton=UnitPathButton then
    Result:=UnitPathEdit
  else if AButton=IncludePathButton then
    Result:=IncludePathEdit
  else if AButton=ObjectPathButton then
    Result:=ObjectPathEdit
  else if AButton=LibraryPathButton then
    Result:=LibraryPathEdit
  else if AButton=LazDocPathButton then
    Result:=LazDocPathEdit
  else
    Result:=nil;
end;

function TPackageOptionsDialog.ShowMsgPackageTypeMustBeDesign: Boolean;
begin
  if MessageDlg(lisPckOptsInvalidPackageType,
    Format(lisPckOptsThePackageHasTheAutoInstallFlagThisMeans, ['"',
      LazPackage.IDAsString, '"', #13, #13]),
    mtWarning,[mbIgnore,mbCancel],0) <>mrIgnore
  then begin
    Result:=true;
    ReadPkgTypeFromPackage;
  end else
    Result:=false;
end;

constructor TPackageOptionsDialog.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  Name:='PackageOptionsDialog';
  Caption:=lisPckOptsPackageOptions;
  SetupComponents;
  OnResize:=@PackageOptionsDialogResize;
  Position:=poScreenCenter;
  IDEDialogLayoutList.ApplyLayout(Self,450,400);
  OnResize(Self);
  OnClose:=@PackageOptionsDialogClose;
end;

destructor TPackageOptionsDialog.Destroy;
begin
  inherited Destroy;
end;

end.

