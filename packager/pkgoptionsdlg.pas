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
  Classes, SysUtils, FileUtil, LCLProc, Forms, Controls, Buttons, 
  ExtCtrls, ComCtrls, StdCtrls, Spin, Dialogs, PathEditorDlg,
  IDEProcs, IDEWindowIntf, IDEDialogs, MacroIntf, PackageIntf,
  LazarusIDEStrConsts,
  BrokenDependenciesDlg, PackageDefs, PackageSystem, CompilerOptions,
  ButtonPanel;

type

  { TPackageOptionsDialog }

  TPackageOptionsDialog = class(TForm)
    AdditionalInfoButton: TButton;
    AddPackageUnitToProjectCheckBox: TCheckBox;
    ButtonPanel1: TButtonPanel;
    EnableI18NCheckBox: TCheckBox;
    ProjectGroupBox: TGroupBox;
    I18NGroupBox: TGroupBox;
    ProvidesGroupBox: TGroupBox;
    LazDocGroupBox: TGroupBox;
    ProvidesMemo: TMemo;
    i18n: TTabSheet;
    POOutDirButton: TButton;
    POOutDirEdit: TEdit;
    PkgTypeRadioGroup: TRadioGroup;
    LazDocPathEdit: TEdit;
    UpdateRadioGroup: TRadioGroup;
    VersionReleaseLabel: TLabel;
    VersionReleaseSpinEdit: TSpinEdit;
    VersionMinorLabel: TLabel;
    VersionMajorSpinEdit: TSpinEdit;
    VersionMajorLabel: TLabel;
    VersionGroupBox: TGroupBox;
    DescriptionGroupBox: TGroupBox;
    AuthorGroupBox: TGroupBox;
    AddPathsGroupBox: TGroupBox;
    CopyrightEdit: TEdit;
    CopyrightLabel: TLabel;
    DescriptionEdit: TEdit;
    LicenseGroupBox: TGroupBox;
    DescriptionLabel: TLabel;
    LicenseMemo: TMemo;
    IncludePathEdit: TEdit;
    LinkerOptionsLabel: TLabel;
    CustomOptionsLabel: TLabel;
    LinkerOptionsMemo: TMemo;
    CustomOptionsMemo: TMemo;
    DescriptionMemo: TMemo;
    ObjectPathEdit: TEdit;
    LibraryPathEdit: TEdit;
    AddOptionsGroupBox: TGroupBox;
    AuthorEdit: TEdit;
    UnitPathLabel: TLabel;
    Notebook: TPageControl;
    UnitPathEdit: TEdit;
    PoOutDirLabel: TLabel;
    IncludePathLabel: TLabel;
    ObjectPathLabel: TLabel;
    LibraryPathLabel: TLabel;
    UsagePage: TTabSheet;
    DescriptionPage: TTabSheet;
    IDEPage: TTabSheet;
    ProvidesPage: TTabSheet;
    VersionBuildLabel: TLabel;
    VersionMinorSpinEdit: TSpinEdit;
    VersionBuildSpinEdit: TSpinEdit;
    UnitPathButton: TPathEditorButton;
    IncludePathButton: TPathEditorButton;
    ObjectPathButton: TPathEditorButton;
    LibraryPathButton: TPathEditorButton;
    LazDocPathButton: TPathEditorButton;

    procedure EnableI18NCheckBoxChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure OkButtonClick(Sender: TObject);
    procedure POOutputDirectoryButtonClick(Sender: TObject);
    procedure PackageOptionsDialogClose(Sender: TObject;
      var CloseAction: TCloseAction);
    procedure PathEditBtnClick(Sender: TObject);
    procedure PathEditBtnExecuted(Sender: TObject);
    procedure PkgTypeRadioGroupClick(Sender: TObject);
  private
    FLazPackage: TLazPackage;
    procedure SetLazPackage(const AValue: TLazPackage);
    procedure SetupComponents;
    procedure SetupUsagePage;
    procedure SetupIDEPage;
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


var
  PackageOptionsDialog: TPackageOptionsDialog;

implementation

{$R *.lfm}

uses 
  Math;

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
  CurDir: string;
  StartPos: Integer;
  DlgResult: TModalResult;
  OldStartPos: LongInt;
begin
  if not (Sender is TPathEditorButton) then exit;
  AButton:=TPathEditorButton(Sender);
  if AButton.CurrentPathEditor.ModalResult<>mrOk then exit;
  NewPath:=AButton.CurrentPathEditor.Path;
  AnEdit:=GetEditForPathButton(AButton);
  OldPath:=AnEdit.Text;
  if OldPath<>NewPath then begin
    // check NewPath
    StartPos:=1;
    repeat
      OldStartPos:=StartPos;
      CurDir:=GetNextDirectoryInSearchPath(NewPath,StartPos);
      if CurDir<>'' then begin
        IDEMacros.SubstituteMacros(CurDir);
        LazPackage.LongenFilename(CurDir);
        if not DirPathExists(CurDir) then begin
          DlgResult:=QuestionDlg(lisEnvOptDlgDirectoryNotFound,
            Format(lisDirectoryNotFound, ['"', CurDir, '"']),
            mtError, [mrIgnore, mrYes, lisRemoveFromSearchPath, mrCancel], 0);
          case DlgResult of
          mrIgnore: ;
          mrYes:
            begin
              // remove directory from search path
              NewPath:=copy(NewPath,1,OldStartPos-1)
                       +copy(NewPath,StartPos,length(NewPath));
              StartPos:=OldStartPos;
            end;
          else
            // undo
            NewPath:=OldPath;
            break;
          end;
        end;
      end;
    until StartPos>length(NewPath);
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

procedure TPackageOptionsDialog.EnableI18NCheckBoxChange(Sender: TObject);
begin
  I18NGroupBox.Enabled := EnableI18NCheckBox.Checked;
end;

procedure TPackageOptionsDialog.FormCreate(Sender: TObject);
begin
  NoteBook.PageIndex := 0;
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
                                 RenameDependencies,true);
  finally
    NewVersion.Free;
  end;

  // Description page
  LazPackage.Description:=DescriptionMemo.Text;
  LazPackage.Author:=AuthorEdit.Text;
  LazPackage.License:=LicenseMemo.Text;

  // Usage page
  LazPackage.PackageType:=NewPackageType;
  with LazPackage.UsageOptions do begin
    UnitPath:=TrimSearchPath(UnitPathEdit.Text,'');
    IncludePath:=TrimSearchPath(IncludePathEdit.Text,'');
    ObjectPath:=TrimSearchPath(ObjectPathEdit.Text,'');
    LibraryPath:=TrimSearchPath(LibraryPathEdit.Text,'');
    LinkerOptions:=LinkerOptionsMemo.Text;
    CustomOptions:=CustomOptionsMemo.Text;
  end;
  LazPackage.AddToProjectUsesSection:=AddPackageUnitToProjectCheckBox.Checked;

  // IDE integration page
  case UpdateRadioGroup.ItemIndex of
  2: LazPackage.AutoUpdate:=pupManually;
  1: LazPackage.AutoUpdate:=pupOnRebuildingAll;
  else LazPackage.AutoUpdate:=pupAsNeeded;
  end;

  LazPackage.LazDocPaths:=LazDocPathEdit.Text;

  // Provides page
  LazPackage.Provides:=ProvidesMemo.Lines;
  
  // i18n
  LazPackage.EnableI18N := EnableI18NCheckBox.Checked;
  LazPackage.POOutputDirectory:=POOutDirEdit.Text;


  ModalResult:=mrOk;
end;

procedure TPackageOptionsDialog.POOutputDirectoryButtonClick(Sender: TObject);
var
  NewDirectory: string;
begin
  NewDirectory:=LazSelectDirectory(lisPOChoosePoFileDirectory,
                                   LazPackage.Directory);
  if NewDirectory='' then exit;
  LazPackage.ShortenFilename(NewDirectory,true);
  POOutDirEdit.Text:=NewDirectory;
end;

procedure TPackageOptionsDialog.PackageOptionsDialogClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  IDEDialogLayoutList.SaveLayout(Self);
end;

procedure TPackageOptionsDialog.SetLazPackage(const AValue: TLazPackage);
begin
  if FLazPackage=AValue then exit;
  FLazPackage:=AValue;
  ReadOptionsFromPackage;
end;

procedure TPackageOptionsDialog.SetupComponents;
begin
  Notebook.Page[0].Caption:=lisPckOptsUsage;
  Notebook.Page[1].Caption:=lisCodeHelpDescrTag;
  Notebook.Page[2].Caption:=lisPckOptsIDEIntegration;
  Notebook.Page[3].Caption:=lisPckOptsProvides;
  Notebook.Page[4].Caption:=dlgPOI18n;

  ButtonPanel1.OKButton.Caption:=lisOk;
  ButtonPanel1.CancelButton.Caption:=dlgCancel;

  SetupUsagePage;
  SetupIDEPage;
  
  //Description page
  DescriptionGroupBox.Caption:=lisPckOptsDescriptionAbstract;
  AuthorGroupBox.Caption:=lisPckOptsAuthor;
  LicenseGroupBox.Caption:=lisPckOptsLicense;
  VersionGroupBox.Caption:=lisVersion;
  VersionMajorLabel.Caption:=lisPckOptsMajor;
  VersionMinorLabel.Caption:=lisPckOptsMinor;
  VersionReleaseLabel.Caption:=lisPckOptsRelease;
  VersionBuildLabel.Caption:=lisBuildNumber;

  //Provides page
  ProvidesGroupBox.Caption:=lisPckOptsThisPackageProvidesTheSameAsTheFollowingPackages;
  
  //i18n page
  EnableI18NCheckBox.Caption:=rsEnableI18n;
  I18NGroupBox.Caption:=rsI18nOptions;
  PoOutDirlabel.Caption:=rsPOOutputDirectory;
end;

procedure TPackageOptionsDialog.SetupIDEPage;
begin
  // lazdoc
  PkgTypeRadioGroup.Caption:=lisPckOptsPackageType;
  PkgTypeRadioGroup.Items[0]:=lisPckOptsDesigntimeOnly;
  PkgTypeRadioGroup.Items[1]:=lisPckOptsRuntimeOnly;
  PkgTypeRadioGroup.Items[2]:=lisPckOptsDesigntimeAndRuntime;
  UpdateRadioGroup.Caption:=lisPckOptsUpdateRebuild;
  UpdateRadioGroup.Items[0]:=lisPckOptsAutomaticallyRebuildAsNeeded;
  UpdateRadioGroup.Items[1]:=lisPckOptsAutoRebuildWhenRebuildingAll;
  UpdateRadioGroup.Items[2]:=lisPckOptsManualCompilationNeverAutomatically;
  LazDocGroupBox.Caption:=lisPckOptsLazDocLazarusDocumentation;

  LazDocPathButton:=TPathEditorButton.Create(Self);
  with LazDocPathButton do begin
    Name:='LazDocPathButton';
    Caption:='...';
    AutoSize:=true;
    Anchors:=[akRight];
    AnchorParallel(akRight,6,LazDocGroupBox);
    AnchorParallel(akTop,0,LazDocPathEdit);
    AnchorParallel(akBottom,0,LazDocPathEdit);
    OnClick:=@PathEditBtnClick;
    OnExecuted:=@PathEditBtnExecuted;
    Parent:=LazDocGroupBox;
  end;
  LazDocPathEdit.AnchorToNeighbour(akRight,0,LazDocPathButton);
end;


procedure TPackageOptionsDialog.SetupUsagePage;
begin
  // Usage page
  AddPathsGroupBox.Caption:=lisPckOptsAddPathsToDependentPackagesProjects;
  UnitPathLabel.Caption:=lisPkgFileTypeUnit;
  IncludePathLabel.Caption:=lisPckOptsInclude;
  ObjectPathLabel.Caption:=lisPckOptsObject;
  LibraryPathLabel.Caption:=lisPckOptsLibrary;
  AddOptionsGroupBox.Caption:=lisPckOptsAddOptionsToDependentPackagesAndProjects;
  LinkerOptionsLabel.Caption:=lisPckOptsLinker;
  CustomOptionsLabel.Caption:=lisPckOptsCustom;

  UnitPathButton:=TPathEditorButton.Create(Self);
  with UnitPathButton do begin
    Name:='UnitPathButton';
    Parent:=AddPathsGroupBox;
    Caption:='...';
    AutoSize:=true;
    Anchors:=[akRight];
    AnchorParallel(akRight,6,AddPathsGroupBox);
    AnchorParallel(akTop,0,UnitPathEdit);
    AnchorParallel(akBottom,0,UnitPathEdit);
    OnClick:=@PathEditBtnClick;
    OnExecuted:=@PathEditBtnExecuted;
  end;
  
  IncludePathButton:=TPathEditorButton.Create(Self);
  with IncludePathButton do begin
    Name:='IncludePathButton';
    Parent:=AddPathsGroupBox;
    Caption:='...';
    AutoSize:=true;
    Anchors:=[akRight];
    AnchorParallel(akRight,6,AddPathsGroupBox);
    AnchorParallel(akTop,0,IncludePathEdit);
    AnchorParallel(akBottom,0,IncludePathEdit);
    OnClick:=@PathEditBtnClick;
    OnExecuted:=@PathEditBtnExecuted;
  end;

  ObjectPathButton:=TPathEditorButton.Create(Self);
  with ObjectPathButton do begin
    Name:='ObjectPathButton';
    Parent:=AddPathsGroupBox;
    Caption:='...';
    AutoSize:=true;
    Anchors:=[akRight];
    AnchorParallel(akRight,6,AddPathsGroupBox);
    AnchorParallel(akTop,0,ObjectPathEdit);
    AnchorParallel(akBottom,0,ObjectPathEdit);
    OnClick:=@PathEditBtnClick;
    OnExecuted:=@PathEditBtnExecuted;
  end;

  LibraryPathButton:=TPathEditorButton.Create(Self);
  with LibraryPathButton do begin
    Name:='LibraryPathButton';
    Parent:=AddPathsGroupBox;
    Caption:='...';
    AutoSize:=true;
    Anchors:=[akRight];
    AnchorParallel(akRight,6,AddPathsGroupBox);
    AnchorParallel(akTop,0,LibraryPathEdit);
    AnchorParallel(akBottom,0,LibraryPathEdit);
    OnClick:=@PathEditBtnClick;
    OnExecuted:=@PathEditBtnExecuted;
  end;

  ProjectGroupBox.Caption:=dlgEnvProject;
  AddPackageUnitToProjectCheckBox.Caption:=podAddPackageUnitToUsesSection;
end;

procedure TPackageOptionsDialog.ReadOptionsFromPackage;
begin
  if LazPackage=nil then exit;
  
  ButtonPanel1.OKButton.Enabled:=not LazPackage.ReadOnly;

  // Description page
  DescriptionMemo.Text:=LazPackage.Description;
  AuthorEdit.Text:=LazPackage.Author;
  LicenseMemo.Text:=LazPackage.License;

  VersionMajorSpinEdit.Value:=LazPackage.Version.Major;
  VersionMinorSpinEdit.Value:=LazPackage.Version.Minor;
  VersionReleaseSpinEdit.Value:=LazPackage.Version.Release;
  VersionBuildSpinEdit.Value:=LazPackage.Version.Build;

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
  AddPackageUnitToProjectCheckBox.Checked:=LazPackage.AddToProjectUsesSection;

  // IDE integration
  LazDocPathEdit.Text:=LazPackage.LazDocPaths;

  // Provides
  ProvidesMemo.Lines.Assign(LazPackage.Provides);
  
  // i18n
  EnableI18NCheckBox.Checked := LazPackage.EnableI18N;
  I18NGroupBox.Enabled := LazPackage.EnableI18N;
  POOutDirEdit.Text:=LazPackage.POOutputDirectory;
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
  Position:=poScreenCenter;
  IDEDialogLayoutList.ApplyLayout(Self,450,400);
end;

destructor TPackageOptionsDialog.Destroy;
begin
  inherited Destroy;
end;

end.

