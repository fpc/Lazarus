unit Package_Usage_Options;

{$mode objfpc}{$H+}

interface

uses
  SysUtils,
  // LazUtils
  LazFileUtils,
  // LCL
  Forms, Controls, StdCtrls, Dialogs,
  // IdeIntf
  IDEOptionsIntf, IDEOptEditorIntf, MacroIntf,
  // IDE
  PathEditorDlg, LazarusIDEStrConsts, IDEProcs, PackageDefs;

type

  { TPackageUsageOptionsFrame }

  TPackageUsageOptionsFrame = class(TAbstractIDEOptionsEditor)
    AddOptionsGroupBox: TGroupBox;
    AddPackageUnitToProjectCheckBox: TCheckBox;
    CompatibilityModeCheckBox: TCheckBox;
    AddPathsGroupBox: TGroupBox;
    CustomOptionsLabel: TLabel;
    CustomOptionsMemo: TMemo;
    IncludePathEdit: TEdit;
    IncludePathLabel: TLabel;
    LibraryPathEdit: TEdit;
    LibraryPathLabel: TLabel;
    LinkerOptionsLabel: TLabel;
    LinkerOptionsMemo: TMemo;
    ObjectPathEdit: TEdit;
    ObjectPathLabel: TLabel;
    ProjectGroupBox: TGroupBox;
    PackageGroupBox: TGroupBox;
    UnitPathEdit: TEdit;
    UnitPathLabel: TLabel;
  private
    UnitPathButton: TPathEditorButton;
    IncludePathButton: TPathEditorButton;
    ObjectPathButton: TPathEditorButton;
    LibraryPathButton: TPathEditorButton;
    FLazPackage: TLazPackage;
    function PathEditBtnExecuted({%H-}Context: String; var NewPath: String): Boolean;
  public
    function GetTitle: string; override;
    procedure Setup({%H-}ADialog: TAbstractOptionsEditorDialog); override;
    procedure ReadSettings(AOptions: TAbstractIDEOptions); override;
    procedure WriteSettings(AOptions: TAbstractIDEOptions); override;
    class function SupportedOptionsClass: TAbstractIDEOptionsClass; override;
  end;

implementation

{$R *.lfm}

{ TPackageUsageOptionsFrame }

function TPackageUsageOptionsFrame.PathEditBtnExecuted(Context: String; var NewPath: String): Boolean;
var
  CurDir: string;
  StartPos, OldStartPos: integer;
  DlgResult: TModalResult;
begin
  // check NewPath
  StartPos := 1;
  repeat
    OldStartPos := StartPos;
    CurDir := GetNextDirectoryInSearchPath(NewPath, StartPos);
    if CurDir <> '' then
    begin
      IDEMacros.SubstituteMacros(CurDir);
      FLazPackage.LongenFilename(CurDir);
      if not DirPathExists(CurDir) then
      begin
        DlgResult := QuestionDlg(lisEnvOptDlgDirectoryNotFound,
          Format(lisDirectoryNotFound, [CurDir]),
          mtError, [mrIgnore, mrYes, lisRemoveFromSearchPath, mrCancel], 0);
        case DlgResult of
          mrIgnore: ;
          mrYes:
          begin  // remove directory from search path
            NewPath := copy(NewPath,1,OldStartPos-1) + copy(NewPath,StartPos,length(NewPath));
            StartPos := OldStartPos;
          end;
          else   // undo
            Exit(False);
        end;
      end;
    end;
  until StartPos > length(NewPath);
  Result := True;
end;

function TPackageUsageOptionsFrame.GetTitle: string;
begin
  Result := lisPckOptsUsage;
end;

procedure TPackageUsageOptionsFrame.Setup(ADialog: TAbstractOptionsEditorDialog);
begin
  AddPathsGroupBox.Caption := lisPckOptsAddPathsToDependentPackagesProjects;
  UnitPathLabel.Caption := lisUnit;
  IncludePathLabel.Caption := lisPckOptsInclude;
  ObjectPathLabel.Caption := lisPckOptsObject;
  LibraryPathLabel.Caption := lisPckOptsLibrary;
  AddOptionsGroupBox.Caption := lisPckOptsAddOptionsToDependentPackagesAndProjects;
  LinkerOptionsLabel.Caption := lisPckOptsLinker;
  CustomOptionsLabel.Caption := lisPckOptsCustom;

  UnitPathButton := TPathEditorButton.Create(Self);
  with UnitPathButton do
  begin
    Name := 'UnitPathButton';
    Parent := AddPathsGroupBox;
    Caption := '...';
    AutoSize := False;
    Width := Height;
    Anchors := [akRight];
    AnchorParallel(akRight, 6, AddPathsGroupBox);
    AnchorParallel(akTop, 0, UnitPathEdit);
    AnchorParallel(akBottom, 0, UnitPathEdit);
    AssociatedEdit := UnitPathEdit;
    Templates := '$(PkgOutDir)' +
       '$(LazarusDir)/lcl/units/$(TargetCPU)-$(TargetOS)' +
      ';$(LazarusDir)/lcl/units/$(TargetCPU)-$(TargetOS)/$(LCLWidgetType)' +
      ';$(LazarusDir)/components/codetools/units/$(TargetCPU)-$(TargetOS)' +
      ';$(LazarusDir)/components/custom' +
      ';$(LazarusDir)/packager/units/$(TargetCPU)-$(TargetOS)';
    OnExecuted := @PathEditBtnExecuted;
  end;
  UnitPathEdit.AnchorToNeighbour(akRight,0,UnitPathButton);

  IncludePathButton := TPathEditorButton.Create(Self);
  with IncludePathButton do
  begin
    Name := 'IncludePathButton';
    Parent := AddPathsGroupBox;
    Caption := '...';
    AutoSize := False;
    Width := Height;
    Anchors := [akRight];
    AnchorParallel(akRight, 6, AddPathsGroupBox);
    AnchorParallel(akTop, 0, IncludePathEdit);
    AnchorParallel(akBottom, 0, IncludePathEdit);
    AssociatedEdit := IncludePathEdit;
    Templates := 'include';
    OnExecuted := @PathEditBtnExecuted;
  end;
  IncludePathEdit.AnchorToNeighbour(akRight,0,IncludePathButton);

  ObjectPathButton := TPathEditorButton.Create(Self);
  with ObjectPathButton do
  begin
    Name := 'ObjectPathButton';
    Parent := AddPathsGroupBox;
    Caption := '...';
    AutoSize := False;
    Width := Height;
    Anchors := [akRight];
    AnchorParallel(akRight, 6, AddPathsGroupBox);
    AnchorParallel(akTop, 0, ObjectPathEdit);
    AnchorParallel(akBottom, 0, ObjectPathEdit);
    AssociatedEdit := ObjectPathEdit;
    Templates := 'objects';
    OnExecuted := @PathEditBtnExecuted;
  end;
  ObjectPathEdit.AnchorToNeighbour(akRight,0,ObjectPathButton);

  LibraryPathButton := TPathEditorButton.Create(Self);
  with LibraryPathButton do
  begin
    Name := 'LibraryPathButton';
    Parent := AddPathsGroupBox;
    Caption := '...';
    AutoSize := False;
    Width := Height;
    Anchors := [akRight];
    AnchorParallel(akRight, 6, AddPathsGroupBox);
    AnchorParallel(akTop, 0, LibraryPathEdit);
    AnchorParallel(akBottom, 0, LibraryPathEdit);
    AssociatedEdit := LibraryPathEdit;
    OnExecuted := @PathEditBtnExecuted;
  end;
  LibraryPathEdit.AnchorToNeighbour(akRight,0,LibraryPathButton);

  ProjectGroupBox.Caption := dlgProject;
  AddPackageUnitToProjectCheckBox.Caption := podAddPackageUnitToUsesSection;
  PackageGroupBox.Caption := lisPackage;
  CompatibilityModeCheckBox.Caption := lisLPKCompatibilityModeCheckBox;
  CompatibilityModeCheckBox.Hint := lisLPKCompatibilityModeCheckBoxHint;
end;

procedure TPackageUsageOptionsFrame.ReadSettings(AOptions: TAbstractIDEOptions);
begin
  FLazPackage := (AOptions as TPackageIDEOptions).Package;
  with FLazPackage.UsageOptions do
  begin
    SetPathTextAndHint(UnitPath, UnitPathEdit);
    SetPathTextAndHint(IncludePath, IncludePathEdit);
    SetPathTextAndHint(ObjectPath, ObjectPathEdit);
    SetPathTextAndHint(LibraryPath, LibraryPathEdit);
    LinkerOptionsMemo.Text := LinkerOptions;
    CustomOptionsMemo.Text := CustomOptions;
  end;
  AddPackageUnitToProjectCheckBox.Checked := FLazPackage.AddToProjectUsesSection;
  CompatibilityModeCheckBox.Checked := FLazPackage.UseLegacyLists;
end;

procedure TPackageUsageOptionsFrame.WriteSettings(AOptions: TAbstractIDEOptions);
var
  LazPackage: TLazPackage;
begin
  LazPackage := (AOptions as TPackageIDEOptions).Package;
  with LazPackage.UsageOptions do
  begin
    UnitPath := TrimSearchPath(UnitPathEdit.Text, '');
    IncludePath := TrimSearchPath(IncludePathEdit.Text, '');
    ObjectPath := TrimSearchPath(ObjectPathEdit.Text, '');
    LibraryPath := TrimSearchPath(LibraryPathEdit.Text, '');
    LinkerOptions := LinkerOptionsMemo.Text;
    CustomOptions := CustomOptionsMemo.Text;
  end;
  LazPackage.AddToProjectUsesSection := AddPackageUnitToProjectCheckBox.Checked;
  FLazPackage.UseLegacyLists := CompatibilityModeCheckBox.Checked;
end;

class function TPackageUsageOptionsFrame.SupportedOptionsClass: TAbstractIDEOptionsClass;
begin
  Result := TPackageIDEOptions;
end;

initialization
  RegisterIDEOptionsEditor(GroupPackage, TPackageUsageOptionsFrame, PackageOptionsUsage);
end.

