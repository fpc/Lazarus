unit package_usage_options;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, StdCtrls, Dialogs, PathEditorDlg,
  IDEOptionsIntf, MacroIntf,
  LazarusIDEStrConsts, IDEProcs, PackageDefs;

type

  { TPackageUsageOptionsFrame }

  TPackageUsageOptionsFrame = class(TAbstractIDEOptionsEditor)
    AddOptionsGroupBox: TGroupBox;
    AddPackageUnitToProjectCheckBox: TCheckBox;
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
    UnitPathEdit: TEdit;
    UnitPathLabel: TLabel;
  private
    UnitPathButton: TPathEditorButton;
    IncludePathButton: TPathEditorButton;
    ObjectPathButton: TPathEditorButton;
    LibraryPathButton: TPathEditorButton;
    FLazPackage: TLazPackage;
    procedure PathEditBtnClick(Sender: TObject);
    procedure PathEditBtnExecuted(Sender: TObject);
    function GetEditForPathButton(AButton: TPathEditorButton): TEdit;
  public
    function GetTitle: string; override;
    procedure Setup(ADialog: TAbstractOptionsEditorDialog); override;
    procedure ReadSettings(AOptions: TAbstractIDEOptions); override;
    procedure WriteSettings(AOptions: TAbstractIDEOptions); override;
    class function SupportedOptionsClass: TAbstractIDEOptionsClass; override;
  end;

implementation

{$R *.lfm}

{ TPackageUsageOptionsFrame }

procedure TPackageUsageOptionsFrame.PathEditBtnClick(Sender: TObject);
var
  AButton: TPathEditorButton;
  OldPath: string;
  AnEdit: TEdit;
  Templates: string;
begin
  if not (Sender is TPathEditorButton) then
    exit;
  AButton := TPathEditorButton(Sender);
  AnEdit := GetEditForPathButton(AButton);
  OldPath := AnEdit.Text;
  if AButton = UnitPathButton then
  begin
    Templates := SetDirSeparators('$(PkgOutDir)' +
      '$(LazarusDir)/lcl/units/$(TargetCPU)-$(TargetOS)' +
      ';$(LazarusDir)/lcl/units/$(TargetCPU)-$(TargetOS)/$(LCLWidgetType)' +
      ';$(LazarusDir)/components/codetools/units/$(TargetCPU)-$(TargetOS)' +
      ';$(LazarusDir)/components/custom' +
      ';$(LazarusDir)/packager/units/$(TargetCPU)-$(TargetOS)');
  end
  else if AButton = IncludePathButton then
  begin
    Templates := 'include';
  end
  else
  if AButton = ObjectPathButton then
  begin
    Templates := 'objects';
  end
  else
  if AButton = LibraryPathButton then
  begin
    Templates := '';
  end;
  AButton.CurrentPathEditor.Path := OldPath;
  AButton.CurrentPathEditor.Templates := SetDirSeparators(Templates);
end;

procedure TPackageUsageOptionsFrame.PathEditBtnExecuted(Sender: TObject);
var
  AButton: TPathEditorButton;
  NewPath: string;
  AnEdit: TEdit;
  OldPath: string;
  CurDir: string;
  StartPos: integer;
  DlgResult: TModalResult;
  OldStartPos: longint;
begin
  if not (Sender is TPathEditorButton) then
    exit;
  AButton := TPathEditorButton(Sender);
  if AButton.CurrentPathEditor.ModalResult <> mrOk then
    exit;
  NewPath := AButton.CurrentPathEditor.Path;
  AnEdit := GetEditForPathButton(AButton);
  OldPath := AnEdit.Text;
  if OldPath <> NewPath then
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
            Format(lisDirectoryNotFound, ['"', CurDir, '"']),
            mtError, [mrIgnore, mrYes, lisRemoveFromSearchPath, mrCancel], 0);
          case DlgResult of
            mrIgnore: ;
            mrYes:
            begin
              // remove directory from search path
              NewPath := copy(NewPath, 1, OldStartPos - 1) +
                copy(NewPath, StartPos, length(NewPath));
              StartPos := OldStartPos;
            end;
            else
              // undo
              NewPath := OldPath;
              break;
          end;
        end;
      end;
    until StartPos > length(NewPath);
  end;
  AnEdit.Text := NewPath;
end;

function TPackageUsageOptionsFrame.GetEditForPathButton(
  AButton: TPathEditorButton): TEdit;
begin
  if AButton = UnitPathButton then
    Result := UnitPathEdit
  else if AButton = IncludePathButton then
    Result := IncludePathEdit
  else if AButton = ObjectPathButton then
    Result := ObjectPathEdit
  else if AButton = LibraryPathButton then
    Result := LibraryPathEdit
  else
    Result := nil;
end;

function TPackageUsageOptionsFrame.GetTitle: string;
begin
  Result := lisPckOptsUsage;
end;

procedure TPackageUsageOptionsFrame.Setup(ADialog: TAbstractOptionsEditorDialog);
begin
  AddPathsGroupBox.Caption := lisPckOptsAddPathsToDependentPackagesProjects;
  UnitPathLabel.Caption := lisPkgFileTypeUnit;
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
    AutoSize := True;
    Anchors := [akRight];
    AnchorParallel(akRight, 6, AddPathsGroupBox);
    AnchorParallel(akTop, 0, UnitPathEdit);
    AnchorParallel(akBottom, 0, UnitPathEdit);
    OnClick := @PathEditBtnClick;
    OnExecuted := @PathEditBtnExecuted;
  end;
  UnitPathEdit.AnchorToNeighbour(akRight,0,UnitPathButton);

  IncludePathButton := TPathEditorButton.Create(Self);
  with IncludePathButton do
  begin
    Name := 'IncludePathButton';
    Parent := AddPathsGroupBox;
    Caption := '...';
    AutoSize := True;
    Anchors := [akRight];
    AnchorParallel(akRight, 6, AddPathsGroupBox);
    AnchorParallel(akTop, 0, IncludePathEdit);
    AnchorParallel(akBottom, 0, IncludePathEdit);
    OnClick := @PathEditBtnClick;
    OnExecuted := @PathEditBtnExecuted;
  end;
  IncludePathEdit.AnchorToNeighbour(akRight,0,IncludePathButton);

  ObjectPathButton := TPathEditorButton.Create(Self);
  with ObjectPathButton do
  begin
    Name := 'ObjectPathButton';
    Parent := AddPathsGroupBox;
    Caption := '...';
    AutoSize := True;
    Anchors := [akRight];
    AnchorParallel(akRight, 6, AddPathsGroupBox);
    AnchorParallel(akTop, 0, ObjectPathEdit);
    AnchorParallel(akBottom, 0, ObjectPathEdit);
    OnClick := @PathEditBtnClick;
    OnExecuted := @PathEditBtnExecuted;
  end;
  ObjectPathEdit.AnchorToNeighbour(akRight,0,ObjectPathButton);

  LibraryPathButton := TPathEditorButton.Create(Self);
  with LibraryPathButton do
  begin
    Name := 'LibraryPathButton';
    Parent := AddPathsGroupBox;
    Caption := '...';
    AutoSize := True;
    Anchors := [akRight];
    AnchorParallel(akRight, 6, AddPathsGroupBox);
    AnchorParallel(akTop, 0, LibraryPathEdit);
    AnchorParallel(akBottom, 0, LibraryPathEdit);
    OnClick := @PathEditBtnClick;
    OnExecuted := @PathEditBtnExecuted;
  end;
  LibraryPathEdit.AnchorToNeighbour(akRight,0,LibraryPathButton);

  ProjectGroupBox.Caption := dlgEnvProject;
  AddPackageUnitToProjectCheckBox.Caption := podAddPackageUnitToUsesSection;
end;

procedure TPackageUsageOptionsFrame.ReadSettings(AOptions: TAbstractIDEOptions);
var
  LazPackage: TLazPackage absolute AOptions;
begin
  FLazPackage := LazPackage;
  with LazPackage.UsageOptions do
  begin
    UnitPathEdit.Text := UnitPath;
    IncludePathEdit.Text := IncludePath;
    ObjectPathEdit.Text := ObjectPath;
    LibraryPathEdit.Text := LibraryPath;
    LinkerOptionsMemo.Text := LinkerOptions;
    CustomOptionsMemo.Text := CustomOptions;
  end;
  AddPackageUnitToProjectCheckBox.Checked := LazPackage.AddToProjectUsesSection;
end;

procedure TPackageUsageOptionsFrame.WriteSettings(AOptions: TAbstractIDEOptions);
var
  LazPackage: TLazPackage absolute AOptions;
begin
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
end;

class function TPackageUsageOptionsFrame.SupportedOptionsClass: TAbstractIDEOptionsClass;
begin
  Result := TLazPackage;
end;

initialization
  RegisterIDEOptionsEditor(GroupPackage, TPackageUsageOptionsFrame, PackageOptionsUsage);
end.

