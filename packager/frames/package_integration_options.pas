unit package_integration_options;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, ExtCtrls, StdCtrls, Dialogs,
  IDEOptionsIntf, MacroIntf,
  LazarusIDEStrConsts, PackageDefs, PathEditorDlg, IDEProcs;

type

  { TPackageIntegrationOptionsFrame }

  TPackageIntegrationOptionsFrame = class(TAbstractIDEOptionsEditor)
    LazDocGroupBox: TGroupBox;
    LazDocPathEdit: TEdit;
    PkgTypeRadioGroup: TRadioGroup;
    UpdateRadioGroup: TRadioGroup;
    procedure PkgTypeRadioGroupClick(Sender: TObject);
  private
    FLazPackage: TLazPackage;
    LazDocPathButton: TPathEditorButton;
    FStoredPkgType: Integer;
    procedure PathEditBtnClick(Sender: TObject);
    procedure PathEditBtnExecuted(Sender: TObject);
    function ShowMsgPackageTypeMustBeDesign: boolean;
  public
    function Check: Boolean; override;
    function GetTitle: string; override;
    procedure Setup(ADialog: TAbstractOptionsEditorDialog); override;
    procedure ReadSettings(AOptions: TAbstractIDEOptions); override;
    procedure WriteSettings(AOptions: TAbstractIDEOptions); override;
    class function SupportedOptionsClass: TAbstractIDEOptionsClass; override;
  end;

implementation

{$R *.lfm}

{ TPackageIntegrationOptionsFrame }

procedure TPackageIntegrationOptionsFrame.PkgTypeRadioGroupClick(Sender: TObject);
begin
  if (PkgTypeRadioGroup.ItemIndex = 1) and (FLazPackage.PackageType <> lptRunTime) then
  begin
    // user sets to runtime only
    if (FLazPackage.AutoInstall <> pitNope) then
      ShowMsgPackageTypeMustBeDesign;
  end;
end;

procedure TPackageIntegrationOptionsFrame.PathEditBtnClick(Sender: TObject);
var
  AButton: TPathEditorButton absolute Sender;
begin
  AButton.CurrentPathEditor.Path := LazDocPathEdit.Text;
  AButton.CurrentPathEditor.Templates := '';
end;

procedure TPackageIntegrationOptionsFrame.PathEditBtnExecuted(Sender: TObject);
var
  AButton: TPathEditorButton absolute Sender;
  NewPath: string;
  OldPath: string;
  CurDir: string;
  StartPos: integer;
  DlgResult: TModalResult;
  OldStartPos: longint;
begin
  if AButton.CurrentPathEditor.ModalResult <> mrOk then
    Exit;
  NewPath := AButton.CurrentPathEditor.Path;
  OldPath := LazDocPathEdit.Text;
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
  LazDocPathEdit.Text := NewPath;
end;

function TPackageIntegrationOptionsFrame.GetTitle: string;
begin
  Result := lisPckOptsIDEIntegration;
end;

procedure TPackageIntegrationOptionsFrame.Setup(ADialog: TAbstractOptionsEditorDialog);
begin
  PkgTypeRadioGroup.Caption := lisPckOptsPackageType;
  PkgTypeRadioGroup.Items[0] := lisPckOptsDesigntimeOnly;
  PkgTypeRadioGroup.Items[1] := lisPckOptsRuntimeOnly;
  PkgTypeRadioGroup.Items[2] := lisPckOptsDesigntimeAndRuntime;
  UpdateRadioGroup.Caption := lisPckOptsUpdateRebuild;
  UpdateRadioGroup.Items[0] := lisPckOptsAutomaticallyRebuildAsNeeded;
  UpdateRadioGroup.Items[1] := lisPckOptsAutoRebuildWhenRebuildingAll;
  UpdateRadioGroup.Items[2] := lisPckOptsManualCompilationNeverAutomatically;
  LazDocGroupBox.Caption := lisCodeHelpPathsGroupBox;

  LazDocPathButton := TPathEditorButton.Create(Self);
  with LazDocPathButton do
  begin
    Name := 'LazDocPathButton';
    Caption := '...';
    AutoSize := True;
    Anchors := [akRight];
    AnchorParallel(akRight, 6, LazDocGroupBox);
    AnchorParallel(akTop, 0, LazDocPathEdit);
    AnchorParallel(akBottom, 0, LazDocPathEdit);
    OnClick := @PathEditBtnClick;
    OnExecuted := @PathEditBtnExecuted;
    Parent := LazDocGroupBox;
  end;
  LazDocPathEdit.AnchorToNeighbour(akRight, 0, LazDocPathButton);
end;

procedure TPackageIntegrationOptionsFrame.ReadSettings(AOptions: TAbstractIDEOptions);
var
  LazPackage: TLazPackage absolute AOptions;
begin
  FLazPackage := LazPackage;
  case LazPackage.PackageType of
    lptDesignTime: PkgTypeRadioGroup.ItemIndex := 0;
    lptRunTime: PkgTypeRadioGroup.ItemIndex := 1;
    else
      PkgTypeRadioGroup.ItemIndex := 2;
  end;
  FStoredPkgType := PkgTypeRadioGroup.ItemIndex;
  case LazPackage.AutoUpdate of
    pupAsNeeded: UpdateRadioGroup.ItemIndex := 0;
    pupOnRebuildingAll: UpdateRadioGroup.ItemIndex := 1;
    else
      UpdateRadioGroup.ItemIndex := 2;
  end;
  LazDocPathEdit.Text := LazPackage.LazDocPaths;
end;

function TPackageIntegrationOptionsFrame.ShowMsgPackageTypeMustBeDesign: boolean;
begin
  if MessageDlg(lisPckOptsInvalidPackageType,
    Format(lisPckOptsThePackageHasTheAutoInstallFlagThisMeans,
    ['"', FLazPackage.IDAsString, '"', #13, #13]), mtWarning,
    [mbIgnore, mbCancel], 0) <> mrIgnore then
  begin
    Result := True;
    PkgTypeRadioGroup.ItemIndex := FStoredPkgType;
  end
  else
    Result := False;
end;

function TPackageIntegrationOptionsFrame.Check: Boolean;
var
  NewPackageType: TLazPackageType;
begin
  case PkgTypeRadioGroup.ItemIndex of
    0: NewPackageType := lptDesignTime;
    1: NewPackageType := lptRunTime;
    else
      NewPackageType := lptRunAndDesignTime;
  end;
  if NewPackageType <> FLazPackage.PackageType then
  begin
    if (NewPackageType = lptRunTime) and (FLazPackage.AutoInstall <> pitNope) then
    begin
      if ShowMsgPackageTypeMustBeDesign then
        Exit(False);
    end;
  end;
  Result := True;
end;

procedure TPackageIntegrationOptionsFrame.WriteSettings(AOptions: TAbstractIDEOptions);
var
  LazPackage: TLazPackage absolute AOptions;
  NewPackageType: TLazPackageType;
begin
  case PkgTypeRadioGroup.ItemIndex of
    0: NewPackageType := lptDesignTime;
    1: NewPackageType := lptRunTime;
    else
      NewPackageType := lptRunAndDesignTime;
  end;
  LazPackage.PackageType := NewPackageType;
  case UpdateRadioGroup.ItemIndex of
    2: LazPackage.AutoUpdate := pupManually;
    1: LazPackage.AutoUpdate := pupOnRebuildingAll;
    else
      LazPackage.AutoUpdate := pupAsNeeded;
  end;

  LazPackage.LazDocPaths := LazDocPathEdit.Text;
end;

class function TPackageIntegrationOptionsFrame.SupportedOptionsClass: TAbstractIDEOptionsClass;
begin
  Result := TLazPackage;
end;

initialization
  RegisterIDEOptionsEditor(GroupPackage, TPackageIntegrationOptionsFrame,
    PackageOptionsIntegration);
end.

