unit package_integration_options;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, ExtCtrls, StdCtrls, Dialogs,
  IDEOptionsIntf, MacroIntf, PackageIntf,
  LazarusIDEStrConsts, PackageDefs, PathEditorDlg, IDEProcs, CodeHelp;

type

  { TPackageIntegrationOptionsFrame }

  TPackageIntegrationOptionsFrame = class(TAbstractIDEOptionsEditor)
    DesignTimeRadioButton: TRadioButton;
    FPDocPackageNameEdit: TEdit;
    FPDocPackageNameLabel: TLabel;
    FPDocSearchPathsLabel: TLabel;
    DocGroupBox: TGroupBox;
    FPDocSearchPathsEdit: TEdit;
    PkgTypeGroupBox: TGroupBox;
    RunAndDesignTimeRadioButton: TRadioButton;
    RunTimeOnlyRadioButton: TRadioButton;
    RunTimeRadioButton: TRadioButton;
    UpdateRadioGroup: TRadioGroup;
    procedure FPDocPackageNameEditEnter(Sender: TObject);
    procedure FPDocPackageNameEditExit(Sender: TObject);
    procedure PkgTypeGroupBoxClick(Sender: TObject);
  private
    FLazPackage: TLazPackage;
    FPDocPathButton: TPathEditorButton;
    FStoredPkgType: TLazPackageType;
    function GetSelectedPkgType: TLazPackageType;
    procedure PathEditBtnClick(Sender: TObject);
    procedure PathEditBtnExecuted(Sender: TObject);
    procedure SetSelectedPkgType(PkgType: TLazPackageType);
    function ShowMsgPackageTypeMustBeDesign: boolean;
    function GetFPDocPkgNameEditValue: string;
  public
    function Check: Boolean; override;
    function GetTitle: string; override;
    procedure Setup(ADialog: TAbstractOptionsEditorDialog); override;
    procedure ReadSettings(AOptions: TAbstractIDEOptions); override;
    procedure WriteSettings(AOptions: TAbstractIDEOptions); override;
    class function SupportedOptionsClass: TAbstractIDEOptionsClass; override;
    function PkgTypeToCaption(t: TLazPackageType): string;
    function CaptionToPkgType(s: string): TLazPackageType;
  end;

implementation

{$R *.lfm}

{ TPackageIntegrationOptionsFrame }

procedure TPackageIntegrationOptionsFrame.PkgTypeGroupBoxClick(Sender: TObject);
var
  NewPkgType: TLazPackageType;
begin
  NewPkgType:=GetSelectedPkgType;
  if (FStoredPkgType<>NewPkgType)
  and (NewPkgType in [lptRunTime,lptRunTimeOnly])
  then begin
    // user sets to runtime
    if (FLazPackage.AutoInstall <> pitNope) then
      ShowMsgPackageTypeMustBeDesign;
  end;
end;

procedure TPackageIntegrationOptionsFrame.FPDocPackageNameEditEnter(
  Sender: TObject);
begin
  if FPDocPackageNameEdit.Text=lisDefaultPlaceholder then
    FPDocPackageNameEdit.Text:='';
end;

procedure TPackageIntegrationOptionsFrame.FPDocPackageNameEditExit(
  Sender: TObject);
begin
  if GetFPDocPkgNameEditValue='' then
    FPDocPackageNameEdit.Text:=lisDefaultPlaceholder
  else
    FPDocPackageNameEdit.Text:=GetFPDocPkgNameEditValue;
end;

procedure TPackageIntegrationOptionsFrame.PathEditBtnClick(Sender: TObject);
var
  AButton: TPathEditorButton absolute Sender;
begin
  AButton.CurrentPathEditor.Path := FPDocSearchPathsEdit.Text;
  AButton.CurrentPathEditor.Templates := '';
end;

function TPackageIntegrationOptionsFrame.GetSelectedPkgType: TLazPackageType;
begin
  if RunTimeOnlyRadioButton.Checked then
    Result:=lptRunTimeOnly
  else if DesignTimeRadioButton.Checked then
    Result:=lptDesignTime
  else if RunTimeRadioButton.Checked then
    Result:=lptRunTime
  else
    Result:=lptRunAndDesignTime;
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
  OldPath := FPDocSearchPathsEdit.Text;
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
  FPDocSearchPathsEdit.Text := NewPath;
end;

procedure TPackageIntegrationOptionsFrame.SetSelectedPkgType(
  PkgType: TLazPackageType);
begin
  case PkgType of
  lptRunTime: RunTimeRadioButton.Checked:=true;
  lptDesignTime: DesignTimeRadioButton.Checked:=true;
  lptRunAndDesignTime: RunAndDesignTimeRadioButton.Checked:=true;
  lptRunTimeOnly: RunTimeOnlyRadioButton.Checked:=true;
  end;
end;

function TPackageIntegrationOptionsFrame.GetTitle: string;
begin
  Result := lisPckOptsIDEIntegration;
end;

procedure TPackageIntegrationOptionsFrame.Setup(ADialog: TAbstractOptionsEditorDialog);
begin
  PkgTypeGroupBox.Caption := lisPckOptsPackageType;
  RunAndDesignTimeRadioButton.Caption:=PkgTypeToCaption(lptRunAndDesignTime);
  DesignTimeRadioButton.Caption:=PkgTypeToCaption(lptDesignTime);
  RunTimeRadioButton.Caption:=PkgTypeToCaption(lptRunTime);
  RunTimeOnlyRadioButton.Caption:=PkgTypeToCaption(lptRunTimeOnly);

  UpdateRadioGroup.Caption := lisPckOptsUpdateRebuild;
  UpdateRadioGroup.Items[0] := lisPckOptsAutomaticallyRebuildAsNeeded;
  UpdateRadioGroup.Items[1] := lisPckOptsAutoRebuildWhenRebuildingAll;
  UpdateRadioGroup.Items[2] := lisPckOptsManualCompilationNeverAutomatically;

  DocGroupBox.Caption := lisCodeHelpGroupBox;

  FPDocPackageNameLabel.Caption:=lisPckPackage;
  FPDocPackageNameEdit.Hint:=lisPckClearToUseThePackageName;
  FPDocSearchPathsLabel.Caption:=lisPathEditSearchPaths;
  FPDocSearchPathsEdit.Hint:=lisPckSearchPathsForFpdocXmlFilesMultiplePathsMustBeSepa;

  FPDocPathButton := TPathEditorButton.Create(Self);
  with FPDocPathButton do
  begin
    Name := 'FPDocPathButton';
    Caption := '...';
    AutoSize := True;
    Anchors := [akRight];
    AnchorParallel(akRight, 6, DocGroupBox);
    AnchorParallel(akTop, 0, FPDocSearchPathsEdit);
    AnchorParallel(akBottom, 0, FPDocSearchPathsEdit);
    OnClick := @PathEditBtnClick;
    OnExecuted := @PathEditBtnExecuted;
    Parent := DocGroupBox;
  end;
  FPDocSearchPathsEdit.AnchorToNeighbour(akRight, 0, FPDocPathButton);
end;

procedure TPackageIntegrationOptionsFrame.ReadSettings(AOptions: TAbstractIDEOptions);
var
  LazPackage: TLazPackage absolute AOptions;
begin
  FLazPackage := LazPackage;
  FStoredPkgType := LazPackage.PackageType;
  SetSelectedPkgType(FStoredPkgType);
  case LazPackage.AutoUpdate of
    pupAsNeeded: UpdateRadioGroup.ItemIndex := 0;
    pupOnRebuildingAll: UpdateRadioGroup.ItemIndex := 1;
    else
      UpdateRadioGroup.ItemIndex := 2;
  end;
  FPDocSearchPathsEdit.Text:=LazPackage.FPDocPaths;
  if LazPackage.FPDocPackageName='' then
    FPDocPackageNameEdit.Text:=lisDefaultPlaceholder
  else
    FPDocPackageNameEdit.Text:=LazPackage.FPDocPackageName;
end;

function TPackageIntegrationOptionsFrame.ShowMsgPackageTypeMustBeDesign: boolean;
begin
  if MessageDlg(lisPckOptsInvalidPackageType,
    Format(lisPckOptsThePackageHasTheAutoInstallFlagThisMeans,
           ['"', FLazPackage.IDAsString, '"', LineEnding, LineEnding]),
    mtWarning, [mbIgnore, mbCancel], 0) <> mrIgnore then
  begin
    Result := True;
    SetSelectedPkgType(FStoredPkgType);
  end
  else
    Result := False;
end;

function TPackageIntegrationOptionsFrame.GetFPDocPkgNameEditValue: string;
begin
  if FPDocPackageNameEdit.Text=lisDefaultPlaceholder then
    Result:=''
  else
    Result:=MakeValidFPDocPackageName(FPDocPackageNameEdit.Text);
end;

function TPackageIntegrationOptionsFrame.Check: Boolean;
var
  NewPkgType: TLazPackageType;
begin
  NewPkgType:=GetSelectedPkgType;
  if NewPkgType <> FLazPackage.PackageType then
  begin
    if (NewPkgType in [lptRunTime,lptRunTimeOnly])
    and (FLazPackage.AutoInstall <> pitNope) then
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
begin
  LazPackage.PackageType := GetSelectedPkgType;
  case UpdateRadioGroup.ItemIndex of
    2: LazPackage.AutoUpdate := pupManually;
    1: LazPackage.AutoUpdate := pupOnRebuildingAll;
    else
      LazPackage.AutoUpdate := pupAsNeeded;
  end;
  LazPackage.FPDocPaths := FPDocSearchPathsEdit.Text;
  LazPackage.FPDocPackageName := GetFPDocPkgNameEditValue;
end;

class function TPackageIntegrationOptionsFrame.SupportedOptionsClass: TAbstractIDEOptionsClass;
begin
  Result := TLazPackage;
end;

function TPackageIntegrationOptionsFrame.PkgTypeToCaption(t: TLazPackageType
  ): string;
begin
  case t of
  lptRunTime: Result:=lisPckOptsRuntime;
  lptDesignTime: Result:=lisPckOptsDesigntime;
  lptRunAndDesignTime: Result:=lisPckOptsDesigntimeAndRuntime;
  lptRunTimeOnly: Result:=lisRuntimeOnlyCanNotBeInstalledInIDE;
  else Result:='?'+IntToStr(ord(t));
  end;
end;

function TPackageIntegrationOptionsFrame.CaptionToPkgType(s: string
  ): TLazPackageType;
var
  t: TLazPackageType;
begin
  for t:=Low(TLazPackageType) to high(TLazPackageType) do
    if s=PkgTypeToCaption(t) then exit(t);
  Result:=lptRunTime;
end;

initialization
  RegisterIDEOptionsEditor(GroupPackage, TPackageIntegrationOptionsFrame,
    PackageOptionsIntegration);
end.

