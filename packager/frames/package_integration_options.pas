unit package_integration_options;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  // LCL
  Forms, Controls, ExtCtrls, StdCtrls, Dialogs,
  // LazUtils
  LazFileUtils,
  // IdeIntf
  IDEOptionsIntf, IDEOptEditorIntf, MacroIntf, PackageIntf,
  // IDE
  PackageDefs, LazarusIDEStrConsts, PathEditorDlg, IDEProcs, CodeHelp;

type

  { TPackageIntegrationOptionsFrame }

  TPackageIntegrationOptionsFrame = class(TAbstractIDEOptionsEditor)
    DesignTimeRadioButton: TRadioButton;
    DocGroupBox: TGroupBox;
    FPDocPackageNameEdit: TEdit;
    FPDocPackageNameLabel: TLabel;
    FPDocSearchPathsEdit: TEdit;
    FPDocSearchPathsLabel: TLabel;
    PkgTypeGroupBox: TGroupBox;
    RunAndDesignTimeRadioButton: TRadioButton;
    RunTimeOnlyRadioButton: TRadioButton;
    RunTimeRadioButton: TRadioButton;
    UpdateRadioGroup: TRadioGroup;
    procedure PkgTypeGroupBoxClick(Sender: TObject);
  private
    FLazPackage: TLazPackage;
    FPDocPathButton: TPathEditorButton;
    FStoredPkgType: TLazPackageType;
    procedure FPDocPathButtonClick(Sender: TObject);
    function GetSelectedPkgType: TLazPackageType;
    function PathEditBtnExecuted(Context: String; var NewPath: String): Boolean;
    procedure SetSelectedPkgType(PkgType: TLazPackageType);
    function ShowMsgPackageTypeMustBeDesign: boolean;
    function GetFPDocPkgNameEditValue: string;
  public
    function Check: Boolean; override;
    function GetTitle: string; override;
    procedure Setup({%H-}ADialog: TAbstractOptionsEditorDialog); override;
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

procedure TPackageIntegrationOptionsFrame.FPDocPathButtonClick(Sender: TObject);
begin
  FPDocPathButton.CurrentPathEditor.BaseDirectory:=FLazPackage.Directory;
end;

function TPackageIntegrationOptionsFrame.PathEditBtnExecuted(Context: String;
  var NewPath: String): Boolean;
var
  CurDir: string;
  StartPos, OldStartPos: integer;
  DlgResult: TModalResult;
  s: String;
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
        s:=Format(lisDirectoryNotFound, [CurDir]);
        if Context<>'' then
          s:=Context+LineEnding+s;
        DlgResult := QuestionDlg(lisEnvOptDlgDirectoryNotFound,
          s,
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

procedure TPackageIntegrationOptionsFrame.SetSelectedPkgType(PkgType: TLazPackageType);
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
  RunAndDesignTimeRadioButton.Caption := PkgTypeToCaption(lptRunAndDesignTime);
  RunAndDesignTimeRadioButton.Hint := lisRunAndDesignTimePackagesHaveNoLimitations;
  DesignTimeRadioButton.Caption := PkgTypeToCaption(lptDesignTime);
  DesignTimeRadioButton.Hint := lisDesignTimePackagesAddComponentsAndMenuItemsToTheID;
  RunTimeRadioButton.Caption := PkgTypeToCaption(lptRunTime);
  RunTimeRadioButton.Hint := lisRunTimePackagesCanBeUsedByProjectsTheyCanNotBeInst;
  RunTimeOnlyRadioButton.Caption := PkgTypeToCaption(lptRunTimeOnly);
  RunTimeOnlyRadioButton.Hint := lisRunTimeOnlyPackagesAreOnlyForProjectsTheyCanNotBeI;

  UpdateRadioGroup.Caption := lisPckOptsUpdateRebuild;
  UpdateRadioGroup.Items[0] := lisPckOptsAutomaticallyRebuildAsNeeded;
  UpdateRadioGroup.Items[1] := lisPckOptsAutoRebuildWhenRebuildingAll;
  UpdateRadioGroup.Items[2] := lisPckOptsManualCompilationNeverAutomatically;

  DocGroupBox.Caption := lisCodeHelpGroupBox;

  FPDocPackageNameLabel.Caption := lisPckPackage;
  FPDocPackageNameEdit.Hint := lisPckClearToUseThePackageName;
  FPDocSearchPathsLabel.Caption := lisPathEditSearchPaths;
  // ToDo: remove the resource string later.
  //FPDocSearchPathsEdit.Hint := lisPckSearchPathsForFpdocXmlFilesMultiplePathsMustBeSepa;

  FPDocPathButton := TPathEditorButton.Create(Self);
  with FPDocPathButton do
  begin
    Name := 'FPDocPathButton';
    Caption := '...';
    AutoSize := False;
    Width := Height;
    Anchors := [akRight];
    AnchorParallel(akRight, 6, DocGroupBox);
    AnchorParallel(akTop, 0, FPDocSearchPathsEdit);
    AnchorParallel(akBottom, 0, FPDocSearchPathsEdit);
    AssociatedEdit := FPDocSearchPathsEdit;
    OnClick:=@FPDocPathButtonClick;
    OnExecuted := @PathEditBtnExecuted;
    Parent := DocGroupBox;
  end;
  FPDocSearchPathsEdit.AnchorToNeighbour(akRight, 0, FPDocPathButton);
end;

procedure TPackageIntegrationOptionsFrame.ReadSettings(AOptions: TAbstractIDEOptions);
begin
  FLazPackage := (AOptions as TPackageIDEOptions).Package;
  FStoredPkgType := FLazPackage.PackageType;
  SetSelectedPkgType(FStoredPkgType);
  case FLazPackage.AutoUpdate of
    pupAsNeeded: UpdateRadioGroup.ItemIndex := 0;
    pupOnRebuildingAll: UpdateRadioGroup.ItemIndex := 1;
    else
      UpdateRadioGroup.ItemIndex := 2;
  end;
  SetPathTextAndHint(FLazPackage.FPDocPaths, FPDocSearchPathsEdit);

  FPDocPackageNameEdit.TextHint:=lisDefaultPlaceholder;
  FPDocPackageNameEdit.Text:=FLazPackage.FPDocPackageName;
end;

function TPackageIntegrationOptionsFrame.ShowMsgPackageTypeMustBeDesign: boolean;
begin
  if MessageDlg(lisPckOptsInvalidPackageType,
    Format(lisPckOptsThePackageHasTheAutoInstallFlagThisMeans,
           [FLazPackage.IDAsString, LineEnding, LineEnding]),
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
  LazPackage: TLazPackage;
begin
  LazPackage := (AOptions as TPackageIDEOptions).Package;
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
  Result := TPackageIDEOptions;
end;

function TPackageIntegrationOptionsFrame.PkgTypeToCaption(t: TLazPackageType): string;
begin
  case t of
  lptRunTime: Result:=lisPckOptsRuntime;
  lptDesignTime: Result:=lisPckOptsDesigntime;
  lptRunAndDesignTime: Result:=lisPckOptsDesigntimeAndRuntime;
  lptRunTimeOnly: Result:=lisRuntimeOnlyCanNotBeInstalledInIDE;
  else Result:='?'+IntToStr(ord(t));
  end;
end;

function TPackageIntegrationOptionsFrame.CaptionToPkgType(s: string): TLazPackageType;
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

