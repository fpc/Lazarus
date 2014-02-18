unit compiler_compilation_options;

{$mode objfpc}{$H+}

interface

uses
  Controls, StdCtrls, Dialogs, IDEOptionsIntf, Project, CompilerOptions,
  CompOptsIntf, IDEDialogs, IDEUtils, FileProcs, DefineTemplates,
  CodeToolManager, PackageDefs, LazarusIDEStrConsts, EnvironmentOpts, LazConf,
  IDEProcs, InputHistory, InitialSetupDlgs, Classes, sysutils;

type

  { TCompilerCompilationOptionsFrame }

  TCompilerCompilationOptionsFrame = class(TAbstractIDEOptionsEditor)
    BrowseCompilerButton: TButton;
    chkCompilerBuild: TCheckBox;
    chkCompilerCompile: TCheckBox;
    chkCompilerRun: TCheckBox;
    chkCreateMakefile: TCheckBox;
    chkExecAfterBuild: TCheckBox;
    chkExecAfterCompile: TCheckBox;
    chkExecAfterRun: TCheckBox;
    chkExecBeforeBuild: TCheckBox;
    chkExecBeforeCompile: TCheckBox;
    chkExecBeforeRun: TCheckBox;
    cobCompiler: TComboBox;
    ExecuteAfterCommandEdit: TEdit;
    ExecuteAfterCommandLabel: TLabel;
    ExecuteAfterGroupBox: TGroupBox;
    ExecuteAfterScanFPCCheckBox: TCheckBox;
    ExecuteAfterScanLabel: TLabel;
    ExecuteAfterScanMakeCheckBox: TCheckBox;
    ExecuteAfterShowAllCheckBox: TCheckBox;
    ExecuteBeforeCommandEdit: TEdit;
    ExecuteBeforeCommandLabel: TLabel;
    ExecuteBeforeGroupBox: TGroupBox;
    ExecuteBeforeScanFPCCheckBox: TCheckBox;
    ExecuteBeforeScanLabel: TLabel;
    ExecuteBeforeScanMakeCheckBox: TCheckBox;
    ExecuteBeforeShowAllCheckBox: TCheckBox;
    grpCompiler: TGroupBox;
    lblCompiler: TLabel;
    lblRunIfCompiler: TLabel;
    lblRunIfExecAfter: TLabel;
    lblRunIfExecBefore: TLabel;
    procedure BrowseCompilerButtonClick(Sender: TObject);
  private
  public
    function GetTitle: string; override;
    procedure Setup(ADialog: TAbstractOptionsEditorDialog); override;
    procedure ReadSettings(AOptions: TAbstractIDEOptions); override;
    procedure WriteSettings(AOptions: TAbstractIDEOptions); override;
    class function SupportedOptionsClass: TAbstractIDEOptionsClass; override;
  end;

implementation

{$R *.lfm}

{ TCompilerCompilationOptionsFrame }

procedure TCompilerCompilationOptionsFrame.BrowseCompilerButtonClick(
  Sender: TObject);
var
  OpenDialog: TOpenDialog;
  AFilename: string;
  Quality: TSDFilenameQuality;
  Note: string;
begin
  OpenDialog:=TOpenDialog.Create(nil);
  try
    InputHistories.ApplyFileDialogSettings(OpenDialog);
    OpenDialog.Options:=OpenDialog.Options+[ofPathMustExist];
    // set title
    if Sender=BrowseCompilerButton then
      OpenDialog.Title:=Format(lisChooseCompilerPath,[GetDefaultCompilerFilename])
    else
      exit;

    if OpenDialog.Execute then begin
      AFilename:=CleanAndExpandFilename(OpenDialog.Filename);

      if Sender=BrowseCompilerButton then begin
        // check compiler filename
        if IsFPCExecutable(AFilename) then begin
          // check compiler
          Quality:=CheckCompilerQuality(AFilename,Note,
                                     CodeToolBoss.FPCDefinesCache.TestFilename);
          if Quality<>sddqCompatible then begin
            if IDEMessageDialog(lisCCOWarningCaption, Format(
              lisTheCompilerFileDoesNotLookCorrect, [AFilename, #13, Note]),
              mtWarning,[mbIgnore,mbCancel])<>mrIgnore
            then
              exit;
          end;
        end else begin
          // maybe a script
        end;
        SetComboBoxText(cobCompiler,AFilename,cstFilename);
      end;
    end;
    InputHistories.StoreFileDialogSettings(OpenDialog);
  finally
    OpenDialog.Free;
  end;
end;

function TCompilerCompilationOptionsFrame.GetTitle: string;
begin
  Result := dlgCOCompilerCommands;
end;

procedure TCompilerCompilationOptionsFrame.Setup(ADialog: TAbstractOptionsEditorDialog);
begin
  chkCreateMakefile.Caption := dlgCOCreateMakefile;

  ExecuteBeforeGroupBox.Caption := lisCOExecuteBefore;
  chkExecBeforeBuild.Caption := lisBuildStage;
  chkExecBeforeCompile.Caption := lisCompileStage;
  chkExecBeforeRun.Caption := lisRunStage;
  ExecuteBeforeCommandEdit.Text := '';
  ExecuteBeforeCommandLabel.Caption := lisCOCommand;
  ExecuteBeforeScanLabel.Caption := lisCOScanForMessages;
  ExecuteBeforeScanFPCCheckBox.Caption := 'FPC'; // do not translate name
  ExecuteBeforeScanMakeCheckBox.Caption := 'make'; // do not translate name
  ExecuteBeforeShowAllCheckBox.Caption := lisA2PShowAll;
  lblRunIfExecBefore.Caption := lisCOCallOn;

  grpCompiler.Caption := lisCompiler;
  lblRunIfCompiler.Caption := lisCOCallOn;
  chkCompilerBuild.Caption := lisBuildStage;
  chkCompilerBuild.Checked := True;
  chkCompilerCompile.Caption := lisCompileStage;
  chkCompilerCompile.Checked := True;
  chkCompilerRun.Caption := lisRunStage;
  chkCompilerRun.Checked := True;
  lblCompiler.Caption := lisCOCommand;
  cobCompiler.Text := '';
  BrowseCompilerButton.Hint:='Browse and select a compiler (e.g. ppcx64'+ExeExt+')';

  ExecuteAfterGroupBox.Caption := lisCOExecuteAfter;
  chkExecAfterBuild.Caption := lisBuildStage;
  chkExecAfterCompile.Caption := lisCompileStage;
  chkExecAfterRun.Caption := lisRunStage;
  ExecuteAfterCommandEdit.Text := '';
  ExecuteAfterCommandLabel.Caption := lisCOCommand;
  ExecuteAfterScanLabel.Caption := lisCOScanForMessages;
  ExecuteAfterScanFPCCheckBox.Caption := 'FPC'; // do not translate name
  ExecuteAfterScanMakeCheckBox.Caption := 'make'; // do not translate name
  ExecuteAfterShowAllCheckBox.Caption := lisA2PShowAll;
  lblRunIfExecAfter.Caption := lisCOCallOn;
end;

procedure TCompilerCompilationOptionsFrame.ReadSettings(AOptions: TAbstractIDEOptions);
var
  Options: TBaseCompilerOptions absolute AOptions;
begin
  chkCreateMakefile.Checked := Options.CreateMakefileOnBuild;

  ExecuteBeforeCommandEdit.Text := Options.ExecuteBefore.Command;
  ExecuteBeforeScanFPCCheckBox.Checked := Options.ExecuteBefore.ScanForFPCMessages;
  ExecuteBeforeScanMakeCheckBox.Checked :=
    Options.ExecuteBefore.ScanForMakeMessages;
  ExecuteBeforeShowAllCheckBox.Checked := Options.ExecuteBefore.ShowAllMessages;
  if Options.ExecuteBefore is TProjectCompilationToolOptions then
    with TProjectCompilationToolOptions(Options.ExecuteBefore) do
    begin
      chkExecBeforeCompile.Checked := crCompile in CompileReasons;
      chkExecBeforeBuild.Checked := crBuild in CompileReasons;
      chkExecBeforeRun.Checked := crRun in CompileReasons;
      lblRunIfExecBefore.Visible := True;
      chkExecBeforeCompile.Visible := True;
      chkExecBeforeBuild.Visible := True;
      chkExecBeforeRun.Visible := True;
    end
  else
  begin
    lblRunIfExecBefore.Visible := False;
    chkExecBeforeCompile.Visible := False;
    chkExecBeforeBuild.Visible := False;
    chkExecBeforeRun.Visible := False;
  end;

  with cobCompiler do begin
    Items.BeginUpdate;
    Items.Assign(EnvironmentOptions.CompilerFileHistory);
    AddFilenameToList(Items,'$(CompPath)');
    SetComboBoxText(cobCompiler,Options.CompilerPath,cstFilename);
    Items.EndUpdate;
  end;

  if Options is TProjectCompilerOptions then
    with TProjectCompilerOptions(Options) do
    begin
      chkCreateMakefile.Enabled:=false;
      lblRunIfCompiler.Visible := True;
      chkCompilerCompile.AnchorToNeighbour(akLeft, 30, lblRunIfCompiler);
      chkCompilerCompile.Checked := crCompile in CompileReasons;
      chkCompilerBuild.Checked := crBuild in CompileReasons;
      chkCompilerRun.Checked := crRun in CompileReasons;
      chkCompilerCompile.Caption := lisCompileStage;
      chkCompilerCompile.Visible := True;
      chkCompilerBuild.Visible := True;
      chkCompilerRun.Visible := True;
      cobCompiler.AnchorToNeighbour(akTop, 0, chkCompilerCompile);
    end
  else if Options is TPkgCompilerOptions then
  begin
    chkCreateMakefile.Enabled:=true;
    lblRunIfCompiler.Visible := False;
    chkCompilerCompile.AnchorParallel(akTop, 6, chkCompilerCompile.Parent);
    chkCompilerCompile.AnchorParallel(akLeft, 6, chkCompilerCompile.Parent);
    chkCompilerCompile.Visible := True;
    chkCompilerCompile.Caption := lisCOSkipCallingCompiler;
    chkCompilerCompile.Checked := TPkgCompilerOptions(Options).SkipCompiler;
    chkCompilerBuild.Visible := False;
    chkCompilerRun.Visible := False;
    cobCompiler.AnchorToNeighbour(akTop, 0, chkCompilerCompile);
  end
  else
  begin
    lblRunIfCompiler.Visible := False;
    chkCompilerCompile.Visible := False;
    chkCompilerBuild.Visible := False;
    chkCompilerRun.Visible := False;
    cobCompiler.AnchorParallel(akTop, 0, lblCompiler.Parent);
  end;

  ExecuteAfterCommandEdit.Text := Options.ExecuteAfter.Command;
  ExecuteAfterScanFPCCheckBox.Checked := Options.ExecuteAfter.ScanForFPCMessages;
  ExecuteAfterScanMakeCheckBox.Checked := Options.ExecuteAfter.ScanForMakeMessages;
  ExecuteAfterShowAllCheckBox.Checked := Options.ExecuteAfter.ShowAllMessages;
  if Options.ExecuteAfter is TProjectCompilationToolOptions then
    with TProjectCompilationToolOptions(Options.ExecuteAfter) do
    begin
      chkExecAfterCompile.Checked := crCompile in CompileReasons;
      chkExecAfterBuild.Checked := crBuild in CompileReasons;
      chkExecAfterRun.Checked := crRun in CompileReasons;
      lblRunIfExecAfter.Visible := True;
      chkExecAfterCompile.Visible := True;
      chkExecAfterBuild.Visible := True;
      chkExecAfterRun.Visible := True;
    end
  else
  begin
    lblRunIfExecAfter.Visible := False;
    chkExecAfterCompile.Visible := False;
    chkExecAfterBuild.Visible := False;
    chkExecAfterRun.Visible := False;
  end;
end;

procedure TCompilerCompilationOptionsFrame.WriteSettings(AOptions: TAbstractIDEOptions);

  function MakeCompileReasons(const ACompile, ABuild, ARun: TCheckBox): TCompileReasons;
  begin
    Result := [];
    if ACompile.Checked then Include(Result, crCompile);
    if ABuild.Checked then Include(Result, crBuild);
    if ARun.Checked then Include(Result, crRun);
  end;

var
  Options: TBaseCompilerOptions absolute AOptions;
begin
  Options.CreateMakefileOnBuild := chkCreateMakefile.Checked;

  Options.ExecuteBefore.Command := ExecuteBeforeCommandEdit.Text;
  Options.ExecuteBefore.ScanForFPCMessages := ExecuteBeforeScanFPCCheckBox.Checked;
  Options.ExecuteBefore.ScanForMakeMessages :=ExecuteBeforeScanMakeCheckBox.Checked;
  Options.ExecuteBefore.ShowAllMessages := ExecuteBeforeShowAllCheckBox.Checked;
  if Options.ExecuteBefore is TProjectCompilationToolOptions then
  begin
    TProjectCompilationToolOptions(Options.ExecuteBefore).CompileReasons :=
      MakeCompileReasons(chkExecBeforeCompile, chkExecBeforeBuild, chkExecBeforeRun);
  end;

  Options.CompilerPath := cobCompiler.Text;
  EnvironmentOptions.CompilerFileHistory.Assign(cobCompiler.Items);

  if Options is TProjectCompilerOptions then
  begin
    TProjectCompilerOptions(Options).CompileReasons :=
      MakeCompileReasons(chkCompilerCompile, chkCompilerBuild, chkCompilerRun);
  end
  else if Options is TPkgCompilerOptions then
    TPkgCompilerOptions(Options).SkipCompiler := chkCompilerCompile.Checked;

  Options.ExecuteAfter.Command := ExecuteAfterCommandEdit.Text;
  Options.ExecuteAfter.ScanForFPCMessages := ExecuteAfterScanFPCCheckBox.Checked;
  Options.ExecuteAfter.ScanForMakeMessages :=ExecuteAfterScanMakeCheckBox.Checked;
  Options.ExecuteAfter.ShowAllMessages := ExecuteAfterShowAllCheckBox.Checked;
  if Options.ExecuteAfter is TProjectCompilationToolOptions then
  begin
    TProjectCompilationToolOptions(Options.ExecuteAfter).CompileReasons :=
      MakeCompileReasons(chkExecAfterCompile, chkExecAfterBuild, chkExecAfterRun);
  end;
end;

class function TCompilerCompilationOptionsFrame.SupportedOptionsClass: TAbstractIDEOptionsClass;
begin
  Result := TBaseCompilerOptions;
end;

initialization
  RegisterIDEOptionsEditor(GroupCompiler, TCompilerCompilationOptionsFrame,
    CompilerOptionsCompilation);
  RegisterIDEOptionsEditor(GroupPkgCompiler, TCompilerCompilationOptionsFrame,
    CompilerOptionsCompilation);

end.

