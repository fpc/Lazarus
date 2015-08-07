unit compiler_compilation_options;

{$mode objfpc}{$H+}

interface

uses
  // RTL + FCL + LCL
  Classes, sysutils,
  Controls, StdCtrls, Dialogs,
  // CodeTools
  FileProcs, DefineTemplates, CodeToolManager,
  // LazUtils
  FileUtil, LazFileUtils,
  // IDEIntf
  IDEOptionsIntf, CompOptsIntf, IDEDialogs, IDEUtils,
  // IDE
  Project, CompilerOptions, PackageDefs, LazarusIDEStrConsts, EnvironmentOpts,
  LazConf, IDEProcs, DialogProcs, InputHistory, InitialSetupProc;

type

  { TCompilerCompilationOptionsFrame }

  TCompilerCompilationOptionsFrame = class(TAbstractIDEOptionsEditor)
    BrowseCompilerButton: TButton;
    ExecBeforeBrowseButton: TButton;
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
    ExecAfterBrowseButton: TButton;
    ExecuteAfterCommandComboBox: TComboBox;
    ExecuteAfterCommandLabel: TLabel;
    ExecuteAfterGroupBox: TGroupBox;
    ExecuteAfterScanFPCCheckBox: TCheckBox;
    ExecuteAfterScanLabel: TLabel;
    ExecuteAfterScanMakeCheckBox: TCheckBox;
    ExecuteAfterShowAllCheckBox: TCheckBox;
    ExecuteBeforeCommandComboBox: TComboBox;
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
    procedure CompCmdBrowseButtonClick(Sender: TObject);
  private
  public
    function GetTitle: string; override;
    procedure Setup({%H-}ADialog: TAbstractOptionsEditorDialog); override;
    procedure ReadSettings(AOptions: TAbstractIDEOptions); override;
    procedure WriteSettings(AOptions: TAbstractIDEOptions); override;
    class function SupportedOptionsClass: TAbstractIDEOptionsClass; override;
  end;

implementation

{$R *.lfm}

{ TCompilerCompilationOptionsFrame }

procedure TCompilerCompilationOptionsFrame.CompCmdBrowseButtonClick(
  Sender: TObject);
var
  OpenDialog: TOpenDialog;
  NewFilename: string;
  Quality: TSDFilenameQuality;
  Note: string;
  s: string;
  OldFilename: string;
  OldParams: string;
  Combo: TComboBox;
begin
  OpenDialog:=TOpenDialog.Create(nil);
  try
    InputHistories.ApplyFileDialogSettings(OpenDialog);
    OpenDialog.Options:=OpenDialog.Options+[ofFileMustExist];
    OpenDialog.Filter:=dlgFilterAll+'|'+GetAllFilesMask;
    OldFilename:='';
    OldParams:='';
    // set title
    if Sender=BrowseCompilerButton then begin
      Combo:=cobCompiler;
      OpenDialog.Title:=Format(lisChooseCompilerExecutable,[GetDefaultCompilerFilename])
    end else if (Sender=ExecAfterBrowseButton) then begin
      Combo:=ExecuteAfterCommandComboBox;
      OpenDialog.Title:=lisChooseExecutable;
      SplitCmdLine(Combo.Text,OldFilename,OldParams);
    end else if (Sender=ExecBeforeBrowseButton) then begin
      Combo:=ExecuteBeforeCommandComboBox;
      OpenDialog.Title:=lisChooseExecutable;
      SplitCmdLine(Combo.Text,OldFilename,OldParams);
    end else
      exit;

    if not OpenDialog.Execute then exit;
    NewFilename:=TrimAndExpandFilename(OpenDialog.Filename);
    if Sender=BrowseCompilerButton then begin
      // check compiler filename
      if IsFPCExecutable(NewFilename,s) then begin
        // check compiler
        Quality:=CheckCompilerQuality(NewFilename,Note,
                                   CodeToolBoss.FPCDefinesCache.TestFilename);
        if Quality<>sddqCompatible then begin
          if IDEMessageDialog(lisCCOWarningCaption, Format(
            lisTheCompilerFileDoesNotLookCorrect, [NewFilename, #13, Note]),
            mtWarning,[mbIgnore,mbCancel])<>mrIgnore
          then
            exit;
        end;
      end else begin
        // maybe a script
        if not CheckExecutable(OldFilename,NewFilename,lisInvalidExecutable,lisInvalidExecutableMessageText)
        then
          exit;
      end;
    end else if (Sender=ExecBeforeBrowseButton)
    or (Sender=ExecAfterBrowseButton) then begin
      // check executable
      if not CheckExecutable(OldFilename,NewFilename,lisInvalidExecutable,lisInvalidExecutableMessageText)
      then
        exit;
    end;
    SetComboBoxText(Combo,NewFilename,cstFilename);
  finally
    InputHistories.StoreFileDialogSettings(OpenDialog);
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
  chkCreateMakefile.Hint := lisEnabledOnlyForPackages;

  ExecuteBeforeGroupBox.Caption := lisCOExecuteBefore;
  chkExecBeforeBuild.Caption := lisBuildStage;
  chkExecBeforeCompile.Caption := lisCompileStage;
  chkExecBeforeRun.Caption := lisRunStage;
  ExecuteBeforeCommandComboBox.Text := '';
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
  ExecuteAfterCommandComboBox.Text := '';
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

  ExecuteBeforeCommandComboBox.Text := Options.ExecuteBefore.Command;
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
    AddFilenameToList(Items,DefaultCompilerPath);
    SetComboBoxText(cobCompiler,Options.CompilerPath,cstFilename);
    Items.EndUpdate;
  end;

  ExecuteBeforeCommandComboBox.Items.Assign(
    InputHistories.HistoryLists.GetList('BuildExecBefore',true,rltFile));
  ExecuteAfterCommandComboBox.Items.Assign(
    InputHistories.HistoryLists.GetList('BuildExecAfter',true,rltFile));

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

  ExecuteAfterCommandComboBox.Text := Options.ExecuteAfter.Command;
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

  Options.ExecuteBefore.Command := ExecuteBeforeCommandComboBox.Text;
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

  InputHistories.HistoryLists.GetList('BuildExecBefore',true,rltFile).Assign(
    ExecuteBeforeCommandComboBox.Items);
  InputHistories.HistoryLists.GetList('BuildExecAfter',true,rltFile).Assign(
    ExecuteAfterCommandComboBox.Items);

  if Options is TProjectCompilerOptions then
  begin
    TProjectCompilerOptions(Options).CompileReasons :=
      MakeCompileReasons(chkCompilerCompile, chkCompilerBuild, chkCompilerRun);
  end
  else if Options is TPkgCompilerOptions then
    TPkgCompilerOptions(Options).SkipCompiler := chkCompilerCompile.Checked;

  Options.ExecuteAfter.Command := ExecuteAfterCommandComboBox.Text;
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

