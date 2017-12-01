unit compiler_compilation_options;

{$mode objfpc}{$H+}

interface

uses
  // RTL + FCL + LCL
  Classes, sysutils,
  Controls, StdCtrls, Dialogs, ComboEx,
  // CodeTools
  FileProcs, DefineTemplates, CodeToolManager,
  // LazUtils
  FileUtil, LazFileUtils,
  // IDEIntf
  IDEOptionsIntf, CompOptsIntf, IDEDialogs, IDEUtils, IDEExternToolIntf,
  // IDE
  Project, CompilerOptions, PackageDefs, LazarusIDEStrConsts, EnvironmentOpts,
  LazConf, IDEProcs, DialogProcs, InputHistory, InitialSetupProc;

type

  { TCompilerCompilationOptionsFrame }

  TCompilerCompilationOptionsFrame = class(TAbstractIDEOptionsEditor)
    BrowseCompilerButton: TButton;
    ExecAfterParsersCheckComboBox: TCheckComboBox;
    ExecAfterParsersLabel: TLabel;
    ExecAfterParsersSumLabel: TLabel;
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
    ExecBeforeParsersSumLabel: TLabel;
    ExecuteAfterCommandComboBox: TComboBox;
    ExecuteAfterCommandLabel: TLabel;
    ExecuteAfterGroupBox: TGroupBox;
    ExecuteBeforeCommandComboBox: TComboBox;
    ExecuteBeforeCommandLabel: TLabel;
    ExecuteBeforeGroupBox: TGroupBox;
    ExecBeforeParsersCheckComboBox: TCheckComboBox;
    ExecBeforeParsersLabel: TLabel;
    grpCompiler: TGroupBox;
    lblCompiler: TLabel;
    lblRunIfCompiler: TLabel;
    lblRunIfExecAfter: TLabel;
    lblRunIfExecBefore: TLabel;
    procedure CompCmdBrowseButtonClick(Sender: TObject);
    procedure ExecAfterParsersCheckComboBoxItemChange(Sender: TObject;
      {%H-}AIndex: Integer);
    procedure ExecBeforeParsersCheckComboBoxItemChange(Sender: TObject;
      {%H-}AIndex: Integer);
  private
    procedure ReadSettingsParsers(ToolOpts: TCompilationToolOptions;
      Cmb: TCheckComboBox; Lbl: TLabel);
    procedure WriteSettingsParsers(ToolOpts: TCompilationToolOptions;
      Cmb: TCheckComboBox);
    procedure UpdateParsersLabel(Lbl: TLabel; Cmb: TCheckComboBox);
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

procedure TCompilerCompilationOptionsFrame.
  ExecAfterParsersCheckComboBoxItemChange(Sender: TObject; AIndex: Integer);
begin
  UpdateParsersLabel(ExecAfterParsersSumLabel,ExecAfterParsersCheckComboBox);
end;

procedure TCompilerCompilationOptionsFrame.
  ExecBeforeParsersCheckComboBoxItemChange(Sender: TObject; AIndex: Integer);
begin
  UpdateParsersLabel(ExecBeforeParsersSumLabel,ExecBeforeParsersCheckComboBox);
end;

procedure TCompilerCompilationOptionsFrame.ReadSettingsParsers(
  ToolOpts: TCompilationToolOptions; Cmb: TCheckComboBox; Lbl: TLabel);
const
  BoolToChecked: array[boolean] of TCheckBoxState = (cbUnchecked,cbChecked);
var
  l: TFPList;
  i, j: Integer;
  ParserClass: TExtToolParserClass;
  s: String;
begin
  l:=TFPList.Create;
  try
    // add registered parsers
    for i:=0 to ExternalToolList.ParserCount-1 do
    begin
      ParserClass:=ExternalToolList.Parsers[i];
      j:=0;
      while (j<l.Count)
      and (TExtToolParserClass(l[j]).Priority>=ParserClass.Priority) do
        inc(j);
      l.Insert(j,ParserClass);
    end;
    Cmb.Clear;
    for i:=0 to l.Count-1 do
    begin
      ParserClass:=TExtToolParserClass(l[i]);
      s:=ParserClass.GetLocalizedParserName;
      Cmb.AddItem(s,BoolToChecked[ToolOpts.HasParser[ParserClass.GetParserName]]);
    end;
    // add not registered parsers
    // Note: this happens when opening a project, which needs a designtime-package
    for i:=0 to ToolOpts.Parsers.Count-1 do
    begin
      s:=ToolOpts.Parsers[i];
      if ExternalToolList.FindParserWithName(s)=nil then
        Cmb.AddItem(s,cbChecked);
    end;
    UpdateParsersLabel(Lbl,Cmb);
  finally
    l.Free;
  end;
end;

procedure TCompilerCompilationOptionsFrame.WriteSettingsParsers(
  ToolOpts: TCompilationToolOptions; Cmb: TCheckComboBox);
var
  sl: TStringList;
  i, j: Integer;
begin
  sl:=TStringList.Create;
  try
    for i:=0 to Cmb.Items.Count-1 do
      if Cmb.Checked[i] then begin
        j:=ExternalToolList.ParserCount-1;
        while (j>=0)
        and (Cmb.Items[i]<>ExternalToolList.Parsers[i].GetLocalizedParserName) do
          dec(j);
        if j>=0 then
          sl.Add(ExternalToolList.Parsers[i].GetParserName)
        else
          sl.Add(Cmb.Items[i]); // not registered parser
      end;
    ToolOpts.Parsers:=sl;
  finally
    sl.Free;
  end;
end;

procedure TCompilerCompilationOptionsFrame.UpdateParsersLabel(Lbl: TLabel;
  Cmb: TCheckComboBox);
var
  s: String;
  i: Integer;
begin
  s:='';
  for i:=0 to Cmb.Items.Count-1 do
  begin
    if Cmb.Checked[i] then
    begin
      if s<>'' then s:=s+', ';
      s:=s+Cmb.Items[i];
    end;
  end;
  Lbl.Caption:=s;
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
  lblRunIfExecBefore.Caption := lisCOCallOn;
  chkExecBeforeBuild.Caption := lisBuildStage;
  chkExecBeforeCompile.Caption := lisCompileStage;
  chkExecBeforeRun.Caption := lisRunStage;
  ExecuteBeforeCommandComboBox.Text := '';
  ExecuteBeforeCommandLabel.Caption := lisCOCommand;
  ExecBeforeParsersLabel.Caption:=lisParsers;

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
  ExecAfterParsersLabel.Caption:=lisParsers;
  lblRunIfExecAfter.Caption := lisCOCallOn;
end;

procedure TCompilerCompilationOptionsFrame.ReadSettings(AOptions: TAbstractIDEOptions);
var
  Options: TBaseCompilerOptions absolute AOptions;
begin
  chkCreateMakefile.Checked := Options.CreateMakefileOnBuild;

  ExecuteBeforeCommandComboBox.Text := Options.ExecuteBefore.Command;

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
  ReadSettingsParsers(Options.ExecuteBefore,ExecBeforeParsersCheckComboBox,
    ExecBeforeParsersSumLabel);

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
  ReadSettingsParsers(Options.ExecuteAfter,ExecAfterParsersCheckComboBox,
    ExecAfterParsersSumLabel);
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

  WriteSettingsParsers(Options.ExecuteBefore,ExecBeforeParsersCheckComboBox);

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
  WriteSettingsParsers(Options.ExecuteAfter,ExecAfterParsersCheckComboBox);
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

