unit compiler_compilation_options;

{$mode objfpc}{$H+}

interface

uses
  // RTL + FCL
  Classes, sysutils,
  // LCL
  Controls, StdCtrls, Dialogs, CheckLst,
  // CodeTools
  FileProcs, DefineTemplates, CodeToolManager, LinkScanner,
  // LazUtils
  FileUtil, LazFileUtils, LazUTF8,
  // IDEIntf
  IDEOptionsIntf, IDEOptEditorIntf, CompOptsIntf, IDEExternToolIntf,
  IDEDialogs, IDEUtils,
  // IDE
  Project, CompilerOptions, PackageDefs, LazarusIDEStrConsts, EnvironmentOpts,
  LazConf, IDEProcs, DialogProcs, InputHistory, InitialSetupProc;

type

  { TCompilerCompilationOptionsFrame }

  TCompilerCompilationOptionsFrame = class(TAbstractIDEOptionsEditor)
    BrowseCompilerButton: TButton;
    ExecAfterParsersCheckListBox: TCheckListBox;
    ExecAfterParsersLabel: TLabel;
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
    ExecBeforeParsersCheckListBox: TCheckListBox;
    ExecuteAfterCommandComboBox: TComboBox;
    ExecuteAfterCommandLabel: TLabel;
    ExecuteAfterGroupBox: TGroupBox;
    ExecuteBeforeCommandComboBox: TComboBox;
    ExecuteBeforeCommandLabel: TLabel;
    ExecuteBeforeGroupBox: TGroupBox;
    ExecBeforeParsersLabel: TLabel;
    grpCompiler: TGroupBox;
    lblCompiler: TLabel;
    lblRunIfCompiler: TLabel;
    lblRunIfExecAfter: TLabel;
    lblRunIfExecBefore: TLabel;
    procedure CompCmdBrowseButtonClick(Sender: TObject);
  private
    procedure ReadSettingsParsers(ToolOpts: TCompilationToolOptions;
      Cmb: TCheckListBox);
    procedure WriteSettingsParsers(ToolOpts: TCompilationToolOptions;
      Cmb: TCheckListBox);
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

  function ShowQuality(Quality: TSDFilenameQuality;
    const Filename, Note: string): boolean;
  begin
    if Quality<>sddqCompatible then begin
      if IDEMessageDialog(lisCCOWarningCaption, Format(
        lisTheCompilerFileDoesNotLookCorrect, [Filename, #13, Note]),
        mtWarning,[mbIgnore,mbCancel])<>mrIgnore
      then
        exit(false);
    end;
    Result:=true;
  end;

var
  OpenDialog: TOpenDialog;
  NewFilename: string;
  Quality: TSDFilenameQuality;
  Note: string;
  s: string;
  OldFilename: string;
  OldParams: string;
  Combo: TComboBox;
  ok: Boolean;
  Kind: TPascalCompiler;
begin
  OpenDialog:=IDEOpenDialogClass.Create(nil);
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
    // check, even if new file is old filename, so the user sees the warnings again
    ok:=false;
    if Sender=BrowseCompilerButton then begin
      // check compiler filename
      if Pos('pas2js',UTF8LowerCase(ExtractFileNameOnly(NewFilename)))>0 then begin
        // check pas2js
        Quality:=CheckPas2jsQuality(NewFilename,Note,
                                   CodeToolBoss.CompilerDefinesCache.TestFilename);
        if not ShowQuality(Quality,NewFilename,Note) then exit;
        ok:=true;
      end;
      if (not ok)
          and (SameText('ppc',LeftStr(ExtractFileNameOnly(NewFilename),3))
            or (Pos('fpc',UTF8LowerCase(ExtractFileNameOnly(NewFilename)))>0)) then begin
        // check fpc
        Quality:=CheckFPCExeQuality(NewFilename,Note,
                              CodeToolBoss.CompilerDefinesCache.TestFilename);
        if not ShowQuality(Quality,NewFilename,Note) then exit;
        ok:=true;
      end;
      if not ok then begin
        // maybe a wrapper script
        if IsCompilerExecutable(NewFilename,s,Kind,true) then begin
          if (s<>'') and not ShowQuality(sddqInvalid,NewFilename,s) then exit;
          ok:=true;
        end;
        // maybe a script
        if (not ok)
        and not CheckExecutable('',NewFilename,lisInvalidExecutable,lisInvalidExecutableMessageText)
        then
          exit;
        ok:=true;
      end;
    end else if (Sender=ExecBeforeBrowseButton)
    or (Sender=ExecAfterBrowseButton) then begin
      // check executable
      if not CheckExecutable('',NewFilename,lisInvalidExecutable,lisInvalidExecutableMessageText)
      then
        exit;
      ok:=true;
    end;
    SetComboBoxText(Combo,NewFilename,cstFilename);
  finally
    InputHistories.StoreFileDialogSettings(OpenDialog);
    OpenDialog.Free;
  end;
end;

procedure TCompilerCompilationOptionsFrame.ReadSettingsParsers(
  ToolOpts: TCompilationToolOptions; Cmb: TCheckListBox);
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
      j:=Cmb.Items.Add(s);
      Cmb.Checked[j] := ToolOpts.HasParser[ParserClass.GetParserName];
    end;
    // add not registered parsers
    // Note: this happens when opening a project, which needs a designtime-package
    for i:=0 to ToolOpts.Parsers.Count-1 do
    begin
      s:=ToolOpts.Parsers[i];
      if ExternalToolList.FindParserWithName(s)=nil then
      begin
        j:=Cmb.Items.Add(s);
        Cmb.Checked[j]:=True;
      end;
    end;
  finally
    l.Free;
  end;
end;

procedure TCompilerCompilationOptionsFrame.WriteSettingsParsers(
  ToolOpts: TCompilationToolOptions; Cmb: TCheckListBox);
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
  BrowseCompilerButton.Hint:=lisBrowseAndSelectACompiler+ExeExt+')';

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
  Syy: TCompileReasons;
  IsProj: Boolean;
begin
  IsProj := Options is TProjectCompilerOptions;
  chkCreateMakefile.Checked := Options.CreateMakefileOnBuild;

  // execute before
  with ExecuteBeforeCommandComboBox do begin
    Items.BeginUpdate;
    Items.Assign(InputHistories.HistoryLists.GetList('BuildExecBefore',true,rltFile));
    SetComboBoxText(ExecuteBeforeCommandComboBox,Options.ExecuteBefore.Command,cstCaseSensitive);
    Items.EndUpdate;
  end;
  Syy := Options.ExecuteBefore.CompileReasons;
  chkExecBeforeCompile.Checked := crCompile in Syy;
  chkExecBeforeBuild.Checked := crBuild in Syy;
  chkExecBeforeRun.Checked := crRun in Syy;
  lblRunIfExecBefore.Visible := IsProj;
  chkExecBeforeCompile.Visible := IsProj;
  chkExecBeforeBuild.Visible := IsProj;
  chkExecBeforeRun.Visible := IsProj;
  ReadSettingsParsers(Options.ExecuteBefore,ExecBeforeParsersCheckListBox);

  // compiler path
  with cobCompiler do begin
    Items.BeginUpdate;
    Items.Assign(EnvironmentOptions.CompilerFileHistory);
    AddFilenameToList(Items,DefaultCompilerPath);
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

  // execute after
  with ExecuteAfterCommandComboBox do begin
    Items.BeginUpdate;
    Items.Assign(InputHistories.HistoryLists.GetList('BuildExecAfter',true,rltFile));
    SetComboBoxText(ExecuteAfterCommandComboBox,Options.ExecuteAfter.Command,cstCaseSensitive);
    Items.EndUpdate;
  end;
  Syy := Options.ExecuteAfter.CompileReasons;
  chkExecAfterCompile.Checked := crCompile in Syy;
  chkExecAfterBuild.Checked := crBuild in Syy;
  chkExecAfterRun.Checked := crRun in Syy;
  lblRunIfExecAfter.Visible := IsProj;
  chkExecAfterCompile.Visible := IsProj;
  chkExecAfterBuild.Visible := IsProj;
  chkExecAfterRun.Visible := IsProj;
  ReadSettingsParsers(Options.ExecuteAfter,ExecAfterParsersCheckListBox);
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
  HL: THistoryList;
begin
  Options.CreateMakefileOnBuild := chkCreateMakefile.Checked;

  // execute before
  Options.ExecuteBefore.Command := ExecuteBeforeCommandComboBox.Text;
  HL := InputHistories.HistoryLists.GetList('BuildExecBefore',true,rltCaseSensitive);
  HL.Assign(ExecuteBeforeCommandComboBox.Items);
  HL.Push(Options.ExecuteBefore.Command);

  WriteSettingsParsers(Options.ExecuteBefore,ExecBeforeParsersCheckListBox);

  if Options.ExecuteBefore is TProjectCompilationToolOptions then
    Options.ExecuteBefore.CompileReasons :=
      MakeCompileReasons(chkExecBeforeCompile, chkExecBeforeBuild, chkExecBeforeRun);

  // compiler path
  Options.CompilerPath := cobCompiler.Text;
  EnvironmentOptions.CompilerFileHistory.Assign(cobCompiler.Items);
  AddToRecentList(Options.CompilerPath,EnvironmentOptions.CompilerFileHistory,30,rltFile);

  if Options is TProjectCompilerOptions then
  begin
    TProjectCompilerOptions(Options).CompileReasons :=
      MakeCompileReasons(chkCompilerCompile, chkCompilerBuild, chkCompilerRun);
  end
  else if Options is TPkgCompilerOptions then
    TPkgCompilerOptions(Options).SkipCompiler := chkCompilerCompile.Checked;

  // execute after
  Options.ExecuteAfter.Command := ExecuteAfterCommandComboBox.Text;
  HL := InputHistories.HistoryLists.GetList('BuildExecAfter',true,rltCaseSensitive);
  HL.Assign(ExecuteAfterCommandComboBox.Items);
  HL.Push(Options.ExecuteAfter.Command);
  WriteSettingsParsers(Options.ExecuteAfter,ExecAfterParsersCheckListBox);
  if Options.ExecuteAfter is TProjectCompilationToolOptions then
    Options.ExecuteAfter.CompileReasons :=
      MakeCompileReasons(chkExecAfterCompile, chkExecAfterBuild, chkExecAfterRun);
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

