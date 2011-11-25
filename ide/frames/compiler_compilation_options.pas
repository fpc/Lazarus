unit compiler_compilation_options;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs,
  StdCtrls, IDEOptionsIntf, Project, CompilerOptions, LazarusIDEStrConsts,
  ProjectIntf, CompOptsIntf, PackageDefs;

type

  { TCompilerCompilationOptionsFrame }

  TCompilerCompilationOptionsFrame = class(TAbstractIDEOptionsEditor)
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
    edtCompiler: TEdit;
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
  private
    fLoaded: Boolean;
    FSaved: Boolean;
    { private declarations }
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

function TCompilerCompilationOptionsFrame.GetTitle: string;
begin
  Result := dlgCOCompilation;
end;

procedure TCompilerCompilationOptionsFrame.Setup(
  ADialog: TAbstractOptionsEditorDialog);
begin
  chkCreateMakefile.Caption := dlgCOCreateMakefile;

  ExecuteBeforeGroupBox.Caption := lisCOExecuteBefore;
  chkExecBeforeBuild.Caption := lisCOCallOnBuild;
  chkExecBeforeCompile.Caption := lisCOCallOnCompile;
  chkExecBeforeRun.Caption := lisCOCallOnRun;
  ExecuteBeforeCommandEdit.Text := '';
  ExecuteBeforeCommandLabel.Caption := lisCOCommand;
  ExecuteBeforeScanLabel.Caption := lisCOScanForMessages;
  ExecuteBeforeScanFPCCheckBox.Caption := 'FPC'; // do not translate name
  ExecuteBeforeScanMakeCheckBox.Caption := 'make'; // do not translate name
  ExecuteBeforeShowAllCheckBox.Caption := lisA2PShowAll;
  lblRunIfExecBefore.Caption := lisCOCallOn;

  grpCompiler.Caption := lisCompiler;
  chkCompilerBuild.Caption := lisCOCallOnBuild;
  chkCompilerBuild.Checked := True;
  chkCompilerCompile.Caption := lisCOCallOnCompile;
  chkCompilerCompile.Checked := True;
  chkCompilerRun.Caption := lisCOCallOnRun;
  chkCompilerRun.Checked := True;
  edtCompiler.Text := '';
  lblCompiler.Caption := lisCOCommand;
  lblRunIfCompiler.Caption := lisCOCallOn;

  ExecuteAfterGroupBox.Caption := lisCOExecuteAfter;
  chkExecAfterBuild.Caption := lisCOCallOnBuild;
  chkExecAfterCompile.Caption := lisCOCallOnCompile;
  chkExecAfterRun.Caption := lisCOCallOnRun;
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
  if fLoaded then exit;
  fLoaded:=true;
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

  edtCompiler.Text := Options.CompilerPath;
  if Options is TProjectCompilerOptions then
    with TProjectCompilerOptions(Options) do
    begin
      chkCreateMakefile.Enabled:=false;
      lblRunIfCompiler.Visible := True;
      chkCompilerCompile.AnchorToNeighbour(akLeft, 30, lblRunIfCompiler);
      chkCompilerCompile.Checked := crCompile in CompileReasons;
      chkCompilerBuild.Checked := crBuild in CompileReasons;
      chkCompilerRun.Checked := crRun in CompileReasons;
      chkCompilerCompile.Caption := lisCOCallOnCompile;
      chkCompilerCompile.Visible := True;
      chkCompilerBuild.Visible := True;
      chkCompilerRun.Visible := True;
      edtCompiler.AnchorToNeighbour(akTop, 0, chkCompilerCompile);
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
    edtCompiler.AnchorToNeighbour(akTop, 0, chkCompilerCompile);
  end
  else
  begin
    lblRunIfCompiler.Visible := False;
    chkCompilerCompile.Visible := False;
    chkCompilerBuild.Visible := False;
    chkCompilerRun.Visible := False;
    edtCompiler.AnchorParallel(akTop, 0, lblCompiler.Parent);
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

procedure TCompilerCompilationOptionsFrame.WriteSettings(
  AOptions: TAbstractIDEOptions);

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
  if FSaved then exit;
  FSaved:=true;
  Options.CreateMakefileOnBuild := chkCreateMakefile.Checked;

  Options.ExecuteBefore.Command := ExecuteBeforeCommandEdit.Text;
  Options.ExecuteBefore.ScanForFPCMessages :=
    ExecuteBeforeScanFPCCheckBox.Checked;
  Options.ExecuteBefore.ScanForMakeMessages :=
    ExecuteBeforeScanMakeCheckBox.Checked;
  Options.ExecuteBefore.ShowAllMessages := ExecuteBeforeShowAllCheckBox.Checked;
  if Options.ExecuteBefore is TProjectCompilationToolOptions then
  begin
    TProjectCompilationToolOptions(Options.ExecuteBefore).CompileReasons :=
      MakeCompileReasons(chkExecBeforeCompile,
      chkExecBeforeBuild, chkExecBeforeRun);
  end;

  Options.CompilerPath := edtCompiler.Text;
  if Options is TProjectCompilerOptions then
  begin
    TProjectCompilerOptions(Options).CompileReasons :=
      MakeCompileReasons(chkCompilerCompile, chkCompilerBuild,
      chkCompilerRun);
  end
  else if Options is TPkgCompilerOptions then
  begin
    TPkgCompilerOptions(Options).SkipCompiler := chkCompilerCompile.Checked;
  end;

  Options.ExecuteAfter.Command := ExecuteAfterCommandEdit.Text;
  Options.ExecuteAfter.ScanForFPCMessages :=
    ExecuteAfterScanFPCCheckBox.Checked;
  Options.ExecuteAfter.ScanForMakeMessages :=
    ExecuteAfterScanMakeCheckBox.Checked;
  Options.ExecuteAfter.ShowAllMessages := ExecuteAfterShowAllCheckBox.Checked;
  if Options.ExecuteAfter is TProjectCompilationToolOptions then
  begin
    TProjectCompilationToolOptions(Options.ExecuteAfter).CompileReasons :=
      MakeCompileReasons(chkExecAfterCompile,
      chkExecAfterBuild, chkExecAfterRun);
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

