unit compiler_verbosity_options;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, ExtCtrls, StdCtrls, IDEOptionsIntf, CompilerOptions,
  LazarusIDEStrConsts;

type

  { TCompilerVerbosityOptionsFrame }

  TCompilerVerbosityOptionsFrame = class(TAbstractIDEOptionsEditor)
    edtErrorCnt: TEdit;
    grpErrorCnt: TGroupBox;
    grpVerbosity: TCheckGroup;
  public
    function GetTitle: string; override;
    procedure Setup(ADialog: TAbstractOptionsEditorDialog); override;
    procedure ReadSettings(AOptions: TAbstractIDEOptions); override;
    procedure WriteSettings(AOptions: TAbstractIDEOptions); override;
    class function SupportedOptionsClass: TAbstractIDEOptionsClass; override;
  end;

implementation

{$R *.lfm}

{ TCompilerVerbosityOptionsFrame }

function TCompilerVerbosityOptionsFrame.GetTitle: string;
begin
  Result := dlgCOVerbosity;
end;

procedure TCompilerVerbosityOptionsFrame.Setup(ADialog: TAbstractOptionsEditorDialog);
begin
  with grpVerbosity do
  begin
    Caption := dlgVerbosity;
    AutoSize := True;

    Items.Add(dlgCOShowErr + ' (-ve)');
    Items.Add(dlgHintsParameterSenderNotUsed + ' ' + dlgPOIconDescNone);
    Items.Add(dlgShowWarnings + ' (-vw)');
    Items.Add(dlgShowDebugInfo + ' (-vd)');
    Items.Add(dlgShowNotes + ' (-vn)');
    Items.Add(dlgShowUsedFiles + ' (-vu)');
    Items.Add(dlgShowHint + ' (-vh)');
    Items.Add(dlgShowTriedFiles + ' (-vt)');
    Items.Add(dlgShowGeneralInfo + ' (-vi)');
    Items.Add(dlgShowCompilingLineNumbers + ' (-vl)');
    Items.Add(dlgShowCompiledProcedures + ' (-vp)');
    Items.Add(dlgShowFullFileNames + ' (-vb)');
    Items.Add(dlgShowConditionals + ' (-vc)');
    Items.Add(dlgShowEverything + ' (-va)');
    Items.Add(dlgShowExecutableInfo + ' (-vx)');
    Items.Add(dlgShowSummary + ' ' + dlgPOIconDescNone);
    Items.Add(dlgShowNothing + ' (-v0)');
    Items.Add(dlgHintsUnused + ' ' + dlgPOIconDescNone);
    Items.Add(dlgWriteFPCLogo + ' (-l)');
  end;

  grpErrorCnt.Caption := dlgStopAfterNrErr + ' (-Se)';
  edtErrorCnt.Text := '';
end;

procedure TCompilerVerbosityOptionsFrame.ReadSettings(AOptions: TAbstractIDEOptions);
var
  i: Integer;
begin
  i:=0;
  with AOptions as TBaseCompilerOptions, grpVerbosity do
  begin
    Checked[i] := ShowErrors;  i+=1;
    Checked[i] := ShowHintsForSenderNotUsed; i+=1;
    Checked[i] := ShowWarn; i+=1;
    Checked[i] := ShowDebugInfo; i+=1;
    Checked[i] := ShowNotes; i+=1;
    Checked[i] := ShowUsedFiles; i+=1;
    Checked[i] := ShowHints; i+=1;
    Checked[i] := ShowTriedFiles; i+=1;
    Checked[i] := ShowGenInfo; i+=1;
    Checked[i] := ShowLineNum; i+=1;
    Checked[i] := ShowCompProc; i+=1;
    Checked[i] := ShowAllProcsOnError; i+=1;
    Checked[i] := ShowCond; i+=1;
    Checked[i] := ShowAll; i+=1;
    Checked[i] := ShowExecInfo; i+=1;
    Checked[i] := ShowSummary; i+=1;
    Checked[i] := ShowNothing; i+=1;
    Checked[i] := ShowHintsForUnusedUnitsInMainSrc; i+=1;
    Checked[i] := WriteFPCLogo; i+=1;

    edtErrorCnt.Text := IntToStr(StopAfterErrCount);
  end;
end;

procedure TCompilerVerbosityOptionsFrame.WriteSettings(AOptions: TAbstractIDEOptions);
var
  i: Integer;
begin
  i:=0;
  with AOptions as TBaseCompilerOptions, grpVerbosity do
  begin
    ShowErrors := Checked[i]; i+=1;
    ShowHintsForSenderNotUsed := Checked[i]; i+=1;
    ShowWarn := Checked[i]; i+=1;
    ShowDebugInfo := Checked[i]; i+=1;
    ShowNotes := Checked[i]; i+=1;
    ShowUsedFiles := Checked[i]; i+=1;
    ShowHints := Checked[i]; i+=1;
    ShowTriedFiles := Checked[i]; i+=1;
    ShowGenInfo := Checked[i]; i+=1;
    ShowLineNum := Checked[i]; i+=1;
    ShowCompProc := Checked[i]; i+=1;
    ShowAllProcsOnError := Checked[i]; i+=1;
    ShowCond := Checked[i]; i+=1;
    ShowAll := Checked[i]; i+=1;
    ShowExecInfo := Checked[i]; i+=1;
    ShowSummary := Checked[i]; i+=1;
    ShowNothing := Checked[i]; i+=1;
    ShowHintsForUnusedUnitsInMainSrc := Checked[i]; i+=1;
    WriteFPCLogo := Checked[i]; i+=1;
    StopAfterErrCount := StrToIntDef(edtErrorCnt.Text, 1);
  end;
end;

class function TCompilerVerbosityOptionsFrame.SupportedOptionsClass: TAbstractIDEOptionsClass;
begin
  Result := TBaseCompilerOptions;
end;

initialization
  RegisterIDEOptionsEditor(GroupCompiler, TCompilerVerbosityOptionsFrame,
    CompilerOptionsVerbosity);
  RegisterIDEOptionsEditor(GroupPkgCompiler, TCompilerVerbosityOptionsFrame,
    CompilerOptionsVerbosity);

end.

