unit compiler_verbosity_options;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs,
  ExtCtrls, StdCtrls, IDEOptionsIntf, Project, CompilerOptions,
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
    Items.Add(dlgHintsParameterSenderNotUsed + ' (none)');
    Items.Add(dlgShowWarnings + ' (-vw)');
    Items.Add(dlgShowDebugInfo + ' (-vd)');
    Items.Add(dlgShowNotes + ' (-vn)');
    Items.Add(dlgShowUsedFiles + ' (-vu)');
    Items.Add(dlgShowHint + ' (-vh)');
    Items.Add(dlgShowTriedFiles + ' (-vt)');
    Items.Add(dlgShowGeneralInfo + ' (-vi)');
    Items.Add(dlgShowDefinedMacros + ' (-vm)');
    Items.Add(dlgShowCompilingLineNumbers + ' (-vl)');
    Items.Add(dlgShowCompiledProcedures + ' (-vp)');
    Items.Add(dlgShowProcsError + ' (-vb)');
    Items.Add(dlgShowConditionals + ' (-vc)');
    Items.Add(dlgShowEverything + ' (-va)');
    Items.Add(dlgShowExecutableInfo + ' (-vx)');
    Items.Add(dlgShowSummary + ' (none)');
    Items.Add(dlgShowNothing + ' (-v0)');
    Items.Add(dlgHintsUnused + ' (none)');
    Items.Add(dlgWriteFPCLogo + ' (-l)');
  end;

  grpErrorCnt.Caption := dlgStopAfterNrErr + ' (-Se)';
  edtErrorCnt.Text := '';
end;

procedure TCompilerVerbosityOptionsFrame.ReadSettings(AOptions: TAbstractIDEOptions);
begin
  with AOptions as TBaseCompilerOptions, grpVerbosity do
  begin
    Checked[0] := ShowErrors;
    Checked[1] := ShowHintsForSenderNotUsed;
    Checked[2] := ShowWarn;
    Checked[3] := ShowDebugInfo;
    Checked[4] := ShowNotes;
    Checked[5] := ShowUsedFiles;
    Checked[6] := ShowHints;
    Checked[7] := ShowTriedFiles;
    Checked[8] := ShowGenInfo;
    Checked[9] := ShowDefMacros;
    Checked[10] := ShowLineNum;
    Checked[11] := ShowCompProc;
    Checked[12] := ShowAllProcsOnError;
    Checked[13] := ShowCond;
    Checked[14] := ShowAll;
    Checked[15] := ShowExecInfo;
    Checked[16] := ShowSummary;
    Checked[17] := ShowNothing;
    Checked[18] := ShowHintsForUnusedUnitsInMainSrc;
    Checked[19] := WriteFPCLogo;

    edtErrorCnt.Text := IntToStr(StopAfterErrCount);
  end;
end;

procedure TCompilerVerbosityOptionsFrame.WriteSettings(AOptions: TAbstractIDEOptions);
begin
  with AOptions as TBaseCompilerOptions, grpVerbosity do
  begin
    ShowErrors := Checked[0];
    ShowHintsForSenderNotUsed := Checked[1];
    ShowWarn := Checked[2];
    ShowDebugInfo := Checked[3];
    ShowNotes := Checked[4];
    ShowUsedFiles := Checked[5];
    ShowHints := Checked[6];
    ShowTriedFiles := Checked[7];
    ShowGenInfo := Checked[8];
    ShowDefMacros := Checked[9];
    ShowLineNum := Checked[10];
    ShowCompProc := Checked[11];
    ShowAllProcsOnError := Checked[12];
    ShowCond := Checked[13];
    ShowAll := Checked[14];
    ShowExecInfo := Checked[15];
    ShowSummary := Checked[16];
    ShowNothing := Checked[17];
    ShowHintsForUnusedUnitsInMainSrc := Checked[18];
    WriteFPCLogo := Checked[19];
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

