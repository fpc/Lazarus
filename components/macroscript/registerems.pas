unit RegisterEMS;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Dialogs, IDEOptionsIntf,
  {$IFDEF PasScriptNotAvail}
  SrcEditorIntf, EMSStrings,
  {$ENDIF}
  EMScriptMacro, EMSSelfTest, EMSIdeOptions;

procedure Register;

implementation

procedure Register;
var
  OptionsGroup: Integer;
  {$IFDEF PasScriptNotAvail}
  conf: TEMSConfig;
  ok: Boolean;
  {$ENDIF}
begin
  OptionsGroup := GetFreeIDEOptionsGroupIndex(GroupEditor);
  RegisterIDEOptionsGroup(OptionsGroup, TEMSConfig);
  RegisterIDEOptionsEditor(OptionsGroup, TEMSIdeOptionsFrame, 1);

  {$IFDEF PasScriptNotAvail}
  conf := GetEMSConf;
  try
    conf.Load;
  except
    try
      conf.SelfTestFailed := EMSVersion;
      conf.SelfTestActive := False;
      conf.SelfTestError := 'load error';
      conf.Save;
    except
    end;
    MessageDlg(EmsSelfTestErrCaption,
               format(EmsSelfTestFailedLastTime, [LineEnding]),
               mtError, [mbOK], 0);
    exit;
  end;

  if conf.SelfTestActive then begin
    conf.SelfTestFailed := EMSVersion;
    conf.SelfTestActive := False;
    conf.SelfTestError := 'failed last time';
    conf.Save;
    MessageDlg(EmsSelfTestErrCaption,
               format(EmsSelfTestFailedLastTime, [LineEnding]),
               mtError, [mbOK], 0);
  end;
  if conf.SelfTestFailed >= EMSVersion then begin
    exit;
  end;

  conf.SelfTestActive := True;
  conf.Save;

  ok := False;
  try
    ok := DoSelfTest;
  except
  end;

  if not ok then begin
    conf.SelfTestFailed := EMSVersion;
    conf.SelfTestActive := False;
    conf.SelfTestError := SelfTestErrorMsg;
    conf.Save;
    MessageDlg(EmsSelfTestErrCaption,
               format(EmsSelfTestFailed, [LineEnding, SelfTestErrorMsg]),
               mtError, [mbOK], 0);
    exit;
  end;

  conf.SelfTestActive := False;
  conf.SelfTestError := '';
  conf.Save;

  EditorMacroPlayerClass := TEMSEditorMacro;
  {$ENDIF}
end;

end.

