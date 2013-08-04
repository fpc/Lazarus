unit RegisterEMS;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, SrcEditorIntf, IDEOptionsIntf, EMScriptMacro, EMSSelfTest,
  EMSIdeOptions, EMSStrings, Dialogs;

procedure Register;

implementation

procedure Register;
var
  conf: TEMSConfig;
  ok: Boolean;
  OptionsGroup: Integer;
begin
  OptionsGroup := GetFreeIDEOptionsGroupIndex(GroupEditor);
  RegisterIDEOptionsGroup(OptionsGroup, TEMSConfig);
  RegisterIDEOptionsEditor(OptionsGroup, TEMSIdeOptionsFrame, 1);


  conf := GetEMSConf;
  conf.Load;
  if conf.SelfTestActive then begin
    conf.SelfTestFailed := EMSVersion;
    conf.SelfTestActive := False;
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
    conf.Save;
    MessageDlg(EmsSelfTestErrCaption,
               format(EmsSelfTestFailed, [LineEnding, SelfTestErrorMsg]),
               mtError, [mbOK], 0);
    exit;
  end;

  conf.SelfTestActive := False;
  conf.Save;

  EditorMacroPlayerClass := TEMSEditorMacro;
end;

end.

