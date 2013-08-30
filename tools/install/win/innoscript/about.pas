
// Info displayed during install progress

Procedure AddInfoComponentsToProgressWizzard;
var
  m: TMemo;
begin
  WizardForm.ProgressGauge.Parent.Handle;
  m:= TMemo.Create(WizardForm);
  AddComponentToPage(m, WizardForm.ProgressGauge, 10, 0, 0, -15);
  m.ReadOnly  := True;
  m.WordWrap  := True;
  m.ScrollBars := ssVertical;

  m.Text := Format(CustomMessage('DuringInstall'), [#13#10]);
end;

