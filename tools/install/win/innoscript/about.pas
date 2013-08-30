
// Info displayed during install progress

Procedure AddInfoComponentsToProgressWizzard;
var
  m: TMemo;
begin
  WizardForm.ProgressGauge.Parent.Handle;
  m:= TMemo.Create(WizardForm);
  AddComponentToPage(m, WizardForm.ProgressGauge, 10, 0, 0, -15);
  //m.Parent:=WizardForm.ProgressGauge.Parent;
  //m.Top := WizardForm.ProgressGauge.Top + WizardForm.ProgressGauge.Height + 10;
  //m.Left := WizardForm.ProgressGauge.Left;
  //m.Width := WizardForm.ProgressGauge.Width ;
  //m.Height := WizardForm.ProgressGauge.Parent.Height - WizardForm.ProgressGauge.Height - WizardForm.ProgressGauge.Top - 15;
  m.ReadOnly  := True;
  m.WordWrap  := True;
  m.ScrollBars := ssVertical;

  m.Text := Format(CustomMessage('DuringInstall'), [#13#10]);
end;

