{ To use this example, create a new application and add the example code
  to the unit. Remember to add the ComCtls unit in the uses clause. }

procedure AddButtons(ToolBar: TToolBar; const ButtonCaptions: array of String);
var
  i: integer;
begin
  for i := 0 to High(ButtonCaptions) do
  begin
    with TToolButton.Create(ToolBar) do
    begin
      Parent := ToolBar;
      Caption := ButtonCaptions[i];
      if (ButtonCaptions[i] = '|') then
        Style := tbsSeparator
      else
        Style := tbsButton;
      AutoSize := True;
    end;
  end;
end;


procedure TForm1.FormCreate(Sender: TObject);
var
  ToolBar: TToolBar;
begin
  ToolBar := TToolBar.Create(Self);
  ToolBar.Parent := Self;
  ShowMessage(IntToStr(ToolBar.ButtonCount));
  AddButtons(ToolBar, ['New', 'Save', '|', 'Cut', 'Copy', 'Paste']);
  ToolBar.ShowCaptions := True;
  ToolBar.Height := 40;
  ToolBar.ButtonWidth := 75;
  ShowMessage(IntToStr(ToolBar.ButtonCount));
end;
