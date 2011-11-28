{$MainForm customdrawnwsforms.pp}

{ TCDWSCustomForm }

class procedure TCDWSCustomForm.BackendAddCDWinControlToForm(const AForm: TCustomForm; ACDWinControl: TCDWinControl);
var
  lWindowInfo: TCDNonNativeForm;
begin
  lWindowInfo := TCDNonNativeForm(AForm.Handle);
  if lWindowInfo.Children = nil then lWindowInfo.Children := TFPList.Create;
  lWindowInfo.Children.Add(ACDWinControl);
end;

class function TCDWSCustomForm.BackendGetCDWinControlList(const AForm: TCustomForm): TFPList;
var
  lWindowInfo: TCDNonNativeForm;
begin
  lWindowInfo := TCDNonNativeForm(AForm.Handle);
  if lWindowInfo.Children = nil then lWindowInfo.Children := TFPList.Create;
  Result := lWindowInfo.Children;
end;

{------------------------------------------------------------------------------
  Method: TCDWSCustomForm.CreateHandle
  Params:  None
  Returns: Nothing

  Creates a Windows CE Form, initializes it according to it´s properties and shows it
 ------------------------------------------------------------------------------}
class function TCDWSCustomForm.CreateHandle(const AWinControl: TWinControl;
  const AParams: TCreateParams): TLCLIntfHandle;
var
  lWindowInfo: TAndroidWindowInfo;
  AForm: TCustomForm absolute AWinControl;
begin
  {$ifdef VerboseCDWindow}
  DebugLn(Format(':>[TCDWSCustomForm.CreateHandle] AWinControl=%x Name=%s: %s',
    [PtrInt(AWinControl), AWinControl.Name, AWinControl.ClassName]));
  {$endif}

  Result := TLCLIntfhandle(AddNewForm(TCustomForm(AWinControl)));

  {$ifdef VerboseCDWindow}
  DebugLn(Format(':<[TCDWSCustomForm.CreateHandle] Result=%x',
    [Result]));
  {$endif}
end;

class procedure TCDWSCustomForm.DestroyHandle(const AWinControl: TWinControl);
begin

end;

class procedure TCDWSCustomForm.SetBorderIcons(const AForm: TCustomForm;
 const ABorderIcons: TBorderIcons);
begin
end;

class procedure TCDWSCustomForm.SetFormBorderStyle(const AForm: TCustomForm;
          const AFormBorderStyle: TFormBorderStyle);
begin
  RecreateWnd(AForm);
end;

class procedure TCDWSCustomForm.SetBounds(const AWinControl: TWinControl;
    const ALeft, ATop, AWidth, AHeight: Integer);
begin
  {$ifdef VerboseCDWindow}
  DebugLn(Format('[TCDWSCustomForm.SetBounds] AWinControl=%x ALeft=%d ATop=%d AWidth=%d AHeight=%d',
    [PtrInt(AWinControl), ALeft, ATop, AWidth, AHeight]));
  {$endif}
end;

class procedure TCDWSCustomForm.SetIcon(const AForm: TCustomForm; const Small, Big: HICON);
begin
end;

class procedure TCDWSCustomForm.SetShowInTaskbar(const AForm: TCustomForm;
  const AValue: TShowInTaskbar);
begin
end;

class procedure TCDWSCustomForm.ShowModal(const ACustomForm: TCustomForm);
begin
end;

class procedure TCDWSCustomForm.ShowHide(const AWinControl: TWinControl);
begin
  if AWinControl.Visible then
  begin
    {$ifdef VerboseCDWindow}
    DebugLn(Format('[TCDWSCustomForm.ShowHide] Visible=True AWinControl=%x Handle=%x',
      [PtrInt(AWinControl), PtrInt(AWinControl.Handle)]));
    {$endif}
    ShowForm(TCDNonNativeForm(AWinControl.Handle));
  end
  else
  begin
    {$ifdef VerboseCDWindow}
    DebugLn(Format('[TCDWSCustomForm.ShowHide] Visible=False AWinControl=%x', [PtrInt(AWinControl)]));
    {$endif}
    HideForm(TCDNonNativeForm(AWinControl.Handle));
  end;
end;

class function TCDWSCustomForm.GetText(const AWinControl: TWinControl; var AText: String): Boolean;
begin
  AText := '';
end;

class function TCDWSCustomForm.GetTextLen(const AWinControl: TWinControl; var ALength: Integer): Boolean;
var
  lText: string;
begin
  Result := GetText(AWinControl, lText);
  ALength := Length(lText);
end;

class procedure TCDWSCustomForm.SetText(const AWinControl: TWinControl; const AText: String);
begin
end;

class function TCDWSCustomForm.GetClientBounds(const AWincontrol: TWinControl; var ARect: TRect): Boolean;
begin
end;

class function TCDWSCustomForm.GetClientRect(const AWincontrol: TWinControl; var ARect: TRect): Boolean;
begin
end;


