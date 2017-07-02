{%MainUnit ../forms.pp}

{******************************************************************************
                             TCustomDockForm
 ******************************************************************************

 *****************************************************************************
  This file is part of the Lazarus Component Library (LCL)

  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************
}

{ TCustomDockForm }

procedure TCustomDockForm.DoAddDockClient(Client: TControl; const ARect: TRect);
  procedure FixClientPlacing(Client: TControl);
  begin
    Client.BorderSpacing.Left:=0;
    Client.BorderSpacing.Right:=0;
    Client.BorderSpacing.Top:=0;
    Client.BorderSpacing.Bottom:=0;
    Client.BorderSpacing.Around:=0;
  end;
begin
  inherited DoAddDockClient(Client, ARect);
  Client.Align := alClient;
  FixClientPlacing(Client);
  if ([csLoading,csDestroying]*ComponentState=[]) then
    Visible := True;
end;

procedure TCustomDockForm.DoRemoveDockClient(Client: TControl);
begin
  inherited DoRemoveDockClient(Client);
  if DockClientCount = 0 then Release;
end;

procedure TCustomDockForm.GetSiteInfo(Client: TControl;
  var InfluenceRect: TRect; MousePos: TPoint; var CanDock: Boolean);
begin
  CanDock := (DockClientCount=0);
end;

procedure TCustomDockForm.Loaded;
var
  i: Integer;
begin
  // dock child controls after loading
  for i:=0 to ControlCount-1 do
    Controls[i].Dock(Self,ClientRect);
  inherited Loaded;
end;

constructor TCustomDockForm.Create(TheOwner: TComponent);
begin
  BeginFormUpdate;
  CreateNew(TheOwner,0);
  AutoScroll := False;
  BorderStyle := bsSizeToolWin;
  DockSite := True;
  FormStyle := fsStayOnTop;
  EndFormUpdate;
end;

// included by forms.pp
