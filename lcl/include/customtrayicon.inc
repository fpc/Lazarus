{%MainUnit ../extctrls.pp}

{******************************************************************************
                                TCustomTrayIcon
 ******************************************************************************

 *****************************************************************************
 *                                                                           *
 *  This file is part of the Lazarus Component Library (LCL)                 *
 *                                                                           *
 *  See the file COPYING.modifiedLGPL, included in this distribution,        *
 *  for details about the copyright.                                         *
 *                                                                           *
 *  This program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
 *                                                                           *
 *****************************************************************************
}
{

  Delphi compatibility:

  - TCustomTrayIcon is partially compatible with Delphi implementation
}

{*******************************************************************
*  TCustomTrayIcon.Create ()
*
*  DESCRIPTION:    Creates an object from the TCustomTrayIcon class
*
*  PARAMETERS:     TheOwner  - The owner of the component (this may be nil)
*
*  RETURNS:        A pointer to the newly created object
*
*******************************************************************}
constructor TCustomTrayIcon.Create(TheOwner : TComponent);
begin
  inherited Create(TheOwner);

  { Default property values }
  FBalloonTimeout := 3000;
  FShowIcon := True;

  FIcon := TIcon.Create;
  FIcon.OnChange := @IconChanged;

  FNotifier := TPopupNotifier.Create(Self);
  FNotifier.OnClose := @HandleNotifierClose;

  FTimer := TTimer.Create(Self);
  FTimer.Enabled := False;
  FTimer.OnTimer := @HandleNotifierTimeout;
end;

{*******************************************************************
*  TCustomTrayIcon.Destroy ()
*
*  DESCRIPTION:    Destroys an object derived from the TCustomTrayIcon class
*
*  PARAMETERS:     None
*
*  RETURNS:        Nothing
*
*******************************************************************}
destructor TCustomTrayIcon.Destroy;
begin
  { Avoids an unremoved icon on the tray }
  Hide;

  FTimer.Free;
  FNotifier.Free;
  FIcon.Free;

  inherited Destroy;
end;

{*******************************************************************
*  TCustomTrayIcon.Hide ()
*
*  DESCRIPTION:    Hides the Icon
*
*  PARAMETERS:     None
*
*  RETURNS:        If successfull
*
*******************************************************************}
function TCustomTrayIcon.Hide: Boolean;
begin
  if not FVisible then Exit;

  FVisible := False;

  Result := TWSCustomTrayIconClass(WidgetSetClass).Hide(Self);
end;

{*******************************************************************
*  TCustomTrayIcon.Show ()
*
*  DESCRIPTION:    Shows the Icon
*
*  PARAMETERS:     None
*
*  RETURNS:        If successfull
*
*******************************************************************}
function TCustomTrayIcon.Show: Boolean;
begin
  if FVisible then Exit;

  FVisible := True;

  Result := TWSCustomTrayIconClass(WidgetSetClass).Show(Self);
end;

{*******************************************************************
*  TCustomTrayIcon.SetVisible ()
*
*  DESCRIPTION:    Setter method of the Visible property
*
*******************************************************************}
procedure TCustomTrayIcon.SetVisible(Value: Boolean);
begin
  if Value then Show
  else Hide;
end;

{*******************************************************************
*  TCustomTrayIcon.HandleNotifierClose ()
*
*  DESCRIPTION:    Turns the timer off when the notifier closes
*                  to avoid keep it running unnecessarely
*
*******************************************************************}
procedure TCustomTrayIcon.HandleNotifierClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  FTimer.Enabled := False;
end;

{*******************************************************************
*  TCustomTrayIcon.HandleNotifierTimeout ()
*
*  DESCRIPTION:    Handler for the timer that verifies when the notifier
*                  should close.
*
*******************************************************************}
procedure TCustomTrayIcon.HandleNotifierTimeout(Sender: TObject);
begin
  FTimer.Enabled := False;
  
  FNotifier.Hide;
end;

procedure TCustomTrayIcon.IconChanged(Sender: TObject);
begin
  if Handle <> 0 then
    TWSCustomTrayIconClass(WidgetSetClass).InternalUpdate(Self);
end;

{*******************************************************************
*  TCustomTrayIcon.InternalUpdate ()
*
*  DESCRIPTION:    Makes modifications to the Icon while running
*                  i.e. without hiding it and showing again
*
*                  Currently only the following parameters use this:
*
*                  - Hint
*
*                  For event parameters (PopUpMenu, OnMouseMove, etc)
*                  this isn't necessary because they are handled
*                  througth callbacks that can always call the last value.
*
*******************************************************************}
procedure TCustomTrayIcon.InternalUpdate;
begin
  TWSCustomTrayIconClass(WidgetSetClass).InternalUpdate(Self);
end;

{*******************************************************************
*  TCustomTrayIcon.ShowBalloonHint ()
*
*  DESCRIPTION:    Shows a small message balloon near the tray icon
*
*  PARAMETERS:     None
*
*  RETURNS:        Nothing
*
*******************************************************************}
procedure TCustomTrayIcon.ShowBalloonHint;
var
  UsePopUpNotifier: Boolean;
  Pt: TPoint;
begin
  UsePopUpNotifier := TWSCustomTrayIconClass(WidgetSetClass).ShowBalloonHint(Self);
  
  if UsePopUpNotifier then
  begin
    Pt := Self.GetPosition;
    FNotifier.Text := Self.BalloonHint;
    FNotifier.Title := Self.BalloonTitle;
    FNotifier.ShowAtPos(Pt.x, Pt.y);
    
    FTimer.Interval := Self.BalloonTimeout;
    FTimer.Enabled := True;
  end;
end;

{*******************************************************************
*  TCustomTrayIcon.GetPosition ()
*
*  DESCRIPTION:    Returns the position of the tray icon on the display.
*                  This function is utilized to show message boxes near
*                  the icon
*
*  PARAMETERS:     None
*
*  RETURNS:        Nothing
*
*******************************************************************}
function TCustomTrayIcon.GetPosition: TPoint;
begin
  Result := TWSCustomTrayIconClass(WidgetSetClass).GetPosition(Self);
end;

{*******************************************************************
*  TCustomTrayIcon.GetCanvas ()
*
*  DESCRIPTION:    Getter method of the Canvas property
*
*  PARAMETERS:     None
*
*  RETURNS:        The canvas of the underlaying Widgetset component
*
*******************************************************************}
function TCustomTrayIcon.GetCanvas: TCanvas;
begin
  //Result := Icon.Canvas;
  Result := TWSCustomTrayIconClass(WidgetSetClass).GetCanvas(Self);
end;

procedure TCustomTrayIcon.SetHint(const AValue: string);
begin
  FHint := AValue;

  if FVisible then InternalUpdate;
end;

procedure TCustomTrayIcon.SetIcon(const AValue: TIcon);
begin
  FIcon.Assign(AValue);
end;

// included by extctrls.pp
