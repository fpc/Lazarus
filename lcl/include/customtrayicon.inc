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

  FIcon := TIcon.Create;
  FNotifier := TPopupNotifier.Create(Self);

  FShowIcon := True;
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

//  InternalUpdate;

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

  InternalUpdate;

  Result := TWSCustomTrayIconClass(WidgetSetClass).Show(Self);
end;

{*******************************************************************
*  TCustomTrayIcon.SetVisible ()
*
*  DESCRIPTION:    Setter method of the Visible property
*
*  PARAMETERS:     None
*
*  RETURNS:        If successfull
*
*******************************************************************}
procedure TCustomTrayIcon.SetVisible(Value: Boolean);
begin
  if Value then Show
  else Hide;
end;

{*******************************************************************
*  TCustomTrayIcon.InternalUpdate ()
*
*  DESCRIPTION:    Makes modifications to the Icon while running
*                  i.e. without hiding it and showing again
*
*  PARAMETERS:     None
*
*  RETURNS:        Nothing
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

// included by extctrls.pp
