{******************************************************************************
                                  TFontDialog
 ******************************************************************************

 *****************************************************************************
 *                                                                           *
 *  This file is part of the Lazarus Component Library (LCL)                 *
 *                                                                           *
 *  See the file COPYING.LCL, included in this distribution,                 *
 *  for details about the copyright.                                         *
 *                                                                           *
 *  This program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
 *                                                                           *
 *****************************************************************************
}

{------------------------------------------------------------------------------
  Method: TFontDialog.Apply
  Params:  Wnd: HWND
  Returns: Nothing

  Called whenever the Apply button is clicked.
 ------------------------------------------------------------------------------}
procedure TFontDialog.ApplyClicked;
begin
  if Assigned(FOnApplyClicked) then FOnApplyClicked(Self);
end;

{------------------------------------------------------------------------------
  Method: TFontDialog.Create
  Params:  AOwner: the owner of the class
  Returns: Nothing

  Constructor for the class.
 ------------------------------------------------------------------------------}
constructor TFontDialog.Create (AOwner : TComponent);
begin
  inherited Create(AOwner);
  fCompStyle := csFontDialog;
  FTitle:= 'Select a font:';
  FFont := TFont.Create;
  FOptions := [fdEffects];
end;

destructor TFontDialog.Destroy;
begin
  FFont.Free;
  inherited Destroy;
end;

procedure TFontDialog.SetFont(const AValue: TFont);
begin
  FFont.Assign(AValue);
end;


{ =============================================================================

  $Log$
  Revision 1.4  2002/10/25 10:06:34  lazarus
  MG: broke interfacebase uses circles

  Revision 1.3  2002/06/04 15:17:22  lazarus
  MG: improved TFont for XLFD font names

  Revision 1.2  2002/05/10 06:05:52  lazarus
  MG: changed license to LGPL

  Revision 1.1  2000/07/13 10:28:25  michael
  + Initial import

  Revision 1.1  2000/04/02 20:49:56  lazarus
  MWE:
    Moved lazarus/lcl/*.inc files to lazarus/lcl/include

  Revision 1.4  2000/02/22 22:19:49  lazarus
  TCustomDialog is a descendant of TComponent.
  Initial cuts a form's proper Close behaviour.

  Revision 1.3  2000/02/22 17:32:49  lazarus
  Modified the ShowModal call.
  For TCustomForm is simply sets the visible to true now and adds fsModal to FFormState.  In gtkObject.inc FFormState is checked.  If it contains fsModal then either gtk_grab_add or gtk_grab_remove is called depending on the value of VISIBLE.

  The same goes for TCustomDialog (open, save, font, color).
  I moved the Execute out of the individual dialogs and moved it into TCustomDialog and made it virtual because FONT needs to set some stuff before calling the inherited execute.
  Shane

  Revision 1.2  1999/12/10 00:47:01  lazarus
  MWE:
    Fixed some samples
    Fixed Dialog parent is no longer needed
    Fixed (Win)Control Destruction
    Fixed MenuClick


}
