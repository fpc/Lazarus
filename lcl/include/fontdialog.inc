{%MainUnit ../dialogs.pp}
{******************************************************************************
                                  TFontDialog
 ******************************************************************************

 *****************************************************************************
 *                                                                           *
 *  This file is part of the Lazarus Component Library (LCL)                 *
 *                                                                           *
 *  See the file COPYING.modifiedLGPL.txt, included in this distribution,    *
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

class procedure TFontDialog.WSRegisterClass;
begin
  inherited WSRegisterClass;
  RegisterFontDialog;
end;

function TFontDialog.DefaultTitle: string;
begin
  Result:=rsSelectFontTitle;
end;

