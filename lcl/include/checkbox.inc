// included by stdctrls.pp
{
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
{------------------------------------------------------------------------------}

constructor TCheckbox.Create(AOwner : TComponent);
begin
  inherited Create(AOwner);
  fCompStyle := csCheckbox;
  AutoSize := True;
end;

{------------------------------------------------------------------------------
  Method: TCheckbox.SetText
  Params:  Value : TCaption
  Returns: nothing

  Change the caption, and then AutoSize
 ------------------------------------------------------------------------------}
procedure TCheckbox.SetText(const Value: TCaption);
begin
  Inherited SetText(Value);
  DoAutoSize;
end;

{------------------------------------------------------------------------------
  Method: TCheckbox.DoAutoSize
  Params:  Value : Boolean
  Returns: nothing

  Attempts to Size to fit Caption if AutoSize is True
 ------------------------------------------------------------------------------}
procedure TCheckbox.DoAutoSize;
var
  R : TRect;
  DC : hDC;
begin
  If AutoSizing or not AutoSize then
    Exit;
  if (not HandleAllocated) or (csLoading in ComponentState) then exit;
  AutoSizing := True;
  DC := GetDC(Handle);
  Try
    R := Rect(0,0, Width, Height);
    DrawText(DC, PChar(Caption), Length(Caption), R,
      DT_CalcRect or DT_NOPrefix);
    If R.Right > Width then
      Width := R.Right + 25;
    If R.Bottom > Height then
      Height := R.Bottom + 2;
  Finally
    ReleaseDC(Handle, DC);
    AutoSizing := False;
  end;
end;

// included by stdctrls.pp

{
  $Log$
  Revision 1.7  2002/09/10 07:33:37  lazarus
  MG: fixed TCheckBox.DoAutoSize on loading

  Revision 1.6  2002/09/03 08:07:19  lazarus
  MG: image support, TScrollBox, and many other things from Andrew

  Revision 1.5  2002/08/24 06:51:22  lazarus
  MG: from Andrew: style list fixes, autosize for radio/checkbtns

  Revision 1.4  2002/07/23 07:40:51  lazarus
  MG: fixed get widget position for inherited gdkwindows

  Revision 1.3  2002/05/10 06:05:51  lazarus
  MG: changed license to LGPL

  Revision 1.2  2002/04/18 08:09:03  lazarus
  MG: added include comments

  Revision 1.1  2000/07/13 10:28:24  michael
  + Initial import

  Revision 1.1  2000/04/02 20:49:55  lazarus
  MWE:
    Moved lazarus/lcl/*.inc files to lazarus/lcl/include

  Revision 1.3  1999/12/30 19:04:13  lazarus
   - Made TRadiobutton work again
   - Some more cleanups to checkbox code
           stoppok

}

