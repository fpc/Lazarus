// included by stdctrls.pp

{******************************************************************************
                               TRadioButton
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

  current design flaws:

  - derived from TCustomCheckBox instead of TButtonControl

  Delphi compatibility:

   - derived from TCustomCheckBox instead of TButtonControl
   - alignment property is missing
   - lots of unknown issues
     
  TODO:

    - check for Delphi compatibility
    * Who's responsible for the fGroup - pointer and who'll free the
      memory allocated for it?????? - automatically managed by GTK+ when
      destroying widget, no worry (MB)
    * make serious tests
    
    - GTK interface : handle reparenting
      
  Bugs:

    - s.a. TCustomCheckbox
}

{------------------------------------------------------------------------------
  Method: TRadioButton.Create
  Params:  aOwner : owner of this object
  Returns: Nothing

  Create a new TRadioButton
 ------------------------------------------------------------------------------}
constructor TRadioButton.Create(AnOwner : TComponent);
begin
  inherited Create(AnOwner);
  fCompStyle := csRadioButton;
  AutoSize := True;
end;

{------------------------------------------------------------------------------
  Method: TRadioButton.SetText
  Params:  Value: TCaption
  Returns: nothing

  Change the caption, and then recreate to update, then call to AutoSize
 ------------------------------------------------------------------------------}
procedure TRadioButton.SetText(const Value: TCaption);
begin
  Inherited SetText(Value);
  RecreateWnd;
  DoAutoSize;
end;

{------------------------------------------------------------------------------
  Method: TRadioButton.DoAutoSize
  Params:  Value : Boolean
  Returns: nothing

  Attempts to Size to fit Caption if AutoSize is True
 ------------------------------------------------------------------------------}
procedure TRadioButton.DoAutoSize;
var
  R : TRect;
  DC : hDC;
begin
  If Autosizing or (not AutoSize) then
    Exit;
  if (not HandleAllocated) or (csLoading in ComponentState) then exit;
  AutoSizing := True;
  DC := GetDC(Handle);
  Try
    R := Rect(0,0, Width, Height);
    DrawText(DC, PChar(Caption), Length(Caption), R,
      DT_CalcRect or DT_NOPrefix);
    If R.Right > 25 then
      Width := R.Right + 25;
    If R.Bottom > 25 then
      Height := R.Bottom + 2;
  Finally
    ReleaseDC(Handle, DC);
    AutoSizing := False;
  end;
end;
// included by stdctrls.pp


{
  $Log$
  Revision 1.10  2002/11/27 14:37:37  mattias
  added form editor options for rubberband and colors

  Revision 1.9  2002/11/14 14:11:51  lazarus
  MG: fixed TRadioButton.DoAutoSize during loading

  Revision 1.8  2002/09/08 19:09:55  lazarus
  Fixed and simplified TRadioButton

  Revision 1.7  2002/09/03 08:07:19  lazarus
  MG: image support, TScrollBox, and many other things from Andrew

  Revision 1.6  2002/08/24 06:51:22  lazarus
  MG: from Andrew: style list fixes, autosize for radio/checkbtns

  Revision 1.5  2002/05/13 14:47:00  lazarus
  MG: fixed client rectangles, TRadioGroup, RecreateWnd

  Revision 1.4  2002/05/13 06:12:57  lazarus
  MG: fixed saving unitlinks after changing fpc soure path

  Revision 1.3  2002/05/10 06:05:55  lazarus
  MG: changed license to LGPL

  Revision 1.2  2002/04/18 08:09:03  lazarus
  MG: added include comments

  Revision 1.1  2000/07/13 10:28:27  michael
  + Initial import

  Revision 1.2  2000/05/09 02:07:40  lazarus
  Replaced writelns with Asserts.                          CAW

  Revision 1.1  2000/04/02 20:49:56  lazarus
  MWE:
    Moved lazarus/lcl/*.inc files to lazarus/lcl/include

  Revision 1.5  2000/01/02 00:25:12  lazarus
  Stoppok:
    - enhanced TCustomradiogroup & TCustomgroupbox

  Revision 1.4  1999/12/30 19:04:13  lazarus
   - Made TRadiobutton work again
   - Some more cleanups to checkbox code
           stoppok

}

