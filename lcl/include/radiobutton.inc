{%MainUnit ../stdctrls.pp}

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
constructor TRadioButton.Create(TheOwner : TComponent);
begin
  inherited Create(TheOwner);
  fCompStyle := csRadioButton;
end;

{------------------------------------------------------------------------------
  Method: TRadioButton.RealSetText
  Params:  Value: TCaption
  Returns: nothing

  Change the caption, and then recreate to update, then call to AutoSize
 ------------------------------------------------------------------------------}
procedure TRadioButton.RealSetText(const Value: TCaption);
begin
  if Text=Value then exit;
  Inherited RealSetText(Value);
  InvalidatePreferredSize;
  AdjustSize;
end;

function TRadioButton.DialogChar(var Message: TLMKey): boolean;
begin
  if IsAccel(Message.CharCode, Caption) and CanFocus then
  begin
    SetFocus;
    Result := true;
  end else
    Result := inherited;
end;

{$IFNDEF EnablePreferredSize}
procedure TRadioButton.DoAutoSize;
var
  R : TRect;
  DC : hDC;
begin
  If Autosizing or (not AutoSize) then
    Exit;
  if (not HandleAllocated) or ([csLoading,csDestroying]*ComponentState<>[]) then
    exit;
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
{$ENDIF}

// included by stdctrls.pp


{
  $Log$
  Revision 1.22  2005/04/27 12:37:28  micha
  implement/fix button/label shortcut accelchar handling

  Revision 1.21  2005/01/24 12:23:11  mattias
  fixed TColorButton.Paint

  Revision 1.20  2005/01/01 19:36:40  mattias
  fixed loading TRadioButton.Checked

  Revision 1.19  2004/11/03 14:18:35  mattias
  implemented preferred size for controls for theme depending AutoSizing

  Revision 1.18  2004/04/18 23:55:39  marc
  * Applied patch from Ladislav Michl
  * Changed the way TControl.Text is resolved
  * Added setting of text to TWSWinControl

  Revision 1.17  2004/04/10 17:58:57  mattias
  implemented mainunit hints for include files

  Revision 1.16  2004/03/07 09:37:20  mattias
  added workaround for AutoSize in TCustomLabel

  Revision 1.15  2004/02/24 20:26:50  mattias
  published some TRadioButton properties

  Revision 1.14  2004/01/06 17:58:06  mattias
  fixed setting TRadioButton.Caption for gtk

  Revision 1.13  2003/03/17 20:53:16  mattias
  removed SetRadioButtonGroupMode

  Revision 1.12  2003/03/17 20:50:30  mattias
  fixed TRadioGroup.ItemIndex=-1

  Revision 1.11  2002/11/27 15:40:36  mattias
  fixed resize request

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

