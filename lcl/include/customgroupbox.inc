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
{  function TCustomGroupBox.Create                                             }
{------------------------------------------------------------------------------}
constructor TCustomGroupBox.Create(AOwner: TComponent);
begin
  inherited Create (AOwner);
  fCompStyle := csGroupBox;
  ControlStyle := ControlStyle + [csAcceptsControls];
  Width:= 185;
  Height:= 105;
end;
 
// included by stdctrls.pp

{
  $Log$
  Revision 1.8  2002/08/30 06:46:03  lazarus
  Use comboboxes. Use history. Prettify the dialog. Preselect text on show.
  Make the findreplace a dialog. Thus removing resiying code (handled by Anchors now anyway).
  Make Anchors work again and publish them for various controls.
  SelStart and Co. for TEdit, SelectAll procedure for TComboBox and TEdit.
  Clean up and fix some bugs for TComboBox, plus selection stuff.

  Revision 1.7  2002/08/17 15:45:32  lazarus
  MG: removed ClientRectBugfix defines

  Revision 1.6  2002/05/10 06:05:51  lazarus
  MG: changed license to LGPL

  Revision 1.5  2002/05/09 12:41:28  lazarus
  MG: further clientrect bugfixes

  Revision 1.4  2002/04/22 13:07:45  lazarus
  MG: fixed AdjustClientRect of TGroupBox

  Revision 1.3  2002/04/21 06:53:55  lazarus
  MG: fixed save lrs to test dir

  Revision 1.2  2002/04/18 08:09:03  lazarus
  MG: added include comments

  Revision 1.1  2000/07/13 10:28:25  michael
  + Initial import

}
