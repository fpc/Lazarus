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
      memory allocated for it??????
    * make serious tests
      
  Bugs:

    - s.a. TCustomCheckbox
}

{------------------------------------------------------------------------------
  Method: TRadioButton.Create
  Params:  aOwner : owner of this object
  Returns: Nothing

  Create a new TradioButton
 ------------------------------------------------------------------------------}
constructor TRadioButton.Create(AOwner : TComponent);
begin
  inherited Create(AOwner);
  fCompStyle := csRadioButton;
  Visible := False;
  Hint := 'Radiobutton';
  ShowHint := True;
  fGroup := 0;
end;

{------------------------------------------------------------------------------
  Method: TRadioButton.CreateWnd
  Params:  none
  Returns: Nothing

  Create the visual component of a TradioButton.

  HINT: Since every Radiobutton has to belong to a group, we'll search
        our owners componentlist for other radiobuttons. If one is found 
	with it's group property assigned, this instance will get the
	handle of that one as it's fGroup property, otherwise the group 
	property will be 0.
	In the interface we can then use the group property to decide if
	a new group has to be created or which radiobutton is the
	predecessor. The first radiobutton, which handle is created, will get as Group
  the group handle.
	This behaviour is especially neccessary for the GTK bindings.
 ------------------------------------------------------------------------------}
procedure TRadioButton.CreateWnd;
var
  i : integer;
  temp : TComponent;
begin
  fGroup:=0;
  i :=  Owner.ComponentCount-1;
  while (i >= 0) and (fGroup = 0) do
  begin  // search for radiobutton which is predecessor
    Temp := Owner.Components [i];
    if (Temp is TRadioButton) and (TRadioButton (temp).fGroup <> 0) then
      fGroup := THandle (TRadioButton(Temp).Handle); // set group to our predecessor
    dec (i);
  end;
  inherited CreateWnd;
end;

{------------------------------------------------------------------------------
  Method: TRadioButton.DestroyWnd
  Params:  none
  Returns: Nothing

  The handle is destroyed, and therefore the radiobutton does not belong any
  longer to a group. The Group property is the handle of the predecessor
  TRadioButton.
  -> Bind the successor TRadioButton to our predecessor
 ------------------------------------------------------------------------------}
procedure TRadioButton.DestroyWnd;
var
  i: integer;
  ARadioButton: TRadioButton;
begin
  if fGroup<>0 then begin
    if HandleAllocated then begin
      for i:=0 to Owner.ComponentCount-1 do
        if (Owner.Components[i] is TRadioButton) then begin
          ARadioButton:=TRadioButton(Owner.Components[i]);
          if ARadioButton.fGroup=Handle then
            ARadioButton.fGroup:=fGroup;
        end;
    end;
    fGroup:=0;
  end;
  inherited DestroyWnd;
end;

{------------------------------------------------------------------------------
  Method: TRadioButton.SetGroup
  Params:  value : group this radiobutton belongs to
  Returns: Nothing

  Assign this radiobutton to a group of radiobuttons.
 ------------------------------------------------------------------------------}
procedure TRadioButton.SetGroup(Value : THandle);
begin
  Assert(False, 'Trace:IN SETGROUP.  Caption = '+ Self.Caption);
  FGroup := Value;
//SH?   ReCreate;
end;

{------------------------------------------------------------------------------
  Method: TRadioButton.GetGroup
  Params:  none
  Returns: Handle of the group this button belongs to

  Return handle to the group the radiobutton belongs to.
 ------------------------------------------------------------------------------}
function TRadioButton.GetGroup : THandle;
begin
  GetGroup := FGroup;
end;

// included by stdctrls.pp

(*
procedure TRadioButton.ReCreate;
//var
// ******************************
// the following line has been commented out so that gtk dependency can be
// removed from all files. The following function needs to be moved.   MAH
//
//   gLabel : PgChar;

begin

   if Caption = '' then
      Caption := 'RadioGroup1';

// ******************************
// the following line has been commented out so that gtk dependency can be
// removed from all files. The following function needs to be moved.   MAH
//
//   gLAbel := StrAlloc(length(Self.Caption) + 1);
//   StrPCopy(gLabel,Self.Caption);

// ******************************
// the following line has been commented out so that gtk dependency can be
// removed from all files. The following function needs to be moved.   MAH
//
//   FComponent := gtk_radio_button_new_with_label(
//   gtk_radio_button_group(GTk_Radio_Button(Group.FComponent)),gLabel);
//   gtkGroup := gtk_radio_button_group(GTk_Radio_Button(Group.FComponent));
end;
*)

{
  $Log$
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

