{******************************************************************************
                                TCustomRadioBox
 ******************************************************************************}
{ 
	   
  Delphi compatibility:

   - the interface is almost like in delphi 5
   - with the GTK-bindings there must always on button active,
     ItemIndex= -1 can't be handled
   - FlipChildren procedure is missing
   
  TODO:
       - may be neccessary to implement RecreateWnd 
         (can't be checked now, because the implementation TWinControl
	  is not yet ready)
       - 1st button is not placed at right top-position
       - What about OnClick?

  Possible improvements:
       - The current implementation often recreates the group even
         if it might not be neccessary. This could be solved if with
	 an approach like Marc Weustink suggested:
	 
         "Why not on SetColumn/SetItems/FItems.Onchange create the necesary checkboxes
         and align them. This way the RadioGroup is just a control with other
         controls in it. It doesn't matter if the the gtk control is created or not.
         If not created and you already have added checkboxes, they will be created
         when the groupbox is created and wil be destroyed when the groupbox is
         destroyed. This way you internally allways deal with TCheckboxes and you
         dont have to mess with creating/destroying them. Besides that, you dont have
         te recreate the control on every change."
  
         On the other side this might have the following disadvantages:
           - requires some work to find out which buttons to add/delete
	   - the TButtonList and the group property of affected buttons
	     have to be updated according to the new order of buttons
	   - works only if the interface library supports reordering of
	     radiobuttons

  Bugs:
       - Adding/Deleting Items won't work because of missing RecreatWnd
}

{------------------------------------------------------------------------------
  Method: TCustomRadioGroup.Create
  Params:  AOwner: the owner of the class
  Returns: Nothing

  Constructor for the radiogroup
 ------------------------------------------------------------------------------}
constructor TCustomRadioGroup.Create (AOwner : TComponent);
begin
   inherited Create (AOwner);
   ControlStyle := [csCaptureMouse, csClickEvents, csSetCaption, csDoubleClicks];
   FItems      := TStringList.Create;
   TStringList(FItems).OnChanging := @ItemsChanged;
   FItemIndex  := -1;
   FButtonList := TList.Create;
   FColumns    := 1;
end;

{------------------------------------------------------------------------------
  Method: TCustomRadioGroup.Destroy
  Params:  none
  Returns: Nothing

  Destructor for the radiogroup
 ------------------------------------------------------------------------------}
destructor TCustomRadioGroup.Destroy;
begin
   FItems.Free;
   FButtonList.Free;
   inherited Destroy;
end;

{------------------------------------------------------------------------------
  Method: TCustomRadioGroup.CreateWnd
  Params:  none
  Returns: Nothing

  Create the visual component of the Radiogroup.
 ------------------------------------------------------------------------------}
procedure TCustomRadioGroup.CreateWnd;
var
   i       : integer;
   temp    : TRadioButton;
   nextTop : integer;
   nextLeft: integer;
   vertDist: integer;
   horzDist: integer;
   rbWidth : integer;
begin
   vertDist := Height DIV ((FItems.Count DIV FColumns) + 2);
   nextTop  := vertDist;
   horzDist := (Width - 20) DIV FColumns;
   nextLeft := 10;
   rbWidth  := horzDist;
   FButtonList.Clear;
   i := 0;
   while i < FItems.Count do
   begin
      Temp := TRadioButton.Create (self);
      Temp.Parent := self;
      Temp.Top    := nextTop;
      Temp.Left   := nextLeft;
      Temp.Width  := rbWidth;
      Temp.Height := vertDist;
      Temp.Caption := FItems.Strings [i];

      if i = FItemIndex
         then Temp.Checked := true
	 else Temp.Checked := false;
      Temp.Show;
      FButtonList.Add (Temp);
      inc (i);
      if (i MOD FColumns) = 0 then
      begin
         inc (nextTop, vertDist);
	 nextLeft := 10;
      end
      else begin
         inc (nextLeft, horzDist);
      end;
   end;
   inherited CreateWnd;
end;

{------------------------------------------------------------------------------
  Method: TCustomRadioGroup.ItemsChanged
  Params:  sender : object calling this proc. (in fact the FItems instance)
  Returns: Nothing

  Set the FColumns property which determines the no columns in
  which the radiobuttons should be arranged.
  Range: 1 .. ???
 ------------------------------------------------------------------------------}
procedure TCustomRadioGroup.ItemsChanged (Sender : TObject);
begin
   RecreateWnd;
end;

{------------------------------------------------------------------------------
  Method: TCustomRadioGroup.SetColumns
  Params:  value - no of columns of the radiogroup
  Returns: Nothing

  Set the FColumns property which determines the no columns in
  which the radiobuttons should be arranged.
  Range: 1 .. ???
 ------------------------------------------------------------------------------}
procedure TCustomRadioGroup.SetColumns (value : integer);
begin
  if Value <> FColumns then 
  begin
     if (Value < 1) 
        then raise Exception.Create('TCustomRadioGroup: Columns must be > 1');
     FColumns := value;
     if HandleAllocated then RecreateWnd;
  end;
end;

{------------------------------------------------------------------------------
  Method: TCustomRadioGroup.SetItem
  Params:  value - Stringlist containing items to be displayed as radiobuttons
  Returns: Nothing

  Assign items from a stringlist.
 ------------------------------------------------------------------------------}
procedure TCustomRadioGroup.SetItem (value : TStrings);
begin
  if Value <> FItems then 
  begin
     FItems.Assign(Value);
     if HandleAllocated then RecreateWnd;
  end;
end;

{------------------------------------------------------------------------------
  Method: TCustomRadioGroup.SetItemIndex
  Params:  value - index of RadioButton to be selected
  Returns: Nothing

  Select one of the radiobuttons
 ------------------------------------------------------------------------------}
procedure TCustomRadioGroup.SetItemIndex (value : integer);
begin
   if (Value < -1) or (Value >= FItems.Count) 
      then raise Exception.Create('TCustomRadioGroup: Out of bounds');

   if (HandleAllocated) and (value <> FItemIndex) then
   begin
      if (FItemIndex <> -1)
         then TRadioButton (FButtonList [FItemIndex]).Checked := false;
      FItemIndex := value;
      if (value <> -1) 
         then TRadioButton (FButtonList [value]).Checked := true;
   end
   else FItemIndex := value;
end;

{------------------------------------------------------------------------------
  Method: TCustomRadioGroup.SetItemIndex
  Params:  value - index of RadioButton to be selected
  Returns: Nothing

  Retrieve the index of the radiobutton currently selected.
 ------------------------------------------------------------------------------}
function TCustomRadioGroup.GetItemIndex : integer;
var
   i       : integer;
begin
   result := -1;
   if (HandleAllocated) then
   begin
      i := 0;
      // This nasty little loop is neccessary because the group is not informed when a button is pressed	
      while (i < FButtonList.Count) and (result = -1) do
      begin	// find the actice button
	 if TRadioButton (FButtonList [i]).Checked 
            then result := i;
         inc (i);
      end;
      FItemIndex := result;
   end
   else
      result := FItemIndex;
end;

{------------------------------------------------------------------------------
  Method: TCustomRadioGroup.CanModify
  Params:  none
  Returns: always true

  Is the user allowed to select a different radiobutton?
 ------------------------------------------------------------------------------}
function TCustomRadioGroup.CanModify : boolean;
begin
   result := true;
end;

{
  $Log$
  Revision 1.1  2000/07/13 10:28:25  michael
  + Initial import

  Revision 1.2  2000/06/22 20:57:07  lazarus
  *** empty log message ***

  Revision 1.1  2000/04/02 20:49:56  lazarus
  MWE:
    Moved lazarus/lcl/*.inc files to lazarus/lcl/include

  Revision 1.3  2000/01/06 01:10:36  lazarus
  Stoppok:
     - changed ReadState to match current definition in fcl
       (affects TPage & TCustomNotebook)
     - added callback FItems.OnChanging to TCustomRadiogroup

  Revision 1.2  2000/01/02 00:25:12  lazarus
  Stoppok:
    - enhanced TCustomradiogroup & TCustomgroupbox

  Revision 1.1  1999/12/31 02:20:57  lazarus
    Initial implementation of TCustomRadioGroup / TRadioGroup
      stoppok

}
