{******************************************************************************
                                  TMenuItem
 ******************************************************************************}

{------------------------------------------------------------------------------
  Method:  TMenuItem.Add
  Params:  Item: 
  Returns: Nothing

  Description of the procedure for the class.
 ------------------------------------------------------------------------------}
procedure TMenuItem.Add(Item: TMenuItem);
begin
  Insert(GetCount, Item);
end;

{------------------------------------------------------------------------------
  Method: TMenuItem.Create
  Params:  AOwner: the owner of the class
  Returns: Nothing

  Constructor for the class.
 ------------------------------------------------------------------------------}
constructor TMenuItem.Create(AOwner: TComponent);
begin
  Inherited Create(AOwner);
  FCompStyle := csMenuItem;
  FHandle := 0;
  FItems := nil;
  FMenu := nil;
  FParent := nil;
  FShortCut := 0;	
  FChecked := False;
  FVisible := True;
  FEnabled := True;
end;

{------------------------------------------------------------------------------
  Method:  TMenuItem.CreateHandle
  Params:  None
  Returns: Nothing

  Creates the handle ( = object).
 ------------------------------------------------------------------------------}
procedure TMenuItem.CreateHandle;
var
  n: Integer;
begin
  if Parent <> nil 
  then begin
    Parent.HandleNeeded;
    if Parent.HandleAllocated 
    then InterfaceObject.IntSendMessage3(LM_CREATE, Self, nil); 
    if FHandle <> 0 then AttachSignals;
  end;
  if (FItems <> nil) and ((Parent = nil) or Parent.HandleAllocated) 
  then begin
    for n := 0 to FItems.Count - 1 do 
    begin
      InterfaceObject.IntSendMessage3(LM_ATTACHMENU, TObject(FItems[n]), nil);
    end;
  end;
end;

{------------------------------------------------------------------------------
  Method:  TMenuItem.AttachSignals
  Params:  none
  Returns: Nothing

  Gets called after the window is created but before the controls are created 
  and the cached propeties are set. 
  This is the only place where a call to SetCallBack is made.
 ------------------------------------------------------------------------------}
procedure TMenuItem.AttachSignals; 
begin
  InterfaceObject.SetCallback(LM_ACTIVATE, Self);
end;

{------------------------------------------------------------------------------
  Method:  TMenuItem.DetachSignals
  Params:  none
  Returns: Nothing

  Gets called the moment the window is about to be destroyed. All callbacks 
  should be removed here.
 ------------------------------------------------------------------------------}
procedure TMenuItem.DetachSignals; 
begin
  InterfaceObject.RemoveCallbacks(Self);
end;


{------------------------------------------------------------------------------
  Method:  TMenuItem.Delete
  Params:  Index: 
  Returns: Nothing

  Description of the procedure for the class.
 ------------------------------------------------------------------------------}
procedure TMenuItem.Delete(Index: Integer);
begin
  //TODO: Complete
  DetachSignals;
end;

{------------------------------------------------------------------------------
  Method: TMenuItem.Destroy
  Params:  None
  Returns: Nothing

  Destructor for the class.
 ------------------------------------------------------------------------------}
destructor TMenuItem.Destroy;
begin
  FItems.Free;
  inherited Destroy;
end;

{------------------------------------------------------------------------------
  Method:  TMenuItem.DoClicked
  Params:  msg: 
  Returns: Nothing

  Description of the procedure for the class.
 ------------------------------------------------------------------------------}
procedure TMenuItem.DoClicked(var msg); 
begin
   Assert(False, 'Trace:Menu received a message - CLICKED');
   if Assigned (FOnClick) then
      FOnClick(Self);
end;

{------------------------------------------------------------------------------
  Function: TMenuItem.GetCount
  Params:   none
  Returns:  Number of child menuitems.

  Returns the number of child menuitems.
 ------------------------------------------------------------------------------}
function TMenuItem.GetCount: Integer;
begin
  if FItems = nil 
  then Result := 0
  else Result := FItems.Count;
end;

{------------------------------------------------------------------------------
  Function: TMenuItem.GetHandle
  Params:   none
  Returns:  String containing output from the function.

  Description of the function for the class.
 ------------------------------------------------------------------------------}
function TMenuItem.GetHandle: HMenu;
begin
  HandleNeeded;
  Result := FHandle;
end;

{------------------------------------------------------------------------------
  Function: TMenuItem.GetItem
  Params:   none
  Returns:  String containing output from the function.

  Description of the function for the class.
 ------------------------------------------------------------------------------}
function TMenuItem.GetItem(Index: Integer): TMenuItem;
begin
  if FItems = nil 
  then raise EMenuError.Create('Menu index out of range');
  Result := TMenuItem(FItems[Index]);
end;

{------------------------------------------------------------------------------
  Function: TMenuItem.GetParent
  Params:   none
  Returns:  String containing output from the function.

  Description of the function for the class.
 ------------------------------------------------------------------------------}
function TMenuItem.GetParent: TMenuItem;
begin
  Result := FParent;
end;

{------------------------------------------------------------------------------
  Function: TMenuItem.GetParentMenu
  Params:   none
  Returns:  The (popup)menu containing this item.

  Description of the function for the class.
 ------------------------------------------------------------------------------}
function TMenuItem.GetParentMenu: TMenu;
var
  Item: TMenuItem;
begin
  Item := Self;
  while Item.Parent <> nil do Item := Item.Parent;
  Result := Item.FMenu
end;

{------------------------------------------------------------------------------
  Function: TMenuItem.HandleAllocated
  Params:   None
  Returns:  True is handle is allocated

  Checks if a handle is allocated. I.E. if the control is created
 ------------------------------------------------------------------------------}
function TMenuItem.HandleAllocated : Boolean;
begin
   HandleAllocated := (FHandle <> 0);
end;

{------------------------------------------------------------------------------
  Method:  TMenuItem.HandleNeeded
  Params:  AOwner: the owner of the class
  Returns: Nothing

  Description of the procedure for the class.
 ------------------------------------------------------------------------------}
procedure TMenuItem.HandleNeeded;
begin
  if not HandleAllocated 
  then CreateHandle;
end;

{------------------------------------------------------------------------------
  Method:  TMenuItem.Insert
  Params:  Index: Location of the menuitem to insert
           Item: Menu item to insert
  Returns: Nothing

  Inserts a menu child at the given index position.
 ------------------------------------------------------------------------------}
procedure TMenuItem.Insert(Index: Integer; Item: TMenuItem);
begin
  if (Item <> nil) 
  then begin
    if Item.Parent <> nil 
    then raise EMenuError.Create('Menu inserted twice');
    
    // create items if needed;
    if FItems = nil then FItems := TList.Create;
    
    // check if we are the main menu
    // mainmenuitems have csMenu style
//    if FMenu <> nil then Item.FCompStyle := csMenu;

    Item.FParent := Self;
    FItems.Insert(Index, Item);
    MenuChanged(FItems.Count = 1);
  end;
end;

{------------------------------------------------------------------------------
  Function:TMenuItem.IndexOf
  Params:  Item: The index requested for.
  Returns: Nothing

  Returns the index of the menuitem.
 ------------------------------------------------------------------------------}
function TMenuItem.IndexOf(Item: TMenuItem): Integer;
begin
  if FItems = nil 
  then Result := -1
  else Result := FItems.IndexOf(Item);
end;

{------------------------------------------------------------------------------
  Method:  TMenuItem.MenuChanged
  Params:  AOwner: the owner of the class
  Returns: Nothing

  Description of the procedure for the class.
 ------------------------------------------------------------------------------}
Procedure TMenuItem.MenuChanged(Rebuild : Boolean);
Begin
//Use send message to re-create the menu if REBUILD is true.
end;

{------------------------------------------------------------------------------
  Method:  TMenuItem.Remove
  Params:  Item: 
  Returns: Nothing

  Description of the procedure for the class.
 ------------------------------------------------------------------------------}
procedure TMenuItem.Remove(Item: TMenuItem);
begin
end;

{------------------------------------------------------------------------------
  Method:  TMenuItem.SetCaption
  Params:  Value:
  Returns: Nothing

  Sets the caption of a menuItem.
 ------------------------------------------------------------------------------}
procedure TMenuItem.SetCaption(const Value: string);
begin
  FCaption := Value;
  //TODO: Add runtime code here
end;

{------------------------------------------------------------------------------
  Method:  TMenuItem.SetChecked
  Params:  Value:
  Returns: Nothing

  Places a checkmark in fron of the label.
 ------------------------------------------------------------------------------}
procedure TMenuItem.SetChecked(Value: Boolean);
begin
  FChecked := Value;
  //TODO: Add runtime code here
end;

{------------------------------------------------------------------------------
  Method:  TMenuItem.SetDefault
  Params:  Value:
  Returns: Nothing

  Makes a menuItem the default item (BOLD).
 ------------------------------------------------------------------------------}
procedure TMenuItem.SetDefault(Value: Boolean);
begin
  FDefault := Value;
  //TODO: Add runtime code here
end;

{------------------------------------------------------------------------------
  Method:  TMenuItem.SetEnabled
  Params:  Value:
  Returns: Nothing

  Enables a menuItem.
 ------------------------------------------------------------------------------}
procedure TMenuItem.SetEnabled(Value: Boolean);
begin
If FEnabled <> Value then
	Begin
	FEnabled := Value;
        //TODO: this doesn't work. HAndle is 0.  What's wrong?
        InterfaceObject.IntSendMessage3(LM_SETENABLED,Self,nil);
	End;

  //TODO: Add runtime code here
end;

{------------------------------------------------------------------------------
  Method:  TMenuItem.SetImageIndex
  Params:  Value:
  Returns: Nothing

  Enables a menuItem.
 ------------------------------------------------------------------------------}
procedure TMenuItem.SetImageIndex(Value: Integer);
begin
  FImageIndex := Value;
  //TODO: TMENUITEM.SETINDEXINDEX
end;


{------------------------------------------------------------------------------
  Method:  TMenuItem.SetShortCut
  Params:  Value:
  Returns: Nothing

  Description of the procedure for the class.
 ------------------------------------------------------------------------------}
Procedure TMenuItem.SetShortCut(Value : TShortCut);
Begin
  if FShortCut <> Value then
	Begin
	  FShortCut := Value;
	  MenuChanged(True);  //Use this to do a sendmessage
	End;
end;

{------------------------------------------------------------------------------
  Method:  TMenuItem.SetVisible
  Params:  Value: Visibility
  Returns: Nothing

  Description of the procedure for the class.
 ------------------------------------------------------------------------------}
procedure TMenuItem.SetVisible(Value: Boolean);
begin
//  HandleNeeded;
  FVisible := Value;
end;


{ =============================================================================

  $Log$
  Revision 1.1  2000/07/13 10:28:26  michael
  + Initial import

  Revision 1.2  2000/04/17 19:50:06  lazarus
  Added some compiler stuff built into Lazarus.
  This depends on the path to your compiler being correct in the compileroptions
  dialog.
  Shane

  Revision 1.1  2000/04/02 20:49:56  lazarus
  MWE:
    Moved lazarus/lcl/*.inc files to lazarus/lcl/include

  Revision 1.9  1999/12/14 22:21:11  lazarus
  *** empty log message ***

  Revision 1.8  1999/12/10 00:47:01  lazarus
  MWE:
    Fixed some samples
    Fixed Dialog parent is no longer needed
    Fixed (Win)Control Destruction
    Fixed MenuClick


  $Log$
  Revision 1.1  2000/07/13 10:28:26  michael
  + Initial import

  Revision 1.2  2000/04/17 19:50:06  lazarus
  Added some compiler stuff built into Lazarus.
  This depends on the path to your compiler being correct in the compileroptions
  dialog.
  Shane

  Revision 1.1  2000/04/02 20:49:56  lazarus
  MWE:
    Moved lazarus/lcl/*.inc files to lazarus/lcl/include

  Revision 1.9  1999/12/14 22:21:11  lazarus
  *** empty log message ***

  Revision 1.8  1999/12/10 00:47:01  lazarus
  MWE:
    Fixed some samples
    Fixed Dialog parent is no longer needed
    Fixed (Win)Control Destruction
    Fixed MenuClick

  Revision 1.7  1999/12/08 00:56:07  lazarus
  MWE:
    Fixed menus. Events aren't enabled yet (dumps --> invalid typecast ??)

  Revision 1.6  1999/11/05 00:34:11  lazarus
  MWE: Menu structure updated, events and visible code not added yet

  Revision 1.5  1999/10/28 23:48:57  lazarus
  MWE: Added new menu classes and started to use handleneeded


}
