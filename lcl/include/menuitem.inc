// included by menus.pp

{******************************************************************************
                                  TMenuItem
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
const
  SMenuNotFound   = 'Sub-menu is not in menu';
  SMenuIndexError = 'Menu index out of range';
  SMenuItemIsNil  = 'MenuItem is nil';

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
  Params:  TheOwner: the owner of the class
  Returns: Nothing

  Constructor for the class.
 ------------------------------------------------------------------------------}
constructor TMenuItem.Create(TheOwner: TComponent);
begin
  //writeln('TMenuItem.Create START TheOwner=',TheOwner.Name,':',TheOwner.ClassName);
  if not assigned (TheOwner) then writeln ('**SH: Warn: creating MenuItem with Owner = nil');
  
  Inherited Create(TheOwner);

  FCompStyle := csMenuItem;
  FHandle := 0;
  FItems := nil;
  FMenu := nil;
  FParent := nil;
  FShortCut := 0;
  FChecked := False;
  FVisible := True;
  FEnabled := True;
  //writeln('TMenuItem.Create END TheOwner=',TheOwner.Name,':',TheOwner.ClassName);
end;

{------------------------------------------------------------------------------
  Method:  TMenuItem.CreateHandle
  Params:  None
  Returns: Nothing

  Creates the handle ( = object).
 ------------------------------------------------------------------------------}
procedure TMenuItem.CreateHandle;
var i: Integer;
begin
  //writeln('TMenuItem.CreateHandle START ',Name,':',ClassName);
  InterfaceObject.IntSendMessage3(LM_CREATE, Self, nil);
  if FItems<>nil then begin
    for i := 0 to Count - 1 do begin
      Items[i].HandleNeeded;
    end;
  end;
  if Parent <> nil then
  begin
    Parent.HandleNeeded;
    if Parent.HandleAllocated then
      InterfaceObject.IntSendMessage3(LM_ATTACHMENU, Self, nil);
    if HandleAllocated then begin
      if ShortCut <> 0 then ShortCutChanged(0, Shortcut);
    end;  
  end;
  {
  if (FItems <> nil) and ((Parent = nil) or Parent.HandleAllocated)
  then begin
    for n := 0 to FItems.Count - 1 do 
    begin
      InterfaceObject.IntSendMessage3(LM_ATTACHMENU, TObject(FItems[n]), nil);
    end;
  end;
  }
  //writeln('TMenuItem.CreateHandle END ',Name,':',ClassName);
end;

{------------------------------------------------------------------------------
  Method:  TMenuItem.Delete
  Params:  Index: 
  Returns: Nothing

  Description of the procedure for the class.
 ------------------------------------------------------------------------------}
procedure TMenuItem.Delete(Index: Integer);
var
  Cur: TMenuItem;
begin
  if (Index < 0) or (FItems = nil) or (Index >= GetCount) then
    raise EMenuError.Create(SMenuIndexError);
  Cur := TMenuItem(FItems[Index]);
  if Cur=nil then
    raise EMenuError.Create(SMenuItemIsNil);
  Cur.DestroyHandle;
  FItems.Delete(Index);
  Cur.FParent := nil;
  Cur.FOnChange := nil;
  MenuChanged(Count = 0);
end;

{------------------------------------------------------------------------------
  Method: TMenuItem.Destroy
  Params:  None
  Returns: Nothing

  Destructor for the class.
 ------------------------------------------------------------------------------}
destructor TMenuItem.Destroy;
var
  i : integer;
begin
  if FGraphic<>nil then
    FreeAndNil(FGraphic);
  DestroyHandle;
  if assigned (FItems) then begin
    i := FItems.Count-1;
    while i>=0 do begin
      TMenuItem(FItems[i]).Free;
      dec(i);
    end;
  end;
  FItems.Free;
  FItems:=nil;
  if FParent<>nil then
    FParent.FItems.Remove(Self);
  inherited Destroy;
end;

{------------------------------------------------------------------------------
  function TMenuItem.GetImageList: TCustomImageList;


 ------------------------------------------------------------------------------}
function TMenuItem.GetImageList: TCustomImageList;
var
  LItem: TMenuItem;
  LMenu: TMenu;
begin
  Result := nil;
  LItem := Parent;
  while (LItem <> nil) and (LItem.SubMenuImages = nil) do
    LItem := LItem.Parent;
  if LItem <> nil then
    Result := LItem.SubMenuImages
  else
  begin
    LMenu := GetParentMenu;
    if LMenu <> nil then
      Result := LMenu.Images;
  end;
end;

{------------------------------------------------------------------------------
  function TMenuItem.GetParentComponent: TComponent;


 ------------------------------------------------------------------------------}
function TMenuItem.GetParentComponent: TComponent;
begin
  if (FParent <> nil) and (FParent.FMenu <> nil) then
    Result := FParent.FMenu
  else
    Result := FParent;
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
  Function: TMenuItem.GetChildren
  Params:   Proc - proc to be called for each child
  	    Root - root component
  Returns:  nothing

  For each item call "proc"
 ------------------------------------------------------------------------------}
procedure TMenuItem.GetChildren(Proc: TGetChildProc; Root: TComponent);
var
  i : Integer;
Begin
  if not assigned (FItems) then exit;

  for i := 0 to FItems.Count - 1 do
    if TComponent (FItems[i]).Owner = Root then
      Proc(TComponent (FItems [i]));
end;

{------------------------------------------------------------------------------
  Function: TMenuItem.GetCount
  Params:   none
  Returns:  Number of child menuitems.

  Returns the number of child menuitems.
 ------------------------------------------------------------------------------}
function TMenuItem.GetCount: Integer;
begin
  if FItems = nil then
    Result := 0
  else
    Result := FItems.Count;
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
  if FItems = nil then
    raise EMenuError.Create('Menu index out of range');
  Result := TMenuItem(FItems[Index]);
end;

{------------------------------------------------------------------------------
  function TMenuItem.GetMenuIndex: Integer;

  Get position of this menuitem in its menu
 ------------------------------------------------------------------------------}
function TMenuItem.GetMenuIndex: Integer;
begin
  Result := -1;
  if FParent <> nil then Result := FParent.IndexOf(Self);
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
  function TMenuItem.IsCaptionStored: boolean;

  Checks if 'Caption' needs to be saved to stream
 ------------------------------------------------------------------------------}
function TMenuItem.IsCaptionStored: boolean;
begin
  Result := (ActionLink = nil) or not FActionLink.IsCaptionLinked;
end;

{------------------------------------------------------------------------------
  function TMenuItem.IsCheckedStored: boolean;

  Checks if 'Checked' needs to be saved to stream
 ------------------------------------------------------------------------------}
function TMenuItem.IsCheckedStored: boolean;
begin
  Result := (ActionLink = nil) or not FActionLink.IsCheckedLinked;
end;

{------------------------------------------------------------------------------
  function TMenuItem.IsEnabledStored: boolean;

  Checks if 'Checked' needs to be saved to stream
 ------------------------------------------------------------------------------}
function TMenuItem.IsEnabledStored: boolean;
begin
  Result := (ActionLink = nil) or not FActionLink.IsEnabledLinked;
end;

{------------------------------------------------------------------------------
  function TMenuItem.IsShortCutStored: boolean;

  Checks if 'Checked' needs to be saved to stream
 ------------------------------------------------------------------------------}
function TMenuItem.IsShortCutStored: boolean;
begin
  Result := (ActionLink = nil) or not FActionLink.IsShortCutLinked;
end;

{------------------------------------------------------------------------------
  function TMenuItem.IsVisibleStored: boolean;

  Checks if 'Checked' needs to be saved to stream
 ------------------------------------------------------------------------------}
function TMenuItem.IsVisibleStored: boolean;
begin
  Result := (ActionLink = nil) or not FActionLink.IsVisibleLinked;
end;

{------------------------------------------------------------------------------
  procedure TMenuItem.SetAutoCheck(const AValue: boolean);

  If user clicks, toggle 'Checked'
 ------------------------------------------------------------------------------}
procedure TMenuItem.SetAutoCheck(const AValue: boolean);
var
  OldIsCheckItem: boolean;
begin
  if FAutoCheck=AValue then exit;
  OldIsCheckItem:=IsCheckItem;
  FAutoCheck:=AValue;
  if (OldIsCheckItem<>IsCheckItem) and (HandleAllocated) then
    RecreateHandle;
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
  if not HandleAllocated then CreateHandle;
end;

{------------------------------------------------------------------------------
  function TMenuItem.HasIcon: boolean;

  Returns true if there is an icon
 ------------------------------------------------------------------------------}
function TMenuItem.HasIcon: boolean;
begin
  Result:=(FGraphic<>nil) or ((ImageIndex>=0) and (GetImageList<>nil));
end;

{------------------------------------------------------------------------------
  procedure TMenuItem.DestroyHandle;

  Free the Handle
 ------------------------------------------------------------------------------}
procedure TMenuItem.DestroyHandle;
var i: integer;
begin
  if not HandleAllocated then exit;
  if assigned (FItems) then begin
    i := FItems.Count-1;
    while i>=0 do begin
      TMenuItem(FItems[i]).DestroyHandle;
      dec(i);
    end;
  end;
  InterfaceObject.IntSendMessage3(LM_DESTROY, Self, nil);
  FHandle:=0;
end;

{------------------------------------------------------------------------------
  procedure TMenuItem.RecreateHandle;

  Destroy and re-Create handle. This is done, when the type or the context
  of the TMenuItem is changed.
 ------------------------------------------------------------------------------}
procedure TMenuItem.RecreateHandle;
begin
  if not HandleAllocated then exit;
  DestroyHandle;
  HandleNeeded;
end;

{------------------------------------------------------------------------------
  Method:  TMenuItem.HasParent
  Params:  
  Returns: True - the item has a parent responsible for streaming

 ------------------------------------------------------------------------------}
function TMenuItem.HasParent : Boolean;
begin
  Result := assigned (FParent);
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
  if (Item = nil) then exit;
  if Item.Parent <> nil then
    raise EMenuError.Create('Menu inserted twice');
  
  // create Items if needed
  if FItems = nil then FItems := TList.Create;

  // adjust GroupIndex
  if (Index>0) and (Index < FItems.Count) then
    if Item.GroupIndex < TMenuItem(FItems[Index - 1]).GroupIndex then
      Item.GroupIndex := TMenuItem(FItems[Index - 1]).GroupIndex;
  VerifyGroupIndex(Index, Item.GroupIndex);

  Item.FParent := Self;
  Item.FOnChange := @SubItemChanged;
  FItems.Insert(Index, Item);
  
  if HandleAllocated then begin
    Item.HandleNeeded;
    //InterfaceObject.IntSendMessage3(LM_ATTACHMENU, Item, nil);
  end;
  MenuChanged(FItems.Count = 1);
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
  function TMenuItem.IndexOfCaption(const ACaption: string): Integer;

  Resturns the index of the menuitem with the given caption or -1
 ------------------------------------------------------------------------------}
function TMenuItem.IndexOfCaption(const ACaption: string): Integer;
begin
  for Result:=0 to Count-1 do
    if Items[Result].Caption=ACaption then exit;
  Result:=-1;
end;

{------------------------------------------------------------------------------
  Method:  TMenuItem.MenuChanged
  Params:  Rebuild : Boolean
  Returns: Nothing

 ------------------------------------------------------------------------------}
Procedure TMenuItem.MenuChanged(Rebuild : Boolean);
var
  Source: TMenuItem;
begin
  if (Parent = nil) and (Owner is TMenu) then
    Source := nil
  else
    Source := Self;
  if Assigned(FOnChange) then FOnChange(Self, Source, Rebuild);
end;

{------------------------------------------------------------------------------
  procedure TMenuItem.SetChildOrder(Child: TComponent; Order: Integer);

  Reposition the MenuItem
 ------------------------------------------------------------------------------}
procedure TMenuItem.SetChildOrder(Child: TComponent; Order: Integer);
begin
  (Child as TMenuItem).MenuIndex := Order;
end;

{------------------------------------------------------------------------------
  procedure TMenuItem.Remove(Item: TMenuItem);


 ------------------------------------------------------------------------------}
procedure TMenuItem.Remove(Item: TMenuItem);
var
  I: Integer;
begin
  I := IndexOf(Item);
  if I<0 then raise EMenuError.Create(SMenuNotFound);
  Delete(I);
end;

{------------------------------------------------------------------------------
  function TMenuItem.IsCheckItem: boolean;

  Results true if 'Checked' or 'RadioItem' or 'AutoCheck'
  or 'ShowAlwaysCheckable'
 ------------------------------------------------------------------------------}
function TMenuItem.IsCheckItem: boolean;
begin
  Result:=Checked or RadioItem or AutoCheck or ShowAlwaysCheckable;
end;

{------------------------------------------------------------------------------
  Method:  TMenuItem.SetCaption
  Params:  Value:
  Returns: Nothing

  Sets the caption of a menuItem.
 ------------------------------------------------------------------------------}
procedure TMenuItem.SetCaption(const AValue: string);
begin
  if FCaption = AValue then exit;
  FCaption := AValue;
  if HandleAllocated then
    InterfaceObject.IntSendMessage3(LM_SetLabel, Self, PChar(AValue));
end;

{------------------------------------------------------------------------------
  Method:  TMenuItem.SetChecked
  Params:  Value:
  Returns: Nothing

  Places a checkmark in front of the label.
 ------------------------------------------------------------------------------}
procedure TMenuItem.SetChecked(AValue: Boolean);
begin
  if FChecked <> AValue then
  begin
    FChecked := AValue;
    if AValue and FRadioItem then
      TurnSiblingsOff;
    if (FParent <> nil) and not (csReading in ComponentState)
    and HandleAllocated then
      CheckMenuItem(Handle, FCommand, FChecked);
  end;
end;

{------------------------------------------------------------------------------
  Method:  TMenuItem.SetDefault
  Params:  Value:
  Returns: Nothing

  Makes a menuItem the default item (BOLD).
 ------------------------------------------------------------------------------}
procedure TMenuItem.SetDefault(AValue: Boolean);
begin
  FDefault := AValue;
  //TODO: Add runtime code here
end;

{------------------------------------------------------------------------------
  Method:  TMenuItem.SetEnabled
  Params:  Value:
  Returns: Nothing

  Enables a menuItem.
 ------------------------------------------------------------------------------}
procedure TMenuItem.SetEnabled(AValue: Boolean);
begin
  if FEnabled <> AValue then begin
    FEnabled := AValue;
    if HandleAllocated and (Parent <> nil) then
      EnableMenuItem(Handle, FCommand, FEnabled);
    MenuChanged(False);
  end;
end;

{------------------------------------------------------------------------------
  procedure TMenuItem.SetGraphic(const AValue: TGraphic);

  Reposition the MenuItem
 ------------------------------------------------------------------------------}
procedure TMenuItem.SetGraphic(const AValue: TGraphic);
begin
  if FGraphic=AValue then exit;
  FGraphic:=AValue;
  if HandleAllocated then RecreateHandle;
  MenuChanged(False);
end;

{------------------------------------------------------------------------------
  procedure TMenuItem.SetMenuIndex(const AValue: Integer);

  Reposition the MenuItem
 ------------------------------------------------------------------------------}
procedure TMenuItem.SetMenuIndex(AValue: Integer);
var
  OldParent: TMenuItem;
  ParentCount: Integer;
begin
  if FParent <> nil then
  begin
    ParentCount := FParent.Count;
    if AValue < 0 then
      AValue := 0;
    if AValue >= ParentCount then
      AValue := ParentCount - 1;
    if AValue <> MenuIndex then begin
      OldParent := FParent;
      OldParent.Remove(Self);
      OldParent.Insert(AValue, Self);
    end;
  end;
end;

{------------------------------------------------------------------------------
  procedure TMenuItem.SetRadioItem(const AValue: Boolean);

  Sets the 'RadioItem' property of the group of menuitems with the same
  GroupIndex. If RadioItem is true only one menuitem is checked at a time.
 ------------------------------------------------------------------------------}
procedure TMenuItem.SetRadioItem(const AValue: Boolean);
var
  i: integer;
  Item: TMenuItem;
begin
  if FRadioItem <> AValue then
  begin
    FRadioItem := AValue;
    if FChecked and FRadioItem then
      TurnSiblingsOff;
    if (GroupIndex<>0) and (FParent<>nil) then begin
      for I := 0 to FParent.Count - 1 do begin
        Item := FParent[I];
        if (Item <> Self)
        and (Item.GroupIndex = GroupIndex) then
          Item.FRadioItem:=FRadioItem;
      end;
    end;
    if (FParent <> nil) and not (csReading in ComponentState)
    and (HandleAllocated) then
      RadioMenuItemGroup(Handle,FRadioItem);
  end;
end;

{------------------------------------------------------------------------------
  procedure TMenuItem.SetRightJustify(const AValue: boolean);

  Enables a menuItem.
 ------------------------------------------------------------------------------}
procedure TMenuItem.SetRightJustify(const AValue: boolean);
begin
  if FRightJustify=AValue then exit;
  FRightJustify:=AValue;
  if HandleAllocated then
    RightJustifyMenuItem(Handle,RightJustify);
end;

{------------------------------------------------------------------------------
  procedure TMenuItem.SetShowAlwaysCheckable(const AValue: boolean);

  Reserve place for check icon, even if not 'Checked'
 ------------------------------------------------------------------------------}
procedure TMenuItem.SetShowAlwaysCheckable(const AValue: boolean);
var
  OldIsCheckItem: boolean;
begin
  if FShowAlwaysCheckable=AValue then exit;
  OldIsCheckItem:=IsCheckItem;
  FShowAlwaysCheckable:=AValue;
  if (OldIsCheckItem<>IsCheckItem) and (HandleAllocated) then
    RecreateHandle;
end;

{------------------------------------------------------------------------------
  procedure TMenuItem.SetSubMenuImages(const AValue: TCustomImageList);

  Enables a menuItem.
 ------------------------------------------------------------------------------}
procedure TMenuItem.SetSubMenuImages(const AValue: TCustomImageList);
begin
  if FSubMenuImages=AValue then exit;
  // ToDo:
  raise Exception.Create('TMenuItem.SetSubMenuImages: not implemented yet');
  {if FSubMenuImages <> nil then
    FSubMenuImages.UnRegisterChanges(FImageChangeLink);
  FSubMenuImages := Value;
  if FSubMenuImages <> nil then
  begin
    FSubMenuImages.RegisterChanges(FImageChangeLink);
    FSubMenuImages.FreeNotification(Self);
  end;
  UpdateItems;}
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
  Method:  TMenuItem.SetParentComponent
  Params:  Value:
  Returns: Nothing

  Enables a menuItem.
 ------------------------------------------------------------------------------}
Procedure TMenuItem.SetParentComponent(AValue : TComponent);
begin
   if assigned (FParent) and (FParent = AValue)
      then exit;

   if assigned (FParent) and (FParent <> AValue)
      then TMenuItem (FParent).Remove (self);

   if assigned (AValue) then
   begin
      if (AValue is TMenu)
         then TMenu (AValue).Items.Add (Self)
      else if (AValue is TMenuItem)
         then TMenuItem (AValue).Add (Self)
      else
         raise Exception.Create ('TMenuItem.SetParentComponent: suggestet parent not of type TMenu or TMenuItem');
   end;
end;

{------------------------------------------------------------------------------
  Method:  TMenuItem.SetGroupIndex
  Params:  Value: Byte
  Returns: Nothing

  Set the GroupIndex
 ------------------------------------------------------------------------------}
procedure TMenuItem.SetGroupIndex(AValue: Byte);
begin
  if FGroupIndex <> AValue then
  begin
    if Parent <> nil then
      Parent.VerifyGroupIndex(Parent.IndexOf(Self), AValue);
    FGroupIndex := AValue;
    if FChecked and FRadioItem then
      TurnSiblingsOff;
    // tell the interface to regroup this menuitem
    if HandleAllocated then
      RegroupMenuItem(Handle,GroupIndex);
  end;
end;

{------------------------------------------------------------------------------
  Method:  TMenuItem.SetShortCut
  Params:  Value: TShortCut
  Returns: Nothing

  Set the shortcut
 ------------------------------------------------------------------------------}
Procedure TMenuItem.SetShortCut(AValue : TShortCut);
Begin
  if FShortCut <> AValue then
  begin
    ShortCutChanged(FShortcut, AValue);
    FShortCut := AValue;
  end;
end;

{------------------------------------------------------------------------------
  Method:  TMenuItem.SetVisible
  Params:  Value: Visibility
  Returns: Nothing

  Description of the procedure for the class.
 ------------------------------------------------------------------------------}
procedure TMenuItem.SetVisible(AValue: Boolean);
begin
//  HandleNeeded;
  FVisible := AValue;
end;

{------------------------------------------------------------------------------
  Method:  TMenuItem.ShortcutChanged
  Params:  OldValue: Old shortcut, Value: New shortcut
  Returns: Nothing

  Installs a new shortcut, removes an old one.
 ------------------------------------------------------------------------------}
procedure TMenuItem.ShortcutChanged(const OldValue, Value: TShortcut);
var Msg : TLMShortcut;
begin
  if not HandleAllocated then Exit;
  Msg.Handle:= Handle;
  if OldValue = 0 then begin
    Msg.OldKey:= 0;
    Msg.OldModifier:= [];
  end else begin
    ShortCutToKey(OldValue, Msg.OldKey, Msg.OldModifier);
  end;
  if Value = 0 then begin
    Msg.NewKey:= 0;
    Msg.NewModifier:= [];
  end else begin
    ShortCutToKey(Value, Msg.NewKey, Msg.NewModifier);
  end;
  InterfaceObject.IntSendMessage3(LM_SETSHORTCUT, Self, @Msg);
end;

{------------------------------------------------------------------------------
  procedure TMenuItem.SubItemChanged(Sender: TObject; Source: TMenuItem;
    Rebuild: Boolean);

  Is Called whenever one of the childs has changed.
 ------------------------------------------------------------------------------}
procedure TMenuItem.SubItemChanged(Sender: TObject; Source: TMenuItem;
  Rebuild: Boolean);
begin
  if Rebuild and HandleAllocated then
    ; // RebuildHandle;
  if Parent <> nil then
    Parent.SubItemChanged(Self, Source, False)
  else if Owner is TMainMenu then
    TMainMenu(Owner).ItemChanged;
end;

{------------------------------------------------------------------------------
  Method:  TMenuItem.TurnSiblingsOff
  Params:  none
  Returns: Nothing

  Unchecks all siblings.
  In contrary to Delphi this will not use SetChecked, because this is up to the
  interface. This procedure just sets the private variables.
 ------------------------------------------------------------------------------}
procedure TMenuItem.TurnSiblingsOff;
var
  I: Integer;
  Item: TMenuItem;
begin
  if (FParent <> nil) and (GroupIndex<>0) then
    for I := 0 to FParent.Count - 1 do
    begin
      Item := FParent[I];
      if (Item <> Self)
      and Item.FRadioItem and (Item.GroupIndex = GroupIndex) then
        Item.FChecked:=false;
    end;
end;

{------------------------------------------------------------------------------
  Method:  TMenuItem.VerifyGroupIndex
  Params:  Position: Integer; Value: Byte
  Returns: Nothing

  Make sure, that all GroupIndex are in ascending order.
 ------------------------------------------------------------------------------}
procedure TMenuItem.VerifyGroupIndex(Position: Integer; Value: Byte);
var
  i: Integer;
begin
  for i:=0 to GetCount-1 do
    if i<Position then begin
      if Items[i].GroupIndex>Value then
        raise EMenuError.Create('GroupIndex cannot be less than a previous '
                                +'menu item''s GroupIndex')
    end
    else
      { Ripple change to menu items at Position and after }
      if (Items[i].GroupIndex<>0) and (Items[i].GroupIndex<Value) then
        Items[i].FGroupIndex:=Value;
end;

// included by menus.pp


{ =============================================================================

  $Log$
  Revision 1.27  2002/10/08 22:32:27  lazarus
  MG: fixed cool little bug (menu double attaching bug)

  Revision 1.26  2002/08/22 13:45:58  lazarus
  MG: fixed non AutoCheck menuitems and editor bookmark popupmenu

  Revision 1.25  2002/08/15 13:37:57  lazarus
  MG: started menuitem icon, checked, radio and groupindex

  Revision 1.24  2002/08/12 15:32:29  lazarus
  MG: started enhanced menuitem

  Revision 1.23  2002/08/08 17:26:37  lazarus
  MG: added property TMenuItems.RightJustify

  Revision 1.22  2002/08/08 10:33:49  lazarus
  MG: main bar speedbar open arrow now shows recent projects and files

  Revision 1.21  2002/08/08 09:38:36  lazarus
  MG: recent file menus are now updated instantly

  Revision 1.20  2002/08/08 09:07:06  lazarus
  MG: TMenuItem can now be created/destroyed/moved at any time

  Revision 1.19  2002/08/07 09:55:30  lazarus
  MG: codecompletion now checks for filebreaks, savefile now checks for filedate

  Revision 1.18  2002/08/06 20:05:19  lazarus
  MG: added actnlist.pp

  Revision 1.17  2002/08/06 19:57:40  lazarus
  MG: added actnlist.pp

  Revision 1.16  2002/08/05 10:45:03  lazarus
  MG: TMenuItem.Caption can now be set after creation

  Revision 1.15  2002/08/05 08:56:56  lazarus
  MG: TMenuItems can now be enabled and disabled

  Revision 1.14  2002/05/30 21:33:10  lazarus
  + added / fixed streaming functions for TMenu & TMenuItem, stoppok

  Revision 1.13  2002/05/19 08:27:43  lazarus
  + added helper functions to enabled streaming of TMenu /TMenuItem
    stoppok

  Revision 1.12  2002/05/10 06:05:53  lazarus
  MG: changed license to LGPL

  Revision 1.11  2002/05/09 12:41:28  lazarus
  MG: further clientrect bugfixes

  Revision 1.10  2002/03/25 17:59:20  lazarus
  GTK Cleanup
  Shane

  Revision 1.9  2002/02/18 22:46:11  lazarus
  Implented TMenuItem.ShortCut (not much tested).

  Revision 1.8  2001/03/19 14:40:49  lazarus
  MG: fixed many unreleased DC and GDIObj bugs

  Revision 1.6  2001/02/21 22:55:26  lazarus
  small bugfixes + added TOIOptions

  Revision 1.5  2001/01/04 16:12:54  lazarus
  Removed some writelns and changed the property editor for TStrings a bit.
  Shane

  Revision 1.4  2000/12/22 19:55:38  lazarus
  Added the Popupmenu code to the LCL.
  Now you can right click on the editor and a PopupMenu appears.
  Shane

  Revision 1.3  2000/07/30 21:48:32  lazarus
  MWE:
    = Moved ObjectToGTKObject to GTKProc unit
    * Fixed array checking in LoadPixmap
    = Moved LM_SETENABLED to API func EnableWindow and EnableMenuItem
    ~ Some cleanup

  Revision 1.2  2000/07/23 19:01:33  lazarus
  menus will be destroyed now, stoppok

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
  Revision 1.27  2002/10/08 22:32:27  lazarus
  MG: fixed cool little bug (menu double attaching bug)

  Revision 1.26  2002/08/22 13:45:58  lazarus
  MG: fixed non AutoCheck menuitems and editor bookmark popupmenu

  Revision 1.25  2002/08/15 13:37:57  lazarus
  MG: started menuitem icon, checked, radio and groupindex

  Revision 1.24  2002/08/12 15:32:29  lazarus
  MG: started enhanced menuitem

  Revision 1.23  2002/08/08 17:26:37  lazarus
  MG: added property TMenuItems.RightJustify

  Revision 1.22  2002/08/08 10:33:49  lazarus
  MG: main bar speedbar open arrow now shows recent projects and files

  Revision 1.21  2002/08/08 09:38:36  lazarus
  MG: recent file menus are now updated instantly

  Revision 1.20  2002/08/08 09:07:06  lazarus
  MG: TMenuItem can now be created/destroyed/moved at any time

  Revision 1.19  2002/08/07 09:55:30  lazarus
  MG: codecompletion now checks for filebreaks, savefile now checks for filedate

  Revision 1.18  2002/08/06 20:05:19  lazarus
  MG: added actnlist.pp

  Revision 1.17  2002/08/06 19:57:40  lazarus
  MG: added actnlist.pp

  Revision 1.16  2002/08/05 10:45:03  lazarus
  MG: TMenuItem.Caption can now be set after creation

  Revision 1.15  2002/08/05 08:56:56  lazarus
  MG: TMenuItems can now be enabled and disabled

  Revision 1.14  2002/05/30 21:33:10  lazarus
  + added / fixed streaming functions for TMenu & TMenuItem, stoppok

  Revision 1.13  2002/05/19 08:27:43  lazarus
  + added helper functions to enabled streaming of TMenu /TMenuItem
    stoppok

  Revision 1.12  2002/05/10 06:05:53  lazarus
  MG: changed license to LGPL

  Revision 1.11  2002/05/09 12:41:28  lazarus
  MG: further clientrect bugfixes

  Revision 1.10  2002/03/25 17:59:20  lazarus
  GTK Cleanup
  Shane

  Revision 1.9  2002/02/18 22:46:11  lazarus
  Implented TMenuItem.ShortCut (not much tested).

  Revision 1.8  2001/03/19 14:40:49  lazarus
  MG: fixed many unreleased DC and GDIObj bugs

  Revision 1.6  2001/02/21 22:55:26  lazarus
  small bugfixes + added TOIOptions

  Revision 1.5  2001/01/04 16:12:54  lazarus
  Removed some writelns and changed the property editor for TStrings a bit.
  Shane

  Revision 1.4  2000/12/22 19:55:38  lazarus
  Added the Popupmenu code to the LCL.
  Now you can right click on the editor and a PopupMenu appears.
  Shane

  Revision 1.3  2000/07/30 21:48:32  lazarus
  MWE:
    = Moved ObjectToGTKObject to GTKProc unit
    * Fixed array checking in LoadPixmap
    = Moved LM_SETENABLED to API func EnableWindow and EnableMenuItem
    ~ Some cleanup

  Revision 1.2  2000/07/23 19:01:33  lazarus
  menus will be destroyed now, stoppok

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
