{%MainUnit ../menus.pp}

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
  procedure TMenuItem.AddSeparator;
 ------------------------------------------------------------------------------}
procedure TMenuItem.AddSeparator;
var
  Item: TMenuItem;
begin
  Item:=TMenuItem.Create(Self);
  Item.Caption:='-';
  Add(Item);
end;

{------------------------------------------------------------------------------
  procedure TMenuItem.Click;
  
  Call hooks and actions.
 ------------------------------------------------------------------------------}
procedure TMenuItem.Click;
begin
  if Enabled then
  begin
    if Assigned(OnMenuPopupHandler) then OnMenuPopupHandler(Self);

    if (not Assigned(ActionLink) and AutoCheck) or (Assigned(ActionLink) and
        not (ActionLink.IsAutoCheckLinked) and AutoCheck)
    then begin
      // Break a little Delphi compatibility
      // It makes no sense to uncheck a checked RadioItem (besides, GTK can't handle it)
      if (not RadioItem) or (not Checked) then
        Checked := not Checked;
    end;

    { Call OnClick if assigned and not equal to associated action's OnExecute.
      If associated action's OnExecute assigned then call it, otherwise, call
      OnClick. }
    if Assigned(FOnClick)
    and (Action <> nil) and (FOnClick <> Action.OnExecute) then
      FOnClick(Self)
    else if not (csDesigning in ComponentState) and (ActionLink <> nil) then
      FActionLink.Execute(Self)
    else if Assigned(FOnClick) then
      FOnClick(Self);
  end;
end;

{------------------------------------------------------------------------------
  Method: TMenuItem.Create
  Params:  TheOwner: the owner of the class
  Returns: Nothing

  Constructor for the class.
 ------------------------------------------------------------------------------}
constructor TMenuItem.Create(TheOwner: TComponent);
begin
  //DebugLn('TMenuItem.Create START TheOwner=',TheOwner.Name,':',TheOwner.ClassName);
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
  FCommand := UniqueCommand;
  FImageIndex := -1;

  FImageChangeLink := TChangeLink.Create;
  FImageChangeLink.OnChange := @ImageListChange;
  //DebugLn('TMenuItem.Create END TheOwner=',TheOwner.Name,':',TheOwner.ClassName);
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
  //DebugLn('TMenuItem.CreateHandle START ',Name,':',ClassName);
  if not FVisible then RaiseGDBException('');
  Handle := TWSMenuItemClass(WidgetSetClass).CreateHandle(Self);
  if FItems<>nil then begin
    for i := 0 to Count - 1 do begin
      if Items[i].Visible then
        Items[i].HandleNeeded;
    end;
  end;
  if Parent <> nil then
  begin
    Parent.HandleNeeded;
    //DebugLn('TMenuItem.CreateHandle Attaching ... ',Name,':',ClassName);
    if Parent.HandleAllocated then
      TWSMenuItemClass(WidgetSetClass).AttachMenu(Self);
  end;
  if (Parent<>nil) then
  begin
    if HandleAllocated then begin
      if ShortCut <> 0 then ShortCutChanged(0, Shortcut);
    end;
  end;
  //DebugLn('TMenuItem.CreateHandle END ',Name,':',ClassName);
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
  if FBitmap<>nil then
    FreeAndNil(FBitmap);
  DestroyHandle;
  if assigned (FItems) then begin
    i := FItems.Count-1;
    while i>=0 do begin
      TMenuItem(FItems[i]).Free;
      dec(i);
    end;
  end;
  FreeAndNil(FItems);
  FreeAndNil(FActionLink);
  FreeAndNil(FImageChangeLink);
  if FParent<>nil then
    FParent.FItems.Remove(Self);
  if FCommand <> 0 then CommandPool[FCommand] := False;
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
  if (FParent <> nil) and (FParent.FMenu<>nil) then
    Result := FParent.FMenu
  else
    Result := FParent;
end;

{------------------------------------------------------------------------------
  Method:  TMenuItem.DoClicked
 ------------------------------------------------------------------------------}
procedure TMenuItem.DoClicked(var msg);
begin
  if not (csDesigning in ComponentState) then
    Click
  else if Assigned(DesignerMenuItemClick) then
    DesignerMenuItemClick(Self);
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

function TMenuItem.GetAction: TBasicAction;
begin
  if FActionLink <> nil then
    Result := FActionLink.Action
  else
    Result := nil;
end;

procedure TMenuItem.SetAction(NewAction: TBasicAction);
begin
  if NewAction = nil then begin
    FActionLink.Free;
    FActionLink := nil;
  end else begin
    if FActionLink = nil then
      FActionLink := GetActionLinkClass.Create(Self);
    FActionLink.Action := NewAction;
    FActionLink.OnChange := @DoActionChange;
    ActionChange(NewAction, csLoading in NewAction.ComponentState);
    NewAction.FreeNotification(Self);
  end;
end;

procedure TMenuItem.InitiateActions;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
    Items[i].InitiateAction;
end;

procedure TMenuItem.ActionChange(Sender: TObject; CheckDefaults: Boolean);
var
  NewAction: TCustomAction;
begin
  if Sender is TCustomAction then begin
    NewAction:=TCustomAction(Sender);
    if (not CheckDefaults) or (AutoCheck = False) then
      AutoCheck := NewAction.AutoCheck;
    if (not CheckDefaults) or (Caption = '') then
      Caption := NewAction.Caption;
    if (not CheckDefaults) or (Checked = False) then
      Checked := NewAction.Checked;
    if (not CheckDefaults) or (Enabled = True) then
      Enabled := NewAction.Enabled;
    if (not CheckDefaults) or (HelpContext = 0) then
      HelpContext := NewAction.HelpContext;
    if (not CheckDefaults) or (Hint = '') then
      Hint := NewAction.Hint;
    if RadioItem and (not CheckDefaults or (GroupIndex = 0)) then
      GroupIndex := NewAction.GroupIndex;
    if (not CheckDefaults) or (ImageIndex = -1) then
      ImageIndex := NewAction.ImageIndex;
    if (not CheckDefaults) or (ShortCut = scNone) then
      ShortCut := NewAction.ShortCut;
    if (not CheckDefaults) or (Visible = True) then
      Visible := NewAction.Visible;
    if (not CheckDefaults) or not Assigned(OnClick) then
      OnClick := NewAction.OnExecute;
  end;
end;

function TMenuItem.GetActionLinkClass: TMenuActionLinkClass;
begin
  Result := TMenuActionLink;
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

function TMenuItem.GetBitmap: TBitmap;
begin
  if FBitmap=nil then
    FBitmap:=TBitmap.Create;
  FBitmap.Transparent:=True;
  Result:=FBitmap;
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
    raise EMenuError.CreateFmt(rsIndexOutOfBounds,[ClassName,Index,0]);
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

function TMenuItem.IsBitmapStored: boolean;
begin
  Result:=(FBitmap<>nil) and (not FBitmap.Empty)
          and (FBitmap.Width>0) and (FBitmap.Height>0);
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

  Checks if 'Enabled' needs to be saved to stream
 ------------------------------------------------------------------------------}
function TMenuItem.IsEnabledStored: boolean;
begin
  Result := (ActionLink = nil) or not FActionLink.IsEnabledLinked;
end;

function TMenuItem.IsHelpContextStored: boolean;
begin
  Result := (ActionLink = nil) or not FActionLink.IsHelpContextLinked;
end;

function TMenuItem.IsHintStored: Boolean;
begin
  Result := (ActionLink = nil) or not FActionLink.IsHintLinked;
end;

function TMenuItem.IsImageIndexStored: Boolean;
begin
  Result := (ActionLink = nil) or not FActionLink.IsImageIndexLinked;
end;

function TMenuItem.IsOnClickStored: Boolean;
begin
  Result := (ActionLink = nil) or not FActionLink.IsOnExecuteLinked;
end;

{------------------------------------------------------------------------------
  function TMenuItem.IsShortCutStored: boolean;

  Checks if 'ShotCut' needs to be saved to stream
 ------------------------------------------------------------------------------}
function TMenuItem.IsShortCutStored: boolean;
begin
  Result := (ActionLink = nil) or not FActionLink.IsShortCutLinked;
end;

{------------------------------------------------------------------------------
  function TMenuItem.IsVisibleStored: boolean;

  Checks if 'Visible' needs to be saved to stream
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


 ------------------------------------------------------------------------------}
function TMenuItem.GetParentMenu: TMenu;
var
  Item: TMenuItem;
begin
  Item := Self;
  while Item.Parent <> nil do Item := Item.Parent;
  Result := Item.FMenu;
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
  Result:=(FBitmap<>nil) or ((ImageIndex>=0) and (GetImageList<>nil));
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
  TWSMenuItemClass(WidgetSetClass).DestroyHandle(Self);
  FHandle:=0;
end;

procedure TMenuItem.Loaded;
begin
  inherited Loaded;
  if Action <> nil then ActionChange(Action, True);
end;

procedure TMenuItem.Notification(AComponent: TComponent; Operation: TOperation
  );
begin
  inherited Notification(AComponent, Operation);
  if Operation = opRemove then
    if AComponent = Action then
      Action := nil
    else if AComponent = FSubMenuImages then
      SubMenuImages := nil
    {else if AComponent = FMerged then
      MergeWith(nil)};
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

procedure TMenuItem.InitiateAction;
begin
  if FActionLink <> nil then FActionLink.Update;
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
    RaiseGDBException('Menu inserted twice');

  // create Items if needed
  if FItems = nil then FItems := TList.Create;

  // adjust GroupIndex
  (*
   *  MWE: Disabled this feature, it makes not much sense
   *  suppose a menu with items grouped like : G=2, G=2, ---, G=1, G=1
   *  where --- is separator with G=0
   *  Inserting G=1 after --- is OK according to the next check

  if (Index>0) and (Index < FItems.Count) then
    if Item.GroupIndex < TMenuItem(FItems[Index - 1]).GroupIndex then
      Item.GroupIndex := TMenuItem(FItems[Index - 1]).GroupIndex;
  VerifyGroupIndex(Index, Item.GroupIndex);
  *)

  Item.FParent := Self;
  Item.FOnChange := @SubItemChanged;
  FItems.Insert(Index, Item);

  if HandleAllocated then begin
    Item.HandleNeeded;
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

  Returns the index of the menuitem with the given caption or -1
 ------------------------------------------------------------------------------}
function TMenuItem.IndexOfCaption(const ACaption: string): Integer;
begin
  for Result:=0 to Count-1 do
    if Items[Result].Caption=ACaption then exit;
  Result:=-1;
end;

{------------------------------------------------------------------------------
  function TMenuItem.VisibleIndexOf(Item: TMenuItem): Integer;

  Returns the index of the menuitem of all visible menuitems
 ------------------------------------------------------------------------------}
function TMenuItem.VisibleIndexOf(Item: TMenuItem): Integer;

  procedure RaiseVisibleInconsistency;
  begin
    raise Exception.Create('TMenuItem.VisibleIndexOf '+dbgsName(Item)+' is visible, but not in parents list');
  end;

var
  i: Integer;
  CurMenuItem: TMenuItem;
begin
  if (FItems = nil) or (Item=nil) or (not Item.Visible) then
    Result := -1
  else begin
    Result:=0;
    i:=0;
    while (i<FItems.Count) do begin
      CurMenuItem:=TMenuItem(FItems[i]);
      if CurMenuItem.Visible then begin
        if CurMenuItem=Item then exit;
        inc(Result);
      end;
      inc(i);
    end;
    Result:=-1;
    if Item.Visible then RaiseVisibleInconsistency;
  end;
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
  function TMenuItem.IsInMenuBar: boolean;
 ------------------------------------------------------------------------------}
function TMenuItem.IsInMenuBar: boolean;
begin
  Result:=(FParent<>nil) and (FParent.FMenu<>nil)
          and (FParent.FMenu is TMainMenu);
end;

{------------------------------------------------------------------------------
  procedure TMenuItem.Clear;
  
  Deletes all childs
 ------------------------------------------------------------------------------}
procedure TMenuItem.Clear;
var
  I: Integer;
begin
  for I := Count - 1 downto 0 do
    Items[I].Free;
end;

function TMenuItem.HasBitmap: boolean;
begin
  Result:=FBitmap<>nil;
end;

{------------------------------------------------------------------------------
  function TMenuItem.GetIconSize: TPoint;
 ------------------------------------------------------------------------------}
function TMenuItem.GetIconSize: TPoint;
var
  TheImgList: TCustomImageList;
begin
  Result:=Point(0,0);
  if FBitmap<>nil then begin
    Result:=Point(FBitmap.Width,FBitmap.Height);
  end else if (FImageIndex>=0) then begin
    TheImgList:=GetImageList;
    if (TheImgList<>nil) and (FImageIndex<TheImgList.Count) then
      Result:=Point(TheImgList.Width,TheImgList.Height);
  end;
end;

function TMenuItem.MenuVisibleIndex: integer;
begin
  Result:=-1;
  if Parent=nil then
    Result:=-1
  else
    Result:=Parent.VisibleIndexOf(Self);
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
    TWSMenuItemClass(WidgetSetClass).SetCaption(Self, AValue);
  OwnerFormDesignerModified(Self);
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
    if (FParent <> nil)
    and not (csReading in ComponentState)
    and HandleAllocated
    then TWSMenuItemClass(WidgetSetClass).SetCheck(Self, AValue);
    OwnerFormDesignerModified(Self);
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
    if HandleAllocated and (Parent <> nil)
    then TWSMenuItemClass(WidgetSetClass).SetEnable(Self, AValue);
    MenuChanged(False);
  end;
end;

{------------------------------------------------------------------------------
  procedure TMenuItem.SetBitmap(const AValue: TBitmap);

  Reposition the MenuItem
 ------------------------------------------------------------------------------}
procedure TMenuItem.SetBitmap(const AValue: TBitmap);
begin
  if FBitmap=AValue then exit;
  if AValue<>nil then begin
    Bitmap.Assign(AValue);
  end else begin
    FBitmap.Free;
    FBitmap:=nil;
  end;
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
    TWSMenuItemClass(WidgetSetClass).SetRightJustify(Self, AValue);
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

  Sets the new sub images list
 ------------------------------------------------------------------------------}
procedure TMenuItem.SetSubMenuImages(const AValue: TCustomImageList);
begin
  if FSubMenuImages <> nil then
    FSubMenuImages.UnRegisterChanges(FImageChangeLink);
  FSubMenuImages := AValue;
  if FSubMenuImages <> nil then
  begin
    FSubMenuImages.RegisterChanges(FImageChangeLink);
    FSubMenuImages.FreeNotification(Self);
  end;
  UpdateImages;
  if HandleAllocated and ([csLoading,csDestroying]*ComponentState=[]) then begin
    // ToDo: Update images
  end;
end;

{------------------------------------------------------------------------------
  Method:  TMenuItem.SetImageIndex
  Params:  Value:
  Returns: Nothing

  Enables a menuItem.
 ------------------------------------------------------------------------------}
procedure TMenuItem.SetImageIndex(Value: Integer);
begin
  if FImageIndex = Value then exit;
  //debugln('TMenuItem.SetImageIndex A ',Name,' Old=',FImageIndex,' New=',Value);
  FImageIndex := Value;
  //TODO: TMenuItem.SetImageIndex
end;

{------------------------------------------------------------------------------
  Method:  TMenuItem.SetParentComponent
  Params:  Value:
  Returns: Nothing

  Enables a menuItem.
 ------------------------------------------------------------------------------}
Procedure TMenuItem.SetParentComponent(AValue : TComponent);
begin
  if (FParent = AValue) then exit;

  if Assigned(FParent) then
    FParent.Remove(Self);

  if assigned (AValue) then
  begin
    if (AValue is TMenu)
      then TMenu(AValue).Items.Add(Self)
    else if (AValue is TMenuItem)
      then TMenuItem(AValue).Add(Self)
    else
      raise Exception.Create('TMenuItem.SetParentComponent: suggested parent not of type TMenu or TMenuItem');
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
    (*
     *  MWE: Disabled this feature, it makes not much sense
     *  See other comments
    if Parent <> nil then
      Parent.VerifyGroupIndex(Parent.IndexOf(Self), AValue);
    *)
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
Procedure TMenuItem.SetShortCut(const AValue : TShortCut);
Begin
  if FShortCut <> AValue then begin
    ShortCutChanged(FShortCut, AValue);
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
  if FVisible=AValue then exit;
  //debugln('TMenuItem.SetVisible ',dbgsname(Self),' NewValue=',dbgs(AValue),' HandleAllocated=',dbgs(HandleAllocated));
  if ([csDestroying]*ComponentState<>[]) then exit;
  if AValue then begin
    FVisible := AValue;
    if (not (csLoading in ComponentState)) and Parent.HandleAllocated then
      HandleNeeded;
    if HandleAllocated then
      TWSMenuItemClass(WidgetSetClass).SetVisible(Self,true);
  end else begin
    if HandleAllocated then begin
      TWSMenuItemClass(WidgetSetClass).SetVisible(Self,false);
      DestroyHandle;
    end;
    FVisible := AValue;
  end;
end;

procedure TMenuItem.UpdateImages;
begin
  if HandleAllocated and ([csLoading,csDestroying]*ComponentState=[]) then begin
    // ToDo: Update images
  end;
end;

procedure TMenuItem.ImageListChange(Sender: TObject);
begin
  if Sender = SubMenuImages then UpdateImages;
end;

{------------------------------------------------------------------------------
  Method:  TMenuItem.ShortcutChanged
  Params:  OldValue: Old shortcut, Value: New shortcut
  Returns: Nothing

  Installs a new shortcut, removes an old one.
 ------------------------------------------------------------------------------}
procedure TMenuItem.ShortcutChanged(const OldValue, Value: TShortcut);
begin
  if HandleAllocated then
    TWSMenuItemClass(WidgetSetClass).SetShortCut(Self, OldValue, Value);
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

  //todo
  MWE: ??? shouln'd we get checked from the interface in that case ???
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
      and Item.FRadioItem and (Item.GroupIndex = GroupIndex)
      then Item.FChecked := false;
    end;
end;

procedure TMenuItem.DoActionChange(Sender: TObject);
begin
  if Sender=Action then ActionChange(Sender,False);
end;

procedure TMenuItem.AssignTo(Dest: TPersistent);
begin
  if Dest is TCustomAction then begin
    with TCustomAction(Dest) do
    begin
      Caption := Self.Caption;
      Enabled := Self.Enabled;
      HelpContext := Self.HelpContext;
      Hint := Self.Hint;
      ImageIndex := Self.ImageIndex;
      OnExecute := Self.OnClick;
      Visible := Self.Visible;
    end
  end else
    inherited AssignTo(Dest);
end;

// included by menus.pp


{ =============================================================================

  $Log$
  Revision 1.69  2005/07/01 12:07:49  mattias
  moved some menu intf methods to the TXXXWSMenuItem class  from Martin Smat

  Revision 1.68  2005/06/08 08:02:09  mattias
  added message parts to Help interface

  Revision 1.67  2005/05/21 15:58:44  mattias
  implemented right justification for menuitems for winapi intf  from Martin Smat

  Revision 1.66  2005/05/06 07:52:58  micha
  fix spelling

  Revision 1.65  2004/12/10 21:36:27  mattias
  implemented TMenuItem.SetVisible

  Revision 1.64  2004/09/24 21:34:14  micha
  convert LM_CREATE message to interface methods
  remove SendMsgToInterface, CNSendMessage and related methods
  remove TWidgetSet.IntSendMessage3; all LCL to interface messages have been converted

  Revision 1.63  2004/09/18 12:43:15  micha
  convert LM_DESTROY message to interface methods

  Revision 1.62  2004/09/17 10:56:25  micha
  convert LM_SHORTCUT message to interface methods

  Revision 1.61  2004/09/10 18:58:21  micha
  convert LM_ATTACHMENU to interface method

  Revision 1.60  2004/09/10 09:43:13  micha
  convert LM_SETLABEL message to interface methods

  Revision 1.59  2004/08/13 10:20:19  mattias
  fixed codetools ConstSet, implemented notifying TApplication whenmenu popups

  Revision 1.58  2004/06/24 17:45:33  mattias
  fixed TMenuItem.GetIconSize

  Revision 1.57  2004/06/17 21:24:19  mattias
  implemented painting menuitem icons from ImageList

  Revision 1.56  2004/06/17 20:52:19  mattias
  fixed setting ImageIndex when TMenuItem.ActionChange

  Revision 1.55  2004/05/11 12:16:47  mattias
  replaced writeln by debugln

  Revision 1.54  2004/04/10 17:58:57  mattias
  implemented mainunit hints for include files

  Revision 1.53  2004/02/23 08:19:04  micha
  revert intf split

  Revision 1.51  2004/02/08 11:31:32  mattias
  TMenuItem.Bitmap is now auto created on read. Added TMenuItem.HasBitmap

  Revision 1.50  2004/02/05 09:45:33  mattias
  implemented Actions for TSpeedButton, TMenuItem, TCheckBox

  Revision 1.49  2004/02/04 17:06:26  mattias
  fixed updating menu designer caption when editing in OI

  Revision 1.48  2004/02/04 13:55:08  mattias
  implemented generic function to hide TComponents without page in designer

  Revision 1.47  2004/02/04 13:40:19  mattias
  ShortCutToText now deletes any modifier

  Revision 1.46  2004/02/02 18:09:41  mattias
  added TMenuItem.Action

  Revision 1.45  2004/01/11 16:38:29  marc
  * renamed (Check|Enable)MenuItem to MenuItemSet(Check|Enable)
  + Started with accelerator nameing routines
  * precheckin for createwidget splitup

  Revision 1.44  2004/01/10 18:09:38  mattias
  implemented TMenuItem.Clear

  Revision 1.43  2003/11/26 00:23:47  marc
  * implemented new LCL(check|enable)Menuitem functions
  * introduced the lclintf inc files to win32

  Revision 1.42  2003/11/25 14:21:28  micha
  new api lclenable,checkmenuitem according to list

  Revision 1.41  2003/11/24 11:03:07  marc
  * Splitted winapi*.inc into a winapi and a lcl interface communication part

  Revision 1.40  2003/11/16 01:56:15  mattias
  changed TMenuItem.Graphic to TMenuItem.Bitmap

  Revision 1.39  2003/10/22 17:50:16  mattias
  updated rpm scripts

  Revision 1.38  2003/07/21 23:43:32  marc
  * Fixed radiogroup menuitems

  Revision 1.37  2003/07/01 09:29:51  mattias
  attaching menuitems topdown

  Revision 1.36  2003/06/26 17:00:00  mattias
  fixed result on searching proc in interface

  Revision 1.35  2003/06/24 15:57:55  mattias
  applied win32 menu patch from Micha Nelissen

  Revision 1.34  2003/05/18 10:42:58  mattias
  implemented deleting empty submenus

  Revision 1.33  2003/04/19 18:37:58  mattias
  implemented adding OnClick, when clicking on designer menuitem

  Revision 1.32  2003/03/16 09:41:05  mattias
  fixed checking menuitems

  Revision 1.31  2003/03/11 07:46:43  mattias
  more localization for gtk- and win32-interface and lcl

  Revision 1.30  2003/02/23 10:42:06  mattias
  implemented changing TMenuItem.GroupIndex at runtime

  Revision 1.29  2002/10/26 15:15:48  lazarus
  MG: broke LCL<->interface circles

  Revision 1.28  2002/10/22 18:54:56  lazarus
  MG: fixed menu streaming

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
  Revision 1.69  2005/07/01 12:07:49  mattias
  moved some menu intf methods to the TXXXWSMenuItem class  from Martin Smat

  Revision 1.68  2005/06/08 08:02:09  mattias
  added message parts to Help interface

  Revision 1.67  2005/05/21 15:58:44  mattias
  implemented right justification for menuitems for winapi intf  from Martin Smat

  Revision 1.66  2005/05/06 07:52:58  micha
  fix spelling

  Revision 1.65  2004/12/10 21:36:27  mattias
  implemented TMenuItem.SetVisible

  Revision 1.64  2004/09/24 21:34:14  micha
  convert LM_CREATE message to interface methods
  remove SendMsgToInterface, CNSendMessage and related methods
  remove TWidgetSet.IntSendMessage3; all LCL to interface messages have been converted

  Revision 1.63  2004/09/18 12:43:15  micha
  convert LM_DESTROY message to interface methods

  Revision 1.62  2004/09/17 10:56:25  micha
  convert LM_SHORTCUT message to interface methods

  Revision 1.61  2004/09/10 18:58:21  micha
  convert LM_ATTACHMENU to interface method

  Revision 1.60  2004/09/10 09:43:13  micha
  convert LM_SETLABEL message to interface methods

  Revision 1.59  2004/08/13 10:20:19  mattias
  fixed codetools ConstSet, implemented notifying TApplication whenmenu popups

  Revision 1.58  2004/06/24 17:45:33  mattias
  fixed TMenuItem.GetIconSize

  Revision 1.57  2004/06/17 21:24:19  mattias
  implemented painting menuitem icons from ImageList

  Revision 1.56  2004/06/17 20:52:19  mattias
  fixed setting ImageIndex when TMenuItem.ActionChange

  Revision 1.55  2004/05/11 12:16:47  mattias
  replaced writeln by debugln

  Revision 1.54  2004/04/10 17:58:57  mattias
  implemented mainunit hints for include files

  Revision 1.53  2004/02/23 08:19:04  micha
  revert intf split

  Revision 1.51  2004/02/08 11:31:32  mattias
  TMenuItem.Bitmap is now auto created on read. Added TMenuItem.HasBitmap

  Revision 1.50  2004/02/05 09:45:33  mattias
  implemented Actions for TSpeedButton, TMenuItem, TCheckBox

  Revision 1.49  2004/02/04 17:06:26  mattias
  fixed updating menu designer caption when editing in OI

  Revision 1.48  2004/02/04 13:55:08  mattias
  implemented generic function to hide TComponents without page in designer

  Revision 1.47  2004/02/04 13:40:19  mattias
  ShortCutToText now deletes any modifier

  Revision 1.46  2004/02/02 18:09:41  mattias
  added TMenuItem.Action

  Revision 1.45  2004/01/11 16:38:29  marc
  * renamed (Check|Enable)MenuItem to MenuItemSet(Check|Enable)
  + Started with accelerator nameing routines
  * precheckin for createwidget splitup

  Revision 1.44  2004/01/10 18:09:38  mattias
  implemented TMenuItem.Clear

  Revision 1.43  2003/11/26 00:23:47  marc
  * implemented new LCL(check|enable)Menuitem functions
  * introduced the lclintf inc files to win32

  Revision 1.42  2003/11/25 14:21:28  micha
  new api lclenable,checkmenuitem according to list

  Revision 1.41  2003/11/24 11:03:07  marc
  * Splitted winapi*.inc into a winapi and a lcl interface communication part

  Revision 1.40  2003/11/16 01:56:15  mattias
  changed TMenuItem.Graphic to TMenuItem.Bitmap

  Revision 1.39  2003/10/22 17:50:16  mattias
  updated rpm scripts

  Revision 1.38  2003/07/21 23:43:32  marc
  * Fixed radiogroup menuitems

  Revision 1.37  2003/07/01 09:29:51  mattias
  attaching menuitems topdown

  Revision 1.36  2003/06/26 17:00:00  mattias
  fixed result on searching proc in interface

  Revision 1.35  2003/06/24 15:57:55  mattias
  applied win32 menu patch from Micha Nelissen

  Revision 1.34  2003/05/18 10:42:58  mattias
  implemented deleting empty submenus

  Revision 1.33  2003/04/19 18:37:58  mattias
  implemented adding OnClick, when clicking on designer menuitem

  Revision 1.32  2003/03/16 09:41:05  mattias
  fixed checking menuitems

  Revision 1.31  2003/03/11 07:46:43  mattias
  more localization for gtk- and win32-interface and lcl

  Revision 1.30  2003/02/23 10:42:06  mattias
  implemented changing TMenuItem.GroupIndex at runtime

  Revision 1.29  2002/10/26 15:15:48  lazarus
  MG: broke LCL<->interface circles

  Revision 1.28  2002/10/22 18:54:56  lazarus
  MG: fixed menu streaming

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
