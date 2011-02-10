{%MainUnit ../menus.pp}

{******************************************************************************
                                  TMenuItem
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
  Item := TMenuItem.Create(Self);
  Item.Caption := cLineCaption;
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
        not (ActionLink.IsAutoCheckLinked) and AutoCheck) then
    begin
      // Break a little Delphi compatibility
      // It makes no sense to uncheck a checked RadioItem (besides, GTK can't handle it)
      if (not RadioItem) or (not Checked) then
        Checked := not Checked;
    end;

    { Call OnClick if assigned and not equal to associated action's OnExecute.
      If associated action's OnExecute assigned then call it, otherwise, call
      OnClick. }
    if Assigned(FOnClick) and (Action <> nil) and (FOnClick <> Action.OnExecute) then
      FOnClick(Self)
    else
    if not (csDesigning in ComponentState) and (ActionLink <> nil) then
      FActionLink.Execute(Self)
    else
    if Assigned(FOnClick) then
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
  //if not assigned (TheOwner) then debugln ('**SH: Warn: creating MenuItem with Owner = nil');

  inherited Create(TheOwner);

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
  FBitmapIsValid := True;
  FRightJustify := False;
  FShowAlwaysCheckable := False;
  FGlyphShowMode := gsmApplication;

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
begin
  //DebugLn('TMenuItem.CreateHandle ',dbgsName(Self),' ',dbgs(Self));
  //DebugLn('TMenuItem.CreateHandle START ',Name,':',ClassName);
  if not FVisible then RaiseGDBException('');
  Handle := TWSMenuItemClass(WidgetSetClass).CreateHandle(Self);
  CheckChildrenHandles;

  if Parent <> nil then
  begin
    Parent.HandleNeeded;
    //DebugLn('TMenuItem.CreateHandle Attaching ... ',Name,':',ClassName);
    if Parent.HandleAllocated then
      TWSMenuItemClass(WidgetSetClass).AttachMenu(Self);

    if HandleAllocated then
    begin
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
  if Cur = nil then
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
  HandlerType: TMenuItemHandlerType;
begin
  //debugln('TMenuItem.Destroy A ',dbgsName(Self));
  FMenuItemHandlers[mihtDestroy].CallNotifyEvents(Self);
  if FBitmap <> nil then
    FreeAndNil(FBitmap);
  DestroyHandle;
  if Assigned(FItems) then
  begin
    i := FItems.Count-1;
    while i >= 0 do
    begin
      TMenuItem(FItems[i]).Free;
      Dec(i);
    end;
  end;
  FreeAndNil(FItems);
  FreeAndNil(FActionLink);
  FreeAndNil(FImageChangeLink);
  for HandlerType:= low(TMenuItemHandlerType) to high(TMenuItemHandlerType) do
    FreeAndNil(FMenuItemHandlers[HandlerType]);
  if FParent <> nil then
    FParent.FItems.Remove(Self);
  if FCommand <> 0 then CommandPool[FCommand] := False;
  //debugln('TMenuItem.Destroy B ',dbgsName(Self));
  inherited Destroy;
end;


{ Find the menu item with a Caption of ACaption. Also for compatability with
  Delphi. }
function TMenuItem.Find(const ACaption: string): TMenuItem;
var
  Idx: Integer;
begin
  Result := nil;
  Idx := IndexOfCaption(ACaption);
  if Idx <> -1 then
    Result := Items[Idx];
end;

function TMenuItem.GetEnumerator: TMenuItemEnumerator;
begin
  Result := TMenuItemEnumerator.Create(Self);
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
 ------------------------------------------------------------------------------}
procedure TMenuItem.DoClicked(var msg);
begin
  CheckChildrenHandles;
  if not (csDesigning in ComponentState) then
  begin
    InitiateActions;
    Click;
  end
  else
  if Assigned(DesignerMenuItemClick) then
    DesignerMenuItemClick(Self);
end;

procedure TMenuItem.CheckChildrenHandles;

  function GetMenu(Item: TMenuItem): TMenu;
  begin
    repeat
      Result := Item.Menu;
      Item := Item.Parent;
    until (Result <> nil) or (Item = nil);
  end;

var
  i: Integer;
  AMenu: TMenu;
begin
  if FItems <> nil then
  begin
    AMenu := GetMenu(Self);
    for i := 0 to Count - 1 do
    begin
      if Items[i].Visible then
      begin
        if Items[i].HandleAllocated and (GetMenu(Items[i]) <> AMenu) then
          Items[i].DestroyHandle;
        Items[i].HandleNeeded;
      end;
    end;
  end;
end;

procedure TMenuItem.IntfDoSelect;
begin
  Application.Hint := GetLongHint(Hint);
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
begin
  if not assigned (FItems) then exit;

  for i := 0 to FItems.Count - 1 do
    Proc(TComponent(FItems[i]));
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
  if NewAction = nil then
  begin
    FActionLink.Free;
    FActionLink := nil;
  end else
  begin
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
  if Sender is TCustomAction then
  begin
    NewAction := TCustomAction(Sender);
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
var
  iml: TCustomImageList;
begin
  if FBitmap = nil then
  begin
    FBitmap := TBitmap.Create;

    if ImageIndex >= 0 then
    begin
      iml := GetImageList;
      if (iml <> nil) and (ImageIndex < iml.Count) then
        iml.GetBitmap(ImageIndex, FBitmap);
    end;
    
    FBitmap.OnChange := @BitmapChange;
  end;

  Result := FBitmap;
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
    raise EMenuError.CreateFmt(rsIndexOutOfBounds,[ClassName,Index,-1]);
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
  Result :=
    FBitmapIsValid and
    (FBitmap <> nil) and (not FBitmap.Empty) and
    (FBitmap.Width > 0) and (FBitmap.Height > 0);
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
  if FAutoCheck = AValue then exit;
  OldIsCheckItem := IsCheckItem;
  FAutoCheck := AValue;
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
  Function: TMenuItem.GetIsRightToLeft
  Returns:  Get IsRightToLeft value from Menu
  
 ------------------------------------------------------------------------------}

function TMenuItem.GetIsRightToLeft: Boolean;
var
  LMenu:TMenu;
begin
  LMenu := GetParentMenu;
  Result := (LMenu <> nil) and (LMenu.IsRightToLeft);
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

  function CanShowIcon: Boolean;

    function SystemShowGlyphs: Boolean; inline;
    begin
      Result := ThemeServices.GetOption(toShowMenuImages) = 1;
    end;

  begin
    if csDesigning in ComponentState then
      Exit(True);
    case GlyphShowMode of
      gsmAlways:
        Result := True;
      gsmNever:
        Result := False;
      gsmApplication:
        begin
          case Application.ShowMenuGlyphs of
            sbgAlways: Result := True;
            sbgNever: Result := False;
            sbgSystem: Result := SystemShowGlyphs;
          end;
        end;
      gsmSystem:
        Result := SystemShowGlyphs;
    end;
  end;

var
  AImageList: TCustomImageList;
begin
  Result := CanShowIcon;
  if not Result then
    Exit;
  AImageList := GetImageList;
  Result := (AImageList <> nil) and (ImageIndex >= 0) and (ImageIndex < AImageList.Count);
  if not Result then
    Result := (FBitmap <> nil) and not FBitmap.Empty;
end;

{------------------------------------------------------------------------------
  procedure TMenuItem.DestroyHandle;

  Free the Handle
 ------------------------------------------------------------------------------}
procedure TMenuItem.DestroyHandle;
var
  i: integer;
begin
  if not HandleAllocated then Exit;
  //DebugLn('TMenuItem.DestroyHandle ',dbgsName(Self),' ',dbgs(Self));
  if Assigned(FItems) then
  begin
    i := FItems.Count - 1;
    while i >= 0 do
    begin
      TMenuItem(FItems[i]).DestroyHandle;
      dec(i);
    end;
  end;
  TWSMenuItemClass(WidgetSetClass).DestroyHandle(Self);
  FHandle := 0;
end;

procedure TMenuItem.Loaded;
begin
  inherited Loaded;
  if Action <> nil then ActionChange(Action, True);
end;

procedure TMenuItem.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if Operation = opRemove then
    if AComponent = Action then
      Action := nil
    else
    if AComponent = FSubMenuImages then
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
  if not HandleAllocated then Exit;
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
  Result := Assigned(FParent);
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

  if HandleAllocated and Item.Visible then
    Item.HandleNeeded;
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
  if FItems = nil then
    Result := -1
  else
    Result := FItems.IndexOf(Item);
end;

{------------------------------------------------------------------------------
  function TMenuItem.IndexOfCaption(const ACaption: string): Integer;

  Returns the index of the menuitem with the given caption or -1
 ------------------------------------------------------------------------------}
function TMenuItem.IndexOfCaption(const ACaption: string): Integer;
begin
  for Result := 0 to Count - 1 do
    if Items[Result].Caption = ACaption then Exit;
  Result := -1;
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
  else
  begin
    Result := 0;
    i := 0;
    while (i<FItems.Count) do
    begin
      CurMenuItem := TMenuItem(FItems[i]);
      if CurMenuItem.Visible then
      begin
        if CurMenuItem=Item then Exit;
        Inc(Result);
      end;
      Inc(i);
    end;
    Result := -1;
    if Item.Visible then
      RaiseVisibleInconsistency;
  end;
end;

{------------------------------------------------------------------------------
  Method:  TMenuItem.MenuChanged
  Params:  Rebuild : Boolean
  Returns: Nothing

 ------------------------------------------------------------------------------}
procedure TMenuItem.MenuChanged(Rebuild : Boolean);
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
  if I < 0 then
    raise EMenuError.Create(SMenuNotFound);
  Delete(I);
end;

{------------------------------------------------------------------------------
  function TMenuItem.IsInMenuBar: boolean;
 ------------------------------------------------------------------------------}
function TMenuItem.IsInMenuBar: boolean;
begin
  Result := (FParent <> nil) and (FParent.FMenu <> nil) and (FParent.FMenu is TMainMenu);
end;

{------------------------------------------------------------------------------
  procedure TMenuItem.Clear;
  
  Deletes all children
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
  Result := FBitmap <> nil;
end;

{------------------------------------------------------------------------------
  function TMenuItem.GetIconSize: TPoint;
 ------------------------------------------------------------------------------}
function TMenuItem.GetIconSize: TPoint;
var
  AImageList: TCustomImageList;
begin
  FillChar(Result, SizeOf(Result), 0);
  if HasIcon then
  begin
    AImageList := GetImageList;
    if AImageList <> nil then
    begin
      if (FImageIndex < 0) or (FImageIndex >= AImageList.Count) then
        Exit;
      Result.x := AImageList.Width;
      Result.y := AImageList.Height;
    end
    else
    begin
      Result.x := Bitmap.Width;
      Result.y := Bitmap.Height;
    end;
  end;
end;

procedure TMenuItem.RemoveAllHandlersOfObject(AnObject: TObject);
var
  HandlerType: TMenuItemHandlerType;
begin
  inherited RemoveAllHandlersOfObject(AnObject);
  for HandlerType := Low(TMenuItemHandlerType) to High(TMenuItemHandlerType) do
    FMenuItemHandlers[HandlerType].RemoveAllMethodsOfObject(AnObject);
end;

procedure TMenuItem.AddHandlerOnDestroy(const OnDestroyEvent: TNotifyEvent;
  AsLast: boolean);
begin
  AddHandler(mihtDestroy, TMethod(OnDestroyEvent),AsLast);
end;

procedure TMenuItem.RemoveHandlerOnDestroy(const OnDestroyEvent: TNotifyEvent);
begin
  RemoveHandler(mihtDestroy, TMethod(OnDestroyEvent));
end;

procedure TMenuItem.AddHandler(HandlerType: TMenuItemHandlerType;
  const AMethod: TMethod; AsLast: boolean);
begin
  if FMenuItemHandlers[HandlerType] = nil then
    FMenuItemHandlers[HandlerType] := TMethodList.Create;
  FMenuItemHandlers[HandlerType].Add(AMethod);
end;

procedure TMenuItem.RemoveHandler(HandlerType: TMenuItemHandlerType;
  const AMethod: TMethod);
begin
  FMenuItemHandlers[HandlerType].Remove(AMethod);
end;

function TMenuItem.MenuVisibleIndex: integer;
begin
  Result:=-1;
  if Parent=nil then
    Result:=-1
  else
    Result:=Parent.VisibleIndexOf(Self);
end;

procedure TMenuItem.WriteDebugReport(const Prefix: string);
var
  Flags: String;
  i: Integer;
begin
  Flags:='';
  if Visible then Flags:=Flags+'V';
  if Enabled then Flags:=Flags+'E';
  if RadioItem then Flags:=Flags+'R';
  if Checked then Flags:=Flags+'C';
  if HandleAllocated then Flags:=Flags+'H';
  DbgOut(Prefix,' Name="',Name,'" Caption="',DbgStr(Caption),'" Flags=',Flags);
  if Parent<>nil then
    DbgOut(' ',dbgs(MenuIndex),'/',dbgs(Parent.Count));
  DebugLn('');
  for i:=0 to Count-1 do
    Items[i].WriteDebugReport(Prefix+'  ');
end;

{------------------------------------------------------------------------------
  function TMenuItem.IsCheckItem: boolean;

  Results true if 'Checked' or 'RadioItem' or 'AutoCheck'
  or 'ShowAlwaysCheckable'
 ------------------------------------------------------------------------------}
function TMenuItem.IsCheckItem: boolean;
begin
  Result := Checked or RadioItem or AutoCheck or ShowAlwaysCheckable;
end;


{ Returns true if the current menu item is a Line (menu seperator). Added for
  Delphi compatability as well. }
function TMenuItem.IsLine: Boolean;
begin
  Result := FCaption = cLineCaption;
end;


{------------------------------------------------------------------------------
  Method:  TMenuItem.SetCaption
  Params:  Value:
  Returns: Nothing

  Sets the caption of a menuItem.
 ------------------------------------------------------------------------------}
procedure TMenuItem.SetCaption(const AValue: TTranslateString);
begin
  if FCaption = AValue then exit;
  FCaption := AValue;
  if HandleAllocated and ((Parent <> nil) or (FMenu = nil)) then
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
    if (FParent <> nil) and not (csReading in ComponentState) and HandleAllocated then
      TWSMenuItemClass(WidgetSetClass).SetCheck(Self, AValue);
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
  if FEnabled <> AValue then
  begin
    FEnabled := AValue;
    if HandleAllocated and (Parent <> nil) then
      TWSMenuItemClass(WidgetSetClass).SetEnable(Self, AValue);
    MenuChanged(False);
  end;
end;

{------------------------------------------------------------------------------
  procedure TMenuItem.SetBitmap(const AValue: TBitmap);

  Reposition the MenuItem
 ------------------------------------------------------------------------------}
procedure TMenuItem.SetBitmap(const AValue: TBitmap);
begin
  // ImageList have highest priority
  if (FBitmap = AValue) or ((GetImageList <> nil) and (ImageIndex <> -1)) then
    exit;

  FBitmapIsValid := True;
  if (AValue <> nil) then
    Bitmap.Assign(AValue)
  else
    FreeAndNil(FBitmap);

  UpdateWSIcon;
  MenuChanged(False);
end;

procedure TMenuItem.SetGlyphShowMode(const AValue: TGlyphShowMode);
begin
  if FGlyphShowMode = AValue then Exit;
  FGlyphShowMode := AValue;
  UpdateImage;
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
    if AValue <> MenuIndex then
    begin
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
    if (GroupIndex<>0) and (FParent<>nil) then
    begin
      for I := 0 to FParent.Count - 1 do
      begin
        Item := FParent[I];
        if (Item <> Self) and (Item.GroupIndex = GroupIndex) then
          Item.FRadioItem := FRadioItem;
      end;
    end;
    if (FParent <> nil) and not (csReading in ComponentState) and (HandleAllocated) then
      TWSMenuItemClass(WidgetSetClass).SetRadioItem(Self, AValue);
  end;
end;

{------------------------------------------------------------------------------
  procedure TMenuItem.SetRightJustify(const AValue: boolean);

  Enables a menuItem.
 ------------------------------------------------------------------------------}
procedure TMenuItem.SetRightJustify(const AValue: boolean);
begin
  if FRightJustify = AValue then Exit;
  FRightJustify := AValue;
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
  begin
    FSubMenuImages.UnRegisterChanges(FImageChangeLink);
    FSubMenuImages.RemoveFreeNotification(Self);
  end;
  FSubMenuImages := AValue;
  if FSubMenuImages <> nil then
  begin
    FSubMenuImages.RegisterChanges(FImageChangeLink);
    FSubMenuImages.FreeNotification(Self);
  end;
  UpdateImages;
end;

{------------------------------------------------------------------------------
  Method:  TMenuItem.SetImageIndex
  Params:  Value:
  Returns: Nothing

  Enables a menuItem.
 ------------------------------------------------------------------------------}
procedure TMenuItem.SetImageIndex(AValue: TImageIndex);
var
  AImageList: TCustomImageList;
begin
  if (FImageIndex = AValue) then
    Exit;
  //debugln('TMenuItem.SetImageIndex A ',Name,' Old=',FImageIndex,' New=',Value);
  AImageList := GetImageList;
  FImageIndex := AValue;
  if AImageList = nil then
    Exit;
    
  FBitmapIsValid := False;
  if (FImageIndex < 0) or (AImageList = nil) or (FImageIndex >= AImageList.Count) then
    FreeAndNil(FBitmap);
    
  UpdateWSIcon;
  MenuChanged(False);
end;

{------------------------------------------------------------------------------
  Method:  TMenuItem.SetParentComponent
  Params:  Value:
  Returns: Nothing

  Enables a menuItem.
 ------------------------------------------------------------------------------}
procedure TMenuItem.SetParentComponent(AValue : TComponent);
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
procedure TMenuItem.SetShortCut(const AValue : TShortCut);
Begin
  if FShortCut <> AValue then
  begin
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
  if FVisible = AValue then Exit;
  //debugln('TMenuItem.SetVisible ',dbgsname(Self),' NewValue=',dbgs(AValue),' HandleAllocated=',dbgs(HandleAllocated));
  if ([csDestroying] * ComponentState <> []) then Exit;
  if AValue then
  begin
    FVisible := AValue;
    if (not (csLoading in ComponentState)) and Parent.HandleAllocated then
      HandleNeeded;
    if HandleAllocated then
      TWSMenuItemClass(WidgetSetClass).SetVisible(Self, True);
  end else
  begin
    if HandleAllocated then
    begin
      TWSMenuItemClass(WidgetSetClass).SetVisible(Self, False);
      DestroyHandle;
    end;
    FVisible := AValue;
  end;
end;

procedure TMenuItem.UpdateImage;
var
  ImgList: TCustomImageList;
begin
  if HandleAllocated and ([csLoading, csDestroying] * ComponentState = []) then
  begin
    ImgList := GetImageList;
    if FBitmapIsValid then // Bitmap is assigned through Bitmap property
    begin
      if (ImgList <> nil) and (ImageIndex <> -1) then
      begin
        FreeAndNil(FBitmap);
        FBitmapIsValid := False;
      end;
    end
    else
    begin
      if (ImgList = nil) or (ImageIndex = -1) then
      begin
        FreeAndNil(FBitmap);
        FBitmapIsValid := True;
      end;
    end;
    UpdateWSIcon;
  end;
end;

procedure TMenuItem.UpdateImages;
var
  i: integer;
begin
  if HandleAllocated and ([csLoading,csDestroying]*ComponentState=[]) then
  begin
    UpdateImage;
    for i := 0 to Count - 1 do
      Items[i].UpdateImages;
  end;
end;

procedure TMenuItem.UpdateWSIcon;
begin
  if HandleAllocated then
    if HasIcon then // prevent creating bitmap
      TWSMenuItemClass(WidgetSetClass).UpdateMenuIcon(Self, HasIcon, Bitmap)
    else
      TWSMenuItemClass(WidgetSetClass).UpdateMenuIcon(Self, HasIcon, nil);
end;

procedure TMenuItem.ImageListChange(Sender: TObject);
begin
  if Sender = SubMenuImages then
    UpdateImages;
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

  Is Called whenever one of the children has changed.
 ------------------------------------------------------------------------------}
procedure TMenuItem.SubItemChanged(Sender: TObject; Source: TMenuItem;
  Rebuild: Boolean);
begin
  if Rebuild and HandleAllocated then
    ; //RebuildHandle;
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
  if (FParent <> nil) and (GroupIndex <> 0) then
    for I := 0 to FParent.Count - 1 do
    begin
      Item := FParent[I];
      if (Item <> Self) and Item.FRadioItem and (Item.GroupIndex = GroupIndex) then
        Item.FChecked := False;
    end;
end;

procedure TMenuItem.DoActionChange(Sender: TObject);
begin
  if Sender = Action then ActionChange(Sender, False);
end;

class procedure TMenuItem.WSRegisterClass;
begin
  inherited WSRegisterClass;
  RegisterMenuItem;
end;

procedure TMenuItem.AssignTo(Dest: TPersistent);
begin
  if Dest is TCustomAction then
  begin
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

procedure TMenuItem.BitmapChange(Sender: TObject);
begin
  UpdateImage;
end;

// included by menus.pp
