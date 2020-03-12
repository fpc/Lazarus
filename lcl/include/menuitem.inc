{%MainUnit ../menus.pp}

{******************************************************************************
                                  TMenuItem
 ******************************************************************************

 *****************************************************************************
  This file is part of the Lazarus Component Library (LCL)

  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
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

procedure TMenuItem.Add(const AItems: array of TMenuItem);
var
  i: Integer;
begin
  for i := Low(AItems) to High(AItems) do
    Add(AItems[i]);
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

  function OnClickIsActionExecute: boolean;
  begin
    Result:=false;
    if Action=nil then exit;
    if not Assigned(Action.OnExecute) then exit;
    if not Assigned(FOnClick) then exit;
    Result:=CompareMethods(TMethod(FOnClick),TMethod(Action.OnExecute));
  end;

var
  CallAction: Boolean;
begin
  if not Enabled then Exit;
  if Assigned(OnMenuPopupHandler) then
    OnMenuPopupHandler(Self);

  if AutoCheck
  and not (Assigned(ActionLink) and ActionLink.IsAutoCheckLinked)
  and not (csDesigning in ComponentState)
  then begin
    // Break a little Delphi compatibility
    // It makes no sense to uncheck a checked RadioItem (besides, GTK can't handle it)
    if not (RadioItem and Checked) then
      Checked := not Checked;
  end;

  CallAction := Assigned(ActionLink) and not (csDesigning in ComponentState);
  // first call our own OnClick if it differs from Action.OnExecute
  if Assigned(FOnClick) and not (CallAction and OnClickIsActionExecute) then
    FOnClick(Self);
  // then trigger the Action
  if CallAction then
    ActionLink.Execute(Self);
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
  FCommand := TWSMenuItemClass(WidgetSetClass).OpenCommand;
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
  FHandle := TWSMenuItemClass(WidgetSetClass).CreateHandle(Self);
  CheckChildrenHandles;

  if MergedParent <> nil then
  begin
    MergedParent.HandleNeeded;
    //DebugLn('TMenuItem.CreateHandle Attaching ... ',Name,':',ClassName);
    if MergedParent.HandleAllocated then
      TWSMenuItemClass(WidgetSetClass).AttachMenu(Self);

    if HandleAllocated then
    begin
      if ShortCut <> 0 then
        ShortCutChanged;
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
  //debugln('TMenuItem.Destroy A ',dbgsName(Self),' ',Caption);
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
  if Assigned(FMerged) then
    MergeWith(nil);
  FreeAndNil(FItems);
  FreeAndNil(FActionLink);
  FreeAndNil(FImageChangeLink);
  for HandlerType:= low(TMenuItemHandlerType) to high(TMenuItemHandlerType) do
    FreeAndNil(FMenuItemHandlers[HandlerType]);
  if FParent <> nil then
    FParent.FItems.Remove(Self);
  if FCommand <> 0 then TWSMenuItemClass(WidgetSetClass).CloseCommand(FCommand);
  //debugln('TMenuItem.Destroy B ',dbgsName(Self));
  FreeAndNil(FMergedItems);
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
procedure TMenuItem.GetImageList(out aImages: TCustomImageList; out
  aImagesWidth: Integer);
var
  LItem: TMenuItem;
  LMenu: TMenu;
begin
  aImages := nil;
  LItem := Parent;
  while (LItem <> nil) and (LItem.SubMenuImages = nil) do
    LItem := LItem.Parent;
  if LItem <> nil then
  begin
    aImages := LItem.SubMenuImages;
    aImagesWidth := LItem.SubMenuImagesWidth;
  end else
  begin
    LMenu := GetParentMenu;
    if LMenu <> nil then
    begin
      aImages := LMenu.Images;
      aImagesWidth := LMenu.ImagesWidth;
    end;
  end;
end;

function TMenuItem.GetImageList: TCustomImageList;
var
  x: Integer;
begin
  GetImageList(Result, x);
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
  //  CheckChildrenHandles;    <- This is already called when menuitem is created.
  if not (csDesigning in ComponentState) then
  begin
    InitiateActions;
    Click;
  end
  else
  if Assigned(DesignerMenuItemClick) then
    DesignerMenuItemClick(Self);
end;

function TMenuItem.DoDrawItem(ACanvas: TCanvas; ARect: TRect;
  AState: TOwnerDrawState): Boolean;
var
  AParentMenu: TMenu;
begin
  Result := False;
  if Assigned(FOnDrawItem) then
  begin
    FOnDrawItem(Self, ACanvas, ARect, AState);
    Result := True;
  end else
  begin
    AParentMenu := GetParentMenu;
    if Assigned(AParentMenu.OnDrawItem) then
    begin
      AParentMenu.OnDrawItem(Self, ACanvas, ARect, AState);
      Result := True;
    end;
  end;
end;

function TMenuItem.DoMeasureItem(ACanvas: TCanvas; var AWidth,
  AHeight: Integer): Boolean;
var
  AParentMenu: TMenu;
begin
  Result := False;
  if Assigned(FOnMeasureItem) then
  begin
    FOnMeasureItem(Self, ACanvas, AWidth, AHeight);
    Result := True;
  end else
  begin
    AParentMenu := GetParentMenu;
    if Assigned(AParentMenu.OnMeasureItem) then
    begin
      AParentMenu.OnMeasureItem(Self, ACanvas, AWidth, AHeight);
      Result := True;
    end;
  end;
end;

procedure TMenuItem.CheckChildrenHandles;

  function GetMenu(Item: TMenuItem): TMenu;
  begin
    Result := nil;
    repeat
      if Assigned(Item.FMergedWith) then
      begin
        if Assigned(Item.FMergedWith.Menu) then
          Result := Item.FMergedWith.Menu;
        Item := Item.FMergedWith;
      end else
      begin
        if Assigned(Item.Menu) then
          Result := Item.Menu;
        Item := Item.Parent;
      end;
    until (Item = nil);
  end;

var
  i: Integer;
  AMenu: TMenu;
  AMergedItems: TMergedMenuItems;
begin
  if FItems = nil then
    Exit;

  AMenu := GetMenu(Self);
  AMergedItems := MergedItems;
  for i := 0 to AMergedItems.InvisibleCount-1 do
    if AMergedItems.InvisibleItems[i].HandleAllocated then
      AMergedItems.InvisibleItems[i].DestroyHandle;

  for i := 0 to AMergedItems.VisibleCount-1 do
  begin
    if AMergedItems.VisibleItems[i].HandleAllocated and (GetMenu(AMergedItems.VisibleItems[i]) <> AMenu) then
      AMergedItems.VisibleItems[i].DestroyHandle;
    AMergedItems.VisibleItems[i].HandleNeeded;
  end;
end;

procedure TMenuItem.IntfDoSelect;
begin
  Application.Hint := GetLongHint(Hint);
end;

procedure TMenuItem.InvalidateMergedItems;
begin
  FreeAndNil(FMergedItems);
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
  imw: Integer;
begin
  if FBitmap = nil then
  begin
    FBitmap := TBitmap.Create;

    if ImageIndex >= 0 then
    begin
      GetImageList(iml, imw);
      if (iml <> nil) and (ImageIndex < iml.Count) then
        iml.ResolutionForPPI[imw, 96, 1].Resolution.GetBitmap(ImageIndex, FBitmap);
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

function TMenuItem.GetMergedItems: TMergedMenuItems;
begin
  if not Assigned(FMergedItems) then
    FMergedItems := TMergedMenuItems.Create(Self);
  Result := FMergedItems;
end;

function TMenuItem.GetMergedParent: TMenuItem;
begin
  Result := Parent;
  if Assigned(Result) and Assigned(Result.MergedWith) then
    Result := Result.MergedWith;
end;

function TMenuItem.GetMergedParentMenu: TMenu;
var
  Item: TMenuItem;
begin
  Item := Self;
  while Item.MergedParent <> nil do
    Item := Item.MergedParent;
  Result := Item.FMenu;
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
var
    act: TCustomAction;
begin
  if Action <> nil then
  begin
    Result := true;
    act := TCustomAction(Action);
    if (act.ActionList <> nil) and (act.ActionList.Images <> nil) and
      (act.ImageIndex >= 0) and (act.ImageIndex < act.ActionList.Images.Count) then
        Result := false;
  end
  else Result :=
     FBitmapIsValid and
     (FBitmap <> nil) and (not FBitmap.Empty) and
     (FBitmap.Width > 0) and (FBitmap.Height > 0) and
     (ImageIndex < 0);
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

{------------------------------------------------------------------------------
  function TMenuItem.IsShortCutStored: boolean;

  Checks if 'ShortCut' needs to be saved to stream
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

function SystemShowMenuGlyphs: Boolean; inline;
begin
  Result := ThemeServices.GetOption(toShowMenuImages) = 1;
end;

{------------------------------------------------------------------------------
  function TMenuItem.HasIcon: boolean;

  Returns true if there is an icon
 ------------------------------------------------------------------------------}
function TMenuItem.HasIcon: boolean;

  function CanShowIcon: Boolean;
  begin
    Result := True;
    if csDesigning in ComponentState then
      Exit;
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
            sbgSystem: Result := SystemShowMenuGlyphs;
          end;
        end;
      gsmSystem:
        Result := SystemShowMenuGlyphs;
    end;
  end;

var
  AImageList: TCustomImageList;
  AImageListWidth: Integer;
begin
  Result := CanShowIcon;
  if not Result then
    Exit;
  GetImageList(AImageList, AImageListWidth);
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
    for i := FItems.Count - 1 downto 0 do
      TMenuItem(FItems[i]).DestroyHandle;
  end;
  if Assigned(FMerged) then
    for i := FMerged.Count - 1 downto 0 do
      FMerged[i].DestroyHandle;
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
  if FItems = nil then FItems := TMenuItems.Create(Self);

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
var
  i: Integer;
  CurMenuItem: TMenuItem;
  IsMerged: Boolean;
  AMergedItems: TMergedMenuItems;
begin
  if not Item.Visible then
    Exit(-1);
  AMergedItems := GetMergedItems;
  for I := 0 to AMergedItems.VisibleCount-1 do
    if AMergedItems.VisibleItems[I]=Item then
      Exit(I);
  Result := -1;
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
var
  AMergedParent: TMenuItem;
begin
  AMergedParent := MergedParent;
  Result := (AMergedParent <> nil) and (AMergedParent.FMenu <> nil) and (AMergedParent.FMenu is TMainMenu);
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
function TMenuItem.GetIconSize(ADC: HDC): TPoint;
var
  AImageList: TCustomImageList;
  PPI, AImageListWidth: Integer;
  Size: TSize;
begin
  FillChar(Result, SizeOf(Result), 0);
  if HasIcon then
  begin
    GetImageList(AImageList, AImageListWidth);
    if (AImageList <> nil) and (FImageIndex >= 0) then // using size of ImageList
    begin
      if (FImageIndex >= AImageList.Count) then
        Exit;
      PPI := GetDeviceCaps(ADC, LOGPIXELSX);
      Size := AImageList.SizeForPPI[AImageListWidth, PPI];
      Result.x := Size.cx;
      Result.y := Size.cy;
    end
    else // using size of Bitmap
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
  AsFirst: boolean);
begin
  AddHandler(mihtDestroy, TMethod(OnDestroyEvent),not AsFirst);
end;

procedure TMenuItem.RemoveHandlerOnDestroy(const OnDestroyEvent: TNotifyEvent);
begin
  RemoveHandler(mihtDestroy, TMethod(OnDestroyEvent));
end;

procedure TMenuItem.AddHandler(HandlerType: TMenuItemHandlerType;
  const AMethod: TMethod; AsFirst: boolean);
begin
  if FMenuItemHandlers[HandlerType] = nil then
    FMenuItemHandlers[HandlerType] := TMethodList.Create;
  FMenuItemHandlers[HandlerType].Add(AMethod,not AsFirst);
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

procedure TMenuItem.MergeWith(const aMenu: TMenuItem);
var
  i: Integer;
begin
  if (Assigned(aMenu) and (csDestroying in aMenu.ComponentState))
  or (FMerged=aMenu) then
    Exit;

  if Assigned(FMerged) then
  begin
    for i := 0 to FMerged.Count-1 do
      FMerged[i].DestroyHandle;
    FMerged.FMergedWith := nil;
  end;
  FMerged := aMenu;
  if Assigned(FMerged) then
  begin
    FMerged.FMergedWith := Self;
    FMerged.FreeNotification(Self);
  end;
  InvalidateMergedItems;
  CheckChildrenHandles;
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

procedure TMenuItem.SetName(const Value: TComponentName);
var
  ChangeCapt: Boolean;
begin
  if Name=Value then exit;
  ChangeCapt := not (csLoading in ComponentState) and (Name = Caption) and
      ( (Owner = nil) or not (csLoading in Owner.ComponentState) );
  inherited SetName(Value);
  if ChangeCapt then
    Caption := Value;
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

procedure TMenuItem.SetSubMenuImagesWidth(const aSubMenuImagesWidth: Integer);
begin
  if FSubMenuImagesWidth = aSubMenuImagesWidth then Exit;
  FSubMenuImagesWidth := aSubMenuImagesWidth;
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
  //debugln(['TMenuItem.SetImageIndex A ',Name,' Old=',FImageIndex,' New=',AValue]);
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
    if HandleAllocated and not (csLoading in ComponentState) then
      RegroupMenuItem(Handle,GroupIndex);
  end;
end;

{------------------------------------------------------------------------------
  Method:  TMenuItem.SetShortCut
  Params:  Value: TShortCut
  Returns: Nothing

  Set the ShortCut
 ------------------------------------------------------------------------------}
procedure TMenuItem.SetShortCut(const AValue : TShortCut);
Begin
  if FShortCut <> AValue then
  begin
    FShortCut := AValue;
    ShortCutChanged;
  end;
end;

procedure TMenuItem.SetShortCutKey2(const AValue: TShortCut);
begin
  if FShortCutKey2 <> AValue then
  begin
    FShortCutKey2 := AValue;
    ShortCutChanged;
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
    if (not (csLoading in ComponentState)) and (Parent<>nil)
    and Parent.HandleAllocated then
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
  if MergedParent<>nil then
    MergedParent.InvalidateMergedItems;
end;

procedure TMenuItem.UpdateImage(forced: Boolean);
var
  ImgList: TCustomImageList;
begin
  if [csLoading, csDestroying] * ComponentState = [] then
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
      if (forced) or (ImgList = nil) or (ImageIndex = -1) then
      begin
        FreeAndNil(FBitmap);
        FBitmapIsValid := True;
      end;
    end;
    if HandleAllocated then
      UpdateWSIcon;
  end;
end;

procedure TMenuItem.UpdateImages(forced: Boolean);
var
  i: integer;
begin
  if HandleAllocated and ([csLoading,csDestroying]*ComponentState=[]) then
  begin
    UpdateImage(forced);
    for i := 0 to Count - 1 do
      Items[i].UpdateImages(forced);
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

  Installs a new shortCut, removes an old one.
 ------------------------------------------------------------------------------}
procedure TMenuItem.ShortcutChanged;
begin
  if HandleAllocated then
    TWSMenuItemClass(WidgetSetClass).SetShortCut(Self, FShortCut, FShortCutKey2);
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
  MWE: ??? shouldn't we get checked from the interface in that case ???
 ------------------------------------------------------------------------------}
procedure TMenuItem.TurnSiblingsOff;
var
  I: Integer;
  Item: TMenuItem;
begin
  if Assigned(FParent) then
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
      Visible := Self.Visible;
    end
  end
  else
  if Dest is TMenuItem then
    MenuItem_Copy(Self, Dest as TMenuItem)
  else
    inherited AssignTo(Dest);
end;

procedure TMenuItem.BitmapChange(Sender: TObject);
begin
  UpdateImage;
end;

// included by menus.pp
