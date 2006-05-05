{%MainUnit ../menus.pp}

{******************************************************************************
                                  TMenu
 ******************************************************************************

 *****************************************************************************
 *                                                                           *
 *  This file is part of the Lazarus Component Library (LCL)                 *
 *                                                                           *
 *  See the file COPYING.modifiedLGPL, included in this distribution,        *
 *  for details about the copyright.                                         *
 *                                                                           *
 *  This program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
 *                                                                           *
 *****************************************************************************
}

{------------------------------------------------------------------------------
  Method: TMenu.Create
  Params:  AOwner: the owner of the class
  Returns: Nothing

  Constructor for the class.
 ------------------------------------------------------------------------------}
constructor TMenu.Create(AOwner: TComponent);
begin
  FItems := TMenuItem.Create(Self);
  FItems.FOnChange := @MenuChanged;
  FItems.FMenu := Self;
  FImageChangeLink := TChangeLink.Create;
  FImageChangeLink.OnChange := @ImageListChange;
  Inherited Create(AOwner);
end;

{------------------------------------------------------------------------------
  procedure TMenu.SetImages(const AValue: TCustomImageList);

  Creates the handle ( = object).
 ------------------------------------------------------------------------------}
procedure TMenu.SetImages(const AValue: TCustomImageList);
begin
  // ToDo
  FImages:=AValue;
end;

{------------------------------------------------------------------------------
  procedure TMenu.SetParent(const AValue: TComponent);

 ------------------------------------------------------------------------------}
procedure TMenu.SetParent(const AValue: TComponent);
begin
  if FParent=AValue then exit;
  FParent:=AValue;
  if (FParent=nil) and (Items<>nil) and Items.HandleAllocated then begin
    // disconnect from form
    DestroyHandle;
  end;
end;

procedure TMenu.ImageListChange(Sender: TObject);
begin
  if Sender = Images then UpdateItems;
end;

{------------------------------------------------------------------------------
  Method:  TMenu.CreateHandle
  Params:  None
  Returns: Nothing

  Creates the handle ( = object).
 ------------------------------------------------------------------------------}
procedure TMenu.CreateHandle;
var i: integer;
begin
  FItems.Handle := TWSMenuClass(WidgetSetClass).CreateHandle(Self);
  // initiate creation of subitems
  // Note: FItems is a TMenuItem. Using HandleNeeded will create all subitems.
  for i:=0 to Items.Count-1 do
    if Items[i].Visible then
      Items[i].HandleNeeded;
end;

procedure TMenu.DestroyHandle;
begin
  Items.DestroyHandle;
end;

procedure TMenu.DoChange(Source: TMenuItem; Rebuild: Boolean);
begin
  if Assigned(FOnChange) then FOnChange(Self, Source, Rebuild);
end;

{------------------------------------------------------------------------------
  Method: TMenu.Destroy
  Params:  None
  Returns: Nothing

  Destructor for the class.
 ------------------------------------------------------------------------------}
destructor TMenu.Destroy;
begin
  FreeThenNil(FItems);
  FreeThenNil(FImageChangeLink);
  inherited Destroy;
end;

{------------------------------------------------------------------------------
  Function: TMenu.FindItem
  Params:
  Returns:


 ------------------------------------------------------------------------------}
function TMenu.FindItem(AValue: Integer; Kind: TFindItemKind): TMenuItem;

  function Find(Item: TMenuItem): TMenuItem;
  var
    I: Integer;
  begin
    Result := nil;
    if Item=nil then exit;
    if ((Kind = fkCommand) and (AValue = Item.Command))
    or ((Kind = fkHandle) and (AValue = Integer(Item.FHandle)))
    or ((Kind = fkShortCut) and (AValue = Item.ShortCut)) then
    begin
      Result := Item;
    end
    else
      for I := 0 to Item.GetCount - 1 do begin
        Result:=Find(Item[I]);
        if Result<>nil then
          Exit;
      end;
  end;

begin
  Result:=Find(Items);
end;

function TMenu.IsShortcut(var Message: TLMKey): boolean;
var
  Item: TMenuItem;
  Shortcut: TShortcut;
  ShiftState: TShiftState;
begin
  ShiftState := KeyDataToShiftState(Message.KeyData);
  Shortcut := Menus.Shortcut(Message.CharCode, ShiftState);
  Item := FindItem(Shortcut, fkShortcut);
  Result := Item <> nil;
  if Result then
  begin
    FShortcutHandled := true;
    Item.InitiateActions;
    Item.Click;
    Result := FShortcutHandled;
  end;
end;

{------------------------------------------------------------------------------
  Function: TMenu.GetHandle
  Params:   none
  Returns:  String containing output from the function.

  Description of the function for the class.
 ------------------------------------------------------------------------------}
function TMenu.GetHandle: HMenu;
begin
  Result := FItems.Handle;
end;


{------------------------------------------------------------------------------
  Function: TMenu.GetChildren
  Params:   proc - procedure which has to be called for every item
  	    root - root component
  Returns:  nothing

  Helper function for streaming.
 ------------------------------------------------------------------------------}
procedure TMenu.GetChildren(Proc: TGetChildProc; Root: TComponent);
var
   i : integer;
begin
   for i := 0 to FItems.Count - 1
      do if FItems[i].Owner = Root
       	    then Proc(TComponent (FItems [i]));
end;

procedure TMenu.MenuChanged(Sender: TObject; Source: TMenuItem; Rebuild: Boolean
  );
begin
  if ComponentState * [csLoading, csDestroying] = [] then
    DoChange(Source, Rebuild);
end;

procedure TMenu.SetChildOrder(Child: TComponent; Order: Integer);
begin
  (Child as TMenuItem).MenuIndex := Order;
end;

procedure TMenu.UpdateItems;
{
  function UpdateItem(MenuItem: TMenuItem): Boolean;
  begin
    Result := False;
    IterateMenus(@UpdateItem, MenuItem.FMerged, MenuItem);
    MenuItem.SubItemChanged(MenuItem, MenuItem, True);
  end;
}
begin
  //IterateMenus(@UpdateItem, Items.FMerged, Items);
end;

{------------------------------------------------------------------------------
  Function: TMenu.HandleAllocated
  Params:   None
  Returns:  True if handle is allocated

  Checks if a handle is allocated. I.E. if the control is created
 ------------------------------------------------------------------------------}
function TMenu.HandleAllocated : Boolean;
begin
   Result := FItems.HandleAllocated;
end;

{------------------------------------------------------------------------------
  Method:  TMenu.HandleNeeded
  Params:  AOwner: the owner of the class
  Returns: Nothing

  Description of the procedure for the class.
 ------------------------------------------------------------------------------}
procedure TMenu.HandleNeeded;
begin
  if not HandleAllocated then CreateHandle;
end;

function TMenu.DispatchCommand(ACommand: Word): Boolean;
var
  Item: TMenuItem;
begin
  Result := False;
  Item := FindItem(ACommand, fkCommand);
  if Item <> nil then
  begin
    Item.Click;
    Result := True;
  end;
end;

{------------------------------------------------------------------------------
  Function: TMenu.IsRightToLeft
  Params:
  Returns:


 ------------------------------------------------------------------------------}
function TMenu.IsRightToLeft : Boolean;
Begin
  //TODO: Make sure it should return FALSE!!!!!!!!!!
  Result := False;
end;

// included by menus.pp
