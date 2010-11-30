{%MainUnit ../menus.pp}

{******************************************************************************
                                  TMenu
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
  FBidiMode := bdLeftToRight;
  FParentBidiMode := True;
  ParentBidiModeChanged(AOwner);
  inherited Create(AOwner);
end;

{------------------------------------------------------------------------------
  procedure TMenu.SetImages(const AValue: TCustomImageList);

  Creates the handle ( = object).
 ------------------------------------------------------------------------------}
procedure TMenu.SetImages(const AValue: TCustomImageList);
begin
  if FImages <> nil then
  begin
    FImages.UnRegisterChanges(FImageChangeLink);
    FImages.RemoveFreeNotification(Self);
  end;
  FImages := AValue;
  if FImages <> nil then
  begin
    FImages.FreeNotification(Self);
    FImages.RegisterChanges(FImageChangeLink);
  end;
  FItems.UpdateImages;
end;

procedure TMenu.SetBidiMode(const AValue: TBidiMode);
begin
  if FBidiMode=AValue then exit;
  FBidiMode:=AValue;
  FParentBiDiMode := False;
  if not (csLoading in ComponentState) then
    BidiModeChanged;
end;

procedure TMenu.SetParentBidiMode(const AValue: Boolean);
begin
  if FParentBiDiMode = AValue then exit;
  FParentBiDiMode := AValue;
  if not (csLoading in ComponentState) then
    ParentBidiModeChanged;
end;

class procedure TMenu.WSRegisterClass;
begin
  inherited WSRegisterClass;
  RegisterMenu;
end;

procedure TMenu.CMParentBiDiModeChanged(var Message: TLMessage);
begin
  ParentBidiModeChanged;
end;

procedure TMenu.CMAppShowMenuGlyphChanged(var Message: TLMessage);
begin
  FItems.UpdateImages;
end;

procedure TMenu.BidiModeChanged;
begin
  if HandleAllocated then
    TWSMenuClass(WidgetSetClass).SetBiDiMode(Self, UseRightToLeftAlignment, UseRightToLeftReading);
end;

procedure TMenu.ParentBidiModeChanged(AOwner: TComponent);
begin
  if FParentBidiMode then
  begin
    //Take the value from the Owner
    //i can not use parent because TPopupMenu.Parent = nil
    if (AOwner<>nil)
    and (AOwner is TCustomForm)
    and not (csDestroying in AOwner.ComponentState) then
    begin
      BiDiMode := TCustomForm(AOwner).BiDiMode;
      FParentBiDiMode := True;
    end;
  end;
end;

procedure TMenu.ParentBidiModeChanged;
begin
  ParentBidiModeChanged(Owner);
end;

{------------------------------------------------------------------------------
  procedure TMenu.SetParent(const AValue: TComponent);

 ------------------------------------------------------------------------------}
procedure TMenu.SetParent(const AValue: TComponent);
begin
  if FParent = AValue then Exit;
  FParent := AValue;
  if (FParent = nil) and (Items <> nil) and Items.HandleAllocated then
  begin
    // disconnect from the form
    DestroyHandle;
  end
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
begin
  FItems.Handle := TWSMenuClass(WidgetSetClass).CreateHandle(Self);
  FItems.CheckChildrenHandles;
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
  Returns: the menu item with the shortcut


 ------------------------------------------------------------------------------}
function TMenu.FindItem(AValue: PtrInt; Kind: TFindItemKind): TMenuItem;

  function Find(Item: TMenuItem): TMenuItem;
  var
    I: Integer;
    {$IFDEF UseAltKeysForMenuItems}
    Key: Word;
    Shift: TShiftState;
    {$ENDIF}
  begin
    Result := nil;
    //DebugLn(['Find ',dbgsName(Item),' Item.ShortCut=',dbgs(Item.ShortCut),' ',dbgs(TShortCut(AValue))]);
    if Item = nil then exit;
    if ((Kind = fkCommand) and (AValue = Item.Command)) or
       ((Kind = fkHandle) and (HMenu(AValue) = Item.FHandle)) or
       ((Kind = fkShortCut) and (AValue = Item.ShortCut)) then
    begin
      Result := Item;
      Exit;
    end;
    {$IFDEF UseAltKeysForMenuItems}
    if (Kind = fkShortCut) and (Item.IsInMenuBar)
    then begin
      // ToDo: check if parent is currently visible
      // item caption is currently visible -> check caption for
      ShortCutToKey(TShortCut(AValue),Key,Shift);
      if (Shift=[ssAlt]) and IsAccel(Key,Item.Caption) then begin
        Result := Item;
        exit;
      end;
    end;
    {$ENDIF}

    for I := 0 to Item.GetCount - 1 do
    begin
      Result := Find(Item[I]);
      if Assigned(Result) then
        Exit;
    end;
  end;

begin
  Result := Find(Items);
end;

function TMenu.GetHelpContext(AValue: PtrInt; ByCommand: Boolean): THelpContext;
const
  FindKind: array[Boolean] of TFindItemKind = (fkHandle, fkCommand);
var
  Item: TMenuItem;
begin
  Result := 0;
  Item := FindItem(AValue, FindKind[ByCommand]);
  if Item <> nil then
    Result := Item.HelpContext;
end;

function TMenu.IsShortcut(var Message: TLMKey): boolean;

  procedure HandleItem(Item: TMenuItem);
  begin
    if Item = nil then
      Exit;
    HandleItem(Item.Parent);
    if Item.Enabled then
    begin
      Item.InitiateActions;
      Item.Click;
    end
    else
      FShortcutHandled := False;
  end;

var
  Item: TMenuItem;
  Shortcut: TShortcut;
  ShiftState: TShiftState;
begin
  ShiftState := KeyDataToShiftState(Message.KeyData);
  Shortcut := Menus.Shortcut(Message.CharCode, ShiftState);
  Item := FindItem(Shortcut, fkShortcut);
  Result := not (csDesigning in ComponentState) and (Item <> nil);
  //DebugLn(['TMenu.IsShortcut ',dbgsName(Self),' Result=',Result,' Message.CharCode=',Message.CharCode,' ShiftState=',dbgs(ShiftState)]);
  if Result then
  begin
    FShortcutHandled := True;
    HandleItem(Item);
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
  i: integer;
begin
  for i := 0 to FItems.Count - 1 do
    Proc(TComponent(FItems[i]));
end;

procedure TMenu.MenuChanged(Sender: TObject; Source: TMenuItem; Rebuild: Boolean);
begin
  if ComponentState * [csLoading, csDestroying] = [] then
    DoChange(Source, Rebuild);
end;

procedure TMenu.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (AComponent = FImages) then
    Images := nil;
end;

procedure TMenu.SetChildOrder(Child: TComponent; Order: Integer);
begin
  (Child as TMenuItem).MenuIndex := Order;
end;

procedure TMenu.UpdateItems;
begin
  // TODO: implement
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

function TMenu.IsBiDiModeStored: boolean;
begin
  Result := not FParentBidiMode;
end;

{------------------------------------------------------------------------------
  Function: TMenu.IsRightToLeft
  Params:
  Returns:


 ------------------------------------------------------------------------------}
function TMenu.IsRightToLeft : Boolean;
Begin
  Result := BidiMode <> bdLeftToRight;
end;

function TMenu.UseRightToLeftAlignment : Boolean;
begin
  Result := (BiDiMode = bdRightToLeft);
end;

function TMenu.UseRightToLeftReading : Boolean;
begin
  Result := (BiDiMode <> bdLeftToRight);
end;

// included by menus.pp
