{%mainunit cocoawsextctrls.pas}

type

  { TCocoaStatusItemHandle }

  TCocoaStatusItemHandle = objcclass(NSObject, NSMenuDelegateProtocol)
  public
    { Fields }
    statusitem: NSStatusItem;
    TrayIcon: TCustomTrayIcon;
    { Methods }
    procedure lclAction(sender: id); message 'lclAction:';
    procedure lclSetTrayIcon(ATrayIcon: TCustomTrayIcon); message 'lclSetTrayIcon:';
    procedure menuWillOpen(menu: NSMenu); message 'menuWillOpen:';
  end;

{ TCocoaStatusItemHandle }

procedure TCocoaStatusItemHandle.lclAction(sender: id);
begin
  if Assigned(TrayIcon.OnClick) then
    TrayIcon.OnClick(TrayIcon);
end;

procedure TCocoaStatusItemHandle.lclSetTrayIcon(ATrayIcon: TCustomTrayIcon);
var
  image: NSImage;
begin
  TrayIcon := ATrayIcon;

  // Shows the icon

  if (ATrayIcon.icon <> nil) and (ATrayIcon.icon.Handle <> 0) then
  begin
    image := TCocoaBitmap(ATrayIcon.icon.Handle).image;
    if image <> nil then statusitem.setImage(image);
  end;

  // Show the menu

  if (ATrayIcon.PopUpMenu <> nil) then
  begin
    ATrayIcon.PopUpMenu.HandleNeeded();
    //ATrayIcon.PopUpMenu.
    if Assigned(statusitem.menu) and (statusitem.menu.delegate = NSMenuDelegateProtocol(self)) then
      statusitem.menu.setDelegate(nil);

    statusitem.setMenu(TCocoaMenu(ATrayIcon.PopUpMenu.Handle));

    TCocoaMenu(ATrayIcon.PopUpMenu.Handle).setDelegate( self);
  end;
end;

procedure TCocoaStatusItemHandle.menuWillOpen(menu: NSMenu);
begin
  if Assigned(statusitem)
    and (statusitem.menu=menu)
    and (TrayIcon.PopUpMenu = nil) // LCL changes the value w/o letting WS know! :(
  then
    menu.cancelTrackingWithoutAnimation;
end;

{ TCocoaWSCustomTrayIcon }

class function TCocoaWSCustomTrayIcon.Hide(const ATrayIcon: TCustomTrayIcon): Boolean;
var
  StatusItemHandle: TCocoaStatusItemHandle;
  statusitem: NSStatusItem;
begin
  Result:=false;
  if ATrayIcon.Handle = 0 then Exit;
  StatusItemHandle := TCocoaStatusItemHandle(ATrayIcon.Handle);
  statusitem := StatusItemHandle.statusitem;
  if statusitem = nil then Exit;

  statusitem.release;

  Result := True;
end;

class function TCocoaWSCustomTrayIcon.Show(const ATrayIcon: TCustomTrayIcon): Boolean;
var
  statusitem: NSStatusItem;
  bar: NSStatusBar;
  StatusItemHandle: TCocoaStatusItemHandle;
begin
  {$ifdef VerboseCocoaTrayIcon}
    WriteLn(':>[TCocoaWSCustomTrayIcon.Show]');
  {$endif VerboseCocoaTrayIcon}

  Result := False;

  { Creates the handle }
  
  bar := NSStatusBar.systemStatusBar();
  statusitem := bar.statusItemWithLength(NSSquareStatusItemLength);
  statusitem.retain();
  StatusItemHandle := TCocoaStatusItemHandle.alloc.init();
  StatusItemHandle.statusitem := statusitem;
  ATrayIcon.Handle := HWND(StatusItemHandle);

  // OnClick support
  statusitem.setTarget(StatusItemHandle);
  statusitem.setAction(objcselector('lclAction:'));

  // set the main properties
  StatusItemHandle.lclSetTrayIcon(ATrayIcon);

  statusitem.setHighlightMode(True);
  {$ifdef BOOLFIX}
  statusitem.setEnabled_(Ord(True));
  {$else}
  statusitem.setEnabled(True);
  {$endif}

  Result := True;
  
  {$ifdef VerboseCocoaTrayIcon}
{    WriteLn(':<[TCocoaWSCustomTrayIcon.Show]',
     ' Handle: ', IntToHex(ATrayIcon.Handle, 8),
     ' ACGRect.size.width: ', ACGRect.size.width,
     ' ACGRect.size.height: ', ACGRect.size.height,
     ' ACGRect.origin.x: ', ACGRect.origin.x,
     ' ACGRect.origin.y: ', ACGRect.origin.y,
     ' TCocoaBitmap(ATrayIcon.Icon.Handle).CGImage ', IntToHex(Int64(TCocoaBitmap(ATrayIcon.Icon.Handle).CGImage), 8)
     );}
  {$endif VerboseCocoaTrayIcon}
end;

class procedure TCocoaWSCustomTrayIcon.InternalUpdate(const ATrayIcon: TCustomTrayIcon);
var
  StatusItemHandle: TCocoaStatusItemHandle;
begin
  if ATrayIcon.Handle = 0 then Exit;
  StatusItemHandle := TCocoaStatusItemHandle(ATrayIcon.Handle);

  StatusItemHandle.lclSetTrayIcon(ATrayIcon);
end;

class function TCocoaWSCustomTrayIcon.ShowBalloonHint(const ATrayIcon: TCustomTrayIcon): Boolean;
begin
  Result := False;
end;

class function TCocoaWSCustomTrayIcon.GetPosition(const ATrayIcon: TCustomTrayIcon): TPoint;
begin
  Result := Point(0, 0);
end;

