{%mainunit cocoawsextctrls.pas}

(*type
  TCocoaStatusItem = objcclass(NSStatusItem)
    { Fields }
    bar: NSStatusBar;
    image: NSImage;
    menu: NSMenu;
    { Methods }
    //  procedure lclItemSelected(sender: id); message 'lclItemSelected:';
  end;*)

{ TCocoaWSCustomTrayIcon }

class function TCocoaWSCustomTrayIcon.Hide(const ATrayIcon: TCustomTrayIcon): Boolean;
{var
  statusitem: TCocoaStatusItem;
  bar: NSStatusBar;}
begin
{  bar := NSStatusBar.systemStatusBar();
  statusitem := bar.statusItemWithLength(NSSquareStatusItemLength);
  statusitem.bar := bar;
  statusicon.callback:=TLCLCommonCallback.Create(box, AWinControl);
  statusitem.retain();
  statusitem.item.setImage(APrivateTrayIcon.image.Handle);
  Result:=TLCLIntfHandle(box);
//  APrivateTrayIcon := TPrivateCocoaCocoaTrayIcon(ATrayIcon.Handle);

  if APrivateTrayIcon.item <> nil then
  begin
    APrivateTrayIcon.item.Free;
    APrivateTrayIcon.item := nil;
  end;}

  Result := True;
end;

{
}
class function TCocoaWSCustomTrayIcon.Show(const ATrayIcon: TCustomTrayIcon): Boolean;
var
  statusitem: NSStatusItem;
  bar: NSStatusBar;
  image: NSImage;
  {var
    APrivateTrayIcon: TPrivateCocoaCocoaTrayIcon;
    ASize: NSSize;
    ACGRect: CGRect;
    AcurrentContext: NSGraphicsContext;}
begin
  {$ifdef VerboseCocoaTrayIcon}
    WriteLn(':>[TCocoaWSCustomTrayIcon.Show]');
  {$endif VerboseCocoaTrayIcon}

  Result := False;

  { Creates the handle }
  
  bar := NSStatusBar.systemStatusBar();
  statusitem := bar.statusItemWithLength(NSSquareStatusItemLength);
//  statusitem.bar := bar;
  //statusicon.callback:=TLCLCommonCallback.Create(box, AWinControl);

  { Shows the icon }

  statusitem.retain();
  if (ATrayIcon.icon <> nil) and (ATrayIcon.icon.Handle <> 0) then
  begin
    image := TCocoaBitmap(ATrayIcon.icon.Handle).image;
    if image <> nil then statusitem.setImage(image);
  end;

//  statusitem.setTitle('Menu');
  statusitem.setHighlightMode(True);
  statusitem.setEnabled(True);

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
begin

end;

class function TCocoaWSCustomTrayIcon.ShowBalloonHint(const ATrayIcon: TCustomTrayIcon): Boolean;
begin
  Result := False;
end;

class function TCocoaWSCustomTrayIcon.GetPosition(const ATrayIcon: TCustomTrayIcon): TPoint;
begin
  Result := Point(0, 0);
end;

