{%mainunit cocoawsextctrls.pp}

{$STATIC ON}
(*
type

  { TPrivateCocoaCocoaTrayIcon }

    TPrivateCocoaCocoaTrayIcon = class(NSObject)
  public
    { Fields }
    bar: NSStatusBar;
    item: NSStatusItem;
    image: NSImage;
    menu: NSMenu;
    { Structural Methods }
//    constructor Create; override;
  end;
*)

{ TCocoaWSCustomTrayIcon }

(*
class function TCocoaWSCustomTrayIcon.Hide(const ATrayIcon: TCustomTrayIcon): Boolean;
var
  APrivateTrayIcon: TPrivateCocoaCocoaTrayIcon;
begin
  APrivateTrayIcon := TPrivateCocoaCocoaTrayIcon(ATrayIcon.Handle);

  if APrivateTrayIcon.item <> nil then
  begin
    APrivateTrayIcon.item.Free;
    APrivateTrayIcon.item := nil;
  end;

  Result := True;
end;

{
}
class function TCocoaWSCustomTrayIcon.Show(const ATrayIcon: TCustomTrayIcon): Boolean;
var
  APrivateTrayIcon: TPrivateCocoaCocoaTrayIcon;
  ASize: NSSize;
  ACGRect: CGRect;
  AcurrentContext: NSGraphicsContext;
begin
  {$ifdef VerboseCocoaTrayIcon}
    WriteLn(':>[TCocoaWSCustomTrayIcon.Show]');
  {$endif VerboseCocoaTrayIcon}

  Result := False;

  { Creates the handle }
  
  APrivateTrayIcon := TPrivateCocoaCocoaTrayIcon.Create;
  
  APrivateTrayIcon.bar := NSStatusBar.systemStatusBar();

  ATrayIcon.Handle := PtrInt(APrivateTrayIcon);
  
  { Shows the icon }

{  if APrivateTrayIcon.item <> nil then Exit(True);

  APrivateTrayIcon.item := NSStatusItem.CreateWithHandle(APrivateTrayIcon.bar.statusItemWithLength(NSSquareStatusItemLength));
  APrivateTrayIcon.item.retain();
  APrivateTrayIcon.item.setImage(APrivateTrayIcon.image.Handle);}

  { Inserts the menu }

{  if ATrayIcon.PopUpMenu <> nil then
  begin
    APrivateTrayIcon.menu := APrivateTrayIcon.CreateMenu(ATrayIcon.PopUpMenu);
    APrivateTrayIcon.item.setMenu(APrivateTrayIcon.menu.Handle);
  end;}

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
*)

