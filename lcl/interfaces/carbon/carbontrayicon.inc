{%mainunit carbonwsextctrls.pp}

{$ifdef CarbonUseCocoa}

{ TCarbonWSCustomTrayIcon }

type
  TPrivateCocoaCarbonTrayIcon = class(TObject)
  public
    { Fields }
    bar: NSStatusBar;
    item: NSStatusItem;
    image: NSImage;
  end;

class function TCarbonWSCustomTrayIcon.Hide(const ATrayIcon: TCustomTrayIcon): Boolean;
var
  APrivateTrayIcon: TPrivateCocoaCarbonTrayIcon;
begin
  APrivateTrayIcon := TPrivateCocoaCarbonTrayIcon(ATrayIcon.Handle);

  if APrivateTrayIcon.item <> nil then
  begin
    APrivateTrayIcon.item.Free;
    APrivateTrayIcon.item := nil;
  end;

  Result := True;
end;

{
  Documentation for converting a CGImageRef to a NSImage can be found here:
  
  http://www.cocoadev.com/index.pl?CGImageRef
}
class function TCarbonWSCustomTrayIcon.Show(const ATrayIcon: TCustomTrayIcon): Boolean;
var
  APrivateTrayIcon: TPrivateCocoaCarbonTrayIcon;
  ASize: NSSize;
  ACGRect: CGRect;
  AcurrentContext: NSGraphicsContext;
begin
  {$ifdef VerboseCarbonTrayIcon}
    WriteLn(':>[TCarbonWSCustomTrayIcon.Show]');
  {$endif VerboseCarbonTrayIcon}

  Result := False;

  { Creates the handle }
  
  APrivateTrayIcon := TPrivateCocoaCarbonTrayIcon.Create;
  
  APrivateTrayIcon.bar := NSStatusBar.systemStatusBar();

  ATrayIcon.Handle := PtrInt(APrivateTrayIcon);
  
  { Convert our CFImageRef to a NSImage }

  ASize.width := TCarbonBitmap(ATrayIcon.Icon.Handle).Width;
  ASize.height := TCarbonBitmap(ATrayIcon.Icon.Handle).Height;
  ACGRect.size.width := ASize.width;
  ACGRect.size.height := ASize.height;
  ACGRect.origin.x := 0;
  ACGRect.origin.y := 0;
  
  APrivateTrayIcon.image := NSImage.initWithSize(ASize);
  APrivateTrayIcon.image.setCacheMode(NSImageCacheNever);
  APrivateTrayIcon.image.lockFocus;
  AcurrentContext := NSGraphicsContext.currentContext();
  CGContextDrawImage(AcurrentContext.graphicsPort, ACGRect, TCarbonBitmap(ATrayIcon.Icon.Handle).CGImage);
  {$ifdef VerboseCarbonTrayIcon}
    WriteLn('::[TCarbonWSCustomTrayIcon.Show]',
     ' AcurrentContext ', IntToHex(PtrUInt(Pointer(AcurrentContext)), 8),
     ' AcurrentContext.ClassID ', IntToHex(Int64(AcurrentContext.ClassID), 8),
     ' AcurrentContext.Handle ', IntToHex(Int64(AcurrentContext.Handle), 8),
     ' AcurrentContext.graphicsPort ', IntToHex(Int64(AcurrentContext.graphicsPort), 8)
     );
  {$endif VerboseCarbonTrayIcon}
  APrivateTrayIcon.image.unlockFocus;

  { Inserts the menu }

  if ATrayIcon.PopUpMenu <> nil then
  begin
  
  end;

  { Shows the icon }

  if APrivateTrayIcon.item <> nil then Exit(True);

  APrivateTrayIcon.item := NSStatusItem.CreateWithHandle(APrivateTrayIcon.bar.statusItemWithLength(NSSquareStatusItemLength));
  APrivateTrayIcon.item.retain();
  APrivateTrayIcon.item.setImage(APrivateTrayIcon.image.Handle);

  Result := True;
  
  {$ifdef VerboseCarbonTrayIcon}
    WriteLn(':<[TCarbonWSCustomTrayIcon.Show]',
     ' Handle: ', IntToHex(ATrayIcon.Handle, 8),
     ' ACGRect.size.width: ', ACGRect.size.width,
     ' ACGRect.size.height: ', ACGRect.size.height,
     ' ACGRect.origin.x: ', ACGRect.origin.x,
     ' ACGRect.origin.y: ', ACGRect.origin.y,
     ' TCarbonBitmap(ATrayIcon.Icon.Handle).CGImage ', IntToHex(Int64(TCarbonBitmap(ATrayIcon.Icon.Handle).CGImage), 8)
     );
  {$endif VerboseCarbonTrayIcon}
end;

class procedure TCarbonWSCustomTrayIcon.InternalUpdate(const ATrayIcon: TCustomTrayIcon);
begin

end;

class function TCarbonWSCustomTrayIcon.ShowBalloonHint(const ATrayIcon: TCustomTrayIcon): Boolean;
begin
  Result := True;
end;

class function TCarbonWSCustomTrayIcon.GetPosition(const ATrayIcon: TCustomTrayIcon): TPoint;
begin
  Result := Point(0, 0);
end;

{$endif CarbonUseCocoa}

