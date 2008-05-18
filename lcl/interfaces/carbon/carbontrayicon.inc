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

class function TCarbonWSCustomTrayIcon.Show(const ATrayIcon: TCustomTrayIcon): Boolean;
var
  APrivateTrayIcon: TPrivateCocoaCarbonTrayIcon;
  ASize: NSSize;
  ACGSize: CGRect absolute ASize;
  AcurrentContext: NSGraphicsContext;
begin
  Result := False;

  { Creates the handle }
  
  APrivateTrayIcon := TPrivateCocoaCarbonTrayIcon.Create;
  
  APrivateTrayIcon.bar := NSStatusBar.systemStatusBar();

  ATrayIcon.Handle := PtrInt(APrivateTrayIcon);
  
  { Convert our CFImageRef to a NSImage }

{  NSImage* image = [[NSImage alloc] initWithSize:rect.size];
   [image lockFocus];
   CGContextDrawImage([[NSGraphicsContext currentContext]
 graphicsPort], *(CGRect*)&rect, imageRef);
   [image unlockFocus];}
   
  ASize.width := TCarbonBitmap(ATrayIcon.Icon.Handle).Width;
  ASize.height := TCarbonBitmap(ATrayIcon.Icon.Handle).Height;

  APrivateTrayIcon.image := NSImage.initWithSize(ASize);
  APrivateTrayIcon.image.lockFocus;
  AcurrentContext := NSGraphicsContext.currentContext();
  CGContextDrawImage(AcurrentContext.graphicsPort, ACGSize, TCarbonBitmap(ATrayIcon.Icon.Handle).CGImage);
  APrivateTrayIcon.image.unlockFocus;

  { Shows the icon }

  if APrivateTrayIcon.item <> nil then Exit(True);

  APrivateTrayIcon.item := APrivateTrayIcon.bar.statusItemWithLength(NSSquareStatusItemLength);
  APrivateTrayIcon.item.retain();
  APrivateTrayIcon.item.setImage(APrivateTrayIcon.image);

  Result := True;
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

