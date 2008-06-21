{%mainunit carbonwsextctrls.pp}

{$ifdef CarbonUseCocoa}

{$STATIC ON}

type

  { TPrivateCocoaCarbonTrayIcon }

  TPrivateCocoaCarbonTrayIcon = class(NSObject)
  public
    { Fields }
    bar: NSStatusBar;
    item: NSStatusItem;
    image: NSImage;
    menu: NSMenu;
    MenuItems: array of NSMenuItem;
    SubMenuOwners: array of NSMenu;
    SubMenuItems: array of NSMenuItem;
    { Structural Methods }
    constructor Create; override;
    class function getClass: objc.id; override;
    procedure AddMethods; override;
    { Pascal Methods }
    function TrimAllChar(const S: string; const ch: Char): string;
    function CreateMenu(APopUpMenu: TPopUpMenu): NSMenu;
    function CreateMenuItem(AMenuItem: TMenuItem;
      ACallbackName: string; ACallbackClass: NSObject): NSMenuItem;
    { Objective-C compatible methods }
    class procedure HandleMenuItemClick(_self: objc.id; _cmd: SEL; sender: objc.id); cdecl; static;
  end;

const
  Str_TPrivateCocoaCarbonTrayIcon = 'TTrayIcon';

  Str_HandleMenuItemClick = 'HandleMenuItemClick';

{ TPrivateCocoaCarbonTrayIcon }

{@@
  Adds methods to the class

  Details of the parameters string:

  The first parameter is the result (v = void),
  followed by self and _cmd (@ = id and : = SEL),
  and on the end "sender" (@ = id)
}
procedure TPrivateCocoaCarbonTrayIcon.AddMethods;
begin
  AddMethod(Str_HandleMenuItemClick, 'v@:@', Pointer(HandleMenuItemClick));
end;

constructor TPrivateCocoaCarbonTrayIcon.Create;
begin
  { The class is registered on the Objective-C runtime before the NSObject constructor is called }
  // The original plan was to create a descendent class, but causes wierd crashes
  // so now we just stuck our method into NSObject
//  if not CreateClassDefinition(Str_TPrivateCocoaCarbonTrayIcon, Str_NSObject) then WriteLn('Failed to create objc class');

  inherited Create;
end;

class function TPrivateCocoaCarbonTrayIcon.getClass: objc.id;
begin
  Result := objc_getClass(Str_NSObject);
end;

{Removes/replaces all occurences of a character from a string}
function TPrivateCocoaCarbonTrayIcon.TrimAllChar(const S: string; const ch: Char): string;
var
  buf: string;
begin
  buf := S;
  Result := '';
  {while Pos finds a blank}
  while (Pos(ch, buf) > 0) do
    begin
      {copy the substrings before the blank in to Result}
      Result := Result + Copy(buf, 1, Pos(ch, buf) - 1);
      buf := Copy(buf, Pos(ch, buf) + 1, Length(buf) - Pos(ch, buf));
    end;
  {There will still be a remainder in buf, so copy remainder into Result}
  Result := Result + buf;
end;

{ Creates a NSMenu structure representing a TPopUpMenu }
function TPrivateCocoaCarbonTrayIcon.CreateMenu(APopUpMenu: TPopUpMenu): NSMenu;
var
  MenuTitle: CFStringRef;
  i, j, index, subindex, subitemindex: Integer;
begin
  MenuTitle := CFStringCreateWithPascalString(nil, '', kCFStringEncodingUTF8);
  Result := NSMenu.initWithTitle(MenuTitle);

  for i := 0 to APopUpMenu.Items.Count - 1 do
  begin
    index := Length(MenuItems);
    SetLength(MenuItems, index + 1);
    MenuItems[index] := CreateMenuItem(APopUpMenu.Items[i], Str_HandleMenuItemClick, Self);
    Result.addItem(MenuItems[index].Handle);

    { If the submenu has a submenu it needs special treatment }
    if APopUpMenu.Items[i].Count > 0 then
    begin
      subindex := Length(SubMenuOwners);
      SetLength(SubMenuOwners, subindex + 1);
      SubMenuOwners[subindex] := NSMenu.initWithTitle(MenuTitle);

      { Add all submenus in this submenu }
      for j := 0 to APopUpMenu.Items[i].Count - 1 do
      begin
        subitemindex := Length(SubMenuItems);
        SetLength(SubMenuItems, subitemindex + 1);
        SubMenuItems[subitemindex] := CreateMenuItem(APopUpMenu.Items[i].Items[j], Str_HandleMenuItemClick, Self);
        SubMenuOwners[subindex].addItem(SubMenuItems[subitemindex].Handle);
      end;

      MenuItems[index].setSubmenu(SubMenuOwners[subindex].Handle);
    end;
  end;
end;

function TPrivateCocoaCarbonTrayIcon.CreateMenuItem(AMenuItem: TMenuItem;
  ACallbackName: string; ACallbackClass: NSObject): NSMenuItem;
var
  ItemText: CFStringRef;
  KeyText: CFStringRef;
begin
  { The MenuItem is a separator }
  if AMenuItem.Caption = '-' then
  begin
    Result := NSMenuItem.separatorItem();
  end
  { A normal menu item }
  else
  begin
    { While creating the menus we ignore the & shortcut identifiers }
    KeyText := CFStringCreateWithPascalString(nil, '', kCFStringEncodingUTF8);
    ItemText := CFStringCreateWithPascalString(nil, TrimAllChar(AMenuItem.Caption, '&'), kCFStringEncodingUTF8);
    {$ifdef VerboseCarbonTrayIcon}
    WriteLn(' ItemText: ', IntToHex(Int64(ItemText), 8), ' ATitle: ', AMenuItem.Caption);
    {$endif}

    Result := NSMenuItem.initWithTitle_action_keyEquivalent(ItemText, nil, KeyText);
    
    { Assign the OnClick event handler }
    Result.setTarget(ACallbackClass.Handle);
    Result.setAction(sel_registerName(PChar(ACallbackName)));

    { We use the Tag to hold the LCL MenuItem
      RepresentedObject was also tried, by it crashed.
      Cocoa probably tryes to use it as a real Cocoa object }
    Result.setTag(PtrInt(AMenuItem));
  end;
end;

{ Here we try to get the LCL MenuItem from the Tag and then call
  it's OnClick method }
class procedure TPrivateCocoaCarbonTrayIcon.HandleMenuItemClick(_self: objc.id;
  _cmd: SEL; sender: objc.id); cdecl;
var
  AMenuItem: NSMenuItem;
  LCLMenu: TMenuItem;
begin
  AMenuItem := NSMenuItem.CreateWithHandle(sender);
  LCLMenu := TMenuItem(PtrInt(AMenuItem.Tag()));
  if (LCLMenu <> nil) and Assigned(LCLMenu.OnClick) then LCLMenu.OnClick(nil);
end;

{ TCarbonWSCustomTrayIcon }

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

  { Shows the icon }

  if APrivateTrayIcon.item <> nil then Exit(True);

  APrivateTrayIcon.item := NSStatusItem.CreateWithHandle(APrivateTrayIcon.bar.statusItemWithLength(NSSquareStatusItemLength));
  APrivateTrayIcon.item.retain();
  APrivateTrayIcon.item.setImage(APrivateTrayIcon.image.Handle);

  { Inserts the menu }

  if ATrayIcon.PopUpMenu <> nil then
  begin
    APrivateTrayIcon.menu := APrivateTrayIcon.CreateMenu(ATrayIcon.PopUpMenu);
    APrivateTrayIcon.item.setMenu(APrivateTrayIcon.menu.Handle);
  end;

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

