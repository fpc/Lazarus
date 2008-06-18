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
    { Structural Methods }
    constructor Create; override;
    class function getClass: objc.id; override;
    procedure AddMethods; override;
    { Pascal Methods }
    function CreateMenu(APopUpMenu: TPopUpMenu): NSMenu;
    function CreateMenuItem(ATitle: shortstring;
      ACallbackName: string; ACallbackClass: NSObject): NSMenuItem;
    { Objective-C compatible methods }
    class procedure HandleMenuItemClick(_self: objc.id; _cmd: SEL; sender: objc.id); cdecl; static;
  end;

const
  Str_TPrivateCocoaCarbonTrayIcon = 'TPrivateCocoaCarbonTrayIcon';

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
//  AddMethod(Str_HandleMenuItemClick, 'v@:@', Pointer(HandleMenuItemClick));
end;

constructor TPrivateCocoaCarbonTrayIcon.Create;
var
  fileName: CFStringRef;
begin
  { The class is registered on the Objective-C runtime before the NSObject constructor is called }
  if not CreateClassDefinition(Str_TPrivateCocoaCarbonTrayIcon, Str_NSObject) then WriteLn('Failed to create objc class');

  inherited Create;
end;

class function TPrivateCocoaCarbonTrayIcon.getClass: objc.id;
begin
  Result := objc_getClass(Str_NSObject);
end;

{ Creates a NSMenu structure representing a TPopUpMenu }
function TPrivateCocoaCarbonTrayIcon.CreateMenu(APopUpMenu: TPopUpMenu): NSMenu;
var
  MenuTitle: CFStringRef;
  i: Integer;
begin
  MenuTitle := CFStringCreateWithPascalString(nil, '', kCFStringEncodingUTF8);
  Result := NSMenu.initWithTitle(MenuTitle);

  for i := 0 to APopUpMenu.Items.Count - 1 do
  begin
    SetLength(MenuItems, i + 1);
    MenuItems[i] := CreateMenuItem(APopUpMenu.Items[i].Caption, Str_HandleMenuItemClick, Self);
    Result.addItem(MenuItems[i].Handle);
  end;
end;

function TPrivateCocoaCarbonTrayIcon.CreateMenuItem(ATitle: shortstring;
  ACallbackName: string; ACallbackClass: NSObject): NSMenuItem;
var
  ItemText: CFStringRef;
  KeyText: CFStringRef;
begin
  KeyText := CFStringCreateWithPascalString(nil, '', kCFStringEncodingUTF8);
  ItemText := CFStringCreateWithPascalString(nil, ATitle, kCFStringEncodingUTF8);
  WriteLn(' ItemText: ', IntToHex(Int64(ItemText), 8), ' ATitle: ', ATitle);

  Result := NSMenuItem.initWithTitle_action_keyEquivalent(ItemText, nil, KeyText);
  Result.setTarget(ACallbackClass.Handle);
  Result.setAction(sel_registerName(PChar(ACallbackName)));
end;

class procedure TPrivateCocoaCarbonTrayIcon.HandleMenuItemClick(_self: objc.id;
  _cmd: SEL; sender: objc.id); cdecl;
begin

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

