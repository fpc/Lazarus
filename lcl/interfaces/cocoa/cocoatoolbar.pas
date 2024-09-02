unit CocoaToolBar;

{$mode objfpc}{$H+}
{$modeswitch objectivec2}

interface

uses
  Classes, SysUtils,
  CocoaAll, CocoaConfig, Cocoa_Extra, CocoaUtils;

type

  TCocoaToolBarItemHandler = procedure ( Sender: id );

  { TCocoaToolBarItem }

  TCocoaToolBarItem = objcclass( NSToolBarItem )
  private
    _handler: TCocoaToolBarItemHandler;
    procedure lclItemAction( sender: id ); message 'lclItemAction:';
  public
    procedure lclSetHandler( handler: TCocoaToolBarItemHandler );
      message 'lclSetHandler:';
  end;

  TCocoaToolBarItemCreator = function ( identifier: String ): NSToolbarItem;
  
  { TCocoaToolBar }

  TCocoaToolBar = objcclass( NSToolBar, NSToolbarDelegateProtocol )
  private
    _itemCreator: TCocoaToolBarItemCreator;
    _defaultItemIdentifiers: NSArray;
    _allowedItemIdentifiers: NSArray;
  public
    function initWithIdentifier( aIdentifier: NSString ): id; override;
    procedure dealloc; override;
  public
    // NSToolbarDelegate
    function toolbar_itemForItemIdentifier_willBeInsertedIntoToolbar(
      toolbar: NSToolbar; itemIdentifier: NSString; flag: ObjCBOOL ): NSToolbarItem;
    function toolbarDefaultItemIdentifiers( toolbar: NSToolbar ): NSArray;
    function toolbarAllowedItemIdentifiers( toolbar: NSToolbar ): NSArray;
  public
    procedure lclSetItemCreator( itemCreator: TCocoaToolBarItemCreator );
      message 'lclSetItemCreator:';
    procedure lclSetDefaultItemIdentifiers( identifiers: NSArray );
      message 'lclSetDefaultItemIdentifiers:';
    procedure lclSetAllowedItemIdentifiers( identifiers: NSArray );
      message 'lclSetAllowedItemIdentifiers:';
  end;

  { TCocoaToolBarUtils }

  TCocoaToolBarUtils = class
  public
    class procedure createToolBar( win: NSWindow );
    class function createToolBarItem( identifier: String; itemsConfig: TCocoaConfigToolBarItems ): NSToolbarItem; overload;
    class function createToolBarItem( lclItemConfig: TCocoaConfigToolBarItem ): NSToolBarItem; overload;
  end;

  function defaultToolBarItemCreatorImplement( identifier: String ): NSToolbarItem;

implementation

class procedure TCocoaToolBarUtils.createToolBar( win: NSWindow );
var
  toolBar: TCocoaToolBar;
  defaultArray: NSArray;
  allowedArray: NSArray;
begin
  if NOT Assigned(CocoaConfigForm.toolBar.itemCreator) then
    Exit;

  defaultArray:= StringArrayFromLCLToNS( CocoaConfigForm.toolBar.defaultItemsIdentifiers );
  allowedArray:= StringArrayFromLCLToNS( CocoaConfigForm.toolBar.allowedItemsIdentifiers );

  toolBar:= TCocoaToolBar.alloc.initWithIdentifier(
    StrToNSString(CocoaConfigForm.toolBar.identifier) );
  toolBar.lclSetDefaultItemIdentifiers( defaultArray );
  toolBar.lclSetAllowedItemIdentifiers( allowedArray );
  toolBar.lclSetItemCreator( TCocoaToolBarItemCreator(CocoaConfigForm.toolBar.itemCreator) );

  win.setToolbar( toolBar );
end;

class function TCocoaToolBarUtils.createToolBarItem( identifier: String;
  itemsConfig: TCocoaConfigToolBarItems ): NSToolbarItem;
var
  i: Integer;
  count: Integer;
begin
  Result:= nil;
  count:= length( itemsConfig );
  for i:=0 to count-1 do begin
    if identifier = itemsConfig[i].identifier then begin
      Result:= self.createToolBarItem( itemsConfig[i] );
      Exit;
    end;
  end;
end;

class function TCocoaToolBarUtils.createToolBarItem( lclItemConfig: TCocoaConfigToolBarItem ): NSToolBarItem;
var
  cocoaItem: TCocoaToolBarItem;
  cocoaIdentifier: NSString;
  cocoaImageName: NSString;
  cocoaLabel: NSString;
  cocoaTips: NSString;
begin
  cocoaIdentifier:= StrToNSString( lclItemConfig.identifier );
  cocoaImageName:= StrToNSString( lclItemConfig.iconName );
  cocoaLabel:= StrToNSString( lclItemConfig.title );
  cocoaTips:= StrToNSString( lclItemConfig.tips );

  cocoaItem:= TCocoaToolBarItem.alloc.initWithItemIdentifier( cocoaIdentifier );
  cocoaItem.setImage( NSImage.imageWithSystemSymbolName_accessibilityDescription(
    cocoaImageName, nil ) );
  cocoaItem.setLabel( cocoaLabel );
  cocoaItem.setPaletteLabel( cocoaLabel );
  cocoaItem.setToolTip( cocoaTips );
  cocoaItem.setBordered( True );
  cocoaItem.setVisibilityPriority( NSToolbarItemVisibilityPriorityHigh );
  cocoaItem.setTarget( cocoaItem );
  cocoaItem.setAction( ObjCSelector('lclItemAction:') );
  cocoaItem.lclSetHandler( TCocoaToolBarItemHandler(lclItemConfig.onClick) );

  Result:= cocoaItem;
end;

function defaultToolBarItemCreatorImplement(identifier: String
  ): NSToolbarItem;
begin
  Result:= TCocoaToolBarUtils.createToolBarItem( identifier, CocoaConfigForm.toolBar.items );
end;

{ TCocoaToolBarItem }

procedure TCocoaToolBarItem.lclItemAction(sender: id);
begin
  _handler( sender );
end;

procedure TCocoaToolBarItem.lclSetHandler(handler: TCocoaToolBarItemHandler);
begin
  _handler:= handler;
end;

{ TCocoaToolBar }

function TCocoaToolBar.initWithIdentifier( aIdentifier: NSString): id;
begin
  Result:= inherited;
  TCocoaToolBar(Result).setDelegate( Result );
end;

procedure TCocoaToolBar.dealloc;
begin
  if Assigned(_defaultItemIdentifiers) then
    _defaultItemIdentifiers.release;
  if Assigned(_allowedItemIdentifiers) then
    _allowedItemIdentifiers.release;
end;

function TCocoaToolBar.toolbar_itemForItemIdentifier_willBeInsertedIntoToolbar(
  toolbar: NSToolbar; itemIdentifier: NSString; flag: ObjCBOOL): NSToolbarItem;
begin
  Result:= _itemCreator( itemIdentifier.UTF8String );
end;

function TCocoaToolBar.toolbarDefaultItemIdentifiers(toolbar: NSToolbar
  ): NSArray;
begin
  Result:= _defaultItemIdentifiers;
end;

function TCocoaToolBar.toolbarAllowedItemIdentifiers(toolbar: NSToolbar
  ): NSArray;
begin
  Result:= _allowedItemIdentifiers;
end;

procedure TCocoaToolBar.lclSetItemCreator(itemCreator: TCocoaToolBarItemCreator
  );
begin
  _itemCreator:= itemCreator;
end;

procedure TCocoaToolBar.lclSetDefaultItemIdentifiers(identifiers: NSArray);
begin
  if Assigned(_defaultItemIdentifiers) then
    _defaultItemIdentifiers.release;
  _defaultItemIdentifiers := identifiers;
  _defaultItemIdentifiers.retain;
end;

procedure TCocoaToolBar.lclSetAllowedItemIdentifiers(identifiers: NSArray);
begin
  if Assigned(_allowedItemIdentifiers) then
    _allowedItemIdentifiers.release;
  _allowedItemIdentifiers := identifiers;
  _allowedItemIdentifiers.retain;
end;

end.

