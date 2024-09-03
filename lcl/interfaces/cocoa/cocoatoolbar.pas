unit CocoaToolBar;

{$mode objfpc}{$H+}
{$interfaces corba}
{$modeswitch objectivec2}

interface

uses
  Classes, SysUtils,
  CocoaAll, CocoaConfig, CocoaMenus, Cocoa_Extra, CocoaUtils;

type

  TCocoaToolBarItemHandler = procedure ( Sender: id );

  { TCocoaConfigToolBarItemClass }

  TCocoaConfigToolBarItemClass = class( TCocoaConfigToolBarItemInterface )
  private
    _itemConfig: TCocoaConfigToolBarItem;
  public
    constructor Create( itemConfig: TCocoaConfigToolBarItem );
    function createItem: NSToolBarItem;
    function identifier: String;
    function iconName: String;
    function title: String;
    function tips: String;
    function onClick: Pointer;
  end;

  { TCocoaConfigToolBarItemSharingClass }

  TCocoaConfigToolBarItemSharingClass = class( TCocoaConfigToolBarItemInterface )
  private
    _itemConfig: TCocoaConfigToolBarItemSharing;
  public
    constructor Create( itemConfig: TCocoaConfigToolBarItemSharing );
    function createItem: NSToolBarItem;
    function identifier: String;
    function iconName: String;
    function title: String;
    function tips: String;
    function onClick: Pointer;
  end;

  { TCocoaConfigToolBarItemSearchClass }

  TCocoaConfigToolBarItemSearchClass = class( TCocoaConfigToolBarItemInterface )
  private
    _itemConfig: TCocoaConfigToolBarItemSearch;
  public
    constructor Create( itemConfig: TCocoaConfigToolBarItemSearch );
    function createItem: NSToolBarItem;
    function identifier: String;
    function iconName: String;
    function title: String;
    function tips: String;
    function onClick: Pointer;
  end;

  { TCocoaConfigToolBarItemMenuClass }

  TCocoaConfigToolBarItemMenuClass = class( TCocoaConfigToolBarItemInterface )
  private
    _itemConfig: TCocoaConfigToolBarItemMenu;
  public
    constructor Create( itemConfig: TCocoaConfigToolBarItemMenu );
    function createItem: NSToolBarItem;
    function identifier: String;
    function iconName: String;
    function title: String;
    function tips: String;
    function onClick: Pointer;
  end;

  { TCocoaConfigToolBarItemGroupClass }

  TCocoaConfigToolBarItemGroupClass = class( TCocoaConfigToolBarItemInterface )
  private
    _itemConfig: TCocoaConfigToolBarItemGroup;
  public
    constructor Create( itemConfig: TCocoaConfigToolBarItemGroup );
    function createItem: NSToolBarItem;
    function identifier: String;
    function iconName: String;
    function title: String;
    function tips: String;
    function onClick: Pointer;

    function representation: NSToolbarItemGroupControlRepresentation;
    function selectionMode: NSToolbarItemGroupSelectionMode;
    function selectedIndex: NSInteger;
    function subitems: TCocoaConfigToolBarItems;
  end;

  { TCocoaToolBarItem }

  TCocoaToolBarItem = objcclass( NSToolBarItem )
  private
    _handler: TCocoaToolBarItemHandler;
    procedure lclItemAction( sender: id ); message 'lclItemAction:';
  public
    procedure lclSetHandler( handler: TCocoaToolBarItemHandler );
      message 'lclSetHandler:';
  end;

  { TCocoaToolBarItemSharing }

  TCocoaToolBarItemSharingOnGetItems = function ( item: NSToolBarItem ): TStringArray;

  TCocoaToolBarItemSharingDelegate = objcclass( NSObject, NSSharingServicePickerToolbarItemDelegateProtocol )
  private
    _onGetItems: TCocoaToolBarItemSharingOnGetItems;
  public
    function itemsForSharingServicePickerToolbarItem(
      pickerToolbarItem: NSSharingServicePickerToolbarItem ): NSArray;
  public
    procedure lclSetOnGetItems( onGetItems: TCocoaToolBarItemSharingOnGetItems );
      message 'lclSetOnGetItems:';
  end;

  TCocoaToolBarItemSharing  = objcclass( NSSharingServicePickerToolbarItem )
  public
    procedure dealloc; override;
  end;

  { TCocoaToolBarItemSearch }

  TCocoaToolBarItemSearch = objcclass( NSSearchToolBarItem, NSSearchFieldDelegateProtocol )
  private
    _isSearching: Boolean;
    _handler: TCocoaToolBarItemHandler;
    procedure lclItemAction( sender: id ); message 'lclItemAction:';
  public
    procedure searchFieldDidStartSearching( sender: NSSearchField );
    procedure searchFieldDidEndSearching( sender: NSSearchField );
  public
    procedure lclSetHandler( handler: TCocoaToolBarItemHandler );
      message 'lclSetHandler:';
  end;

  { TCocoaToolBarItemMenu }

  TCocoaToolBarItemMenu = objcclass( NSMenuToolBarItem )
  private
    _handler: TCocoaToolBarItemHandler;
    procedure lclItemAction( sender: id ); message 'lclItemAction:';
  public
    procedure lclSetHandler( handler: TCocoaToolBarItemHandler );
      message 'lclSetHandler:';
  end;

  { TCocoaToolBarItemGroupWrapper }

  TCocoaToolBarItemGroupWrapper = objcclass( NSObject )
  private
    _itemGroup: NSToolbarItemGroup;
    _handler: TCocoaToolBarItemHandler;
    procedure lclItemAction( sender: id ); message 'lclItemAction:';
    function lclGetSelectedItem: NSToolBarItem; message 'lclGetSelectedItem';
  public
    procedure lclSetHandler( handler: TCocoaToolBarItemHandler );
      message 'lclSetHandler:';
    procedure lclSetItemGroup( itemGroup: NSToolbarItemGroup );
      message 'lclSetItemGroup:';
    procedure lclSetSelectedIndex( index: NSInteger );
      message 'lclSetSelectedIndex:';
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
  private
    class procedure initItemCommonData( item: NSToolBarItem; itemConfig: TCocoaConfigToolBarItemInterface );
  public
    class function forItem( itemsConfig: TCocoaConfigToolBarItem ): TCocoaConfigToolBarItemInterface;
    class function forItem( itemsConfig: TCocoaConfigToolBarItemSharing ): TCocoaConfigToolBarItemInterface;
    class function forItem( itemsConfig: TCocoaConfigToolBarItemSearch ): TCocoaConfigToolBarItemInterface;
    class function forItem( itemsConfig: TCocoaConfigToolBarItemMenu ): TCocoaConfigToolBarItemInterface;
    class function forItem( itemsConfig: TCocoaConfigToolBarItemGroup ): TCocoaConfigToolBarItemInterface;
    class function createItem( identifier: String; itemsConfig: TCocoaConfigToolBarItems ): NSToolbarItem;
    class procedure createToolBar( win: NSWindow );
  end;

  function defaultToolBarItemCreatorImplement( identifier: String ): NSToolbarItem;

implementation

class procedure TCocoaToolBarUtils.initItemCommonData( item: NSToolBarItem;
  itemConfig: TCocoaConfigToolBarItemInterface );
var
  cocoaImageName: NSString;
  cocoaLabel: NSString;
  cocoaTips: NSString;
begin
  cocoaImageName:= StrToNSString( itemConfig.iconName );
  cocoaLabel:= StrToNSString( itemConfig.title );
  cocoaTips:= StrToNSString( itemConfig.tips );

  if cocoaImageName.length > 0 then begin
    item.setImage( NSImage.imageWithSystemSymbolName_accessibilityDescription(
      cocoaImageName, nil ) );
  end;
  item.setLabel( cocoaLabel );
  item.setPaletteLabel( cocoaLabel );
  item.setToolTip( cocoaTips );
  item.setBordered( True );
  item.setVisibilityPriority( NSToolbarItemVisibilityPriorityHigh );
  item.setTarget( item );
  item.setAction( ObjCSelector('lclItemAction:') );
end;

class function TCocoaToolBarUtils.forItem(itemsConfig: TCocoaConfigToolBarItem
  ): TCocoaConfigToolBarItemInterface;
begin
  Result:= TCocoaConfigToolBarItemClass.Create( itemsConfig );
end;

class function TCocoaToolBarUtils.forItem( itemsConfig: TCocoaConfigToolBarItemSharing
  ): TCocoaConfigToolBarItemInterface;
begin
  Result:= TCocoaConfigToolBarItemSharingClass.Create( itemsConfig );
end;

class function TCocoaToolBarUtils.forItem(
  itemsConfig: TCocoaConfigToolBarItemSearch): TCocoaConfigToolBarItemInterface;
begin
  Result:= TCocoaConfigToolBarItemSearchClass.Create( itemsConfig );
end;

class function TCocoaToolBarUtils.forItem(
  itemsConfig: TCocoaConfigToolBarItemMenu): TCocoaConfigToolBarItemInterface;
begin
  Result:= TCocoaConfigToolBarItemMenuClass.Create( itemsConfig );
end;

class function TCocoaToolBarUtils.forItem(
  itemsConfig: TCocoaConfigToolBarItemGroup): TCocoaConfigToolBarItemInterface;
begin
  Result:= TCocoaConfigToolBarItemGroupClass.Create( itemsConfig );
end;

class function TCocoaToolBarUtils.createItem( identifier: String;
  itemsConfig: TCocoaConfigToolBarItems ): NSToolbarItem;
var
  i: Integer;
  count: Integer;
begin
  Result:= nil;
  count:= length( itemsConfig );
  for i:=0 to count-1 do begin
    if identifier = itemsConfig[i].identifier then begin
      Result:= itemsConfig[i].createItem;
      Exit;
    end;
  end;
end;

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
  toolBar.setAllowsUserCustomization( True );
  toolBar.lclSetDefaultItemIdentifiers( defaultArray );
  toolBar.lclSetAllowedItemIdentifiers( allowedArray );
  toolBar.lclSetItemCreator( TCocoaToolBarItemCreator(CocoaConfigForm.toolBar.itemCreator) );

  win.setToolbar( toolBar );
end;

function defaultToolBarItemCreatorImplement(identifier: String
  ): NSToolbarItem;
begin
  Result:= TCocoaToolBarUtils.createItem( identifier, CocoaConfigForm.toolBar.items );
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

{ TCocoaToolBarItemSharing }

function TCocoaToolBarItemSharingDelegate.itemsForSharingServicePickerToolbarItem(
  pickerToolbarItem: NSSharingServicePickerToolbarItem): NSArray;
var
  lclArray: TStringArray;
begin
  lclArray:= _onGetItems( pickerToolbarItem );
  Result:= UrlArrayFromLCLToNS( lclArray );
end;

procedure TCocoaToolBarItemSharingDelegate.lclSetOnGetItems(
  onGetItems: TCocoaToolBarItemSharingOnGetItems );
begin
  _onGetItems:= onGetItems;
end;

procedure TCocoaToolBarItemSharing.dealloc;
begin
  NSObject(delegate).release;
  Inherited;
end;

{ TCocoaToolBarItemSearch }

procedure TCocoaToolBarItemSearch.lclItemAction(sender: id);
begin
  if _isSearching then
    _handler( sender );
end;

procedure TCocoaToolBarItemSearch.searchFieldDidStartSearching(
  sender: NSSearchField);
begin
  _isSearching:= True;
end;

procedure TCocoaToolBarItemSearch.searchFieldDidEndSearching(
  sender: NSSearchField);
begin
  _isSearching:= False;
end;

procedure TCocoaToolBarItemSearch.lclSetHandler(
  handler: TCocoaToolBarItemHandler);
begin
  _handler:= handler;
end;

{ TCocoaToolBarItemMenu }

procedure TCocoaToolBarItemMenu.lclItemAction(sender: id);
begin
  _handler( sender );
end;

procedure TCocoaToolBarItemMenu.lclSetHandler(handler: TCocoaToolBarItemHandler);
begin
  _handler:= handler;
end;

{ TCocoaToolBarItemGroupWrapper }

procedure TCocoaToolBarItemGroupWrapper.lclItemAction(sender: id);
var
  index: NSInteger;
begin
  index:= _itemGroup.selectedIndex;
  self.lclSetSelectedIndex( index );
  _handler( self.lclGetSelectedItem );
end;

function TCocoaToolBarItemGroupWrapper.lclGetSelectedItem: NSToolBarItem;
var
  index: NSInteger;
begin
  Result:= nil;
  index:= _itemGroup.selectedIndex;
  if (index>=0) and (index<_itemGroup.subitems.count) then
    Result:= _itemGroup.subitems.objectAtIndex( index );
end;

procedure TCocoaToolBarItemGroupWrapper.lclSetHandler(handler: TCocoaToolBarItemHandler
  );
begin
  _handler:= handler;
end;

procedure TCocoaToolBarItemGroupWrapper.lclSetItemGroup(
  itemGroup: NSToolbarItemGroup);
begin
  _itemGroup:= itemGroup;
end;

procedure TCocoaToolBarItemGroupWrapper.lclSetSelectedIndex(index: NSInteger);
var
  item: NSToolBarItem;
begin
  if (index>=0) and (index<_itemGroup.subitems.count) then begin
    _itemGroup.setSelected_atIndex( True, index );
    item:= self.lclGetSelectedItem;
    _itemGroup.setImage( item.image );
  end;
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

{ TCocoaConfigToolBarItemClass }

constructor TCocoaConfigToolBarItemClass.Create(
  itemConfig: TCocoaConfigToolBarItem);
begin
  _itemConfig:= itemConfig;
end;

function TCocoaConfigToolBarItemClass.createItem: NSToolBarItem;
var
  cocoaIdentifier: NSString;
  cocoaItem: TCocoaToolBarItem;
begin
  cocoaIdentifier:= StrToNSString( _itemConfig.identifier );
  cocoaItem:= TCocoaToolBarItem.alloc.initWithItemIdentifier( cocoaIdentifier );
  TCocoaToolBarUtils.initItemCommonData( cocoaItem, self );
  cocoaItem.lclSetHandler( TCocoaToolBarItemHandler(self.onClick) );
  cocoaItem.autorelease;
  Result:= cocoaItem;
end;

function TCocoaConfigToolBarItemClass.identifier: String;
begin
  Result:= _itemConfig.identifier;
end;

function TCocoaConfigToolBarItemClass.iconName: String;
begin
  Result:= _itemConfig.iconName;
end;

function TCocoaConfigToolBarItemClass.title: String;
begin
  Result:= _itemConfig.title;
end;

function TCocoaConfigToolBarItemClass.tips: String;
begin
  Result:= _itemConfig.tips;
end;

function TCocoaConfigToolBarItemClass.onClick: Pointer;
begin
  Result:= _itemConfig.onClick;
end;

{ TCocoaConfigToolBarItemClass }

constructor TCocoaConfigToolBarItemSharingClass.Create(
  itemConfig: TCocoaConfigToolBarItemSharing );
begin
  _itemConfig:= itemConfig;
end;

function TCocoaConfigToolBarItemSharingClass.createItem: NSToolBarItem;
var
  cocoaIdentifier: NSString;
  cocoaItem: TCocoaToolBarItemSharing;
  cocoaItemDelegate: TCocoaToolBarItemSharingDelegate;
begin
  cocoaIdentifier:= StrToNSString( _itemConfig.identifier );
  cocoaItem:= TCocoaToolBarItemSharing.alloc.initWithItemIdentifier( cocoaIdentifier );
  TCocoaToolBarUtils.initItemCommonData( cocoaItem, self );
  cocoaItem.setAutovalidates( False );

  // release in TCocoaToolBarItemSharing
  cocoaItemDelegate:= TCocoaToolBarItemSharingDelegate.new;
  cocoaItemDelegate.lclSetOnGetItems( TCocoaToolBarItemSharingOnGetItems(_itemConfig.onGetItems) );
  cocoaItem.setDelegate( cocoaItemDelegate );

  cocoaItem.autorelease;
  Result:= cocoaItem;
end;

function TCocoaConfigToolBarItemSharingClass.identifier: String;
begin
  Result:= _itemConfig.identifier;
end;

function TCocoaConfigToolBarItemSharingClass.iconName: String;
begin
  Result:= _itemConfig.iconName;
end;

function TCocoaConfigToolBarItemSharingClass.title: String;
begin
  Result:= _itemConfig.title;
end;

function TCocoaConfigToolBarItemSharingClass.tips: String;
begin
  Result:= _itemConfig.tips;
end;

function TCocoaConfigToolBarItemSharingClass.onClick: Pointer;
begin
  // NSSharingServicePickerToolbarItem fully handle all action
  Result:= nil;
end;

{ TCocoaConfigToolBarItemSearchClass }

constructor TCocoaConfigToolBarItemSearchClass.Create(
  itemConfig: TCocoaConfigToolBarItemSearch );
begin
  _itemConfig:= itemConfig;
end;

function TCocoaConfigToolBarItemSearchClass.createItem: NSToolBarItem;
var
  cocoaIdentifier: NSString;
  cocoaItem: TCocoaToolBarItemSearch;
begin
  cocoaIdentifier:= StrToNSString( _itemConfig.identifier );
  cocoaItem:= TCocoaToolBarItemSearch.alloc.initWithItemIdentifier( cocoaIdentifier );
  cocoaItem.searchField.setSendsWholeSearchString( _itemConfig.sendWhole );
  cocoaItem.searchField.setSendsSearchStringImmediately( _itemConfig.sendImmediately );
  cocoaItem.searchField.setDelegate( NSTextFieldDelegateProtocol(cocoaItem) );
  cocoaItem.setResignsFirstResponderWithCancel( _itemConfig.resignsWithCancel );
  if _itemConfig.preferredWidth > 0 then
    cocoaItem.setPreferredWidthForSearchField( _itemConfig.preferredWidth );
  TCocoaToolBarUtils.initItemCommonData( cocoaItem, self );
  cocoaItem.lclSetHandler( TCocoaToolBarItemHandler(self.onClick) );
  cocoaItem.autorelease;
  Result:= cocoaItem;
end;

function TCocoaConfigToolBarItemSearchClass.identifier: String;
begin
  Result:= _itemConfig.identifier;
end;

function TCocoaConfigToolBarItemSearchClass.iconName: String;
begin
  Result:= _itemConfig.iconName;
end;

function TCocoaConfigToolBarItemSearchClass.title: String;
begin
  Result:= _itemConfig.title;
end;

function TCocoaConfigToolBarItemSearchClass.tips: String;
begin
  Result:= _itemConfig.tips;
end;

function TCocoaConfigToolBarItemSearchClass.onClick: Pointer;
begin
  Result:= _itemConfig.onClick;
end;

{ TCocoaConfigToolBarItemMenuClass }

constructor TCocoaConfigToolBarItemMenuClass.Create(
  itemConfig: TCocoaConfigToolBarItemMenu );
begin
  _itemConfig:= itemConfig;
end;

function TCocoaConfigToolBarItemMenuClass.createItem: NSToolBarItem;
var
  cocoaIdentifier: NSString;
  cocoaItem: TCocoaToolBarItemMenu;
  cocoaMenu: NSMenu;
begin
  cocoaIdentifier:= StrToNSString( _itemConfig.identifier );
  cocoaItem:= TCocoaToolBarItemMenu.alloc.initWithItemIdentifier( cocoaIdentifier );
  cocoaItem.setShowsIndicator( _itemConfig.showsIndicator );
  TCocoaToolBarUtils.initItemCommonData( cocoaItem, self );
  if Assigned(self.onClick) then
    cocoaItem.lclSetHandler( TCocoaToolBarItemHandler(self.onClick) )
  else
    cocoaItem.setAction( nil );

  cocoaMenu:= NSMenu.new;
  NSMenuAddItemsFromLCLMenu( cocoaMenu, _itemConfig.menu );
  cocoaItem.setMenu( cocoaMenu );
  cocoaMenu.autorelease;

  cocoaItem.autorelease;
  Result:= cocoaItem;
end;

function TCocoaConfigToolBarItemMenuClass.identifier: String;
begin
  Result:= _itemConfig.identifier;
end;

function TCocoaConfigToolBarItemMenuClass.iconName: String;
begin
  Result:= _itemConfig.iconName;
end;

function TCocoaConfigToolBarItemMenuClass.title: String;
begin
  Result:= _itemConfig.title;
end;

function TCocoaConfigToolBarItemMenuClass.tips: String;
begin
  Result:= _itemConfig.tips;
end;

function TCocoaConfigToolBarItemMenuClass.onClick: Pointer;
begin
  Result:= _itemConfig.onClick;
end;

{ TCocoaConfigToolBarItemGroupClass }

constructor TCocoaConfigToolBarItemGroupClass.Create(
  itemConfig: TCocoaConfigToolBarItemGroup);
begin
  _itemConfig:= itemConfig;
end;

function TCocoaConfigToolBarItemGroupClass.createItem: NSToolBarItem;
var
  groupWrapper: TCocoaToolBarItemGroupWrapper;
  toolbarItem: NSToolbarItemGroup;
  images: NSMutableArray;
  labels: NSMutableArray;

  procedure initSubitems;
  var
    i: Integer;
    count: Integer;
  begin
    count:= length( self.subitems );
    images:= NSMutableArray.arrayWithCapacity( count );
    labels:= NSMutableArray.arrayWithCapacity( count );
    for i:=0 to count-1 do begin
      images.addObject( NSImage.imageWithSystemSymbolName_accessibilityDescription(
                          StrToNSString(self.subitems[i].iconName), nil) );
      labels.addObject( StrToNSString(self.subitems[i].title) );
    end;
  end;

begin
  initSubitems;
  toolbarItem:= NSToolbarItemGroup.groupWithItemIdentifier_images_selectionMode_labels_target_action(
    StrToNSString( self.identifier ),
    images,
    self.selectionMode,
    labels,
    nil,
    nil );

  groupWrapper:= TCocoaToolBarItemGroupWrapper.new;
  groupWrapper.lclSetItemGroup( toolbarItem );
  groupWrapper.lclSetSelectedIndex( self.selectedIndex );
  groupWrapper.lclSetHandler( TCocoaToolBarItemHandler(self.onClick) );
  TCocoaToolBarUtils.initItemCommonData( toolbarItem, self );
  toolbarItem.setTarget( groupWrapper );
  toolbarItem.setAction( ObjCSelector('lclItemAction:') );
  Result:= toolbarItem;
end;

function TCocoaConfigToolBarItemGroupClass.identifier: String;
begin
  Result:= _itemConfig.identifier;
end;

function TCocoaConfigToolBarItemGroupClass.iconName: String;
begin
  Result:= _itemConfig.iconName;
end;

function TCocoaConfigToolBarItemGroupClass.title: String;
begin
  Result:= _itemConfig.title;
end;

function TCocoaConfigToolBarItemGroupClass.tips: String;
begin
  Result:= _itemConfig.tips;
end;

function TCocoaConfigToolBarItemGroupClass.onClick: Pointer;
begin
  Result:= _itemConfig.onClick;
end;

function TCocoaConfigToolBarItemGroupClass.representation: NSToolbarItemGroupControlRepresentation;
begin
  Result:= _itemConfig.representation;
end;

function TCocoaConfigToolBarItemGroupClass.selectionMode: NSToolbarItemGroupSelectionMode;
begin
  Result:= _itemConfig.selectionMode;
end;

function TCocoaConfigToolBarItemGroupClass.selectedIndex: NSInteger;
begin
  Result:= _itemConfig.selectedIndex;
end;

function TCocoaConfigToolBarItemGroupClass.subitems: TCocoaConfigToolBarItems;
begin
  Result:= _itemConfig.subitems;
end;

end.

