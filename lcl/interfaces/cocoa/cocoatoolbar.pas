unit CocoaToolBar;

{$mode objfpc}{$H+}
{$interfaces corba}
{$modeswitch objectivec2}

interface

uses
  Classes, SysUtils,
  Menus,
  CocoaAll, CocoaConfig, CocoaMenus, Cocoa_Extra, CocoaUtils;

type
  TCocoaToolBarItemHandler = procedure ( Sender: id );
  TCocoaToolBarItemSharingOnGetItems = function ( item: NSToolBarItem ): TStringArray;
  PCocoaConfigToolBarItem = ^TCocoaConfigToolBarItem;

  { TCocoaConfigToolBarItemAdapterBase }

  TCocoaConfigToolBarItemAdapterBase = class( TCocoaConfigToolBarItemAdapter )
  protected
    _identifier: String;
    _iconName: String;
    _title: String;
    _tips: String;
    _onClick: Pointer;
  protected
    procedure initItemConfig( pItemConfig: PCocoaConfigToolBarItem );
  public
    constructor Create( itemConfig: TCocoaConfigToolBarItem );
    function createItem: NSToolBarItem; virtual;
    function identifier: String; virtual;
    function iconName: String; virtual;
    function title: String; virtual;
    function tips: String; virtual;
    function onClick: Pointer; virtual;
  end;

  { TCocoaConfigToolBarItemAdapterSharing }

  TCocoaConfigToolBarItemAdapterSharing = class( TCocoaConfigToolBarItemAdapterBase )
  protected
    _onGetItems: TCocoaToolBarItemSharingOnGetItems;
  public
    constructor Create( itemConfig: TCocoaConfigToolBarItemSharing );
    function createItem: NSToolBarItem; override;
    function onGetItems: TCocoaToolBarItemSharingOnGetItems;
  end;

  { TCocoaConfigToolBarItemAdapterSearch }

  TCocoaConfigToolBarItemAdapterSearch = class( TCocoaConfigToolBarItemAdapterBase )
  protected
    _sendWhole: Boolean;
    _sendImmediately: Boolean;
    _resignsWithCancel: Boolean;
    _preferredWidth: Double;
  public
    constructor Create( itemConfig: TCocoaConfigToolBarItemSearch );
    function createItem: NSToolBarItem; override;
    function sendWhole: Boolean;
    function sendImmediately: Boolean;
    function resignsWithCancel: Boolean;
    function preferredWidth: Double;
  end;

  { TCocoaConfigToolBarItemAdapterMenu }

  TCocoaConfigToolBarItemAdapterMenu = class( TCocoaConfigToolBarItemAdapterBase )
  protected
    _showsIndicator: Boolean;
    _menu: TMenuItem;
  public
    constructor Create( itemConfig: TCocoaConfigToolBarItemMenu );
    function createItem: NSToolBarItem; override;
    function showsIndicator: Boolean;
    function menu: TMenuItem;
  end;

  { TCocoaConfigToolBarItemAdapterGroup }

  TCocoaConfigToolBarItemAdapterGroup = class( TCocoaConfigToolBarItemAdapterBase )
  protected
    _representation: NSToolbarItemGroupControlRepresentation;
    _selectionMode: NSToolbarItemGroupSelectionMode;
    _selectedIndex: NSInteger;
    _subitems: TCocoaConfigToolBarItems;
  public
    constructor Create( itemConfig: TCocoaConfigToolBarItemGroup );
    function createItem: NSToolBarItem; override;
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
    class procedure initItemCommonData( item: NSToolBarItem; itemConfig: TCocoaConfigToolBarItemAdapter );
  public
    class function forItem( itemConfig: TCocoaConfigToolBarItem ):
      TCocoaConfigToolBarItemAdapter;
    class function forItem( itemConfig: TCocoaConfigToolBarItemSharing ):
      TCocoaConfigToolBarItemAdapter;
    class function forItem( itemConfig: TCocoaConfigToolBarItemSearch ):
      TCocoaConfigToolBarItemAdapter;
    class function forItem( itemConfig: TCocoaConfigToolBarItemMenu ):
      TCocoaConfigToolBarItemAdapter;
    class function forItem( itemConfig: TCocoaConfigToolBarItemGroup ):
      TCocoaConfigToolBarItemAdapter;
    class function createItem( identifier: String; itemsConfig: TCocoaConfigToolBarItems ): NSToolbarItem;
    class procedure createToolBar( win: NSWindow );
  end;

  function defaultToolBarItemCreatorImplement( identifier: String ): NSToolbarItem;

implementation

class procedure TCocoaToolBarUtils.initItemCommonData( item: NSToolBarItem;
  itemConfig: TCocoaConfigToolBarItemAdapter );
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

class function TCocoaToolBarUtils.forItem(itemConfig: TCocoaConfigToolBarItem
  ): TCocoaConfigToolBarItemAdapter;
begin
  Result:= TCocoaConfigToolBarItemAdapterBase.Create( itemConfig );
end;

class function TCocoaToolBarUtils.forItem( itemConfig: TCocoaConfigToolBarItemSharing
  ): TCocoaConfigToolBarItemAdapter;
begin
  Result:= TCocoaConfigToolBarItemAdapterSharing.Create( itemConfig );
end;

class function TCocoaToolBarUtils.forItem( itemConfig: TCocoaConfigToolBarItemSearch
  ): TCocoaConfigToolBarItemAdapter;
begin
  Result:= TCocoaConfigToolBarItemAdapterSearch.Create( itemConfig );
end;

class function TCocoaToolBarUtils.forItem( itemConfig: TCocoaConfigToolBarItemMenu
  ): TCocoaConfigToolBarItemAdapter;
begin
  Result:= TCocoaConfigToolBarItemAdapterMenu.Create( itemConfig );
end;

class function TCocoaToolBarUtils.forItem( itemConfig: TCocoaConfigToolBarItemGroup
  ): TCocoaConfigToolBarItemAdapter;
begin
  Result:= TCocoaConfigToolBarItemAdapterGroup.Create( itemConfig );
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

{ TCocoaConfigToolBarItemAdapterBase }

procedure TCocoaConfigToolBarItemAdapterBase.initItemConfig(
  pItemConfig: PCocoaConfigToolBarItem);
begin
  _identifier:= pItemConfig^.identifier;
  _iconName:= pItemConfig^.iconName;
  _title:= pItemConfig^.title;
  _tips:= pItemConfig^.tips;
  _onClick:= pItemConfig^.onClick;
end;

constructor TCocoaConfigToolBarItemAdapterBase.Create(
  itemConfig: TCocoaConfigToolBarItem );
begin
  self.initItemConfig( @itemConfig );
end;

function TCocoaConfigToolBarItemAdapterBase.createItem: NSToolBarItem;
var
  cocoaIdentifier: NSString;
  cocoaItem: TCocoaToolBarItem;
begin
  cocoaIdentifier:= StrToNSString( self.identifier );
  cocoaItem:= TCocoaToolBarItem.alloc.initWithItemIdentifier( cocoaIdentifier );
  TCocoaToolBarUtils.initItemCommonData( cocoaItem, self );
  cocoaItem.lclSetHandler( TCocoaToolBarItemHandler(self.onClick) );
  cocoaItem.autorelease;
  Result:= cocoaItem;
end;

function TCocoaConfigToolBarItemAdapterBase.identifier: String;
begin
  Result:= _identifier;
end;

function TCocoaConfigToolBarItemAdapterBase.iconName: String;
begin
  Result:= _iconName;
end;

function TCocoaConfigToolBarItemAdapterBase.title: String;
begin
  Result:= _title;
end;

function TCocoaConfigToolBarItemAdapterBase.tips: String;
begin
  Result:= _tips;
end;

function TCocoaConfigToolBarItemAdapterBase.onClick: Pointer;
begin
  Result:= _onClick;
end;

{ TCocoaConfigToolBarItemAdapterSharing }

constructor TCocoaConfigToolBarItemAdapterSharing.Create(
  itemConfig: TCocoaConfigToolBarItemSharing);
begin
  self.initItemConfig( @itemConfig );
  _onGetItems:= TCocoaToolBarItemSharingOnGetItems( itemConfig.onGetItems );
end;

function TCocoaConfigToolBarItemAdapterSharing.createItem: NSToolBarItem;
var
  cocoaIdentifier: NSString;
  cocoaItem: TCocoaToolBarItemSharing;
  cocoaItemDelegate: TCocoaToolBarItemSharingDelegate;
begin
  cocoaIdentifier:= StrToNSString( self.identifier );
  cocoaItem:= TCocoaToolBarItemSharing.alloc.initWithItemIdentifier( cocoaIdentifier );
  TCocoaToolBarUtils.initItemCommonData( cocoaItem, self );
  cocoaItem.setAutovalidates( False );

  // release in TCocoaToolBarItemSharing
  cocoaItemDelegate:= TCocoaToolBarItemSharingDelegate.new;
  cocoaItemDelegate.lclSetOnGetItems( self.onGetItems );
  cocoaItem.setDelegate( cocoaItemDelegate );

  cocoaItem.autorelease;
  Result:= cocoaItem;
end;

function TCocoaConfigToolBarItemAdapterSharing.onGetItems: TCocoaToolBarItemSharingOnGetItems;
begin
  Result:= _onGetItems;
end;

{ TCocoaConfigToolBarItemAdapterSearch }

constructor TCocoaConfigToolBarItemAdapterSearch.Create(
  itemConfig: TCocoaConfigToolBarItemSearch);
begin
  self.initItemConfig( @itemConfig );
  _sendWhole:= itemConfig.sendWhole;
  _sendImmediately:= itemConfig.sendImmediately;
  _resignsWithCancel:= itemConfig.resignsWithCancel;
  _preferredWidth:= itemConfig.preferredWidth;
end;

function TCocoaConfigToolBarItemAdapterSearch.createItem: NSToolBarItem;
var
  cocoaIdentifier: NSString;
  cocoaItem: TCocoaToolBarItemSearch;
begin
  cocoaIdentifier:= StrToNSString( self.identifier );
  cocoaItem:= TCocoaToolBarItemSearch.alloc.initWithItemIdentifier( cocoaIdentifier );
  cocoaItem.searchField.setSendsWholeSearchString( self.sendWhole );
  cocoaItem.searchField.setSendsSearchStringImmediately( self.sendImmediately );
  cocoaItem.searchField.setDelegate( NSTextFieldDelegateProtocol(cocoaItem) );
  cocoaItem.setResignsFirstResponderWithCancel( self.resignsWithCancel );
  if self.preferredWidth > 0 then
    cocoaItem.setPreferredWidthForSearchField( self.preferredWidth );
  TCocoaToolBarUtils.initItemCommonData( cocoaItem, self );
  cocoaItem.lclSetHandler( TCocoaToolBarItemHandler(self.onClick) );
  cocoaItem.autorelease;
  Result:= cocoaItem;
end;

function TCocoaConfigToolBarItemAdapterSearch.sendWhole: Boolean;
begin
  Result:= _sendWhole;
end;

function TCocoaConfigToolBarItemAdapterSearch.sendImmediately: Boolean;
begin
  Result:= _sendImmediately;
end;

function TCocoaConfigToolBarItemAdapterSearch.resignsWithCancel: Boolean;
begin
  Result:= _resignsWithCancel;
end;

function TCocoaConfigToolBarItemAdapterSearch.preferredWidth: Double;
begin
  Result:= _preferredWidth;
end;

{ TCocoaConfigToolBarItemAdapterMenu }

constructor TCocoaConfigToolBarItemAdapterMenu.Create(
  itemConfig: TCocoaConfigToolBarItemMenu);
begin
  self.initItemConfig( @itemConfig );
  _showsIndicator:= itemConfig.showsIndicator;
  _menu:= itemConfig.menu;
end;

function TCocoaConfigToolBarItemAdapterMenu.createItem: NSToolBarItem;
var
  cocoaIdentifier: NSString;
  cocoaItem: TCocoaToolBarItemMenu;
  cocoaMenu: NSMenu;
begin
  cocoaIdentifier:= StrToNSString( self.identifier );
  cocoaItem:= TCocoaToolBarItemMenu.alloc.initWithItemIdentifier( cocoaIdentifier );
  cocoaItem.setShowsIndicator( self.showsIndicator );
  TCocoaToolBarUtils.initItemCommonData( cocoaItem, self );
  if Assigned(self.onClick) then
    cocoaItem.lclSetHandler( TCocoaToolBarItemHandler(self.onClick) )
  else
    cocoaItem.setAction( nil );

  cocoaMenu:= NSMenu.new;
  NSMenuAddItemsFromLCLMenu( cocoaMenu, self.menu );
  cocoaItem.setMenu( cocoaMenu );
  cocoaMenu.autorelease;

  cocoaItem.autorelease;
  Result:= cocoaItem;
end;

function TCocoaConfigToolBarItemAdapterMenu.showsIndicator: Boolean;
begin
  Result:= _showsIndicator;
end;

function TCocoaConfigToolBarItemAdapterMenu.menu: TMenuItem;
begin
  Result:= _menu;
end;

{ TCocoaConfigToolBarItemAdapterGroup }

constructor TCocoaConfigToolBarItemAdapterGroup.Create(
  itemConfig: TCocoaConfigToolBarItemGroup);
begin
  self.initItemConfig( @itemConfig );
  _representation:= itemConfig.representation;
  _selectionMode:= itemConfig.selectionMode;
  _selectedIndex:= itemConfig.selectedIndex;
  _subitems:= itemConfig.subitems;
end;

function TCocoaConfigToolBarItemAdapterGroup.createItem: NSToolBarItem;
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

function TCocoaConfigToolBarItemAdapterGroup.representation: NSToolbarItemGroupControlRepresentation;
begin
  Result:= _representation;
end;

function TCocoaConfigToolBarItemAdapterGroup.selectionMode: NSToolbarItemGroupSelectionMode;
begin
  Result:= _selectionMode;
end;

function TCocoaConfigToolBarItemAdapterGroup.selectedIndex: NSInteger;
begin
  Result:= _selectedIndex;
end;

function TCocoaConfigToolBarItemAdapterGroup.subitems: TCocoaConfigToolBarItems;
begin
  Result:= _subitems;
end;

end.

