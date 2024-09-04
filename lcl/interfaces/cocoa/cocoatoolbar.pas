unit CocoaToolBar;

{$mode objfpc}{$H+}
{$modeswitch objectivec2}
{$interfaces corba}

interface

uses
  Classes, SysUtils,
  Menus,
  CocoaAll, CocoaPrivate, CocoaConfig, CocoaMenus, Cocoa_Extra, CocoaUtils;

type
  TCocoaToolBarItemHandler = procedure ( Sender: id );
  TCocoaToolBarItemSharingOnGetItems = function ( item: NSToolBarItem ): TStringArray;

  PCocoaConfigToolBar = ^TCocoaConfigToolBar;

  PCocoaConfigToolBarItemBase = ^TCocoaConfigToolBarItemBase;
  PCocoaConfigToolBarItemWithUI = ^TCocoaConfigToolBarItemWithUI;
  PCocoaConfigToolBarItemWithAction = ^TCocoaConfigToolBarItemWithAction;
  PCocoaConfigToolBarItem = ^TCocoaConfigToolBarItem;

  { TCocoaConfigToolBarItemClassBase }

  TCocoaConfigToolBarItemClassBase = class( TCocoaConfigToolBarItemClassAbstract )
  protected
    _identifier: String;
    _priority: NSInteger;
    _navigational: Boolean;
  protected
    procedure toClassConfig( pItemConfig: PCocoaConfigToolBarItemBase );
  public
    procedure setItemAttribs( item: NSToolBarItem ); virtual;
    function identifier: NSString; override;
  end;

  { TCocoaConfigToolBarItemClassWithUI }

  TCocoaConfigToolBarItemClassWithUI = class( TCocoaConfigToolBarItemClassBase )
  protected
    _iconName: String;
    _title: String;
    _tips: String;
    _bordered: Boolean;
  protected
    procedure toClassConfig( pItemConfig: PCocoaConfigToolBarItemWithUI );
  public
    procedure setItemAttribs( item: NSToolBarItem ); override;
    function iconName: NSString; virtual;
    function title: NSString; virtual;
    function tips: NSString; virtual;
  end;

  { TCocoaConfigToolBarItemClassWithAction }

  TCocoaConfigToolBarItemClassWithAction = class( TCocoaConfigToolBarItemClassWithUI )
  protected
    _onAction: TCocoaToolBarItemHandler;
  protected
    procedure toClassConfig( pItemConfig: PCocoaConfigToolBarItemWithAction );
  public
    procedure setItemAttribs( item: NSToolBarItem ); override;
  end;

  { TCocoaConfigToolBarItemClass }

  TCocoaConfigToolBarItemClass = class( TCocoaConfigToolBarItemClassWithAction )
    constructor Create( const itemConfig: TCocoaConfigToolBarItem );
    function createItem: NSToolBarItem; override;
  end;

  { TCocoaConfigToolBarItemClassSharing }

  TCocoaConfigToolBarItemClassSharing = class( TCocoaConfigToolBarItemClassWithUI )
  protected
    _onGetItems: TCocoaToolBarItemSharingOnGetItems;
  public
    constructor Create( const itemConfig: TCocoaConfigToolBarItemSharing );
    function createItem: NSToolBarItem; override;
  end;

  { TCocoaConfigToolBarItemClassSearch }

  TCocoaConfigToolBarItemClassSearch = class( TCocoaConfigToolBarItemClassWithAction )
  protected
    _sendWhole: Boolean;
    _sendImmediately: Boolean;
    _resignsWithCancel: Boolean;
    _preferredWidth: Double;
  public
    constructor Create( const itemConfig: TCocoaConfigToolBarItemSearch );
    function createItem: NSToolBarItem; override;
  end;

  { TCocoaConfigToolBarItemClassMenu }

  TCocoaConfigToolBarItemClassMenu = class( TCocoaConfigToolBarItemClassWithAction )
  protected
    _showsIndicator: Boolean;
    _menu: TMenuItem;
  public
    constructor Create( const itemConfig: TCocoaConfigToolBarItemMenu );
    function createItem: NSToolBarItem; override;
  end;

  { TCocoaConfigToolBarItemClassGroup }

  TCocoaConfigToolBarItemClassGroup = class( TCocoaConfigToolBarItemClassWithAction )
  protected
    _representation: NSToolbarItemGroupControlRepresentation;
    _selectionMode: NSToolbarItemGroupSelectionMode;
    _selectedIndex: NSInteger;
    _subitems: TCocoaConfigToolBarItems;
  public
    constructor Create( const itemConfig: TCocoaConfigToolBarItemGroup );
    function createItem: NSToolBarItem; override;
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

  TCocoaToolBarItemCreator = function ( identifier: String;
    items: TCocoaConfigToolBarItems ): NSToolbarItem;
  
  { TCocoaToolBar }

  TCocoaToolBar = objcclass( NSToolBar, NSToolbarDelegateProtocol )
  private
    _itemCreator: TCocoaToolBarItemCreator;
    _defaultItemIdentifiers: NSArray;
    _allowedItemIdentifiers: NSArray;
    _pToolBarItems: Pointer;
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
    procedure lclSetConfig( const pConfig: Pointer );
      message 'lclSetConfig:';
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
    class function toClass( itemConfig: TCocoaConfigToolBarItem ):
      TCocoaConfigToolBarItemClassAbstract;
    class function toClass( itemConfig: TCocoaConfigToolBarItemSharing ):
      TCocoaConfigToolBarItemClassAbstract;
    class function toClass( itemConfig: TCocoaConfigToolBarItemSearch ):
      TCocoaConfigToolBarItemClassAbstract;
    class function toClass( itemConfig: TCocoaConfigToolBarItemMenu ):
      TCocoaConfigToolBarItemClassAbstract;
    class function toClass( itemConfig: TCocoaConfigToolBarItemGroup ):
      TCocoaConfigToolBarItemClassAbstract;
  public
    class function createItem( identifier: String; itemsConfig: TCocoaConfigToolBarItems ): NSToolbarItem;
    class function createToolBar( const toolBarConfig: TCocoaConfigToolBar ): TCocoaToolBar;
  end;

  function defaultToolBarItemCreatorImplement( identifier: String;
    items: TCocoaConfigToolBarItems ): NSToolbarItem;

implementation

class function TCocoaToolBarUtils.toClass(itemConfig: TCocoaConfigToolBarItem
  ): TCocoaConfigToolBarItemClassAbstract;
begin
  Result:= TCocoaConfigToolBarItemClass.Create( itemConfig );
end;

class function TCocoaToolBarUtils.toClass( itemConfig: TCocoaConfigToolBarItemSharing
  ): TCocoaConfigToolBarItemClassAbstract;
begin
  Result:= TCocoaConfigToolBarItemClassSharing.Create( itemConfig );
end;

class function TCocoaToolBarUtils.toClass( itemConfig: TCocoaConfigToolBarItemSearch
  ): TCocoaConfigToolBarItemClassAbstract;
begin
  Result:= TCocoaConfigToolBarItemClassSearch.Create( itemConfig );
end;

class function TCocoaToolBarUtils.toClass( itemConfig: TCocoaConfigToolBarItemMenu
  ): TCocoaConfigToolBarItemClassAbstract;
begin
  Result:= TCocoaConfigToolBarItemClassMenu.Create( itemConfig );
end;

class function TCocoaToolBarUtils.toClass( itemConfig: TCocoaConfigToolBarItemGroup
  ): TCocoaConfigToolBarItemClassAbstract;
begin
  Result:= TCocoaConfigToolBarItemClassGroup.Create( itemConfig );
end;

class function TCocoaToolBarUtils.createItem( identifier: String;
  itemsConfig: TCocoaConfigToolBarItems ): NSToolbarItem;
var
  i: Integer;
  count: Integer;
  identifierNSString: NSString;
begin
  Result:= nil;
  count:= length( itemsConfig );
  identifierNSString:= StrToNSString( identifier );
  for i:=0 to count-1 do begin
    if identifierNSString.isEqualToString(itemsConfig[i].identifier) then begin
      Result:= itemsConfig[i].createItem;
      Exit;
    end;
  end;
end;

class function TCocoaToolBarUtils.createToolBar(
  const toolBarConfig: TCocoaConfigToolBar ): TCocoaToolBar;
var
  toolBar: TCocoaToolBar;
  defaultArray: NSArray;
  allowedArray: NSArray;
begin
  toolBar:= TCocoaToolBar.alloc.initWithIdentifier(
    StrToNSString(toolBarConfig.identifier) );
  toolBar.lclSetConfig( @toolBarConfig );

  defaultArray:= StringArrayFromLCLToNS( toolBarConfig.defaultItemsIdentifiers );
  allowedArray:= StringArrayFromLCLToNS( toolBarConfig.allowedItemsIdentifiers );

  toolBar.setDisplayMode( toolBarConfig.displayMode );
  toolBar.setAllowsUserCustomization( toolBarConfig.allowsUserCustomization );
  toolBar.setAutosavesConfiguration( toolBarConfig.autosavesConfiguration );
  toolBar.lclSetDefaultItemIdentifiers( defaultArray );
  toolBar.lclSetAllowedItemIdentifiers( allowedArray );

  if Assigned(toolBarConfig.itemCreator) then
    toolBar.lclSetItemCreator( TCocoaToolBarItemCreator(toolBarConfig.itemCreator) );

  Result:= toolBar;
end;

function defaultToolBarItemCreatorImplement( identifier: String;
  items: TCocoaConfigToolBarItems ): NSToolbarItem;
begin
  Result:= TCocoaToolBarUtils.createItem( identifier, items );
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
  _itemCreator:= @defaultToolBarItemCreatorImplement;
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
  Result:= _itemCreator( itemIdentifier.UTF8String, _pToolBarItems );
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

procedure TCocoaToolBar.lclSetConfig( const pConfig: Pointer );
begin
  _pToolBarItems:= Pointer( PCocoaConfigToolBar(pConfig)^.Items );
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

{ TCocoaConfigToolBarItemClassBase }

procedure TCocoaConfigToolBarItemClassBase.toClassConfig(
  pItemConfig: PCocoaConfigToolBarItemBase);
begin
  _identifier:= pItemConfig^.identifier;
  _priority:= pItemConfig^.priority;
  _navigational:= pItemConfig^.navigational;
end;

procedure TCocoaConfigToolBarItemClassBase.setItemAttribs(item: NSToolBarItem);
begin
  item.setVisibilityPriority( _priority );
  item.setNavigational( _navigational );
end;

function TCocoaConfigToolBarItemClassBase.identifier: NSString;
begin
  Result:= StrToNSString( _identifier );
end;

{ TCocoaConfigToolBarItemClassWithUI }

procedure TCocoaConfigToolBarItemClassWithUI.toClassConfig(
  pItemConfig: PCocoaConfigToolBarItemWithUI );
begin
  Inherited toClassConfig( pItemConfig );
  _iconName:= pItemConfig^.iconName;
  _title:= pItemConfig^.title;
  _tips:= pItemConfig^.tips;
  _bordered:= pItemConfig^.bordered;
end;

procedure TCocoaConfigToolBarItemClassWithUI.setItemAttribs(item: NSToolBarItem
  );
var
  cocoaImageName: NSString;
  cocoaLabel: NSString;
begin
  Inherited;

  cocoaImageName:= self.iconName;
  cocoaLabel:= self.title;

  if cocoaImageName.length > 0 then begin
    item.setImage( NSImage.imageWithSystemSymbolName_accessibilityDescription(
      cocoaImageName, nil ) );
  end;
  item.setLabel( cocoaLabel );
  item.setPaletteLabel( cocoaLabel );
  item.setToolTip( self.tips );
  item.setBordered( _bordered );
end;

function TCocoaConfigToolBarItemClassWithUI.iconName: NSString;
begin
  Result:= StrToNSString( _iconName );
end;

function TCocoaConfigToolBarItemClassWithUI.title: NSString;
begin
  Result:= StrToNSString( _title );
end;

function TCocoaConfigToolBarItemClassWithUI.tips: NSString;
begin
  Result:= StrToNSString( _tips );
end;

{ TCocoaConfigToolBarItemClassWithAction }

procedure TCocoaConfigToolBarItemClassWithAction.toClassConfig(
  pItemConfig: PCocoaConfigToolBarItemWithAction );
begin
  Inherited toClassConfig( pItemConfig );
  _onAction:= TCocoaToolBarItemHandler( pItemConfig^.onAction );
end;

procedure TCocoaConfigToolBarItemClassWithAction.setItemAttribs(
  item: NSToolBarItem);
begin
  inherited setItemAttribs(item);
  item.setTarget( item );
  item.setAction( ObjCSelector('lclItemAction:') );
  if item.respondsToSelector( ObjCSelector('lclSetHandler:') ) then
    item.performSelector_withObject( ObjCSelector('lclSetHandler:'), id(_onAction) );
end;

{ TCocoaConfigToolBarItemClass }

constructor TCocoaConfigToolBarItemClass.Create(
  const itemConfig: TCocoaConfigToolBarItem );
begin
  Inherited toClassConfig( @ItemConfig );
end;

function TCocoaConfigToolBarItemClass.createItem: NSToolBarItem;
var
  cocoaItem: TCocoaToolBarItem;
begin
  cocoaItem:= TCocoaToolBarItem.alloc.initWithItemIdentifier( self.identifier );
  self.setItemAttribs( cocoaItem );
  cocoaItem.autorelease;
  Result:= cocoaItem;
end;

{ TCocoaConfigToolBarItemClassSharing }

constructor TCocoaConfigToolBarItemClassSharing.Create(
  const itemConfig: TCocoaConfigToolBarItemSharing);
begin
  self.toClassConfig( @itemConfig );
  _onGetItems:= TCocoaToolBarItemSharingOnGetItems( itemConfig.onGetItems );
end;

function TCocoaConfigToolBarItemClassSharing.createItem: NSToolBarItem;
var
  cocoaItem: TCocoaToolBarItemSharing;
  cocoaItemDelegate: TCocoaToolBarItemSharingDelegate;
begin
  cocoaItem:= TCocoaToolBarItemSharing.alloc.initWithItemIdentifier( self.identifier );
  self.setItemAttribs( cocoaItem );
  cocoaItem.setAutovalidates( False );

  // release in TCocoaToolBarItemSharing
  cocoaItemDelegate:= TCocoaToolBarItemSharingDelegate.new;
  cocoaItemDelegate.lclSetOnGetItems( _onGetItems );
  cocoaItem.setDelegate( cocoaItemDelegate );

  cocoaItem.autorelease;
  Result:= cocoaItem;
end;

{ TCocoaConfigToolBarItemClassSearch }

constructor TCocoaConfigToolBarItemClassSearch.Create(
  const itemConfig: TCocoaConfigToolBarItemSearch);
begin
  self.toClassConfig( @itemConfig );
  _sendWhole:= itemConfig.sendWhole;
  _sendImmediately:= itemConfig.sendImmediately;
  _resignsWithCancel:= itemConfig.resignsWithCancel;
  _preferredWidth:= itemConfig.preferredWidth;
end;

function TCocoaConfigToolBarItemClassSearch.createItem: NSToolBarItem;
var
  cocoaItem: TCocoaToolBarItemSearch;
begin
  cocoaItem:= TCocoaToolBarItemSearch.alloc.initWithItemIdentifier( self.identifier );
  cocoaItem.searchField.setSendsWholeSearchString( _sendWhole );
  cocoaItem.searchField.setSendsSearchStringImmediately( _sendImmediately );
  cocoaItem.searchField.setDelegate( {%H-}NSTextFieldDelegateProtocol(cocoaItem) );
  cocoaItem.setResignsFirstResponderWithCancel( _resignsWithCancel );
  if _preferredWidth > 0 then
    cocoaItem.setPreferredWidthForSearchField( _preferredWidth );
  self.setItemAttribs( cocoaItem );
  cocoaItem.autorelease;
  Result:= cocoaItem;
end;

{ TCocoaConfigToolBarItemClassMenu }

constructor TCocoaConfigToolBarItemClassMenu.Create(
  const itemConfig: TCocoaConfigToolBarItemMenu);
begin
  self.toClassConfig( @itemConfig );
  _showsIndicator:= itemConfig.showsIndicator;
  _menu:= itemConfig.menu;
end;

function TCocoaConfigToolBarItemClassMenu.createItem: NSToolBarItem;
var
  cocoaItem: TCocoaToolBarItemMenu;
  cocoaMenu: NSMenu;
begin
  cocoaItem:= TCocoaToolBarItemMenu.alloc.initWithItemIdentifier( self.identifier );
  cocoaItem.setShowsIndicator( _showsIndicator );
  self.setItemAttribs( cocoaItem );
  if NOT Assigned(_onAction) then
    cocoaItem.setAction( nil );

  cocoaMenu:= NSMenu.new;
  NSMenuAddItemsFromLCLMenu( cocoaMenu, _menu );
  cocoaItem.setMenu( cocoaMenu );
  cocoaMenu.autorelease;

  cocoaItem.autorelease;
  Result:= cocoaItem;
end;

{ TCocoaConfigToolBarItemClassGroup }

constructor TCocoaConfigToolBarItemClassGroup.Create(
  const itemConfig: TCocoaConfigToolBarItemGroup);
begin
  self.toClassConfig( @itemConfig );
  _representation:= itemConfig.representation;
  _selectionMode:= itemConfig.selectionMode;
  _selectedIndex:= itemConfig.selectedIndex;
  _subitems:= itemConfig.subitems;
end;

function TCocoaConfigToolBarItemClassGroup.createItem: NSToolBarItem;
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
    count:= length( _subitems );
    images:= NSMutableArray.arrayWithCapacity( count );
    labels:= NSMutableArray.arrayWithCapacity( count );
    for i:=0 to count-1 do begin
      images.addObject( NSImage.imageWithSystemSymbolName_accessibilityDescription(
        TCocoaConfigToolBarItemClass(_subitems[i]).iconName, nil) );
      labels.addObject( TCocoaConfigToolBarItemClass(_subitems[i]).title );
    end;
  end;

begin
  initSubitems;
  toolbarItem:= NSToolbarItemGroup.groupWithItemIdentifier_images_selectionMode_labels_target_action(
    self.identifier,
    images,
    _selectionMode,
    labels,
    nil,
    nil );

  groupWrapper:= TCocoaToolBarItemGroupWrapper.new;
  groupWrapper.lclSetItemGroup( toolbarItem );
  groupWrapper.lclSetSelectedIndex( _selectedIndex );
  groupWrapper.lclSetHandler( _onAction );
  self.setItemAttribs( toolbarItem );
  toolbarItem.setTarget( groupWrapper );
  Result:= toolbarItem;
end;

end.

