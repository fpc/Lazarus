unit CocoaConfig;

{$mode objfpc}{$H+}
{$modeswitch objectivec2}
{$interfaces corba}
{$include cocoadefines.inc}

interface

uses
  SysUtils,
  Menus,
  CocoaAll, Cocoa_Extra, CocoaConst;

type
  TCocoaConfigToolBarItemClassAbstract = class
  public
    function identifier: NSString; virtual; abstract;
    function createItem: NSToolBarItem; virtual; abstract;
  end;

  TCocoaConfigToolBarItems = Array of TCocoaConfigToolBarItemClassAbstract;

type
  TCocoaToolBarItemCreator = function ( const identifier: String;
    const items: TCocoaConfigToolBarItems ): NSToolbarItem;
  TCocoaToolBarItemActionHandler = procedure ( const Sender: id );
  TCocoaToolBarItemMenuOnGetMenu = procedure ( const menu: TMenu );
  TCocoaToolBarItemSharingOnGetItems = function ( item: NSToolBarItem ): TStringArray;

type
  { TCocoaConfigToolBarItemBase }

  TCocoaConfigToolBarItemBase = object
    identifier: String;
    priority: NSInteger;
    navigational: Boolean;
  end;

  { TCocoaConfigToolBarItemWithUI }

  TCocoaConfigToolBarItemWithUI = object( TCocoaConfigToolBarItemBase )
    iconName: String;
    title: String;
    tips: String;
    bordered: Boolean;
  end;

  { TCocoaConfigToolBarItemWithAction }

  TCocoaConfigToolBarItemWithAction = object( TCocoaConfigToolBarItemWithUI )
    onAction: TCocoaToolBarItemActionHandler;
  end;

  TCocoaConfigToolBarItem = TCocoaConfigToolBarItemWithAction;

  TCocoaConfigToolBarItemSharing = object( TCocoaConfigToolBarItemWithUI )
    onGetItems: TCocoaToolBarItemSharingOnGetItems;
  end;

  TCocoaConfigToolBarItemSearch = object( TCocoaConfigToolBarItemWithAction )
    sendWhole: Boolean;
    sendImmediately: Boolean;
    resignsWithCancel: Boolean;
    preferredWidth: Double;
  end;

  TCocoaConfigToolBarItemMenu = object( TCocoaConfigToolBarItemWithAction )
    showsIndicator: Boolean;
    dynamic: Boolean;   // dynamically load each time before displaying the menu
    menu: TMenu;
    onGetMenu: TCocoaToolBarItemMenuOnGetMenu;
  end;

  TCocoaConfigToolBarItemGroup = object( TCocoaConfigToolBarItemWithAction )
    representation: NSToolbarItemGroupControlRepresentation;
    selectionMode: NSToolbarItemGroupSelectionMode;
    selectedIndex: NSInteger;
    subitems: TCocoaConfigToolBarItems;
  end;

  TCocoaConfigTitleBar = record
    transparent: Boolean;
    separatorStyle: NSTitlebarSeparatorStyle;
  end;

  TCocoaConfigToolBar = record
    identifier: String;
    style: NSWindowToolbarStyle;
    displayMode: NSToolbarDisplayMode;
    allowsUserCustomization: Boolean;
    autosavesConfiguration: Boolean;
    items: TCocoaConfigToolBarItems;
    defaultItemsIdentifiers: TStringArray;
    allowedItemsIdentifiers: TStringArray;
    itemCreator: Pointer;
  end;

  TCocoaConfigForm = record
    name: String;
    className: String;
    isMainForm: Boolean;
    titleBar: TCocoaConfigTitleBar;
    toolBar: TCocoaConfigToolBar;
  end;

  TCocoaConfigForms = Array of TCocoaConfigForm;

var
  CocoaConfigForms: TCocoaConfigForms;

type

  TCocoaConfigMenuItem = record
    defaultCheckImageName: NSString;
    defaultRadioImageName: NSString;
  end;

  TCocoaConfigAppMenuOnCreate = procedure ( appMenu: NSMenu );

  // Application interface provided to facilitate APP to operate App Menu.
  // it's easy to set About, Preferences, and customized menus,
  // only the LCL TMenuItem is needed to pass in.
  // and we can control whether Cocoa is needed to automatically add
  // Hide, Hide Others, Show All, and Quit menu items.
  TCocoaConfigAppMenu = record
    aboutItem: TMenuItem;
    preferencesItem: TMenuItem;
    customMenus: TMenuItem;
    onCreate: TCocoaConfigAppMenuOnCreate;
    dontAutoCreateItems: Boolean;
  end;

  // Application interface provided to facilitate APP to operate Dock Menu.
  // only the LCL TMenuItem is needed to pass in.
  TCocoaConfigDockMenu = record
    customMenus: TMenuItem;
  end;

  TCocoaConfigMenu = record
    menuItem: TCocoaConfigMenuItem;
    appMenu: TCocoaConfigAppMenu;
    dockMenu: TCocoaConfigDockMenu;
  end;

type
  // return True for event Handled, bypass post system processing
  TCocoaApplicationEventHandler = function( event: NSEvent ): Boolean of object;

  TCocoaConfigApplicationEvent = record
    highestHandler: TCocoaApplicationEventHandler;
  end;

  TCocoaConfigApplication = record
    event: TCocoaConfigApplicationEvent;
  end;

type
  TCocoaConfigGlobal = record
    basePPI: Integer;
    useIcon: Boolean;
    useLocalizedFontName: Boolean;
  end;

type
  TCocoaConfigReadOnlyComboBoxItem = record
    defaultHeight: Integer;
  end;

  TCocoaConfigReadOnlyComboBox = record
    item: TCocoaConfigReadOnlyComboBoxItem;
  end;

  TCocoaConfigComboBox = record
    readOnly: TCocoaConfigReadOnlyComboBox;
  end;

type
  TCocoaConfigSize = record
    width: Double;
    height: Double;
  end;

  TCocoaConfigTableRow = record
    defaultHeight: Integer;
    imageLineSpacing: Integer;
  end;

  TCocoaConfigTableColumn = record
    controlSpacing: Integer;
    textFieldMinWidth: Integer;
  end;

  TCocoaConfigTableColumnAutoFit = record
    // for performance, when the column divider is double-clicked to automatically
    // calculate the column width, the maximum number of rows calculated
    maxCalcRows: Integer;
    // min Column Width when the column divider is double-clicked
    minWidth: Double;
    // additional width for header
    headerAdditionalWidth: Double;
  end;

  TCocoaConfigTable = record
    // default NSTableViewStyle
    tableViewStyle: NSTableViewStyle;
    row: TCocoaConfigTableRow;
    column: TCocoaConfigTableColumn;
    columnAutoFit: TCocoaConfigTableColumnAutoFit;
  end;

  TCocoaConfigCollectionItem = record
    minSize: TCocoaConfigSize;
    controlSpacing: Double;
    textFieldAlignment: NSTextAlignment;
    checkBoxOccupiedWidth: Double;
  end;

  TCocoaConfigCollection = object
    interitemSpacing: Double;
    lineSpacing: Double;
    item: TCocoaConfigCollectionItem;
  end;

  TCocoaConfigCollectionIconImageView = record
    minSize: TCocoaConfigSize;
    padding: Double;
  end;

  TCocoaConfigCollectionLargeIconTextField = record
    defaultHeight: Double;
  end;

  TCocoaConfigCollectionSmallIconTextField = record
    iconWidthFactor: Double;
    baseWidth: Double;
    minWidth: Double;
  end;

  TCocoaConfigCollectionIcon = object( TCocoaConfigCollection )
    imageView: TCocoaConfigCollectionIconImageView;
  end;

  TCocoaConfigCollectionLargeIcon = object( TCocoaConfigCollectionIcon )
    textField: TCocoaConfigCollectionLargeIconTextField;
  end;

  TCocoaConfigCollectionSmallIcon = object( TCocoaConfigCollectionIcon )
    textField: TCocoaConfigCollectionSmallIconTextField;
  end;

  TCocoaConfigListView = record
    vsReport: TCocoaConfigTable;
    vsIcon: TCocoaConfigCollectionLargeIcon;
    vsSmallIcon: TCocoaConfigCollectionSmallIcon;
    vsList: TCocoaConfigCollection;
  end;

type
  NSColorFunction = Function(): NSColor;
  function getCocoaScrollerDefaultKnobColor: NSColor;

type
  TCocoaConfgiScrollerKnob = object
    radius: Double;
    color: NSColorFunction;
    pos: Double;
    shrunkSize: Double;
  end;

  TCocoaConfgiScrollerLegacyKnob = object( TCocoaConfgiScrollerKnob )
    alpha: Double;
    alphaBlack: Double;
    fadeStep: Double;
  end;

  TCocoaConfgiScrollerOverlayKnob = object( TCocoaConfgiScrollerKnob )
    minSize: Double;
  end;

  TCocoaConfigScrollerLegacy = record
    knob: TCocoaConfgiScrollerLegacyKnob;
  end;

  TCocoaConfigScrollerOverlayBar = record
    autoShowDelayTime: Double;
    autoHideDelayTime: Double;
    alpha: Double;
    alphaFadeStep: Double;
    alphaFadeTo: Double;
    expandTimeInterval: Double;
    expandSize: Double;
  end;

  TCocoaConfigScrollerOverlay = record
    bar: TCocoaConfigScrollerOverlayBar;
    knob: TCocoaConfgiScrollerOverlayKnob;
  end;

  TCocoaConfigScroller = record
    preferredStyle: NSScrollerStyle;
    fadeTimeInterval: Double;
    legacy: TCocoaConfigScrollerLegacy;
    overlay: TCocoaConfigScrollerOverlay;
  end;

type
  TCocoaConfigToggleBox = record
    bezelStyle: NSBezelStyle;
    buttonType: NSButtonType;
  end;

type
  TCocoaConfigPanel = record
    classicFrame3d: Boolean;
  end;

type
  TCocoaConfigMouse = record
    controlLeftToRightClick: Boolean;
  end;

type
  TCocoaConfigNotification = record
    alwaysPresent: Boolean;
  end;

type
  
  { TCocoaConfigFocusRing }

  // on macOS, the FocusRing takes up extra space, which may cause strange
  // display in some cases. it may block other controls, or be partially cut off.
  // for example, in the Lazarus IDE - About dialog, the FocusRing of the
  // Tab of TPageControl is partially cut off.
  // by providing a configurable infrastructure, FocusRing can be controlled
  // for different control types.
  TCocoaConfigFocusRing = class
    {$scopedEnums on}
    type Strategy = (
      default,   // by macOS Default
      none,      // no FoucsRing
      required,  // have FocusRing
      border     // by LCL Control Border
    );
  private
    _strategies: NSMutableDictionary;
  public
    constructor Create;
    destructor Destroy; override;

    // set the FocusRing strategy of the control according to ClassName
    // (eg. 'TCocoaTabControl')
    // APP can change the default setting of Cocoa WidgetSet
    // by calling setStrategy() too.
    procedure setStrategy( frs: Strategy; AClassName: NSString );

    // getStrategy() is mainly used internally by Cocoa WidgetSet
    function getStrategy( AClassName: NSString ): Strategy;
  end;

// config data is stored in CocoaConfig.inc
{$include cocoaconfig.inc}

var
  CocoaConfigFocusRing: TCocoaConfigFocusRing;

implementation

constructor TCocoaConfigFocusRing.Create;
begin
  _strategies:= NSMutableDictionary.alloc.initWithCapacity( 16 );
  // FocusRing config data is stored in CocoaConfigFocusRing.inc
  {$include cocoaconfigfocusring.inc}
end;

destructor TCocoaConfigFocusRing.Destroy;
begin
  inherited Destroy;
  _strategies.release;
end;

procedure TCocoaConfigFocusRing.setStrategy(
  frs: Strategy; AClassName: NSString);
var
  valueObject: NSNumber;
begin
  valueObject:= NSNumber.numberWithInt( Ord(frs) );
  _strategies.setValue_forKey( valueObject , AClassName );
end;

function TCocoaConfigFocusRing.getStrategy(
  AClassName: NSString): Strategy;
var
  valueObject: NSNumber;
begin
  Result:= TCocoaConfigFocusRing.Strategy.default;
  valueObject:= NSNumber( _strategies.valueForKey(AClassName) );
  if Assigned(valueObject) then
    Result:= TCocoaConfigFocusRing.Strategy(valueObject.intValue);
end;

function getCocoaScrollerDefaultKnobColor: NSColor;
begin
  Result:= NSColor.controlTextColor;
end;

initialization
  CocoaConfigMenu.menuItem.defaultCheckImageName:= NSSTR('NSMenuCheckmark');
  CocoaConfigMenu.menuItem.defaultRadioImageName:= NSSTR('NSDatePickerCalendarHome');

  CocoaConfigFocusRing:= TCocoaConfigFocusRing.Create;

finalization
  FreeAndNil( CocoaConfigFocusRing );

end.

