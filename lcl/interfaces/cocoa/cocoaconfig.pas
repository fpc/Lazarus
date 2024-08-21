unit CocoaConfig;

{$mode objfpc}{$H+}
{$modeswitch objectivec1}
{$include cocoadefines.inc}

interface

uses
  CocoaAll, Cocoa_Extra, CocoaConst;

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
  TCocoaConfigNotification = record
    alwaysPresent: Boolean;
  end;

type
  // on macOS, the FocusRing takes up extra space, which may cause strange
  // display in some cases. it may block other controls, or be partially cut off.
  // for example, in the Lazarus IDE - About dialog, the FocusRing of the
  // Tab of TPageControl is partially cut off.
  // by providing a configurable infrastructure, FocusRing can be controlled
  // for different control types.
  {$scopedEnums on}
  TCocoaFocusRingStrategy = (
    default,   // by macOS Default
    none,      // no FoucsRing
    required,  // have FocusRing
    border     // by LCL Control Border
  );

  // set the FocusRing strategy of the control according to ClassName
  // (eg. 'TCocoaTabControl')
  // APP can change the default setting of Cocoa WidgetSet
  // by calling setCocoaControlFocusRingStrategy() too.
  procedure setCocoaControlFocusRingStrategry( frs: TCocoaFocusRingStrategy; AClassName: NSString );

  // getCocoaControlFocusRingStrategy() is mainly used internally by Cocoa WidgetSet
  function getCocoaControlFocusRingStrategry( AClassName: NSString ): TCocoaFocusRingStrategy;

var
  // for compatiblity with LCL 1.8 release. The macOS base is 72ppi
  CocoaBasePPI : Integer = 96;

  // if set to true, then WS would not assign icons via TCocoaWSForm SetIcon
  // The icon would have to be changed manually. By default LCL behaviour is used
  CocoaIconUse: Boolean = false;

  CocoaHideFocusNoBorder : Boolean = true;

  // some localized named might be too long to be returned properly by APIs
  CocoaUseLocalizedFontName : Boolean = false;

  // default Image Name for MenuItem
  CocoaDefaultCheckMenuImageName : NSString;
  CocoaDefaultRadioMenuImageName : NSString;

  {$ifdef COCOALOOPHIJACK}
  // The flag is set to true once hi-jacked loop is finished (at the end of app)
  // The flag is checked in Menus to avoid "double" Cmd+Q menu
  LoopHiJackEnded : Boolean = false;
  {$endif}

{$include cocoaconfig.inc}

implementation

var
  FocusRingStrategySetting: NSMutableDictionary;

procedure setCocoaControlFocusRingStrategry( frs: TCocoaFocusRingStrategy; AClassName: NSString );
var
  valueObject: NSNumber;
begin
  valueObject:= NSNumber.numberWithInt( Ord(frs) );
  FocusRingStrategySetting.setValue_forKey( valueObject , AClassName );
end;

function getCocoaControlFocusRingStrategry( AClassName: NSString ): TCocoaFocusRingStrategy;
var
  valueObject: NSNumber;
begin
  Result:= TCocoaFocusRingStrategy.default;
  valueObject:= NSNumber( FocusRingStrategySetting.valueForKey(AClassName) );
  if Assigned(valueObject) then
    Result:= TCocoaFocusRingStrategy(valueObject.intValue);
  if Result = TCocoaFocusRingStrategy.required then
    Writeln( 'required' );
end;

// no need to set TCocoaFocusRingStrategy.default control
// the controls not in FocusRingStrategySetting are TCocoaFocusRingStrategy.default
procedure initDefaultFoucsRingSetting;
begin
  FocusRingStrategySetting:= NSMutableDictionary.alloc.initWithCapacity( 16 );

  setCocoaControlFocusRingStrategry( TCocoaFocusRingStrategy.none, NSSTR('TCocoaTabControl') );
  setCocoaControlFocusRingStrategry( TCocoaFocusRingStrategy.none, NSSTR('TCocoaButton') );
  setCocoaControlFocusRingStrategry( TCocoaFocusRingStrategy.none, NSSTR('TCocoaTextField') );
  setCocoaControlFocusRingStrategry( TCocoaFocusRingStrategy.none, NSSTR('TCocoaComboBox') );
  setCocoaControlFocusRingStrategry( TCocoaFocusRingStrategy.none, NSSTR('TCocoaReadOnlyComboBox') );
  setCocoaControlFocusRingStrategry( TCocoaFocusRingStrategy.none, NSSTR('TCocoaTableListView') );
  setCocoaControlFocusRingStrategry( TCocoaFocusRingStrategy.none, NSSTR('TCocoaCollectionView') );

  setCocoaControlFocusRingStrategry( TCocoaFocusRingStrategy.border, NSSTR('TCocoaTextView') );
end;

function getCocoaScrollerDefaultKnobColor: NSColor;
begin
  Result:= NSColor.controlTextColor;
end;

initialization
  CocoaDefaultCheckMenuImageName:= NSSTR('NSMenuCheckmark');
  CocoaDefaultRadioMenuImageName:= NSSTR('NSDatePickerCalendarHome');
  initDefaultFoucsRingSetting;
end.

