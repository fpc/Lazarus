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

var
  CocoaConfigListView: TCocoaConfigListView = (
    vsReport: (
      tableViewStyle: NSTableViewStyleAutomatic;
      row: ( defaultHeight: 16; imageLineSpacing: 4*2 );
      column: ( controlSpacing: 4; textFieldMinWidth: 16 );
      columnAutoFit: ( maxCalcRows: 100; minWidth: 20; headerAdditionalWidth: 4 );
    );
    vsIcon: (
      interitemSpacing: 4;
      lineSpacing: 4;
      item: (
        minSize: ( width:64; height:68 );
        controlSpacing: 2;
        textFieldAlignment: {$ifdef USE_IOS_VALUES}1{$else}2{$endif}; // NSTextAlignmentCenter
        checkBoxOccupiedWidth: 24
      );
      imageView: ( minSize: ( width: 32; height: 32 ); padding: 10; );
      textField: ( defaultHeight: 15 );
    );
    vsSmallIcon: (
      interitemSpacing: 10;
      lineSpacing: 0;
      item: (
        minSize: ( width:150; height:28 );
        controlSpacing: 2;
        textFieldAlignment: 0;       // NSTextAlignmentLeft
        checkBoxOccupiedWidth: 24
      );
      imageView: ( minSize: ( width: 16; height: 16 ); padding: 4; );
      textField: ( iconWidthFactor: 3; baseWidth: 0; minWidth: 128 );
    );
    vsList: (
      interitemSpacing: 0;
      lineSpacing: 10;
      item: (
        minSize: ( width:146; height:24 );
        controlSpacing: 4;
        textFieldAlignment: 0;       // NSTextAlignmentLeft
        checkBoxOccupiedWidth: 24
      );
    );
  );

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

var
  CocoaConfigScroller: TCocoaConfigScroller = (
    // the style of the TCocoaScrollBar managed by TCocoaManualScrollView,
    // the default value is System Preferred.
    // note: TCocoaScrollBar not managed by TCocoaManualScrollView is always
    //       Legacy Style.
    preferredStyle: -1;

    // fade in/out time interval, in Seconds
    fadeTimeInterval: 0.02;

    legacy: (
      knob: (
        radius: 4;               // Knob Radius, in Dots
        color: @getCocoaScrollerDefaultKnobColor;
        pos: 3;                  // Knob Position, in Dots
        shrunkSize: 5;           // Knob Shrunk Size, in Dots
        alpha: 0.25;             // Knob Alpha in Normal
        alphaBlack: 0.5;         // Knob Alpha when mouse enters
        fadeStep: 0.05;          // Knob Alpha Step when fading in/out
      );
    );

    overlay: (
      bar: (
        // Overly Style Scroller Auto Show Delay Time, in Seconds
        // used when the Scroller is triggered by double-finger tapping the touchpad
        autoShowDelayTime: 0.2;
        autoHideDelayTime: 0.9;    // Bar Auto Hide Delay Time, in Seconds
        alpha: 0.5;                // Bar Alpha in Normal
        alphaFadeStep: -0.1;       // Bar Alpha Step when fading out
        alphaFadeTo: 0;            // Bar Alpha when fade out ends
        expandTimeInterval: 0.03;  // Bar Expands time interval, in Seconds
        expandSize: 4;             // Bar Expands when the mouse enters, in Dots
      );
      knob: (
        radius: 4;                 // Knob Radius, in Dots
        color: @getCocoaScrollerDefaultKnobColor;
        pos: 5;                    // Knob Position, in Dots
        shrunkSize: 6;             // Knob Shrunk Size, in Dots
        // in extreme cases, the normally calculated Knob size of Overlay Scroller
        // may be too small, keep the min size.
        // min height for the Knob of VerticalScroller
        // min width for the Knob of HorizontalScroller
        minSize: 25;
      )
    );
  );

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
  // Scroller Knob Fade in/out time interval, in Seconds
  CocoaScrollerKnobFadeTimeInterval : Double = 0.02;

  // Scroller Knob Radius, in Dots
  CocoaScrollerKnobRadius : Double = 4;


  // Legacy Style Scroller Knob Color
  CocoaScrollerOverlayStyleKnobColor : NSColorFunction = @getCocoaScrollerDefaultKnobColor;

  // Legacy Style Scroller Knob Alpha in Normal
  CocoaScrollerLegacyStyleAlpha : Double = 0.25;

  // Legacy Style Scroller Knob Alpha when mouse enters
  CocoaScrollerLegacyStyleAlphaBlack : Double = 0.50;

  // Legacy Style Scroller Knob Alpha Step when fading in/out
  CocoaScrollerLegacyStyleFadeStep : Double = 0.05;

  // Legacy Style Scroller Knob Position, in Dots
  CocoaScrollerLegacyStyleKnobPos : Double = 3;

  // Legacy Style Scroller Knob Shrunk Size, in Dots
  CocoaScrollerLegacyStyleKnobShrunkSize : Double = 5;


  // Overly Style Scroller Knob Color
  CocoaScrollerLegacyStyleKnobColor : NSColorFunction = @getCocoaScrollerDefaultKnobColor;

  // Overly Style Scroller Auto Show Delay Time, in Seconds
  // the scrollbar is not shown because the value is updated,
  // but because it is triggered by other factors, such as
  // double-finger tapping the touchpad
  CocoaScrollerOverlayStyleAutoShowDelayTime : Double = 0.2;

  // Overly Style Scroller Auto Hide Delay Time, in Seconds
  CocoaScrollerOverlayStyleAutoHideDelayTime : Double = 0.9;

  // Overlay Style Scroller Alpha in Normal
  CocoaScrollerOverlayStyleAlpha : Double = 0.5;

  // Overlay Style Scroller Alpha Step when fading out
  CocoaScrollerOverlayStyleFadeStep : Double = -0.1;

  // Overlay Style Scroller Alpha when fade out ends
  CocoaScrollerOverlayStyleFadeTo : Double = 0;

  // Overlay Style Scroller expands time interval, in Seconds
  CocoaScrollerOverlayStyleExpandTimeInterval : Double = 0.03;

  // Overlay Style Scroller expands when the mouse enters, in Dots
  CocoaScrollerOverlayStyleExpandSize : Double = 4;

  // Overlay Style Scroller Knob Position, in Dots
  CocoaScrollerOverlayStyleKnobPos : Double = 5;

  // Overlay Style Scroller Knob Shrunk Size, in Dots
  CocoaScrollerOverlayStyleKnobShrunkSize : Double = 6;

  // in extreme cases, the normally calculated Knob size of Overlay Scroller
  // may be too small, keep the min size.
  // min height for the Knob of VerticalScroller
  // min width for the Knob of HorizontalScroller
  CocoaScrollerOverlayStyleKnobMinSize : Double = 25;


  // by default on macOS, Notification is only Presented when the APP is
  // in the background.
  // when CocoaAlwaysPresentNotification is set to True, Notification is
  // always Presented.
  CocoaAlwaysPresentNotification : Boolean = True;


  // for compatiblity with LCL 1.8 release. The macOS base is 72ppi
  CocoaBasePPI : Integer = 96;

  // if set to true, then WS would not assign icons via TCocoaWSForm SetIcon
  // The icon would have to be changed manually. By default LCL behaviour is used
  CocoaIconUse: Boolean = false;
  CocoaToggleBezel : NSBezelStyle = NSRoundedBezelStyle;
  CocoaToggleType  : NSButtonType = NSPushOnPushOffButton;

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

