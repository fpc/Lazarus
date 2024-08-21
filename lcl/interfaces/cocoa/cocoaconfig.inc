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

var
  CocoaConfigNotification: TCocoaConfigNotification = (
    // by default on macOS, Notification is only Presented when the APP is
    // in the background.
    // when CocoaAlwaysPresentNotification is set to True, Notification is
    // always Presented.
    alwaysPresent: True;
  );
