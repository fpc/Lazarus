{%MainUnit cocoaconfig.pas}

{
  default config of Cocoa FocusRing.
  we can modify the default config in the APP.

  on macOS, the FocusRing takes up extra space, which may cause strange
  display in some cases. it may block other controls, or be partially cut off.

  for example, in the Lazarus IDE - About dialog, the FocusRing of the
  Tab of TPageControl is partially cut off.

  by providing a configurable infrastructure, FocusRing can be controlled
  for different control types.

  there are four Strategy:
  1. default      by macOS Default
  2. none         no FoucsRing
  3. required     have FocusRing
  4. border       by LCL Control Border
}

// no need to set TCocoaConfigFocusRing.Strategy.default control
// the controls not in StrategySetting are TCocoaConfigFocusRing.Strategy.default
setStrategy( TCocoaConfigFocusRing.Strategy.none, NSSTR('TCocoaTabControl') );
setStrategy( TCocoaConfigFocusRing.Strategy.none, NSSTR('TCocoaButton') );
setStrategy( TCocoaConfigFocusRing.Strategy.none, NSSTR('TCocoaTextField') );
setStrategy( TCocoaConfigFocusRing.Strategy.none, NSSTR('TCocoaSpinEdit') );
setStrategy( TCocoaConfigFocusRing.Strategy.none, NSSTR('TCocoaComboBox') );
setStrategy( TCocoaConfigFocusRing.Strategy.none, NSSTR('TCocoaTableListView') );
setStrategy( TCocoaConfigFocusRing.Strategy.none, NSSTR('TCocoaCollectionView') );

setStrategy( TCocoaConfigFocusRing.Strategy.border, NSSTR('TCocoaTextView') );
