// no need to set TCocoaConfigFocusRing.Strategy.default control
// the controls not in StrategySetting are TCocoaConfigFocusRing.Strategy.default
setStrategy( TCocoaConfigFocusRing.Strategy.none, NSSTR('TCocoaTabControl') );
setStrategy( TCocoaConfigFocusRing.Strategy.none, NSSTR('TCocoaButton') );
setStrategy( TCocoaConfigFocusRing.Strategy.none, NSSTR('TCocoaTextField') );
setStrategy( TCocoaConfigFocusRing.Strategy.none, NSSTR('TCocoaComboBox') );
setStrategy( TCocoaConfigFocusRing.Strategy.none, NSSTR('TCocoaReadOnlyComboBox') );
setStrategy( TCocoaConfigFocusRing.Strategy.none, NSSTR('TCocoaTableListView') );
setStrategy( TCocoaConfigFocusRing.Strategy.none, NSSTR('TCocoaCollectionView') );

setStrategy( TCocoaConfigFocusRing.Strategy.border, NSSTR('TCocoaTextView') );
