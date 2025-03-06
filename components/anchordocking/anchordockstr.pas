{ For license see anchordocking.pas
}
unit AnchorDockStr;

{$mode objfpc}{$H+}

interface

resourcestring
  adrsClose = 'Close';
  adrsMinimize = 'Minimize';
  adrsQuit = 'Quit %s';
  adrsTabPosition = 'Tab position';
  adrsMovePageRight = 'Move page right';
  adrsMovePageRightmost = 'Move page rightmost';
  adrsUndock = 'Undock';
  adrsHeaderPosition = 'Header position';
  adrsEnlargeSide = 'Enlarge %s side';
  adrsMerge = 'Merge';
  adrsAutomatically = 'Automatically';
  adrsLeft = 'left';
  adrsTop = 'top';
  adrsRight = 'right';
  adrsBottom = 'bottom';
  adrsLocked = 'Locked';
  adrsDockingOptions = 'Docking options';
  adrsMovePageLeft = 'Move page left';
  adrsMovePageLeftmost = 'Move page leftmost';
  adrsRequestedButCreated = '%s requested, but %s created';
  adrsDragAndDockC = 'Use the mouse to drag and dock window "%s"';
  adrsMissingControlName = 'missing control name';
  adrsModalFormsCanNotBeMadeDockable = 'modal forms can not be made dockable';
  adrsControlIsAlreadyADocksite = 'control is already a docksite';
  adrsNotSupportedHasParent = 'Not supported: %s has parent %s';
  adrsAnchorNotFoundNodeAnchors = 'Anchor not found: Node="%s" Anchors[%s]="%s"';
  adrsAnchorIsNotSplitterNodeAnchors = 'Anchor is not splitter: Node="%s" Anchors[%s]="%s"';
  adrsAFreeSideOfASplitterMustNotBeAnchoredNodeTypeAncho = 'A free side of a '
    +'splitter must not be anchored: Node="%s" Type=%s Anchors[%s]="%s"';
  adrsAPageMustNotBeAnchoredNodeParentParentTypeAnchors = 'A page must not be '
    +'anchored: Node="%s" Parent=%s ParentType=%s Anchors[%s]="%s"';
  adrsAnchorToWrongSideOfSplitterNodeAnchors = 'Anchor to wrong side of '
    +'splitter: Node="%s" Anchors[%s]="%s"';
  adrsNoChildrenAllowedForNodeType = 'No children allowed for Node="%s" Type=%s';
  adrsCustomDockSiteCanHaveOnlyOneSite = 'Custom dock site "%s" can have only one site.';
  adrsEmptyName = 'Empty name: ';
  adrsDuplicateName = 'Duplicate name: ';
  adrsDragThreshold = 'Drag threshold';
  adrsGeneralDockingOptions = 'General docking options';
  adrsAmountOfPixelTheMouseHasToDragBeforeDragStarts = 'Amount of pixel the '
    +'mouse has to drag before drag starts';
  adrsHeaderAlignTop = 'Header align top';
  adrsMoveHeaderToTopWhenWidthHeight100HeaderAlignTop = 'Move header to top '
    +'when (Width/Height)*100<=HeaderAlignTop';
  adrsHeaderAlignLeft = 'Header align left';
  adrsMoveHeaderToLeftWhenWidthHeight100HeaderAlignLeft = 'Move header to '
    +'left when (Width/Height)*100>=HeaderAlignLeft';
  adrsSplitterWidth = 'Splitter width';
  adrsSplitterResizeStyle = 'Splitter resize style:';
  adrsSplitterThickness = 'Splitter thickness';
  adrsScaleOnResize = 'Scale on resize';
  adrsScaleSubSitesWhenASiteIsResized ='Scale sub sites when a site is resized';
  adrsShowHeaderCaptions = 'Show header captions';
  adrsShowCaptionsOfDockedControlsInTheHeader = 'Show captions of docked '
    +'controls in the header';
  adrsShowHeaders = 'Show headers';
  adrsEachDockedWindowHasAHeaderThatAllowsDraggingHasACo = 'Each docked window'
    +' has a header that allows dragging, has a context menu with extra layout'
    +' functions and shows the caption of the docked window';
  adrsNoCaptionsForFloatingSites = 'No captions for floating sites';
  adrsHideHeaderCaptionsForSitesWithOnlyOneDockedControl = 'Hide header '
    +'captions for sites with only one docked control, as that is already '
    +'shown in the normal window title';
  adrsToUseAnchordockingYouMustFirstUninstall = 'To use anchordocking you '
    +'must first uninstall %s';
  adrsThereIsAnotherDockMasterInstalledOnlyOneDockingPac = 'There is another '
    +'dock master installed. Only one docking package can be installed at a '
    +'time. Please uninstall the other dock master %s and restart the IDE';
  adrsDockingAnchordocking = 'Docking / Anchordocking';
  adrsHeaderStyle = 'Header Style:';
  adrsFlattenHeaders = 'Flatten headers';
  adrsFlattenHeadersHint = 'Flatten headers of docked controls';
  adrsFilledHeaders = 'Fill headers';
  adrsFilledHeadersHint = 'Fill headers of docked controls';
  adrsHighlightFocused = 'Highlight focused';
  adrsHighlightFocusedHint = 'Highlight header of focused docked control';
  adrsAllowDockSitesToBeMinimized = 'Allow dock sites to be minimized';
  adrsMultiLinePages = 'Multiline Tabs';
  adrsMultiLinePagesHint = 'Tabs of pages can be shown in multiple lines';
  adrsFloatingWindowsOnTop = 'Floating windows on top';
  adrsFloatingWindowsOnTopHint = 'Show floating windows on top of main form';
  adrsFlatHeadersButtons = 'Flat header buttons';
  adrsFlatHeadersButtonsHint = 'Flat buttons in headers of docked controls';
  SIDELayout = 'IDE Layout';
  setupMultiWindowIDEClassic = 'Classic IDE (Multi-Window):';
  setupMultiWindowIDESeperateWindows = 'There are separate windows for each part of ' +
    'the IDE (Editor, Components, Debug,...). ' +
    'They can all be individually moved around on the screen.';
  setupMultiWindowIDEModern = 'Modern IDE (Single-Window):';
  setupMultiWindowIDESingleWindow = 'The IDE consists out of a single Window. ' +
    'Different parts of the IDE are shown in panels of that Window. Some can be reached ' +
    'through a tabbed interface.';
  setupMultiWindowIDEInfo = 'The "modern" single Window mode still allows you to undock ' +
    'individual Windows and have several groups of Windows. The IDE will show additional "Dock-Handles" ' +
    'in each Window, which will allow you to change layout, but also require additional space.';
  setupMultiWindowIDEOption = 'You can change this in the IDE options under: "Anchordocking"';
  adrsEnableAnchorDock = 'Enable docking of IDE Windows (Requires IDE restart)';
  adrsEnableAnchorDockHint = 'This option allows to organize groups of windows (or all windows) ' +
    'into a single window. For a "classic" IDE with only floating windows turn the option off.';
  SSingleMultiWindow = 'Single/Multi Window';

implementation

end.

