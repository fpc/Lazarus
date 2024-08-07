unit DockedStrConsts;

{$mode objfpc}{$H+}

interface

uses
  // LCL
  ComCtrls;

resourceString
  SCode                       = 'Code';
  SDesigner                   = 'Form';
  SAnchors                    = 'Anchors';
  SAllowSizingCaption         = 'Allow size changing';
  SAllowSizingHint            = 'Grips can change size in Anchor Editor';
  SAnchorControlBorderCaption = 'Border Around Control';
  SAnchorControlColorCaption  = 'Control Background';
  SAnchorTabVisibleCaption    = 'Show Anchors Tab';
  SAnchorTabVisibleHint       = 'Show Anchor Editor';
  SAnchorTargetColorCaption   = 'Anchor Target';
  SAnchorTopColorCaption      = 'Top Anchor';
  SAnchorLeftColorCaption     = 'Left Anchor';
  SAnchorRightColorCaption    = 'Right Anchor';
  SAnchorBottomColorCaption   = 'Bottom Anchor';
  SAttachControl              = 'Attach Control';
  SAttachPoint                = 'Attach Control Sides';
  SAttachSide                 = 'Attach Control Side';
  SCaptionDockedFormEditor    = 'Docked Form Editor';
  SCaptureDistanceCaption     = 'Capture Distance';
  SCaptureDistanceHint        = 'Minimal distance to capture a control with mouse';
  SCircularDependency         = 'This will create a circular dependency.';
  SColorsCaption              = 'Colors';
  SDetachControl              = 'Detach Control';
  SDetachPoint                = 'Detach Control Sides';
  SDetachSide                 = 'Detach Control Side';
  SForceRefreshingCaption     = 'Force Refreshing At Sizing';
  SForceRefreshingHint        = 'Force refreshing form when user is sizing it';
  SMouseBorderFactorCaption   = 'Mouse cursor factor for BorderSpacing property';
  SMouseBorderFactorHint      = 'Mouse cursor factor is used to set small values of BorderSpacing property more precisely';
  SResizerColorCaption        = 'Resizer Color';
  SResizerColorHint           = 'Color of resizer bars';
  STabPositionCaption         = 'Tab Position';
  STabPositionHint            = 'Position of Code, Form, Anchors tabs';
  STreatAlignCaption          = 'Automatically treat Align properties';
  STreatAlignHint             = 'Automatically replace Align properties with appropriate Anchors';
  STreatBorderCaption         = 'Automatically treat BorderSpacing properties';
  STreatBorderHint            = 'Automatically replace BorderSpacing.Around properties of controls and adapt BorderSpacing.Left/Right/Top/Bottom if needed';
  SWarningCaption             = 'Warning';

  STabPositionTop             = 'Top';
  STabPositionBottom          = 'Bottom';
  STabPositionLeft            = 'Left';
  STabPositionRight           = 'Right';

  SArgumentOutOfRange         = 'Argument out of range.';
  setupDesignerClassic = 'Classic Form Editor (floating):';
  setupDesignerFloat = 'The form editor shows the edited form as a normal window.';
  setupDesignerModern = 'Modern Form Editor (docked/tabbed):';
  setupDesignerDocked = 'The form editor is part of the IDE''s source editor (tabbed interface).';
  setupDesignerInfo = 'The "classic" designer can be positioned on your screen independent of ' +
    'other parts of the IDE. The "modern" docked designer will be embedded into the ' +
    'source-edit window, and you will have a tab to toggle between source and form. ' +
    'You can open a 2nd source-edit window to see both at the same time.';
  setupMultiWindowIDEOption = 'You can change this in the IDE options under: "Docked Form Editor"';
  SDisableRequiresRestart = 'Disabled (requires restart)';
  SIDELayout = 'IDE Layout';
  SFormEditor = 'Form editor / Designer';

const
  STabPosition: array [Low(TTabPosition)..High(TTabPosition)] of String = (
    STabPositionTop,
    STabPositionBottom,
    STabPositionLeft,
    STabPositionRight);

implementation

end.

