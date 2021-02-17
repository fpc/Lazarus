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

const
  STabPosition: array [Low(TTabPosition)..High(TTabPosition)] of String = (
    STabPositionTop,
    STabPositionBottom,
    STabPositionLeft,
    STabPositionRight);

implementation

end.

