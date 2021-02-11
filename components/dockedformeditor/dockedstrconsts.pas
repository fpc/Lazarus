unit DockedStrConsts;

{$mode objfpc}{$H+}

interface

uses
  // LCL
  ComCtrls;

resourceString
  SCode                       = 'Code';
  SDesigner                   = 'Designer';
  SAnchors                    = 'Anchors';
  SAnchorControlBorderCaption = 'Border Around Control';
  SAnchorControlBorderHint    = 'Color of borders around selected control';
  SAnchorControlColorCaption  = 'Control Background';
  SAnchorControlColorHint     = 'Normal panel background color';
  SAnchorTabVisibleCaption    = 'Show Anchors Tab';
  SAnchorTabVisibleHint       = 'Show Anchor Editor';
  SAnchorTargetColorCaption   = 'Anchor Target';
  SAnchorTargetColorHint      = 'Color of selected Anchor of selected control';
  SAnchorTopColorCaption      = 'Top Anchor';
  SAnchorTopColorHint         = 'Color of top side, when top is anchored';
  SAnchorLeftColorCaption     = 'Left Anchor';
  SAnchorLeftColorHint        = 'Color of left side, when left is anchored';
  SAnchorRightColorCaption    = 'Right Anchor';
  SAnchorRightColorHint       = 'Color of right side, when right is anchored';
  SAnchorBottomColorCaption   = 'Bottom Anchor';
  SAnchorBottomColorHint      = 'Color of bottom side, when bottom is anchored';
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
  SMouseBorderFactorCaption   = 'Mouse Border Factor';
  SMouseBorderFactorHint      = 'Factor to border more precise small distances';
  SResizerColorCaption        = 'Resizer Color';
  SResizerColorHint           = 'Color of resizer bars';
  STabPositionCaption         = 'Tab Position';
  STabPositionHint            = 'Position of Tab Code, Designer, Anchors';
  STreatAlignCaption          = 'Automatically treat Aligns';
  STreatAlignHint             = 'Automatically replace Aligns with appropriate Anchors';
  STreatBorderCaption         = 'Automatically treat Around Border';
  STreatBorderHint            = 'Automatically replace BorderSpace around the control and adapt side BorderSpaces if needed';
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

