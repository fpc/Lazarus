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
  SAnchorTabVisibleCaption    = 'Show Tab Anchors';
  SAnchorTabVisibleHint       = 'Show Anchor Designer';
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
  SCaptionDockedFormEditor    = 'Docked Form Editor';
  SCaptureDistanceCaption     = 'Capture Distance';
  SCaptureDistanceHint        = 'Minimal distance to capture a control with mouse';
  SCircularDependency         = 'This will create a circular dependency';
  SColorsCaption              = 'Colors';
  SForceRefreshingCaption     = 'Force Refreshing At Sizing';
  SForceRefreshingHint        = 'Force refreshing form when user is sizing it';
  SMouseBorderFactorCaption   = 'Mouse Border Factor';
  SMouseBorderFactorHint      = 'Factor to border more precise small distances';
  SResizerColorCaption        = 'Resizer Color';
  SResizerColorHint           = 'Color of resizer bars';
  STabPositionCaption         = 'Tab Position';
  STabPositionHint            = 'Position of Tab Code, Designer, Anchors';
  STreatAlignCaption          = 'Automatic Treat Aligns';
  STreatAlignHint             = 'Automatically replacing Aligns with appropriate Anchors';
  STreatBorderCaption         = 'Automatic Treat Around Border';
  STreatBorderHint            = 'Automatically replacing Bordering Around and adapt side Borderings if needed';
  SWarningCaption             = 'Warning';

  STabPositionTop             = 'Top';
  STabPositionBottom          = 'Bottom';
  STabPositionLeft            = 'Left';
  STabPositionRight           = 'Right';

  SArgumentOutOfRange         = 'Argument out of range';

const
  STabPosition: array [Low(TTabPosition)..High(TTabPosition)] of String = (
    STabPositionTop,
    STabPositionBottom,
    STabPositionLeft,
    STabPositionRight);

implementation

end.

