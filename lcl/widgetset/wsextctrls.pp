{
 *****************************************************************************
 *                               WSExtCtrls.pp                               * 
 *                               -------------                               * 
 *                                                                           *
 *                                                                           *
 *****************************************************************************

 *****************************************************************************
  This file is part of the Lazarus Component Library (LCL)

  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************
}
unit WSExtCtrls;

{$mode objfpc}{$H+}
{$I lcl_defines.inc}

interface
////////////////////////////////////////////////////
// I M P O R T A N T                                
////////////////////////////////////////////////////
// 1) Only class methods allowed
// 2) Class methods have to be published and virtual
// 3) To get as little as posible circles, the uses
//    clause should contain only those LCL units 
//    needed for registration. WSxxx units are OK
// 4) To improve speed, register only classes in the 
//    initialization section which actually 
//    implement something
// 5) To enable your XXX widgetset units, look at
//    the uses clause of the XXXintf.pp
////////////////////////////////////////////////////
uses
////////////////////////////////////////////////////
// To get as little as posible circles,
// uncomment only when needed for registration
////////////////////////////////////////////////////
  LCLProc, Controls, ExtCtrls, Classes, ImgList, Graphics, LResources,
////////////////////////////////////////////////////
  WSLCLClasses, WSControls, WSStdCtrls, WSFactory;

type
  { TWSPage }

  TWSPage = class(TWSCustomControl)
  published
  end;

  { TWSNotebook }

  TWSNotebook = class(TWSCustomControl)
  published
    class function GetDefaultColor(const AControl: TControl;
      const ADefaultColorType: TDefaultColorType): TColor; override;
  end;

  { TWSShape }

  TWSShape = class(TWSGraphicControl)
  published
  end;

  { TWSCustomSplitter }

  TWSCustomSplitter = class(TWSCustomControl)
  published
  end;

  { TWSSplitter }

  TWSSplitter = class(TWSCustomSplitter)
  published
  end;

  { TWSPaintBox }

  TWSPaintBox = class(TWSGraphicControl)
  published
  end;

  { TWSCustomImage }

  TWSCustomImage = class(TWSGraphicControl)
  published
  end;

  { TWSImage }

  TWSImage = class(TWSCustomImage)
  published
  end;

  { TWSBevel }

  TWSBevel = class(TWSGraphicControl)
  published
  end;

  { TWSCustomRadioGroup }

  TWSCustomRadioGroup = class(TWSCustomGroupBox)
  published
  end;

  { TWSRadioGroup }

  TWSRadioGroup = class(TWSCustomRadioGroup)
  published
  end;

  { TWSCustomCheckGroup }

  TWSCustomCheckGroup = class(TWSCustomGroupBox)
  published
  end;

  { TWSCheckGroup }

  TWSCheckGroup = class(TWSCustomCheckGroup)
  published
  end;

  { TWSCustomLabeledEdit }

  TWSCustomLabeledEdit = class(TWSCustomEdit)
  published
  end;

  { TWSLabeledEdit }

  TWSLabeledEdit = class(TWSCustomLabeledEdit)
  published
  end;

  { TWSCustomPanel }

  TWSCustomPanel = class(TWSCustomControl)
  published
    class function GetDefaultColor(const AControl: TControl; const ADefaultColorType: TDefaultColorType): TColor; override;
  end;

  { TWSPanel }

  TWSPanel = class(TWSCustomPanel)
  end;

  { TWSCustomTrayIcon }

  TWSCustomTrayIcon = class(TWSLCLComponent)
  published
    class function Hide(const ATrayIcon: TCustomTrayIcon): Boolean; virtual;
    class function Show(const ATrayIcon: TCustomTrayIcon): Boolean; virtual;
    class procedure InternalUpdate(const ATrayIcon: TCustomTrayIcon); virtual;
    class function ShowBalloonHint(const ATrayIcon: TCustomTrayIcon): Boolean; virtual;
    class function GetPosition(const ATrayIcon: TCustomTrayIcon): TPoint; virtual;
    class function GetCanvas(const ATrayIcon: TCustomTrayIcon): TCanvas; virtual;
  end;
  TWSCustomTrayIconClass = class of TWSCustomTrayIcon;

  { WidgetSetRegistration }

  procedure RegisterShape;
  procedure RegisterCustomSplitter;
  procedure RegisterPaintBox;
  procedure RegisterCustomImage;
  procedure RegisterBevel;
  procedure RegisterCustomRadioGroup;
  procedure RegisterCustomCheckGroup;
  procedure RegisterCustomLabeledEdit;
  procedure RegisterCustomPanel;
  procedure RegisterCustomTrayIcon;

implementation

{ TWSNotebook }

class function TWSNotebook.GetDefaultColor(const AControl: TControl;
  const ADefaultColorType: TDefaultColorType): TColor;
begin
  Result:=DefBtnColors[ADefaultColorType];
end;

{ TWSCustomTrayIcon }

class function TWSCustomTrayIcon.Hide(const ATrayIcon: TCustomTrayIcon): Boolean;
begin
  Result := False;
end;

class function TWSCustomTrayIcon.Show(const ATrayIcon: TCustomTrayIcon): Boolean;
begin
  Result := False;
end;

class procedure TWSCustomTrayIcon.InternalUpdate(const ATrayIcon: TCustomTrayIcon);
begin

end;

{*******************************************************************
*  TWSCustomTrayIcon.ShowBalloonHint ()
*
*  RETURNS:        False if we should use the popupnotifier to implement this method
*                  True if a platform-specific baloon is implemented
*
*******************************************************************}
class function TWSCustomTrayIcon.ShowBalloonHint(const ATrayIcon: TCustomTrayIcon): Boolean;
begin
  Result := False;
end;

class function TWSCustomTrayIcon.GetPosition(const ATrayIcon: TCustomTrayIcon): TPoint;
begin
  Result := Point(0, 0);
end;

class function TWSCustomTrayIcon.GetCanvas(const ATrayIcon: TCustomTrayIcon): TCanvas;
begin
  Result := ATrayIcon.Icon.Canvas;
end;

{ WidgetSetRegistration }

procedure RegisterShape;
const
  Done: Boolean = False;
begin
  if Done then exit;
  WSRegisterShape;
//  if not WSRegisterShape then
//    RegisterWSComponent(TShape, TWSShape);
  Done := True;
end;

procedure RegisterCustomSplitter;
const
  Done: Boolean = False;
begin
  if Done then exit;
  WSRegisterCustomSplitter;
//  if not WSRegisterCustomSplitter then
//    RegisterWSComponent(TCustomSplitter, TWSCustomSplitter);
  Done := True;
end;

procedure RegisterPaintBox;
const
  Done: Boolean = False;
begin
  if Done then exit;
  WSRegisterPaintBox;
//  if not WSRegisterPaintBox then
//    RegisterWSComponent(TPaintBox, TWSPaintBox);
  Done := True;
end;

procedure RegisterCustomImage;
const
  Done: Boolean = False;
begin
  if Done then exit;
  WSRegisterCustomImage;
//  if not WSRegisterCustomImage then
//    RegisterWSComponent(TCustomImage, TWSCustomImage);
  Done := True;
end;

procedure RegisterBevel;
const
  Done: Boolean = False;
begin
  if Done then exit;
  WSRegisterBevel;
//  if not WSRegisterBevel then
//    RegisterWSComponent(TBevel, TWSBevel);
  Done := True;
end;

procedure RegisterCustomRadioGroup;
const
  Done: Boolean = False;
begin
  if Done then exit;
  WSRegisterCustomRadioGroup;
//  if not WSRegisterCustomRadioGroup then
//    RegisterWSComponent(TCustomRadioGroup, TWSCustomRadioGroup);
  Done := True;
end;

procedure RegisterCustomCheckGroup;
const
  Done: Boolean = False;
begin
  if Done then exit;
  WSRegisterCustomCheckGroup;
//  if not WSRegisterCustomCheckGroup then
//    RegisterWSComponent(TCustomCheckGroup, TWSCustomCheckGroup);
  Done := True;
end;

procedure RegisterCustomLabeledEdit;
const
  Done: Boolean = False;
begin
  if Done then exit;
  WSRegisterCustomLabeledEdit;
//  if not WSRegisterCustomLabeledEdit then
//    RegisterWSComponent(TCustomLabeledEdit, TWSCustomLabeledEdit);
  Done := True;
end;

procedure RegisterCustomPanel;
const
  Done: Boolean = False;
begin
  if Done then exit;
  WSRegisterCustomPanel;
  RegisterPropertyToSkip(TCustomPanel, 'VerticalAlignment', 'VCL compatibility property', '');
  RegisterPropertyToSkip(TCustomPanel, 'ExplicitWidth', 'VCL compatibility property', '');
  RegisterPropertyToSkip(TCustomPanel, 'ShowCaption', 'VCL compatibility property', '');
  RegisterPropertyToSkip(TCustomPanel, 'ParentBackground', 'VCL compatibility property', '');
  RegisterPropertyToSkip(TCustomPanel, 'BevelEdges', 'VCL compatibility property', '');
  RegisterPropertyToSkip(TCustomPanel, 'BevelKind', 'VCL compatibility property', '');
//  if not WSRegisterCustomPanel then
//    RegisterWSComponent(TCustomPanel, TWSCustomPanel);
  Done := True;
end;

procedure RegisterCustomTrayIcon;
const
  Done: Boolean = False;
begin
  if Done then exit;
  if not WSRegisterCustomTrayIcon then
    RegisterWSComponent(TCustomTrayIcon, TWSCustomTrayIcon);
  Done := True;
end;

{ TWSCustomPanel }

class function TWSCustomPanel.GetDefaultColor(const AControl: TControl; const ADefaultColorType: TDefaultColorType): TColor;
begin
  Result := DefBtnColors[ADefaultColorType];
end;

end.
