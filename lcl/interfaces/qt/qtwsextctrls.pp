{ $Id$}
{
 *****************************************************************************
 *                              QtWSExtCtrls.pp                              * 
 *                              ---------------                              * 
 *                                                                           *
 *                                                                           *
 *****************************************************************************

 *****************************************************************************
  This file is part of the Lazarus Component Library (LCL)

  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************
}
unit QtWSExtCtrls;

{$mode objfpc}{$H+}

interface

{$I qtdefines.inc}

uses
  // Bindings
  qt4,
  qtwidgets, qtobjects, qtproc, QtWSControls,
  // LCL
  SysUtils, Classes, Controls, Graphics, Forms, ExtCtrls, LCLType, LazUTF8,
  // Widgetset
  WSExtCtrls, WSLCLClasses;

type
  { TQtWSPage }

  TQtWSPage = class(TWSPage)
  published
  end;

  { TQtWSNotebook }

  TQtWSNotebook = class(TWSNotebook)
  published
  end;

  { TQtWSShape }

  TQtWSShape = class(TWSShape)
  published
  end;

  { TQtWSCustomSplitter }

  TQtWSCustomSplitter = class(TWSCustomSplitter)
  published
  end;

  { TQtWSSplitter }

  TQtWSSplitter = class(TWSSplitter)
  published
  end;

  { TQtWSPaintBox }

  TQtWSPaintBox = class(TWSPaintBox)
  published
  end;

  { TQtWSCustomImage }

  TQtWSCustomImage = class(TWSCustomImage)
  published
  end;

  { TQtWSImage }

  TQtWSImage = class(TWSImage)
  published
  end;

  { TQtWSBevel }

  TQtWSBevel = class(TWSBevel)
  published
  end;

  { TQtWSCustomRadioGroup }

  TQtWSCustomRadioGroup = class(TWSCustomRadioGroup)
  published
    class function  CreateHandle(const AWinControl: TWinControl;
      const AParams: TCreateParams): TLCLIntfHandle; override;
  end;

  { TQtWSRadioGroup }

  TQtWSRadioGroup = class(TWSRadioGroup)
  published
  end;

  { TQtWSCustomCheckGroup }

  TQtWSCustomCheckGroup = class(TWSCustomCheckGroup)
  published
    class function  CreateHandle(const AWinControl: TWinControl;
      const AParams: TCreateParams): TLCLIntfHandle; override;
  end;

  { TQtWSCheckGroup }

  TQtWSCheckGroup = class(TWSCheckGroup)
  published
  end;

  { TQtWSCustomLabeledEdit }

  TQtWSCustomLabeledEdit = class(TWSCustomLabeledEdit)
  published
  end;

  { TQtWSLabeledEdit }

  TQtWSLabeledEdit = class(TWSLabeledEdit)
  published
  end;

  { TQtWSCustomPanel }

  TQtWSCustomPanel = class(TWSCustomPanel)
  published
    class function CreateHandle(const AWinControl: TWinControl;
          const AParams: TCreateParams): TLCLIntfHandle; override;
    class function GetDefaultColor(const AControl: TControl; const ADefaultColorType: TDefaultColorType): TColor; override;
  end;

  { TQtWSPanel }

  TQtWSPanel = class(TWSPanel)
  published
  end;

  { TQtWSCustomTrayIcon }

  TQtWSCustomTrayIcon = class(TWSCustomTrayIcon)
  published
    class function Hide(const ATrayIcon: TCustomTrayIcon): Boolean; override;
    class function Show(const ATrayIcon: TCustomTrayIcon): Boolean; override;
    class procedure InternalUpdate(const ATrayIcon: TCustomTrayIcon); override;
    class function ShowBalloonHint(const ATrayIcon: TCustomTrayIcon): Boolean; override;
    class function GetPosition(const ATrayIcon: TCustomTrayIcon): TPoint; override;
    class function GetCanvas(const ATrayIcon: TCustomTrayIcon): TCanvas; override;
  end;

implementation
uses qtsystemtrayicon;

{ TQtWSCustomPanel }

{------------------------------------------------------------------------------
  Method: TQtWSCustomPanel.CreateHandle
  Params:  None
  Returns: Nothing

  Allocates memory and resources for the control and shows it
 ------------------------------------------------------------------------------}
class function TQtWSCustomPanel.CreateHandle(const AWinControl: TWinControl;
  const AParams: TCreateParams): TLCLIntfHandle;
var
  QtFrame: TQtFrame;
begin
  QtFrame := TQtFrame.Create(AWinControl, AParams);
  QtFrame.AttachEvents;

  // Set's initial properties
  QtFrame.setFrameShape(TBorderStyleToQtFrameShapeMap[TCustomPanel(AWinControl).BorderStyle]);
  
  // Return the Handle
  Result := TLCLIntfHandle(QtFrame);
end;

class function TQtWSCustomPanel.GetDefaultColor(const AControl: TControl;
  const ADefaultColorType: TDefaultColorType): TColor;
const
  DefColors: array[TDefaultColorType] of TColor = (
 { dctBrush } clBackground,
 { dctFont  } clBtnText
  );
begin
  Result := DefColors[ADefaultColorType];
end;

{ TQtWSCustomRadioGroup }

{------------------------------------------------------------------------------
  Method: TQtWSCustomRadioGroup.CreateHandle
  Params:  None
  Returns: Nothing

  Allocates memory and resources for the control and shows it
 ------------------------------------------------------------------------------}

class function TQtWSCustomRadioGroup.CreateHandle(const AWinControl: TWinControl;
  const AParams: TCreateParams): TLCLIntfHandle;
var
  QtGroupBox: TQtGroupBox;
  Str: WideString;
begin
  QtGroupBox := TQtGroupBox.Create(AWinControl, AParams);
  QtGroupBox.GroupBoxType := tgbtRadioGroup;

  Str := AWinControl{%H-}.Caption;
  QGroupBox_setTitle(QGroupBoxH(QtGroupBox.Widget), @Str);

  QtGroupBox.AttachEvents;

  Result := TLCLIntfHandle(QtGroupBox);
end;

{ TQtWSCustomCheckGroup }

{------------------------------------------------------------------------------
  Method: TQtWSCustomCheckGroup.CreateHandle
  Params:  None
  Returns: Nothing

  Allocates memory and resources for the control and shows it
 ------------------------------------------------------------------------------}
class function TQtWSCustomCheckGroup.CreateHandle(const AWinControl: TWinControl;
  const AParams: TCreateParams): TLCLIntfHandle;
var
  QtGroupBox: TQtGroupBox;
  Str: WideString;
begin
  QtGroupBox := TQtGroupBox.Create(AWinControl, AParams);
  QtGroupBox.GroupBoxType := tgbtCheckGroup;

  Str := AWinControl{%H-}.Caption;
  QGroupBox_setTitle(QGroupBoxH(QtGroupBox.Widget), @Str);

  QtGroupBox.AttachEvents;

  Result := TLCLIntfHandle(QtGroupBox);
end;

{ TQtWSCustomTrayIcon }

class function TQtWSCustomTrayIcon.Hide(const ATrayIcon: TCustomTrayIcon): Boolean;
var
  SystemTrayIcon: TQtSystemTrayIcon;
begin
  Result := False;

  SystemTrayIcon := TQtSystemTrayIcon(ATrayIcon.Handle);

  SystemTrayIcon.Hide;

  SystemTrayIcon.Free;

  ATrayIcon.Handle := 0;

  Result := True;
end;

class function TQtWSCustomTrayIcon.Show(const ATrayIcon: TCustomTrayIcon): Boolean;
var
  Text: WideString;
  SystemTrayIcon: TQtSystemTrayIcon;
  IconH: QIconH;
begin
  Result := False;

  if ATrayIcon.Icon.Handle = 0 then
    IconH := nil
  else
    IconH := TQtIcon(ATrayIcon.Icon.Handle).Handle;
    
  SystemTrayIcon := TQtSystemTrayIcon.Create(IconH);
  SystemTrayIcon.FTrayIcon := ATrayIcon;

  ATrayIcon.Handle := HWND(SystemTrayIcon);

  Text := ATrayIcon{%H-}.Hint;
  SystemTrayIcon.setToolTip(Text);

  if Assigned(ATrayIcon.PopUpMenu) then
    if TQtMenu(ATrayIcon.PopUpMenu.Handle).Widget <> nil then
      SystemTrayIcon.setContextMenu(QMenuH(TQtMenu(ATrayIcon.PopUpMenu.Handle).Widget));

  SystemTrayIcon.show;

  Result := True;
end;

{*******************************************************************
*  TQtWSCustomTrayIcon.InternalUpdate ()
*
*  DESCRIPTION:    Makes modifications to the Icon while running
*                  i.e. without hiding it and showing again
*******************************************************************}
class procedure TQtWSCustomTrayIcon.InternalUpdate(const ATrayIcon: TCustomTrayIcon);
var
  SystemTrayIcon: TQtSystemTrayIcon;
  AIcon: QIconH;
  AHint: WideString;
begin
  if (ATrayIcon.Handle = 0) then Exit;

  SystemTrayIcon := TQtSystemTrayIcon(ATrayIcon.Handle);
  if Assigned(ATrayIcon.Icon) then
  begin
    // animate
    if ATrayIcon.Animate and Assigned(ATrayIcon.Icons) then
      SystemTrayIcon.setIcon(TQtImage(ATrayIcon.Icon.BitmapHandle).AsIcon)
    else
    // normal
    if (ATrayIcon.Icon.Handle <> 0) then
      SystemTrayIcon.setIcon(TQtIcon(ATrayIcon.Icon.Handle).Handle)
    else
    begin
      AIcon := QIcon_create();
      SystemTrayIcon.setIcon(AIcon);
      QIcon_destroy(AIcon);
    end;
  end else
  begin
    AIcon := QIcon_create;
    SystemTrayIcon.setIcon(AIcon);
    QIcon_destroy(AIcon);
  end;

  { PopUpMenu }
  if Assigned(ATrayIcon.PopUpMenu) then
    if TQtMenu(ATrayIcon.PopUpMenu.Handle).Widget <> nil then
      SystemTrayIcon.setContextMenu(QMenuH(TQtMenu(ATrayIcon.PopUpMenu.Handle).Widget));

  AHint := ATrayIcon{%H-}.Hint;
  SystemTrayIcon.setToolTip(AHint);

  SystemTrayIcon.UpdateSystemTrayWidget;
end;

class function TQtWSCustomTrayIcon.ShowBalloonHint(
  const ATrayIcon: TCustomTrayIcon): Boolean;
var
  QtTrayIcon: TQtSystemTrayIcon;
begin
  Result := False;
  if (ATrayIcon.Handle = 0) then Exit;
  QtTrayIcon := TQtSystemTrayIcon(ATrayIcon.Handle);

  QtTrayIcon.showBaloonHint(ATrayIcon.BalloonTitle, ATrayIcon.BalloonHint,
    QSystemTrayIconMessageIcon(Ord(ATrayIcon.BalloonFlags)),
    ATrayIcon.BalloonTimeout);

  Result := True;
end;

class function TQtWSCustomTrayIcon.GetPosition(const ATrayIcon: TCustomTrayIcon): TPoint;
begin
  Result := Point(0, 0);
  if (ATrayIcon.Handle = 0) then
    exit;
  Result := TQtSystemTrayIcon(ATrayIcon.Handle).GetPosition;
end;

class function TQtWSCustomTrayIcon.GetCanvas(const ATrayIcon: TCustomTrayIcon
  ): TCanvas;
begin
  Result := nil;
  if (ATrayIcon.Handle <> 0) then
    Result := TQtSystemTrayIcon(ATrayIcon.Handle).Canvas;
end;

end.
