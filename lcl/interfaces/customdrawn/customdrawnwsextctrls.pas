{
 *****************************************************************************
 *                          CustomDrawnWSExtCtrls.pp                         *
 *                              ---------------                              * 
 *                                                                           *
 *                                                                           *
 *****************************************************************************

 *****************************************************************************
 *                                                                           *
 *  This file is part of the Lazarus Component Library (LCL)                 *
 *                                                                           *
 *  See the file COPYING.modifiedLGPL.txt, included in this distribution,    * 
 *  for details about the copyright.                                         *
 *                                                                           *
 *  This program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
 *                                                                           *
 *****************************************************************************
}
unit CustomDrawnWSExtCtrls;

{$mode objfpc}{$H+}

interface

//{$I qtdefines.inc}

uses
  // LCL
  LCLProc,
  SysUtils, Classes, Controls, Graphics, Forms, ExtCtrls, LCLType,
  ImgList,
  // Widgetset
  WSExtCtrls, WSProc, WSLCLClasses,
  customdrawncontrols;

type
  { TCDWSPage }

  TCDWSPage = class(TWSPage)
  published
  end;

  { TCDWSNotebook }

  TCDWSNotebook = class(TWSNotebook)
  published
  end;

  { TCDWSShape }

  TCDWSShape = class(TWSShape)
  published
  end;

  { TCDWSCustomSplitter }

  TCDWSCustomSplitter = class(TWSCustomSplitter)
  published
  end;

  { TCDWSSplitter }

  TCDWSSplitter = class(TWSSplitter)
  published
  end;

  { TCDWSPaintBox }

  TCDWSPaintBox = class(TWSPaintBox)
  published
  end;

  { TCDWSCustomImage }

  TCDWSCustomImage = class(TWSCustomImage)
  published
  end;

  { TCDWSImage }

  TCDWSImage = class(TWSImage)
  published
  end;

  { TCDWSBevel }

  TCDWSBevel = class(TWSBevel)
  published
  end;

  { TCDWSCustomRadioGroup }

  TCDWSCustomRadioGroup = class(TWSCustomRadioGroup)
  published
{    class function  CreateHandle(const AWinControl: TWinControl;
      const AParams: TCreateParams): TLCLIntfHandle; override;}
  end;

  { TCDWSRadioGroup }

  TCDWSRadioGroup = class(TWSRadioGroup)
  published
  end;

  { TCDWSCustomCheckGroup }

  TCDWSCustomCheckGroup = class(TWSCustomCheckGroup)
  published
{    class function  CreateHandle(const AWinControl: TWinControl;
      const AParams: TCreateParams): TLCLIntfHandle; override;}
  end;

  { TCDWSCheckGroup }

  TCDWSCheckGroup = class(TWSCheckGroup)
  published
  end;

  { TCDWSCustomLabeledEdit }

  TCDWSCustomLabeledEdit = class(TWSCustomLabeledEdit)
  published
  end;

  { TCDWSLabeledEdit }

  TCDWSLabeledEdit = class(TWSLabeledEdit)
  published
  end;

  { TCDWSCustomPanel }

  TCDWSCustomPanel = class(TWSCustomPanel)
  public
    class procedure CreateCDControl(const AWinControl: TWinControl; var ACDControlField: TCDControl);
  published
    class function CreateHandle(const AWinControl: TWinControl;
          const AParams: TCreateParams): TLCLIntfHandle; override;
  end;

  { TCDWSPanel }

  TCDWSPanel = class(TWSPanel)
  published
  end;

  { TCDWSCustomTrayIcon }

  TCDWSCustomTrayIcon = class(TWSCustomTrayIcon)
  published
{    class function Hide(const ATrayIcon: TCustomTrayIcon): Boolean; override;
    class function Show(const ATrayIcon: TCustomTrayIcon): Boolean; override;
    class procedure InternalUpdate(const ATrayIcon: TCustomTrayIcon); override;
    class function ShowBalloonHint(const ATrayIcon: TCustomTrayIcon): Boolean; override;
    class function GetPosition(const ATrayIcon: TCustomTrayIcon): TPoint; override;}
  end;

implementation

{ TCDWSCustomRadioGroup }

(*{------------------------------------------------------------------------------
  Method: TCDWSCustomRadioGroup.CreateHandle
  Params:  None
  Returns: Nothing

  Allocates memory and resources for the control and shows it
 ------------------------------------------------------------------------------}

class function TCDWSCustomRadioGroup.CreateHandle(const AWinControl: TWinControl;
  const AParams: TCreateParams): TLCLIntfHandle;
var
  QtGroupBox: TQtGroupBox;
  Str: WideString;
begin
  QtGroupBox := TQtGroupBox.Create(AWinControl, AParams);
  QtGroupBox.GroupBoxType := tgbtRadioGroup;

  Str := GetUtf8String(AWinControl.Caption);
  QGroupBox_setTitle(QGroupBoxH(QtGroupBox.Widget), @Str);

  QtGroupBox.AttachEvents;

  Result := TLCLIntfHandle(QtGroupBox);
end;

{ TCDWSCustomCheckGroup }

{------------------------------------------------------------------------------
  Method: TCDWSCustomCheckGroup.CreateHandle
  Params:  None
  Returns: Nothing

  Allocates memory and resources for the control and shows it
 ------------------------------------------------------------------------------}
class function TCDWSCustomCheckGroup.CreateHandle(const AWinControl: TWinControl;
  const AParams: TCreateParams): TLCLIntfHandle;
var
  QtGroupBox: TQtGroupBox;
  Str: WideString;
begin
  QtGroupBox := TQtGroupBox.Create(AWinControl, AParams);
  QtGroupBox.GroupBoxType := tgbtCheckGroup;

  Str := GetUtf8String(AWinControl.Caption);
  QGroupBox_setTitle(QGroupBoxH(QtGroupBox.Widget), @Str);

  QtGroupBox.AttachEvents;

  Result := TLCLIntfHandle(QtGroupBox);
end;          *)

{ TCDWSCustomPanel }

class procedure TCDWSCustomPanel.CreateCDControl(
  const AWinControl: TWinControl; var ACDControlField: TCDControl);
begin

end;

{------------------------------------------------------------------------------
  Method: TCDWSCustomPanel.CreateHandle
  Params:  None
  Returns: Nothing

  Allocates memory and resources for the control and shows it
 ------------------------------------------------------------------------------}
class function TCDWSCustomPanel.CreateHandle(const AWinControl: TWinControl;
  const AParams: TCreateParams): TLCLIntfHandle;
begin
{  QtFrame := TQtFrame.Create(AWinControl, AParams);
  QtFrame.AttachEvents;

  // Set's initial properties
  QtFrame.setFrameShape(TBorderStyleToQtFrameShapeMap[TCustomPanel(AWinControl).BorderStyle]);

  // Return the Handle
  Result := TLCLIntfHandle(QtFrame);}
end;

(*{ TCDWSCustomTrayIcon }

class function TCDWSCustomTrayIcon.Hide(const ATrayIcon: TCustomTrayIcon): Boolean;
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

class function TCDWSCustomTrayIcon.Show(const ATrayIcon: TCustomTrayIcon): Boolean;
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

  Text := UTF8ToUTF16(ATrayIcon.Hint);
  SystemTrayIcon.setToolTip(Text);

  if Assigned(ATrayIcon.PopUpMenu) then
    if TQtMenu(ATrayIcon.PopUpMenu.Handle).Widget <> nil then
      SystemTrayIcon.setContextMenu(QMenuH(TQtMenu(ATrayIcon.PopUpMenu.Handle).Widget));

  SystemTrayIcon.show;

  Result := True;
end;

{*******************************************************************
*  TCDWSCustomTrayIcon.InternalUpdate ()
*
*  DESCRIPTION:    Makes modifications to the Icon while running
*                  i.e. without hiding it and showing again
*******************************************************************}
class procedure TCDWSCustomTrayIcon.InternalUpdate(const ATrayIcon: TCustomTrayIcon);
var
  SystemTrayIcon: TQtSystemTrayIcon;
  AIcon: QIconH;
begin
  if (ATrayIcon.Handle = 0) then Exit;

  SystemTrayIcon := TQtSystemTrayIcon(ATrayIcon.Handle);
  if Assigned(ATrayIcon.Icon) then
  begin
    // normal icon
    if (ATrayIcon.Icon.HandleAllocated) then
      SystemTrayIcon.setIcon(TQtIcon(ATrayIcon.Icon.Handle).Handle)
    else
    // image list (animate)
    if (ATrayIcon.Icon.BitmapHandle <> 0) then
      SystemTrayIcon.setIcon(TQtImage(ATrayIcon.Icon.BitmapHandle).AsIcon)
    else
    begin
      AIcon := QIcon_create;
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
end;

class function TCDWSCustomTrayIcon.ShowBalloonHint(
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

class function TCDWSCustomTrayIcon.GetPosition(const ATrayIcon: TCustomTrayIcon): TPoint;
begin
  Result := Point(0, 0);
end;             *)

end.
