{
 *****************************************************************************
 *                          CustomDrawnWSExtCtrls.pas                        *
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

{$I customdrawndefines.inc}

interface

//{$I qtdefines.inc}

uses
  // RTL
  Types,
//  {$ifdef CD_Windows}Windows, customdrawn_WinProc,{$endif}
  {$ifdef CD_Cocoa}MacOSAll, CocoaAll, CocoaPrivate, CocoaGDIObjects,{$endif}
//  {$ifdef CD_X11}X, XLib, XUtil, BaseUnix, customdrawn_x11proc,{$ifdef CD_UseNativeText}xft, fontconfig,{$endif}{$endif}
//  {$ifdef CD_Android}customdrawn_androidproc, jni, bitmap, log, keycodes,{$endif}
  // LCL
  LCLProc,
  SysUtils, Classes, Controls, Graphics, Forms, ExtCtrls, LCLType,
  ImgList, InterfaceBase,
  // Widgetset
  WSExtCtrls, WSProc, WSLCLClasses,
  customdrawncontrols, customdrawnwscontrols, customdrawnproc;

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
  // TPanel draws itself, so there is no need to inject a sub-control
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
    class function Hide(const ATrayIcon: TCustomTrayIcon): Boolean; override;
    class function Show(const ATrayIcon: TCustomTrayIcon): Boolean; override;
    class procedure InternalUpdate(const ATrayIcon: TCustomTrayIcon); override;
    class function ShowBalloonHint(const ATrayIcon: TCustomTrayIcon): Boolean; override;
    class function GetPosition(const ATrayIcon: TCustomTrayIcon): TPoint; override;
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

{------------------------------------------------------------------------------
  Method: TCDWSCustomPanel.CreateHandle
  Params:  None
  Returns: Nothing

  Allocates memory and resources for the control and shows it
 ------------------------------------------------------------------------------}
class function TCDWSCustomPanel.CreateHandle(const AWinControl: TWinControl;
  const AParams: TCreateParams): TLCLIntfHandle;
var
  lCDWinControl: TCDWinControl;
begin
  Result := TCDWSWinControl.CreateHandle(AWinControl, AParams);
  lCDWinControl := TCDWinControl(Result);
end;

{ TCDWSCustomTrayIcon }

{$ifdef CD_Windows}
  {$I customdrawntrayicon_win.inc}
{$endif}
{$ifdef CD_Cocoa}
  {$I customdrawntrayicon_cocoa.inc}
{$endif}
{$ifdef CD_X11}
  {$I customdrawntrayicon_x11.inc}
{$endif}
{$ifdef CD_Android}
  {$I customdrawntrayicon_android.inc}
{$endif}

end.
