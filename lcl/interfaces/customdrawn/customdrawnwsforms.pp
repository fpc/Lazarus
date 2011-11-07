{
 *****************************************************************************
 *                               QtWSForms.pp                                * 
 *                               ------------                                * 
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
unit CustomDrawnWSForms;

{$mode objfpc}{$H+}

{$I customdrawndefines.inc}

interface

//{$I qtdefines.inc}

uses
  // RTL
  SysUtils, Classes,
  {$ifdef CD_Windows}Windows, WinProc,{$endif}
  {$ifdef CD_Cocoa}MacOSAll, CocoaAll, CocoaPrivate, CocoaUtils,{$endif}
  // LCL
  Controls, LCLType, Forms, LCLProc,
  // Widgetset
  InterfaceBase, WSForms, WSProc, WSLCLClasses, LCLMessageGlue,
  customdrawnwscontrols, customdrawnint;

type
  { TCDWSScrollingWinControl }

  TCDWSScrollingWinControl = class(TWSScrollingWinControl)
  published
  end;

  { TCDWSScrollBox }

  TCDWSScrollBox = class(TWSScrollBox)
  published
//    class procedure ScrollBy(const AWinControl: TScrollingWinControl;
//      const DeltaX, DeltaY: integer); override;
  end;

  { TCDWSCustomFrame }

  TCDWSCustomFrame = class(TWSCustomFrame)
  published
//    class procedure ScrollBy(const AWinControl: TScrollingWinControl;
//      const DeltaX, DeltaY: integer); override;
  end;

  { TCDWSFrame }

  TCDWSFrame = class(TWSFrame)
  published
  end;

  { TCDWSCustomForm }

  TCDWSCustomForm = class(TWSCustomForm)
  private
    {$ifdef CD_Windows}
    class function CalcBorderIconsFlags(const AForm: TCustomForm): dword;
    class procedure CalcFormWindowFlags(const AForm: TCustomForm;
      var Flags, FlagsEx: dword);
    class procedure CalculateDialogPosition(var Params: TCreateWindowExParams;
     Bounds: TRect; lForm: TCustomForm);
    {$endif}
  published
    class function  CreateHandle(const AWinControl: TWinControl; const AParams: TCreateParams): TLCLIntfHandle; override;
//    class procedure DestroyHandle(const AWinControl: TWinControl); override;

    class procedure SetBounds(const AWinControl: TWinControl;
      const ALeft, ATop, AWidth, AHeight: Integer); override;
    {$ifdef CD_Windows}
    class procedure SetBorderIcons(const AForm: TCustomForm;
                                   const ABorderIcons: TBorderIcons); override;
    class procedure SetFormBorderStyle(const AForm: TCustomForm;
                             const AFormBorderStyle: TFormBorderStyle); override;
    class procedure SetIcon(const AForm: TCustomForm; const Small, Big: HICON); override;
    class procedure SetShowInTaskbar(const AForm: TCustomForm; const AValue: TShowInTaskbar); override;
    class procedure ShowModal(const ACustomForm: TCustomForm); override;
    {$endif}
    class procedure ShowHide(const AWinControl: TWinControl); override;

    {$ifdef CD_Cocoa}
    class function  GetText(const AWinControl: TWinControl; var AText: String): Boolean; override;
    class function  GetTextLen(const AWinControl: TWinControl; var ALength: Integer): Boolean; override;
    class procedure SetText(const AWinControl: TWinControl; const AText: String); override;
    class function  GetClientBounds(const AWincontrol: TWinControl; var ARect: TRect): Boolean; override;
    class function  GetClientRect(const AWincontrol: TWinControl; var ARect: TRect): Boolean; override;
    {$endif}
{    class function  CanFocus(const AWinControl: TWinControl): Boolean; override;

    class procedure CloseModal(const ACustomForm: TCustomForm); override;
    class procedure SetAllowDropFiles(const AForm: TCustomForm; AValue: Boolean); override;
    class procedure SetFormBorderStyle(const AForm: TCustomForm; const AFormBorderStyle: TFormBorderStyle); override;
    class procedure SetFormStyle(const AForm: TCustomform; const AFormStyle, AOldFormStyle: TFormStyle); override;
    class procedure SetIcon(const AForm: TCustomForm; const Small, Big: HICON); override;
    class procedure SetPopupParent(const ACustomForm: TCustomForm;
       const APopupMode: TPopupMode; const APopupParent: TCustomForm); override;
    class procedure SetShowInTaskbar(const AForm: TCustomForm; const AValue: TShowInTaskbar); override;
    class procedure ShowHide(const AWinControl: TWinControl); override; //TODO: rename to SetVisible(control, visible)
    class procedure ShowModal(const ACustomForm: TCustomForm); override;
    class procedure SetBorderIcons(const AForm: TCustomForm; const ABorderIcons: TBorderIcons); override;
    class procedure SetAlphaBlend(const ACustomForm: TCustomForm;
       const AlphaBlend: Boolean; const Alpha: Byte); override;}
  end;

  { TCDWSForm }

  TCDWSForm = class(TWSForm)
  published
  end;

  { TCDWSHintWindow }

  TCDWSHintWindow = class(TWSHintWindow)
  published
//    class function CreateHandle(const AWinControl: TWinControl; const AParams: TCreateParams): TLCLIntfHandle; override;
  end;

  { TCDWSScreen }

  TCDWSScreen = class(TWSScreen)
  published
  end;

  { TCDWSApplicationProperties }

  TCDWSApplicationProperties = class(TWSApplicationProperties)
  published
  end;


implementation

{$ifdef CD_Windows}
  {$include customdrawnwsforms_win.inc}
{$endif}
{$ifdef CD_Cocoa}
  {$include customdrawnwsforms_cocoa.inc}
{$endif}

end.
