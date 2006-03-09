{ $Id: WinCEwsbuttons.pp 8815 2006-02-24 13:31:16Z mattias $}
{
 *****************************************************************************
 *                              WinCEWSButtons.pp                               *
 *                              --------------                               * 
 *                                                                           *
 *                                                                           *
 *****************************************************************************

 *****************************************************************************
 *                                                                           *
 *  This file is part of the Lazarus Component Library (LCL)                 *
 *                                                                           *
 *  See the file COPYING.LCL, included in this distribution,                 *
 *  for details about the copyright.                                         *
 *                                                                           *
 *  This program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
 *                                                                           *
 *****************************************************************************
}
unit WinCEWSButtons;

{$mode delphi}{$H+}

interface

uses
  // Libs
  Windows,
  // LCL
  SysUtils, Controls, LCLType, Forms, InterfaceBase, Buttons, LMessages,
  // Widgetset
  WSButtons, WSLCLClasses;

type

  { TWinCEWSButton }

  TWinCEWSButton = class(TWSButton)
  private
  protected
  public
    class function  CreateHandle(const AWinControl: TWinControl; const AParams: TCreateParams): TLCLIntfHandle; override;
    class procedure DestroyHandle(const AWinControl: TWinControl); override;
//    class procedure ActiveDefaultButtonChanged(const AButton: TCustomButton); override;
    class function  GetText(const AWinControl: TWinControl; var AText: String): Boolean; override;
//    class procedure SetShortcut(const AButton: TCustomButton; const OldShortcut, NewShortcut: TShortcut); override;
    class procedure SetText(const AWinControl: TWinControl; const AText: String); override;
//    class procedure GetPreferredSize(const AWinControl: TWinControl;
//                        var PreferredWidth, PreferredHeight: integer); override;
  end;

  { TWinCEWSBitBtn }

  TWinCEWSBitBtn = class(TWSBitBtn)
  private
  protected
  public
  end;

  { TWinCEWSSpeedButton }

  TWinCEWSSpeedButton = class(TWSSpeedButton)
  private
  protected
  public
  end;


implementation

uses WinCEInt;

{ TWinCEWSButton }

{------------------------------------------------------------------------------
  Function: TWinCEWSButton.CreateHandle
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
class function TWinCEWSButton.CreateHandle(const AWinControl: TWinControl;
  const AParams: TCreateParams): TLCLIntfHandle;
var
  Str: array[0..255] of WideChar;
begin
  {$ifdef VerboseWinCE}
  WriteLn('TWinCEWSButton.CreateHandle');
  {$endif}

  MultiByteToWideChar(CP_ACP, 0, PChar(AWinControl.Caption), -1, @Str, 256);

  Result := CreateWindow(
    @ButtonClsName,             // Name of the registered class
    @Str,                       // Title of the window
    WS_CHILD or WS_VISIBLE,     // Style of the window
    AWinControl.Left,           // x-position (at beginning)
    AWinControl.Top,            // y-position (at beginning)
    AWinControl.Width,          // window width
    AWinControl.Height,         // window height
    AWinControl.Parent.Handle,  // handle to parent or owner window
    0,                          // handle to menu
    System.hInstance,           // handle to application instance
    nil);                       // pointer to window-creation data

//  if (Result = 0) then WriteLn('Create Button failed');
  {$ifdef VerboseWinCE}
  WriteLn('End Create Button. Handle = ' + IntToStr(Result) +
   ' Left ' + IntToStr(AWinControl.Left) +
   ' Top ' + IntToStr(AWinControl.Top) +
   ' Width ' + IntToStr(AWinControl.Width) +
   ' Height ' + IntToStr(AWinControl.Height) +
   ' ParentHandle ' + IntToStr(AWinControl.Parent.Handle));
  {$endif}
end;

{------------------------------------------------------------------------------
  Function: TWinCEWSButton.DestroyHandle
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
class procedure TWinCEWSButton.DestroyHandle(const AWinControl: TWinControl);
begin
end;

{------------------------------------------------------------------------------
  Function: TWinCEWSButton.GetText
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
class function TWinCEWSButton.GetText(const AWinControl: TWinControl; var AText: String): Boolean;
begin
  Result := True;
end;

{------------------------------------------------------------------------------
  Function: TWinCEWSButton.SetText
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
class procedure TWinCEWSButton.SetText(const AWinControl: TWinControl; const AText: String);
begin
end;

initialization

////////////////////////////////////////////////////
// I M P O R T A N T
////////////////////////////////////////////////////
// To improve speed, register only classes
// which actually implement something
////////////////////////////////////////////////////
  RegisterWSComponent(TCustomButton, TWinCEWSButton);
//  RegisterWSComponent(TCustomBitBtn, TWinCEWSBitBtn);
//  RegisterWSComponent(TCustomSpeedButton, TWinCEWSSpeedButton);
////////////////////////////////////////////////////
end.
