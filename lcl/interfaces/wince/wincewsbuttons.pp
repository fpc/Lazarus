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
 *  See the file COPYING.modifiedLGPL, included in this distribution,        *
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
  SysUtils, Controls, LCLType, Forms, InterfaceBase, Buttons, LMessages,WinCEProc, WinCEWSControls,
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
  Params: TCreateWindowExParams;
  str : array[0..255] of WideChar;
begin
  {$ifdef VerboseWinCE}
  WriteLn('TWinCEWSButton.CreateHandle');
  {$endif}

  // general initialization of Params
  PrepareCreateWindow(AWinControl, Params);
  
  // customization of Params
  with Params do
  begin
   // if TCustomButton(AWinControl).Default Then
   //   Flags := Flags or BS_DEFPUSHBUTTON
   // else
   //   Flags := Flags or BS_PUSHBUTTON;
    Flags := WS_CHILD or WS_VISIBLE;
    pClassName := @ButtonClsName;
    WindowTitle := CreatePWideCharFromString(StrCaption);
    Left := AWinControl.Left;
    Top := AWinControl.Top;
    Width := AWinControl.Width;
    Height := AWinControl.Height;
    Parent := AWinControl.Parent.Handle;
    MenuHandle := 0;
  end;
  
  // create window
  FinishCreateWindow(AWinControl, Params, false);
  Result := Params.Window;
  
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
var
tmpStr : PWideChar;
begin
  tmpstr := SysAllocStringLen(nil,256);
  Result := Boolean(Windows.GetWindowText(AWinControl.Handle,tmpStr,256));
  AText := String(tmpStr);
  SysFreeString(tmpStr);
end;

{------------------------------------------------------------------------------
  Function: TWinCEWSButton.SetText
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
class procedure TWinCEWSButton.SetText(const AWinControl: TWinControl; const AText: String);
var
tmpStr : PWideChar;
begin
 tmpstr := CreatePWideCharFromString(AText);
 Windows.SetWindowText(AWinControl.Handle,tmpStr);
 DisposePWideChar(tmpStr);
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