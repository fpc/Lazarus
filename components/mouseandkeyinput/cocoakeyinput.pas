{ CocoaKeyInput

  Copyright (C) 2019 Sammarco Francesco

  This source is free software; you can redistribute it and/or modify it under the terms of the
  GNU General Public License as published by the Free Software Foundation; either version 2 of the
  License, or (at your option) any later version.

  This code is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without
  even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
  General Public License for more details.

  A copy of the GNU General Public License is available on the World Wide Web at
  <http://www.gnu.org/copyleft/gpl.html>. You can also obtain it by writing to the Free Software
  Foundation, Inc., 51 Franklin Street - Fifth Floor, Boston, MA 02110-1335, USA.
}
unit CocoaKeyInput;

{$mode objfpc}{$H+}

interface

uses
  MacOSAll,
  Classes, SysUtils, Controls, Forms,
  CocoaUtils, CGEventSource, CGEvent, CGEventTypes, CGRemoteOperation, CFBase,
  KeyInputIntf;

type

  { TCocoaKeyInput }

  TCocoaKeyInput = class(TKeyInput)
  protected
    procedure DoDown(Key: Word); override;
    procedure DoUp(Key: Word); override;
  end;

function InitializeKeyInput: TKeyInput;


implementation

uses
  LCLType;

function InitializeKeyInput: TKeyInput;
begin
  Result := TCocoaKeyInput.Create;
end;

function VKToMacCode(AKey: Word): Word;
begin
  case AKey of
    VK_A : Result :=  kVK_ANSI_A;
    VK_S : Result :=  kVK_ANSI_S;
    VK_D : Result :=  kVK_ANSI_D;
    VK_F : Result :=  kVK_ANSI_F;
    VK_H : Result :=  kVK_ANSI_H;
    VK_G : Result :=  kVK_ANSI_G;
    VK_Z : Result :=  kVK_ANSI_Z;
    VK_X : Result :=  kVK_ANSI_X;
    VK_C : Result :=  kVK_ANSI_C;
    VK_V : Result :=  kVK_ANSI_V;

    //kVK_ISO_Section   // ISO keyboard only

    VK_B : Result := kVK_ANSI_B;
    VK_Q : Result := kVK_ANSI_Q;
    VK_W : Result := kVK_ANSI_W;
    VK_E : Result := kVK_ANSI_E;
    VK_R : Result := kVK_ANSI_R;
    VK_Y : Result := kVK_ANSI_Y;
    VK_T : Result := kVK_ANSI_T;
    VK_1 : Result := kVK_ANSI_1;
    VK_2 : Result := kVK_ANSI_2;
    VK_3 : Result := kVK_ANSI_3;
    VK_4 : Result := kVK_ANSI_4;
    VK_6 : Result := kVK_ANSI_6;
    VK_5 : Result := kVK_ANSI_5;
    VK_LCL_EQUAL          : Result := kVK_ANSI_Equal; // aka VK_EQUAL = 187 = $BB;
    VK_9                  : Result := kVK_ANSI_9;
    VK_7                  : Result := kVK_ANSI_7;
    VK_OEM_MINUS          : Result := kVK_ANSI_Minus;
    VK_8                  : Result := kVK_ANSI_8;
    VK_0                  : Result := kVK_ANSI_0;
    VK_OEM_6              : Result := kVK_ANSI_RightBracket;
    VK_O                  : Result := kVK_ANSI_O;
    VK_U                  : Result := kVK_ANSI_U;
    VK_LCL_OPEN_BRAKET    : Result := kVK_ANSI_LeftBracket;
    VK_I                  : Result := kVK_ANSI_I;
    VK_P                  : Result := kVK_ANSI_P;

    VK_RETURN             : Result := kVK_Return;

    VK_L                  : Result := kVK_ANSI_L;
    VK_J                  : Result := kVK_ANSI_J;
    VK_LCL_QUOTE          : Result := kVK_ANSI_Quote;
    VK_K                  : Result := kVK_ANSI_K;
    VK_LCL_SEMI_COMMA     : Result := kVK_ANSI_Semicolon;
    VK_LCL_BACKSLASH      : Result := kVK_ANSI_Backslash;
    VK_LCL_COMMA          : Result := kVK_ANSI_Comma;
    VK_LCL_SLASH          : Result := kVK_ANSI_Slash;
    VK_N                  : Result := kVK_ANSI_N;
    VK_M                  : Result := kVK_ANSI_M;
    VK_LCL_POINT          : Result := kVK_ANSI_Period;

    VK_TAB                : Result := kVK_Tab;
    VK_SPACE              : Result := kVK_Space;

    VK_LCL_TILDE          : Result := kVK_ANSI_Grave;

    VK_BACK               : Result := kVK_Delete;
    VK_DELETE             : Result := kVK_Delete; //Added from Francesco Sammarco

    VK_ESCAPE             : Result := kVK_Escape;
    VK_LWIN               : Result := kVK_Command;
    // todo: Application.ExtendedKeysSupport must be true!

    VK_SHIFT             : Result := kVK_Shift;   //Added from Francesco Sammarco
    VK_MENU              : Result := kVK_Option;  //Added from Francesco Sammarco
    VK_CONTROL           : Result := kVK_Control; //Added from Francesco Sammarco


    VK_LSHIFT             : Result := kVK_Shift;
    VK_CAPITAL            : Result := kVK_CapsLock;
    VK_LMENU              : Result := kVK_Option;
    VK_LCONTROL           : Result := kVK_Control;
    VK_RSHIFT             : Result := kVK_RightShift;
    VK_RMENU              : Result := kVK_RightOption;
    VK_RCONTROL           : Result := kVK_RightControl;
    //kVK_Function          : Result := VK_; todo:
    VK_F17                : Result := kVK_F17;

    VK_DECIMAL              : Result := kVK_ANSI_KeypadDecimal;
    VK_MULTIPLY             : Result := kVK_ANSI_KeypadMultiply;
    VK_ADD                  : Result := kVK_ANSI_KeypadPlus;
    VK_NUMLOCK              : Result := kVK_ANSI_KeypadClear;

    VK_VOLUME_UP            : Result := kVK_VolumeUp;
    VK_VOLUME_DOWN          : Result := kVK_VolumeDown;
    VK_VOLUME_MUTE          : Result := kVK_Mute;

    VK_DIVIDE               : Result := kVK_ANSI_KeypadDivide;
    //VK_RETURN               : Result := kVK_ANSI_KeypadEnter;  //Commented from Francesco Sammarco (duplicate VK_RETURN)
    VK_SUBTRACT             : Result := kVK_ANSI_KeypadMinus;

    VK_F18         : Result := kVK_F18;
    VK_F19         : Result := kVK_F19;

    //kVK_ANSI_KeypadEquals : Result := VK_;
    VK_NUMPAD0      : Result := kVK_ANSI_Keypad0;
    VK_NUMPAD1      : Result := kVK_ANSI_Keypad1;
    VK_NUMPAD2      : Result := kVK_ANSI_Keypad2;
    VK_NUMPAD3      : Result := kVK_ANSI_Keypad3;
    VK_NUMPAD4      : Result := kVK_ANSI_Keypad4;
    VK_NUMPAD5      : Result := kVK_ANSI_Keypad5;
    VK_NUMPAD6      : Result := kVK_ANSI_Keypad6;
    VK_NUMPAD7      : Result := kVK_ANSI_Keypad7;

    VK_F20  : Result := kVK_F20;

    VK_NUMPAD8 : Result := kVK_ANSI_Keypad8;
    VK_NUMPAD9 : Result := kVK_ANSI_Keypad9;

    //kVK_JIS_Yen                   = $5D;
    //kVK_JIS_Underscore            = $5E;
    //kVK_JIS_KeypadComma           = $5F;

    VK_F5           : Result := kVK_F5;
    VK_F6           : Result := kVK_F6;
    VK_F7           : Result := kVK_F7;
    VK_F3           : Result := kVK_F3;
    VK_F8           : Result := kVK_F8;
    VK_F9           : Result := kVK_F9;

    //kVK_JIS_Eisu                  = $66;
    VK_KANA         : Result := kVK_JIS_Kana;

    VK_F11          : Result := kVK_F11;

    VK_SNAPSHOT           : Result := kVK_F13;
    VK_F16                : Result := kVK_F16;
    VK_SCROLL             : Result := kVK_F14;
    VK_F10                : Result := kVK_F10;

    VK_APPS               : Result := kVK_SubMenu;

    VK_F12                : Result := kVK_F12;
    VK_PAUSE              : Result := kVK_F15;
    VK_HELP               : Result := kVK_Help; //VK_INSERT; // todo!
    VK_HOME               : Result := kVK_Home;
    VK_PRIOR              : Result := kVK_PageUp;
    //VK_DELETE             : Result := kVK_ForwardDelete; // Commented from Francesco Sammarco
    VK_F4                 : Result := kVK_F4;
    VK_END                : Result := kVK_End;
    VK_F2                 : Result := kVK_F2;
    VK_NEXT               : Result := kVK_PageDown;
    VK_F1                 : Result := kVK_F1;
    VK_LEFT               : Result := kVK_LeftArrow;
    VK_RIGHT              : Result := kVK_RightArrow;
    VK_DOWN               : Result := kVK_DownArrow;
    VK_UP                 : Result := kVK_UpArrow;

  else
    Result := VK_UNKNOWN;
  end;
end;

procedure SendKeyInput(Key: Word; Down: Boolean);
var
  EventSourceRef  : CGEventSourceRef;
  KeyDownRef, KeyUpRef: CGEventRef;
begin

  EventSourceRef := CGEventSourceCreate(kCGEventSourceStateHIDSystemState);
  if EventSourceRef = nil then RaiseLastOSError;
  try

    if Down then
    begin
      KeyDownRef := CGEventCreateKeyboardEvent(EventSourceRef, VKToMacCode(Key), 1); //1 = key down
      if KeyDownRef = nil then RaiseLastOSError;
      CGEventSetFlags(KeyDownRef, CGEventGetFlags(KeyDownRef)); //combine flags
      CGEventPost(kCGHIDEventTap, KeyDownRef);
    end else begin
      KeyUpRef := CGEventCreateKeyboardEvent(EventSourceRef, VKToMacCode(Key), 0);  //0 = key up
      if KeyUpRef = nil then RaiseLastOSError;
      CGEventSetFlags(KeyDownRef, CGEventGetFlags(KeyDownRef)); //combine flags
      CGEventPost(kCGHIDEventTap, KeyUpRef);
    end;

  finally
    if Down then
       if KeyDownRef <> nil then CFRelease(KeyDownRef)
    else
        if KeyUpRef <> nil then CFRelease(KeyUpRef);
    CFRelease(EventSourceRef);
  end;

end;

{ TCocoaKeyInput }
procedure TCocoaKeyInput.DoDown(Key: Word);
begin
  SendKeyInput(Key, True);
end;

procedure TCocoaKeyInput.DoUp(Key: Word);
begin
  SendKeyInput(Key, False);
end;

end.

