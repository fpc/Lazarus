{
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
unit KeyMap;

{$mode objfpc}{$H+}

interface

uses
  LCLType, {$IFDEF gtk2}gdk2{$ELSE}gdk{$ENDIF};

function VK2GDK(const Key : word) : word;
function GDK2VK(const Key : word) : word;


implementation


function VK2GDK(const Key : word) : word;
begin
  case Key of
   VK_UNKNOWN: Result:= 0;
   VK_LBUTTON: Result:= 1;
   VK_RBUTTON: Result:= 2;
   VK_CANCEL: Result:= {$ifdef GTK2}GDK_KEY_CANCEL{$else}GDK_CANCEL{$EndIf};
   VK_MBUTTON: Result:= 4;
   VK_BACK: Result:= {$ifdef GTK2}GDK_KEY_BACKSPACE{$else}GDK_BACKSPACE{$EndIf};
   VK_TAB: Result:= {$ifdef GTK2}GDK_KEY_TAB{$else}GDK_TAB{$EndIf};
   VK_CLEAR: Result:= {$ifdef GTK2}GDK_KEY_CLEAR{$else}GDK_CLEAR{$EndIf};
   VK_RETURN: Result:= {$ifdef GTK2}GDK_KEY_RETURN{$else}GDK_RETURN{$EndIf};
   VK_SHIFT: Result:= {$ifdef GTK2}GDK_KEY_SHIFT_L{$else}GDK_SHIFT_L{$EndIf};
   VK_CONTROL: Result:= {$ifdef GTK2}GDK_KEY_CONTROL_L{$else}GDK_CONTROL_L{$EndIf};
   VK_MENU: Result:= {$ifdef GTK2}GDK_KEY_MENU{$else}GDK_MENU{$EndIf};
   VK_PAUSE: Result:= {$ifdef GTK2}GDK_KEY_PAUSE{$else}GDK_PAUSE{$EndIf};
   VK_CAPITAL: Result:= 20;
   VK_KANA: Result:= 21;
   VK_JUNJA: Result:= 23;
   VK_FINAL: Result:= 24;
   VK_HANJA: Result:= 25;
   VK_ESCAPE: Result:= {$ifdef GTK2}GDK_KEY_ESCAPE{$else}GDK_ESCAPE{$EndIf};
   VK_CONVERT: Result:= 28;
   VK_NONCONVERT: Result:= 29;
   VK_ACCEPT: Result:= 30;
   VK_MODECHANGE: Result:= {$ifdef GTK2}GDK_KEY_MODE_SWITCH{$else}GDK_MODE_SWITCH{$EndIf};
   VK_SPACE: Result:= {$ifdef GTK2}GDK_KEY_SPACE{$else}GDK_SPACE{$EndIf};
   VK_PRIOR: Result:= {$ifdef GTK2}GDK_KEY_PRIOR{$else}GDK_PRIOR{$EndIf};
   VK_NEXT: Result:= {$ifdef GTK2}GDK_KEY_NEXT{$else}GDK_NEXT{$EndIf};
   VK_END: Result:= {$ifdef GTK2}GDK_KEY_END{$else}GDK_END{$EndIf};
   VK_HOME: Result:= {$ifdef GTK2}GDK_KEY_HOME{$else}GDK_HOME{$EndIf};
   VK_LEFT: Result:= {$ifdef GTK2}GDK_KEY_LEFT{$else}GDK_LEFT{$EndIf};
   VK_UP: Result:= {$ifdef GTK2}GDK_KEY_UP{$else}GDK_UP{$EndIf};
   VK_RIGHT: Result:= {$ifdef GTK2}GDK_KEY_RIGHT{$else}GDK_RIGHT{$EndIf};
   VK_DOWN: Result:= {$ifdef GTK2}GDK_KEY_DOWN{$else}GDK_DOWN{$EndIf};
   VK_SELECT: Result:= {$ifdef GTK2}GDK_KEY_SELECT{$else}GDK_SELECT{$EndIf};
   VK_PRINT: Result:= {$ifdef GTK2}GDK_KEY_PRINT{$else}GDK_PRINT{$EndIf};
   VK_EXECUTE: Result:= {$ifdef GTK2}GDK_KEY_EXECUTE{$else}GDK_EXECUTE{$EndIf};
   VK_SNAPSHOT: Result:= 44;
   VK_INSERT: Result:= {$ifdef GTK2}GDK_KEY_INSERT{$else}GDK_INSERT{$EndIf};
   VK_DELETE: Result:= {$ifdef GTK2}GDK_KEY_DELETE{$else}GDK_DELETE{$EndIf};
   VK_HELP: Result:= {$ifdef GTK2}GDK_KEY_HELP{$else}GDK_HELP{$EndIf};
   VK_0: Result:= {$ifdef GTK2}GDK_KEY_0{$else}GDK_0{$EndIf};
   VK_1: Result:= {$ifdef GTK2}GDK_KEY_1{$else}GDK_1{$EndIf};
   VK_2: Result:= {$ifdef GTK2}GDK_KEY_2{$else}GDK_2{$EndIf};
   VK_3: Result:= {$ifdef GTK2}GDK_KEY_3{$else}GDK_3{$EndIf};
   VK_4: Result:= {$ifdef GTK2}GDK_KEY_4{$else}GDK_4{$EndIf};
   VK_5: Result:= {$ifdef GTK2}GDK_KEY_5{$else}GDK_5{$EndIf};
   VK_6: Result:= {$ifdef GTK2}GDK_KEY_6{$else}GDK_6{$EndIf};
   VK_7: Result:= {$ifdef GTK2}GDK_KEY_7{$else}GDK_7{$EndIf};
   VK_8: Result:= {$ifdef GTK2}GDK_KEY_8{$else}GDK_8{$EndIf};
   VK_9: Result:= {$ifdef GTK2}GDK_KEY_9{$else}GDK_9{$EndIf};
   VK_A: Result:= {$ifdef GTK2}GDK_KEY_CAPITAL_A{$else}GDK_CAPITAL_A{$EndIf};
   VK_B: Result:= {$ifdef GTK2}GDK_KEY_CAPITAL_B{$else}GDK_CAPITAL_B{$EndIf};
   VK_C: Result:= {$ifdef GTK2}GDK_KEY_CAPITAL_C{$else}GDK_CAPITAL_C{$EndIf};
   VK_D: Result:= {$ifdef GTK2}GDK_KEY_CAPITAL_D{$else}GDK_CAPITAL_D{$EndIf};
   VK_E: Result:= {$ifdef GTK2}GDK_KEY_CAPITAL_E{$else}GDK_CAPITAL_E{$EndIf};
   VK_F: Result:= {$ifdef GTK2}GDK_KEY_CAPITAL_F{$else}GDK_CAPITAL_F{$EndIf};
   VK_G: Result:= {$ifdef GTK2}GDK_KEY_CAPITAL_F{$else}GDK_CAPITAL_G{$EndIf};
   VK_H: Result:= {$ifdef GTK2}GDK_KEY_CAPITAL_H{$else}GDK_CAPITAL_H{$EndIf};
   VK_I: Result:= {$ifdef GTK2}GDK_KEY_CAPITAL_I{$else}GDK_CAPITAL_I{$EndIf};
   VK_J: Result:= {$ifdef GTK2}GDK_KEY_CAPITAL_J{$else}GDK_CAPITAL_J{$EndIf};
   VK_K: Result:= {$ifdef GTK2}GDK_KEY_CAPITAL_K{$else}GDK_CAPITAL_K{$EndIf};
   VK_L: Result:= {$ifdef GTK2}GDK_KEY_CAPITAL_L{$else}GDK_CAPITAL_L{$EndIf};
   VK_M: Result:= {$ifdef GTK2}GDK_KEY_CAPITAL_M{$else}GDK_CAPITAL_M{$EndIf};
   VK_N: Result:= {$ifdef GTK2}GDK_KEY_CAPITAL_N{$else}GDK_CAPITAL_N{$EndIf};
   VK_O: Result:= {$ifdef GTK2}GDK_KEY_CAPITAL_O{$else}GDK_CAPITAL_O{$EndIf};
   VK_P: Result:= {$ifdef GTK2}GDK_KEY_CAPITAL_P{$else}GDK_CAPITAL_P{$EndIf};
   VK_Q: Result:= {$ifdef GTK2}GDK_KEY_CAPITAL_Q{$else}GDK_CAPITAL_Q{$EndIf};
   VK_R: Result:= {$ifdef GTK2}GDK_KEY_CAPITAL_R{$else}GDK_CAPITAL_R{$EndIf};
   VK_S: Result:= {$ifdef GTK2}GDK_KEY_CAPITAL_S{$else}GDK_CAPITAL_S{$EndIf};
   VK_T: Result:= {$ifdef GTK2}GDK_KEY_CAPITAL_T{$else}GDK_CAPITAL_T{$EndIf};
   VK_U: Result:= {$ifdef GTK2}GDK_KEY_CAPITAL_U{$else}GDK_CAPITAL_U{$EndIf};
   VK_V: Result:= {$ifdef GTK2}GDK_KEY_CAPITAL_V{$else}GDK_CAPITAL_V{$EndIf};
   VK_W: Result:= {$ifdef GTK2}GDK_KEY_CAPITAL_W{$else}GDK_CAPITAL_W{$EndIf};
   VK_X: Result:= {$ifdef GTK2}GDK_KEY_CAPITAL_X{$else}GDK_CAPITAL_X{$EndIf};
   VK_Y: Result:= {$ifdef GTK2}GDK_KEY_CAPITAL_Y{$else}GDK_CAPITAL_Y{$EndIf};
   VK_Z: Result:= {$ifdef GTK2}GDK_KEY_CAPITAL_Z{$else}GDK_CAPITAL_Z{$EndIf};
 
   VK_LWIN: Result:= 91;
   VK_RWIN: Result:= 92;
   VK_APPS: Result:= 93;
   VK_NUMPAD0: Result:= {$ifdef GTK2}GDK_KEY_KP_0{$else}GDK_KP_0{$EndIf};
   VK_NUMPAD1: Result:= {$ifdef GTK2}GDK_KEY_KP_1{$else}GDK_KP_1{$EndIf};
   VK_NUMPAD2: Result:= {$ifdef GTK2}GDK_KEY_KP_2{$else}GDK_KP_2{$EndIf};
   VK_NUMPAD3: Result:= {$ifdef GTK2}GDK_KEY_KP_3{$else}GDK_KP_3{$EndIf};
   VK_NUMPAD4: Result:= {$ifdef GTK2}GDK_KEY_KP_4{$else}GDK_KP_4{$EndIf};
   VK_NUMPAD5: Result:= {$ifdef GTK2}GDK_KEY_KP_5{$else}GDK_KP_5{$EndIf};
   VK_NUMPAD6: Result:= {$ifdef GTK2}GDK_KEY_KP_6{$else}GDK_KP_6{$EndIf};
   VK_NUMPAD7: Result:= {$ifdef GTK2}GDK_KEY_KP_7{$else}GDK_KP_7{$EndIf};
   VK_NUMPAD8: Result:= {$ifdef GTK2}GDK_KEY_KP_8{$else}GDK_KP_8{$EndIf};
   VK_NUMPAD9: Result:= {$ifdef GTK2}GDK_KEY_KP_9{$else}GDK_KP_9{$EndIf};
   VK_MULTIPLY: Result:= {$ifdef GTK2}GDK_KEY_KP_MULTIPLY{$else}GDK_KP_MULTIPLY{$EndIf};
   VK_ADD: Result:= {$ifdef GTK2}GDK_KEY_KP_ADD{$else}GDK_KP_ADD{$EndIf};
   VK_SEPARATOR: Result:= {$ifdef GTK2}GDK_KEY_KP_SEPARATOR{$else}GDK_KP_SEPARATOR{$EndIf};
   VK_SUBTRACT: Result:= {$ifdef GTK2}GDK_KEY_KP_SUBTRACT{$else}GDK_KP_SUBTRACT{$EndIf};
   VK_DECIMAL: Result:= {$ifdef GTK2}GDK_KEY_KP_DECIMAL{$else}GDK_KP_DECIMAL{$EndIf};
   VK_DIVIDE: Result:= {$ifdef GTK2}GDK_KEY_KP_DIVIDE{$else}GDK_KP_DIVIDE{$EndIf};
   VK_F1: Result:= {$ifdef GTK2}GDK_KEY_F1{$else}GDK_F1{$EndIf};
   VK_F2: Result:= {$ifdef GTK2}GDK_KEY_F2{$else}GDK_F2{$EndIf};
   VK_F3: Result:= {$ifdef GTK2}GDK_KEY_F3{$else}GDK_F3{$EndIf};
   VK_F4: Result:= {$ifdef GTK2}GDK_KEY_F4{$else}GDK_F4{$EndIf};
   VK_F5: Result:= {$ifdef GTK2}GDK_KEY_F5{$else}GDK_F5{$EndIf};
   VK_F6: Result:= {$ifdef GTK2}GDK_KEY_F6{$else}GDK_F6{$EndIf};
   VK_F7: Result:= {$ifdef GTK2}GDK_KEY_F7{$else}GDK_F7{$EndIf};
   VK_F8: Result:= {$ifdef GTK2}GDK_KEY_F8{$else}GDK_F8{$EndIf};
   VK_F9: Result:= {$ifdef GTK2}GDK_KEY_F9{$else}GDK_F9{$EndIf};
   VK_F10: Result:= {$ifdef GTK2}GDK_KEY_F10{$else}GDK_F10{$EndIf};
   VK_F11: Result:= {$ifdef GTK2}GDK_KEY_F11{$else}GDK_F11{$EndIf};
   VK_F12: Result:= {$ifdef GTK2}GDK_KEY_F12{$else}GDK_F12{$EndIf};
   VK_F13: Result:= {$ifdef GTK2}GDK_KEY_F13{$else}GDK_F13{$EndIf};
   VK_F14: Result:= {$ifdef GTK2}GDK_KEY_F14{$else}GDK_F14{$EndIf};
   VK_F15: Result:= {$ifdef GTK2}GDK_KEY_F15{$else}GDK_F15{$EndIf};
   VK_F16: Result:= {$ifdef GTK2}GDK_KEY_F16{$else}GDK_F16{$EndIf};
   VK_F17: Result:= {$ifdef GTK2}GDK_KEY_F17{$else}GDK_F17{$EndIf};
   VK_F18: Result:= {$ifdef GTK2}GDK_KEY_F18{$else}GDK_F18{$EndIf};
   VK_F19: Result:= {$ifdef GTK2}GDK_KEY_F19{$else}GDK_F19{$EndIf};
   VK_F20: Result:= {$ifdef GTK2}GDK_KEY_F20{$else}GDK_F20{$EndIf};
   VK_F21: Result:= {$ifdef GTK2}GDK_KEY_F21{$else}GDK_F21{$EndIf};
   VK_F22: Result:= {$ifdef GTK2}GDK_KEY_F22{$else}GDK_F22{$EndIf};
   VK_F23: Result:= {$ifdef GTK2}GDK_KEY_F23{$else}GDK_F23{$EndIf};
   VK_F24: Result:= {$ifdef GTK2}GDK_KEY_F24{$else}GDK_F24{$EndIf};
   VK_NUMLOCK: Result:= {$ifdef GTK2}GDK_KEY_NUM_LOCK{$else}GDK_NUM_LOCK{$EndIf};
   VK_SCROLL: Result:= {$ifdef GTK2}GDK_KEY_SCROLL_LOCK{$else}GDK_SCROLL_LOCK{$EndIf};
   
   VK_EQUAL: Result:= {$ifdef GTK2}GDK_KEY_KP_EQUAL{$else}GDK_KP_EQUAL{$EndIf};
   VK_COMMA: Result:= {$ifdef GTK2}GDK_KEY_COMMA{$else}GDK_COMMA{$EndIf};
   VK_POINT: Result:= {$ifdef GTK2}GDK_KEY_PERIOD{$else}GDK_PERIOD{$EndIf};
   VK_SLASH: Result:= {$ifdef GTK2}GDK_KEY_SLASH{$else}GDK_SLASH{$EndIf};
 
   VK_LSHIFT: Result:= {$ifdef GTK2}GDK_KEY_SHIFT_L{$else}GDK_SHIFT_L{$EndIf};
   VK_RSHIFT: Result:= {$ifdef GTK2}GDK_KEY_SHIFT_R{$else}GDK_SHIFT_R{$EndIf};
   VK_LCONTROL: Result:= {$ifdef GTK2}GDK_KEY_CONTROL_L{$else}GDK_CONTROL_L{$EndIf};
   VK_RCONTROL: Result:= {$ifdef GTK2}GDK_KEY_CONTROL_R{$else}GDK_CONTROL_R{$EndIf};
   VK_LMENU: Result:= {$ifdef GTK2}GDK_KEY_META_L{$else}GDK_META_L{$EndIf};
   VK_RMENU: Result:= {$ifdef GTK2}GDK_KEY_META_R{$else}GDK_META_R{$EndIf};
   VK_PROCESSKEY: Result:= 229;
   VK_ATTN: Result:= 246;
   VK_CRSEL: Result:= 247;
   VK_EXSEL: Result:= 248;
   VK_EREOF: Result:= 249;
   VK_PLAY: Result:= 250;
   VK_ZOOM: Result:= 251;
   VK_NONAME: Result:= 252;
   VK_PA1: Result:= 253;
   VK_OEM_CLEAR: Result:= 254;
   else Result:= Key;
 end;  
end;

function GDK2VK(const Key : word) : word;
begin
  Result:= Key;
end;

end.
