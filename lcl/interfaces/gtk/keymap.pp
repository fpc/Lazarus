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
   VK_CANCEL: Result:= GDK_CANCEL;
   VK_MBUTTON: Result:= 4;
   VK_BACK: Result:= GDK_BACKSPACE;
   VK_TAB: Result:= GDK_TAB;
   VK_CLEAR: Result:= GDK_CLEAR_KEY;
   VK_RETURN: Result:= GDK_RETURN;
   VK_SHIFT: Result:= GDK_SHIFT_L;
   VK_CONTROL: Result:= GDK_CONTROL_L;
   VK_MENU: Result:= GDK_MENU;
   VK_PAUSE: Result:= GDK_PAUSE;
   VK_CAPITAL: Result:= 20;
   VK_KANA: Result:= 21;
   VK_JUNJA: Result:= 23;
   VK_FINAL: Result:= 24;
   VK_HANJA: Result:= 25;
   VK_ESCAPE: Result:= GDK_ESCAPE;
   VK_CONVERT: Result:= 28;
   VK_NONCONVERT: Result:= 29;
   VK_ACCEPT: Result:= 30;
   VK_MODECHANGE: Result:= GDK_MODE_SWITCH;
   VK_SPACE: Result:= GDK_SPACE;
   VK_PRIOR: Result:= GDK_PRIOR;
   VK_NEXT: Result:= GDK_NEXT;
   VK_END: Result:= GDK_END;
   VK_HOME: Result:= GDK_HOME;
   VK_LEFT: Result:= GDK_LEFT;
   VK_UP: Result:= GDK_UP;
   VK_RIGHT: Result:= GDK_RIGHT;
   VK_DOWN: Result:= GDK_DOWN;
   VK_SELECT: Result:= GDK_SELECT;
   VK_PRINT: Result:= GDK_PRINT;
   VK_EXECUTE: Result:= GDK_EXECUTE;
   VK_SNAPSHOT: Result:= 44;
   VK_INSERT: Result:= GDK_INSERT;
   VK_DELETE: Result:= GDK_DELETE_KEY;
   VK_HELP: Result:= GDK_HELP;
   VK_0: Result:= GDK_0;
   VK_1: Result:= GDK_1;
   VK_2: Result:= GDK_2;
   VK_3: Result:= GDK_3;
   VK_4: Result:= GDK_4;
   VK_5: Result:= GDK_5;
   VK_6: Result:= GDK_6;
   VK_7: Result:= GDK_7;
   VK_8: Result:= GDK_8;
   VK_9: Result:= GDK_9;
   VK_A: Result:= GDK_CAPITAL_A;
   VK_B: Result:= GDK_CAPITAL_B;
   VK_C: Result:= GDK_CAPITAL_C;
   VK_D: Result:= GDK_CAPITAL_D;
   VK_E: Result:= GDK_CAPITAL_E;
   VK_F: Result:= GDK_CAPITAL_F;
   VK_G: Result:= GDK_CAPITAL_G;
   VK_H: Result:= GDK_CAPITAL_H;
   VK_I: Result:= GDK_CAPITAL_I;
   VK_J: Result:= GDK_CAPITAL_J;
   VK_K: Result:= GDK_CAPITAL_K;
   VK_L: Result:= GDK_CAPITAL_L;
   VK_M: Result:= GDK_CAPITAL_M;
   VK_N: Result:= GDK_CAPITAL_N;
   VK_O: Result:= GDK_CAPITAL_O;
   VK_P: Result:= GDK_CAPITAL_P;
   VK_Q: Result:= GDK_CAPITAL_Q;
   VK_R: Result:= GDK_CAPITAL_R;
   VK_S: Result:= GDK_CAPITAL_S;
   VK_T: Result:= GDK_CAPITAL_T;
   VK_U: Result:= GDK_CAPITAL_U;
   VK_V: Result:= GDK_CAPITAL_V;
   VK_W: Result:= GDK_CAPITAL_W;
   VK_X: Result:= GDK_CAPITAL_X;
   VK_Y: Result:= GDK_CAPITAL_Y;
   VK_Z: Result:= GDK_CAPITAL_Z;
 
   VK_LWIN: Result:= 91;
   VK_RWIN: Result:= 92;
   VK_APPS: Result:= 93;
   VK_NUMPAD0: Result:= GDK_KP_0;
   VK_NUMPAD1: Result:= GDK_KP_1;
   VK_NUMPAD2: Result:= GDK_KP_2;
   VK_NUMPAD3: Result:= GDK_KP_3;
   VK_NUMPAD4: Result:= GDK_KP_4;
   VK_NUMPAD5: Result:= GDK_KP_5;
   VK_NUMPAD6: Result:= GDK_KP_6;
   VK_NUMPAD7: Result:= GDK_KP_7;
   VK_NUMPAD8: Result:= GDK_KP_8;
   VK_NUMPAD9: Result:= GDK_KP_9;
   VK_MULTIPLY: Result:= GDK_KP_MULTIPLY;
   VK_ADD: Result:= GDK_KP_ADD;
   VK_SEPARATOR: Result:= GDK_KP_SEPARATOR;
   VK_SUBTRACT: Result:= GDK_KP_SUBTRACT;
   VK_DECIMAL: Result:= GDK_KP_DECIMAL;
   VK_DIVIDE: Result:= GDK_KP_DIVIDE;
   VK_F1: Result:= GDK_F1;
   VK_F2: Result:= GDK_F2;
   VK_F3: Result:= GDK_F3;
   VK_F4: Result:= GDK_F4;
   VK_F5: Result:= GDK_F5;
   VK_F6: Result:= GDK_F6;
   VK_F7: Result:= GDK_F7;
   VK_F8: Result:= GDK_F8;
   VK_F9: Result:= GDK_F9;
   VK_F10: Result:= GDK_F10;
   VK_F11: Result:= GDK_F11;
   VK_F12: Result:= GDK_F12;
   VK_F13: Result:= GDK_F13;
   VK_F14: Result:= GDK_F14;
   VK_F15: Result:= GDK_F15;
   VK_F16: Result:= GDK_F16;
   VK_F17: Result:= GDK_F17;
   VK_F18: Result:= GDK_F18;
   VK_F19: Result:= GDK_F19;
   VK_F20: Result:= GDK_F20;
   VK_F21: Result:= GDK_F21;
   VK_F22: Result:= GDK_F22;
   VK_F23: Result:= GDK_F23;
   VK_F24: Result:= GDK_F24;
   VK_NUMLOCK: Result:= GDK_NUM_LOCK;
   VK_SCROLL: Result:= GDK_SCROLL_LOCK;
   
   VK_EQUAL: Result:= GDK_KP_EQUAL;
   VK_COMMA: Result:= GDK_COMMA;
   VK_POINT: Result:= GDK_PERIOD;
   VK_SLASH: Result:= GDK_SLASH;
 
   VK_LSHIFT: Result:= GDK_SHIFT_L;
   VK_RSHIFT: Result:= GDK_SHIFT_R;
   VK_LCONTROL: Result:= GDK_CONTROL_L;
   VK_RCONTROL: Result:= GDK_CONTROL_R;
   VK_LMENU: Result:= GDK_META_L;
   VK_RMENU: Result:= GDK_META_R;
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
