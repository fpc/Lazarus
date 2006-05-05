{
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
unit qt;

interface

{$linklib lzqt}
{$linklib c}

const
WIDGET = 1;
WIDGET_BUTTON = 2;
WIDGET_PUSH_BUTTON = 3;
WIDGET_CHECK_BOX = 4;
WIDGET_RADIO_BUTTON = 5;
WIDGET_DIALOG = 6;
WIDGET_DIALOG_FILE = 7;
WIDGET_MESSAGE_BOX = 8;
WIDGET_TAB_DIALOG = 9;
WIDGET_FRAME = 10;
WIDGET_GROUP_BOX = 11;
WIDGET_BUTTON_GROUP = 12;
WIDGET_LCD_NUMBER = 13;
WIDGET_LABEL = 14;
WIDGET_MENU_BAR = 15;
WIDGET_LISTBOX = 16;
WIDGET_MULTI_LINE_EDIT = 17;
WIDGET_POPUP_MENU = 18;
WIDGET_LINE_EDIT = 19;
WIDGET_SCROLL_BAR = 20;
WIDGET_TAB_BAR = 21;
WIDGET_WINDOW = 22;

{
 //event callback definitions
 MousePressedEvent(qwid,button,x,y,state)
}


procedure InitializeEngine;cdecl;external;
function  CreateWidget(wtype: longint):longint;cdecl;external;
procedure MainLoop;cdecl;external;
procedure ReparentWidget(qwidparent: longint; qwidchild: longint);cdecl;external;
procedure SetMainWidget(qwid:longint);cdecl;external;
procedure ShowWidget(wid: longint);cdecl;external;
procedure MoveWidget(qwid: longint; x: longint; y: longint);cdecl;external;
procedure ResizeWidget(qwid: longint; h: longint; w: longint);cdecl;external;
procedure SetWidgetText(qwid: longint; wtext: pchar);cdecl;external;
procedure HookMousePressedEvent(qwid: longint; ptrcall: pointer);cdecl;external;
procedure SetData(qwid: longint; data: pointer);cdecl;external;
function GetData(qwid:longint):pointer;cdecl;external;
procedure Shutdown;cdecl;external;
implementation
end.