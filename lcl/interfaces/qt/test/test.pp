{
 QT Interface Test Program
}
program test;

uses
  qt, sysutils;

procedure ButtonClickEvent(qwid: longint);cdecl;
begin
 DebugLn('Click Event For Hwnd ' + IntToStr(qwid));
end;


{main qt library test}
var
winhwnd : longint;
buttonhwnd : longint;
cbhwnd: longint;
sbhwnd: longint;
mbhwnd: longint;
lbhwnd: longint;
fmhwnd: longint;
btnlabel : pchar;
lindex: integer;
ltop: integer;
lleft: integer;
lheight: integer;
lwidth: integer;

begin

btnlabel := 'LAZARUS QT';
ltop := 10;
lleft := 10;
lwidth := 85;
lheight := 30;

{ Initialize the QT Engine }
InitializeEngine;

{ Create a Form Widget }
winhwnd := CreateWidget(WIDGET);
ResizeWidget(winhwnd,400,400);

{ Create a button Widget}
buttonhwnd := CreateWidget(WIDGET_PUSH_BUTTON);

cbhwnd := CreateWidget(WIDGET_CHECK_BOX);
ReparentWidget(winhwnd,cbhwnd);
MoveWidget(cbhwnd,10,40);
ResizeWidget(cbhwnd,15,30);
ShowWidget(cbhwnd);

lbhwnd := CreateWidget(WIDGET_LISTBOX);
ReparentWidget(winhwnd,lbhwnd);
MoveWidget(lbhwnd,10,80);
ResizeWidget(lbhwnd,50,50);
ShowWidget(lbhwnd);

mbhwnd := CreateWidget(WIDGET_MENU_BAR);
ReparentWidget(winhwnd,mbhwnd);
MoveWidget(mbhwnd,10,140);
ResizeWidget(mbhwnd,10,50);
ShowWidget(mbhwnd);

sbhwnd := CreateWidget(WIDGET_SCROLL_BAR);
ReparentWidget(winhwnd,sbhwnd);
MoveWidget(sbhwnd,120,10);
ResizeWidget(sbhwnd,14,100);
ShowWidget(sbhwnd);

fmhwnd := CreateWidget(WIDGET_LCD_NUMBER);
ReparentWidget(winhwnd,fmhwnd);
MoveWidget(fmhwnd,220,10);
ResizeWidget(fmhwnd,75,75);
ShowWidget(fmhwnd);

{ hook click event }
HookSimpleClickEvent(buttonhwnd,@ButtonClickEvent);

{ Add button to parent }
ReparentWidget(winhwnd,buttonhwnd);

{ move widget }
MoveWidget(buttonhwnd,10,10);

{ resize widget }
ResizeWidget(buttonhwnd,85,30);

{ label button }
SetButtonText(buttonhwnd,btnlabel);



{ set the main widget }
SetMainWidget(winhwnd);



{ show buttons }
ShowWidget(buttonhwnd);

{ Show the widgets }
ShowWidget(winhwnd);



MainLoop;

end.
