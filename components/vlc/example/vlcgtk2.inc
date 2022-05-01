{%MainUnit lclvlc.pp}

uses gtk2proc, gtk2extra, ctypes, gtk2;

Function GetXHandle(AWinControl : TWinControl) : culong;

var
  Widget: PGtkWidget;
begin
  Result:=0;
  if (AWinControl=nil) or (not AWinControl.HandleAllocated) then exit;
  Widget:={%H-}PGtkWidget(AWinControl.Handle);
  if Widget^.window = nil then exit;
  Result := gdk_window_xwindow(Widget^.window);
end;

