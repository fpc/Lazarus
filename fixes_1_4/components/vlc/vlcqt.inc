{%MainUnit lclvlc.pp}

uses qt45;

Function GetXHandle(AWinControl : TWinControl) : culong;

var
  Widget: PGtkWidget;
begin
  Result:=0;
  if (AWinControl=nil) or (not AWinControl.HandleAllocated) then exit;
  Result:=QWidget_winID(TQtWidget(AWinControl.Handle).Widget);
end;

