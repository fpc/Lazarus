unit fpguicrosshelpers;

{$mode objfpc}{$H+}

interface

uses
  Classes
  , SysUtils
  , fpg_main
  {$IFDEF WINDOWS}
  , windows
  {$ENDIF}
  {$IFDEF UNIX}
  {$ENDIF}
  ;

function GetCursorPos(var aPoint: TPoint): Boolean;

implementation

function GetCursorPos(var aPoint: TPoint): Boolean;
begin
  {$IFDEF WINDOWS}
  Result:=windows.GetCursorPos(aPoint);
  {$ENDIF}
  {$IFDEF UNIX}
  // TODO: Implement XQueryPointer
  // Bool XQueryPointer(display, w, root_return, child_return, root_x_return, root_y_return,
  //                    win_x_return, win_y_return, mask_return)
  // XQueryPointer(xapplication.Display, wh, @root_return, @child_return, @root_x_return, @root_y_return, @win_x_return, @win_y_return, @mask_return);
  aPoint.x:=0;
  aPoint.y:=0;
  Result:=false;
  {$ENDIF}
end;

end.

