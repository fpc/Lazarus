unit gtk2windows;

{$mode objfpc}{$H+}

interface

uses
  Windows;
  
  function GetAppHandle: Thandle;

implementation

const
  ClsName: array[0..6] of char = 'Window'#0;
  PrivateAppHandle: THandle = 0;

function GetAppHandle: Thandle;
var
  WindowClass: WndClass;
begin
  if PrivateAppHandle=0 then begin
    // register class
    with WindowClass do
    begin
      Style := CS_DBLCLKS{CS_HRedraw or CS_VRedraw};
      LPFnWndProc := @Windows.DefWindowProc;
      CbClsExtra := 0;
      CbWndExtra := 0;
      hInstance := System.HInstance;
      hIcon := Windows.LoadIcon(0, IDI_APPLICATION);
      hCursor := Windows.LoadCursor(0, IDC_ARROW);
      hbrBackground := 0; {GetSysColorBrush(Color_BtnFace);}
      LPSzMenuName := Nil;
      LPSzClassName := @ClsName;
    end;
    if Windows.RegisterClass(@WindowClass) <> 0 then begin
      PrivateAppHandle := CreateWindow(@ClsName, nil,
        WS_POPUP or WS_CLIPSIBLINGS or WS_SYSMENU or WS_MINIMIZEBOX,
        0, 0, 0, 0, HWND(nil), HMENU(nil), HInstance, nil);
    end;
  end;
  result := PrivateAppHandle;
end;


finalization
  if PrivateAppHandle <> 0 then begin
    DestroyWindow(PrivateAppHandle);
    Windows.UnregisterClass(@ClsName, System.HInstance);
  end;

end.

