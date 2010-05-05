unit win32compat;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Windows;

{$ifdef Win32}

{ From System.pas }

function SysAllocStringLen(psz:pointer;len:dword):pointer;stdcall;
 external 'oleaut32.dll' name 'SysAllocStringLen';

procedure SysFreeString(bstr:pointer);stdcall;
 external 'oleaut32.dll' name 'SysFreeString';

function SysReAllocStringLen(var bstr:pointer;psz: pointer;
  len:dword): Integer; stdcall;external 'oleaut32.dll' name 'SysReAllocStringLen';

{ From Win32Int.pas }
type
  PInitCommonControlsEx = ^TInitCommonControlsEx;
  TInitCommonControlsEx = record
    dwSize: dword;
    dwICC: dword;
  end;

var
  InitCommonControlsEx: function(ICC: PInitCommonControlsEx): LongBool; stdcall;

{ From Win32Extras.pas }

const
  // Comctl32 version:
  // 4.70
  LVS_EX_GRIDLINES        = $00000001;
  LVS_EX_SUBITEMIMAGES    = $00000002;
  LVS_EX_CHECKBOXES       = $00000004;
  LVS_EX_TRACKSELECT      = $00000008;
  LVS_EX_HEADERDRAGDROP   = $00000010;
  LVS_EX_FULLROWSELECT    = $00000020;
  LVS_EX_ONECLICKACTIVATE = $00000040;
  LVS_EX_TWOCLICKACTIVATE = $00000080;
  // 4.71
  LVS_EX_FLATSB           = $00000100;
  LVS_EX_REGIONAL         = $00000200;
  LVS_EX_INFOTIP          = $00000400;
  LVS_EX_UNDERLINEHOT     = $00000800;
  LVS_EX_UNDERLINECOLD    = $00001000;
  LVS_EX_MULTIWORKAREAS   = $00002000;
  // 5.80
  LVS_EX_LABELTIP         = $00004000;
  // 4.71
  LVS_EX_BORDERSELECT     = $00008000;
  // 6
  LVS_EX_DOUBLEBUFFER     = $00010000;   // TODO: investigate
                                         // this may be a valid (ex) style message for other controls as well
                                         // atleast the same value is used for controls on the .net framework
                                         // coincidence ??
  LVS_EX_HIDELABELS       = $00020000;
  LVS_EX_SINGLEROW        = $00040000;
  LVS_EX_SNAPTOGRID       = $00080000;
  LVS_EX_SIMPLESELECT     = $00100000;

  { Tab Control Styles}
  TCS_RIGHT = $0002;
  TCS_BOTTOM = $0002;
  TCS_VERTICAL = $0080;
  TCS_MULTILINE = $0200;

{ From Windows.pas (adapted for win32) }
const
  SYS_COLOR_INDEX_FLAG = 0;

{$endif}

implementation

initialization

{$ifdef Win32}
  Pointer(InitCommonControlsEx) := GetProcAddress(GetModuleHandle(comctl32), 'InitCommonControlsEx');
{$endif}

end.

