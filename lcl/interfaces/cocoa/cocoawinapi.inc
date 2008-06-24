{%MainUnit cocoaint.pp}
{ $Id: cocoawinapi.inc 15525 2008-06-23 06:39:58Z paul $ }
{******************************************************************************
  All Cocoa Winapi implementations.
  This are the implementations of the overrides of the Cocoa Interface for the
  methods defined in the
  lcl/include/winapi.inc


  !! Keep alphabetical !!


 ******************************************************************************
 Implementation
 ******************************************************************************

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

//##apiwiz##sps##   // Do not remove, no wizard declaration before this line

{------------------------------------------------------------------------------
  function ShowWindow(hWnd: HWND; nCmdShow: Integer): Boolean;

  nCmdShow:
    SW_SHOWNORMAL, SW_MINIMIZE, SW_SHOWMAXIMIZED
------------------------------------------------------------------------------}
function TCocoaWidgetSet.ShowWindow(hWnd: HWND; nCmdShow: Integer): Boolean;
var
  CocoaForm: TCocoaForm;
begin
  {$ifdef VerboseCocoaWinAPI}
    WriteLn('[WinAPI ShowWindow]');
  {$endif}

  Result := False;
  
  CocoaForm := TCocoaForm(hWnd);

  if CocoaForm <> nil then
  begin
    case nCmdShow of
      SW_SHOW: CocoaForm.MainWindow.orderFront(nil);
      SW_SHOWNORMAL: CocoaForm.MainWindow.orderFront(nil);
//      SW_MINIMIZE: Widget.setWindowState(QtWindowMinimized);
//      SW_SHOWMINIMIZED: Widget.ShowMinimized;
//      SW_SHOWMAXIMIZED: Widget.ShowMaximized;
      SW_HIDE: CocoaForm.MainWindow.orderOut(nil);
    end;
    Result := True;
  end;
end;

//##apiwiz##eps##   // Do not remove, no wizard declaration after this line
