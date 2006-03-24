{%MainUnit winceint.pp}

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

{*************************************************************}
{            callback routines                                }
{*************************************************************}

{------------------------------------------------------------------------------
 Function: WindowProc
 Params: Window - The window that receives a message
         Msg    - The message received
         WParam - Word parameter
         LParam - Long-integer parameter
  Returns: 0 if Msg is handled; non-zero long-integer result otherwise

  Handles the messages sent to the specified window, in parameter Window, by
  Windows or other applications
 ------------------------------------------------------------------------------}
function WindowProc(Window: HWnd; Msg: UInt; WParam: Windows.WParam;
    LParam: Windows.LParam): LResult; stdcall;
var
  LMessage: TLMessage;
  PLMsg: PLMessage;
begin
  {$ifdef VerboseWinCE}
//  WriteLn('WindowProc: Msg = ' + IntToStr(Msg));
  {$endif}

  Result := 0;
 
  LMessage.Result := 0;
  LMessage.Msg := LM_NULL;

  case Msg of
  
    WM_CLOSE:
    begin
      DestroyWindow(Window);
    end;
    
    WM_DESTROY :
    begin
      PostQuitMessage(0);
    end;
  else
    Result := DefWindowProc(Window, Msg, wParam, lParam);
  end;
end;

