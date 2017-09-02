{
 /***************************************************************************
                               Messages.pp
                               -----------

                   Initial Revision  : Tue Oct 19 CST 1999

 ***************************************************************************/

 *****************************************************************************
  This file is part of the Lazarus Component Library (LCL)

  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************

  This unit defines some of the most common definitions of Delphi's messages.pas
  to make porting easy.
  It is not used by the LCL itself.
}
{$ifdef WINDOWS}
  {$Error This unit is not for MS Windows. Windows has unit windows for that. }
{$ENDIF}

unit Messages;

{$mode objfpc}{$H+}

interface

uses
  Classes, LCLType, LMessages;

const
  WM_DROPFILES        = LM_DROPFILES;
  WM_ERASEBKGND       = LM_ERASEBKGND;
  WM_GETDLGCODE       = LM_GETDLGCODE;
  WM_HSCROLL          = LM_HSCROLL;
  WM_KILLFOCUS        = LM_KILLFOCUS;
  WM_LBUTTONUP        = LM_LBUTTONUP;
  WM_LBUTTONDBLCLK    = LM_LBUTTONDBLCLK;
  WM_MOUSEMOVE        = LM_MOUSEMOVE;
  WM_MOUSEENTER       = LM_MOUSEENTER;
  WM_MOUSELEAVE       = LM_MOUSELEAVE;
  WM_NCHITTEST        = LM_NCHITTEST;
  WM_QUIT             = LM_QUIT;
  WM_SETFOCUS         = LM_SETFOCUS;
  WM_SIZE             = LM_SIZE;
  WM_SYSCHAR          = LM_SYSCHAR;
  WM_SYSKEYDOWN       = LM_SYSKEYDOWN;
  WM_SYSKEYUP         = LM_SYSKEYUP;
  WM_VSCROLL          = LM_VSCROLL;
  WM_NCPAINT          = LM_NCPAINT;
  WM_CAPTURECHANGED   = LM_CAPTURECHANGED;
  WM_KEYDOWN          = LM_KEYDOWN;
  WM_KEYUP            = LM_KEYUP;
  WM_WINDOWPOSCHANGED = LM_WINDOWPOSCHANGED;
  WM_CUT              = LM_CUT;
  WM_COPY             = LM_COPY;
  WM_PASTE            = LM_PASTE;
  WM_SETCURSOR        = LM_SETCURSOR;
  WM_PAINT            = LM_PAINT;
  WM_LBUTTONDOWN      = LM_LBUTTONDOWN;
  WM_CHAR             = LM_CHAR;
  WM_COMMAND          = LM_COMMAND;
  WM_CANCELMODE       = LM_CANCELMODE;
  WM_NOTIFY           = LM_NOTIFY;
  WM_CREATE           = LM_CREATE;
  WM_ACTIVATE         = LM_ACTIVATE;

type

  TMessage = TLMessage;
  // under Windows in unit windows
  TWMEraseBkgnd       = TLMEraseBkgnd;
  TWMKillFocus        = TLMKillFocus;
  TWMMouse            = TLMMouse;
  TWMNCHITTEST        = TLMNCHITTEST;
  TWMSCROLL           = TLMSCROLL;
  TWMSetFocus         = TLMSetFocus;
  TWMSIZE             = TLMSIZE;
  TWMHScroll          = TLMHScroll;
  TWMVScroll          = TLMVScroll;
  TWMKeyDown          = TLMKeyDown;
  TWMKeyUp            = TLMKeyUp;
  TWMWindowPosChanged = TLMWindowPosChanged;
  TWMPaint            = TLMPaint;
  TWMLButtonDown      = TLMLButtonDown;
  TWMLButtonUp        = TLMLButtonUp;
  TWMLButtonDblClk    = TLMLButtonDblClk;
  TWMKey              = TLMKey;
  TWMCommand          = TLMCommand;
  TWMChar             = TLMChar;
  TWMNotify           = TLMNotify;
  TWMCut              = TLMCut;
  TWMCopy             = TLMCopy;
  TWMPaste            = TLMPaste;
  TWMSetCursor        = TLMSetCursor;
  TWMNoParams         = TLMNoParams;
  TWMGetDlgCode       = TLMNoParams;
  TWMMouseMove        = TLMMouseMove;
  TWMActivate         = TLMActivate;

implementation

end.
