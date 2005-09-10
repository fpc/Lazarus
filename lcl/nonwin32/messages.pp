{
 /***************************************************************************
                               Messages.pp
                               -----------

                   Initial Revision  : Tue Oct 19 CST 1999

 ***************************************************************************/

/***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 ***************************************************************************/

  This unit defines some of the most common definitions of Delphi's messages.pas
  to make porting easy.
  It is not used by the LCL itself.
}

unit Messages;

{$mode objfpc}{$H+}

interface

uses
  Classes, LCLType, LMessages;

const
  WM_DROPFILES     = LM_DROPFILES;
  WM_ERASEBKGND    = LM_ERASEBKGND;
  WM_GETDLGCODE    = LM_GETDLGCODE;
  WM_HSCROLL       = LM_HSCROLL;
  WM_KILLFOCUS     = LM_KILLFOCUS;
  WM_LButtonDblClk = LM_LBUTTONDBLCLK;
  WM_MOUSEMOVE     = LM_MOUSEMOVE;
  WM_NCHITTEST     = LM_NCHITTEST;
  WM_QUIT          = LM_QUIT;
  WM_SETFOCUS      = LM_SETFOCUS;
  WM_SIZE          = LM_SIZE;
  WM_SYSCHAR       = LM_SYSCHAR;
  WM_SYSKEYDOWN    = LM_SYSKEYDOWN;
  WM_SYSKEYUP      = LM_SYSKEYUP;
  WM_VSCROLL       = LM_VSCROLL;

type

  TMessage = TLMessage;

  TWMEraseBkgnd     = TLMEraseBkgnd;
  TWMKillFocus      = TLMKillFocus;
  TWMMouse          = TLMMouse;
  TWMNCHITTEST      = TLMNCHITTEST;
  TWMSCROLL         = TLMSCROLL;
  TWMSetFocus       = TLMSetFocus;
  TWMSIZE           = TLMSIZE;
  TWMHScroll        = TLMHScroll;
  TWMVScroll        = TLMVScroll;

  TWMGetDlgCode = TLMNoParams;


  TWMMouseMove = TLMMouseMove;

implementation

end.
