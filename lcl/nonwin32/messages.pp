{
 /***************************************************************************
                               Messages.pp
                               -----------

                   Initial Revision  : Tue Oct 19 CST 1999

 ***************************************************************************/

 *****************************************************************************
 *                                                                           *
 *  This file is part of the Lazarus Component Library (LCL)                 *
 *                                                                           *
 *  See the file COPYING.modifiedLGPL.txt, included in this distribution,    *
 *  for details about the copyright.                                         *
 *                                                                           *
 *  This program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
 *                                                                           *
 *****************************************************************************

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
  WM_LBUTTONUP     = LM_LBUTTONUP;
  WM_LBUTTONDBLCLK = LM_LBUTTONDBLCLK;
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
