{ $Id$ }
(******************************************************************************
  All interface communication related stuff goes here.
  This file is used by LCLIntf.pas
  If a procedure is platform dependent then it should call:
    InterfaceObject.MyDependentProc

  If a procedure insn't platform dependent, it is no part of InterfaseBase has
  to be implementerd here

  !! Keep this alphabetical !!

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
 ******************************************************************************)

{ =============================================================================

  $Log$
  Revision 1.18  2004/02/22 22:52:58  micha
  split interface into non-lcl and lcl-component dependent parts

  Revision 1.17  2004/02/20 19:52:18  micha
  fixed: tarrow crash in win32
  added: interface function DrawArrow to draw themed arrow

  Revision 1.16  2004/02/19 05:07:17  mattias
  CreateBitmapFromRawImage now creates mask only if needed

  Revision 1.15  2004/02/17 00:32:25  mattias
  fixed TCustomImage.DoAutoSize fixing uninitialized vars

  Revision 1.14  2004/02/03 08:54:09  mattias
  Frame3D rect now var again

  Revision 1.13  2004/02/02 15:46:19  mattias
  implemented basic TSplitter, still many ToDos

  Revision 1.12  2004/02/02 12:44:45  mattias
  implemented interface constraints

  Revision 1.11  2004/01/12 08:36:34  micha
  statusbar interface dependent reimplementation (from vincent)

  Revision 1.10  2004/01/11 16:38:29  marc
  * renamed (Check|Enable)MenuItem to MenuItemSet(Check|Enable)
  + Started with accelerator nameing routines
  * precheckin for createwidget splitup

  Revision 1.9  2004/01/11 11:57:54  mattias
  implemented TCustomListBox.ItemRect for gtk1 intf

  Revision 1.8  2004/01/10 18:00:42  mattias
  fixed GetWindowOrgEx, added GetDCOriginRelativeToWindow

  Revision 1.7  2004/01/09 20:03:13  mattias
  implemented new statusbar methods in gtk intf

  Revision 1.6  2004/01/03 11:57:47  mattias
  applied implementation for LM_LB_GETINDEXAT  from Vincent

  Revision 1.5  2003/11/27 23:02:30  mattias
  removed menutype.pas

  Revision 1.4  2003/11/26 21:30:19  mattias
  reduced unit circles, fixed fpImage streaming

  Revision 1.3  2003/11/26 00:23:47  marc
  * implemented new LCL(check|enable)Menuitem functions
  * introduced the lclintf inc files to win32

  Revision 1.2  2003/11/25 14:21:28  micha
  new api lclenable,checkmenuitem according to list

  Revision 1.1  2003/11/24 11:03:07  marc
  * Splitted winapi*.inc into a winapi and a lcl interface communication part

}
