{  $Id$  }
{
 *****************************************************************************
                               alllclunits.pp

                      dummy unit to compile all units

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
unit AllLCLUnits;

{$mode objfpc}{$H+}

{$IFDEF VER1_0_10}
  {$DEFINE DisableFPImage}
{$ENDIF}

interface

uses
  // resource strings
  LCLStrConsts,
  // base classes
  FPCAdds, LazLinkedList, DynHashArray, LCLMemManager, AvgLvlTree,
  StringHashList, ExtendedStrings, DynamicArray, UTrace, TextStrings,
  // base types and base functions
  LCLProc, LCLType, GraphMath, VCLGlobals, FileCtrl, LMessages,
  // the interface base
  InterfaceBase, {$IFNDEF DisableFPImage}IntfGraphics,{$ENDIF}
  // components and functions
  Buttons, Extctrls, Calendar, Clipbrd, Forms, LCLIntf, Spin,
  Comctrls, Graphics, StdCtrls, Arrow, Controls, ImgList, Menus, Toolwin,
  Dialogs, Messages, Clistbox, ActnList, Grids, MaskEdit,
  Printers, PostScriptPrinter, CheckLst, PairSplitter, ExtDlgs,
  DBCtrls, DBGrids, EditBtn;

implementation

end.

{ =============================================================================

  $Log$
  Revision 1.9  2004/02/10 00:38:43  mattias
  deactivated fpImage or fpc 1.0.10

  Revision 1.8  2004/02/02 22:01:51  mattias
  fpImage is now used as default, deactivate it with -dDisableFPImage

  Revision 1.7  2004/01/26 11:58:37  mattias
  excluded dirsel.pp from makefile

  Revision 1.6  2003/12/25 14:17:07  mattias
  fixed many range check warnings

  Revision 1.5  2003/12/23 20:40:43  mattias
  added TEditButton, TFileNameEdit, TDirectoryEdit, TDateEdit, TCalcEdit from Michael V.C.

  Revision 1.4  2003/12/07 09:07:22  mattias
  fixed ambigious units registry and forms

  Revision 1.3  2003/11/27 23:02:30  mattias
  removed menutype.pas

  Revision 1.2  2003/11/26 21:30:19  mattias
  reduced unit circles, fixed fpImage streaming

  Revision 1.1  2003/11/15 13:07:09  mattias
  added ambigious unit check for IDE

  Revision 1.31  2003/09/20 09:16:07  mattias
  added TDBGrid from Jesus

  Revision 1.30  2003/09/18 09:21:03  mattias
  renamed LCLLinux to LCLIntf

  Revision 1.29  2003/09/16 11:35:14  mattias
  started TDBCheckBox

  Revision 1.28  2003/09/02 21:32:56  mattias
  implemented TOpenPictureDialog

  Revision 1.27  2003/08/18 13:21:23  mattias
  renamed lazqueue to lazlinkedlist, patch from Jeroen

  Revision 1.26  2003/08/01 09:44:52  mattias
  added SelectDirectory dialog

  Revision 1.25  2003/07/31 17:16:32  mattias
  added ToDo for textstrings.pas

  Revision 1.24  2003/07/04 22:06:49  mattias
  implemented interface graphics

  Revision 1.23  2002/08/19 15:15:23  mattias
  implemented TPairSplitter

  Revision 1.22  2003/06/20 01:37:47  marc
  + Added TCheckListBox component

  Revision 1.21  2003/06/19 16:36:35  mattias
  started codeexplorer

  Revision 1.20  2003/04/16 22:59:35  mattias
  added TMaskEdit from Tony

  Revision 1.19  2003/03/25 18:40:56  mattias
  added Tonys printer units

  Revision 1.18  2003/01/04 20:46:32  mattias
  added grids.pas from Jesus Reyes A.

  Revision 1.17  2002/10/26 15:15:44  lazarus
  MG: broke LCL<->interface circles

  Revision 1.16  2002/10/04 14:24:14  lazarus
  MG: added DrawItem to TComboBox/TListBox

  Revision 1.15  2002/09/11 15:04:49  lazarus
  MG: added stringhashlist.pas

  Revision 1.14  2002/08/21 08:13:37  lazarus
  MG: accelerated new/dispose of gdiobjects

  Revision 1.13  2002/08/08 18:05:46  lazarus
  MG: added graphics extensions from Andrew Johnson

  Revision 1.12  2002/07/04 11:46:00  lazarus
  MG: moved resourcestring to lclstrconsts.pas

  Revision 1.11  2002/05/10 06:05:48  lazarus
  MG: changed license to LGPL

  Revision 1.10  2001/12/06 13:39:36  lazarus
  Added TArrow component
  Shane

  Revision 1.9  2001/12/05 18:19:11  lazarus
  MG: added calendar to allunits and removed unused vars

  Revision 1.8  2001/06/16 09:14:38  lazarus
  MG: added lazqueue and used it for the messagequeue

  Revision 1.6  2001/03/27 20:55:23  lazarus
  MWE:
    * changed makefiles so that the LCL is build separate fron interfaces

  Revision 1.5  2001/03/19 18:51:57  lazarus
  MG: added dynhasharray and renamed tsynautocompletion

  Revision 1.4  2001/01/30 22:56:54  lazarus
  MWE:
    + added $mode objfpc directive

  Revision 1.3  2001/01/10 23:54:59  lazarus
  MWE:
    * Fixed make clean
    + moved allunits from exe to unit, skipping link stage

}
