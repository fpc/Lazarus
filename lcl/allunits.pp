{  $Id$  }
{
 *****************************************************************************
                               allunits.pp

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
unit AllUnits;

{$mode objfpc}{$H+}

interface

uses
  InterfaceBase,  LCLStrConsts,  Lazqueue,      GraphMath,
  StringHashList, DynHashArray,  LCLMemManager, ExtendedStrings,
  Buttons,        Extctrls,      Registry,      VCLGlobals,
  Calendar,       Clipbrd,       FileCtrl,      Forms,
  LCLLinux,       Spin,          Comctrls,      Graphics,
  LMessages,      StdCtrls,      Arrow,         Controls,
  Imglist,        Menus,         Toolwin,       Dialogs,
  Messages,       UTrace,        Clistbox,      ActnList,
  DynamicArray,   Grids,         Printers,      PostScriptPrinter,
  MaskEdit;

implementation

end.

{ =============================================================================

  $Log$
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
