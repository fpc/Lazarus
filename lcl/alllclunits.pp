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

{ At least 2.0.0 is required }
{$ifdef VER1}
  {$fatal You need at least FPC 2.0.0}
{$endif}

{$mode objfpc}{$H+}

interface

uses
  // resource strings
  LCLStrConsts,
  // base classes
  FPCAdds, LazLinkedList, DynHashArray, LCLMemManager, AvgLvlTree,
  StringHashList, ExtendedStrings, DynamicArray, UTrace, TextStrings,
  // base types and base functions
  LCLProc, LCLType, LCLResCache, GraphMath, FileCtrl, LMessages, LResources,
  FileUtil,
  // the interface base
  InterfaceBase,
  IntfGraphics,
  // components and functions
  LCLClasses,
  StdActns, Buttons, Extctrls, Calendar, Clipbrd, Forms, LCLIntf, Spin,
  Comctrls, Graphics, StdCtrls, Arrow, Controls, ImgList, Menus, Toolwin,
  Dialogs, Messages, Clistbox, ActnList, Grids, MaskEdit,
  Printers, PostScriptPrinter, PostScriptCanvas, CheckLst, PairSplitter,
  ExtDlgs, DBCtrls, DBGrids, DBActns, EditBtn, ExtGraphics, ColorBox,
  PropertyStorage, IniPropStorage, XMLPropStorage, Chart, LDockTree,
  // widgetset skeleton
  WSActnList, WSArrow, WSButtons, WSCalendar,
  WSCheckLst, WSCListBox, WSComCtrls, WSControls,
  WSDbCtrls, WSDBGrids, WSDialogs, WSDirSel,
  WSEditBtn, WSExtCtrls, WSExtDlgs, WSFileCtrl,
  WSForms, WSGrids, WSImgList, WSMaskEdit,
  WSMenus, WSPairSplitter, WSSpin, WSStdCtrls,
  WSToolwin,
  WSProc
  {$ifdef TRANSLATESTRING}
  ,DefaultTranslator
  {$ENDIF};

implementation

end.

{
  $Log$
  Revision 1.28  2005/07/19 08:31:21  vincents
  added ColorBox (from Darius) to LCL

  Revision 1.27  2005/07/16 00:08:26  marc
  * Reimplemented ZOrder
  + Added IDE option to move a control one forward/back
  * Fixed IDE control selection
  - Removed some IFDEF VER 1_0
  + Added some inline

  Revision 1.26  2005/07/13 07:33:14  mattias
  implemented renaming of new package files

  Revision 1.25  2005/03/23 10:45:06  mattias
  fixed ambigious with ambiguous

  Revision 1.24  2004/12/27 12:56:42  mattias
  started TTranslateStrings and .lrt files support  from Vasily

  Revision 1.23  2004/10/04 09:05:23  mattias
  added postscript canvas example  from Olivier

  Revision 1.22  2004/09/27 21:45:44  vincents
  splitted off unit FileUtil, it doesn't depend on other LCL units

  Revision 1.21  2004/09/24 13:45:31  mattias
  fixed TCanvas.TextRect Delphi compatible Rect and added TBarChart from Michael VC

  Revision 1.20  2004/08/18 09:31:21  mattias
  removed obsolete unit vclglobals

  Revision 1.19  2004/08/15 14:39:36  mattias
  implemented platform independent binary object streamer

  Revision 1.18  2004/08/11 20:57:09  mattias
  moved intfstrconsts.pp to lclstrconsts.pas, implemented TPenHandleCache

  Revision 1.17  2004/08/03 10:01:22  mattias
  added DBActns  from Michael VC

  Revision 1.16  2004/07/25 15:39:55  mattias
  added rx components  from Michal Van Canneyt

  Revision 1.15  2004/07/23 22:06:56  mattias
  started propertystorage enable with -dEnableSessionProps

  Revision 1.14  2004/05/16 23:24:41  marc
  + Added WSBitBtn interface
  + Implemented WSBitBtn interface for gtk

  Revision 1.13  2004/05/01 23:24:19  mattias
  fixed range check error and added extgraphics.pas

  Revision 1.12  2004/04/29 18:08:17  mattias
  fixed 1.0.10 compilation

  Revision 1.11  2004/03/19 00:03:14  marc
  * Moved the implementation of (GTK)ButtonCreateHandle to the new
    (GTK)WSButton class

  Revision 1.10  2004/02/24 21:53:12  mattias
  added StdActns definitions, no code yet

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
  fixed ambiguous units registry and forms

  Revision 1.3  2003/11/27 23:02:30  mattias
  removed menutype.pas

  Revision 1.2  2003/11/26 21:30:19  mattias
  reduced unit circles, fixed fpImage streaming

  Revision 1.1  2003/11/15 13:07:09  mattias
  added ambiguous unit check for IDE

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
