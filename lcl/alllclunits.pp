{  $Id$  }
{
 *****************************************************************************
                               alllclunits.pp

                      dummy unit to compile all units

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
unit AllLCLUnits;

{ At least 2.0.2 is required }
{$if defined(ver1) or (defined(ver2_0) and (fpc_patch<2))}
  {$fatal Lazarus requires at least FPC 2.0.2}
{$endif}
{$mode objfpc}{$H+}

interface

uses
  // lcl version
  LCLVersion,
  // resource strings
  LCLStrConsts,
  // base classes
  FPCAdds, LazLinkedList, DynHashArray, LCLMemManager, AvgLvlTree, DynQueue,
  StringHashList, ExtendedStrings, DynamicArray, UTrace, TextStrings,
  // base types and base functions
  LCLProc, LCLType, LCLResCache, GraphMath, FileCtrl, LMessages, LResources,
  LConvEncoding, FileUtil, Translations, LazConfigStorage,
  // the interface base
  InterfaceBase,
  IntfGraphics,
  // components and functions
  LCLClasses, AsyncProcess, Maps, HelpIntfs, LazHelpIntf, LazHelpHTML,
  StdActns, Buttons, Extctrls, Calendar, Clipbrd, Forms, LCLIntf, Spin,
  Comctrls, Graphics, StdCtrls, Arrow, Controls, ImgList, Menus, Toolwin,
  Dialogs, Messages, Clistbox, ActnList, Grids, MaskEdit, ButtonPanel,
  Printers, PostScriptPrinter, PostScriptCanvas, CheckLst, PairSplitter,
  ExtDlgs, DBCtrls, DBGrids, DBActns, EditBtn, ExtGraphics, ColorBox,
  PropertyStorage, IniPropStorage, XMLPropStorage, Chart, LDockTree, LDockCtrl,
  CalendarPopup, Themes,
  LCLMessageGlue,
  RubberBand,
  // widgetset skeleton
  WSActnList, WSArrow, WSButtons, WSCalendar,
  WSCheckLst, WSCListBox, WSComCtrls, WSControls,
  WSDbCtrls, WSDBGrids, WSDialogs, WSDirSel,
  WSEditBtn, WSExtCtrls, WSExtDlgs, WSFileCtrl,
  WSForms, WSGrids, WSImgList, WSMaskEdit,
  WSMenus, WSPairSplitter, WSSpin, WSStdCtrls,
  WSToolwin,
  WSProc,
  WSDesigner,
  // Other units
  DefaultTranslator;

implementation

end.




