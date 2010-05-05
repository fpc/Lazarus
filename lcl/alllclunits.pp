{  $Id$  }
{
 *****************************************************************************
                               alllclunits.pp

                      dummy unit to compile all units

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
}
unit AllLCLUnits;

{ At least 2.4.0 is required, except for wince which supports fpc 2.2.0+ too }
{$ifdef Wince}
  {$if defined(ver1) or (defined(ver2) and (fpc_release<2))}
    {$fatal Lazarus for WinCE requires at least FPC 2.2.0}
  {$endif}
{$else}
  {$if defined(ver1) or (defined(ver2) and (fpc_release<4))}
    {$fatal Lazarus requires at least FPC 2.4.0}
  {$endif}
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
  LCLProc, LCLType, LCLResCache, GraphMath, GraphType, GraphUtil,
  LMessages, LResources, LConvEncoding, LCLUnicodeData, FileUtil, Translations, 
  LazConfigStorage,
  // the interface base
  InterfaceBase,
  IntfGraphics,
  // components and functions
  LCLClasses, AsyncProcess, FileCtrl, Maps, HelpIntfs, LazHelpIntf, LazHelpHTML,
  StdActns, Buttons, Extctrls, Calendar, Clipbrd, Forms, LCLIntf, Spin,
  Comctrls, Graphics, StdCtrls, Arrow, Controls, ImgList, Menus, Toolwin,
  Dialogs, Messages, ActnList, Grids, MaskEdit, ButtonPanel,
  Printers, PostScriptPrinter, PostScriptCanvas, CheckLst, PairSplitter,
  ExtDlgs, DBCtrls, DBGrids, DBActns, EditBtn, ExtGraphics, ColorBox,
  PropertyStorage, IniPropStorage, XMLPropStorage, Chart, LDockTree, LDockCtrl,
  CalendarPopup, Themes, PopupNotifier, ShellCtrls, UTF8Process,
  LCLMessageGlue,
  RubberBand,
  // widgetset skeleton
  WSArrow, WSButtons, WSCalendar,
  WSCheckLst, WSComCtrls, WSControls,
  WSDialogs, WSDesigner, WSExtCtrls,
  WSExtDlgs, WSForms, WSGrids, WSImgList, WSMenus,
  WSPairSplitter, WSSpin, WSStdCtrls, WSToolwin,
  WSProc,
  // Other units
  DefaultTranslator;

implementation

end.




