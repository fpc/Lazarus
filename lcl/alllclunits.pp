{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit alllclunits;

{$warn 5023 off : no warning about unused units}
interface

uses
  ActnList, Arrow, AsyncProcess, ButtonPanel, Buttons, Calendar, CheckLst, 
  Clipbrd, ColorBox, ComboEx, ComCtrls, Controls, CustomDrawn_Android, 
  CustomDrawn_Common, CustomDrawn_Mac, CustomDrawn_WinXP, CustomDrawnControls, 
  CustomDrawnDrawers, CustomTimer, DBActns, DBCtrls, DBExtCtrls, DBGrids, 
  DefaultTranslator, DialogRes, Dialogs, EditBtn, ExtCtrls, ExtDlgs, 
  ExtGraphics, FileCtrl, Forms, Graphics, GraphUtil, Grids, GroupedEdit, 
  HelpIntfs, IcnsTypes, ImageListCache, ImgList, IndustrialBase, 
  IniPropStorage, InterfaceBase, IntfGraphics, JSONPropStorage, LazCanvas, 
  LazDeviceApis, LazDialogs, LazFreeTypeIntfDrawer, LazHelpHTML, LazHelpIntf, 
  LazRegions, LCLClasses, LCLExceptionStackTrace, LCLIntf, LCLMessageGlue, 
  LCLPlatformDef, LCLProc, LCLResCache, LCLStrConsts, LCLTranslator, LCLType, 
  LCLUnicodeData, LCLVersion, LDockTree, LMessages, LResources, MaskEdit, 
  Menus, PairSplitter, PopupNotifier, PostScriptCanvas, PostScriptPrinter, 
  PostScriptUnicode, Printers, PropertyStorage, RegisterLCL, RubberBand, 
  ShellCtrls, Spin, StdActns, StdCtrls, TaskDlgEmulation, Themes, TmSchema, 
  Toolwin, TreeStorage, UTrace, ValEdit, XMLPropStorage, CalcForm, TimePopup, 
  Messages, WSButtons, WSCalendar, WSCheckLst, WSComCtrls, WSControls, 
  WSDesigner, WSDialogs, WSExtCtrls, WSExtDlgs, WSFactory, WSForms, WSGrids, 
  WSImgList, WSLazDeviceAPIS, WSLCLClasses, WSMenus, WSPairSplitter, WSProc, 
  WSReferences, WSSpin, WSStdCtrls, WSToolwin, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('RegisterLCL', @RegisterLCL.Register);
end;

initialization
  RegisterPackage('LCLBase', @Register);
end.
