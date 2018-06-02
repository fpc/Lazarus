{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit alllclunits;

{$warn 5023 off : no warning about unused units}
interface

uses
  CheckLst, Clipbrd, ColorBox, ComCtrls, Controls, CustomTimer, DBActns, 
  DBCtrls, DBGrids, DefaultTranslator, Dialogs, ExtCtrls, ExtDlgs, 
  ExtGraphics, FileCtrl, Forms, Graphics, GraphMath, GraphType, GraphUtil, 
  Grids, HelpIntfs, IcnsTypes, ImageListCache, ImgList, IniPropStorage, 
  InterfaceBase, IntfGraphics, LazHelpHTML, LazHelpIntf, LCLClasses, LCLIntf, 
  LCLMemManager, LCLMessageGlue, LCLProc, LCLResCache, LCLStrConsts, LCLType, 
  Menus, LCLUnicodeData, LCLVersion, LMessages, LResources, MaskEdit, 
  PairSplitter, PopupNotifier, PostScriptCanvas, PostScriptPrinter, 
  PostScriptUnicode, Printers, PropertyStorage, RubberBand, ShellCtrls, Spin, 
  StdActns, StdCtrls, Themes, TmSchema, Toolwin, UTrace, XMLPropStorage, 
  TimePopup, Messages, WSButtons, WSCalendar, WSCheckLst, WSComCtrls, 
  WSControls, WSDesigner, WSDialogs, WSExtCtrls, WSExtDlgs, WSFactory, 
  WSForms, WSGrids, WSImgList, WSLCLClasses, WSMenus, WSPairSplitter, WSProc, 
  WSReferences, WSSpin, WSStdCtrls, WSToolwin, ActnList, AsyncProcess, 
  ButtonPanel, Buttons, Calendar, RegisterLCL, ValEdit, LazCanvas, LazDialogs, 
  LazRegions, CustomDrawn_Common, CustomDrawnControls, CustomDrawnDrawers, 
  LazDeviceApis, LDockTree, LazFreeTypeIntfDrawer, CustomDrawn_WinXP, 
  CustomDrawn_Android, Arrow, EditBtn, ComboEx, DBExtCtrls, CustomDrawn_Mac, 
  CalcForm, LCLTranslator, GroupedEdit, LCLTaskDialog, WSLazDeviceAPIS, 
  LCLPlatformDef, IndustrialBase, JSONPropStorage, LCLExceptionStacktrace, 
  LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('RegisterLCL', @RegisterLCL.Register);
end;

initialization
  RegisterPackage('LCLBase', @Register);
end.
