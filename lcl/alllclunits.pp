{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit alllclunits;

{$warn 5023 off : no warning about unused units}
interface

uses
  CheckLst, Clipbrd, ColorBox, ComCtrls, Controls, CustomTimer, DBActns, 
  DbCtrls, DBGrids, DefaultTranslator, Dialogs, DynamicArray, DynHashArray, 
  DynQueue, ExtCtrls, ExtDlgs, ExtendedStrings, ExtGraphics, FileCtrl, Forms, 
  Graphics, GraphMath, GraphType, GraphUtil, Grids, HelpIntfs, IcnsTypes, 
  ImageListCache, ImgList, IniPropStorage, InterfaceBase, IntfGraphics, 
  LazHelpHTML, LazHelpIntf, LazLinkedList, LCLClasses, LCLIntf, LCLMemManager, 
  LCLMessageGlue, LCLProc, LCLResCache, LCLStrConsts, LCLType, Menus, 
  LCLUnicodeData, LCLVersion, LMessages, LResources, Maps, MaskEdit, 
  PairSplitter, PopupNotifier, PostScriptCanvas, PostScriptPrinter, 
  PostScriptUnicode, Printers, PropertyStorage, RubberBand, ShellCtrls, Spin, 
  StdActns, StdCtrls, StringHashList, TextStrings, Themes, TmSchema, Toolwin, 
  Translations, UTrace, XMLPropStorage, TimePopup, Messages, WSButtons, 
  WSCalendar, WSCheckLst, WSComCtrls, WSControls, WSDesigner, WSDialogs, 
  WSExtCtrls, WSExtDlgs, WSFactory, WSForms, WSGrids, WSImgList, WSLCLClasses, 
  WSMenus, WSPairSplitter, WSProc, WSReferences, WSSpin, WSStdCtrls, 
  WSToolwin, ActnList, AsyncProcess, ButtonPanel, Buttons, Calendar, 
  RegisterLCL, ValEdit, LazCanvas, LazDialogs, LazRegions, CustomDrawn_Common, 
  CustomDrawnControls, CustomDrawnDrawers, LazDeviceApis, LDockTree, 
  LazFreeTypeIntfDrawer, CustomDrawn_WinXP, CustomDrawn_Android, Arrow, 
  EditBtn, ComboEx, DBExtCtrls, CustomDrawn_Mac, CalcForm, LCLTranslator, 
  GroupedEdit, LCLTaskDialog, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('RegisterLCL', @RegisterLCL.Register);
  RegisterUnit('EditBtn', @EditBtn.Register);
end;

initialization
  RegisterPackage('LCLBase', @Register);
end.
