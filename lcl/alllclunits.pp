{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit alllclunits;

interface

uses
  BarChart, CheckLst, Clipbrd, ColorBox, ComCtrls, Controls, CustomTimer, 
  DBActns, DbCtrls, DBGrids, DefaultTranslator, Dialogs, DynamicArray, 
  DynHashArray, DynQueue, ExtCtrls, ExtDlgs, ExtendedStrings, extgraphics, 
  FileCtrl, Forms, FPCAdds, Graphics, GraphMath, GraphType, GraphUtil, Grids, 
  HelpIntfs, IcnsTypes, ImageListCache, ImgList, IniPropStorage, 
  InterfaceBase, IntfGraphics, LazHelpHTML, LazHelpIntf, LazLinkedList, 
  LCLClasses, LCLIntf, LCLMemManager, LCLMessageGlue, LCLProc, LCLResCache, 
  LCLStrConsts, LCLType, Menus, LCLUnicodeData, LCLVersion, LMessages, 
  LResources, maps, MaskEdit, PairSplitter, PopupNotifier, PostScriptCanvas, 
  PostScriptPrinter, postscriptunicode, Printers, PropertyStorage, RubberBand, 
  ShellCtrls, Spin, StdActns, StdCtrls, StringHashList, TextStrings, Themes, 
  TmSchema, Toolwin, Translations, UTrace, XMLPropStorage, Messages, 
  WSButtons, WSCalendar, WSCheckLst, WSComCtrls, WSControls, WSDesigner, 
  WSDialogs, WSExtCtrls, WSExtDlgs, WSFactory, WSForms, WSGrids, WSImgList, 
  WSLCLClasses, WSMenus, WSPairSplitter, WSProc, WSReferences, WSSpin, 
  WSStdCtrls, WSToolwin, ActnList, AsyncProcess, ButtonPanel, Buttons, 
  Calendar, RegisterLCL, ValEdit, lazcanvas, lazdialogs, lazregions, 
  customdrawn_common, customdrawncontrols, customdrawndrawers, lazdeviceapis, 
  LDockTree, LazFreeTypeIntfDrawer, customdrawn_winxp, customdrawn_android, 
  Arrow, EditBtn, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('RegisterLCL', @RegisterLCL.Register);
  RegisterUnit('EditBtn', @EditBtn.Register);
end;

initialization
  RegisterPackage('LCLBase', @Register);
end.
