{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit alllclunits; 

interface

uses
  Chart, CheckLst, Clipbrd, ColorBox, ComCtrls, Controls, CustomTimer, 
  DBActns, DbCtrls, DBGrids, DefaultTranslator, Dialogs, DynamicArray, 
  DynHashArray, DynQueue, EditBtn, ExtCtrls, ExtDlgs, ExtendedStrings, 
  extgraphics, FileCtrl, FileUtil, Forms, FPCAdds, Graphics, GraphMath, 
  GraphType, GraphUtil, Grids, HelpIntfs, IcnsTypes, ImageListCache, ImgList, 
  IniPropStorage, InterfaceBase, IntfGraphics, LazConfigStorage, LazHelpHTML, 
  LazHelpIntf, LazLinkedList, LCLClasses, LCLIntf, LCLMemManager, 
  LCLMessageGlue, LCLProc, LCLResCache, LCLStrConsts, LCLType, LCLUnicodeData, 
  LCLVersion, LConvEncoding, LDockCtrl, LDockCtrlEdit, LDockTree, LMessages, 
  LResources, maps, MaskEdit, Masks, Menus, PairSplitter, PopupNotifier, 
  PostScriptCanvas, PostScriptPrinter, postscriptunicode, Printers, 
  PropertyStorage, RubberBand, ShellCtrls, Spin, StdActns, StdCtrls, 
  StringHashList, TextStrings, Themes, TmSchema, Toolwin, Translations, 
  UTF8Process, UTrace, XMLPropStorage, WSArrow, WSButtons, WSCalendar, 
  WSCheckLst, WSComCtrls, WSControls, WSDesigner, WSDialogs, WSExtCtrls, 
  WSExtDlgs, WSFactory, WSForms, WSGrids, WSImgList, WSLCLClasses, WSMenus, 
  WSPairSplitter, WSProc, WSReferences, WSSpin, WSStdCtrls, WSToolwin, 
  ActnList, Arrow, AsyncProcess, AvgLvlTree, ButtonPanel, Buttons, Calendar, 
  LazarusPackageIntf;

implementation

procedure Register; 
begin
  RegisterUnit('Chart', @Chart.Register); 
  RegisterUnit('CheckLst', @CheckLst.Register); 
  RegisterUnit('ColorBox', @ColorBox.Register); 
  RegisterUnit('ComCtrls', @ComCtrls.Register); 
  RegisterUnit('Controls', @Controls.Register); 
  RegisterUnit('DBActns', @DBActns.Register); 
  RegisterUnit('DbCtrls', @DbCtrls.Register); 
  RegisterUnit('DBGrids', @DBGrids.Register); 
  RegisterUnit('Dialogs', @Dialogs.Register); 
  RegisterUnit('EditBtn', @EditBtn.Register); 
  RegisterUnit('ExtCtrls', @ExtCtrls.Register); 
  RegisterUnit('ExtDlgs', @ExtDlgs.Register); 
  RegisterUnit('FileCtrl', @FileCtrl.Register); 
  RegisterUnit('Forms', @Forms.Register); 
  RegisterUnit('Graphics', @Graphics.Register); 
  RegisterUnit('Grids', @Grids.Register); 
  RegisterUnit('IniPropStorage', @IniPropStorage.Register); 
  RegisterUnit('LazHelpHTML', @LazHelpHTML.Register); 
  RegisterUnit('LDockCtrl', @LDockCtrl.Register); 
  RegisterUnit('LResources', @LResources.Register); 
  RegisterUnit('MaskEdit', @MaskEdit.Register); 
  RegisterUnit('Menus', @Menus.Register); 
  RegisterUnit('PairSplitter', @PairSplitter.Register); 
  RegisterUnit('PopupNotifier', @PopupNotifier.Register); 
  RegisterUnit('ShellCtrls', @ShellCtrls.Register); 
  RegisterUnit('Spin', @Spin.Register); 
  RegisterUnit('StdActns', @StdActns.Register); 
  RegisterUnit('StdCtrls', @StdCtrls.Register); 
  RegisterUnit('UTF8Process', @UTF8Process.Register); 
  RegisterUnit('XMLPropStorage', @XMLPropStorage.Register); 
  RegisterUnit('ActnList', @ActnList.Register); 
  RegisterUnit('Arrow', @Arrow.Register); 
  RegisterUnit('AsyncProcess', @AsyncProcess.Register); 
  RegisterUnit('ButtonPanel', @ButtonPanel.Register); 
  RegisterUnit('Buttons', @Buttons.Register); 
  RegisterUnit('Calendar', @Calendar.Register); 
end; 

initialization
  RegisterPackage('LCLBase', @Register); 
end.
