{
  Abstract:
    This unit registers all LCL components in a predefined order.
    It is called by ide/formeditor.pp
}
unit RegisterLCL;

{$mode objfpc}{$H+}

interface

uses
  LazarusPackageIntf, Menus, Buttons, StdCtrls, ExtCtrls, ComCtrls, MaskEdit,
  CheckLst, Forms, Grids, Controls, Dialogs, Spin, Arrow, Calendar,
  PairSplitter, ExtDlgs, StdActns, DbCtrls, DBGrids, DBActns, EditBtn,
  ActnList, FileCtrl, Graphics, XMLPropStorage, IniPropStorage, BarChart,
  ColorBox, ButtonPanel, LResources, LazHelpHTML, PopupNotifier, AsyncProcess,
  UTF8Process, ShellCtrls, ValEdit;

procedure Register;
procedure RegisterLCLBase;

implementation

procedure Register;
begin
  // register manually to get a predefined order in the component palette
  RegisterUnit('Menus',@Menus.Register);
  RegisterUnit('Buttons',@Buttons.Register);
  RegisterUnit('StdCtrls',@StdCtrls.Register);
  RegisterUnit('ExtCtrls',@ExtCtrls.Register);
  RegisterUnit('ComCtrls',@ComCtrls.Register);
  RegisterUnit('MaskEdit',@MaskEdit.Register);
  RegisterUnit('CheckLst',@CheckLst.Register);
  RegisterUnit('Forms',@Forms.Register);
  RegisterUnit('Grids',@Grids.Register);
  RegisterUnit('Controls',@Controls.Register);
  RegisterUnit('Dialogs',@Dialogs.Register);
  RegisterUnit('Spin',@Spin.Register);
  RegisterUnit('Arrow',@Arrow.Register);
  RegisterUnit('Calendar',@Calendar.Register);
  RegisterUnit('PairSplitter',@PairSplitter.Register);
  RegisterUnit('ExtDlgs',@ExtDlgs.Register);
  RegisterUnit('StdActns',@StdActns.Register);
  RegisterUnit('DBCtrls',@DBCtrls.Register);
  RegisterUnit('DBGrids',@DBGrids.Register);
  RegisterUnit('DBActns',@DBActns.Register);
  RegisterUnit('EditBtn',@EditBtn.Register);
  RegisterUnit('ActnList',@ActnList.Register);
  RegisterUnit('FileCtrl',@FileCtrl.Register);
  RegisterUnit('Graphics',@Graphics.Register);
  RegisterUnit('XMLPropStorage',@XMLPropStorage.Register);
  RegisterUnit('IniPropStorage',@IniPropStorage.Register);
  RegisterUnit('BarChart',@BarChart.Register);
  RegisterUnit('ColorBox',@ColorBox.Register);
  RegisterUnit('ButtonPanel',@ButtonPanel.Register);
  RegisterUnit('LResources',@LResources.Register);
  RegisterUnit('LazHelpHTML',@LazHelpHTML.Register);
  RegisterUnit('PopupNotifier', @PopupNotifier.Register);
  RegisterUnit('AsyncProcess', @AsyncProcess.Register);
  RegisterUnit('UTF8Process', @UTF8Process.Register);
  RegisterUnit('ShellCtrls', @ShellCtrls.Register);
  RegisterUnit('ValEdit', @ValEdit.Register);
end;

procedure RegisterLCLBase;
begin
  RegisterPackage('LCLBase', @Register);
end;

end.

