{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit IDEIntf;

interface

uses
  ActionsEditor, ActionsEditorStd, BaseIDEIntf, CheckGroupEditorDlg, 
  CheckListboxEditorDlg, CollectionPropEditForm, ColumnDlg, ComponentEditors, 
  ComponentReg, ComponentTreeView, DBPropEdits, fieldseditor, fieldslist, 
  FormEditingIntf, frmSelectProps, GraphicPropEdit, GraphPropEdits, 
  HeaderControlPropEdit, HelpFPDoc, IDECommands, IDEDialogs, 
  IDEExternToolIntf, IDEHelpIntf, IDEImagesIntf, IDEMsgIntf, IDEOptionsIntf, 
  IDETextConverter, IDEWindowIntf, ImageListEditor, LazIDEIntf, 
  LazStringGridEdit, ListViewPropEdit, MacroIntf, MaskPropEdit, MenuIntf, 
  newfield, NewItemIntf, ObjectInspector, ObjInspStrConsts, PackageIntf, 
  ProjectIntf, ProjectResourcesIntf, PropEdits, PropEditUtils, SrcEditorIntf, 
  StatusBarPropEdit, StringsPropEditDlg, TextTools, TreeViewPropEdit, 
  CompOptsIntf, OIFavoriteProperties, UnitResources, MacroDefIntf, 
  FileFilterPropEditor, IDEUtils, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('IDEWindowIntf', @IDEWindowIntf.Register);
end;

initialization
  RegisterPackage('IDEIntf', @Register);
end.
