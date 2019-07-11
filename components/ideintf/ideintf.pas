{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit IDEIntf;

{$warn 5023 off : no warning about unused units}
interface

uses
  ActionsEditor, ActionsEditorStd, BaseIDEIntf, CheckGroupEditorDlg, 
  CheckListboxEditorDlg, CollectionPropEditForm, ColumnDlg, ComponentEditors, 
  ComponentReg, ComponentTreeView, CompOptsIntf, DBPropEdits, 
  EditorSyntaxHighlighterDef, fieldseditor, FieldsList, FileFilterPropEditor, 
  FormEditingIntf, frmSelectProps, GraphicPropEdit, GraphPropEdits, 
  HeaderControlPropEdit, HelpFPDoc, IDECommands, IDEDialogs, 
  IDEExternToolIntf, IDEHelpIntf, IDEImagesIntf, IDEMsgIntf, IDEOptionsIntf, 
  IDETextConverter, IDEUtils, IDEWindowIntf, ImageListEditor, 
  KeyValPropEditDlg, LazIDEIntf, LazStringGridEdit, ListViewPropEdit, 
  MacroDefIntf, MacroIntf, MaskPropEdit, MenuIntf, NewField, NewItemIntf, 
  ObjectInspector, ObjInspStrConsts, OIFavoriteProperties, PackageIntf, 
  ProjectIntf, ProjectResourcesIntf, PropEdits, PropEditUtils, SrcEditorIntf, 
  StatusBarPropEdit, StringsPropEditDlg, TextTools, TreeViewPropEdit, 
  UnitResources, ProjPackIntf, DBGridColumnsPropEditForm, ToolBarIntf, 
  ChangeParentDlg, PackageDependencyIntf, PackageLinkIntf, FppkgIntf, 
  LazMsgDialogs, ProjectGroupIntf, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('IDEWindowIntf', @IDEWindowIntf.Register);
end;

initialization
  RegisterPackage('IDEIntf', @Register);
end.
