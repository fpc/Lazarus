{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit IDEIntf;

{$warn 5023 off : no warning about unused units}
interface

uses
  ActionsEditor, ActionsEditorStd, ChangeParentDlg, CheckGroupEditorDlg, 
  CheckListboxEditorDlg, CollectionPropEditForm, ColumnDlg, ComponentEditors, 
  ComponentReg, ComponentTreeView, DBGridColumnsPropEditForm, DBPropEdits, 
  EditorSyntaxHighlighterDef, fieldseditor, FieldsList, FileFilterPropEditor, 
  FormEditingIntf, frmSelectProps, GraphicPropEdit, GraphPropEdits, 
  HeaderControlPropEdit, HelpFPDoc, IDECommands, IDEDialogs, IDEHelpIntf, 
  IDEImagesIntf, IDEMsgIntf, IDEOptEditorIntf, IDETextConverter, IDEUtils, 
  IDEWindowIntf, ImageListEditor, KeyValPropEditDlg, LazIDEIntf, 
  LazStringGridEdit, ListViewPropEdit, MaskPropEdit, MenuIntf, NewField, 
  ObjectInspector, ObjInspStrConsts, OIFavoriteProperties, ProjectGroupIntf, 
  PropEdits, PropEditUtils, SrcEditorIntf, StatusBarPropEdit, 
  StringsPropEditDlg, TextTools, ToolBarIntf, TreeViewPropEdit, UnitResources, 
  LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('IDEWindowIntf', @IDEWindowIntf.Register);
end;

initialization
  RegisterPackage('IDEIntf', @Register);
end.
