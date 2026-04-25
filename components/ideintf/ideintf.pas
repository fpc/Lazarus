{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit IDEIntf;

{$warn 5023 off : no warning about unused units}
interface

uses
  ActionsEditor, ActionsEditorStd, bufdatasetdsgn, ChangeParentDlg, 
  CheckGroupEditorDlg, CheckListboxEditorDlg, CollectionPropEditForm, 
  ColumnDlg, ComponentEditors, ComponentTreeView, DBGridColumnsPropEditForm, 
  DBPropEdits, EditorOptionsIntf, EditorSyntaxHighlighterDef, FieldsEditor, 
  FieldsList, FileFilterPropEditor, FormEditingIntf, frmSelectProps, 
  GraphicPropEdit, GraphPropEdits, HeaderControlPropEdit, HelpFPDoc, 
  IDECommands, IdeDebuggerValueFormatterIntf, IdeDebuggerWatchValueIntf, 
  IDEDialogs, IDEHelpIntf, IDEImagesIntf, IdeIntfStrConsts, IDEIntfUtils, 
  IDEMsgIntf, IDEOptEditorIntf, IDETextConverter, IDEUtils, IDEWindowIntf, 
  ImageListEditor, KeyValPropEditDlg, LazIDEIntf, LazStringGridEdit, 
  ListViewPropEdit, MaskPropEdit, MenuIntf, NewField, ObjectInspector, 
  ObjInspStrConsts, OIFavoriteProperties, PropEditConfig, PropEdits, 
  PropEditUtils, selectdatasetdlg, SelEdits, SrcEditorIntf, StatusBarPropEdit, 
  StringsPropEditDlg, ToolBarIntf, TreeViewPropEdit, UnitResources, 
  LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('IDEWindowIntf', @IDEWindowIntf.Register);
end;

initialization
  RegisterPackage('IDEIntf', @Register);
end.
