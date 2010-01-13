{ Copyright (C) 2004

 *****************************************************************************
 *                                                                           *
 *  See the file COPYING.modifiedLGPL.txt, included in this distribution,    *
 *  for details about the copyright.                                         *
 *                                                                           *
 *  This program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
 *                                                                           *
 *****************************************************************************
 
  Dummy unit to compile all IDE interface units.
}
unit AllIDEIntf;

{$mode objfpc}{$H+}

interface

uses
  ActionsEditor,
  ActionsEditorStd,
  BaseIDEIntf,
  CollectionPropEditForm,
  ComponentEditors,
  ComponentReg,
  ComponentTreeView,
  DBPropEdits,
  FieldsEditor,
  FormEditingIntf,
  FrmSelectProps,
  GraphPropEdits,
  HelpFPDoc,
  IDECommands,
  IDEDialogs,
  IDEExternToolIntf,
  IDEHelpIntf,
  IDEImagesIntf,
  IDEMsgIntf,
  IDEOptionsIntf,
  IDETextConverter,
  IDEWindowIntf,
  ImageListEditor,
  LazIDEIntf,
  ListViewPropEdit,
  HeaderControlPropEdit,
  MacroIntf,
  MaskPropEdit,
  MenuIntf,
  NewItemIntf,
  ObjectInspector,
  OIFavouriteProperties,
  ObjInspStrConsts,
  PackageIntf,
  ProjectIntf,
  ProjectResourcesIntf,
  PropEdits,
  SrcEditorIntf,
  StatusBarPropEdit,
  StringsPropEditDlg,
  LazStringGridEdit,
  TextTools,
  TreeViewPropEdit;

implementation

end.

