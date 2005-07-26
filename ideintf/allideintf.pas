{ Copyright (C) 2004

 *****************************************************************************
 *                                                                           *
 *  See the file COPYING.modifiedLGPL, included in this distribution,        *
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
  IDECommands, PropEdits, ObjInspStrConsts, ObjectInspector, ColumnDlg,
  ComponentEditors, GraphPropEdits, DBPropEdits, ListViewPropEdit,
  ImageListEditor, ComponentTreeView, ActionsEditor, HelpIntf, TextTools,
  FormEditingIntf, SrcEditorIntf, ComponentReg, PackageIntf, HelpHTML,
  FieldsEditor, ConfigStorage, HelpFPDoc, ProjectIntf, LazIDEIntf, NewItemIntf,
  MacroIntf, MenuIntf;

implementation

end.

