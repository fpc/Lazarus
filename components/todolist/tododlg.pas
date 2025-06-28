{
 ***************************************************************************
 *                                                                         *
 *   This source is free software; you can redistribute it and/or modify   *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 *   This code is distributed in the hope that it will be useful, but      *
 *   WITHOUT ANY WARRANTY; without even the implied warranty of            *
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU     *
 *   General Public License for more details.                              *
 *                                                                         *
 *   A copy of the GNU General Public License is available on the World    *
 *   Wide Web at <http://www.gnu.org/copyleft/gpl.html>. You can also      *
 *   obtain it by writing to the Free Software Foundation,                 *
 *   Inc., 51 Franklin Street - Fifth Floor, Boston, MA 02110-1335, USA.   *
 *                                                                         *
 ***************************************************************************
}
(*
Modified by Gerard Visent <gerardusmercator@gmail.com> on 5/11/2007
- Extended to allow adding Owner, Category and priority
Modified by Kevin Jesshope <KevinOfOz@gmail.com> 15 Mar 2020
- See ToDoListCore for details
*)

unit ToDoDlg;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  // LCL
  LCLType, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  Buttons, ButtonPanel, Menus, Spin, XMLPropStorage,
  // LazUtils
  LazFileUtils,
  // BuildIntf
  PackageIntf,
  // IdeIntf
  IDECommands, MenuIntf, ToolBarIntf, SrcEditorIntf, IDEWindowIntf, LazIDEIntf,
  // TodoList
  ToDoList, ToDoListStrConsts, ToDoListCore;

type

  { TTodoDialog }

  TTodoDialog = class(TForm)
    Bevel1: TBevel;
    Bevel2: TBevel;
    BtnPanel: TButtonPanel;
    chkAlternateTokens: TCheckBox;
    grpboxToDoType: TGroupBox;
    OwnerEdit: TEdit;
    CategoryEdit: TEdit;
    CategoryLabel: TLabel;
    PriorityEdit: TSpinEdit;
    PriorityLabel: TLabel;
    OwnerLabel: TLabel;
    rdoToDo: TRadioButton;
    rdoDone: TRadioButton;
    rdoNote: TRadioButton;
    TodoLabel: TLabel;
    TodoMemo: TMemo;
    XMLPropStorage: TXMLPropStorage;
    procedure FormCloseQuery(Sender: TObject; var {%H-}CanClose: Boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure rdoToDoTypeChange(Sender: TObject);
  end;
  
var
  InsertToDoCmd: TIDECommand;
  ViewToDoListCmd: TIDECommand;

procedure Register;
// ToDo List
procedure ViewToDoList(Sender: TObject);
procedure CreateIDEToDoWindow(Sender: TObject; aFormName: string;
                          var AForm: TCustomForm; DoDisableAutoSizing: boolean);
// ToDo Dialog
function ExecuteTodoDialog(const aCaption: string; var aTodoItem: TTodoItem): TModalResult;
procedure InsertToDoForActiveSE;
procedure EditToDo(aTodoItem: TTodoItem);
procedure InsertOrEditToDo(Sender: TObject);


implementation

{$R *.lfm}

const
  DefaultTodoListCfgFile = 'todolistdialogoptions.xml';

procedure Register;
var
  Key: TIDEShortCut;
  Cat: TIDECommandCategory;
  MenuCmd: TIDEMenuCommand;
  ButtonCmd: TIDEButtonCommand;
begin
  // mattias: move icon resource item_todo to package
  // mattias: add menu item to package editor
  // mattias: test short cut

  // register shortcut for insert todo
  Key := IDEShortCut(VK_T,[ssCtrl,ssShift],VK_UNKNOWN,[]);
  Cat:=IDECommandList.FindCategoryByName(CommandCategoryTextEditingName);
  InsertToDoCmd:=RegisterIDECommand(Cat, 'InsertToDo', lisTDDInsertToDo,Key,
    nil,@InsertOrEditToDo);

  // add a menu item in the source editor
  SrcEditMenuCmd:=RegisterIDEMenuCommand(SrcEditMenuSectionFirstStatic, 'InsertToDo',
    lisTDDInsertToDo,nil,nil,InsertToDoCmd,'item_todo');
  // add a menu item in the Edit / Insert Text section
  IdeSourceMenuCmd:=RegisterIDEMenuCommand(itmSourceInsertions,'itmSourceInsertTodo',
    lisTDDInsertToDo,nil,nil,InsertToDoCmd,'item_todo');
  ButtonCmd:=RegisterIDEButtonCommand(InsertToDoCmd);    // toolbutton
  ButtonCmd.ImageIndex:=IdeSourceMenuCmd.ImageIndex;

  // register shortcut for view todo list
  Key := IDEShortCut(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  Cat:=IDECommandList.FindCategoryByName(CommandCategoryViewName);
  ViewToDoListCmd:=RegisterIDECommand(Cat, 'View ToDo list', lisViewToDoList,
    Key,nil,@ViewToDoList);

  // add a menu item in the view menu
  MenuCmd:=RegisterIDEMenuCommand(itmViewMainWindows, 'ViewToDoList',
    lisToDoList, nil, nil, ViewToDoListCmd, 'menu_view_todo');
  ButtonCmd:=RegisterIDEButtonCommand(ViewToDoListCmd);    // toolbutton
  ButtonCmd.ImageIndex:=MenuCmd.ImageIndex;

  // add a menu item in the package editor
  RegisterIDEMenuCommand(PkgEditMenuSectionMisc, 'ViewPkgToDoList',
    lisToDoList, nil, nil, ViewToDoListCmd, 'menu_view_todo');

  // register window creator
  IDEWindowCreators.Add(ToDoWindowName,@CreateIDEToDoWindow,nil,'250','250','','');
end;

procedure ViewToDoList(Sender: TObject);
var
  Pkg: TIDEPackage;
begin
  IDEWindowCreators.ShowForm(ToDoWindowName,true);
  if IDETodoWindow<>nil then begin
    Pkg:=PackageEditingInterface.GetPackageOfEditorItem(Sender);
    if Pkg<>nil then
      IDETodoWindow.IDEItem:=Pkg.Name
    else
      IDETodoWindow.IDEItem:='';
  end;
end;

procedure CreateIDEToDoWindow(Sender: TObject; aFormName: string;
  var AForm: TCustomForm; DoDisableAutoSizing: boolean);
begin
  Assert(aFormName=ToDoWindowName, 'CreateIDEToDoWindow: aFormName<>ToDoWindowName');
  IDEWindowCreators.CreateForm(IDETodoWindow,TIDETodoWindow,DoDisableAutoSizing,
                               LazarusIDE.OwningComponent);
  IDETodoWindow.OnEditItem:=@EditToDo;  // Edit the selected ToDo item.
  AForm:=IDETodoWindow;
end;

function ExecuteTodoDialog(const aCaption: string; var aTodoItem: TTodoItem): TModalResult;
var
  aTodoDialog: TTodoDialog;
begin
  aTodoDialog := TTodoDialog.Create(nil);
  aTodoDialog.Caption:=aCaption;
  if Assigned(aTodoItem) then begin
    aTodoDialog.CategoryEdit.Text  := aTodoItem.Category;
    aTodoDialog.grpboxToDoType.Tag := PtrInt(aTodoItem.ToDoType);
    case aTodoItem.ToDoType of
      tdToDo: aTodoDialog.rdoToDo.Checked := True;
      tdDone: aTodoDialog.rdoDone.Checked := True;
      tdNote: aTodoDialog.rdoNote.Checked := True;
    end;
    aTodoDialog.chkAlternateTokens.Checked := aTodoItem.TokenStyle=tsAlternate;
    aTodoDialog.OwnerEdit.Text     := aTodoItem.Owner;
    aTodoDialog.TodoMemo.Text      := aTodoItem.Text;
    aTodoDialog.PriorityEdit.Value := aTodoItem.Priority;
  end;
  aTodoDialog.ShowModal;
  Result := aTodoDialog.ModalResult;
  if Result = mrOk then
  begin
    if aTodoItem=nil then begin
      aTodoItem := TTodoItem.Create;
      aTodoItem.CommentType := '{';
      aTodoItem.HasColon := True;
    end;
    aTodoItem.Category    := aTodoDialog.CategoryEdit.Text;
    aTodoItem.ToDoType    := TToDoType(aTodoDialog.grpboxToDoType.Tag);
    if aTodoDialog.chkAlternateTokens.Checked then
      aTodoItem.TokenStyle:=tsAlternate
    else
      aTodoItem.TokenStyle:=tsNormal;
    aTodoItem.Owner       := aTodoDialog.OwnerEdit.Text;
    aTodoItem.Text        := aTodoDialog.TodoMemo.Text;
    aTodoItem.Priority    := aTodoDialog.PriorityEdit.Value;
  end;
  aTodoDialog.Free;
end;

procedure InsertToDoForActiveSE;
var
  SrcEdit: TSourceEditorInterface;
  TodoItem: TTodoItem;
begin
  SrcEdit := SourceEditorManagerIntf.ActiveEditor;
  if (SrcEdit=nil) or SrcEdit.ReadOnly then exit;
  TodoItem := nil;
  if ExecuteTodoDialog(lisTDDInsertToDo, TodoItem) <> mrOK then exit;
  try
    if Assigned(TodoItem) then
      SrcEdit.Selection := TodoItem.AsComment;
  finally
    TodoItem.Free;
  end;
  IDETodoWindow.UpdateTodos;
end;

procedure EditToDo(aTodoItem: TTodoItem);
var
  SrcEdit: TSourceEditorInterface;
  EndPos: TPoint;
begin
  SrcEdit := SourceEditorManagerIntf.ActiveEditor;
  if (SrcEdit=nil) or SrcEdit.ReadOnly then exit;
  if ExecuteTodoDialog(lisTDDEditToDo, aTodoItem) <> mrOK then exit;
  EndPos.X := aTodoItem.CodePos.X + aTodoItem.CommentLen;
  EndPos.Y := aTodoItem.CodePos.Y;  // ToDo -oJuha : Deal with multiline comments.
  SrcEdit.SelectText(aTodoItem.CodePos, EndPos);
  SrcEdit.Selection := aTodoItem.AsComment;
  IDETodoWindow.UpdateTodos;  { TODO -oJuha : Retain selection in the list. }
end;

procedure InsertOrEditToDo(Sender: TObject);
begin
  if Assigned(IDETodoWindow) and Assigned(IDETodoWindow.TodoItemToEdit) then
    EditToDo(IDETodoWindow.TodoItemToEdit)
  else
    InsertToDoForActiveSE
end;

{ TTodoDialog }

procedure TTodoDialog.FormCreate(Sender: TObject);
begin
  ActiveControl:=TodoMemo;
  TodoLabel.Caption:=lisPkgFileTypeText;
  PriorityLabel.Caption:=lisToDoLPriority;
  OwnerLabel.Caption:=lisToDoLOwner;
  CategoryLabel.Caption:=listToDoLCategory;
  grpboxToDoType.Caption:=lisToDoToDoType;
  grpboxToDoType.Tag:=Ord(tdToDo);
  rdoToDo.Tag:=Ord(tdToDo);
  rdoDone.Tag:=Ord(tdDone);
  rdoNote.Tag:=Ord(tdNote);
  chkAlternateTokens.Caption:=lisAlternateTokens;
  chkAlternateTokens.Hint:=lisAlternateTokensHint;
  XMLPropStorage.FileName:=Concat(AppendPathDelim(LazarusIDE.GetPrimaryConfigPath),
    DefaultTodoListCfgFile);
  XMLPropStorage.Active := True;
end;

procedure TTodoDialog.FormShow(Sender: TObject);
begin
  TodoMemo.Constraints.MinHeight := OwnerEdit.Height;
  Constraints.MinHeight := ToDoMemo.Top + ToDoMemo.Constraints.MinHeight +
    ClientHeight - PriorityLabel.Top + PriorityLabel.BorderSpacing.Top;
  // Constraints.MinWidth is set in Object Inspector for LCL scaling

  // Enforce constraints
  if Width < Constraints.MinWidth then Width := 0;
  if Height < Constraints.MinHeight then Height := 0;
end;

procedure TTodoDialog.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  XMLPropStorage.Save;
end;

procedure TTodoDialog.rdoToDoTypeChange(Sender: TObject);
var
  lRadioButton: TRadioButton;
  lGroupBox: TGroupBox;
begin
  lRadioButton := Sender as TRadioButton;
  lGroupBox := lRadioButton.Parent as TGroupBox;
  lGroupBox.Tag := lRadioButton.Tag;
end;

end.

