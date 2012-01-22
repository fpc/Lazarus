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
 *   Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.        *
 *                                                                         *
 ***************************************************************************
}
(*
Modified by Gerard Visent <gerardusmercator@gmail.com> on 5/11/2007
- Extended to allow adding Owner, Category and priority
*)
unit ToDoDlg;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LCLProc, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, TodoList, ToDoListStrConsts, Buttons, ButtonPanel, Menus, MenuIntf,
  PackageIntf, SrcEditorIntf, IDECommands, LCLType, IDEWindowIntf, LazIDEIntf;

type

  { TTodoDialog }

  TTodoDialog = class(TForm)
    BtnPanel: TButtonPanel;
    OwnerEdit: TEdit;
    CategoryEdit: TEdit;
    CategoryLabel: TLabel;
    PriorityEdit: TEdit;
    PriorityLabel: TLabel;
    OwnerLabel: TLabel;
    TodoLabel: TLabel;
    TodoMemo: TMemo;
    procedure FormCreate(Sender: TObject);
    procedure PriorityEditKeyPress(Sender: TObject; var Key: char);
  private

  public

  end;
  
var
  InsertToDoCmd: TIDECommand;
  ViewToDoListCmd: TIDECommand;

function ExecuteTodoDialog: TTodoItem;

procedure Register;
procedure InsertToDoForActiveSourceEditor(Sender: TObject);
procedure ViewToDoList(Sender: TObject);
procedure CreateIDEToDoWindow(Sender: TObject; aFormName: string;
                          var AForm: TCustomForm; DoDisableAutoSizing: boolean);

implementation

{$R *.lfm}

procedure Register;
var
  Key: TIDEShortCut;
  Cat: TIDECommandCategory;
begin
  // mattias: move icon resource item_todo to package
  // mattias: add menu item to package editor
  // mattias: test short cut

  // register shortcut for insert todo
  Key := IDEShortCut(VK_T,[ssCtrl,ssShift],VK_UNKNOWN,[]);
  Cat:=IDECommandList.FindCategoryByName(CommandCategoryTextEditingName);
  InsertToDoCmd:=RegisterIDECommand(Cat, 'Insert ToDo', lisTDDInsertToDo,Key,
    nil,@InsertToDoForActiveSourceEditor);

  // add a menu item in the source editor
  RegisterIDEMenuCommand(SrcEditMenuSectionFirstStatic, 'InsertToDo',
    lisTDDInsertToDo,nil,nil,InsertToDoCmd,'item_todo');
  // add a menu item in the Edit / Insert Text section
  RegisterIDEMenuCommand(itmSourceInsertions,'itmSourceInsertTodo',lisTDDInsertToDo,
    nil,nil,InsertToDoCmd,'item_todo');

  // register shortcut for view todo list
  Key := IDEShortCut(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  Cat:=IDECommandList.FindCategoryByName(CommandCategoryViewName);
  ViewToDoListCmd:=RegisterIDECommand(Cat, 'View ToDo list', lisViewToDoList,
    Key,nil,@ViewToDoList);

  // add a menu item in the view menu
  RegisterIDEMenuCommand(itmViewMainWindows, 'ViewToDoList',
    lisToDoList, nil, nil, ViewToDoListCmd, 'item_todo');

  // add a menu item in the package editor
  RegisterIDEMenuCommand(PkgEditMenuSectionMisc, 'ViewPkgToDoList',
    lisToDoList, nil, nil, ViewToDoListCmd, 'item_todo');

  // register window creator
  IDEWindowCreators.Add(ToDoWindowName,@CreateIDEToDoWindow,nil,'250','250','','');
end;

procedure InsertToDoForActiveSourceEditor(Sender: TObject);
var
  SrcEdit: TSourceEditorInterface;
  aTodoItem: TTodoItem;
begin
  SrcEdit:=SourceEditorManagerIntf.ActiveEditor;
  if SrcEdit=nil then exit;
  if SrcEdit.ReadOnly then exit;
  aTodoItem := ExecuteTodoDialog;
  try
    if Assigned(aTodoItem) then
      SrcEdit.Selection:=aTodoItem.AsComment;
  finally
    aTodoItem.Free;
  end;
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
  if CompareText(aFormName,ToDoWindowName)<>0 then exit;
  IDEWindowCreators.CreateForm(IDETodoWindow,TIDETodoWindow,DoDisableAutoSizing,
                               LazarusIDE.OwningComponent);
  AForm:=IDETodoWindow;
end;

{ TTodoDialog }

procedure TTodoDialog.PriorityEditKeyPress(Sender: TObject; var Key: char);
begin
  if not (Key in ['0'..'9']) then
    Key := #0;
end;

procedure TTodoDialog.FormCreate(Sender: TObject);
begin
  ActiveControl:=TodoMemo;
  Caption:=lisTDDInsertToDo;
  TodoLabel.Caption:=lisPkgFileTypeText;
  PriorityLabel.Caption:=lisToDoLPriority;
  OwnerLabel.Caption:=lisToDoLOwner;
  CategoryLabel.Caption:=listToDoLCategory;
end;

function ExecuteTodoDialog: TTodoItem;
var
  aTodoDialog: TTodoDialog;
  aPriority: integer;
begin
  Result := nil;
  aTodoDialog := TTodoDialog.Create(nil);
  aTodoDialog.ShowModal;
  if aTodoDialog.ModalResult = mrOk then
  begin
    Result := TTodoItem.Create(nil);
    Result.AltNotation := True; // TODO: Should be an option in the future
    Result.Category    := aTodoDialog.CategoryEdit.Text;
    Result.Done        := False;
    Result.Owner       := aTodoDialog.OwnerEdit.Text;
    Result.Text        := aTodoDialog.TodoMemo.Text;
    if TryStrToInt(aTodoDialog.PriorityEdit.Text, aPriority) then
      Result.Priority  := aPriority;
  end;
  aTodoDialog.Free;
end;

end.

