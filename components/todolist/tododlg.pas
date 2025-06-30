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
  LazFileUtils, LazLoggerBase,
  // BuildIntf
  PackageIntf,
  // IdeIntf
  IDECommands, MenuIntf, ToolBarIntf, SrcEditorIntf, IDEWindowIntf, LazIDEIntf,
  SynEdit,
  // TodoList
  ToDoList, ToDoListStrConsts, ToDoListCore, TodoSynMarkup;

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
  SrcEditMenuCmd: TIDEMenuCommand;     // MenuItem in Source editor popup menu.
  IdeSourceMenuCmd: TIDEMenuCommand;   // MenuItem in IDE's Source menu.

procedure Register;
// ToDo List
procedure ViewToDoList(Sender: TObject);
procedure CreateIDEToDoWindow(Sender: TObject; aFormName: string;
                          var AForm: TCustomForm; DoDisableAutoSizing: boolean);
// ToDo Dialog
function ExecuteTodoDialog(const aCaption: string; var aTodoItem: TTodoItem): TModalResult;
procedure InsertToDoForActiveSE(aSrcEdit: TSourceEditorInterface);
procedure EditToDo(aTodoItem: TTodoItem; aSrcEdit: TSourceEditorInterface);
procedure InsertOrEditToDo(Sender: TObject);


implementation

{$R *.lfm}

type
  TToDoManager = class  // For an event handler that needs a class instance.
    procedure DecideMenuCaption(Sender: TObject; var ACaption, AHint: string);
  end;

const
  DefaultTodoListCfgFile = 'todolistdialogoptions.xml';

var
  ToDoManager: TToDoManager;
  TodoItemToEdit: TTodoItem;

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

  ToDoManager := TToDoManager.Create;

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
  SrcEditMenuCmd.OnRequestCaptionHint := @ToDoManager.DecideMenuCaption;
  IdeSourceMenuCmd.OnRequestCaptionHint := @ToDoManager.DecideMenuCaption;

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

procedure InsertToDoForActiveSE(aSrcEdit: TSourceEditorInterface);
var
  TodoItem: TTodoItem;
begin
  TodoItem := nil;
  if ExecuteTodoDialog(lisTDDInsertToDo, TodoItem) <> mrOK then exit;
  try
    if Assigned(TodoItem) then
      aSrcEdit.Selection := TodoItem.AsComment;
  finally
    TodoItem.Free;
  end;
  if Assigned(IDETodoWindow) then
    IDETodoWindow.UpdateTodos;
end;

procedure EditToDo(aTodoItem: TTodoItem; aSrcEdit: TSourceEditorInterface);
begin
  if ExecuteTodoDialog(lisTDDEditToDo, aTodoItem) <> mrOK then exit;
  aSrcEdit.SelectText(aTodoItem.StartPos, aTodoItem.EndPos);
  aSrcEdit.Selection := aTodoItem.AsComment;
  if aTodoItem.Temporary then
    aTodoItem.Free;
  if Assigned(IDETodoWindow) then
    IDETodoWindow.UpdateTodos;  { TODO -oJuha : Retain selection in the list. }
end;

procedure InsertOrEditToDo(Sender: TObject);
var
  SrcEdit: TSourceEditorInterface;
begin
  SrcEdit := SourceEditorManagerIntf.ActiveEditor;
  if (SrcEdit=nil) or SrcEdit.ReadOnly then exit;
  if Assigned(TodoItemToEdit) then
    EditToDo(TodoItemToEdit, SrcEdit)
  else
    InsertToDoForActiveSE(SrcEdit)
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

{ TToDoManager }

procedure TToDoManager.DecideMenuCaption(Sender: TObject; var ACaption, AHint: string);
var
  SrcEdit: TSourceEditorInterface;
  SrcPos: TPoint;
  SynEd: TSynEdit;
  MarkUp: TSynEditTodoMarkup;
  InToDo: Boolean;
  ToDoLoc: TFoundTodo;
  Line: String;
begin
  TodoItemToEdit := nil;
  ACaption := lisTDDInsertToDo;
  SrcEditMenuCmd.Enabled := True;
  IdeSourceMenuCmd.Enabled := True;
  SrcEdit := SourceEditorManagerIntf.ActiveEditor;
  if SrcEdit=nil then exit;
  SrcEditMenuCmd.Enabled := not SrcEdit.ReadOnly;
  IdeSourceMenuCmd.Enabled := not SrcEdit.ReadOnly;

  SrcPos := SrcEdit.CursorTextXY;
  SynEd := SrcEdit.EditorControl as TSynEdit;
  MarkUp := SynEd.MarkupManager.MarkupByClass[TSynEditTodoMarkup] as TSynEditTodoMarkup;
  InToDo := MarkUp.CursorInsideToDo(SrcPos.X, ToDoLoc);
  debugln(['DecideMenuCaption: ToDo start=', ToDoLoc.StartPos.X,':',ToDoLoc.StartPos.Y,
           ', ToDo end=', ToDoLoc.EndPos.X,':',ToDoLoc.EndPos.Y, ', Line=', SrcPos.Y]);
  if InToDo then
    with ToDoLoc do begin
      if (EndPos.X=MaxInt)
      or ((StartPos.Y<>EndPos.Y) and (EndPos.Y=SrcPos.Y) and (EndPos.X>SrcPos.X)) then
      begin
        // ToDo: Support multiline ToDo comments. Now just disable the menu items.
        SrcEditMenuCmd.Enabled := False;
        IdeSourceMenuCmd.Enabled := False;
      end
      else begin
        ACaption := lisTDDEditToDo;
        Line := SrcEdit.Lines[StartPos.Y-1];
        Line := Copy(Line, StartPos.X, EndPos.X-StartPos.X);
        // Now InsertOrEditToDo in ToDoDlg knows what to do.
        TodoItemToEdit := CreateToDoItem(Line, StartPos);
        TodoItemToEdit.Temporary := True;  // Will be freed after editing.
      end;
    end;
end;

finalization
  ToDoManager.Free;

end.

