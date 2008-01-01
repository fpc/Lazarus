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
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, StdCtrls, TodoList;

type

  { TTodoDialog }

  TTodoDialog = class(TForm)
    OwnerEdit: TEdit;
    CategoryEdit: TEdit;
    CategoryLabel: TLabel;
    PriorityEdit: TEdit;
    PriorityLabel: TLabel;
    OwnerLabel: TLabel;
    OkButton: TButton;
    CancelButton: TButton;
    TodoLabel: TLabel;
    TodoMemo: TMemo;
    procedure FormCreate(Sender: TObject);
    procedure PriorityEditKeyPress(Sender: TObject; var Key: char);
  private
    { private declarations }
  public
    { public declarations }
  end;
  
Function ExecuteTodoDialog: TTodoItem;


implementation

{ TTodoDialog }

procedure TTodoDialog.PriorityEditKeyPress(Sender: TObject; var Key: char);
begin
  if not (Key in ['0'..'9']) then
    Key := #0;
end;

procedure TTodoDialog.FormCreate(Sender: TObject);
begin
  ActiveControl:=TodoMemo;
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
    Result := TTodoItem.Create;
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

initialization
  {$I tododlg.lrs}

end.

