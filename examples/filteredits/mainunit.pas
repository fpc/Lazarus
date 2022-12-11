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
unit MainUnit;

{$mode objfpc}{$H+}

{
An Example project to demonstrate use of FilterEdit components.
They provide a simple means to filter ListBox, ListView and TreeView contents.

David Bannon, Juha Manninen

}
interface

uses
    Forms, Controls, StdCtrls,
    ListFilterUnit, ListViewFilterUnit, TreeFilterUnit;

type

    { TMainForm }

    TMainForm = class(TForm)
        ListFilterEditButton: TButton;
        ListViewFilterEditButton: TButton;
        TreeFilterEditButton: TButton;
        Label1: TLabel;
        procedure FormCreate(Sender: TObject);
        procedure ListFilterEditButtonClick(Sender: TObject);
        procedure ListViewFilterEditButtonClick(Sender: TObject);
        procedure TreeFilterEditButtonClick(Sender: TObject);
    private

    public

    end;

var
    MainForm: TMainForm;

implementation

{$R *.lfm}

procedure TMainForm.FormCreate(Sender: TObject);
begin
  ;
end;

procedure TMainForm.ListFilterEditButtonClick(Sender: TObject);
begin
  ListFilterForm.Show;
end;

procedure TMainForm.ListViewFilterEditButtonClick(Sender: TObject);
begin
  ListViewFilterForm.Show;
end;

procedure TMainForm.TreeFilterEditButtonClick(Sender: TObject);
begin
  TreeFilterForm.Show;
end;

end.

