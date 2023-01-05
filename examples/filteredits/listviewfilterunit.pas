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
unit ListViewFilterUnit;

{$mode objfpc}{$H+}

{ An Example project to demonstrate use of the ListViewFilterEdit
component, a simple means to filter ListView contents.


Content to be displayed in the ListView must be loaded via the ListViewFilterEdit
control. Although not demonstrated here, its possible to include an object with
each row, much the same as can be done with ListView.


There does not appear to be a way to define an external search method. Therefore
you can search only the content actually loaded into the TListViewFilterEdit.

Do not use a ListViewFilterEdit with a ListView in Owner Data mode.

FilterOptions property should be used to make filtering case sensitive or not.

Following changes were made in the Object Inspector -
    ListView1.ViewStyle := vsReport;
    ListViewFilterEdit1.FilteredListview := ListView1;
    ListViewFilterEdit1.TextHint := 'Search Here';
    ListViewFilterEdit1.CharCase := ecNormal;
    Create two columns in the ListView, set first one to be autowidth


David Bannon, 2022-12-07

}
interface

uses
    Forms, Controls, StdCtrls, ComCtrls,
    ListViewFilterEdit, EditBtn;

type

    { TListViewFilterForm }

    TListViewFilterForm = class(TForm)
        ButtonProgram: TButton;
        CheckBothColumns: TCheckBox;
        CheckCaseSensitive: TCheckBox;
        Label1: TLabel;
        ListView1: TListView;
        ListViewFilterEdit1: TListViewFilterEdit;
        procedure ButtonProgramClick(Sender: TObject);
        procedure CheckBothColumnsChange(Sender: TObject);
        procedure CheckCaseSensitiveChange(Sender: TObject);
        procedure FormCreate(Sender: TObject);
    private
        procedure AddLVItem(St1, St2: string);
    public

    end;

var
    ListViewFilterForm: TListViewFilterForm;

implementation

{$R *.lfm}

procedure TListViewFilterForm.AddLVItem(St1, St2 : string);
var
    ListItem: TListViewDataItem;
begin
    ListItem.Data := Nil;
    SetLength(ListItem.StringArray, 2);
    ListItem.StringArray[0] := St1;
    ListItem.StringArray[1] := St2;
    {%H-}ListViewFilterEdit1.Items.Add(ListItem);
end;

procedure TListViewFilterForm.FormCreate(Sender: TObject);
begin
    AddLVItem('String1',  'String2');
    AddLVItem('String3',  'String4');
    AddLVItem('A String', 'another String');
    AddLVItem('x string', 'Extra String');
    ListViewFilterEdit1.ResetFilter;
end;

procedure TListViewFilterForm.ButtonProgramClick(Sender: TObject);
begin
    ListViewFilterEdit1.Filter := 'x';        // Filter for strings containing x
end;

procedure TListViewFilterForm.CheckBothColumnsChange(Sender: TObject);
begin
    ListViewFilterEdit1.ByAllFields := CheckBothColumns.Checked;
end;

procedure TListViewFilterForm.CheckCaseSensitiveChange(Sender: TObject);
begin
    with ListViewFilterEdit1 do
        if CheckCaseSensitive.checked then
            FilterOptions := FilterOptions + [fsoCaseSensitive]
        else
            FilterOptions := FilterOptions - [fsoCaseSensitive];
end;

end.

