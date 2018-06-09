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
unit codeexplorer_categories_options;

{$mode objfpc}{$H+}

interface

uses
  SysUtils,
  // LCL
  ExtCtrls,
  // IdeIntf
  IDEOptionsIntf, IDEOptEditorIntf,
  // IDE
  LazarusIDEStrConsts, CodeExplOpts;

type

  { TCodeExplorerCategoriesOptionsFrame }

  TCodeExplorerCategoriesOptionsFrame = class(TAbstractIDEOptionsEditor)
    CategoriesCheckGroup: TCheckGroup;
  public
    function GetTitle: String; override;
    procedure Setup({%H-}ADialog: TAbstractOptionsEditorDialog); override;
    procedure ReadSettings(AOptions: TAbstractIDEOptions); override;
    procedure WriteSettings(AOptions: TAbstractIDEOptions); override;
    class function SupportedOptionsClass: TAbstractIDEOptionsClass; override;
  end;

implementation

{$R *.lfm}

{ TCodeExplorerCategoriesOptionsFrame }

function TCodeExplorerCategoriesOptionsFrame.GetTitle: String;
begin
  Result := lisCECategories;
end;

procedure TCodeExplorerCategoriesOptionsFrame.Setup(
  ADialog: TAbstractOptionsEditorDialog);
var
  c: TCodeExplorerCategory;
begin
  CategoriesCheckGroup.Caption := lisCEOnlyUsedInCategoryMode;
  for c := FirstCodeExplorerCategory to high(TCodeExplorerCategory) do
    CategoriesCheckGroup.Items.Add(CodeExplorerLocalizedString(c));
end;

procedure TCodeExplorerCategoriesOptionsFrame.ReadSettings(
  AOptions: TAbstractIDEOptions);
var
  c: TCodeExplorerCategory;
begin
  with AOptions as TCodeExplorerOptions do
    for c := FirstCodeExplorerCategory to high(TCodeExplorerCategory) do
      CategoriesCheckGroup.Checked[ord(c) - 1] := c in Categories;
end;

procedure TCodeExplorerCategoriesOptionsFrame.WriteSettings(
  AOptions: TAbstractIDEOptions);
var
  NewCategories: TCodeExplorerCategories;
  c: TCodeExplorerCategory;
begin
  NewCategories:=[];
  for c := FirstCodeExplorerCategory to high(TCodeExplorerCategory) do
    if CategoriesCheckGroup.Checked[ord(c) - 1] then
      Include(NewCategories, c);
  with AOptions as TCodeExplorerOptions do
    Categories := NewCategories;
end;

class function TCodeExplorerCategoriesOptionsFrame.SupportedOptionsClass: TAbstractIDEOptionsClass;
begin
  Result := TCodeExplorerOptions;
end;

initialization
  RegisterIDEOptionsEditor(GroupCodeExplorer, TCodeExplorerCategoriesOptionsFrame, cdeOptionsCategories);

end.

