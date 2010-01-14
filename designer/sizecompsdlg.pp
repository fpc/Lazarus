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
 
  Author: Mattias Gaertner

  Abstract:
    Defines TSizeComponentsDialog.
}
unit SizeCompsDlg;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LCLIntf, Forms, Controls, Buttons, ExtCtrls, StdCtrls,
  ButtonPanel;

type
  { TSizeComponentsDialog }
  TSizeComponentsDialog = class(TForm)
    ButtonPanel1: TButtonPanel;
    PosLabel: TLabel;
    WidthRadioGroup: TRadioGroup;
    HeightRadioGroup: TRadioGroup;
    WidthEdit: TEdit;
    HeightEdit: TEdit;
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
  public
    constructor Create(AOwner: TComponent);  override;
  end;

function ShowSizeComponentsDialog(var HorizSizingID, FixedWidth,
  VertSizingID, FixedHeight: integer): TModalResult;

implementation

{$R *.lfm}

uses 
  LazarusIDEStrConsts;

function ShowSizeComponentsDialog(var HorizSizingID, FixedWidth,
  VertSizingID, FixedHeight: integer): TModalResult;
var
  SizeComponentsDialog: TSizeComponentsDialog;
begin
  SizeComponentsDialog := TSizeComponentsDialog.Create(nil);
  with SizeComponentsDialog do
  begin
    Result := ShowModal;
    HorizSizingID := WidthRadioGroup.ItemIndex;
    FixedWidth := StrToIntDef(WidthEdit.Text,0);
    VertSizingID := HeightRadioGroup.ItemIndex;
    FixedHeight := StrToIntDef(HeightEdit.Text,0);
  end;
end;

{ TSizeComponentsDialog }

procedure TSizeComponentsDialog.FormClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  CloseAction := caFree;
end;

constructor TSizeComponentsDialog.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  Caption:=fdmSizeWord;

  with WidthRadioGroup do
  begin
    Caption:=dlgWidthPos;
    with Items do
    begin
      BeginUpdate;
      Add(lisNoChange);
      Add(lisShrinkToSmal);
      Add(lisGrowToLarges);
      Add(dlgWidthPos);
      EndUpdate;
    end;
    ItemIndex:=0;
  end;

  with HeightRadioGroup do
  begin
    Caption:=DlgHeightPos;
    with Items do
    begin
      BeginUpdate;
      Add(lisNoChange);
      Add(lisShrinkToSmal);
      Add(lisGrowToLarges);
      Add(DlgHeightPos);
      EndUpdate;
    end;
    ItemIndex:=0;
  end;

  WidthEdit.Text:='';
  HeightEdit.Text:='';
end;

end.
