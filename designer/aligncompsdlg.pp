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
    Defines TAlignComponentsDialog.
}
unit AlignCompsDlg;

{$mode objfpc}{$H+}

interface

uses
  Classes, LCLIntf, Forms, Controls, Buttons, ExtCtrls, 
  LazarusIDEStrConsts, ButtonPanel, StdCtrls;

type
  { TAlignComponentsDialog }
  TAlignComponentsDialog = class(TForm)
    BtnPanel: TButtonPanel;
    HorizontalRadioGroup: TRadioGroup;
    PosLabel: TLabel;
    VerticalRadioGroup: TRadioGroup;
  public
    constructor Create(AOwner: TComponent);  override;
  end;

function ShowAlignComponentsDialog(var HorizAlignID, VertAlignID: integer): TModalResult;

implementation

{$R *.lfm}

function ShowAlignComponentsDialog(var HorizAlignID, VertAlignID: integer): TModalResult;
var
  AlignComponentsDialog: TAlignComponentsDialog;
begin
  AlignComponentsDialog := TAlignComponentsDialog.Create(nil);
  try
    with AlignComponentsDialog do
    begin
      HorizontalRadioGroup.ItemIndex := 0;
      VerticalRadioGroup.ItemIndex := 0;
      Result := ShowModal;
      HorizAlignID := AlignComponentsDialog.HorizontalRadioGroup.ItemIndex;
      VertAlignID := AlignComponentsDialog.VerticalRadioGroup.ItemIndex;
    end;
  finally
    AlignComponentsDialog.Free;
  end;
end;

constructor TAlignComponentsDialog.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  Caption:=lisAlignment;

  with HorizontalRadioGroup do
  begin
    Caption:=lisHorizontal;
    with Items do
    begin
      BeginUpdate;
      Add(lisNoChange);
      Add(lisLeftSides);
      Add(lisCenters);
      Add(lisRightSides);
      Add(lisCenterInWindow);
      Add(lisSpaceEqually);
      Add(lisLeftSpaceEqually);
      Add(lisRightSpaceEqually);
      EndUpdate;
    end;
  end;

  with VerticalRadioGroup do
  begin
    Caption:=lisVertical;
    with Items do
    begin
      BeginUpdate;
      Add(lisNoChange);
      Add(lisTops);
      Add(lisCenters);
      Add(lisBottoms);
      Add(lisCenterInWindow);
      Add(lisSpaceEqually);
      Add(lisTopSpaceEqually);
      Add(lisBottomSpaceEqually);
      EndUpdate;
    end;
  end;
end;

end.
