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
 
  Author: Mattias Gaertner

  Abstract:
    Defines TScaleComponentsDialog.
}
unit ScaleCompsDlg;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LCLIntf, LCLProc, Forms, Controls, Buttons, StdCtrls,
  ExtCtrls, LazarusIDEStrConsts, ButtonPanel, Spin;

type

  { TScaleComponentsDialog }

  TScaleComponentsDialog = class(TForm)
    ButtonPanel1: TButtonPanel;
    PercentEdit: TSpinEdit;
    ScaleLabel: TLabel;
    PercentLabel: TLabel;
  public
    constructor Create(AOwner: TComponent);  override;
  end;

function ShowScaleComponentsDialog(out ScaleInPercent: integer): TModalResult;

implementation

{$R *.lfm}

function ShowScaleComponentsDialog(out ScaleInPercent: integer): TModalResult;
begin
  with TScaleComponentsDialog.Create(nil) do
  try
    PercentEdit.Value:=100;
    Result:=ShowModal;
    ScaleInPercent:=PercentEdit.Value;
  finally
    Free;
  end;
end;

{ TScaleComponentsDialog }

constructor TScaleComponentsDialog.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  Caption := fdmScaleWord;

  ScaleLabel.Caption := lisScalingFactor;
  PercentLabel.Caption := '%';
end;

end.
