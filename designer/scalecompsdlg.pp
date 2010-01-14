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
    Defines TScaleComponentsDialog.
}
unit ScaleCompsDlg;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LCLIntf, LCLProc, Forms, Controls, Buttons, StdCtrls,
  ExtCtrls, LazarusIDEStrConsts, ButtonPanel;

type

  { TScaleComponentsDialog }

  TScaleComponentsDialog = class(TForm)
    ButtonPanel1: TButtonPanel;
    ScaleLabel: TLabel;
    PercentEdit: TEdit;
    PercentLabel: TLabel;
  public
    constructor Create(AOwner: TComponent);  override;
  end;

function ShowScaleComponentsDialog(var ScaleInPercent: integer): TModalResult;

implementation

{$R *.lfm}

function ShowScaleComponentsDialog(var ScaleInPercent: integer): TModalResult;
var
  ScaleComponentsDialog: TScaleComponentsDialog;
begin
  ScaleComponentsDialog:=TScaleComponentsDialog.Create(nil);
  with ScaleComponentsDialog do
  begin
    PercentEdit.Text:='100';
    Result:=ShowModal;
    ScaleInPercent:=StrToIntDef(ScaleComponentsDialog.PercentEdit.Text,100);
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
