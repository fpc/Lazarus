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
  ExtCtrls, LResources;

type
  TScaleComponentsDialog = class(TForm)
    Bevel: TBevel;
    ScaleLabel: TLabel;
    PercentEdit: TEdit;
    PercentLabel: TLabel;
    OkButton: TButton;
    CancelButton: TButton;
    procedure OkButtonClick(Sender: TObject);
    procedure CancelButtonClick(Sender: TObject);
  public
    constructor Create(AOwner: TComponent);  override;
  end;


function ShowScaleComponentsDialog(var ScaleInPercent: integer): TModalResult;

implementation

function ShowScaleComponentsDialog(var ScaleInPercent: integer): TModalResult;
var ScaleComponentsDialog: TScaleComponentsDialog;
begin
  ScaleComponentsDialog:=TScaleComponentsDialog.Create(nil);
  with ScaleComponentsDialog do begin
    SetBounds((Screen.Width-270) div 2,(Screen.Height-110) div 2,260,100);
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
  if LazarusResources.Find(Classname)=nil then begin
    SetBounds((Screen.Width-270) div 2,(Screen.Height-110) div 2,260,100);
    Caption:='Scale';

    Bevel:=TBevel.Create(Self);
    with Bevel do begin
      Name:='Bevel';
      Parent:=Self;
      Left:=5;
      Top:=5;
      Width:=250;
      Height:=50;
      Visible:=true;
    end;

    ScaleLabel:=TLabel.Create(Self);
    with ScaleLabel do begin
      Name:='ScaleLabel';
      Parent:=Self;
      Left:=12;
      Top:=15;
      Width:=90;
      Height:=25;
      Caption:='Scaling factor:';
      Visible:=true;
    end;

    PercentEdit:=TEdit.Create(Self);
    with PercentEdit do begin
      Name:='PercentEdit';
      Parent:=Self;
      Left:=140;
      Top:=20;
      Width:=60;
      Text:='100';
      Visible:=true;
    end;

    PercentLabel:=TLabel.Create(Self);
    with PercentLabel do begin
      Name:='PercentLabel';
      Parent:=Self;
      Left:=PercentEdit.Left+PercentEdit.Width+5;
      Top:=ScaleLabel.Top;
      Width:=15;
      Height:=25;
      Caption:='%';
      Visible:=true;
    end;

    OkButton:=TButton.Create(Self);
    with OkButton do begin
      Name:='OkButton';
      Parent:=Self;
      Left:=85;
      Top:=Bevel.Top+Bevel.Height+10;
      Width:=75;
      Height:=25;
      Caption:='Ok';
      OnClick:=@OkButtonClick;
      Visible:=true;
    end;

    CancelButton:=TButton.Create(Self);
    with CancelButton do begin
      Name:='CancelButton';
      Parent:=Self;
      Left:=175;
      Top:=OkButton.Top;
      Width:=75;
      Height:=25;
      Caption:='Cancel';
      OnClick:=@CancelButtonClick;
      Visible:=true;
    end;
  end;
  DebugLn('[TScaleComponentsDialog.Create] END');
end;

procedure TScaleComponentsDialog.OkButtonClick(Sender: TObject);
begin
  ModalResult:=mrOk;
end;

procedure TScaleComponentsDialog.CancelButtonClick(Sender: TObject);
begin
  ModalResult:=mrCancel;
end;

end.
