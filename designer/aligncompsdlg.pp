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
  Classes, LCLIntf, Forms, Controls, Buttons, ExtCtrls, LResources;

type
  TAlignComponentsDialog = class(TForm)
    HorizontalRadioGroup: TRadioGroup;
    VerticalRadioGroup: TRadioGroup;
    OkButton: TButton;
    CancelButton: TButton;
    procedure OkButtonClick(Sender: TObject);
    procedure CancelButtonClick(Sender: TObject);
  public
    constructor Create(AOwner: TComponent);  override;
  end;


function ShowAlignComponentsDialog(var HorizAlignID, VertAlignID: integer
  ): TModalResult;

implementation

function ShowAlignComponentsDialog(var HorizAlignID, VertAlignID: integer
  ): TModalResult;
var AlignComponentsDialog: TAlignComponentsDialog;
begin
  AlignComponentsDialog:=TAlignComponentsDialog.Create(nil);
  with AlignComponentsDialog do begin
    SetBounds((Screen.Width-365) div 2,(Screen.Height-225) div 2,355,215);
    HorizontalRadioGroup.ItemIndex:=0;
    VerticalRadioGroup.ItemIndex:=0;
    Result:=ShowModal;
    HorizAlignID:=AlignComponentsDialog.HorizontalRadioGroup.ItemIndex;
    VertAlignID:=AlignComponentsDialog.VerticalRadioGroup.ItemIndex;
  end;
end;


{ TAlignComponentsDialog }

constructor TAlignComponentsDialog.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  if LazarusResources.Find(Classname)=nil then begin
    SetBounds((Screen.Width-365) div 2,(Screen.Height-225) div 2,355,215);
    Caption:='Alignment';

    HorizontalRadioGroup:=TRadioGroup.Create(Self);
    with HorizontalRadioGroup do begin
      Name:='HorizontalRadioGroup';
      Parent:=Self;
      Left:=5;
      Top:=5;
      Width:=170;
      Height:=165;
      Caption:='Horizontal';
      with Items do begin
        BeginUpdate;
        Add('No change');
        Add('Left sides');
        Add('Centers');
        Add('Right sides');
        Add('Center in window');
        Add('Space equally');
        Add('Left space equally');
        Add('Right space equally');
        EndUpdate;
      end;
      Show;
    end;

    VerticalRadioGroup:=TRadioGroup.Create(Self);
    with VerticalRadioGroup do begin
      Name:='VerticalRadioGroup';
      Parent:=Self;
      Left:=180;
      Top:=5;
      Width:=170;
      Height:=165;
      Caption:='Vertical';
      with Items do begin
        BeginUpdate;
        Add('No change');
        Add('Tops');
        Add('Centers');
        Add('Bottoms');
        Add('Center in window');
        Add('Space equally');
        Add('Top space equally');
        Add('Bottom space equally');
        EndUpdate;
      end;
      Show;
    end;

    OkButton:=TButton.Create(Self);
    with OkButton do begin
      Name:='OkButton';
      Parent:=Self;
      Left:=145;
      Top:=179;
      Width:=75;
      Height:=25;
      Caption:='Ok';
      OnClick:=@OkButtonClick;
      Show;
    end;

    CancelButton:=TButton.Create(Self);
    with CancelButton do begin
      Name:='CancelButton';
      Parent:=Self;
      Left:=235;
      Top:=OkButton.Top;
      Width:=75;
      Height:=25;
      Caption:='Cancel';
      OnClick:=@CancelButtonClick;
      Show;
    end;
  end;
end;

procedure TAlignComponentsDialog.OkButtonClick(Sender: TObject);
begin
  ModalResult:=mrOk;
end;

procedure TAlignComponentsDialog.CancelButtonClick(Sender: TObject);
begin
  ModalResult:=mrCancel;
end;

end.
