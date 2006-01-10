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
  LResources;

type
  { TSizeComponentsDialog }
  TSizeComponentsDialog = class(TForm)
    OKButton: TBitBtn;
    CancelButton: TBitBtn;
    WidthRadioGroup: TRadioGroup;
    HeightRadioGroup: TRadioGroup;
    WidthEdit: TEdit;
    HeightEdit: TEdit;
    procedure FormResize(Sender: TObject);
  public
    constructor Create(AOwner: TComponent);  override;
  end;

function ShowSizeComponentsDialog(var HorizSizingID, FixedWidth,
  VertSizingID, FixedHeight: integer): TModalResult;

implementation

function ShowSizeComponentsDialog(var HorizSizingID, FixedWidth,
  VertSizingID, FixedHeight: integer): TModalResult;
var SizeComponentsDialog: TSizeComponentsDialog;
begin
  SizeComponentsDialog:=TSizeComponentsDialog.Create(nil);
  with SizeComponentsDialog do begin
    Result:=ShowModal;
    HorizSizingID:=SizeComponentsDialog.WidthRadioGroup.ItemIndex;
    FixedWidth:=StrToIntDef(SizeComponentsDialog.WidthEdit.Text,0);
    VertSizingID:=SizeComponentsDialog.HeightRadioGroup.ItemIndex;
    FixedHeight:=StrToIntDef(SizeComponentsDialog.HeightEdit.Text,0);
  end;
end;

{ TSizeComponentsDialog }

procedure TSizeComponentsDialog.FormResize(Sender: TObject);
begin
  WidthRadioGroup.Width := ClientWidth div 2 - 9;
  HeightRadioGroup.Left := WidthRadioGroup.Width + 12;
  HeightRadioGroup.Width := ClientWidth div 2 - 9;
end;

constructor TSizeComponentsDialog.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  Caption:='Size';

  with WidthRadioGroup do begin
    Caption:='Width:';
    with Items do begin
      BeginUpdate;
      Add('No change');
      Add('Shrink to smallest');
      Add('Grow to Largest');
      Add('Width:');
      EndUpdate;
    end;
    ItemIndex:=0;
  end;

  with HeightRadioGroup do begin
    Caption:='Height:';
    with Items do begin
      BeginUpdate;
      Add('No change');
      Add('Shrink to smallest');
      Add('Grow to Largest');
      Add('Height:');
      EndUpdate;
    end;
    ItemIndex:=0;
  end;

  WidthEdit.Text:='';
  HeightEdit.Text:='';

  OkButton.Caption:='Ok';
end;

initialization
  {$i sizecompsdlg.lrs}

end.
