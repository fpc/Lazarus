{ Lazarus IDE wizard for fpweb package.

  Copyright (C) 2010 Lagunov Aleksey alexs75@hotbox.ru

  This library is free software; you can redistribute it and/or modify it
  under the terms of the GNU Library General Public License as published by
  the Free Software Foundation; either version 2 of the License, or (at your
  option) any later version with the following modification:

  As a special exception, the copyright holders of this library give you
  permission to link this library with independent modules to produce an
  executable, regardless of the license terms of these independent modules,and
  to copy and distribute the resulting executable under terms of your choice,
  provided that you also meet, for each linked independent module, the terms
  and conditions of the license of that module. An independent module is a
  module which is not derived from or based on this library. If you modify
  this library, you may extend this exception to your version of the library,
  but you are not obligated to do so. If you do not wish to do so, delete this
  exception statement from your version.

  This program is distributed in the hope that it will be useful, but WITHOUT
  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
  FITNESS FOR A PARTICULAR PURPOSE. See the GNU Library General Public License
  for more details.

  You should have received a copy of the GNU Library General Public License
  along with this library; if not, write to the Free Software Foundation,
  Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
}
unit fpWebNewHtmlTableUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Spin, ColorBox, ButtonPanel, Grids;

type

  { TfpWebNewHtmlTableForm }

  TfpWebNewHtmlTableForm = class(TForm)
    ButtonPanel1: TButtonPanel;
    cbUseHeader: TCheckBox;
    CheckBox2: TCheckBox;
    ColorBox1: TColorBox;
    cbTableWidthUnits: TComboBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    lblCellspacing: TLabel;
    Label6: TLabel;
    edtColCount: TSpinEdit;
    edtRowCount: TSpinEdit;
    edtTableWidth: TSpinEdit;
    edtCellpadding: TSpinEdit;
    edtCellspacing: TSpinEdit;
    edtBorderWidth: TSpinEdit;
    StringGrid1: TStringGrid;
    procedure ColorBox1Change(Sender: TObject);
    procedure edtColCountChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { private declarations }
  public
    function HtmlText:string;
  end; 

var
  fpWebNewHtmlTableForm: TfpWebNewHtmlTableForm;

implementation
uses fpWebStrConsts;

{$R *.lfm}

{ TfpWebNewHtmlTableForm }

procedure TfpWebNewHtmlTableForm.FormCreate(Sender: TObject);
begin
  Caption:=SHTMLTableFormCaption;
  Label1.Caption:=SHTMLTableFormColumnCount;
  Label2.Caption:=SHTMLTableFormRowCount;
  Label3.Caption:=SHTMLTableFormBorderWidth;
  cbUseHeader.Caption:=SHTMLTableFormUseHeader;
  Label4.Caption:=SHTMLTableFormCellpadding;
  lblCellspacing.Caption:=SHTMLTableFormCellspacing;
  CheckBox2.Caption:=SHTMLTableFormWidth;
  Label6.Caption:=SHTMLTableFormHeaderBGColor;
end;

function TfpWebNewHtmlTableForm.HtmlText: string;
var
  i, j:integer;
begin
  Result:=Format('<table border="%d" cellpadding="%d" cellspacing="%d"',
    [edtBorderWidth.Value,
     edtCellpadding.Value,
     edtCellspacing.Value]);
  if CheckBox2.Checked then
  begin
    Result:=Result + ' width="'+IntToStr(edtTableWidth.Value);
    if cbTableWidthUnits.ItemIndex = 0 then
      Result:=Result + '%';
    Result:=Result + '">';
  end
  else
    Result:=Result + '>';

  Result:=Result + LineEnding;

  if cbUseHeader.Checked then
  begin
    Result:=Result + '  <thead>'+LineEnding;// bgcolor="#4ebad2" class="QListHeaderText">
    for I:=1 to edtColCount.Value do
      Result:=Result + '    <th> </th>'+LineEnding;
    Result:=Result + '  </thead>'+LineEnding;
  end;

  Result:=Result + '  <tbody>'+LineEnding;
  for I:=1 to edtRowCount.Value do
  begin
    Result:=Result + '    <tr>'+LineEnding;
    for j:=1 to edtColCount.Value do
    begin
      Result:=Result + '      <td> </td>'+LineEnding;
    end;
    Result:=Result + '    </tr>'+LineEnding;
  end;
  Result:=Result + '  </tbody>'+LineEnding;
  Result:=Result + '</table>';
end;

procedure TfpWebNewHtmlTableForm.ColorBox1Change(Sender: TObject);
begin
  StringGrid1.FixedColor:=ColorBox1.Selected;
end;

procedure TfpWebNewHtmlTableForm.edtColCountChange(Sender: TObject);
begin
  StringGrid1.RowCount:=edtRowCount.Value + ord(cbUseHeader.Checked);
  StringGrid1.ColCount:=edtColCount.Value;
end;

end.

