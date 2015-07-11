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

unit fpWebSelectTagUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ButtonPanel,
  ComCtrls, StdCtrls, Spin, Grids;

type

  { TfpWebSelectTagForm }

  TfpWebSelectTagForm = class(TForm)
    ButtonPanel1: TButtonPanel;
    CBMultiple: TCheckBox;
    CBDisabled: TCheckBox;
    CBOnFocus: TComboBox;
    ComboBox2: TComboBox;
    CBOnChange: TComboBox;
    edtName: TEdit;
    EdtTab: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    PageControl1: TPageControl;
    edtSize: TSpinEdit;
    SECount: TSpinEdit;
    StringGrid1: TStringGrid;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    TabSheet3: TTabSheet;
    TabSheet4: TTabSheet;
    procedure FormCreate(Sender: TObject);
    procedure SECountChange(Sender: TObject);
  private
    { private declarations }
  public
    function HtmlText:string;
  end;

var
  fpWebSelectTagForm: TfpWebSelectTagForm;

implementation

{$R *.lfm}

{ TfpWebSelectTagForm }

procedure TfpWebSelectTagForm.FormCreate(Sender: TObject);
begin
  SECountChange(nil);
end;

procedure TfpWebSelectTagForm.SECountChange(Sender: TObject);
begin
  StringGrid1.RowCount:=SECount.Value + 1;
end;

function TfpWebSelectTagForm.HtmlText: string;
var
  i:integer;
begin
  Result:='<SELECT name="'+edtName.Text+'"';
  if EdtTab.Text <> '' then
    Result:=Result + ' tabindex="'+EdtTab.Text+'"';

  if edtSize.Value > 1 then
    Result:=Result + ' size="'+IntToStr(edtSize.Value)+'"';

  Result:=Result + '>'+LineEnding;

  for i:=0 to SECount.Value - 1  do
  begin
     Result:=Result + '  <option';
     if StringGrid1.Cells[1, i+1]<>'' then
       Result:=Result + ' value="'+StringGrid1.Cells[1, i+1]+'"';
     Result:=Result + '>'+StringGrid1.Cells[0, i+1]+'</option>'+LineEnding;
  end;
  Result:=Result + '</SELECT>';
end;

end.

