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
unit fpwebNewHTMLInputUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ButtonPanel,
  ComCtrls, StdCtrls, EditBtn, ExtCtrls;

type

  { TfpwebNewHTMLInputForm }

  TfpwebNewHTMLInputForm = class(TForm)
    ButtonPanel1: TButtonPanel;
    cgOptions: TCheckGroup;
    cbType: TComboBox;
    cbAlign: TComboBox;
    edtName: TEdit;
    EdtOnClick: TEdit;
    EdtOnDblClick: TEdit;
    edtValue: TEdit;
    edtAlt: TEdit;
    edtSize: TEdit;
    edtMaxLen: TEdit;
    edtTab: TEdit;
    edtKey: TEdit;
    EdtID: TEdit;
    EdtClass: TEdit;
    edtSrc: TEditButton;
    Label1: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    Label13: TLabel;
    Label14: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    TabSheet3: TTabSheet;
    procedure FormCreate(Sender: TObject);
  private
    { private declarations }
  public
    function HtmlText:string;
  end; 

var
  fpwebNewHTMLInputForm: TfpwebNewHTMLInputForm;

implementation


uses fpwebstrconsts;
{$R *.lfm}

{ TfpwebNewHTMLInputForm }

procedure TfpwebNewHTMLInputForm.FormCreate(Sender: TObject);
begin
   Caption:=SHTMLInputFormCaption;
   Label1.Caption:=SHTMLInputFormType;
   Label2.Caption:=SHTMLInputFormName;
   Label3.Caption:=SHTMLInputFormValue;
   Label4.Caption:=SHTMLInputFormSize;
   Label5.Caption:=SHTMLInputFormMaxLen;
   Label6.Caption:=SHTMLInputFormAlt;
   Label7.Caption:=SHTMLInputFormImageSrc;
   Label8.Caption:=SHTMLInputFormTabIndex;
   Label9.Caption:=SHTMLInputFormAlign;
   Label10.Caption:=SHTMLInputFormAccessKey;
end;

function TfpwebNewHTMLInputForm.HtmlText: string;
begin
  { TODO : temp code - need rewrite }
  Result:='<INPUT type="' + cbType.Text + '" ' +
          'name="'+edtName.Text+'"';

  if edtValue.Text <> '' then
    Result:=Result + ' value="'+edtValue.Text+'" ';

  if (Trim(edtSize.Text) <> '') then
    Result:=Result + ' size="'+trim(edtSize.Text)+'" ';

  if edtMaxLen.Text<>'' then
    Result:=Result + ' maxlength="'+edtMaxLen.Text+'"';
  if cbAlign.Text <> '' then
    Result:=Result + ' align="'+cbAlign.Text+'"';
  if cgOptions.Checked[2] then
    Result:=Result + ' readonly ';
  Result:=Result + '>';
end;

end.

