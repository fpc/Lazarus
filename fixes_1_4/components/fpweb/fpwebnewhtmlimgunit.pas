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
unit fpwebNewHTMLImgUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ButtonPanel,
  StdCtrls, EditBtn, ExtCtrls;

type

  { TfpwebNewHTMLImgForm }

  TfpwebNewHTMLImgForm = class(TForm)
    ButtonPanel1: TButtonPanel;
    edtWidth: TEdit;
    edtHeigth: TEdit;
    edtHSpace: TEdit;
    edtVSpace: TEdit;
    edtAlt: TEdit;
    edtFileName: TFileNameEdit;
    Image1: TImage;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    procedure edtFileNameAcceptFileName(Sender: TObject; var Value: String);
    procedure FormCreate(Sender: TObject);
  private
    { private declarations }
  public
    function HtmlText:string;
  end;

var
  fpwebNewHTMLImgForm: TfpwebNewHTMLImgForm;

implementation

{$R *.lfm}

{ TfpwebNewHTMLImgForm }

procedure TfpwebNewHTMLImgForm.FormCreate(Sender: TObject);
begin
  //
end;

procedure TfpwebNewHTMLImgForm.edtFileNameAcceptFileName(Sender: TObject;
  var Value: String);
begin
  if FileExistsUTF8(Value) then
  begin
    Image1.Picture.LoadFromFile(Value);
    edtHeigth.Text:=IntToStr(Image1.Picture.Bitmap.Height);
    edtWidth.Text:=IntToStr(Image1.Picture.Bitmap.Width);
  end;
end;

function TfpwebNewHTMLImgForm.HtmlText: string;
begin
  Result:='<IMG src="'+edtFileName.FileName+'" alt="'+edtAlt.Text+'" ';
  if edtWidth.Text<>'' then
    Result:=Result + 'width="'+edtWidth.Text+'" ';
  if edtHeigth.Text<>'' then
    Result:=Result + 'height="'+edtHeigth.Text+'" ';
  if edtHSpace.Text<>'' then
    Result:=Result + 'hspace="'+edtHSpace.Text+'" ';
  if edtVSpace.Text<>'' then
    Result:=Result + 'vspace="'+edtVSpace.Text+'" ';

  Result:=Result + '>';
end;

end.

