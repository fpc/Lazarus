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

unit fpWebFieldSetTagUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ComCtrls,
  ButtonPanel, StdCtrls;

type

  { TfpWebFieldSetTagForm }

  TfpWebFieldSetTagForm = class(TForm)
    ButtonPanel1: TButtonPanel;
    EdtLegend: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    TabSheet3: TTabSheet;
  private
    { private declarations }
  public
    function HtmlText(const S:string):string;
  end;

var
  fpWebFieldSetTagForm: TfpWebFieldSetTagForm;

implementation

{$R *.lfm}

{ TfpWebFieldSetTagForm }

function TfpWebFieldSetTagForm.HtmlText(const S: string): string;
begin
  Result:='<fieldset>'+LineEnding;
  if EdtLegend.Text<>'' then
    Result:=Result + '  <LEGEND >'+EdtLegend.Text+'</LEGEND>'+LineEnding;
  Result:=Result + S+LineEnding + '</fieldset>';
end;

end.

