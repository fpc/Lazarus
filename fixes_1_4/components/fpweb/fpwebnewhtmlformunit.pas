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

unit fpwebNewHTMLFormUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ButtonPanel,
  ComCtrls, StdCtrls, EditBtn;

type

  { TfpwebNewHTMLFormForm }

  TfpwebNewHTMLFormForm = class(TForm)
    ButtonPanel1: TButtonPanel;
    cbAction: TComboBox;
    cbMetod: TComboBox;
    cbEncType: TComboBox;
    cbTarget: TComboBox;
    cbAcceptCharset: TComboBox;
    ComboBox6: TComboBox;
    ComboBox7: TComboBox;
    ComboBox8: TComboBox;
    ComboBox9: TComboBox;
    EditButton1: TEditButton;
    Label1: TLabel;
    Label10: TLabel;
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
    procedure FillActionList;
  public
    function HtmlText(const S:string): string;
  end; 

var
  fpwebNewHTMLFormForm: TfpwebNewHTMLFormForm;

implementation
uses fpWeb;

{$R *.lfm}

{ TfpwebNewHTMLFormForm }

procedure TfpwebNewHTMLFormForm.FormCreate(Sender: TObject);
begin
  cbAction.Items.Clear;
  FillActionList;
end;

procedure TfpwebNewHTMLFormForm.FillActionList;
var
  i, j:integer;
  WD:TFPWebModule;
begin
  for i:=0 to Screen.DataModuleCount - 1 do
  begin
    if Screen.DataModules[i] is TFPWebModule then
    begin
      WD:=Screen.DataModules[i] as TFPWebModule;
      for j:=0 to WD.Actions.Count - 1 do
        cbAction.Items.Add(WD.ActionVar +'='+ WD.Actions[j].Name);
    end;
  end;
end;

function TfpwebNewHTMLFormForm.HtmlText(const S:string): string;
begin
  Result:='<FORM action="?'+cbAction.Text+
          '" method="'+cbMetod.Text+'"';

  if cbEncType.Text<>'' then
    Result:=Result +' enctype="' + cbEncType.Text+ '"';

  if cbTarget.Text <> '' then
    Result:=Result +' target="' + cbTarget.Text + '"';

  if cbAcceptCharset.Text <> '' then
    Result:=Result +' accept-charset="' + cbAcceptCharset.Text + '">';

  Result:=Result +'>' + LineEnding + S+ LineEnding + '</FORM>';
end;

end.

