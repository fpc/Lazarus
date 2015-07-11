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

unit fpWebHREFEditUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ComCtrls,
  ButtonPanel, StdCtrls, Buttons;

type

  { TfpWebHREFEditForm }

  TfpWebHREFEditForm = class(TForm)
    ButtonPanel1: TButtonPanel;
    cbHREF: TComboBox;
    ComboBox10: TComboBox;
    CBOnMouseUp: TComboBox;
    ComboBox12: TComboBox;
    CBOnMouseMove: TComboBox;
    CBOnMouseOut: TComboBox;
    CBOnKeyPress: TComboBox;
    CBOnKeyDown: TComboBox;
    CBonKeyUp: TComboBox;
    cbType: TComboBox;
    cbTarg: TComboBox;
    CBID: TComboBox;
    CBClass: TComboBox;
    CBOnFocus: TComboBox;
    CBOnBlur: TComboBox;
    CBOnClick: TComboBox;
    CBOnDblClick: TComboBox;
    EdtRel: TComboBox;
    EdtRev: TComboBox;
    edtLinkName: TEdit;
    edtLinkText: TEdit;
    Label1: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    Label13: TLabel;
    Label14: TLabel;
    Label15: TLabel;
    Label16: TLabel;
    Label17: TLabel;
    Label18: TLabel;
    Label19: TLabel;
    Label2: TLabel;
    Label20: TLabel;
    Label21: TLabel;
    Label22: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    OpenDialog1: TOpenDialog;
    PageControl1: TPageControl;
    SpeedButton1: TSpeedButton;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    TabSheet3: TTabSheet;
    procedure FormCreate(Sender: TObject);
    procedure SpeedButton1Click(Sender: TObject);
  private
    procedure FillRefList;
  public
    function HtmlText:string;
  end;

var
  fpWebHREFEditForm: TfpWebHREFEditForm;

implementation
uses fpWebStrConsts, SrcEditorIntf, ProjectIntf, LazIDEIntf, fpWeb;

{$R *.lfm}

{ TfpWebHREFEditForm }

procedure TfpWebHREFEditForm.SpeedButton1Click(Sender: TObject);
begin
  if OpenDialog1.Execute then
    cbHREF.Text:=OpenDialog1.FileName;
end;

procedure TfpWebHREFEditForm.FormCreate(Sender: TObject);
begin
  FillRefList;
end;

procedure TfpWebHREFEditForm.FillRefList;

procedure DoFillProjectFiles;
var
  i: integer;
  S: string;
begin
  if Assigned(LazarusIDE) and Assigned(LazarusIDE.ActiveProject) then
  begin
    for i:=0 to LazarusIDE.ActiveProject.FileCount - 1 do
    begin
      if LazarusIDE.ActiveProject.Files[i].IsPartOfProject then
      begin
        S:=LazarusIDE.ActiveProject.Files[i].Filename;
        if Copy(UpperCase(ExtractFileExt(S)), 1, 4) = '.HTM' then
          cbHREF.Items.Add(S);
      end;
    end;
  end;
end;

procedure DoFillProjectActions;
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
        cbHREF.Items.Add('?'+WD.ActionVar +'='+ WD.Actions[j].Name);
    end;
  end;
end;

begin
  cbHREF.Items.Clear;
  //Fill files
  DoFillProjectActions;
  DoFillProjectFiles;
end;

function TfpWebHREFEditForm.HtmlText: string;
begin
  //<A href="work/ProgectManager/www_ui/html_template/pmQuestionStatusEdit.html" name="srger" rel="Contents" type="text/html" target="_self"></A>

  Result:='<a';
  if cbHREF.Text<>'' then
    Result:=Result + ' href="'+cbHREF.Text+'"';
  if edtLinkName.Text <> '' then
    Result:=Result + ' name="'+edtLinkName.Text+'"';
  if cbType.Text <> '' then
    Result:=Result + ' type="'+cbType.Text+'"';
  if cbTarg.Text <> '' then
    Result:=Result + ' target="'+cbTarg.Text+'"';
  Result:=Result + '>'+edtLinkText.Text+'</a>';
end;

end.

