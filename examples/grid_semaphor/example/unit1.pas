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
Author: Salvatore Coppola
}
unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs,
  Grids, SemaphorGrids, Buttons, StdCtrls, ExtCtrls;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    Button10: TButton;
    Button11: TButton;
    Button12: TButton;
    Button13: TButton;
    Button14: TButton;
    Button15: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    Button5: TButton;
    Button6: TButton;
    Button7: TButton;
    Button8: TButton;
    Button9: TButton;
    CheckBox1: TCheckBox;
    CheckBox2: TCheckBox;
    OpenDialog1: TOpenDialog;
    RadioButton1: TRadioButton;
    RadioButton2: TRadioButton;
    SaveDialog1: TSaveDialog;
    SemaphorGrid1: TSemaphorGrid;
    SemaphorGrid2: TSemaphorGrid;
    SemaphorGrid3: TSemaphorGrid;
    ToggleBox1: TToggleBox;
    procedure Button10Click(Sender: TObject);
    procedure Button11Click(Sender: TObject);
    procedure Button12Click(Sender: TObject);
    procedure Button13Click(Sender: TObject);
    procedure Button14Click(Sender: TObject);
    procedure Button15Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure Button6Click(Sender: TObject);
    procedure Button7Click(Sender: TObject);
    procedure Button8Click(Sender: TObject);
    procedure Button9Click(Sender: TObject);
    procedure CheckBox2Click(Sender: TObject);
    procedure Form1Create(Sender: TObject);
    procedure ToggleBox1Click(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end; 

var
  Form1: TForm1; 

implementation

{ TForm1 }

procedure TForm1.Button1Click(Sender: TObject);
begin
  SemaphorGrid1.Semaphor:=not SemaphorGrid1.Semaphor;
end;

procedure TForm1.Button10Click(Sender: TObject);
begin
  if SemaphorGrid1.ColCount>1 then
    if SemaphorGrid1.ColWidths[1]=SemaphorGrid1.GridLineWidth then
      SemaphorGrid1.ShowCol(1)
    else
      SemaphorGrid1.HideCol(1);
end;

procedure TForm1.Button11Click(Sender: TObject);
begin
  SemaphorGrid1.Clear(CheckBox1.Checked);
end;

procedure TForm1.Button12Click(Sender: TObject);
var TD:TDirection;
begin
  if SemaphorGrid1.ColCount>1 then begin
    TD:=TDirection(RadioButton1.Checked);
    SemaphorGrid1.SortFromColumn(1,tsAutomatic,TD,false);
  end;
end;

procedure TForm1.Button13Click(Sender: TObject);
begin
  SemaphorGrid1.AutoFit;
end;

procedure TForm1.Button14Click(Sender: TObject);
begin
  SemaphorGrid1.AssignToG(SemaphorGrid2,false);
  SemaphorGrid3.AssignG(SemaphorGrid2,true);
end;

procedure TForm1.Button15Click(Sender: TObject);
var strtmp:string;
    oldCHSEP:char;
begin
  oldCHSEP:=SemaphorGrid1.CHSEP;
  SemaphorGrid1.CHSEP:=#32;
  SemaphorGrid1.SaveToString(strtmp,false);
  ShowMessage(strtmp);
  SemaphorGrid1.CHSEP:=oldCHSEP;
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  with SemaphorGrid1 do begin
    if SemaphorShape<>ssDisk then
      SemaphorShape:=succ(SemaphorShape)
    else
      SemaphorShape:=ssTopBar;
  end;
end;

procedure TForm1.Button3Click(Sender: TObject);
begin
  if SemaphorGrid1.Alignment=taCenter then
    SemaphorGrid1.Alignment:=taLeftJustify
  else
    SemaphorGrid1.Alignment:=succ(SemaphorGrid1.Alignment);
end;

procedure TForm1.Button4Click(Sender: TObject);
begin
  Close;
end;

procedure TForm1.Button5Click(Sender: TObject);
begin
  SaveDialog1.Filter:='Semaphor Table (*.stb)|*.stb';
  SaveDialog1.DefaultExt:='.stb';
  if SaveDialog1.Execute then
    SemaphorGrid1.SaveToFileG(SaveDialog1.FileName,true);
end;

procedure TForm1.Button6Click(Sender: TObject);
begin
  OpenDialog1.Filter:='Semaphor Table (*.stb)|*.stb';
  if OpenDialog1.Execute then begin
    SemaphorGrid1.LoadFromFileG(OpenDialog1.FileName,false);
    SemaphorGrid1.AutoWidth;
    SemaphorGrid1.AutoHeight;
  end;
end;

procedure TForm1.Button7Click(Sender: TObject);
begin
  SemaphorGrid1.AutoWidth;
end;

procedure TForm1.Button8Click(Sender: TObject);
begin
  SemaphorGrid1.AutoHeight;
end;

procedure TForm1.Button9Click(Sender: TObject);
var TD:TDirection;
begin
  if SemaphorGrid1.ColCount>4 then begin
    TD:=TDirection(RadioButton1.Checked);
    SemaphorGrid1.SortFromColumn(4,tsAutomatic,TD,false);
  end;
end;

procedure TForm1.CheckBox2Click(Sender: TObject);
begin
  SemaphorGrid1.SemaphorOnlyFloat:=CheckBox2.Checked;
end;

procedure TForm1.Form1Create(Sender: TObject);
begin
  RadioButton1.Checked:=true;
end;

procedure TForm1.ToggleBox1Click(Sender: TObject);
begin
  if ToggleBox1.State=cbUnchecked then begin
    ToggleBox1.Caption:='UnEditable';
    SemaphorGrid1.Options:=SemaphorGrid1.Options-[goEditing]
  end else begin
    ToggleBox1.Caption:='Editable';
    SemaphorGrid1.Options:=SemaphorGrid1.Options+[goEditing]
  end;

end;

initialization
  {$I Unit1.lrs}

end.

