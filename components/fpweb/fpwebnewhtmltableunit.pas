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

