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
  private
    { private declarations }
  public
    function HtmlText:string;
  end; 

var
  fpwebNewHTMLInputForm: TfpwebNewHTMLInputForm;

implementation

{$R *.lfm}

{ TfpwebNewHTMLInputForm }

function TfpwebNewHTMLInputForm.HtmlText: string;
begin
  { TODO : temp code - need rewrite }
  Result:='<INPUT type="' + cbType.Text + '" ' +
          'name="'+edtName.Text+'"';

  if edtValue.Text <> '' then;
    Result:=Result + ' value="'+edtValue.Text+'" ';

  if edtSize.Text <> '' then;
    Result:=Result + ' size="'+edtSize.Text+'" ';

  if edtMaxLen.Text<>'' then
    Result:=Result + ' maxlength="'+edtMaxLen.Text+'"';
  if cbAlign.Text <> '' then
    Result:=Result + ' align="'+cbAlign.Text+'"';
  if cgOptions.Checked[2] then
    Result:=Result + ' readonly ';
  Result:=Result + '>';
end;

end.

