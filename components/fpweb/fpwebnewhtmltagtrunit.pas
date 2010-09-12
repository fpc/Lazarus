unit fpwebNewHtmlTagTRUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ButtonPanel,
  ComCtrls, StdCtrls, ColorBox;

type

  { TfpwebNewHtmlTagTRForm }

  TfpwebNewHtmlTagTRForm = class(TForm)
    ButtonPanel1: TButtonPanel;
    CBColor: TColorBox;
    CBAlign: TComboBox;
    CBValign: TComboBox;
    CBId: TComboBox;
    CBClass: TComboBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
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
  fpwebNewHtmlTagTRForm: TfpwebNewHtmlTagTRForm;

implementation

{$R *.lfm}

{ TfpwebNewHtmlTagTRForm }

function TfpwebNewHtmlTagTRForm.HtmlText: string;
begin
  Result:='';
end;

end.

