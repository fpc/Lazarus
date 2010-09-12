unit fpwebNewHTMLListUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ButtonPanel,
  StdCtrls, Spin, ExtCtrls;

type

  { TfpwebNewHTMLListForm }

  TfpwebNewHTMLListForm = class(TForm)
    ButtonPanel1: TButtonPanel;
    Label1: TLabel;
    RGStyle: TRadioGroup;
    SERows: TSpinEdit;
  private
    { private declarations }
  public
    function HtmlText:string;
  end; 

var
  fpwebNewHTMLListForm: TfpwebNewHTMLListForm;

implementation
uses strutils;

{$R *.lfm}

{ TfpwebNewHTMLListForm }

function TfpwebNewHTMLListForm.HtmlText: string;
var
  i:integer;
begin
  Result:=DupeString('<li>  </li>'+LineEnding, SERows.Value);
  if RGStyle.ItemIndex = 0 then
    Result:='<ol>' + LineEnding + Result + '</ol>'
  else
    Result:='ul>' + LineEnding + Result + '</ul>';
end;

end.

