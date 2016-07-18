unit sumabfrm;

{$mode objfpc}{$H+}
{$MODESWITCH NESTEDPROCVARS}

interface

uses
  Classes, SysUtils, Ipfilebroker, IpHtml, Forms, Dialogs;

type

  { TForm1 }

  TForm1 = class(TForm)
    IpFileDataProvider1: TIpFileDataProvider;
    IpHtmlPanel1: TIpHtmlPanel;
    procedure FormShow(Sender: TObject);
    procedure IpHtmlPanel1ControlClick2(Sender: TIpHtmlCustomPanel;
      Frame: TIpHtmlFrame; Html: TIpHtml; Node: TIpHtmlNodeControl;
      var cancel: boolean);
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.FormShow(Sender: TObject);
begin
  IpHtmlPanel1.OpenURL(
    ExpandLocalHtmlFileName(ExtractFilePath(ParamStr(0)) + 'sum.html'));
end;

procedure TForm1.IpHtmlPanel1ControlClick2(Sender: TIpHtmlCustomPanel;
  Frame: TIpHtmlFrame; Html: TIpHtml; Node: TIpHtmlNodeControl;
  var cancel: boolean);
var
  A, B: TIpHtmlNodeINPUT;
begin
  A := FindNodeByElemId(Html.HtmlNode, 'a') as TIpHtmlNodeINPUT;
  B := FindNodeByElemId(Html.HtmlNode, 'b') as TIpHtmlNodeINPUT;
  ShowMessageFmt('Sum: %d', [StrToInt(A.Value) + StrToInt(B.Value)]);
end;

end.

