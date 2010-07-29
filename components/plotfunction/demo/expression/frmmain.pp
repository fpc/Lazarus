unit frmmain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  ExtCtrls, StdCtrls, fpexprpars,exprplotpanel;

type

  { TMainForm }

  TMainForm = class(TForm)
    Button1: TButton;
    EA: TEdit;
    EB: TEdit;
    EC: TEdit;
    LEA: TLabel;
    LEC: TLabel;
    LEB: TLabel;
    Panel1: TPanel;
    PParams: TPanel;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { private declarations }
    FPanel : TPlotExpressionPanel;
  public
    { public declarations }
  end; 

var
  MainForm: TMainForm;

implementation

{$R *.lfm}

{ TMainForm }

procedure TMainForm.FormCreate(Sender: TObject);
begin
  FPanel:=TPlotExpressionPanel.Create(Self);
  FPanel.Parent:=Panel1;
  FPanel.Align:=alClient;
  FPanel.Identifiers.AddFloatVariable('a',1.0);
  FPanel.Identifiers.AddFloatVariable('b',0);
  FPanel.Identifiers.AddFloatVariable('c',0);
  FPanel.XAxis.Origin:=-30;
  FPanel.XAxis.DrawZero:=True;
  FPanel.YAxis.Caption.Alignment:=taCenter;
  FPanel.YAxis.Origin:=-10;
  FPanel.YAxis.DrawZero:=True;
  FPanel.Expression:='a * (x * x) + b * x + c';
  FPanel.Active:=True;
  FPanel.Caption.Title:='Square function demo';
  FPanel.Caption.Alignment:=taRightJustify;
end;

procedure TMainForm.Button1Click(Sender: TObject);

Var
  A,b,c : TExprFloat;

begin
  A:=StrToFLoat(EA.Text);
  b:=StrToFLoat(EB.Text);
  C:=StrToFLoat(EC.Text);
  FPanel.Identifiers.IdentifierByName('a').AsFloat:=A;
  FPanel.Identifiers.IdentifierByName('b').AsFloat:=B;
  FPanel.Identifiers.IdentifierByName('c').AsFloat:=C;
  FPanel.Invalidate;
end;

end.

