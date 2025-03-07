unit frmExpression;

{$mode ObjFPC}{$H+}

interface

uses
  SysUtils, Classes,
  Forms, Controls, StdCtrls, ExtCtrls,
  TAGraph, TAExpressionSeries;

type
  TExpressionFrame = class(TFrame)
    EdExprDomain: TEdit;
    EdExpression: TEdit;
    EdExprParamA: TEdit;
    EdExprParamB: TEdit;
    Chart: TChart;
    ExpressionSeries: TExpressionSeries;
    LblExprDomain: TLabel;
    LblExpression: TLabel;
    LblExprParamA: TLabel;
    LblExprParamB: TLabel;
    Panel2: TPanel;
    procedure EdExprDomainEditingDone(Sender: TObject);
    procedure EdExpressionEditingDone(Sender: TObject);
    procedure EdExprParamAEditingDone(Sender: TObject);
    procedure EdExprParamBEditingDone(Sender: TObject);
  private

  public
    constructor Create(AOwner: TComponent); override;

  end;

implementation

{$R *.lfm}

constructor TExpressionFrame.Create(AOwner: TComponent);
begin
  inherited;

  EdExpression.Text := ExpressionSeries.Expression;
  EdExprDomain.Text := ExpressionSeries.Domain;
  EdExprParamA.Text := FloatToStr(ExpressionSeries.Params.ValueByName['a']);
  EdExprParamB.Text := FloatToStr(ExpressionSeries.Params.ValueByName['b']);
end;

procedure TExpressionFrame.EdExpressionEditingDone(Sender: TObject);
begin
  ExpressionSeries.Expression := EdExpression.Text;
end;

procedure TExpressionFrame.EdExprParamAEditingDone(Sender: TObject);
begin
  ExpressionSeries.Params.ValueByName['a'] := StrToFloat(EdExprParamA.Text);
end;

procedure TExpressionFrame.EdExprParamBEditingDone(Sender: TObject);
begin
  ExpressionSeries.Params.ValueByName['b'] := StrToFloat(EdExprParamB.Text);
end;

procedure TExpressionFrame.EdExprDomainEditingDone(Sender: TObject);
begin
  ExpressionSeries.Domain := EdExprDomain.Text;
end;

end.

