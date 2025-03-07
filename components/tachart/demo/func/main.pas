unit main;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes,
  Forms, Graphics, Controls, ExtCtrls, ComCtrls, RTTICtrls,
  frmDomain, frmColorMap, frmSpline, frmAutoYExtent, frmParametric, frmExpression;

type

  { TMainForm }

  TMainForm = class(TForm)
    PageControl: TPageControl;
    tsExpression: TTabSheet;
    tsParametric: TTabSheet;
    tsAutoExtentY: TTabSheet;
    tsSpline: TTabSheet;
    tsDomain: TTabSheet;
    tsColorMap: TTabSheet;
    Splitter1: TSplitter;
    procedure FormCreate(Sender: TObject);
    procedure PageControlChange(Sender: TObject);
  private
    FDomainFrame: TDomainFrame;
    FColorMapFrame: TColorMapFrame;
    FSplineFrame: TSplineFrame;
    FAutoYExtentFrame: TAutoYExtentFrame;
    FParametricFrame: TParametricFrame;
    FExpressionFrame: TExpressionFrame;
  end;

var
  MainForm: TMainForm;

implementation

{$R *.lfm}

{ TMainForm }

procedure TMainForm.FormCreate(Sender: TObject);
begin
  FDomainFrame := TDomainFrame.Create(self);
  FDomainFrame.Parent := tsDomain;
  FDomainFrame.Align := alClient;

  FColorMapFrame := TColorMapFrame.Create(self);
  FColorMapFrame.Parent := tsColorMap;
  FColorMapFrame.Align := alClient;

  FSplineFrame := TSplineFrame.Create(self);
  FSplineFrame.Parent := tsSpline;
  FSplineFrame.Align := alClient;

  FAutoYExtentFrame := TAutoYExtentFrame.Create(self);
  FAutoYExtentFrame.Parent := tsAutoExtentY;
  FAutoYExtentFrame.Align := alClient;

  FParametricFrame := TParametricFrame.Create(self);
  FParametricFrame.Parent := tsParametric;
  FParametricFrame.Align := alClient;

  FExpressionFrame := TExpressionFrame.Create(self);
  FExpressionFrame.Parent := tsExpression;
  FExpressionFrame.Align := alClient;
end;

procedure TMainForm.PageControlChange(Sender: TObject);
begin
  FAutoYExtentFrame.Timer1.Enabled := PageControl.ActivePage = tsAutoExtentY;
end;

end.

