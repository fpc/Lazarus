{
    This file is part of the Free Component Library.
    Copyright (c) 2017 Michael Van Canneyt, member of the Free Pascal development team

    Edit form for report shape.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit frmreportshapeedit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ButtonPanel, ExtCtrls, StdCtrls, fpreport,
  fpreportlclexport;

type

  { TReportShapeEditForm }
//  TFPReportShapeType      = (stEllipse, stCircle, stLine, stSquare, stTriangle{, stArrow});  // rectangle can be handled by Frame
//  TFPReportOrientation    = (orNorth, orNorthEast, orEast, orSouthEast, orSouth, orSouthWest, orWest, orNorthWest);

  TReportShapeEditForm = class(TForm)
    BPShape: TButtonPanel;
    LPreview: TLabel;
    PBPreview: TPaintBox;
    RGOrientation: TRadioGroup;
    RGShape: TRadioGroup;
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure PBPreviewPaint(Sender: TObject);
    procedure RGShapeClick(Sender: TObject);
  private
    FRenderer : TFPReportExportCanvas;
    FLayout : TFPReportLayout;
    FShape: TFPReportCustomShape;
    procedure FormToShape;
    procedure SetShape(AValue: TFPReportCustomShape);
    procedure ShapeToForm;
  public
    Property Shape : TFPReportCustomShape Read FShape Write SetShape;
  end;

  { TDefaultReportShapeEditor }

  TDefaultReportShapeEditor = Class(TFPReportElementEditor)
    Class function DefaultClass: TFPReportElementClass; override;
    Function Execute : Boolean; override;
  end;

var
  ReportShapeEditForm: TReportShapeEditForm;

implementation

{$R *.lfm}

{ TDefaultReportShapeEditor }

class function TDefaultReportShapeEditor.DefaultClass: TFPReportElementClass;
begin
  Result:=TFPReportShape;
end;

function TDefaultReportShapeEditor.Execute: Boolean;
Var
  F : TReportShapeEditForm;

begin
  F:=TReportShapeEditForm.Create(Self);
  try
    F.Shape:=Self.Element as TFPReportCustomShape;
    Result:=F.ShowModal=mrOK;
  finally
    F.Free;
  end;
end;

{ TReportShapeEditForm }

procedure TReportShapeEditForm.PBPreviewPaint(Sender: TObject);

Var
  P : TFPReportPoint;
  O : TFPReportOrientation;

begin
  P.Left:=0;
  P.Top:=0;
  FLayout.Left:=0;
  FLayout.Top:=0;
  FLayout.Width:=((pbPreview.Width-1)*cMMperInch/PixelsPerInch);
  FLayout.Height:=((pbPreview.Height-1)*cMMperInch/PixelsPerInch);
  if RGOrientation.ItemIndex=-1 then
    O:=orNorth
  else
    O:=TFPReportOrientation(RGOrientation.ItemIndex);
  FRenderer.Canvas:=pbPreview.Canvas;
  FRenderer.HDPI:=PixelsPerInch;
  FRenderer.VDPI:=PixelsPerInch;
  PBPreview.Canvas.Brush.Color:=clWhite;
  PBPreview.Canvas.Brush.Style:=bsSolid;
  PBPreview.Canvas.Pen.Color:=clBlack;
  PBPreview.Canvas.Pen.Style:=psSolid;
  PBPreview.Canvas.FillRect(0,0,PBPreview.Canvas.Width-1,PBPreview.Canvas.Height-1);
  Case RGShape.ItemIndex of
    0 : FRenderer.RenderShapeEllipse(P,FLayout);
    1 : FRenderer.RenderShapeCircle(P,FLayout);
    2 : FRenderer.RenderShapeLine(P,O,FLayout);
    3 : FRenderer.RenderShapeRect(P,FLayout);
    4 : FRenderer.RenderShapeTriangle(P,O,FLayout);
  else
    PBPreview.Canvas.TextOut(10,10,Format('Unknown shape type: %d',[RGShape.ItemIndex]));
  end;
end;

procedure TReportShapeEditForm.RGShapeClick(Sender: TObject);
begin
  PBPreview.Invalidate;
end;

procedure TReportShapeEditForm.SetShape(AValue: TFPReportCustomShape);
begin
  if FShape=AValue then Exit;
  FShape:=AValue;
  ShapeToForm;
end;

procedure TReportShapeEditForm.ShapeToForm;

Var
  S : TFPReportShape;

begin
  S:=TFPReportShape(Shape);
  RGShape.ItemIndex:=Ord(S.ShapeType);
  RGOrientation.ItemIndex:=Ord(S.Orientation);
  PBPreview.Invalidate;
end;


procedure TReportShapeEditForm.FormCreate(Sender: TObject);
begin
  FRenderer:=TFPReportExportCanvas.Create(Self);
  FLayout:=TFPReportLayout.Create(Nil);
end;

procedure TReportShapeEditForm.FormToShape;

Var
  S : TFPReportShape;

begin
  S:=TFPReportShape(Shape);
  S.ShapeType:=TFPReportShapeType(RGShape.ItemIndex);
  S.Orientation:=TFPReportOrientation(RGOrientation.ItemIndex);
end;

procedure TReportShapeEditForm.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
  CanClose:=True;
  FormToShape;
end;

procedure TReportShapeEditForm.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FRenderer);
  FReeAndNil(FLayout);
end;

initialization
  TDefaultReportShapeEditor.RegisterEditor;
end.

