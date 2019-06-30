{****************************************************}
{                                                    }
{             FastReport v2.3                        }
{            PDF export filter Ver 1.0               }
{                                                    }
{       By : Ricardo Cardona Ramirez                 }
{                                                    }
{ PowerPDF                                           }
{ http://www.est.hi-ho.ne.jp/takeshi_kanno/powerpdf/ }
{ ZLib Units Delphi 5-6                              }
{ http://www.base2ti.com/zlib.htm                    }
{                                                    }
{****************************************************}

unit lr_e_pdf;

{$mode objfpc}{$H+}

interface

uses
    SysUtils, Classes, Graphics, Forms, StdCtrls, lr_class, lr_BarC,
    lr_shape, PdfDoc, PdfTypes, PdfFonts, PRJpegImage, PReport, Dialogs,
    Controls, lr_rrect;

type
    TShapeData = record
      ShapeType: TfrShapeType;
      FillColor: TColor;
      FrameStyle: TfrFrameStyle;
      FrameWidth: Double;
      FrameColor: TColor;
      Radius: Single;
      Corners: TCornerSet;
      GradientColor: TColor;
      GradientDirection: TGradientDirection;
    end;

    TfrTNPDFExport = class(TComponent) // fake component
    end;

    { TfrTNPDFExportFilter }

    TfrTNPDFExportFilter = class(TfrExportFilter)
    private
        NewPage: Boolean;
        PDF: TPReport;
        PPage: TPRPage;
        PRPanel: TPRPanel;
        FOutline: TPROutLineEntry;
        FPageNo : Integer;
        DummyControl: TForm;
        procedure AddShape(Data: TShapeData; x, y, h, w: integer);
        procedure DefaultShowView(View: TfrView; nx, ny, ndy, ndx: Integer);
    public
        constructor Create(AStream: TStream); override;
        destructor Destroy; override;
        procedure OnBeginPage; override;
        procedure OnEndDoc; override;
        procedure OnEndPage; override;
        procedure ShowBackGround(View: TfrView; x, y, h, w: integer);
        procedure Frame(View: TfrView; x, y, h, w: integer);
        procedure ShowFrame(View: TfrView; x, y, h, w: integer);
        procedure ShowBarCode(View: TfrCustomBarCodeView; x, y, h, w: integer);
        procedure ShowPicture(View: TfrPictureView; x, y, h, w: integer);
        procedure ShowRoundRect(View: TfrRoundRectView; x, y, h, w: integer);
        procedure ShowShape(View: TfrShapeView; x, y, h, w: integer);
        procedure OnText(X, Y: Integer; const Text: string; View: TfrView);
            override;
        procedure OnData(x, y: Integer; View: TfrView); override;
    end;

implementation

uses lr_Const;

type
    TfrMemoView_ = class(TfrMemoView);
    TPRText_ = class(TPRText);

const
    PDFEscx = 0.792553191;
    PDFEscy = 0.785447761;

procedure TfrTNPDFExportFilter.AddShape(Data: TShapeData; x, y, h, w: integer);

  function CreateShape(ShapeClass: TPRShapeClass): TPRShape;
  begin
    result := ShapeClass.Create(PRPanel);
    result.Parent := PRPanel;
    result.FillColor := Data.FillColor;
    result.Left := x;
    result.Top := y;
    result.Height := h;
    result.Width := w;
    result.LineStyle := TPenStyle(Data.FrameStyle);
    result.LineWidth := Data.FrameWidth - 0.5;
    result.LineColor := Data.FrameColor;
  end;

begin
  case Data.ShapeType of
    frstRectangle:
      CreateShape(TPRRect);

    frstEllipse:
      CreateShape(TPREllipse);

    frstRoundRect:
      with TPRRect(CreateShape(TPRRect)) do begin
        Radius := Data.Radius;
        SquaredCorners := TPdfCorners(Data.Corners);
        GradientColor := Data.GradientColor;
        GradientDirection := Data.GradientDirection;
      end;

    frstTriangle:
      with TPRPolygon(CreateShape(TPRPolygon)) do begin
        SetLength(Points, 3);
        Points[0] := PRPoint(x+w, y+h);
        Points[1] := PRPoint(x, y+h);
        Points[2] := PRPoint(x+w/2, y);
      end;

    frstDiagonal1:
      with TPRPolygon(CreateShape(TPRPolygon)) do begin
        SetLength(Points, 2);
        Points[0] := PRPoint(x,y);
        Points[1] := PRPoint(x+w,y+h);
      end;

    frstDiagonal2:
      with TPRPolygon(CreateShape(TPRPolygon)) do begin
        SetLength(Points, 2);
        Points[0] := PRPoint(x,y+h);
        Points[1] := PRPoint(x+w,y);
      end;
  end;
end;

procedure TfrTNPDFExportFilter.DefaultShowView(View: TfrView;
  nx, ny, ndy, ndx: Integer);
begin
  if (View.FillColor <> clNone)
     and not (View is TfrCustomBarCodeView)
     and not (View is TfrPictureView)
  then
    ShowBackGround(View, nx, ny, ndy, ndx);

  if View is TfrCustomBarCodeView then
      ShowBarCode(TfrCustomBarCodeView(View), nx, ny, ndy, ndx)
  else if View is TfrPictureView then
      ShowPicture(TfrPictureView(View), nx, ny, ndy, ndx);

  if (View.Frames<>[]) and not (View is TfrCustomBarCodeView) then
     ShowFrame(View, nx, ny, ndy, ndx);
end;

constructor TfrTNPDFExportFilter.Create(AStream: TStream);
begin
    inherited;
    PDF := TPReport.Create(nil);
    PDF.CompressionMethod := cmFlateDecode;
    PDF.UseOutlines := True;
    PDF.PageLayout := plOneColumn;
    PDF.BeginDoc;
    DummyControl := TForm.Create(nil);
    NewPage := False;
    FPageNo := 0;
end;

destructor TfrTNPDFExportFilter.Destroy;
begin
    PDF.Free;
    DummyControl.Free;
    inherited;
end;

procedure TfrTNPDFExportFilter.OnBeginPage;
begin
    {Add New Page}
    Inc(FPageNo);

    PPage := TPRPage.Create(PDF);
    PPage.Parent := DummyControl;
    PPage.MarginBottom := 0;
    PPage.MarginTop := 0;
    PPage.MarginLeft := 0;
    PPage.MarginRight := 0;

    PPage.Height := trunc(CurReport.EMFPages[FPageNo - 1]^.PrnInfo.Pgh*PDFEscy + 0.5);
    PPage.Width := trunc(CurReport.EMFPages[FPageNo - 1]^.PrnInfo.Pgw*PDFEscx + 0.5);

    PRPanel := TPRPanel.Create(PPage);
    PRPanel.Parent := PPage;
    PRPanel.Left := 0;
    PRPanel.Top := 0;
    PRPanel.Width := PPage.Width;
    PRPanel.Height := PPage.Height;
end;

procedure TfrTNPDFExportFilter.OnEndDoc;
begin
  PDF.GetPdfDoc.SaveToStream(Stream);
end;

procedure TfrTNPDFExportFilter.OnEndPage;
begin
    PDF.Print(PPage);

    FOutline := PDF.OutlineRoot.AddChild;
    FOutline.Dest := PDF.CreateDestination;
    FOutline.Dest.Top := 0;
    FOutline.Title := 'Page ' + IntToStr(FPageNo);

    FreeAndNil(PPage);
end;

procedure TfrTNPDFExportFilter.ShowBackGround(View: TfrView; x, y, h, w:
    integer);
var
    PRRect: TPRRect;
begin
    PRRect := TPRRect.Create(PRPanel);
    PRRect.Parent := PRPanel;
    PRRect.FillColor := ColorToRGB(View.FillColor);
    PRRect.LineColor := clNone;
    PRRect.LineStyle := psSolid;
    PRRect.Left := x;
    PRRect.Top := y;
    PRRect.Height := h;
    PRRect.Width := w;
end;

procedure TfrTNPDFExportFilter.Frame(View: TfrView; x, y, h, w: integer);
var
    PRRect: TPRRect;
begin
    PRRect := TPRRect.Create(PRPanel);
    PRRect.Parent := PRPanel;
    PRRect.FillColor := clNone;

    PRRect.Left := x;
    PRRect.Top := y;
    PRRect.Height := h;
    PRRect.Width := w;

    PRRect.LineStyle := TPenStyle(View.FrameStyle);
    PRRect.LineWidth := View.FrameWidth - 0.5;
    PRRect.LineColor := View.FrameColor;
end;

procedure TfrTNPDFExportFilter.ShowFrame(View: TfrView; x, y, h, w: integer);
begin

  if ([frbLeft,frbTop,frbRight,frbBottom]-View.Frames=[]) and
     (View.FrameStyle = frsSolid) then
  begin
    Frame(View, x, y, h, w);
  end
  else
  begin
    if frbRight in View.Frames then
      Frame(View, x + w - 1, y, h, 0); //Right
    if frbLeft in View.Frames then
      Frame(View, x, y, h, 0); //Left
    if frbBottom in View.Frames then
      Frame(View, x, y + h - 1, 0, w); //Botton
    if frbTop in View.Frames then
      Frame(View, x, y, 0, w); //Top
  end;
end;

procedure TfrTNPDFExportFilter.ShowBarCode(View: TfrCustomBarCodeView; x, y, h, w:
    integer);
var
    Bitmap: TBitmap;
    PRImage: TPRImage;
    oldX, oldY: Integer;
begin
    oldX := View.x;
    oldy := View.y;
    View.x := 0;
    View.y := 0;

    Bitmap := TfrCustomBarCodeView(View).GenerateBitmap;
    try
        w := trunc(Bitmap.Width * PDFEscx + 1.5) ;
        h := trunc(Bitmap.Height * PDFEscy + 1.5) ;

        PRImage := TPRImage.Create(PRPanel);
        PRImage.Parent := PRPanel;
        PRImage.Stretch := True;
        PRImage.SharedImage := False;
        PRImage.Left := x;
        PRImage.Top := y;
        PRImage.Height := h;
        PRImage.Width := w;

        PRImage.Picture.Bitmap := Bitmap;
    finally
        FreeAndNil(Bitmap);
    end;

    View.x := oldX;
    View.y := oldY;
end;

procedure TfrTNPDFExportFilter.ShowPicture(View: TfrPictureView; x, y, h,
    w: integer);
var
  PRImage: TPRImage;
  r: Double;
  L: Integer;
  pw, ph: Integer;
  Picture: TPicture;
begin
  Picture := View.Picture;

  if Picture.Graphic is TJpegImage then
    PRImage := TPRJpegImage.Create(PRPanel)
  else
    PRImage := TPRImage.Create(PRPanel);

  PRImage.Parent := PRPanel;

  ph := h;
  pw := w;

  if view.Stretched then
  begin
    if (View.Flags and flPictRatio<>0) and
       (Picture.Width>0) and (Picture.Height>0) then
    begin
      r  := Picture.Width/Picture.Height;
      if (w/h) < r then
      begin
        L := h;
        ph := trunc(w/r + 0.5);
        if (View.Flags and flPictCenter<>0) then
          y := y + (L-ph) div 2;
      end
      else
      begin
        L := w;
        pw := trunc(h*r + 0.5);
        if (View.Flags and flPictCenter<>0) then
          x := x + (L-pw) div 2;
      end;
    end;
  end
  else begin
    PRImage.ScaleX := PDFEscX;
    PRImage.ScaleY := PDFEscY;
    if (View.Flags and flPictCenter<>0) then begin
      pw := trunc(Picture.Width * PDFEscX + 1.5);
      ph := trunc(Picture.Height * PDFEscY + 1.5);
       x := x + (w - pw) div 2 - 1;
       y := y + (h - ph) div 2 - 1;
    end;
  end;

  PRImage.Stretch := View.Stretched;
  PRImage.SharedName := View.SharedName;
  PRImage.SharedImage := (View.SharedName<>'');

  PRImage.Left := x;
  PRImage.Top := y;
  PRImage.Height := ph;
  PRImage.Width := pw;

  PRImage.Picture.Graphic := Picture.Graphic;
end;

procedure TfrTNPDFExportFilter.ShowRoundRect(View: TfrRoundRectView; x, y, h,
  w: integer);
var
  Data: TShapeData;
  SWidth: Integer;
begin

  if view.ShowGradian and (View.GradianStyle in [gsElliptic, gsHorizCenter, gsVertCenter, gsRectangle]) then
    // not supported yet
    DefaultShowView(View, x, y, h, w)

  else
  begin

    SWidth := trunc((View.RoundRectCurve/2) * PDFEscx + 0.5);
    if View.RoundRect then
      Data.Radius := SWidth
    else
      Data.Radius := 0.0;
    Data.Corners:=View.SquaredCorners;

    // draw shadow
    if View.ShowGradian then
    begin
      Data.GradientColor := View.ShadowColor;
      case View.GradianStyle of
        gsVertical:   Data.GradientDirection := gdVertical;
        gsHorizontal: Data.GradientDirection := gdHorizontal;
      end;
    end else
    begin
      Data.GradientColor := clNone;
      Data.ShapeType := frstRoundRect;
      Data.FillColor := View.ShadowColor;
      Data.FrameColor := clNone;
      Data.FrameWidth := 0;
      Data.FrameStyle := frsSolid;
      SWidth := trunc(View.ShadowWidth * PDFEscx + 0.5);
      if View.ShadowWidth>0 then
        AddShape(Data, x + SWidth, y + SWidth, h - SWidth, w - SWidth);
    end;

    // draw roundrect
    Data.ShapeType := frstRoundRect;
    if View.FillColor=clNone then begin
      if not View.ShowGradian and (View.ShadowWidth>0) then
        Data.FillColor := clWhite
      else
        Data.FillColor := clNone
    end
    else
      Data.FillColor := ColorToRGB(View.FillColor);
    if View.Frames=[] then
      Data.FrameColor := clNone
    else
      Data.FrameColor := View.FrameColor;
    Data.FrameWidth := View.FrameWidth;
    Data.FrameStyle := View.FrameStyle;
    AddShape(Data, x, y, h - SWidth, w - SWidth);
  end;
end;

procedure TfrTNPDFExportFilter.ShowShape(View: TfrShapeView; x, y, h, w: integer);
var
  Data: TShapeData;
begin
  Data.ShapeType := View.ShapeType;
  Data.FillColor := View.FillColor;
  Data.FrameColor := View.FrameColor;
  Data.FrameStyle := View.FrameStyle;
  Data.FrameWidth := View.FrameWidth;
  Data.Radius := -1.0;
  Data.Corners := [];
  AddShape(Data, x, y, h, w);
end;

procedure TfrTNPDFExportFilter.OnData(x, y: Integer; View: TfrView);
var
    nx, ny, ndx, ndy: Integer;
begin
    nx := trunc(x * PDFEscx + 0.5);
    ny := trunc(y * PDFEscy + 0.5);
    ndx := trunc((View.dx) * PDFEscx + 1.5) ;
    ndy := trunc((View.dy) * PDFEscy + 1.5) ;

    if View is TfrShapeView then begin

      ShowShape(TfrShapeView(View), nx, ny, ndy, ndx);

    end else
    if View is TfrRoundRectView then begin

      ShowRoundRect(TfrRoundRectView(View), nx, ny, ndy, ndx);

    end else
      DefaultShowView(View, nx, ny, ndy, ndx);
end;

procedure TfrTNPDFExportFilter.OnText(X, Y: Integer; const Text: string;
    View: TfrView);
var
    PRTLabel: TPRLabel;
    nx, ny, ndx, ndy: Integer;
    gapx, gapy: integer;
    memo: TfrMemoView;
begin
    gapx := trunc(View.FrameWidth / 2 + 0.5) + 2;
    gapy := trunc(View.FrameWidth / 4 + 0.5) + 1;
    nx := trunc((x+gapx)  * PDFEscx + 0.5);
    ny := trunc((y+gapy) * PDFEscy + 0.5);
    ndx := trunc((View.dx-gapx) * PDFEscx + 1.5);
    ndy := trunc((View.dy-gapy) * PDFEscy + 1.5);

    PRTLabel := TPRLabel.Create(PRPanel);
    PRTLabel.Parent := PRPanel;
    PRTLabel.Clipping := true;
    try
      PRTLabel.Caption := Text;
      PRTLabel.Left := nx;
      PRTLabel.Top := ny;
      PRTLabel.Width := ndx;
      PRTLabel.Height := ndy;
      if View is TfrMemoView then
      begin
        memo := View as TfrMemoView;
        PRTLabel.Alignment :=  memo.Alignment;
        if Pos('Arial', memo.Font.Name) > 0 then
          PRTLabel.FontName := fnArial
        else if Pos('Courier', memo.Font.Name) > 0 then
          PRTLabel.FontName := fnFixedWidth
        else if Pos('Times', memo.Font.Name) > 0 then
          PRTLabel.FontName := fnTimesRoman;
        PRTLabel.FontSize := memo.Font.Size;
        PRTLabel.FontBold := fsBold in memo.Font.Style;
        PRTLabel.FontItalic := fsItalic in memo.Font.Style;
        PRTLabel.FontColor := ColorToRGB(memo.Font.Color);
        PRTLabel.FontUnderline := fsUnderline in memo.Font.Style;
        PRTLabel.Angle:= memo.Angle;
        PRTLabel.AlignJustified :=  memo.Justify and not memo.LastLine;
      end;
    finally
    end;
end;



initialization
    frRegisterExportFilter(TfrTNPDFExportFilter, 'Adobe Acrobat PDF ' + ' (*.pdf)',
        '*.pdf');

end.

