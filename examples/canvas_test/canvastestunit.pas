unit canvastestunit;

{$mode objfpc}{$H+}

interface

uses
  // RTL, FCL, LCL
  Forms, StdCtrls,
  // Project units
  rectanglestest, brushtest, pentest, fonttest, shapedwindowtest,
  ellipseunit, screenshotunit, drawtest, imagetest, systemcolorstest;

type

  { TfrmCanvasTest }

  TfrmCanvasTest = class(TForm)
    btnBrush: TButton;
    btnSystemColors: TButton;
    btnPen: TButton;
    btnFont: TButton;
    btnClose: TButton;
    btnRectangles: TButton;
    btnShapedWindow: TButton;
    btnEllipse: TButton;
    btnScreenshot: TButton;
    btnDraw: TButton;
    btnImage: TButton;
    procedure btnBrushClick(Sender: TObject);
    procedure btnCloseClick(Sender: TObject);
    procedure btnDrawClick(Sender: TObject);
    procedure btnEllipseClick(Sender: TObject);
    procedure btnFontClick(Sender: TObject);
    procedure btnImageClick(Sender: TObject);
    procedure btnPenClick(Sender: TObject);
    procedure btnRectanglesClick(Sender: TObject);
    procedure btnScreenshotClick(Sender: TObject);
    procedure btnShapedWindowClick(Sender: TObject);
    procedure btnSystemColorsClick(Sender: TObject);
  private

  public

  end; 

var
  frmCanvasTest: TfrmCanvasTest;

implementation

{$R *.lfm}

{ TfrmCanvasTest }

procedure TfrmCanvasTest.btnBrushClick(Sender: TObject);
begin
  frmBrush.Show;
end;

procedure TfrmCanvasTest.btnCloseClick(Sender: TObject);
begin
  Close;
end;

procedure TfrmCanvasTest.btnDrawClick(Sender: TObject);
begin
  frmDraw.Show;
end;

procedure TfrmCanvasTest.btnEllipseClick(Sender: TObject);
begin
  frmEllipse.Show;
end;

procedure TfrmCanvasTest.btnFontClick(Sender: TObject);
begin
  frmFont.Show;
end;

procedure TfrmCanvasTest.btnImageClick(Sender: TObject);
begin
  frmImage.Show;
end;

procedure TfrmCanvasTest.btnPenClick(Sender: TObject);
begin
  frmPen.Show;
end;

procedure TfrmCanvasTest.btnRectanglesClick(Sender: TObject);
begin
  frmRectangles.Show;
end;

procedure TfrmCanvasTest.btnScreenshotClick(Sender: TObject);
begin
  frmScreenshot.Show;
end;

procedure TfrmCanvasTest.btnShapedWindowClick(Sender: TObject);
begin
  frmShapedWindow.Show;
end;

procedure TfrmCanvasTest.btnSystemColorsClick(Sender: TObject);
begin
  frmSystemColors.Show;
end;

end.

