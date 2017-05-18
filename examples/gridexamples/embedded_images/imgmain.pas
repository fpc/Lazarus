unit imgMain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Grids,
  ExtCtrls, StdCtrls, LMessages, Spin, ComCtrls;

type
  { TImageData is stored in the list FImageList of the modified string grid and
    contains the image bitmap as well information on the position of the image. }
  TImageData = class
    Bitmap: TBitmap;        // Bitmap to be overlayed
    Col, Row: Integer;      // Anchor of the bitmap in the grid
    dx, dy: Integer;        // Offset with respect to anchor, in pixels
    destructor Destroy; override;
  end;

  { Events for image clicking and moving }
  TImageClickEvent = procedure(Sender: TObject; AShift: TShiftState;
    AImageIndex, X, Y: Integer) of object;
  TImageMoveEvent = procedure(Sender: TObject; AShift: TShiftState;
    AImageIndex, dx, dy: Integer) of object;

  { Modified StringGrid with support of embedded images }
  TStringGridEx = class(TStringGrid)
  private
    FImageList: TFPlist;
    FMouseImgIndex: Integer;
    FMouseDownPt: TPoint;
    FOnImageClick: TImageCLickEvent;
    FOnImageMove: TImageMoveEvent;
  protected
    procedure DoImageClick(Shift: TShiftState; X, Y: Integer); virtual;
    procedure DoImageMove(Shift: TShiftState; dx, dy: Integer); virtual;
    procedure DrawAllRows; override;
    procedure MouseDown(Button: TMouseButton; Shift:TShiftState; X,Y:Integer); override;
    procedure MouseMove(Shift: TShiftState; X,Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift:TShiftState; X,Y:Integer); override;
    procedure WMHScroll(var message : TLMHScroll); message LM_HSCROLL;
    procedure WMVScroll(var message : TLMVScroll); message LM_VSCROLL;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure AddImage(APicture: TPicture; ACol, ARow: Integer; dx, dy: Integer;
      AWidth: Integer = 0);
    function GetImageRect(AImageIndex: Integer): TRect;
    procedure MoveImageBy(AImageIndex, dx, dy: Integer);
    function PointInImage(APoint: TPoint): Integer;
    procedure UpdateImageAnchor(AImageIndex: Integer);

  published
    property OnImageClick: TImageClickEvent read FOnImageClick write FOnImageClick;
    property OnImageMove: TImageMoveEvent read FOnImageMove write FOnImageMove;
  end;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    OpenDialog1: TOpenDialog;
    Panel1: TPanel;
    Panel2: TPanel;
    SpinEdit1: TSpinEdit;
    SpinEdit2: TSpinEdit;
    StatusBar1: TStatusBar;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    Grid: TStringGridEx;
    procedure AddImageFromFile(AFileName: String; ACol, ARow: Integer;
      dx, dy: Integer; AScaledSize: Integer = 0);
    procedure ImageClickHandler(Sender: TObject; Shift: TShiftState;
      AImageIndex, X, Y: Integer);
    procedure ImageMoveHandler(Sender: TObject; Shift: TShiftState;
      AImageIndex, dX, dY: Integer);

  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

uses
  Types, LCLIntf;

const
  JPEG_FILE = '../../../images/splash_source/cheetah.jpg';
  PNG_FILE1 = '../../../images/codetoolsdefines/da_block.png';
  PNG_FILE2 = '../../../images/icons/lazarus256x256.png';

{ TForm1 }

procedure TForm1.AddImageFromFile(AFileName: String; ACol, ARow, dx, dy: Integer;
  AScaledSize: Integer = 0);
// AScaledSize = 0 means: no size scaling
var
  pic: TPicture;
begin
  pic := TPicture.Create;
  try
    pic.LoadFromFile(AFileName);
    Grid.AddImage(pic, ACol, ARow, dx, dy, AScaledSize);
  finally
    pic.Free;
  end;
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  if OpenDialog1.Execute then
    AddImageFromFile(OpenDialog1.FileName, Grid.Col, Grid.Row, SpinEdit1.Value, SpinEdit2.Value);
end;

procedure TForm1.FormCreate(Sender: TObject);
var
  i: Integer;
begin
  Grid := TStringGridEx.Create(self);
  Grid.Parent := self;
  Grid.Align := alClient;
  Grid.RowCount := 13; //100;
  Grid.ColCount := 10; //30;
  Grid.Options := Grid.Options + [goThumbTracking, goColSizing, goRowSizing, goEditing];
  Grid.MouseWheelOption := mwGrid;
  Grid.Col := 3;
  Grid.Row := 16;
  Grid.OnImageClick := @ImageClickHandler;
  Grid.OnImageMove := @ImageMoveHandler;

  for i:=1 to Grid.ColCount-1 do
    Grid.Cells[i, 0] := 'column ' + IntToStr(i);
  for i:=1 to Grid.RowCount-1 do
    Grid.Cells[0, i] := 'row ' + IntToStr(i);

  ActiveControl := Grid;

  //                         col row dx dy width
  AddImageFromFile(JPEG_FILE,  1,  1, 5, 5, 300);  // Offset by 5 pixels, scaled to width 300
  AddImageFromFile(PNG_FILE1,  2, 12, 0, 0);
  AddImageFromFile(PNG_FILE2,  5,  2, 0, 0);
end;

procedure TForm1.ImageClickHandler(Sender: TObject; Shift: TShiftState;
  AImageIndex, X, Y: Integer);
begin
  Statusbar1.SimpleText := Format('Image #%d clicked at x = %d, y = %d', [AImageIndex, X, Y]);
end;

procedure TForm1.ImageMoveHandler(Sender: TObject; Shift: TShiftState;
  AImageIndex, dX, dY: Integer);
begin
  if [ssLeft, ssCtrl] * Shift = [ssLeft, ssCtrl] then begin
    // Moves the image by dx, dy pixels
    Grid.MoveImageBy(AImageIndex, dx, dy);
    // Resets the image anchor such that the top-left image corner is contained
    // in the anchor cell.
    Grid.UpdateImageAnchor(AImageIndex);
  end;
end;


{ TImageData }

destructor TImageData.Destroy;
begin
  Bitmap.Free;
  inherited;
end;


{ Modified TStringGrid }

constructor TStringGridEx.Create(AOwner: TComponent);
begin
  inherited;
  FImageList := TFPList.Create;
end;

destructor TStringGridEx.Destroy;
var
  j: Integer;
begin
  for j:=0 to FImageList.Count-1 do
    TImageData(FImageList[j]).Free;
  FImageList.Free;
  inherited;
end;

{ Adds a new image to the image list. ACol and ARow indicate the column and
  row index of the anchor cell to which the image is attached. If the left or
  top edge of the anchor cell is moved then the image follows. The image can
  be shifted by dx and dy pixels from the top left corner of the anchor cell.
  AWidth defines the width of the image in pixels; the image is automatically
  rescaled. If AWidth is missing (or 0) the original size of the image is used.}
procedure TStringGridEx.AddImage(APicture: TPicture; ACol, ARow: Integer;
  dx, dy: Integer; AWidth: Integer = 0);
var
  bmp: TBitmap;
  imgdata: TImageData;
begin
  bmp := TBitmap.Create;
  if AWidth <= 0 then begin
    // Keep original image size
    bmp.Width := APicture.Width;
    bmp.Height := APicture.Height;
  end else begin
    // Scale image
    bmp.Width := AWidth;
    bmp.Height := round(bmp.Width / APicture.Width * APicture.Height);
  end;
  bmp.PixelFormat := pf32Bit;
  bmp.Canvas.Brush.Color := clWhite;
  bmp.Canvas.FillRect(0, 0, bmp.Width, bmp.Height);
  bmp.Canvas.StretchDraw(Rect(0, 0, bmp.Width, bmp.Height), APicture.Graphic);

  imgData := TImageData.Create;
  imgData.Bitmap := bmp;
  imgData.Col := ACol;
  imgData.Row := ARow;
  imgData.dx := dx;
  imgData.dy := dy;
  FImageList.Add(imgData);

  Invalidate;
end;

procedure TStringGridEx.DoImageClick(Shift: TShiftState; X, Y: Integer);
begin
  if Assigned(FOnImageClick) then
    FOnImageClick(self, Shift, FMouseImgIndex, X, Y);
end;

procedure TStringGridEx.DoImageMove(Shift: TShiftState; dx, dy: Integer);
begin
  if Assigned(FOnImageMove) then
    FOnImageMove(self, Shift, FMouseImgIndex, dx, dy);
end;

procedure TStringGridEx.DrawAllRows;
var
  imgdata: TImagedata;
  i: Integer;
  clipArea: TRect;
  ImgRect: TRect;
  R: TRect;
  tmp: Integer;
begin
  inherited;

  // Calculate the clip area, i.e. the rectangle enclosing the non-fixed cells...
  clipArea := Canvas.ClipRect;
  ColRowToOffset(true, false, FixedCols, clipArea.Left, tmp);
  ColRowToOffset(false, false, FixedRows, clipArea.Top, tmp);

  Canvas.SaveHandleState;
  try
    // ... and use it for clipping
    IntersectClipRect(Canvas.Handle, clipArea.Left, clipArea.Top, clipArea.Right, clipArea.Bottom);

    for i := 0 to FImageList.Count-1 do begin
      // Get bounding rectangle of the image
      imgRect := GetImageRect(i);

      // Calculate the intersection of the image rectangle with the clip rectangle
      // Nothing to do if the image rectangle does not intersect the clip rectangle
      if not IntersectRect(R, clipArea, imgRect) then
        continue;

      imgdata := TImageData(FImageList[i]);
      Canvas.Draw(imgRect.Left, imgRect.Top, imgData.Bitmap);
    end;

  finally
    Canvas.RestoreHandlestate;
  end;
end;

{ Extracts, in pixels, the bounding rectangle of the image with the
  specified index }
function TStringGridEx.GetImageRect(AImageIndex: Integer): TRect;
var
  imgdata: TImageData;
begin
  if (AImageIndex >= 0) and (AImageIndex < FImageList.Count) then
  begin
    // Extract image data from image list
    imgdata := TImageData(FImageList[AImageIndex]);
    // Find coordinates of cell anchor
    Result := CellRect(imgdata.Col, imgdata.Row);
    // Fix size of the image rectangle
    Result.Right := Result.Left + imgData.Bitmap.Width;
    Result.Bottom := Result.Top + imgData.Bitmap.Height;
    // Shift image to final position
    OffsetRect(Result, imgdata.dx, imgdata.dy);
  end else
    Result := Rect(0, 0, 0, 0);
end;

procedure TStringGridEx.MouseDown(Button: TMouseButton; Shift:TShiftState;
  X,Y:Integer);
var
  idx: Integer;
begin
  idx := PointInImage(Point(X, Y));
  if idx > -1 then begin
    FMouseDownPt := Point(X, Y);
    FMouseImgIndex := idx;
    DoImageClick(Shift, X, Y);
    Abort;
  end else
    inherited;
end;

procedure TStringGridEx.MouseMove(Shift: TShiftState; X,Y: Integer);
begin
  if (FMouseImgIndex > -1) then begin
    DoImageMove(Shift, X - FMouseDownPt.X, Y - FMouseDownPt.Y);
    FMouseDownPt := Point(X, Y);
  end
  else
    inherited;
end;

procedure TStringGridEx.MouseUp(Button: TMouseButton; Shift:TShiftState;
  X,Y:Integer);
begin
  FMouseImgIndex := -1;
  inherited;
end;

{ Moves the image by dx pixels horizontally and dy pixels vertically.
  It is recommended to call UpdateImageAnchor afterwards. If this is not done
  then it is not clear whether an image will move if column widths are changed. }
procedure TStringGridEx.MoveImageBy(AImageIndex, dx, dy: Integer);
var
  imgData: TImageData;
begin
  if (AImageIndex > -1) and (AImageIndex < FImageList.Count) then
  begin
    imgData := TImageData(FImageList[AImageIndex]);
    inc(imgData.dx, dx);
    inc(imgData.dy, dy);
    Invalidate;
  end;
end;

{ Finds the index of the image which contains the specified point. Images
  are checked in reverse order, this means that in case of overlapping images
  the "top" one is selected.
  NOTE: Transparent areas of images are ignored, it is always the enclosing
  rectangle of the entire image which is checked. }
function TStringGridEx.PointInImage(APoint: TPoint): Integer;
var
  imgRect: TRect;
  topleftPx: TPoint;
  i: Integer;
begin
  topleftPx := GetPxTopLeft;
  APoint.X := APoint.X + topleftPx.X;
  APoint.Y := APoint.Y + topleftPx.Y;
  for i:=FImageList.Count-1 downto 0 do begin
    imgRect := GetImageRect(i);
    if PtInRect(imgRect, APoint) then
      exit(i);
  end;
  Result := -1;
end;

{ Recalculates the anchor cell to which the image is attached. The anchor cell
  is always the cell which contains the upper left corner of the image. It
  may change when an image is dragged across the grid.
  This method should be called after moving an image because otherwise it will
  not be clear whether an image moves if column widths are changed. }
procedure TStringGridEx.UpdateImageAnchor(AImageIndex: Integer);
var
  imgdata: TImageData;
  cell: TPoint;
  R: TRect;
  P: TPoint;
begin
  if (AImageIndex < 0) or (AImageIndex >= FImageList.Count) then
    exit;
  R := GetImageRect(AImageIndex);         // Current pixel coordinates of image
  cell := MouseToLogCell(R.TopLeft);      // Col/row of cell with top left image corner
  P := CellRect(cell.X, cell.Y).TopLeft;  // Pixel coordinates of top left corner of anchor cell
  imgdata := TImageData(FImageList[AImageIndex]);
  imgData.Col := cell.X;
  imgData.Row := cell.Y;
  imgData.dx := R.Left - P.X;
  imgData.dy := R.Top - P.Y;
end;

procedure TStringGridEx.WMHScroll(var message: TLMHScroll);
begin
  inherited;
  Invalidate;
end;

procedure TStringGridEx.WMVScroll(var message: TLMVScroll);
begin
  inherited;
  Invalidate;
end;

end.

