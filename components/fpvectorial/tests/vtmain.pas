{ Current issues:
  - Radial gradient not rendered correctly (position, colors), saving to svg ok.
  - Save polygon to svg empty
  - Nonzero/even-odd winding rule not working
}

unit vtmain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, ComCtrls, EditBtn, fpimage, fpvectorial, Types;

type

  TRenderEvent = procedure(APage: TvVectorialPage) of object;

  TRenderParams = class
    RefFile: String;
    OnRender: TRenderEvent;
    constructor Create(ARenderEvent: TRenderEvent; ARefFilename: String);
  end;

  { TMainForm }

  TMainForm = class(TForm)
    BtnSaveAsRef: TButton;
    BtnSaveAsWMF: TButton;
    BtnSaveAsSvg: TButton;
    BtnViewWMF: TButton;
    BtnViewSVG: TButton;
    gbWMF: TGroupBox;
    gbSVG: TGroupBox;
    gbRenderTest: TGroupBox;
    gbBottomLeft: TGroupBox;
    gbTopLeft: TGroupBox;
    gbReferenceImageTest: TGroupBox;
    GroupBox1: TGroupBox;
    gbReadWriteTest: TGroupBox;
    GbTree: TGroupBox;
    Label14: TLabel;
    LblBothImagesMustMatch1: TLabel;
    RefImage: TImage;
    Label10: TLabel;
    Label11: TLabel;
    Label13: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    LblBothImagesMustMatch: TLabel;
    LblRefImgMustMatch: TLabel;
    LblRefImgMustMatch1: TLabel;
    BottomLeftPaintbox: TPaintBox;
    ScrollBox1: TScrollBox;
    TopLeftPaintbox: TPaintBox;
    SVGPaintbox: TPaintBox;
    WMFPaintBox: TPaintBox;
    AllTestsPanel: TPanel;
    Tree: TTreeView;
    procedure BtnSaveToFileClick(Sender: TObject);
    procedure BtnSaveAsRefClick(Sender: TObject);
    procedure BtnViewImageClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure PaintBoxPaint(Sender: TObject);
    procedure TreeCustomDrawItem(Sender: TCustomTreeView; Node: TTreeNode;
      State: TCustomDrawState; var DefaultDraw: Boolean);
    procedure TreeSelectionChanged(Sender: TObject);

  private
    { private declarations }
    FDocTopLeft: TvVectorialDocument;
    FDocBottomLeft: TvVectorialDocument;
    FDocFromWMF: TvVectorialDocument;
    FDocFromSVG: TvVectorialDocument;
    procedure CreateDocument(var ADoc: TvVectorialDocument;
      var APage: TvVectorialPage; AUseTopLeftCoords: boolean);
    procedure Populate;
    procedure ReadIni;
    procedure ShowFileImage(AFilename: String);
    procedure ShowReadWriteTestImages;
    procedure ShowRefImageTest;
    procedure ShowRenderTestImages;
    procedure UpdateCmdStates;
    procedure WriteIni;

    // Simple shapes
    procedure Render_Rect_Solid(APage: TvVectorialPage);
    procedure Render_RoundedRect_Solid(APage: TvVectorialPage);
    procedure Render_Circle_Solid(APage: TvVectorialPage);
    procedure Render_Ellipse_Solid(APage: TvVectorialPage);
    procedure Render_Polygon_Solid(APage: TvVectorialPage);

    // Complex shapes
    procedure Render_Path_Hole_SolidFill(APage: TvVectorialPage);
    procedure Render_Path_Hole_GradientFill(APage: TvVectorialPage);
    procedure Render_SelfIntersectingPoly_SolidFill_EvenOdd(APage: TvVectorialPage);
    procedure Render_SelfIntersectingPoly_GradientFill_EvenOdd(APage: TvVectorialPage);
    procedure Render_SelfIntersectingPoly_SolidFill_NonZeroWinding(APage: TvVectorialPage);
    procedure Render_SelfIntersectingPoly_GradientFill_NonZeroWinding(APage: TvVectorialPage);

    // Gradients
    procedure Render_Circle_GradientHor(APage: TvVectorialPage);
    procedure Render_Circle_GradientVert(APage: TvVectorialPage);
    procedure Render_Circle_GradientLinear(APage: TvVectorialPage);
    procedure Render_Circle_GradientRadial(APage: TvVectorialPage);
    procedure Render_Ellipse_GradientHor(APage: TvVectorialPage);
    procedure Render_Ellipse_GradientVert(APage: TvVectorialPage);
    procedure Render_Ellipse_GradientLinear(APage: TvVectorialPage);
    procedure Render_Ellipse_GradientRadial(APage: TvVectorialPage);
    procedure Render_Rect_GradientHor(APage: TvVectorialPage);
    procedure Render_Rect_GradientVert(APage: TvVectorialPage);
    procedure Render_Rect_GradientLinear(APage: TvVectorialPage);
    procedure Render_Rect_GradientRadial(APage: TvVectorialPage);
    procedure Render_RoundedRect_GradientHor(APage: TvVectorialPage);
    procedure Render_RoundedRect_GradientVert(APage: TvVectorialPage);
    procedure Render_RoundedRect_GradientLinear(APage: TvVectorialPage);
    procedure Render_RoundedRect_GradientRadial(APage: TvVectorialPage);
    procedure Render_Polygon_GradientHor(APage: TvVectorialPage);
    procedure Render_Polygon_GradientVert(APage: TvVectorialPage);
    procedure Render_Polygon_GradientLinear(APage: TvVectorialPage);
    procedure Render_Polygon_GradientRadial(APage: TvVectorialPage);

    // Arcs
    procedure Render_Arc_CW_Q1(APage: TvVectorialPage);
    procedure Render_Arc_CW_Q12(APage: TvVectorialPage);
    procedure Render_Arc_CW_Q2(APage: TvVectorialPage);
    procedure Render_Arc_CW_Q23(APage: TvVectorialPage);
    procedure Render_Arc_CW_Q3(APage: TvVectorialPage);
    procedure Render_Arc_CW_Q34(APage: TvVectorialPage);
    procedure Render_Arc_CW_Q4(APage: TvVectorialPage);
    procedure Render_Arc_CW_Q41(APage: TvVectorialPage);
    procedure Render_Arc_CCW_Q1(APage: TvVectorialPage);
    procedure Render_Arc_CCW_Q12(APage: TvVectorialPage);
    procedure Render_Arc_CCW_Q2(APage: TvVectorialPage);
    procedure Render_Arc_CCW_Q23(APage: TvVectorialPage);
    procedure Render_Arc_CCW_Q3(APage: TvVectorialPage);
    procedure Render_Arc_CCW_Q34(APage: TvVectorialPage);
    procedure Render_Arc_CCW_Q4(APage: TvVectorialPage);
    procedure Render_Arc_CCW_Q41(APage: TvVectorialPage);

    // Text
    procedure Render_Text(APage: TvVectorialPage; Anchor: TvTextAnchor; Angle: Double);
    procedure Render_Text_Left(APage: TvVectorialPage);
    procedure Render_Text_Center(APage: TvVectorialPage);
    procedure Render_Text_Right(APage: TvVectorialPage);
    procedure Render_Text_Left_30deg(APage: TvVectorialPage);
    procedure Render_Text_Center_30deg(APage: TvVectorialPage);
    procedure Render_Text_Right_30deg(APage: TvVectorialPage);
    procedure Render_Text_Left_90deg(APage: TvVectorialPage);
    procedure Render_Text_Center_90deg(APage: TvVectorialPage);
    procedure Render_Text_Right_90deg(APage: TvVectorialPage);
    procedure Render_Text_Left_m90deg(APage: TvVectorialPage);
    procedure Render_Text_Center_m90deg(APage: TvVectorialPage);
    procedure Render_Text_Right_m90deg(APage: TvVectorialPage);
    procedure Render_Text_Fonts(APage: TvVectorialPage);
    procedure Render_Text_Colors(APage: TvVectorialPage);
  public
    { public declarations }
  end;

var
  MainForm: TMainForm;


implementation

{$R *.lfm}

uses
  FPCanvas, Process, IniFiles, LazFileUtils, LCLIntf,
  fpvutils, vtprimitives;

const
  IMG_FOLDER = 'images' + PathDelim;
  NOT_SAVED = '(not saved)';

{ TRenderParams }

constructor TRenderParams.Create(ARenderEvent: TRenderEvent; ARefFilename: String);
begin
  OnRender := ARenderEvent;
  RefFile := ARefFileName;
end;


{ TMainForm }

procedure TMainForm.BtnSaveAsRefClick(Sender: TObject);
var
  bmp: TBitmap;
  png: TPortableNetworkGraphic;
  fn: String;
  renderParams: TRenderParams;
  page: TvVectorialPage;
begin
  renderParams := TRenderParams(Tree.Selected.Data);
  if RenderParams = nil then
    exit;
  if FDocBottomLeft = nil then
    exit;

  page := FDocBottomLeft.GetPageAsVectorial(0);

  bmp := TBitmap.Create;
  try
    bmp.SetSize(BottomLeftPaintbox.Width, BottomLeftPaintbox.Height);
    bmp.Canvas.GetUpdatedHandle([csHandleValid]);  // create the Handle needed by next line
    page.DrawBackground(bmp.Canvas);
    // bmp canvas has origin at top/left
    page.Render(bmp.Canvas, 0, bmp.Height, 1.0, -1.0);
    png := TPortableNetworkGraphic.Create;
    try
      png.Assign(bmp);
     // renderParams := TRenderParams(Tree.Selected.Data);
      ForceDirectory(IMG_FOLDER);
      fn := IncludeTrailingPathDelimiter(IMG_FOLDER) + renderParams.RefFile;
      png.SaveToFile(fn);
    finally
      png.Free;
    end;
    RefImage.Picture.Assign(bmp);
    RefImage.Hint := fn;
  finally
    bmp.Free;
  end;
end;

procedure TMainForm.BtnSaveToFileClick(Sender: TObject);
var
  fn: String;
  renderParams: TRenderParams;
  folder: String;
  fmt: TvVectorialFormat;
  ext: String;
begin
  if FDocBottomLeft = nil then
    exit;

  renderParams := TRenderParams(Tree.Selected.Data);
  if RenderParams = nil then
    exit;

  if Sender = BtnSaveAsSVG then begin
    ext := 'svg';
    fmt := vfSVG;
  end else
  if Sender = BtnSaveAsWMF then begin
    ext := 'wmf';
    fmt := vfWindowsMetafileWMF;
  end else
    exit;

  folder := IMG_FOLDER + ext + PathDelim;
  fn := folder + ChangeFileExt(renderParams.RefFile, '.' + ext);
  ForceDirectory(folder);
  FDocBottomLeft.WriteToFile(fn, fmt);

  ShowFileImage(fn);

  UpdateCmdStates;
end;

procedure TMainForm.BtnViewImageClick(Sender: TObject);
var
  fn: String;
  ext: String;
  renderParams: TRenderParams;
begin
  if Sender = BtnViewSVG then
  begin
    ext := 'svg';
  end else
  if Sender = BtnViewWMF then
  begin
    ext := 'wmf';
  end else
    exit;

  renderParams := TRenderParams(Tree.Selected.Data);
  if renderParams = nil then
    exit;

  fn := IMG_FOLDER + ext + PathDelim + ChangeFileExt(renderParams.RefFile, '.'+ext);
  if FileExists(fn) then
    OpenDocument(fn);
end;

procedure TMainForm.CreateDocument(var ADoc: TvVectorialDocument;
  var APage: TvVectorialPage; AUseTopLeftCoords: boolean);
var
  r: TvRectangle;
begin
  FreeAndNil(ADoc);
  ADoc := TvVectorialDocument.Create;
  APage := ADoc.AddPage;
  APage.BackgroundColor := colWhite;
  APage.Width := PAGE_SIZE;
  APage.Height := PAGE_SIZE;
  ADoc.Width := PAGE_SIZE;
  ADoc.Height := PAGE_SIZE;
  APage.UseTopLeftCoordinates := AUseTopLeftCoords;

  // Add a frame around the page
  r := TvRectangle.Create(APage);
  r.X := 0;
  if AUseTopLeftCoords then
    r.Y := 0
  else
    r.Y := APage.Height;
  r.CX := APage.Width - 1;
  r.CY := APage.Height - 1;
  r.Brush := CreateSimpleBrush(bsClear);
  r.Pen := CreatePen(psSolid, 1, colSilver);
  APage.AddEntity(r);
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  RefImage.Hint := NOT_SAVED;
  SVGPaintbox.Hint := NOT_SAVED;
  WMFPaintbox.Hint := NOT_SAVED;
  ReadIni;
  Populate;
  TreeSelectionChanged(nil);
end;

procedure TMainForm.FormDestroy(Sender: TObject);
var
  parentnode, node: TTreeNode;
begin
  parentnode := Tree.Items.GetFirstNode;
  while parentnode <> nil do begin
    node := parentnode.GetFirstChild;
    while node <> nil do begin
      TObject(node.Data).Free;
      node := node.GetNextSibling;
    end;
    parentnode := parentnode.GetNextSibling;
  end;

  FreeAndNil(FDocBottomLeft);
  FreeAndNil(FDocTopLeft);
  FreeAndNil(FDocFromSVG);
  FreeAndNil(FDocFromWMF);

  WriteIni;
end;

procedure TMainForm.PaintBoxPaint(Sender: TObject);
var
  doc: TvVectorialDocument;
  page: TvVectorialPage;
  w, h: Integer;
begin
  if Sender = BottomLeftPaintbox then
    doc := FDocBottomLeft
  else if Sender = TopLeftPaintbox then
    doc := FDocTopLeft
  else if Sender = SVGPaintbox then
    doc := FDocFromSVG
  else if Sender = WMFPaintbox then
    doc := FDocFromWMF
  else
    exit;

  w := TPaintbox(Sender).Width;
  h := TPaintbox(Sender).Height;

  if doc = nil then begin
    TPaintbox(Sender).Canvas.Brush.Color := clDefault;
    TPaintbox(Sender).Canvas.Brush.Style := bsSolid;
    TPaintbox(Sender).Canvas.FillRect(0, 0, w, h);
    exit;
  end;

  page := doc.GetPageAsVectorial(0);
  page.DrawBackground(TPaintbox(Sender).Canvas);
  if page.UseTopLeftCoordinates then
    page.Render(TPaintbox(Sender).Canvas, 0, 0, 1.0, 1.0)
  else
    page.Render(TPaintbox(Sender).Canvas, 0, h, 1.0, -1.0);
end;

procedure TMainForm.Populate;
var
  node, node0, node1: TTreeNode;
begin
  Tree.Items.Clear;

  node := Tree.Items.AddChild(nil, 'Simple shapes');
  Tree.Items.AddChildObject(node, 'Rectangle (solid) - moved up',
    TRenderParams.Create(@Render_Rect_Solid, 'rectangle_solid.png'));
  Tree.Items.AddChildObject(node, 'Rounded rectangle (solid) - moved up',
    TRenderParams.Create(@Render_RoundedRect_Solid, 'rounded_rect_solid.png'));
  Tree.Items.AddChildObject(node, 'Ellipse (solid) - moved up',
    TRenderParams.Create(@Render_Ellipse_Solid, 'ellipse_solid.png'));
  Tree.Items.AddChildObject(node, 'Circle (solid) - moved up',
    TRenderParams.Create(@Render_Circle_Solid, 'circle_solid.png'));
  Tree.Items.AddChildObject(node, 'Polygon (solid) - Triangle, base at bottom',
    TRenderParams.Create(@Render_Polygon_Solid, 'polygon_solid.png'));

  node := Tree.Items.AddChild(nil, 'Complex shapes');
  Tree.Items.AddChildObject(node, 'Path with hole (solid fill)',
    TRenderParams.Create(@Render_Path_Hole_SolidFill, 'path_hole_solid.png'));
  Tree.Items.AddChildObject(node, 'Path with hole (gradient fill)',
    TRenderParams.Create(@Render_Path_Hole_GradientFill, 'path_hole_gradient.png'));
  Tree.Items.AddChildObject(node, 'Self-intersecting polygon (solid fill, even-odd rule) - tip at bottom',
    TRenderParams.Create(@Render_SelfIntersectingPoly_SolidFill_EvenOdd, 'selfintersecting_poly_solid_eo.png'));
  Tree.Items.AddChildObject(node, 'Self-intersecting polygon (gradient fill, even-odd rule) - tip at bottom',
    TRenderParams.Create(@Render_SelfIntersectingPoly_GradientFill_EvenOdd, 'selfintersecting_poly_gradient_eo.png'));
  Tree.Items.AddChildObject(node, 'Self-intersecting polygon (solid fill, nonzero winding rule) - tip at bottom',
    TRenderParams.Create(@Render_SelfIntersectingPoly_SolidFill_NonZeroWinding, 'selfintersecting_poly_solid_nzw.png'));
  Tree.Items.AddChildObject(node, 'Self-intersecting polygon (gradient fill, nonzero winding rule) - tip at bottom',
    TRenderParams.Create(@Render_SelfIntersectingPoly_GradientFill_NonZeroWinding, 'selfintersecting_poly_gradient_nzw.png'));

  node0 := Tree.Items.AddChild(nil, 'Arcs');
  node := Tree.Items.AddChild(node0, 'clockwise "1" --> "2"');
  Tree.Items.AddChildObject(node, 'Quarter circle in quadrant I',
    TRenderParams.Create(@Render_Arc_CW_Q1, 'arc_cw_q1.png'));
  Tree.Items.AddChildObject(node, 'Quarter circle in quadrant I+II',
    TRenderParams.Create(@Render_Arc_CW_Q12, 'arc_cw_q12.png'));
  Tree.Items.AddChildObject(node, 'Quarter circle in quadrant II',
    TRenderParams.Create(@Render_Arc_CW_Q2, 'arc_cw_q2.png'));
  Tree.Items.AddChildObject(node, 'Quarter circle in quadrant II+III',
    TRenderParams.Create(@Render_Arc_CW_Q23, 'arc_cw_q23.png'));
  Tree.Items.AddChildObject(node, 'Quarter circle in quadrant III',
    TRenderParams.Create(@Render_Arc_CW_Q3, 'arc_cw_q3.png'));
  Tree.Items.AddChildObject(node, 'Quarter circle in quadrant III+IV',
    TRenderParams.Create(@Render_Arc_CW_Q34, 'arc_cw_q34.png'));
  Tree.Items.AddChildObject(node, 'Quarter circle in quadrant IV',
    TRenderParams.Create(@Render_Arc_CW_Q4, 'arc_cw_q4.png'));
  Tree.Items.AddChildObject(node, 'Quarter circle in quadrant IV+I',
    TRenderParams.Create(@Render_Arc_CW_Q41, 'arc_cw_q41.png'));
  node := Tree.Items.AddChild(node0, 'counterclockwise "1" --> "2"');
  Tree.Items.AddChildObject(node, 'Quarter circle in quadrant I',
    TRenderParams.Create(@Render_Arc_CCW_Q1, 'arc_ccw_q1.png'));
  Tree.Items.AddChildObject(node, 'Quarter circle in quadrant I+II',
    TRenderParams.Create(@Render_Arc_CCW_Q12, 'arc_ccw_q12.png'));
  Tree.Items.AddChildObject(node, 'Quarter circle in quadrant II',
    TRenderParams.Create(@Render_Arc_CCW_Q2, 'arc_ccw_q2.png'));
  Tree.Items.AddChildObject(node, 'Quarter circle in quadrant II+III',
    TRenderParams.Create(@Render_Arc_CCW_Q23, 'arc_ccw_q23.png'));
  Tree.Items.AddChildObject(node, 'Quarter circle in quadrant III',
    TRenderParams.Create(@Render_Arc_CCW_Q3, 'arc_ccw_q3.png'));
  Tree.Items.AddChildObject(node, 'Quarter circle in quadrant III+IV',
    TRenderParams.Create(@Render_Arc_CCW_Q34, 'arc_ccw_q34.png'));
  Tree.Items.AddChildObject(node, 'Quarter circle in quadrant IV',
    TRenderParams.Create(@Render_Arc_CCW_Q4, 'arc_ccw_q4.png'));
  Tree.Items.AddChildObject(node, 'Quarter circle in quadrant IV+I',
    TRenderParams.Create(@Render_Arc_CCW_Q41, 'arc_ccw_q41.png'));

  node0 := Tree.Items.AddChild(nil, 'Gradients');
  node := Tree.Items.AddChild(node0, 'horizontal');
  Tree.Items.AddChildObject(node, 'Ellipse',
    TRenderParams.Create(@Render_Ellipse_GradientHor, 'ellipse_gradienthor.png'));
  Tree.Items.AddChildObject(node, 'Circle',
    TRenderParams.Create(@Render_Circle_GradientHor, 'circle_gradienthor.png'));
  Tree.Items.AddChildObject(node, 'Rectangle',
    TRenderParams.Create(@Render_Rect_GradientHor, 'rect_gradienthor.png'));
  Tree.Items.AddChildObject(node, 'RoundedRect',
    TRenderParams.Create(@Render_RoundedRect_GradientHor, 'rounded_rect_gradienthor.png'));
  Tree.Items.AddChildObject(node, 'Polygon',
    TRenderParams.Create(@Render_Polygon_GradientHor, 'polygon_gradienthor.png'));

  node := Tree.Items.AddChild(node0, 'vertical');
  Tree.Items.AddChildObject(node, 'ellipse',
    TRenderParams.Create(@Render_Ellipse_GradientVert, 'ellipse_gradientvert.png'));
  Tree.Items.AddChildObject(node, 'circle',
    TRenderParams.Create(@Render_Circle_GradientVert, 'circle_gradientvert.png'));
  Tree.Items.AddChildObject(node, 'Rectangle',
    TRenderParams.Create(@Render_Rect_GradientVert, 'rect_gradientvert.png'));
  Tree.Items.AddChildObject(node, 'Rounded rectangle',
    TRenderParams.Create(@Render_RoundedRect_GradientVert, 'rounded_rect_gradientvert.png'));
  Tree.Items.AddChildObject(node, 'Polygon',
    TRenderParams.Create(@Render_Polygon_GradientVert, 'polygon_gradientvert.png'));

  node := Tree.Items.AddChild(node0, 'linear');
  Tree.Items.AddChildObject(node, 'Ellipse',
    TRenderParams.Create(@Render_Ellipse_GradientLinear, 'ellipse_gradientlinear.png'));
  Tree.Items.AddChildObject(node, 'Circle',
    TRenderParams.Create(@Render_Circle_GradientLinear, 'circle_gradientlinear.png'));
  Tree.Items.AddChildObject(node, 'Rectangle',
    TRenderParams.Create(@Render_Rect_GradientLinear, 'rect_gradientlinear.png'));
  Tree.Items.AddChildObject(node, 'Rounded rectangle',
    TRenderParams.Create(@Render_RoundedRect_GradientLinear, 'rounded_rect_gradientlinear.png'));
  Tree.Items.AddChildObject(node, 'Polygon',
    TRenderParams.Create(@Render_Polygon_GradientLinear, 'polygon_gradientlinear.png'));

  node := Tree.Items.AddChild(node0, 'radial');
  Tree.Items.AddChildObject(node, 'Ellipse',
    TRenderParams.Create(@Render_Ellipse_GradientRadial, 'ellipse_gradientradialar.png'));
  Tree.Items.AddChildObject(node, 'Circle',
    TRenderParams.Create(@Render_Circle_GradientRadial, 'circle_gradientradial.png'));
  Tree.Items.AddChildObject(node, 'Rectangle',
    TRenderParams.Create(@Render_Rect_GradientRadial, 'rect_gradientradial.png'));
  Tree.Items.AddChildObject(node, 'Rounded rectangle',
    TRenderParams.Create(@Render_RoundedRect_GradientRadial, 'rounded_rect_gradientradial.png'));
  Tree.Items.AddChildObject(node, 'Polygon',
    TRenderParams.Create(@Render_Polygon_GradientRadial, 'polygon_gradientradial.png'));

  node0 := Tree.Items.AddChild(nil, 'Text');
  node := Tree.Items.AddChild(node0, 'horizontal');
  Tree.Items.AddChildObject(node, 'left aligned',
    TRenderParams.Create(@Render_Text_Left, 'text_left.png'));
  Tree.Items.AddChildObject(node, 'centered',
    TRenderParams.Create(@Render_Text_Center, 'text_center.png'));
  Tree.Items.AddChildObject(node, 'right aligned',
    TRenderParams.Create(@Render_Text_Right, 'text_right.png'));
  node := Tree.Items.AddChild(node0, 'rotated by 30 deg');
  Tree.Items.AddChildObject(node, 'left aligned',
    TRenderParams.Create(@Render_Text_Left_30deg, 'text_left_30.png'));
  Tree.Items.AddChildObject(node, 'centered',
    TRenderParams.Create(@Render_Text_Center_30deg, 'text_center_30.png'));
  Tree.Items.AddChildObject(node, 'right aligned',
    TRenderParams.Create(@Render_Text_Right_30deg, 'text_right_30.png'));
  node := Tree.Items.AddChild(node0, 'vertical (90 deg, upward)');
  Tree.Items.AddChildObject(node, 'left aligned',
    TRenderParams.Create(@Render_Text_Left_90deg, 'text_left_90.png'));
  Tree.Items.AddChildObject(node, 'centered',
    TRenderParams.Create(@Render_Text_Center_90deg, 'text_center_90.png'));
  Tree.Items.AddChildObject(node, 'right aligned',
    TRenderParams.Create(@Render_Text_Right_90deg, 'text_right_90.png'));
  node := Tree.Items.AddChild(node0, 'vertical (-90 deg, downward)');
  Tree.Items.AddChildObject(node, 'left aligned',
    TRenderParams.Create(@Render_Text_Left_m90deg, 'text_left_m90.png'));
  Tree.Items.AddChildObject(node, 'centered',
    TRenderParams.Create(@Render_Text_Center_m90deg, 'text_center_m90.png'));
  Tree.Items.AddChildObject(node, 'right aligned',
    TRenderParams.Create(@Render_Text_Right_m90deg, 'text_right_m90.png'));
  node := Tree.Items.AddChild(node0, 'Fonts');
  Tree.Items.AddChildObject(node, 'Times New Roman + Courier New',
    TRenderParams.Create(@Render_Text_Fonts, 'text_fonts.png'));
  Tree.Items.AddChildObject(node0, 'Text colors',
    TRenderParams.Create(@Render_Text_Colors, 'text_colors.png'));
end;

procedure TMainForm.Render_Circle_Solid(APage: TvVectorialPage);
var
  circ: TvCircle;
begin
  circ := CreateStdCircle(APage);
  circ.Brush := StdSolidBrush;
  APage.AddEntity(circ);
end;

procedure TMainForm.Render_Circle_GradientHor(APage: TvVectorialPage);
var
  circ: TvCircle;
begin
  circ := CreateStdCircle(APage);
  circ.Brush := StdHorizGradientBrush;
  APage.AddEntity(circ);
end;

procedure TMainForm.Render_Circle_GradientVert(APage: TvVectorialPage);
var
  circ: TvCircle;
begin
  circ := CreateStdCircle(APage);
  circ.Brush := StdVertGradientBrush;
  APage.AddEntity(circ);
end;

procedure TMainForm.Render_Circle_GradientLinear(APage: TvVectorialPage);
var
  circ: TvCircle;
begin
  circ := CreateStdCircle(APage);
  circ.Brush := StdLinearGradientBrush;
  APage.AddEntity(circ);
end;

procedure TMainForm.Render_Circle_GradientRadial(APage: TvVectorialPage);
var
  circ: TvCircle;
begin
  circ := CreateStdCircle(APage);
  circ.Brush := StdRadialGradientBrush;
  APage.AddEntity(circ);
end;

procedure TMainForm.Render_Ellipse_GradientHor(APage: TvVectorialPage);
var
  ell: TvEllipse;
begin
  ell := CreateStdEllipse(APage);
  ell.Brush := StdHorizGradientBrush;
  APage.AddEntity(ell);
end;

procedure TMainForm.Render_Ellipse_GradientVert(APage: TvVectorialPage);
var
  ell: TvEllipse;
begin
  ell := CreateStdEllipse(APage);
  ell.Brush := StdVertGradientBrush;
  APage.AddEntity(ell);
end;

procedure TMainForm.Render_Ellipse_GradientLinear(APage: TvVectorialPage);
var
  ell: TvEllipse;
begin
  ell := CreateStdEllipse(APage);
  ell.Brush := StdLinearGradientBrush;
  APage.AddEntity(ell);
end;

procedure TMainForm.Render_Ellipse_GradientRadial(APage: TvVectorialPage);
var
  ell: TvEllipse;
begin
  ell := CreateStdEllipse(APage);
  ell.Brush := StdRadialGradientBrush;
  APage.AddEntity(ell);
end;

procedure TMainForm.Render_Ellipse_Solid(APage: TvVectorialPage);
var
  ell: TvEllipse;
begin
  ell := CreateStdEllipse(APage);
  ell.Brush := StdSolidBrush;
  APage.AddEntity(ell);
end;

procedure TMainForm.Render_Rect_Solid(APage: TvVectorialPage);
var
  rect: TvRectangle;
begin
  rect := CreateStdRect(APage);
  rect.Brush := StdSolidBrush;
  APage.AddEntity(rect);
end;

procedure TMainForm.Render_Rect_GradientHor(APage: TvVectorialPage);
var
  rect: TvRectangle;
begin
  rect := CreateStdRect(APage);
  rect.Brush := StdHorizGradientBrush;
  APage.AddEntity(rect);
end;

procedure TMainForm.Render_Rect_GradientVert(APage: TvVectorialPage);
var
  rect: TvRectangle;
begin
  rect := CreateStdRect(APage);
  rect.Brush := StdVertGradientBrush;
  APage.AddEntity(rect);
end;

procedure TMainForm.Render_Rect_GradientLinear(APage: TvVectorialPage);
var
  rect: TvRectangle;
begin
  rect := CreateStdRect(APage);
  rect.Brush := StdLinearGradientBrush;
  APage.AddEntity(rect);
end;

procedure TMainForm.Render_Rect_GradientRadial(APage: TvVectorialPage);
var
  rect: TvRectangle;
begin
  rect := CreateStdRect(APage);
  rect.Brush := StdRadialGradientBrush;
  APage.AddEntity(rect);
end;

procedure TMainForm.Render_RoundedRect_Solid(APage: TvVectorialPage);
var
  rect: TvRectangle;
begin
  rect := CreateStdRoundedRect(APage);
  rect.Brush := StdSolidBrush;
  APage.AddEntity(rect);
end;

procedure TMainForm.Render_RoundedRect_GradientHor(APage: TvVectorialPage);
var
  rect: TvRectangle;
begin
  rect := CreateStdRoundedRect(APage);
  rect.Brush := StdHorizGradientBrush;
  APage.AddEntity(rect);
end;

procedure TMainForm.Render_RoundedRect_GradientVert(APage: TvVectorialPage);
var
  rect: TvRectangle;
begin
  rect := CreateStdRoundedRect(APage);
  rect.Brush := StdVertGradientBrush;
  APage.AddEntity(rect);
end;

procedure TMainForm.Render_RoundedRect_GradientLinear(APage: TvVectorialPage);
var
  rect: TvRectangle;
begin
  rect := CreateStdRoundedRect(APage);
  rect.Brush := StdLinearGradientBrush;
  APage.AddEntity(rect);
end;

procedure TMainForm.Render_RoundedRect_GradientRadial(APage: TvVectorialPage);
var
  rect: TvRectangle;
begin
  rect := CreateStdRoundedRect(APage);
  rect.Brush := StdRadialGradientBrush;
  APage.AddEntity(rect);
end;

procedure TMainForm.Render_Polygon_GradientHor(APage: TvVectorialPage);
var
  poly: TvPolygon;
begin
  poly := CreateStdPolygon(APage);
  poly.Brush := StdHorizGradientBrush;
  APage.AddEntity(poly);
end;

procedure TMainForm.Render_Polygon_GradientVert(APage: TvVectorialPage);
var
  poly: TvPolygon;
begin
  poly := CreateStdPolygon(APage);
  poly.Brush := StdVertGradientBrush;
  APage.AddEntity(poly);
end;

procedure TMainForm.Render_Polygon_GradientLinear(APage: TvVectorialPage);
var
  poly: TvPolygon;
begin
  poly := CreateStdPolygon(APage);
  poly.Brush := StdLinearGradientBrush;
  APage.AddEntity(poly);
end;

procedure TMainForm.Render_Polygon_GradientRadial(APage: TvVectorialPage);
var
  poly: TvPolygon;
begin
  poly := CreateStdPolygon(APage);
  poly.Brush := StdRadialGradientBrush;
  APage.AddEntity(poly);
end;

procedure TMainForm.Render_Arc_CW_Q1(APage: TvVectorialPage);
begin
  CreateStdArcQ1(APage, true);
end;

procedure TMainForm.Render_Arc_CW_Q12(APage: TvVectorialPage);
begin
  CreateStdArcQ12(APage, true);
end;

procedure TMainForm.Render_Arc_CW_Q2(APage: TvVectorialPage);
begin
  CreateStdArcQ2(APage, true);
end;

procedure TMainForm.Render_Arc_CW_Q23(APage: TvVectorialPage);
begin
  CreateStdArcQ23(APage, true);
end;

procedure TMainForm.Render_Arc_CW_Q3(APage: TvVectorialPage);
begin
  CreateStdArcQ3(APage, true);
end;

procedure TMainForm.Render_Arc_CW_Q34(APage: TvVectorialPage);
begin
  CreateStdArcQ34(APage, true);
end;

procedure TMainForm.Render_Arc_CW_Q4(APage: TvVectorialPage);
begin
  CreateStdArcQ4(APage, true);
end;

procedure TMainForm.Render_Arc_CW_Q41(APage: TvVectorialPage);
begin
  CreateStdArcQ41(APage, true);
end;

procedure TMainForm.Render_Arc_CCW_Q1(APage: TvVectorialPage);
begin
  CreateStdArcQ1(APage, false);
end;

procedure TMainForm.Render_Arc_CCW_Q12(APage: TvVectorialPage);
begin
  CreateStdArcQ12(APage, false);
end;

procedure TMainForm.Render_Arc_CCW_Q2(APage: TvVectorialPage);
begin
  CreateStdArcQ2(APage, false);
end;

procedure TMainForm.Render_Arc_CCW_Q23(APage: TvVectorialPage);
begin
  CreateStdArcQ23(APage, false);
end;

procedure TMainForm.Render_Arc_CCW_Q3(APage: TvVectorialPage);
begin
  CreateStdArcQ3(APage, false);
end;

procedure TMainForm.Render_Arc_CCW_Q34(APage: TvVectorialPage);
begin
  CreateStdArcQ34(APage, false);
end;

procedure TMainForm.Render_Arc_CCW_Q4(APage: TvVectorialPage);
begin
  CreateStdArcQ4(APage, false);
end;

procedure TMainForm.Render_Arc_CCW_Q41(APage: TvVectorialPage);
begin
  CreateStdArcQ41(APage, false);
end;

procedure TMainForm.Render_Polygon_Solid(APage: TvVectorialPage);
var
  poly: TvPolygon;
begin
  poly := CreateStdPolygon(APage);
  poly.Brush := StdSolidBrush;
  APage.AddEntity(poly);
end;

procedure TMainForm.Render_Path_Hole_SolidFill(APage: TvVectorialPage);
var
  obj: TPath;
begin
  obj := CreatePathWithHole(APage);  // no need to AddEntity!
  obj.Brush.Color := colYellow;
  obj.Brush.Style := bsSolid;
  obj.Pen.Width := 3;
  obj.Pen.Color := colBlue;
end;

procedure TMainForm.Render_Path_Hole_GradientFill(APage: TvVectorialPage);
var
  obj: TPath;
begin
  obj := CreatePathWithHole(APage);  // No need to AddEntity!
  obj.Brush := StdLinearGradientBrush;
  obj.Pen.Width := 3;
  obj.Pen.Color := colBlue;
end;

procedure TMainForm.Render_SelfIntersectingPoly_SolidFill_EvenOdd(APage: TvVectorialPage);
var
  obj: TvPolygon;
begin
  obj := CreateStdSelfIntersectingPolygon(APage);
  obj.WindingRule := vcmEvenOddRule;
  obj.Brush := StdSolidBrush;
  APage.AddEntity(obj);
end;

procedure TMainForm.Render_SelfIntersectingPoly_GradientFill_EvenOdd(
  APage: TvVectorialPage);
var
  obj: TvPolygon;
begin
  obj := CreateStdSelfIntersectingPolygon(APage);
  obj.Brush := StdHorizGradientBrush;
  obj.WindingRule := vcmEvenOddRule;
  APage.AddEntity(obj);
end;

procedure TMainForm.Render_SelfIntersectingPoly_SolidFill_NonZeroWinding(
  APage: TvVectorialPage);
var
  obj: TvPolygon;
begin
  obj := CreateStdSelfIntersectingPolygon(APage);
  obj.WindingRule := vcmNonZeroWindingRule;
  obj.Brush := StdSolidBrush;
  APage.AddEntity(obj);
end;

procedure TMainForm.Render_SelfIntersectingPoly_GradientFill_NonZeroWinding(
  APage: TvVectorialPage);
var
  obj: TvPolygon;
begin
  obj := CreateStdSelfIntersectingPolygon(APage);
  obj.Brush := StdHorizGradientBrush;
  obj.WindingRule := vcmNonzeroWindingRule;
  APage.AddEntity(obj);
end;

procedure TMainForm.Render_Text(APage: TvVectorialPage; Anchor: TvTextAnchor;
  Angle: Double);
const
  XTEXT = 50;
  YTEXT = 40;  // y points up
  L = 10;
var
  txt: TvText;
  obj: TPath;
begin
  // Draw "+" at the origin of the text
  if APage.UseTopLeftCoordinates then begin
    APage.StartPath    (XTEXT - L, PAGE_SIZE - YTEXT);
    APage.AddLineToPath(XTEXT + L, PAGE_SIZE - YTEXT);
    APage.AddMoveToPath(XTEXT,     PAGE_SIZE - YTEXT - L);
    APage.AddLineToPath(XTEXT,     PAGE_SIZE - YTEXT + L);
  end else begin
    APage.StartPath    (XTEXT - L, YTEXT);
    APage.AddLineToPath(XTEXT + L, YTEXT);
    APage.AddMoveToPath(XTEXT,     YTEXT - L);
    APage.AddLineToPath(XTEXT,     YTEXT + L);
  end;
  obj := APage.EndPath;
  obj.Pen.Width := 1;
  obj.Pen.Color := colRed;

  txt := TvText.Create(APage);
  txt.X := XTEXT;
  if APage.UseTopLeftCoordinates then
    txt.Y := PAGE_SIZE - YTEXT else
    txt.Y := YTEXT;
  txt.Value.Add('ABC');
  txt.Font.Size := 14;
  txt.TextAnchor := Anchor;
  txt.Font.Orientation := Angle;

  APage.AddEntity(txt);
end;

procedure TMainForm.Render_Text_Left(APage: TvVectorialPage);
begin
  Render_Text(APage, vtaStart, 0);
end;

procedure TMainForm.Render_Text_Right(APage: TvVectorialPage);
begin
  Render_Text(APage, vtaEnd, 0);
end;

procedure TMainForm.Render_Text_Center(APage: TvVectorialPage);
begin
  Render_Text(APage, vtaMiddle, 0);
end;

procedure TMainForm.Render_Text_Left_30deg(APage: TvVectorialPage);
begin
  Render_Text(APage, vtaStart, 30);
end;

procedure TMainForm.Render_Text_Right_30deg(APage: TvVectorialPage);
begin
  Render_Text(APage, vtaEnd, 30);
end;

procedure TMainForm.Render_Text_Center_30deg(APage: TvVectorialPage);
begin
  Render_Text(APage, vtaMiddle, 30);
end;

procedure TMainForm.Render_Text_Left_90deg(APage: TvVectorialPage);
begin
  Render_Text(APage, vtaStart, 90);
end;

procedure TMainForm.Render_Text_Right_90deg(APage: TvVectorialPage);
begin
  Render_Text(APage, vtaEnd, 90);
end;

procedure TMainForm.Render_Text_Center_90deg(APage: TvVectorialPage);
begin
  Render_Text(APage, vtaMiddle, 90);
end;

procedure TMainForm.Render_Text_Left_m90deg(APage: TvVectorialPage);
begin
  Render_Text(APage, vtaStart, -90);
end;

procedure TMainForm.Render_Text_Right_m90deg(APage: TvVectorialPage);
begin
  Render_Text(APage, vtaEnd, -90);
end;

procedure TMainForm.Render_Text_Center_m90deg(APage: TvVectorialPage);
begin
  Render_Text(APage, vtaMiddle, -90);
end;

procedure TMainForm.Render_Text_Fonts(APage: TvVectorialPage);
var
  txt: TvText;
begin
  txt := TvText.Create(APage);
  txt.X := 10;
  txt.Y := 80;
  txt.Font.Name := 'Times New Roman';
  txt.Font.Size := 10;
  txt.Value.Add('Times');
  APage.AddEntity(txt);

  txt := TvText.Create(APage);
  txt.X := 10;
  txt.Y := 60;
  txt.Font.Name := 'Courier New';
  txt.Font.Size := 12;
  txt.Value.Add('Courier');
  APage.AddEntity(txt);
end;

procedure TMainForm.Render_Text_Colors(APage: TvVectorialPage);
var
  txt: TvText;
begin
  txt := TvText.Create(APage);
  txt.X := 10;
  txt.Y := 80;
  txt.Font.Name := 'Times New Roman';
  txt.Font.Size := 14;
  txt.Font.Color := colRed;
  txt.Value.Add('Text');
  txt.Brush.Style := bsSolid;
  txt.Brush.Color := colYellow;
  txt.Brush.Kind := bkSimpleBrush;
  APage.AddEntity(txt);
end;

procedure TMainForm.ReadIni;
var
  ini: TCustomIniFile;
  L, T, W, H: Integer;
  rct: TRect;
begin
  ini := TMemIniFile.Create(ChangeFileExt(Application.ExeName, '.ini'));
  try
    L := ini.ReadInteger('MainForm', 'Left', Left);
    T := ini.ReadInteger('MainForm', 'Top', Top);
    W := ini.ReadInteger('MainForm', 'Width', Width);
    H := ini.ReadInteger('MainForm', 'Height', Height);
    rct := Screen.DesktopRect;
    if L + W > rct.Right - rct.Left then L := rct.Right - W;
    if L < 0 then L := rct.Left;
    if T + H > rct.Bottom - rct.Top then T := rct.Bottom - H;
    if T < 0 then T := rct.Top;
    SetBounds(L, T, W, H);
  finally
    ini.Free;
  end;
end;

procedure TMainForm.WriteIni;
var
  ini: TCustomIniFile;
begin
  ini := TMemIniFile.Create(ChangeFileExt(Application.ExeName, '.ini'));
  try
    if WindowState = wsNormal then
    begin
      ini.WriteInteger('MainForm', 'Left', Left);
      ini.WriteInteger('MainForm', 'Top', Top);
      ini.WriteInteger('MainForm', 'Width', Width);
      ini.WriteInteger('MainForm', 'Height', Height);
    end;
  finally
    ini.Free;
  end;
end;

procedure TMainForm.ShowFileImage(AFilename: String);
var
  ext: String;
begin
  ext := Lowercase(ExtractFileExt(AFileName));
  if ext = '.svg' then begin
    FreeAndNil(FDocFromSVG);
    if FileExists(AFileName) then begin
      FDocFromSVG := TvVectorialDocument.Create;
      FDocFromSVG.ReadFromFile(AFileName);
      SVGPaintbox.Hint := AFilename;
    end else begin
      SVGPaintbox.Hint := NOT_SAVED;
    end;
    SVGPaintbox.Invalidate;
  end else
  if ext = '.wmf' then begin
    FreeAndNil(FDocFromWMF);
    if FileExists(AFileName) then begin
      FDocFromWMF := TvVectorialDocument.Create;
      FDocFromWMF.ReadFromFile(AFilename);
      WMFPaintBox.Hint := AFileName;
    end else begin
      WMFPaintBox.Hint := NOT_SAVED;
    end;
    WMFPaintBox.Invalidate;
  end;
end;

procedure TMainForm.ShowReadWriteTestImages;
var
  renderParams: TRenderParams;
  fn: String;
begin
  if Tree.Selected = nil then
    exit;

  renderParams := TRenderParams(Tree.Selected.Data);
  if renderParams = nil then
  begin
    SVGPaintbox.Invalidate;
    WMFPaintbox.Invalidate;
    exit;
  end;

  fn := IMG_FOLDER + 'svg' + PathDelim + ChangeFileExt(renderParams.RefFile, '.svg');
  ShowFileImage(fn);

  fn := IMG_FOLDER + 'wmf' + PathDelim + ChangeFileExt(renderParams.RefFile, '.wmf');
  ShowFileImage(fn);
end;

procedure TMainForm.ShowRefImageTest;
var
  renderParams: TRenderParams;
  fn: String;
begin
  if Tree.Selected = nil then
    exit;

  renderParams := TRenderParams(Tree.Selected.Data);
  if renderParams = nil then
  begin
    RefImage.Picture := nil;
    exit;
  end;

  fn := IncludeTrailingPathDelimiter(IMG_FOLDER) + renderParams.RefFile;
  if FileExists(fn) then begin
    RefImage.Picture.LoadFromFile(fn);
    RefImage.Hint := fn;
  end else begin
    RefImage.Picture := nil;
    RefImage.Hint := NOT_SAVED;
  end;
end;

procedure TMainForm.ShowRenderTestImages;
var
  renderParams: TRenderParams;
  page: TvVectorialPage = nil;
begin
  if Tree.Selected = nil then
    exit;

  renderParams := TRenderParams(Tree.Selected.Data);
  if renderParams = nil then
  begin
    BottomLeftPaintbox.Invalidate;
    TopLeftPaintbox.Invalidate;
    exit;
  end;

  CreateDocument(FDocBottomLeft, page, false);
  renderParams.OnRender(page);
  BottomLeftPaintbox.Invalidate;

  CreateDocument(FDocTopLeft, page, true);
  renderParams.OnRender(page);
  TopLeftPaintbox.Invalidate;
end;

procedure TMainForm.TreeSelectionChanged(Sender: TObject);
begin
  UpdateCmdStates;
  ShowRenderTestImages;
  ShowRefImageTest;
  ShowReadWriteTestImages;
end;

procedure TMainForm.TreeCustomDrawItem(Sender: TCustomTreeView; Node: TTreeNode;
  State: TCustomDrawState; var DefaultDraw: Boolean);
begin
  if Node.HasChildren then
    Sender.Canvas.Font.Style := [fsBold] else
    Sender.Canvas.Font.Style := [];
end;

procedure TMainForm.UpdateCmdStates;
var
  fn: String;
  renderParams: TRenderParams;
  svgOK, wmfOK: boolean;
begin
  BtnSaveAsRef.Enabled := Tree.Selected <> nil;
  BtnSaveAsWMF.Enabled := Tree.Selected <> nil;
  BtnSaveAsSVG.Enabled := Tree.Selected <> nil;

  svgOK := false;
  wmfOK := false;
  if Tree.Selected <> nil then begin
    renderParams := TRenderParams(Tree.Selected.Data);
    if renderParams <> nil then begin
      fn := IMG_FOLDER + 'svg' + PathDelim + ChangeFileExt(renderParams.RefFile, '.svg');
      svgOK := FileExists(fn);
      fn := IMG_FOLDER + 'wmf' + PathDelim + ChangeFileExt(renderParams.RefFile, '.wmf');
      wmfOK := FileExists(fn);
    end;
  end;
  BtnViewSVG.Enabled := svgOK;
  BtnViewWMF.Enabled := wmfOK;
end;

end.

