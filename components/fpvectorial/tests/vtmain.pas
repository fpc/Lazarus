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

  TRenderEvent = procedure(APage: TvVectorialPage;
    AIntParam: Integer = MaxInt) of object;

  TRenderParams = class
    RefFile: String;
    IntParam: Integer;
    OnRender: TRenderEvent;
    constructor Create(ARenderEvent: TRenderEvent; ARefFilename: String;
      AIntParam: Integer = MaxInt);
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
    RbWriteBottomLeftCoords: TRadioButton;
    RbWriteTopLeftCoords: TRadioButton;
    RefImage: TImage;
    Label10: TLabel;
    Label11: TLabel;
    Label13: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    LblBothImagesMustMatch: TLabel;
    LblRefImgMustMatch: TLabel;
    LblReadWriteInstructions: TLabel;
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

    // Simple shapes, solid fills and gradients
    procedure Render_Shape(APage: TvVectorialPage; AIntParam: Integer);

    // Complex shapes
    procedure Render_Path_Hole_SolidFill(APage: TvVectorialPage;
      AIntParam: Integer);
    procedure Render_Path_Hole_GradientFill(APage: TvVectorialPage;
      AIntParam: Integer);
    procedure Render_SelfIntersectingPoly_SolidFill_EvenOdd(APage: TvVectorialPage;
      AIntParam: Integer);
    procedure Render_SelfIntersectingPoly_GradientFill_EvenOdd(APage: TvVectorialPage;
      AIntParam: Integer);
    procedure Render_SelfIntersectingPoly_SolidFill_NonZeroWinding(APage: TvVectorialPage;
      AIntParam: Integer);
    procedure Render_SelfIntersectingPoly_GradientFill_NonZeroWinding(APage: TvVectorialPage;
      AIntParam: Integer);

    // Arcs
    procedure Render_Arc(APage: TvVectorialPage; AIntParam: Integer);

    // Text
//    procedure Render_Text(APage: TvVectorialPage; Anchor: TvTextAnchor; Angle: Double);
    procedure Render_Text(APage: TvVectorialpage; AIntParam: Integer);
    {
    procedure Render_Text_Left(APage: TvVectorialPage; AIntParam: Integer);
    procedure Render_Text_Center(APage: TvVectorialPage; AIntParam: Integer);
    procedure Render_Text_Right(APage: TvVectorialPage; AIntParam: Integer);
    procedure Render_Text_Left_30deg(APage: TvVectorialPage; AIntParam: Integer);
    procedure Render_Text_Center_30deg(APage: TvVectorialPage; AIntParam: Integer);
    procedure Render_Text_Right_30deg(APage: TvVectorialPage; AIntParam: Integer);
    procedure Render_Text_Left_90deg(APage: TvVectorialPage; AIntParam: Integer);
    procedure Render_Text_Center_90deg(APage: TvVectorialPage; AIntParam: Integer);
    procedure Render_Text_Right_90deg(APage: TvVectorialPage; AIntParam: Integer);
    procedure Render_Text_Left_m90deg(APage: TvVectorialPage; AIntParam: Integer);
    procedure Render_Text_Center_m90deg(APage: TvVectorialPage; AIntParam: Integer);
    procedure Render_Text_Right_m90deg(APage: TvVectorialPage; AIntParam: Integer);
    }
    procedure Render_Text_Fonts(APage: TvVectorialPage; AIntParam: Integer);
    procedure Render_Text_Colors(APage: TvVectorialPage; AIntParam: Integer);
  public
    { public declarations }
  end;

var
  MainForm: TMainForm;


implementation

{$R *.lfm}

uses
  Math, FPCanvas, IniFiles, LazFileUtils, LCLIntf,
  fpvutils, vtprimitives;

const
  IMG_FOLDER = 'images' + PathDelim;
  NOT_SAVED = '(not saved)';


{ TRenderParams }

constructor TRenderParams.Create(ARenderEvent: TRenderEvent;
  ARefFilename: String; AIntParam: Integer = MaxInt);
begin
  OnRender := ARenderEvent;
  RefFile := ARefFileName;
  IntParam := AIntParam;
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
  if RbWriteBottomLeftCoords.Checked then
    FDocBottomLeft.WriteToFile(fn, fmt)
  else if RbWriteTopLeftCoords.Checked then
    FDocTopLeft.WriteToFile(fn, fmt);

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
    BtnSaveToFileClick(BtnSaveAsSVG);
    ext := 'svg';
  end else
  if Sender = BtnViewWMF then
  begin
    BtnSaveToFileClick(BtnSaveAsWMF);
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

  { --------------------------------------------------}
  node := Tree.Items.AddChild(nil, 'Simple shapes');
  { --------------------------------------------------}
  Tree.Items.AddChildObject(node, 'Circle (solid) - move up',
    TRenderParams.Create(@Render_Shape, 'circle_solid.png', $0100));
  Tree.Items.AddChildObject(node, 'Ellipse (solid) - moved up',
    TRenderParams.Create(@Render_Shape, 'ellipse_solid.png', $0200));
  Tree.Items.AddChildObject(node, 'Rectangle(solid) - moved up',
    TRenderParams.Create(@Render_Shape, 'rect_solid.png', $0300));
  Tree.Items.AddChildObject(node, 'Rounded rectangle (solid) - moved up',
    TRenderParams.Create(@Render_Shape, 'rounded_rect_solid.png', $0400));
  Tree.Items.AddChildObject(node, 'Polygon (solid) - moved up',
    TRenderParams.Create(@Render_Shape, 'polygon_solid.png', $0500));

  { --------------------------------------------------}
  node := Tree.Items.AddChild(nil, 'Complex shapes');
  { --------------------------------------------------}
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

  { -----------------------------------------}
  node0 := Tree.Items.AddChild(nil, 'Arcs');
  { -----------------------------------------}
  node1 := Tree.Items.AddChild(node0, 'circular');
  node := Tree.Items.AddChild(node1, 'clockwise from point 1 to point 2');
  Tree.Items.AddChildObject(node, 'Quadrant I',
    TRenderParams.Create(@Render_Arc, 'arc_cw_q1.png', $0200));
  Tree.Items.AddChildObject(node, 'Quadrant I+II',
    TRenderParams.Create(@Render_Arc, 'arc_cw_q12.png', $0201));
  Tree.Items.AddChildObject(node, 'Quadrant II',
    TRenderParams.Create(@Render_Arc, 'arc_cw_q2.png', $0202));
  Tree.Items.AddChildObject(node, 'Quadrant II+III',
    TRenderParams.Create(@Render_Arc, 'arc_cw_q23.png', $0203));
  Tree.Items.AddChildObject(node, 'Quadrant III',
    TRenderParams.Create(@Render_Arc, 'arc_cw_q3.png', $0204));
  Tree.Items.AddChildObject(node, 'Quadrant III+IV',
    TRenderParams.Create(@Render_Arc, 'arc_cw_q34.png', $0205));
  Tree.Items.AddChildObject(node, 'Quadrant IV',
    TRenderParams.Create(@Render_Arc, 'arc_cw_q4.png', $0206));
  Tree.Items.AddChildObject(node, 'Quadrant IV+I',
    TRenderParams.Create(@Render_Arc, 'arc_cw_q41.png', $0207));

  Tree.Items.AddChildObject(node, 'Quadrant I, reverse',
    TRenderParams.Create(@Render_Arc, 'arc_rev_cw_q1.png', $0300));
  Tree.Items.AddChildObject(node, 'Quadrant I+II, reverse',
    TRenderParams.Create(@Render_Arc, 'arc_rev_cw_q12.png', $0301));
  Tree.Items.AddChildObject(node, 'Quadrant II, reverse',
    TRenderParams.Create(@Render_Arc, 'arc_rev_cw_q2.png', $0302));
  Tree.Items.AddChildObject(node, 'Quadrant II+III, reverse',
    TRenderParams.Create(@Render_Arc, 'arc_rev_cw_q23.png', $0303));
  Tree.Items.AddChildObject(node, 'Quadrant III, reverse',
    TRenderParams.Create(@Render_Arc, 'arc_rev_cw_q3.png', $0304));
  Tree.Items.AddChildObject(node, 'Quadrant III+IV, reverse',
    TRenderParams.Create(@Render_Arc, 'arc_rev_cw_q34.png', $0305));
  Tree.Items.AddChildObject(node, 'Quadrant IV, reverse',
    TRenderParams.Create(@Render_Arc, 'arc_rev_cw_q4.png', $0306));
  Tree.Items.AddChildObject(node, 'Quadrant IV+I, reverse',
    TRenderParams.Create(@Render_Arc, 'arc_rev_cw_q41.png', $0307));

  node := Tree.Items.AddChild(node1, 'counter-clockwise from point 1 to point 2');
  Tree.Items.AddChildObject(node, 'Quadrant I',
    TRenderParams.Create(@Render_Arc, 'arc_ccw_q1.png', $0000));
  Tree.Items.AddChildObject(node, 'Quadrant I+II',
    TRenderParams.Create(@Render_Arc, 'arc_ccw_q12.png', $0001));
  Tree.Items.AddChildObject(node, 'Quadrant II',
    TRenderParams.Create(@Render_Arc, 'arc_ccw_q2.png', $0002));
  Tree.Items.AddChildObject(node, 'Quadrant II+III',
    TRenderParams.Create(@Render_Arc, 'arc_ccw_q23.png', $0003));
  Tree.Items.AddChildObject(node, 'Quadrant III',
    TRenderParams.Create(@Render_Arc, 'arc_ccw_q3.png', $0004));
  Tree.Items.AddChildObject(node, 'Quadrant III+IV',
    TRenderParams.Create(@Render_Arc, 'arc_ccw_q34.png', $0005));
  Tree.Items.AddChildObject(node, 'Quadrant IV',
    TRenderParams.Create(@Render_Arc, 'arc_ccw_q4.png', $0006));
  Tree.Items.AddChildObject(node, 'Quadrant IV+I',
    TRenderParams.Create(@Render_Arc, 'arc_ccw_q41.png', $0007));

  Tree.Items.AddChildObject(node, 'Quadrant I, reverse',
    TRenderParams.Create(@Render_Arc, 'arc_rev_ccw_q1.png', $0100));
  Tree.Items.AddChildObject(node, 'Quadrant I+II, reverse',
    TRenderParams.Create(@Render_Arc, 'arc_rev_ccw_q12.png', $0101));
  Tree.Items.AddChildObject(node, 'Quadrant II, reverse',
    TRenderParams.Create(@Render_Arc, 'arc_rev_ccw_q2.png', $0102));
  Tree.Items.AddChildObject(node, 'Quadrant II+III, reverse',
    TRenderParams.Create(@Render_Arc, 'arc_rev_ccw_q23.png', $0103));
  Tree.Items.AddChildObject(node, 'Quadrant III, reverse',
    TRenderParams.Create(@Render_Arc, 'arc_rev_ccw_q3.png', $0104));
  Tree.Items.AddChildObject(node, 'Quadrant III+IV, reverse',
    TRenderParams.Create(@Render_Arc, 'arc_rev_ccw_q34.png', $0105));
  Tree.Items.AddChildObject(node, 'Quadrant IV, reverse',
    TRenderParams.Create(@Render_Arc, 'arc_rev_ccw_q4.png', $0106));
  Tree.Items.AddChildObject(node, 'Quadrant IV+I, reverse',
    TRenderParams.Create(@Render_Arc, 'arc_rev_ccw_q41.png', $0107));

  node1 := Tree.Items.AddChild(node0, 'elliptical');
  node := Tree.Items.AddChild(node1, 'clockwise from point 1 to point 2');
  Tree.Items.AddChildObject(node, 'Quadrant I',
    TRenderParams.Create(@Render_Arc, 'arc_ell_cw_q1.png', $1200));
  Tree.Items.AddChildObject(node, 'Quadrant I+II',
    TRenderParams.Create(@Render_Arc, 'arc_ell_cw_q12.png', $1201));
  Tree.Items.AddChildObject(node, 'Quadrant II',
    TRenderParams.Create(@Render_Arc, 'arc_ell_cw_q2.png', $1202));
  Tree.Items.AddChildObject(node, 'Quadrant II+III',
    TRenderParams.Create(@Render_Arc, 'arc_ell_cw_q23.png', $1203));
  Tree.Items.AddChildObject(node, 'Quadrant III',
    TRenderParams.Create(@Render_Arc, 'arc_ell_cw_q3.png', $1204));
  Tree.Items.AddChildObject(node, 'Quadrant III+IV',
    TRenderParams.Create(@Render_Arc, 'arc_ell_cw_q34.png', $1205));
  Tree.Items.AddChildObject(node, 'Quadrant IV',
    TRenderParams.Create(@Render_Arc, 'arc_ell_cw_q4.png', $1206));
  Tree.Items.AddChildObject(node, 'Quadrant IV+I',
    TRenderParams.Create(@Render_Arc, 'arc_ell_cw_q41.png', $1207));

  Tree.Items.AddChildObject(node, 'Quadrant I, reverse',
    TRenderParams.Create(@Render_Arc, 'arc_ell_rev_cw_q1.png', $1300));
  Tree.Items.AddChildObject(node, 'Quadrant I+II, reverse',
    TRenderParams.Create(@Render_Arc, 'arc_ell_rev_cw_q12.png', $1301));
  Tree.Items.AddChildObject(node, 'Quadrant II, reverse',
    TRenderParams.Create(@Render_Arc, 'arc_ell_rev_cw_q2.png', $1302));
  Tree.Items.AddChildObject(node, 'Quadrant II+III, reverse',
    TRenderParams.Create(@Render_Arc, 'arc_ell_rev_cw_q23.png', $1303));
  Tree.Items.AddChildObject(node, 'Quadrant III, reverse',
    TRenderParams.Create(@Render_Arc, 'arc_ell_rev_cw_q3.png', $1304));
  Tree.Items.AddChildObject(node, 'Quadrant III+IV, reverse',
    TRenderParams.Create(@Render_Arc, 'arc_ell_rev_cw_q34.png', $1305));
  Tree.Items.AddChildObject(node, 'Quadrant IV, reverse',
    TRenderParams.Create(@Render_Arc, 'arc_ell_rev_cw_q4.png', $1306));
  Tree.Items.AddChildObject(node, 'Quadrant IV+I, reverse',
    TRenderParams.Create(@Render_Arc, 'arc_ell_rev_cw_q41.png', $1307));

  node := Tree.Items.AddChild(node1, 'counter-clockwise from point 1 to point 2');
  Tree.Items.AddChildObject(node, 'Quadrant I',
    TRenderParams.Create(@Render_Arc, 'arc_ell_ccw_q1.png', $1000));
  Tree.Items.AddChildObject(node, 'Quadrant I+II',
    TRenderParams.Create(@Render_Arc, 'arc_ell_ccw_q12.png', $1001));
  Tree.Items.AddChildObject(node, 'Quadrant II',
    TRenderParams.Create(@Render_Arc, 'arc_ell_ccw_q2.png', $1002));
  Tree.Items.AddChildObject(node, 'Quadrant II+III',
    TRenderParams.Create(@Render_Arc, 'arc_ell_ccw_q23.png', $1003));
  Tree.Items.AddChildObject(node, 'Quadrant III',
    TRenderParams.Create(@Render_Arc, 'arc_ell_ccw_q3.png', $1004));
  Tree.Items.AddChildObject(node, 'Quadrant III+IV',
    TRenderParams.Create(@Render_Arc, 'arc_ell_ccw_q34.png', $1005));
  Tree.Items.AddChildObject(node, 'Quadrant IV',
    TRenderParams.Create(@Render_Arc, 'arc_ell_ccw_q4.png', $1006));
  Tree.Items.AddChildObject(node, 'Quadrant IV+I',
    TRenderParams.Create(@Render_Arc, 'arc_ell_ccw_q41.png', $1007));

  Tree.Items.AddChildObject(node, 'Quadrant I, reverse',
    TRenderParams.Create(@Render_Arc, 'arc_ell_rev_ccw_q1.png', $1100));
  Tree.Items.AddChildObject(node, 'Quadrant I+II, reverse',
    TRenderParams.Create(@Render_Arc, 'arc_ell_rev_ccw_q12.png', $1101));
  Tree.Items.AddChildObject(node, 'Quadrant II, reverse',
    TRenderParams.Create(@Render_Arc, 'arc_ell_rev_ccw_q2.png', $1102));
  Tree.Items.AddChildObject(node, 'Quadrant II+III, reverse',
    TRenderParams.Create(@Render_Arc, 'arc_ell_rev_ccw_q23.png', $1103));
  Tree.Items.AddChildObject(node, 'Quadrant III, reverse',
    TRenderParams.Create(@Render_Arc, 'arc_ell_rev_ccw_q3.png', $1104));
  Tree.Items.AddChildObject(node, 'Quadrant III+IV, reverse',
    TRenderParams.Create(@Render_Arc, 'arc_ell_rev_ccw_q34.png', $1105));
  Tree.Items.AddChildObject(node, 'Quadrant IV, reverse',
    TRenderParams.Create(@Render_Arc, 'arc_ell_rev_ccw_q4.png', $1106));
  Tree.Items.AddChildObject(node, 'Quadrant IV+I, reverse',
    TRenderParams.Create(@Render_Arc, 'arc_ell_rev_ccw_q41.png', $1107));

  node1 := Tree.Items.AddChild(node0, 'elliptical, rotated 30deg');
  node := Tree.Items.AddChild(node1, 'clockwise from point 1 to point 2');
  Tree.Items.AddChildObject(node, 'Quadrant I',
    TRenderParams.Create(@Render_Arc, 'arc_ellrot_cw_q1.png', $2200));
  Tree.Items.AddChildObject(node, 'Quadrant I+II',
    TRenderParams.Create(@Render_Arc, 'arc_ellrot_cw_q12.png', $2201));
  Tree.Items.AddChildObject(node, 'Quadrant II',
    TRenderParams.Create(@Render_Arc, 'arc_ellrot_cw_q2.png', $2202));
  Tree.Items.AddChildObject(node, 'Quadrant II+III',
    TRenderParams.Create(@Render_Arc, 'arc_ellrot_cw_q23.png', $2203));
  Tree.Items.AddChildObject(node, 'Quadrant III',
    TRenderParams.Create(@Render_Arc, 'arc_ellrot_cw_q3.png', $2204));
  Tree.Items.AddChildObject(node, 'Quadrant III+IV',
    TRenderParams.Create(@Render_Arc, 'arc_ellrot_cw_q34.png', $2205));
  Tree.Items.AddChildObject(node, 'Quadrant IV',
    TRenderParams.Create(@Render_Arc, 'arc_ellrot_cw_q4.png', $2206));
  Tree.Items.AddChildObject(node, 'Quadrant IV+I',
    TRenderParams.Create(@Render_Arc, 'arc_ellrot_cw_q41.png', $2207));

  Tree.Items.AddChildObject(node, 'Quadrant I, reverse',
    TRenderParams.Create(@Render_Arc, 'arc_ellrot_rev_cw_q1.png', $2300));
  Tree.Items.AddChildObject(node, 'Quadrant I+II, reverse',
    TRenderParams.Create(@Render_Arc, 'arc_ellrot_rev_cw_q12.png', $2301));
  Tree.Items.AddChildObject(node, 'Quadrant II, reverse',
    TRenderParams.Create(@Render_Arc, 'arc_ellrot_rev_cw_q2.png', $2302));
  Tree.Items.AddChildObject(node, 'Quadrant II+III, reverse',
    TRenderParams.Create(@Render_Arc, 'arc_ellrot_rev_cw_q23.png', $2303));
  Tree.Items.AddChildObject(node, 'Quadrant III, reverse',
    TRenderParams.Create(@Render_Arc, 'arc_ellrot_rev_cw_q3.png', $2304));
  Tree.Items.AddChildObject(node, 'Quadrant III+IV, reverse',
    TRenderParams.Create(@Render_Arc, 'arc_ellrot_rev_cw_q34.png', $2305));
  Tree.Items.AddChildObject(node, 'Quadrant IV, reverse',
    TRenderParams.Create(@Render_Arc, 'arc_ellrot_rev_cw_q4.png', $2306));
  Tree.Items.AddChildObject(node, 'Quadrant IV+I, reverse',
    TRenderParams.Create(@Render_Arc, 'arc_ellrot_rev_cw_q41.png', $2307));

  node := Tree.Items.AddChild(node1, 'counter-clockwise from point 1 to point 2');
  Tree.Items.AddChildObject(node, 'Quadrant I',
    TRenderParams.Create(@Render_Arc, 'arc_ellrot_ccw_q1.png', $2000));
  Tree.Items.AddChildObject(node, 'Quadrant I+II',
    TRenderParams.Create(@Render_Arc, 'arc_ellrot_ccw_q12.png', $2001));
  Tree.Items.AddChildObject(node, 'Quadrant II',
    TRenderParams.Create(@Render_Arc, 'arc_ellrot_ccw_q2.png', $2002));
  Tree.Items.AddChildObject(node, 'Quadrant II+III',
    TRenderParams.Create(@Render_Arc, 'arc_ellrot_ccw_q23.png', $2003));
  Tree.Items.AddChildObject(node, 'Quadrant III',
    TRenderParams.Create(@Render_Arc, 'arc_ellrot_ccw_q3.png', $2004));
  Tree.Items.AddChildObject(node, 'Quadrant III+IV',
    TRenderParams.Create(@Render_Arc, 'arc_ellrot_ccw_q34.png', $2005));
  Tree.Items.AddChildObject(node, 'Quadrant IV',
    TRenderParams.Create(@Render_Arc, 'arc_ellrot_ccw_q4.png', $2006));
  Tree.Items.AddChildObject(node, 'Quadrant IV+I',
    TRenderParams.Create(@Render_Arc, 'arc_ellrot_ccw_q41.png', $2007));

  Tree.Items.AddChildObject(node, 'Quadrant I, reverse',
    TRenderParams.Create(@Render_Arc, 'arc_ellrot_rev_ccw_q1.png', $2100));
  Tree.Items.AddChildObject(node, 'Quadrant I+II, reverse',
    TRenderParams.Create(@Render_Arc, 'arc_ellrot_rev_ccw_q12.png', $2101));
  Tree.Items.AddChildObject(node, 'Quadrant II, reverse',
    TRenderParams.Create(@Render_Arc, 'arc_ellrot_rev_ccw_q2.png', $2102));
  Tree.Items.AddChildObject(node, 'Quadrant II+III, reverse',
    TRenderParams.Create(@Render_Arc, 'arc_ellrot_rev_ccw_q23.png', $2103));
  Tree.Items.AddChildObject(node, 'Quadrant III, reverse',
    TRenderParams.Create(@Render_Arc, 'arc_ellrot_rev_ccw_q3.png', $2104));
  Tree.Items.AddChildObject(node, 'Quadrant III+IV, reverse',
    TRenderParams.Create(@Render_Arc, 'arc_ellrot_rev_ccw_q34.png', $2105));
  Tree.Items.AddChildObject(node, 'Quadrant IV, reverse',
    TRenderParams.Create(@Render_Arc, 'arc_ellrot_rev_ccw_q4.png', $2106));
  Tree.Items.AddChildObject(node, 'Quadrant IV+I, reverse',
    TRenderParams.Create(@Render_Arc, 'arc_ellrot_rev_ccw_q41.png', $2107));

  { -----------------------------------------------}
  node0 := Tree.Items.AddChild(nil, 'Gradients');
  { -----------------------------------------------}
  node := Tree.Items.AddChild(node0, 'horizontal');
  Tree.Items.AddChildObject(node, 'Circle',
    TRenderParams.Create(@Render_Shape, 'circle_gradienthor.png', $0101));
  Tree.Items.AddChildObject(node, 'Ellipse',
    TRenderParams.Create(@Render_Shape, 'ellipse_gradienthor.png', $0201));
  Tree.Items.AddChildObject(node, 'Rectangle',
    TRenderParams.Create(@Render_Shape, 'rect_gradienthor.png', $0301));
  Tree.Items.AddChildObject(node, 'Rounded rectangle',
    TRenderParams.Create(@Render_Shape, 'rounded_rect_gradienthor.png', $0401));
  Tree.Items.AddChildObject(node, 'Polygon',
    TRenderParams.Create(@Render_Shape, 'polygon_gradienthor.png', $0501));

  node := Tree.Items.AddChild(node0, 'vertical');
  Tree.Items.AddChildObject(node, 'Circle',
    TRenderParams.Create(@Render_Shape, 'circle_gradientvert.png', $0102));
  Tree.Items.AddChildObject(node, 'Ellipse',
    TRenderParams.Create(@Render_Shape, 'ellipse_gradientvert.png', $0202));
  Tree.Items.AddChildObject(node, 'Rectangle',
    TRenderParams.Create(@Render_Shape, 'rect_gradientvert.png', $0302));
  Tree.Items.AddChildObject(node, 'Rounded rectangle',
    TRenderParams.Create(@Render_Shape, 'rounded_rect_gradientvert.png', $0402));
  Tree.Items.AddChildObject(node, 'Polygon',
    TRenderParams.Create(@Render_Shape, 'polygon_gradientvert.png', $0502));

  node := Tree.Items.AddChild(node0, 'linear');
  Tree.Items.AddChildObject(node, 'Circle',
    TRenderParams.Create(@Render_Shape, 'circle_gradientlinear.png', $0103));
  Tree.Items.AddChildObject(node, 'Ellipse',
    TRenderParams.Create(@Render_Shape, 'ellipse_gradientlinear.png', $0203));
  Tree.Items.AddChildObject(node, 'Rectangle',
    TRenderParams.Create(@Render_Shape, 'rect_gradientlinear.png', $0303));
  Tree.Items.AddChildObject(node, 'Rounded rectangle',
    TRenderParams.Create(@Render_Shape, 'rounded_rect_gradientlinear.png', $0403));
  Tree.Items.AddChildObject(node, 'Polygon',
    TRenderParams.Create(@Render_Shape, 'polygon_gradientlinear.png', $0503));

  node := Tree.Items.AddChild(node0, 'radial');
  Tree.Items.AddChildObject(node, 'Circle',
    TRenderParams.Create(@Render_Shape, 'circle_gradientradial.png', $0104));
  Tree.Items.AddChildObject(node, 'Ellipse',
    TRenderParams.Create(@Render_Shape, 'ellipse_gradientradial.png', $0204));
  Tree.Items.AddChildObject(node, 'Rectangle',
    TRenderParams.Create(@Render_Shape, 'rect_gradientradial.png', $0304));
  Tree.Items.AddChildObject(node, 'Rounded rectangle',
    TRenderParams.Create(@Render_Shape, 'rounded_rect_gradientradial.png', $0404));
  Tree.Items.AddChildObject(node, 'Polygon',
    TRenderParams.Create(@Render_Shape, 'polygon_gradientradial.png', $0504));

  { -----------------------------------------------}
  node0 := Tree.Items.AddChild(nil, 'Text');
  { -----------------------------------------------}
  node := Tree.Items.AddChild(node0, 'horizontal');
  Tree.Items.AddChildObject(node, 'left aligned',
    TRenderParams.Create(@Render_Text, 'text_left.png', $0000));
  Tree.Items.AddChildObject(node, 'centered',
    TRenderParams.Create(@Render_Text, 'text_center.png', $0001));
  Tree.Items.AddChildObject(node, 'right aligned',
    TRenderParams.Create(@Render_Text, 'text_right.png', $0002));

  node := Tree.Items.AddChild(node0, 'rotated by 30 deg');
  Tree.Items.AddChildObject(node, 'left aligned',
    TRenderParams.Create(@Render_Text, 'text_left_30.png', $1000));
  Tree.Items.AddChildObject(node, 'centered',
    TRenderParams.Create(@Render_Text, 'text_center_30.png', $1001));
  Tree.Items.AddChildObject(node, 'right aligned',
    TRenderParams.Create(@Render_Text, 'text_right_30.png', $1002));

  node := Tree.Items.AddChild(node0, 'vertical (90 deg, upward)');
  Tree.Items.AddChildObject(node, 'left aligned',
    TRenderParams.Create(@Render_Text, 'text_left_90.png', $2000));
  Tree.Items.AddChildObject(node, 'centered',
    TRenderParams.Create(@Render_Text, 'text_center_90.png', $2001));
  Tree.Items.AddChildObject(node, 'right aligned',
    TRenderParams.Create(@Render_Text, 'text_right_90.png', $2002));

  node := Tree.Items.AddChild(node0, 'vertical (-90 deg, downward)');
  Tree.Items.AddChildObject(node, 'left aligned',
    TRenderParams.Create(@Render_Text, 'text_left_m90.png', $3000));
  Tree.Items.AddChildObject(node, 'centered',
    TRenderParams.Create(@Render_Text, 'text_center_m90.png', $3001));
  Tree.Items.AddChildObject(node, 'right aligned',
    TRenderParams.Create(@Render_Text, 'text_right_m90.png', $3002));

  node := Tree.Items.AddChild(node0, 'Fonts');
  Tree.Items.AddChildObject(node, 'Times New Roman + Courier New',
    TRenderParams.Create(@Render_Text_Fonts, 'text_fonts.png'));
  Tree.Items.AddChildObject(node0, 'Text colors',
    TRenderParams.Create(@Render_Text_Colors, 'text_colors.png'));
end;

procedure TMainForm.Render_Shape(APage: TvVectorialPage;
  AIntParam: Integer);
{ AIntParam and $00FF = 0 --> solid fill
                        1 --> horizontal gradient
                        2 --> vertical gradient
                        3 --> linear gradient
                        4 --> radial gradient
  AIntParam and $FF00 = $0100 --> circle
                        $0200 --> ellipse
                        $0300 --> rectangle
                        $0400 --> rounded rect
                        $0500 --> polygon (triangle) }
var
  ent: TvEntityWithPenAndBrush;
begin
  case AIntParam and $FF00 of
    $0100: ent := CreateStdCircle(APage);
    $0200: ent := CreateStdEllipse(APage);
    $0300: ent := CreateStdRect(APage);
    $0400: ent := CreateStdRoundedRect(APage);
    $0500: ent := CreateStdPolygon(APage);
    else   raise Exception.Create('Shape not supported.');
  end;
  case AIntParam and $00FF of
    $0000: ent.Brush := StdSolidBrush;
    $0001: ent.Brush := StdHorizGradientBrush;
    $0002: ent.Brush := StdVertGradientBrush;
    $0003: ent.Brush := StdLinearGradientBrush;
    $0004: ent.Brush := StdRadialGradientBrush;
    else raise Exception.Create('Brush not supported');
  end;
  APage.AddEntity(ent);
end;

procedure TMainForm.Render_Arc(APage: TvVectorialPage; AIntParam: Integer);
//
// AIntParam and $000F = $0000  --> circular arc
//                       $1000  --> elliptical arc
//                       $2000  --> elliptical arc, rotated
// AIntParam and $000F = $0000  --> quarter 1
//                       $0001  --> quarter 1 + 2
//                       $0002  --> quarter 2
//                       $0003  --> quarter 2 + 3
//                       $0004  --> quarter 3
//                       $0005  --> quarter 3+4
//                       $0006  --> quarter 4
//                       $0007  --> quarter 4+1
// AIntParam and $0100 = $0100  --> start and end points exchanged
// AIntParan and $0200 = $0200  --> clockwise
const
  ROT_ANGLE = 30;
  RY_MULT = 0.6;
  CX = 50;
  CY = 55;
  R = 30;
var
  isReversed, isClockwise, isEllipse, isRotated: Boolean;
  p: T3dPoint;
  x1, y1, x2, y2, rx, ry: Double;
  startAngle, endAngle, phi: Double;
begin
  isReversed := AIntParam and $0100 <> 0;
  isClockwise := AIntParam and $0200 <> 0;
  isEllipse := AIntParam and $F000 <> 0;
  isRotated := AIntParam and $F000 = $2000;

  rx := R;
  ry := IfThen(isEllipse, R * RY_MULT, R);
  phi := IfThen(isRotated, DegToRad(ROT_ANGLE), 0.0);

  startAngle := DegToRad((AIntParam and $000F) * 45);  //  0°,  45°,  90°, ...
  endAngle := startAngle + pi/2;                       // 90°, 135°, 180°, ...
  x1 := CX + rx * cos(startAngle);
  y1 := CY + ry * sin(startAngle);
  x2 := CX + rx * cos(endAngle);
  y2 := CY + ry * sin(endAngle);
  if isRotated then begin
    p := Rotate3DPointInXY(Make3DPoint(x1, y1), Make3DPoint(CX, CY), -phi);
    // See comment at Rotate3DPointInXY regarding the negative sign of phi
    x1 := p.x;
    y1 := p.y;
    p := Rotate3DPointInXY(Make3DPoint(x2, y2), Make3DPoint(CX, CY), -phi);
    x2 := p.x;
    y2 := p.y;
  end;

  if isReversed then
    CreateArc(APage, x2, y2, x1, y1, CX, CY, rx, ry, phi, isClockwise)
  else
    CreateArc(APage, x1, y1, x2, y2, CX, CY, rx, ry, phi, isClockwise);
end;

procedure TMainForm.Render_Path_Hole_SolidFill(APage: TvVectorialPage;
  AIntParam: Integer);
var
  obj: TPath;
begin
  obj := CreatePathWithHole(APage);  // no need to AddEntity!
  obj.Brush.Color := colYellow;
  obj.Brush.Style := bsSolid;
  obj.Pen.Width := 3;
  obj.Pen.Color := colBlue;
end;

procedure TMainForm.Render_Path_Hole_GradientFill(APage: TvVectorialPage;
  AIntParam: Integer);
var
  obj: TPath;
begin
  obj := CreatePathWithHole(APage);  // No need to AddEntity!
  obj.Brush := StdLinearGradientBrush;
  obj.Pen.Width := 3;
  obj.Pen.Color := colBlue;
end;

procedure TMainForm.Render_SelfIntersectingPoly_SolidFill_EvenOdd(
  APage: TvVectorialPage; AIntParam: Integer);
var
  obj: TvPolygon;
begin
  obj := CreateStdSelfIntersectingPolygon(APage);
  obj.WindingRule := vcmEvenOddRule;
  obj.Brush := StdSolidBrush;
  APage.AddEntity(obj);
end;

procedure TMainForm.Render_SelfIntersectingPoly_GradientFill_EvenOdd(
  APage: TvVectorialPage; AIntParam: Integer);
var
  obj: TvPolygon;
begin
  obj := CreateStdSelfIntersectingPolygon(APage);
  obj.Brush := StdHorizGradientBrush;
  obj.WindingRule := vcmEvenOddRule;
  APage.AddEntity(obj);
end;

procedure TMainForm.Render_SelfIntersectingPoly_SolidFill_NonZeroWinding(
  APage: TvVectorialPage; AIntParam: Integer);
var
  obj: TvPolygon;
begin
  obj := CreateStdSelfIntersectingPolygon(APage);
  obj.WindingRule := vcmNonZeroWindingRule;
  obj.Brush := StdSolidBrush;
  APage.AddEntity(obj);
end;

procedure TMainForm.Render_SelfIntersectingPoly_GradientFill_NonZeroWinding(
  APage: TvVectorialPage; AIntParam: Integer);
var
  obj: TvPolygon;
begin
  obj := CreateStdSelfIntersectingPolygon(APage);
  obj.Brush := StdHorizGradientBrush;
  obj.WindingRule := vcmNonzeroWindingRule;
  APage.AddEntity(obj);
end;

procedure TMainForm.Render_Text(APage: TvVectorialPage; AIntParam: Integer);
{ AIntParam and $000F = $0000  --> anchor at left
                        $0001  --> anchor at center
                        $0002  --> anchor at right
  AIntParam and $F000 = $0000  --> horizontal
                        $1000  --> rotated 30deg
                        $2000  --> rotated 90deg
                        $3000  --> rotated -90deg }
const
  XTEXT = 50;
  YTEXT = 40;  // we assume that y points up
  L = 10;
var
  txt: TvText;
  p: TPath;
  angle: double;
  anchor: TvTextAnchor;
begin
  case AIntParam and $000F of
    $0000 : anchor := vtaStart;
    $0001 : anchor := vtaMiddle;
    $0002 : anchor := vtaEnd;
    else raise Exception.Create('Text anchor not supported');
  end;
  case AIntParam and $F000 of
    $0000 : angle := 0;
    $1000 : angle := 30;
    $2000 : angle := 90;
    $3000 : angle := -90;
    else raise Exception.Create('Text angle not supported.');
  end;

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
  p := APage.EndPath;
  p.Pen.Width := 1;
  p.Pen.Color := colRed;

  // Draw text
  txt := TvText.Create(APage);
  txt.X := XTEXT;
  if APage.UseTopLeftCoordinates then
    txt.Y := PAGE_SIZE - YTEXT else
    txt.Y := YTEXT;
  txt.Value.Add('ABC');
  txt.Font.Size := 14;
  txt.TextAnchor := anchor;
  txt.Font.Orientation := angle;

  APage.AddEntity(txt);
end;

procedure TMainForm.Render_Text_Fonts(APage: TvVectorialPage;
  AIntParam: Integer);
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

procedure TMainForm.Render_Text_Colors(APage: TvVectorialPage;
  AIntParam: Integer);
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

  // Render document with bottom/left origin
  CreateDocument(FDocBottomLeft, page, false);
  renderParams.OnRender(page, renderParams.IntParam);
  BottomLeftPaintbox.Invalidate;

  // Render document with top/left origin
  CreateDocument(FDocTopLeft, page, true);
  renderParams.OnRender(page, renderParams.IntParam);
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

