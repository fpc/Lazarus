{ Current issues:
  - Radial gradient not rendered correctly (position, colors), saving to svg ok.
  - Save polygon to svg empty
  - Nonzero/even-odd winding rule not working
}

unit vtmain;

{$mode objfpc}{$H+}

interface

uses              lazlogger,
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, ComCtrls, Buttons, fpimage, fpvectorial, Types;

type

  TRenderEvent = procedure(APage: TvVectorialPage; AIntParam: Integer = MaxInt) of object;

  TRenderState = (rsUnknown, rsPassed, rsFailed);

  TRenderParams = class
    RefFile: String;
    IntParam: Integer;
    OnRender: TRenderEvent;
    RenderState: array[0..1] of TRenderState;  // 0 = svg, 1 = wmf
    constructor Create(ARenderEvent: TRenderEvent; ARefFilename: String;
      AIntParam: Integer = MaxInt);
  end;

  TRenderCoords  = (rcBottomLeftCoords, rcTopLeftCoords);

  { TMainForm }

  TMainForm = class(TForm)
    BtnSaveAsRef: TButton;
    BtnSaveToFiles: TButton;
    BtnViewBottomLeft: TButton;
    BtnViewTopLeft: TButton;
    CbFileFormat: TComboBox;
    gbWRBottomLeft: TGroupBox;
    gbRenderTest: TGroupBox;
    gbBottomLeft: TGroupBox;
    gbWRTopLeft: TGroupBox;
    gbTopLeft: TGroupBox;
    gbReferenceImageTest: TGroupBox;
    GroupBox1: TGroupBox;
    gbReadWriteTest: TGroupBox;
    GbTree: TGroupBox;
    gbResults: TGroupBox;
    imgUnknown: TImage;
    ImgPassed: TImage;
    ImgFailed: TImage;
    ImageList: TImageList;
    Label1: TLabel;
    Label14: TLabel;
    LblBothImagesMustMatch1: TLabel;
    rbUnknown: TRadioButton;
    rbPassed: TRadioButton;
    rbFailed: TRadioButton;
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
    WRTopLeftPaintbox: TPaintBox;
    TopLeftPaintbox: TPaintBox;
    WRBottomLeftPaintbox: TPaintBox;
    AllTestsPanel: TPanel;
    Tree: TTreeView;
    procedure BtnSaveToFilesClick(Sender: TObject);
    procedure BtnSaveAsRefClick(Sender: TObject);
    procedure BtnViewImageClick(Sender: TObject);
    procedure CbFileFormatChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure PaintBoxPaint(Sender: TObject);
    procedure ResultStateChange(Sender: TObject);
    procedure rgTestResultsSelectionChanged(Sender: TObject);
    procedure TreeCustomDrawItem(Sender: TCustomTreeView; Node: TTreeNode;
      State: TCustomDrawState; var DefaultDraw: Boolean);
    procedure TreeGetImageIndex(Sender: TObject; Node: TTreeNode);
    procedure TreeGetSelectedIndex(Sender: TObject; Node: TTreeNode);
    procedure TreeSelectionChanged(Sender: TObject);

  private
    { private declarations }
    FDoc: array[TRenderCoords] of TvVectorialDocument;
    FDocFromWMF: array[TRenderCoords] of TvVectorialDocument;
    FDocFromSVG: array[TRenderCoords] of TvVectorialDocument;
    FLockResults: Integer;
    function GetFileFormat: TvVectorialFormat;
    function GetFileFormatExt: String;
    procedure Populate;
    procedure PrepareDoc(var ADoc: TvVectorialDocument; var APage: TvVectorialPage;
      AUseTopLeftCoords: boolean);
    procedure ShowFileImage(AFilename: String; AUseTopLeftCoords: Boolean;
      APaintbox: TPaintbox);
    procedure ShowRefImageTest;
    procedure ShowRenderTestImages;
    procedure ShowWriteReadTestImages;
    procedure UpdateCmdStates;
    procedure UpdateResultStates;
    procedure UpdateTestResults;

    procedure ReadIni;
    procedure WriteIni;

  private
    // Simple shapes, solid fills and gradients
    procedure Render_Shape(APage: TvVectorialPage; AIntParam: Integer);

    // Complex shapes
    procedure Render_Path_Hole(APage: TvVectorialPage; AIntParam: Integer);
    procedure Render_SelfIntersectingPoly(APage: TvVectorialPage; AIntParam: Integer);

    // Arcs
    procedure Render_Arc(APage: TvVectorialPage; AIntParam: Integer);

    // Bezier
    procedure Render_Bezier(Apage: TvVectorialPage; AIntParam: Integer);

    // Text - single line
    procedure Render_Text(APage: TvVectorialpage; AIntParam: Integer);
    procedure Render_Text_Fonts(APage: TvVectorialPage; AIntParam: Integer);
    procedure Render_Text_Colors(APage: TvVectorialPage; AIntParam: Integer);

    // Text as paragraph: 2 lines
    procedure Render_2Lines(APage: TvVectorialPage; AIntParam: Integer);

  public
    { public declarations }
  end;

var
  MainForm: TMainForm;


implementation

{$R *.lfm}

uses
  Math, TypInfo, FPCanvas, IniFiles, LazFileUtils, LCLIntf,
  fpvutils, vtprimitives;

const
  IMG_FOLDER = 'images' + PathDelim;
  REFIMG_FOLDER = IMG_FOLDER + 'ref' + PathDelim;
  NOT_SAVED = '(not saved)';
  FORMAT_SEPARATOR = ';';

function RenderStateToStr(AState: TRenderState): String;
begin
  Result := GetEnumName(TypeInfo(TRenderState), ord(AState));
  Delete(Result, 1, 2);
end;

function StrToRenderState(s: String): TRenderState;
var
  n: Integer;
  p: Integer;
begin
  Result := rsUnknown;

  p := pos(':', s);
  if p > 0 then
    s := Copy(s, p+1);
  if s = '' then
    exit;

  n := GetEnumValue(TypeInfo(TRenderState), 'rs' + s);
  if n in [0..2] then
    Result := TRenderState(n);
end;


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
  if FDoc[rcBottomLeftCoords] = nil then
    exit;

  page := FDoc[rcBottomLeftCoords].GetPageAsVectorial(0);

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
      ForceDirectory(REFIMG_FOLDER);
      fn := REFIMG_FOLDER + renderParams.RefFile;
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

procedure TMainForm.BtnSaveToFilesClick(Sender: TObject);
var
  fn: String;
  renderParams: TRenderParams;
  folder: String;
  fmt: TvVectorialFormat;
  ext: String;
begin
  renderParams := TRenderParams(Tree.Selected.Data);
  if RenderParams = nil then
    exit;

  fmt := GetFileFormat;
  ext := GetFileFormatExt;
  folder := IMG_FOLDER + ext + PathDelim;
  ForceDirectory(folder);

  if FDoc[rcBottomLeftCoords] <> nil then begin
    fn := folder + 'bl_' + ChangeFileExt(renderParams.RefFile, '.' + ext);
    FDoc[rcBottomLeftCoords].WriteToFile(fn, fmt);
    ShowFileImage(fn, false, WRBottomLeftPaintbox);
  end;

  if FDoc[rcTopLeftCoords] <> nil then begin
    fn := folder + 'tl_' + ChangeFileExt(renderParams.RefFile, '.' + ext);
    FDoc[rcTopLeftCoords].WriteToFile(fn, fmt);
    ShowFileImage(fn, true, WRTopLeftPaintbox);
  end;

  UpdateCmdStates;
end;

procedure TMainForm.BtnViewImageClick(Sender: TObject);
var
  fn: String;
  ext: String;
  folder: String;
  renderParams: TRenderParams;
begin
  BtnSaveToFilesClick(nil);

  renderParams := TRenderParams(Tree.Selected.Data);
  if renderParams = nil then
    exit;

  ext := GetFileFormatExt;
  folder := IMG_FOLDER + ext + PathDelim;

  if Sender = BtnViewBottomLeft then
    fn := folder + 'bl_' + ChangeFileExt(renderParams.RefFile, '.' + ext)
  else if Sender = BtnViewTopLeft then
    fn := folder + 'tl_' + ChangeFileExt(renderParams.RefFile, '.' + ext)
  else
    raise Exception.Create('BtnViewImageClick: this sender is not supported.');

  if FileExists(fn) then
    OpenDocument(fn);
end;

procedure TMainForm.CbFileFormatChange(Sender: TObject);
begin
  ShowWriteReadTestImages;
  UpdateCmdStates;
  UpdateResultStates;
  UpdateTestResults;
end;

procedure TMainForm.PrepareDoc(var ADoc: TvVectorialDocument;
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
  WRBottomLeftPaintbox.Hint := NOT_SAVED;
  WRTopLeftPaintbox.Hint := NOT_SAVED;

  Populate;
  ReadIni;
  TreeSelectionChanged(nil);
end;

procedure TMainForm.FormDestroy(Sender: TObject);
var
  parentnode, node: TTreeNode;
  rc: TRenderCoords;
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

  for rc in TRenderCoords do begin
    FreeAndNil(FDoc[rc]);
    FreeAndNil(FDocFromSVG[rc]);
    FreeAndNil(FDocFromWMF[rc]);
  end;

  WriteIni;
end;

function TMainForm.GetFileFormat: TvVectorialFormat;
begin
  case CbFileFormat.ItemIndex of
    0: Result := vfSVG;
    1: Result := vfWindowsMetafileWMF;
    else raise Exception.Create('Format not supported');
  end;
end;

function TMainForm.GetFileFormatExt: String;
begin
  case CbFileFormat.ItemIndex of
    0: Result := 'svg';
    1: Result := 'wmf';
    else raise Exception.Create('Format not supported');
  end;
end;

procedure TMainForm.PaintBoxPaint(Sender: TObject);
var
  doc: TvVectorialDocument;
  page: TvVectorialPage;
  w, h: Integer;
  fmt: TvVectorialFormat;
  rc: TRenderCoords;
  factorX, factorY: Double;
begin
  fmt := GetFileFormat;

  if (Sender = BottomLeftPaintbox) or (Sender = WRBottomLeftPaintbox) then
    rc := rcBottomLeftCoords
  else
  if (Sender = TopLeftPaintbox) or (Sender = WRTopLeftPaintbox) then
    rc := rcTopLeftCoords
  else
    raise Exception.Create('This sender is not supported here.');

  doc := nil;
  if (Sender = BottomLeftPaintbox) or (Sender = TopLeftPaintbox) then
    doc := FDoc[rc]
  else
  if (Sender = WRBottomLeftPaintbox) or (Sender = WRTopLeftPaintbox) then
    case GetFileFormat of
      vfSVG:
        doc := FDocFromSVG[rc];
      vfWindowsMetafileWMF:
        doc := FDocFromWMF[rc];
      else
        raise Exception.Create('File format not supported.');
    end;

  w := TPaintbox(Sender).Width;
  h := TPaintbox(Sender).Height;

  if doc = nil then begin
    TPaintbox(Sender).Canvas.Brush.Color := clDefault;
    TPaintbox(Sender).Canvas.Brush.Style := bsSolid;
    TPaintbox(Sender).Canvas.FillRect(0, 0, w, h);
    exit;
  end;

  page := doc.GetPageAsVectorial(0);
  factorX := w / page.Width;
  factorY := h / page.Height;
  page.DrawBackground(TPaintbox(Sender).Canvas);
  if page.UseTopLeftCoordinates then
    page.Render(TPaintbox(Sender).Canvas, 0, 0, factorX, factorY)
  else
    page.Render(TPaintbox(Sender).Canvas, 0, h, factorX, -factorY);
end;

procedure TMainForm.ResultStateChange(Sender: TObject);
var
  renderParams: TRenderParams;
begin
  if FLockResults > 0 then
    exit;
  if (Tree.Selected <> nil) and (Tree.Selected.Data <> nil) then
  begin
    renderParams := TRenderParams(Tree.Selected.Data);
    if rbUnknown.Checked then
      renderParams.RenderState[cbFileFormat.ItemIndex] := rsUnknown
    else if rbPassed.Checked then
      renderParams.RenderState[cbFileFormat.ItemIndex] := rsPassed
    else if rbFailed.Checked then
      renderParams.RenderState[cbFileFormat.ItemIndex] := rsFailed;
    TreeGetImageIndex(nil, Tree.Selected);
    Tree.Invalidate;
  end;
end;

procedure TMainForm.rgTestResultsSelectionChanged(Sender: TObject);
var
  renderParams: TRenderParams;
begin
  if FLockResults > 0 then
    exit;
  if (Tree.Selected <> nil) and (Tree.Selected.Data <> nil) then
  begin
    renderParams := TRenderParams(Tree.Selected.Data);

    //renderParams.RenderState[CbFileFormat.ItemIndex] := TRenderState(rgTestResults.ItemIndex);
    TreeGetImageIndex(nil, Tree.Selected);
    Tree.Invalidate;
  end;
end;

procedure TMainForm.Populate;
var
  mainNode: TTreeNode;
  node, node1, node2: TTreeNode;  // needed by include files
begin
  Tree.Items.Clear;

  { --------------------------------------------------}
  mainnode := Tree.Items.AddChild(nil, 'Simple shapes');
  { --------------------------------------------------}
  {$I vt_simpleshapes.inc}

  { --------------------------------------------------}
  mainnode := Tree.Items.AddChild(nil, 'Complex shapes');
  { --------------------------------------------------}
  {$I vt_complexshapes.inc}

  { -----------------------------------------}
  mainnode := Tree.Items.AddChild(nil, 'Arcs');
  { -----------------------------------------}
  {$I vt_arcs_circular.inc}
  {$I vt_arcs_elliptical.inc}
  {$I vt_arcs_elliptical_rotated.inc}

  { -----------------------------------------------}
  node := Tree.Items.AddChild(nil, 'Bezier');
  { -----------------------------------------------}
  Tree.Items.AddChildObject(node, 'Single segment (rotated around (10,10) by 30° CCW)',
    TRenderParams.Create(@Render_Bezier, 'bezier_rot30ccw.png', $00010000));
  Tree.Items.AddChildObject(node, 'Single segment (normal)',
    TRenderParams.Create(@Render_Bezier, 'bezier.png'));
  Tree.Items.AddChildObject(node, 'Single segment (rotated around (10,10) by 30° CW)',
    TRenderParams.Create(@Render_Bezier, 'bezier_rot30cw.png', $00020000));

  { -----------------------------------------------}
  mainnode := Tree.Items.AddChild(nil, 'Gradients');
  { -----------------------------------------------}
  {$I vt_gradients.inc}

  { -----------------------------------------------}
  mainnode := Tree.Items.AddChild(nil, 'Text');
  { -----------------------------------------------}
  {$I vt_text.inc}
end;

procedure TMainForm.Render_Shape(APage: TvVectorialPage;
  AIntParam: Integer);
{ AIntParam and $000000FF = $00000000 --> solid fill
                            $00000001 --> horizontal gradient
                            $00000002 --> vertical gradient
                            $00000003 --> linear gradient
                            $00000004 --> radial gradient (centered)
                            $00000005 --> radial gradient (off-center)
  AIntParam and $0000FF00 = $00000100 --> circle
                            $00000200 --> ellipse
                            $00000300 --> rectangle
                            $00000400 --> rounded rect
                            $00000500 --> polygon (triangle)
  AIntParam and $000F0000 = $00010000 --> rotation of entire shape by 30°C
                            $00020000 --> rotation of entire shape by -30°C
}
var
  ent: TvEntityWithPenAndBrush;
begin
  case AIntParam and $0000FF00 of
    $00000100: ent := CreateStdCircle(APage);
    $00000200: ent := CreateStdEllipse(APage);
    $00000300: ent := CreateStdRect(APage);
    $00000400: ent := CreateStdRoundedRect(APage);
    $00000500: ent := CreateStdPolygon(APage);
    else   raise Exception.Create('Shape not supported.');
  end;
  case AIntParam and $000000FF of
    $00000000: ent.Brush := StdSolidBrush(colRed);
    $00000001: ent.Brush := StdHorizGradientBrush(colYellow, colRed);
    $00000002: ent.Brush := StdVertGradientBrush(colYellow, colRed);
    $00000003: ent.Brush := StdLinearGradientBrush(colYellow, colRed);
    $00000004: ent.Brush := StdRadialGradientBrush(colYellow, colRed, 0.5, 0.5, 0.5);
    $00000005: ent.Brush := StdRadialGradientBrush(colYellow, colRed, 0.25, 0.25, 0.75);
    else raise Exception.Create('Brush not supported');
  end;
  case AIntParam and $000F0000 of
    $00010000: Rotate(APage, ent, 30);
    $00020000: Rotate(APage, ent, -30);
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
// AIntParam and $0200 = $0200  --> clockwise
//
// AIntParam and $000F0000 = $00010000 --> rotation by 30°C
//                           $00020000 --> rotation by -30°C
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
  arc: TPath;
  txt1, txt2: TvText;
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
    CreateArc(APage, x2, y2, x1, y1, CX, CY, rx, ry, phi, isClockwise, arc, txt1, txt2)
  else
    CreateArc(APage, x1, y1, x2, y2, CX, CY, rx, ry, phi, isClockwise, arc, txt1, txt2);

  case AIntParam and $000F0000 of
    $00010000:
      begin
        Rotate(APage, arc, 30);
        Rotate(APage, txt1, 30);
        Rotate(APage, txt2, 30);
      end;
    $00020000:
      begin
        Rotate(APage, arc, -30);
        Rotate(APage, txt1, -30);
        Rotate(APage, txt2, -30);
      end;
  end;

end;

procedure TMainForm.Render_Bezier(APage: TvVectorialpage; AIntParam: Integer);
const
  X1 = 10;
  Y1 = 25;
  X2 = 30;
  Y2 = 80;
  X3 = 50;
  Y3 = 70;
  X4 = 90;
  Y4 = 25;
var
  bezier, line1, line2: TPath;
  txt1, txt2, txt3, txt4: TvText;
begin
  CreateBezier(APage, X1,Y1, X2,Y2, X3,Y3, X4,Y4, bezier, line1, line2, txt1, txt2, txt3, txt4);

  case AIntParam and $000F0000 of
    $00010000:
      begin
        Rotate(APage, bezier, 30);
        Rotate(APage, line1, 30);
        Rotate(APage, line2, 30);
        Rotate(APage, txt1, 30);
        Rotate(APage, txt2, 30);
        Rotate(APage, txt3, 30);
        Rotate(APage, txt4, 30);
      end;
    $00020000:
      begin
        Rotate(APage, bezier, -30);
        Rotate(APage, line1, -30);
        Rotate(APage, line2, -30);
        Rotate(APage, txt1, -30);
        Rotate(APage, txt2, -30);
        Rotate(APage, txt3, -30);
        Rotate(APage, txt4, -30);
      end;
  end;

end;

procedure TMainForm.Render_Path_Hole(APage: TvVectorialPage;
  AIntParam: Integer);
{ AIntParam and $000000FF = $00000000 --> solid fill
                            $00000001 --> horizontal gradient
                            $00000002 --> vertical gradient
                            $00000003 --> linear gradient
                            $00000004 --> radial gradient
  AIntParam and $000F0000 = $00010000 --> rotation of entire shape by 30°C
                            $00020000 --> rotation of entire shape by -30°C
}
var
  obj: TPath;
begin
  obj := CreatePathWithHole(APage);  // no need to AddEntity!
  obj.Pen.Width := 3;
  obj.Pen.Color := colBlue;
  case AIntParam and $000000FF of
    $00000000: obj.Brush := StdSolidBrush(colYellow);
    $00000001: obj.Brush := StdHorizGradientBrush(colYellow, colRed);
    $00000002: obj.Brush := StdVertGradientBrush(colYellow, colRed);
    $00000003: obj.Brush := StdLinearGradientBrush(colYellow, colRed);
    $00000004: obj.Brush := StdRadialGradientBrush(colYellow, colRed, 0.5, 0.5, 0.5);
    $00000005: obj.Brush := StdRadialGradientBrush(colYellow, colRed, 0.25, 0.25, 0.75);
    else raise Exception.Create('Brush not supported');
  end;
  case AIntParam and $000F0000 of
    $00010000: Rotate(APage, obj, 30);
    $00020000: Rotate(APage, obj, -30);
  end;
end;

procedure TMainForm.Render_SelfIntersectingPoly(APage: TvVectorialPage;
  AIntParam: Integer);
{ AIntParam and $0000000F = $00000000 --> solid fill
                            $00000001 --> horizontal gradient
                            $00000002 --> vertical gradient
                            $00000003 --> linear gradient
                            $00000004 --> radial gradient
  AIntParam and $00000F00 = $00000000 --> even-odd rule
                            $00000100 --> non-zero winding rule
  AIntParam and $000F0000 = $00010000 --> rotation of entire shape by 30°C
                            $00020000 --> rotation of entire shape by -30°C
}
var
  obj: TvPolygon;
begin
  obj := CreateStdSelfIntersectingPolygon(APage);
  case AIntParam and $0000000F of
    $00000000: obj.Brush := StdSolidBrush(colRed);
    $00000001: obj.Brush := StdHorizGradientBrush(colBlue, colWhite);
    $00000002: obj.Brush := StdVertGradientBrush(colBlue, colWhite);
    $00000003: obj.Brush := StdLinearGradientBrush(colBlue, colWhite);
    $00000004: obj.Brush := StdRadialGradientBrush(colBlue, colWhite, 0.5, 0.5, 0.5);
    $00000005: obj.Brush := StdRadialGradientBrush(colBlue, colWhite, 0.25, 0.25, 0.75);
    else raise Exception.Create('Brush not supported');
  end;
  case AIntParam and $00000F00 of
    $00000000: obj.WindingRule := vcmEvenOddRule;
    $00000100: obj.WindingRule := vcmNonZeroWindingRule;
  end;
  case AIntParam and $000F0000 of
    $00010000: Rotate(APage, obj, 30);
    $00020000: Rotate(APage, obj, -30);
  end;
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

procedure TMainForm.Render_2Lines(APage: TvVectorialPage; AIntParam: Integer);
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
  para: TvParagraph;
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

  // Draw "+" at the origin of the first line of the text
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

  // Create paragraph
  para := TvParagraph.Create(APage);
  para.X := XTEXT;
  if APage.UseTopLeftCoordinates then
    para.Y := PAGE_SIZE - YTEXT
  else
    para.Y := YTEXT;
  para.Font.Size := 16;
  para.TextAnchor := anchor;
  para.Font.Orientation := angle;

  // Add text to the paragraph
  txt := para.AddText('ABCDE');
  txt := para.AddText('Fghij');
  txt.Y := txt.Y + 1.5 * txt.Font.Size * APage.GetTopLeftCoords_Adjustment();

  APage.AddEntity(para);
end;

procedure TMainForm.Render_Text_Fonts(APage: TvVectorialPage;
  AIntParam: Integer);
var
  txt: TvText;
  yText: Integer;
begin
  txt := TvText.Create(APage);
  txt.X := 10;
  yText := 80;
  if APage.UseTopLeftCoordinates then
    txt.Y := PAGE_SIZE - yText
  else
    txt.Y := yText;
  txt.Font.Name := 'Times New Roman';
  txt.Font.Size := 10;
  txt.Value.Add('Times');
  APage.AddEntity(txt);

  txt := TvText.Create(APage);
  txt.X := 10;
  yText := 60;
  if APage.UseTopLeftCoordinates then
    txt.Y := PAGE_SIZE - yText
  else
    txt.Y := yText;
  txt.Font.Name := 'Courier New';
  txt.Font.Size := 12;
  txt.Value.Add('Courier');
  APage.AddEntity(txt);
end;

procedure TMainForm.Render_Text_Colors(APage: TvVectorialPage;
  AIntParam: Integer);
const
  YTEXT = 80;
var
  txt: TvText;
begin
  txt := TvText.Create(APage);
  txt.X := 10;
  if APage.UseTopLeftCoordinates then
    txt.Y := PAGE_SIZE - YTEXT
  else
    txt.Y := YTEXT;
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
  i: Integer;
  List: TStrings;
  node: TTreeNode;
  s: String;
  sa: TStringArray;
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

    List := TStringList.Create;
    try
      ini.ReadSection('Results', List);
      for i := 0 to List.Count-1 do begin
        s := ini.ReadString('Results', List[i], '');
        node := Tree.Items.FindNodeWithTextPath(List[i]);
        if (s = '') or (node = nil) or (node.Data = nil) then
          Continue;
        sa := s.Split(FORMAT_SEPARATOR);
        TRenderParams(node.Data).RenderState[0] := StrToRenderState(sa[0]);
        TRenderParams(node.Data).RenderState[1] := StrToRenderState(sa[1]);
      end;
    finally
      List.Free;
    end;

    s := ini.ReadString('MainForm', 'FileFormat', '');
    if s <> '' then
    begin
      i := CbFileFormat.Items.IndexOf(s);
      if i <> -1 then
        CbFileFormat.ItemIndex := i;
    end;

  finally
    ini.Free;
  end;
end;

procedure TMainForm.WriteIni;
var
  ini: TCustomIniFile;

  procedure WriteTestState(ANode: TTreeNode);
  var
    renderParams: TRenderParams;
    s: String;
  begin
    if ANode = nil then
      exit;
    if (ANode.Data <> nil) then
    begin
      renderParams := TRenderParams(ANode.Data);
      if (renderParams.RenderState[0] <> rsUnknown) or (renderParams.RenderState[1] <> rsUnknown) then
      begin
        s := 'svg:' + RenderStateToStr(renderParams.RenderState[0]) + FORMAT_SEPARATOR +
             'wmf:' + RenderStateToStr(renderParams.RenderState[1]);
        ini.WriteString('Results', ANode.GetTextPath, s);
      end;
    end;
    if ANode.HasChildren then
      WriteTestState(ANode.GetFirstChild);
    WriteTestState(ANode.GetNextSibling);
  end;

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

    ini.WriteString('MainForm', 'FileFormat', cbFileFormat.Items[cbFileFormat.ItemIndex]);

    ini.EraseSection('Results');
    WriteTestState(Tree.Items.GetFirstNode);

  finally
    ini.Free;
  end;
end;

procedure TMainForm.ShowFileImage(AFilename: String; AUseTopLeftCoords: Boolean;
  APaintbox: TPaintbox);
var
  ext: String;
  rc: TRenderCoords;
begin
  if AUseTopLeftCoords then
    rc := rcTopLeftCoords else
    rc := rcBottomLeftCoords;

  ext := Lowercase(ExtractFileExt(AFileName));

  if not FileExists(AFileName) then begin
    case ext of
      '.svg': FreeAndNil(FDocFromSVG[rc]);
      '.wmf': FreeAndNil(FDocFromWMF[rc]);
      else    raise Exception.Create('File type not supported');
    end;
    APaintbox.Hint := NOT_SAVED;
    APaintbox.Invalidate;
    exit;
  end;

  if ext = '.svg' then begin
    FreeAndNil(FDocFromSVG[rc]);
    FDocFromSVG[rc] := TvVectorialDocument.Create;
    FDocFromSVG[rc].ReadFromFile(AFileName);
  end else
  if ext = '.wmf' then begin
    FreeAndNil(FDocFromWMF[rc]);
    FDocFromWMF[rc] := TvVectorialDocument.Create;
    FDocFromWMF[rc].ReadFromFile(AFilename);
  end;
  APaintbox.Hint := AFileName;
  APaintBox.Invalidate;
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

  fn := IncludeTrailingPathDelimiter(REFIMG_FOLDER) + renderParams.RefFile;
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
  PrepareDoc(FDoc[rcBottomLeftCoords], page, false);
  renderParams.OnRender(page, renderParams.IntParam);
  BottomLeftPaintbox.Invalidate;

  // Render document with top/left origin
  PrepareDoc(FDoc[rcTopLeftCoords], page, true);
  renderParams.OnRender(page, renderParams.IntParam);
  TopLeftPaintbox.Invalidate;
end;

procedure TMainForm.ShowWriteReadTestImages;
var
  renderParams: TRenderParams;
  folder: String;
  fn: String;
  ext: String;
  rc: TRenderCoords;
begin
  for rc in TRenderCoords do begin
    FreeAndNil(FDocFromSVG[rc]);
    FreeAndNil(FDocFromWMF[rc]);
  end;

  if Tree.Selected = nil then
    exit;

  renderParams := TRenderParams(Tree.Selected.Data);
  if renderParams = nil then
  begin
    WRBottomLeftPaintbox.Invalidate;
    WRTopLeftPaintbox.Invalidate;
    exit;
  end;

  ext := GetFileFormatExt;
  folder := IMG_FOLDER + ext + PathDelim;

  fn := folder + 'bl_' + ChangeFileExt(renderParams.RefFile, '.' + ext);
  ShowFileImage(fn, false, WRBottomLeftPaintbox);

  fn := folder + 'tl_' + ChangeFileExt(renderParams.RefFile, '.' + ext);
  ShowFileImage(fn, true, WRTopLeftPaintbox);
end;

procedure TMainForm.TreeCustomDrawItem(Sender: TCustomTreeView; Node: TTreeNode;
  State: TCustomDrawState; var DefaultDraw: Boolean);
begin
  if Node.HasChildren then
    Sender.Canvas.Font.Style := [fsBold]
  else
    Sender.Canvas.Font.Style := [];
  DefaultDraw := true;
end;

procedure TMainForm.TreeGetImageIndex(Sender: TObject; Node: TTreeNode);
var
  renderParams: TRenderParams;
begin
  if Node.HasChildren then
    Node.ImageIndex := -1
  else begin
    renderParams := TRenderParams(Node.Data);
    if renderParams = nil then
      Node.ImageIndex := 0
    else
      Node.ImageIndex := ord(renderParams.RenderState[CbFileFormat.ItemIndex]);
  end;
end;

procedure TMainForm.TreeGetSelectedIndex(Sender: TObject; Node: TTreeNode);
begin
  Node.SelectedIndex := Node.ImageIndex;
end;

procedure TMainForm.TreeSelectionChanged(Sender: TObject);
begin
  ShowRenderTestImages;
  ShowRefImageTest;
  try
    ShowWriteReadTestImages;
  except
    on E:Exception do
      MessageDlg(E.Message, mtError, [mbOK], 0);
  end;
  UpdateTestResults;
  UpdateCmdStates;
end;

procedure TMainForm.UpdateCmdStates;
var
  fn: String;
  folder: string;
  renderParams: TRenderParams;
  ext: String;
  rc: TRenderCoords;
  rcOK: array[TRenderCoords] of boolean = (false, false);
begin
  BtnSaveAsRef.Enabled := Tree.Selected <> nil;
  BtnSaveToFiles.Enabled := Tree.Selected <> nil;
  BtnViewBottomLeft.Enabled := Tree.Selected <> nil;
  BtnViewTopLeft.Enabled := Tree.Selected <> nil;

  if Tree.Selected <> nil then begin
    renderParams := TRenderParams(Tree.Selected.Data);
    if renderParams <> nil then begin
      ext := GetFileFormatExt;
      folder := IMG_FOLDER + ext + PathDelim;
      fn := folder + 'bl_' + ChangeFileExt(renderParams.RefFile, '.' + ext);
      rcOK[rcBottomLeftCoords] := FileExists(fn);
      fn := folder + 'tl_' + ChangeFileExt(renderParams.RefFile, '.' + ext);
      rcOK[rcTopLeftCoords] := FileExists(fn);
    end;
  end;
  BtnViewBottomLeft.Enabled := rcOK[rcBottomLeftcoords];
  BtnViewTopLeft.Enabled := rcOK[rcTopLeftCoords];
end;

procedure TMainForm.UpdateResultStates;

  procedure UpdateImageIndex(ANode: TTreeNode);
  begin
    if ANode = nil then
      exit;
    TreeGetImageIndex(nil, ANode);
    ANode.SelectedIndex := ANode.ImageIndex;
    if ANode.HasChildren then
      UpdateImageIndex(ANode.GetFirstChild);
    UpdateImageIndex(ANode.GetNextSibling);
  end;

begin
  UpdateImageIndex(Tree.Items.GetFirstNode);
end;

procedure TMainForm.UpdateTestResults;
var
  renderParams: TRenderParams;
begin
  if not Assigned(Tree.Selected) or not Assigned(Tree.Selected.Data) then
    exit;
  inc(FLockResults);
  renderParams := TRenderParams(Tree.Selected.Data);
  case renderParams.RenderState[cbFileFormat.ItemIndex] of
    rsUnknown: rbUnknown.Checked := true;
    rsPassed: rbPassed.Checked := true;
    rsFailed: rbFailed.Checked := true;
  end;
  dec(FLockResults);
end;

end.

