unit rptfrma;

interface

uses
  Types,
  SysUtils, Classes,
  Controls, Menus, StdCtrls,
  ComCtrls, ExtCtrls, Forms,Graphics,
  Dialogs,
  Buttons,
  Printers,
  PrintersDlgs;

type

  TDrawPanel=class(TPanel)
  public
    constructor Create(AOwner: TComponent); override;
    procedure Paint; override;
    property Canvas;
  end;

  TViewMode = (vmCustom=-1,vm200,vm150,vm100,vm75,vm50,vm25,vm10,vmPageWidth,vmFullPage);

  { TRptFormA }

  TRptFormA = class(TForm)
    sbxMain: TScrollBox;
    tbrMain: TToolBar;
    imlMain: TImageList;
    tbtPrint: TToolButton;
    tbtPrintDialog: TToolButton;
    pnlShadow: TPanel;
    tbtPrinterSetupDialog: TToolButton;
    tbtPrevPage: TToolButton;
    tbtNextPage: TToolButton;
    stbMain: TStatusBar;
    ilSmall: TImageList;
    cbxScale: TComboBox;
    ToolButton1: TToolButton;
    btnRefresh: TButton;
    PrinterSetupDialog: TPrinterSetupDialog;
    PrintDialog: TPrintDialog;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    ckbGridPrint: TCheckBox;
    procedure btnRefreshClick(Sender: TObject);
    procedure ckbGridPrintChange(Sender: TObject);
    procedure mniScaleClick(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure sbxMainResize(Sender: TObject);
    procedure tbtPrintDialogClick(Sender: TObject);
    procedure tbtPrintClick(Sender: TObject);
    procedure tbtPrinterSetupDialogClick(Sender: TObject);
    procedure tbtPrevPageClick(Sender: TObject);
    procedure tbtNextPageClick(Sender: TObject);
    procedure sbxMainMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure cbxScaleKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure ckbGridPrintClick(Sender: TObject);
  private
    FDrawPanel: TDrawPanel;
    FViewMode: TViewMode;
    FPageCount: Integer;
    fCurrPage:integer;
    fnLastPage:integer;

    fSpecialMode:boolean;
    fImages:TList;
    function GetImage(ndx:integer):TGraphic;
    procedure SetViewMode(Value: TViewMode);
    procedure SetPageCount(Value: Integer);
    procedure SetCurrPage(Value: Integer);
    procedure DoPrint;
    procedure GenerateExamplePage;

    procedure DisplayPage(ACanvas:TCanvas;ARect:TRect;Page : Integer);
    procedure PrintMetafile(const ARect:types.TRect;AMetaFile:TGraphic);
    procedure PrintRange(const nFrom,nTo : Integer);
    procedure UpdatePageSetup;
    procedure UpdatePreview;
    property PageCount: Integer read FPageCount write SetPageCount;
    property Images[index:integer]:TGraphic read GetImage;
  public

    { Public declarations }
    constructor Create(AOwner: TComponent); override;

    property ViewMode: TViewMode read FViewMode write SetViewMode;
    property CurrPage:integer read fCurrPage write SetCurrpage;
  end;

var
  RptFormA:TRptFormA;

implementation

{$R *.lfm}

uses
  lresources, lmf;


const
  reportDPI = 1200;

function mm(val:double):longint;
begin
  Result:=round(val/25.4*reportDPI)
end;

constructor TDrawPanel.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Color:=clWhite;
  Visible:=False; // this is XP fix
  {$IFDEF VCL}
  BorderStyle:=bsNone;
  BevelInner:=bvNone;
  BevelOuter:=bvNone;
  {$ENDIF}
  TabStop:=true;
  Self.OnMouseWheel:=TForm(Owner).OnMouseWheel;
end;

procedure TDrawPanel.Paint;
var
  frm:TRptFormA;
begin
  if not Assigned(owner) then exit;

  frm:=TRptFormA(Owner);
  frm.DisplayPage(Canvas,Self.BoundsRect,frm.CurrPage-1);
end;

constructor TRptFormA.Create(AOwner: TComponent);
var
  img:TGraphic;
begin
  inherited Create(AOwner);
  FViewMode:=vmFullPage;
  FPageCount:=1;
  FCurrPage:=1;
  fImages:=TList.Create;
  // fQR:=TQReport.Create(nil);
  // load the image

  GenerateExamplePage;

  img:=TlmfImage.Create;
  img.LoadFromFile('page.lmf'); // image knows its size

  fImages.Add(img);

  img:=TlmfImage.Create;
  img.LoadFromFile('page0.lmf');
  fImages.Add(img);

  PageCount:=3;
  CurrPage:=1;


  FDrawPanel:=TDrawPanel.Create(Self);
  with FDrawPanel do
  begin
    Parent:=sbxMain;
    BorderStyle:=bsNone;
    Left:=8;
    Top:=8;
  end;

end;

procedure TRptFormA.DisplayPage(ACanvas:TCanvas;ARect:TRect;Page : Integer);
var
 // p : TPoint;
 // Sc : Single;
  r : TRect;
  i,j,k,AWidth,AHeight,w4,h4 : integer;
  mf1:TGraphic;
begin
  if (Page >=0) and (Page < PageCount) then
  begin
    AWidth:=ARect.Right-ARect.Left;
    AHeight:=ARect.Bottom-ARect.Top;
  //  Sc := AWidth / PageSize.X;
    ACanvas.Pen.Width:=1;
    ACanvas.Pen.Color:=clBlack;
    ACanvas.Rectangle(0, 0, AWidth,AHeight);

    ACanvas.FillRect(Types.Rect(1, 1, AWidth - 2,AHeight - 2));

    r.left:=0;
    r.top:=0;

    r.Right := r.Left + AWidth;
    r.Bottom := r.Top + AHeight;

    if fSpecialMode then
    begin
      w4:=AWidth div 2;
      h4:=AHeight div 2;
      k:=Page;
      for i:=0 to 1 do
      for j:=0 to 1 do
      begin
        r.Left:=w4*i; r.Right:=w4*(i+1);
        r.Top:=h4*j; r.Bottom:=h4*(j+1);
        mf1:=Images[k];
        if Assigned(mf1) then
        ACanvas.StretchDraw(r,mf1);
        inc(k);
      end;
    end else
      ACanvas.StretchDraw(r,Images[Page]);
  end;
end;

procedure TRptFormA.GenerateExamplePage;
var
  img:TlmfImage;
  cnv:TCanvas;
  bmp:TBitmap;
begin
  img:=TlmfImage.Create;
  fImages.Add(img);
  // A4 with 1200 dpi resolution (see "reportDPI" at the top)
  img.Width:=mm(210);
  img.Height:=mm(297);

  cnv:=TlmfCanvas.Create(img);
  try
    cnv.Font.Name:='Sans';
    cnv.Font.Height:=round(32/72*ReportDPI);
    cnv.TextOut(mm(30),mm(30), 'Hello, Vector world!');
    cnv.font.Color:=clNavy;
    cnv.Font.Height:=round(20/72*ReportDPI);
    cnv.TextOut(mm(35),mm(40),'See other pages using blue arrows in the toolbar');

    // 10 mm from page border with 1 mm thick line
    cnv.Pen.Width:=mm(1);
    cnv.Rectangle(mm(10),mm(10),mm(210-10),mm(297-10));

    // polygon
    cnv.Pen.Width:=0;
    cnv.Brush.Color:=$e0c000;
    cnv.Brush.Style:=bsSolid;
    cnv.Polygon([
      Point(mm(15),mm(50)),
      Point(mm(50),mm(50)),
      Point(mm(60),mm(70)),
      Point(mm(50),mm(90))
      ]);

    // ellipse
    cnv.Pen.Width:=mm(1);
    cnv.Brush.Color:=$f00080;
    cnv.Brush.Style:=bsSolid;
    cnv.Ellipse(mm(25), mm(185), mm(75), mm(235));

    // ellipse
    cnv.Pen.Width:=0;
    cnv.Pen.Color:=$00c000;
    cnv.Brush.Color:=$00e0f0;
    cnv.Brush.Style:=bsSolid;
    cnv.Ellipse(mm(95), mm(195), mm(115), mm(225));

    bmp:=TBitmap.Create;
    imlMain.GetBitmap(0,bmp);
    cnv.StretchDraw(
      Rect(mm(110),mm(50),mm(150),mm(90)),
      bmp);

    cnv.Brush.Style:=bsClear;
    cnv.Font.Color:= $00c000;
    cnv.Font.Style:=[fsBold];
    cnv.Font.Quality:=fqAntialiased;

    cnv.TextOut(mm(35),mm(95),'LMF - Lazarus metafile. True X-Platform');
    cnv.Font.Color:= clNavy;
    cnv.Font.Style:=[];

    cnv.TextOut(mm(35),mm(120),
      'The same image scaled and drawn below');
    cnv.TextOut(mm(35),mm(125),
      'Except the part containing small image');
    cnv.TextOut(mm(35),mm(130),
       'Guess why :)?');

    img.Draw(cnv,Rect(mm(20),mm(140),mm(50),mm(180)));
    { cnv.StretchDraw(Rect(mm(20),mm(140),mm(50),mm(180)), img);}

    img.Draw(cnv,Rect(mm(110),mm(140),mm(194),mm(256)));

  {  cnv.StretchDraw(Rect(mm(120),mm(140),mm(204),mm(256)), img);}

  finally
    cnv.Free;
    bmp.free;
  end;

end;

procedure TRptFormA.UpdatePageSetup;
var
  Scaling,r: Integer;
  PageWidth,PageHeight:integer;
begin
  pnlShadow.Visible:=False;
  if not Assigned(fDrawpanel) then exit;

  PageWidth:=Printer.PageWidth;
  PageHeight:=Printer.PageHeight;

  FDrawPanel.Visible:=False;
  case FViewMode of
    vmCustom:
      begin
        Scaling:=100;
        Val(cbxScale.Text,Scaling,r);
      end;
    vm200: Scaling:=200;
    vm150: Scaling:=150;
    vm100: Scaling:=100;
    vm75: Scaling:=75;
    vm50: Scaling:=50;
    vm25: Scaling:=25;
    vm10: Scaling:=10;
    vmPageWidth: // по ширине страницы
    begin
      with sbxMain do
      begin
        VertScrollBar.Position:=0;
        HorzScrollBar.Position:=0;
      end;
      Scaling:=1;
      FDrawPanel.Left:=8;
      FDrawPanel.Top:=8;
      FDrawPanel.Width:=sbxMain.ClientWidth-16-17;//GetSystemMetrics(sm_CXVScroll);
      FDrawPanel.Height:=FDrawPanel.Width*PageHeight div PageWidth;
      {GetDeviceCaps(Printer.Handle,VertSize) div
        GetDeviceCaps(Printer.Handle,HorzSize)};
      with sbxMain do
      begin
        VertScrollBar.Range:=FDrawPanel.Height+16;
        HorzScrollBar.Range:=0;
      end;
    end;
    vmFullPage: // страница целиком
    begin
      Scaling:=1;
      with sbxMain do
      begin
        VertScrollBar.Range:=0;
        HorzScrollBar.Range:=0;
        VertScrollBar.Position:=0;
        HorzScrollBar.Position:=0;
      end;
      FDrawPanel.Height:=sbxMain.ClientHeight-16;
      FDrawPanel.Width:=FDrawPanel.Height*PageWidth div PageHeight;
      if FDrawPanel.Width>sbxMain.ClientWidth-16 then
      begin
        FDrawPanel.Width:=sbxMain.ClientWidth-16;
        FDrawPanel.Height:=FDrawPanel.Width*PageHeight div PageWidth;
      end;
      FDrawPanel.Left:=(sbxMain.ClientWidth-FDrawPanel.Width) div 2;
      FDrawPanel.Top:=(sbxMain.ClientHeight-FDrawPanel.Height) div 2;
    end;
  else Scaling:=1;
  end;
  case FViewMode of
    vmCustom,vm200..vm10:
    begin
      with sbxMain do
      begin
        VertScrollBar.Position:=0;
        HorzScrollBar.Position:=0;
      end;
      FDrawPanel.Left:=8;
      FDrawPanel.Top:=8;
      FDrawPanel.Width:=Scaling*PageWidth*Screen.PixelsPerInch div Printer.XDPI div 100;
      FDrawPanel.Height:=Scaling*PageHeight*Screen.PixelsPerInch div Printer.YDPI div 100;

      with sbxMain do
      begin
        VertScrollBar.Range:=FDrawPanel.Height+16;
        HorzScrollBar.Range:=FDrawPanel.Width+16;
      end;
    end;
  end;

  {$ifndef LINUX}
  FDrawPanel.Visible:=True;
  {$endif}

  with pnlShadow do
  begin
    Left:=FDrawPanel.Left+4;
    Top:=FDrawPanel.Top+4;
    Width:=FDrawPanel.Width;
    Height:=FDrawPanel.Height;
    {$ifdef VCL}
    ParentBackground:=false;
    {$endif}
    ParentColor:=false;
    Color:=$4b4b4b;
    Visible:=True;
  end;
  {$ifdef LINUX}
  FDrawPanel.Visible:=True;
  {$endif}
end;

procedure TRptFormA.UpdatePreview;
begin
  with FDrawPanel do
  begin
    Hide;
    Show;
  end;
end;

procedure TRptFormA.SetViewMode(Value: TViewMode);
begin
  if Value<>FViewMode then
  begin
    FViewMode:=Value;
    UpdatePageSetup;
  end;
end;

const
  sPageFrom='Page %d from %d';

procedure TRptFormA.SetPageCount(Value: Integer);
begin
  if Value<1 then Value:=1;
  if Value<>PageCount then
    FPageCount:=Value;
  if fCurrPage>PageCount then
    fCurrPage:=PageCount;
  if (PageCount>1) and not stbMain.Visible then Height:=Height+stbMain.Height
  else
    if (PageCount=1) and stbMain.Visible then Height:=Height-stbMain.Height;
  tbtPrevPage.Visible:=PageCount>1;
  tbtNextPage.Visible:=PageCount>1;
  stbMain.Visible:=PageCount>1;
  tbtPrevPage.Enabled:=CurrPage>1;
  tbtNextPage.Enabled:=CurrPage<PageCount;
  stbMain.SimpleText:=Format(sPageFrom,[CurrPage,PageCount]);
  UpdatePageSetup;
end;

procedure TRptFormA.SetCurrPage(Value: Integer);
begin
  if Value<1 then Value:=1;
  if Value>PageCount then Value:=PageCount;
  if Value<>FCurrPage then
  begin
    FCurrPage:=Value;
    tbtPrevPage.Enabled:=CurrPage>1;
    tbtNextPage.Enabled:=CurrPage<PageCount;
    stbMain.SimpleText:=Format(sPageFrom,[CurrPage,PageCount]);
    with sbxMain do
    begin
      VertScrollBar.Position:=0;
      HorzScrollBar.Position:=0;
    end;
    UpdatePreview;
  end;
end;

procedure TRptFormA.FormResize(Sender: TObject);
begin
  UpdatePageSetup;
end;

procedure TRptFormA.sbxMainResize(Sender: TObject);
begin
  UpdatePageSetup;
end;

procedure TRptFormA.mniScaleClick(Sender: TObject);
var ndx:integer;
begin
  ndx:=-1;
  begin
    if Sender is TmenuItem then
    begin
      TmenuItem(Sender).Checked:=True;
      ndx:=TComponent(Sender).Tag;
      cbxScale.ItemIndex:=ndx;
    end
    else
    if Sender is TComboBox then
    begin
      ndx:=TComboBox(Sender).ItemIndex;
    end;

   // if ndx<0 then exit;

    FViewMode:=TViewMode(ndx);

    if fViewMode=vmCustom then exit;

    UpdatePageSetup;
  end;
end;

procedure TRptFormA.ckbGridPrintChange(Sender: TObject);
begin
  fSpecialMode:=ckbGridPrint.Checked;
end;

procedure TRptFormA.btnRefreshClick(Sender: TObject);
begin
  UpdatePageSetup;
  sbxMain.Refresh;
end;

procedure TRptFormA.tbtPrintDialogClick(Sender: TObject);
begin
  fnLastPage:=CurrPage;
  PrintDialog.MinPage:=1;
  PrintDialog.MaxPage:=fImages.Count;
  PrintDialog.FromPage:=fnLastPage;
  PrintDialog.ToPage:=fImages.Count;
  if PrintDialog.Execute then
  begin
    UpdatePageSetup;

    DoPrint;

  end;
end;

procedure TRptFormA.tbtPrinterSetupDialogClick(Sender: TObject);
begin
   if Printer.Printing then
   begin
     if not Printer.Aborted then
       Printer.Abort; // abort the prev job
     //Printer.EndDoc; // finish the doc
   end;

  {$IFDEF CLX}
  if Printer.ExecuteSetup then
  {$ELSE}
  if PrinterSetupDialog.Execute then
  {$ENDIF}
  begin
    UpdatePageSetup;
  end;

end;

procedure TRptFormA.tbtPrintClick(Sender: TObject);
begin
  if Printer.Printing then
  begin
    if not Printer.Aborted then
      Printer.Abort; // abort the prev job
   //Printer.EndDoc; // finish the doc
   end;
   DoPrint;
end;

procedure TRptFormA.tbtPrevPageClick(Sender: TObject);
begin
  CurrPage:=Pred(CurrPage);
end;

procedure TRptFormA.tbtNextPageClick(Sender: TObject);
begin
  CurrPage:=Succ(CurrPage);
end;

procedure TRptFormA.sbxMainMouseWheel(Sender: TObject;
  Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint;
  var Handled: Boolean);
begin
  sbxMain.VertScrollBar.Position:=sbxMain.VertScrollBar.Position-WheelDelta;
  handled:=true;
end;

procedure TRptFormA.DoPrint;
begin
  PrintRange(1,fImages.Count);
end;


procedure TRptFormA.PrintRange(const nFrom,nTo : Integer);
var
  i,j,k,w4,h4,AWidth,Aheight:integer;
  doNewPage:boolean;
  APage,r:types.TRect;
  mf1:TGraphic;
begin
  if (nFrom>0) and (nFrom<=PageCount) and
     (nTo>0) and (nTo<=PageCount)  then
  begin
    Printer.Title := 'LMF Report';
    Printer.BeginDoc;
    doNewPage:=false;
    try
      APage:=Types.Rect(0,0,Printer.PageWidth, Printer.PageHeight);

      AWidth:=Printer.PageWidth;
      AHeight:=Printer.PageHeight;


      w4:=AWidth div 2;
      h4:=AHeight div 2;

      if fSpecialMode then
      begin
        k:=nFrom;
        while k<=nTo do // across pages
        begin
          if doNewPage then Printer.NewPage;
          for i:=0 to 1 do // accross rows
          for j:=0 to 1 do // across columns
          begin
            r.Left:=w4*i; r.Right:=w4*(i+1);
            r.Top:=h4*j; r.Bottom:=h4*(j+1);
            mf1:=Images[k-1];
            if Assigned(mf1) then
              PrintMetafile(r,mf1);
            inc(k);
          end;
          donewPage:=true;
        end;
      end
      else
      for i:=nFrom to nTo do
      begin
        if doNewPage then Printer.NewPage;
        Self.PrintMetafile(APage,Images[i-1]);
        // page painted, on next pass do the NePage
        doNewPage:=true;
      end;
    finally
    	Printer.EndDoc;
    end;
  end;
end;

procedure TRptFormA.PrintMetafile(const ARect:types.TRect;AmetaFile:TGraphic);
begin
  {$ifdef LCL}
    TlmfImage(AMetafile).Draw(Printer.Canvas,ARect);
  {$else}
    Printer.Canvas.StretchDraw(ARect, AMetafile);
  {$endif}
end;


procedure TRptFormA.cbxScaleKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if key=13 then
  begin
    UpdatePageSetup;
    key:=0;
  end;
end;



procedure TRptFormA.FormDestroy(Sender: TObject);
var i:integer;
begin
  for i:=0 to fImages.Count-1 do
    TObject(Images[i]).Free;
  fImages.Free;
end;

procedure TRptFormA.FormShow(Sender: TObject);
begin
  UpdatePageSetup;
end;

procedure TRptFormA.ckbGridPrintClick(Sender: TObject);
begin
  fSpecialMode:=ckbGridPrint.Checked;
end;

function TRptFormA.GetImage(ndx:integer):TGraphic;
begin
  if (ndx>=0) and (ndx<fImages.Count) then
    Result:=TGraphic(fImages[ndx])
  else
    Result:=nil;
end;

end.
