{
    This file is part of the Free Component Library.
    Copyright (c) 2016 Michael Van Canneyt, member of the Free Pascal development team

    FPReport LCL Preview form.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit fpreportpreview;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  ComCtrls, ActnList, Buttons, StdCtrls, Menus, fpreport, fpreportlclexport,
  fpreportformexport;

type

  { TFPReportPreviewForm }
  // Fool the IDE.
  TForm = TCustomFPreportPreviewForm;
  TOpenURLEvent = Procedure (Sender : TObject; Const AURL : String) of Object;

  TFPReportPreviewForm = class(TForm)
    AClose: TAction;
    ALast: TAction;
    AFirst: TAction;
    APrint: TAction;
    AExportPDF: TAction;
    AZoomReset: TAction;
    AZoomOut: TAction;
    AZoomIn: TAction;
    ANext: TAction;
    APrevious: TAction;
    AExport: TAction;
    ALPreview: TActionList;
    EPage: TEdit;
    ILPreview: TImageList;
    LPageCount: TLabel;
    PBPreview: TPaintBox;
    PButtons: TPanel;
    PBottom: TPanel;
    PMExport: TPopupMenu;
    SBPrevious: TSpeedButton;
    SBPrevious1: TSpeedButton;
    ScrollBox1: TScrollBox;
    SpeedButton1: TSpeedButton;
    SpeedButton2: TSpeedButton;
    TBPreview: TToolBar;
    TBClose: TToolButton;
    TBExport: TToolButton;
    TBPDF: TToolButton;
    TBPrint: TToolButton;
    TBFirst: TToolButton;
    TBLast: TToolButton;
    ToolButton3: TToolButton;
    TBPrevious: TToolButton;
    TBNext: TToolButton;
    ToolButton6: TToolButton;
    TBZoomIn: TToolButton;
    TBZoomReset: TToolButton;
    TBZoomOut: TToolButton;
    procedure ACloseExecute(Sender: TObject);
    procedure AExportExecute(Sender: TObject);
    procedure AExportPDFExecute(Sender: TObject);
    procedure AExportPDFUpdate(Sender: TObject);
    procedure AFirstExecute(Sender: TObject);
    procedure AFirstUpdate(Sender: TObject);
    procedure ALastExecute(Sender: TObject);
    procedure ALastUpdate(Sender: TObject);
    procedure ANextExecute(Sender: TObject);
    procedure ANextUpdate(Sender: TObject);
    procedure APreviousExecute(Sender: TObject);
    procedure APreviousUpdate(Sender: TObject);
    procedure APrintExecute(Sender: TObject);
    procedure AZoomInExecute(Sender: TObject);
    procedure AZoomInUpdate(Sender: TObject);
    procedure AZoomOutExecute(Sender: TObject);
    procedure AZoomOutUpdate(Sender: TObject);
    procedure AZoomResetExecute(Sender: TObject);
    procedure EPageEditingDone(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure PBPreviewClick(Sender: TObject);
    procedure PBPreviewMouseLeave(Sender: TObject);
    procedure PBPreviewMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
  private
    FHorzOffset : Integer;
    FBitmap:TBitmap;
    FLastPage : Integer;
    FCurrentZoom : Integer;
    FOnOpenURL: TOpenURLEvent;
    FRender:TFPReportExportCanvas;
    procedure DoExport(Sender: TObject);
    procedure ExportReport(REC: TFPReportExporterClass);
    procedure FillExportMenu;
    function GetPageIndex: Integer;
    procedure RecreateBitmap;
    procedure ResizePreview;
    procedure SetCurrentZoom(AValue: Integer);
    procedure SetPageIndex(AValue: Integer);
  Protected
    function GetEnableHyperLinks: Boolean ; override;
    procedure SetEnableHyperLinks(AValue: Boolean); override;
    procedure SetReport(AValue: TFPCustomReport); override;
    procedure ShowHyperLink(const AURL: String); virtual;
      { private declarations }
  public
    procedure DoPaintReport(Sender: TObject);
    Class Function LoadFromResource : Boolean; override;
    { public declarations }
    Property CurrentZoom : Integer Read FCurrentZoom Write SetCurrentZoom;
    // Zero based  !
    Property PageIndex : Integer Read GetPageIndex Write SetPageIndex;
    // If not set, the OpenURL method of LCL will be called.
    Property OnOpenURL : TOpenURLEvent Read FOnOpenURL Write FOnOpenURL;
  end;

var
  FPReportPreviewForm: TFPReportPreviewForm;

implementation

{$R *.lfm}

uses dlginputcombo,lclintf;

Const
  MaxZoomIndex = 4;
  Zooms : Array[-4..4] of Double = (1/4, 1/2, 1/1.5, 1/1.25, 1, 1.25, 1.5, 2, 4);
  PDFExport = 'PDF';
  PrintExport = 'Print';

Resourcestring
  SPageCount = 'of %d';
  SExport = 'Export Report';
  SSelectExportFormat = 'Select export format';
  SErrNoHyperLinkSupport = 'Hyperlink support not enabled.';

Type
  TExportMenuItem = Class(TMenuItem)
  Public
    TheClass : TFPReportExporterClass;
  end;

{ TFPReportPreviewForm }

procedure TFPReportPreviewForm.ACloseExecute(Sender: TObject);
begin
  Close;
end;

procedure TFPReportPreviewForm.AExportExecute(Sender: TObject);

Var
  I : integer;
  L : TStringList;

begin
  // Do Export
  L:=TstringList.Create;
  try
    For I:=0 to PMExport.Items.Count-1 do
      L.AddObject(PMExport.Items[I].Caption,PMExport.Items[I]);
    if L.Count=0 then
        exit;
    L.Sort;
    I:=InputCombo(SExport,SSelectExportFormat,L);
    If I<>-1 then
      ExportReport((L.Objects[i] as TExportMenuItem).TheClass);
  finally
    L.Free;
  end;
end;

procedure TFPReportPreviewForm.AExportPDFExecute(Sender: TObject);
begin
  ExportReport(ReportExportManager.FindExporter(PDFExport));
end;

procedure TFPReportPreviewForm.AExportPDFUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled:=ReportExportManager.FindExporter(PDFExport)<>Nil;
end;

procedure TFPReportPreviewForm.AFirstExecute(Sender: TObject);
begin
  PageIndex:=0;
end;

procedure TFPReportPreviewForm.AFirstUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled:=PageIndex>0;
end;

procedure TFPReportPreviewForm.ALastExecute(Sender: TObject);
begin
  PageIndex:=ReportPages.Count-1;
end;

procedure TFPReportPreviewForm.ALastUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled:=PageIndex<ReportPages.Count-1;
end;

procedure TFPReportPreviewForm.ANextExecute(Sender: TObject);
begin
  PageIndex:=PageIndex+1;
end;

procedure TFPReportPreviewForm.ANextUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled:=PageIndex<ReportPages.Count-1;
end;

procedure TFPReportPreviewForm.APreviousExecute(Sender: TObject);
begin
  PageIndex:=PageIndex-1;
end;

procedure TFPReportPreviewForm.APreviousUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled:=PageIndex>0;
end;

procedure TFPReportPreviewForm.APrintExecute(Sender: TObject);
begin
  ExportReport(ReportExportManager.FindExporter(PrintExport));
end;

procedure TFPReportPreviewForm.AZoomInExecute(Sender: TObject);
begin
  if FCurrentZoom<MaxZoomIndex then
    CurrentZoom:=CurrentZoom+1;
end;

procedure TFPReportPreviewForm.AZoomInUpdate(Sender: TObject);
begin
  (Sender as Taction).Enabled:=FCurrentZoom<MaxZoomIndex;
end;

procedure TFPReportPreviewForm.AZoomOutExecute(Sender: TObject);
begin
  if FCurrentZoom>-MaxZoomIndex then
    CurrentZoom:=CurrentZoom-1;
end;

procedure TFPReportPreviewForm.AZoomOutUpdate(Sender: TObject);
begin
  (Sender as Taction).Enabled:=FCurrentZoom>-MaxZoomIndex;
end;

procedure TFPReportPreviewForm.AZoomResetExecute(Sender: TObject);
begin
  CurrentZoom:=0;
end;

procedure TFPReportPreviewForm.EPageEditingDone(Sender: TObject);
Var
  PN : Integer;
begin
  PN:=StrToIntDef(EPage.Text,-1);
  if (PN<1) or (PN>FRender.PageCount) then exit;
  FRender.PageIndex:=PN-1;
  PBPreview.Invalidate;
end;


procedure TFPReportPreviewForm.SetCurrentZoom(AValue: Integer);
begin
  if FCurrentZoom=AValue then Exit;
  FCurrentZoom:=AValue;
  FRender.HDPI:=PixelsPerInch;
  FRender.VDPI:=PixelsPerInch;
  FRender.Zoom:=Zooms[FCurrentZoom];
  ResizePreview;
  FLastPage:=-1; // Force recreate
  PBPreview.Invalidate;
end;

function TFPReportPreviewForm.GetPageIndex: Integer;
begin
  Result:=FRender.PageIndex;
end;

procedure TFPReportPreviewForm.SetPageIndex(AValue: Integer);
begin
  FRender.PageIndex:=AValue;
  EPage.Text:=IntToStr(AValue+1);
  PBPreview.Invalidate;
end;

function TFPReportPreviewForm.GetEnableHyperLinks: Boolean;
begin
  Result:=FRender.HyperLinksEnabled;
end;

procedure TFPReportPreviewForm.SetEnableHyperLinks(AValue: Boolean);
begin
  FRender.HyperLinksEnabled:=AValue;
end;

procedure TFPReportPreviewForm.SetReport(AValue: TFPCustomReport);
begin
  inherited SetReport(AValue);
  FRender.Report:=AValue;
  If Assigned(AValue) then
    begin
    FRender.Execute;
    LPageCount.Caption:=Format(SPageCount,[FRender.PageCount]);
    EPage.Text:='1';
    PageIndex:=0;
    end;
end;

procedure TFPReportPreviewForm.ResizePreview;

Var
  W,H : Integer;

begin
  FRender.GetCurrentPageRenderSize(W,H);
  PBPreview.Width:=W+FHorzOffset*2;
  PBPreview.Height:=H;
end;

procedure TFPReportPreviewForm.RecreateBitmap;

Var
  W,H : Integer;

begin
  FRender.GetCurrentPageRenderSize(W,H);
  FBitmap.SetSize(W,H);
  FRender.RenderCurrentPage;
  FLastPage:=FRender.PageIndex;
end;

procedure TFPReportPreviewForm.DoPaintReport(Sender: TObject);


begin
  if FLastPage<>FRender.PageIndex then
    RecreateBitmap;
  ResizePreview;
  PBPreview.Canvas.Draw(FHorzOffset*2,0,FBitmap);
end;

class function TFPReportPreviewForm.LoadFromResource: Boolean;
begin
  Result:=True;
end;

procedure TFPReportPreviewForm.FormCreate(Sender: TObject);

begin
  FHorzOffset:=100;
  FRender:=TFPReportExportCanvas.Create(Self);
  FRender.HyperLinksEnabled:=True;
  FBitmap:=TBitmap.Create;
  FLastPage:=-1;
  FRender.Zoom:=1;
  FRender.Canvas:=FBitmap.Canvas;
  PBPreview.OnPaint:=@DoPaintReport;
  AExportPDF.Enabled:=ReportExportManager.IndexOfExporter(PDFExport)<>-1;
  APrint.Enabled:=ReportExportManager.IndexOfExporter(PrintExport)<>-1;
  CurrentZoom:=0;
  FillExportMenu;
end;

procedure TFPReportPreviewForm.FormDestroy(Sender: TObject);
begin
  FBitmap.Free;
end;

procedure TFPReportPreviewForm.ShowHyperLink(const AURL: String);

begin
  if Assigned(FOnOpenURL) then
    FOnOpenURL(Self,AURL)
  else
    OpenURL(AURL);
end;

procedure TFPReportPreviewForm.PBPreviewClick(Sender: TObject);

Var
  P : TPoint;
  H : THyperLinkItem;

begin
  P:=PBPreview.ScreenToClient(Mouse.CursorPos);
  if Not Assigned(FRender.HyperLinks) then
    Raise EReportExportError.Create(SErrNoHyperlinkSupport);
  H:=FRender.HyperLinks.FindLinkAtPoint(P);
  if Assigned(H) then
    ShowHyperLink(H.URL);
end;

procedure TFPReportPreviewForm.PBPreviewMouseLeave(Sender: TObject);
begin
  if Screen.Cursor=crHandPoint then
    Screen.Cursor:=crDefault;
end;

procedure TFPReportPreviewForm.PBPreviewMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);

Var
  H : THyperLinkItem;

begin
  if Assigned(FRender.HyperLinks) then
    begin
    H:=FRender.HyperLinks.FindLinkAtPoint(Point(X,Y));
    If Assigned(h) then
      Screen.Cursor:=crHandPoint
    else
      Screen.Cursor:=crDefault;
    end;
end;


procedure TFPReportPreviewForm.ExportReport(REC : TFPReportExporterClass);

Var
  E : TFPReportExporter;

begin
  E:=Rec.Create(Self);
  try
    E.Report:=Self.Report;
    if E.ShowConfig then
      E.Execute;
  finally
    E.Free;
  end;
end;

procedure TFPReportPreviewForm.DoExport(Sender : TObject);

Var
  REC : TFPReportExporterClass;

begin
  REC:=(Sender As TExportMenuItem).TheClass;
  ExportReport(REC);
end;

procedure TFPReportPreviewForm.FillExportMenu;

Var
  REC : TFPReportExporterClass;
  I: Integer;
  MI : TExportMenuItem;

begin
  PMExport.Items.Clear;
  For I:=0 to ReportExportManager.ExporterCount-1 do
    begin
    REC:=ReportExportManager.Exporter[i];
    if REc<>TFPreportPreviewExport then
      begin
      MI:=TExportMenuItem.Create(Self);
      MI.TheClass:=Rec;
      MI.Caption:=Rec.Description;
      MI.Hint:=Rec.Description;
      MI.OnClick:=@DoExport;
      PMExport.Items.Add(MI);
      end;
    end;
end;

initialization
  TFPreportPreviewExport.DefaultPreviewFormClass:=TFPReportPreviewForm;
end.

