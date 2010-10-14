{******************************************************************}
{*     IPHTMLPV.PAS - HTML Browser Print Preview                  *}
{******************************************************************}

(* ***** BEGIN LICENSE BLOCK *****
 * Version: MPL 1.1
 *
 * The contents of this file are subject to the Mozilla Public License Version
 * 1.1 (the "License"); you may not use this file except in compliance with
 * the License. You may obtain a copy of the License at
 * http://www.mozilla.org/MPL/
 *
 * Software distributed under the License is distributed on an "AS IS" basis,
 * WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License
 * for the specific language governing rights and limitations under the
 * License.
 *
 * The Original Code is TurboPower Internet Professional
 *
 * The Initial Developer of the Original Code is
 * TurboPower Software
 *
 * Portions created by the Initial Developer are Copyright (C) 2000-2002
 * the Initial Developer. All Rights Reserved.
 *
 * Contributor(s):
 *
 * ***** END LICENSE BLOCK ***** *)

{ Global defines potentially affecting this unit }
{$I IPDEFINE.INC}

unit IpHtmlPv;

interface

uses
  {$IFDEF IP_LAZARUS}
  LCLType,
  GraphType,
  LCLIntf,
  Buttons,
  {$ELSE}
  Windows,
  {$ENDIF}
  Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, IpHtml;

resourcestring
  rsIpHTMLPreviewPrintPreview = 'Print preview';
  rsIpHTMLPreviewPrint = 'Print';
  rsIpHTMLPreviewZoom = 'Zoom:';
  rsIpHTMLPreviewClose = 'Close';
  rsIpHTMLPreviewPage = 'Page:';
  rsIpHTMLPreviewOf = 'of';
  rsIpHTMLPreviewSelectPrinter = 'Select printer...';

type

  { TIpHTMLPreview }

  TIpHTMLPreview = class(TForm)
    btnSelectPrinter: TButton;
    Label3: TLabel;
    ZoomCombo: TComboBox;
    PaperPanel: TPanel;
    PaintBox1: TPaintBox;
    procedure btnNextClick(Sender: TObject);
    procedure btnLastClick(Sender: TObject);
    procedure btnSelectPrinterClick(Sender: TObject);
    procedure edtPageChange(Sender: TObject);
    procedure btnPrintClick(Sender: TObject);
    procedure PaintBox1Paint(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ZoomComboChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormResize(Sender: TObject);
  published
    Panel1: TPanel;
    btnPrint: TButton;
    btnFirst: TButton;
    btnPrev: TButton;
    btnNext: TButton;
    btnLast: TButton;
    btnClose: TButton;
    edtPage: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    lblMaxPage: TLabel;
    ScrollBox1: TScrollBox;
    procedure FormShow(Sender: TObject);
    procedure btnFirstClick(Sender: TObject);
    procedure btnPrevClick(Sender: TObject);
    procedure SetCurPage(const Value: Integer);
  protected
    FClipRect, SourceRect: TRect;
    Scratch: TBitmap;
    FScale: double;
    FZoom: Integer;
    procedure SetZoom(const Value: Integer);
    procedure ResizeCanvas;
    procedure ScaleSourceRect;
    procedure AlignPaintBox;
    procedure Render;
  public
    FCurPage: Integer;
    HTML : TIpHtml;
    PageRect: TRect;
    OwnerPanel: TIpHtmlInternalPanel;
    property ClipRect: TRect read FClipRect write FClipRect;
    property CurPage: Integer read FCurPage write SetCurPage;
    procedure RenderPage(PageNo: Integer);
    property Zoom: Integer read FZoom write SetZoom;
    property Scale: double read FScale;
  end;

implementation

uses
  Printers;

{$IFNDEF IP_LAZARUS}
{$R *.DFM}
{$ELSE}
{$R *.lfm}
{$ENDIF}

const
  SCRATCH_WIDTH = 800; //640;
  SCRATCH_HEIGHT = 600; //480;

procedure TIpHTMLPreview.FormShow(Sender: TObject);
begin
  Zoom := 100;
  RenderPage(CurPage);
end;

procedure TIpHTMLPreview.RenderPage(PageNo: Integer);
var
  CR : TRect;
begin
  CR := Rect(0, 0, OwnerPanel.PrintWidth, 0);
  CR.Top := (PageNo - 1) * OwnerPanel.PrintHeight;
  CR.Bottom := Cr.Top + OwnerPanel.PrintHeight;
  PageRect := CR;
  PaintBox1.Invalidate;
end;

procedure TIpHTMLPreview.btnFirstClick(Sender: TObject);
begin
  CurPage := 1;
end;

procedure TIpHTMLPreview.btnPrevClick(Sender: TObject);
begin
  CurPage := CurPage - 1;
end;

procedure TIpHTMLPreview.SetCurPage(const Value: Integer);
begin
  if (Value <> FCurPage)
  and (Value >= 1)
  and (Value <= OwnerPanel.PageCount) then begin
    FCurPage := Value;
    RenderPage(Value);
    edtPage.Text := IntToStr(CurPage);
  end;
end;

procedure TIpHTMLPreview.btnNextClick(Sender: TObject);
begin
  CurPage := CurPage + 1;
end;

procedure TIpHTMLPreview.btnLastClick(Sender: TObject);
begin
  CurPage := OwnerPanel.PageCount;
end;

procedure TIpHTMLPreview.btnSelectPrinterClick(Sender: TObject);
begin
  if OwnerPanel <> nil then
    if OwnerPanel.SelectPrinterDlg then
      SetZoom(Zoom); {force recalc of preview sizes}
end;

procedure TIpHTMLPreview.edtPageChange(Sender: TObject);
begin
  CurPage := StrToInt(edtPage.Text);
end;

procedure TIpHTMLPreview.btnPrintClick(Sender: TObject);
begin
  Screen.Cursor := crHourglass;
  ScaleFonts := False;
  try
    OwnerPanel.PrintPages(1, OwnerPanel.PageCount);
  finally
    ScaleFonts := True;
    Screen.Cursor := crDefault;
    Close;
  end;
end;

procedure TIpHTMLPreview.ScaleSourceRect;
begin
  SourceRect.Left := round(SourceRect.Left / Scale);
  SourceRect.Top := round(SourceRect.Top / Scale);
  SourceRect.Right := round(SourceRect.Right / Scale);
  SourceRect.Bottom := round(SourceRect.Bottom / Scale);
end;

procedure TIpHTMLPreview.Render;
var
  TTop, TLeft,
  WindowTop, WindowLeft: Integer;
  R: TRect;
begin
  {GDI won't let us create a bitmap for a whole page
   since it would become too big for large resolutions,
   so we have to do banding by hand}

  Screen.Cursor := crHourglass;
  try
    PaintBox1.Canvas.Brush.Color := clWhite;
    PaintBox1.Canvas.FillRect(PaintBox1.Canvas.ClipRect);
    WindowTop := SourceRect.Top;
    TTop := 0;
    while WindowTop < SourceRect.Bottom do begin
      WindowLeft := SourceRect.Left;
      TLeft := 0;
      while WindowLeft < SourceRect.Right do begin
        R.Left := WindowLeft;
        R.Top := WindowTop;
        R.Right := R.Left + SCRATCH_WIDTH + 1;
        R.Bottom := R.Top + SCRATCH_HEIGHT + 1;

        HTML.Render(Scratch.Canvas, R, False, Point(0, 0));

        R.Left := PaintBox1.Canvas.ClipRect.Left + round(TLeft * Scale);
        R.Top := PaintBox1.Canvas.ClipRect.Top + round(TTop * Scale);
        R.Right := R.Left + round(SCRATCH_WIDTH * Scale) + 1;
        R.Bottom := R.Top + round(SCRATCH_HEIGHT * Scale) + 1;

        PaintBox1.Canvas.StretchDraw(R, Scratch);

        inc(WindowLeft, SCRATCH_WIDTH);
        inc(TLeft, SCRATCH_WIDTH);
      end;
      inc(WindowTop, SCRATCH_HEIGHT);
      inc(TTop, SCRATCH_HEIGHT);
    end;
  finally
    Screen.Cursor := crDefault;
  end;
end;

procedure TIpHTMLPreview.PaintBox1Paint(Sender: TObject);
begin
  SourceRect := PaintBox1.Canvas.ClipRect;
  ScaleSourceRect;
  ClipRect := SourceRect;
  OffsetRect(SourceRect, 0, PageRect.Top);
  Render;
end;

procedure TIpHTMLPreview.FormDestroy(Sender: TObject);
begin
  Scratch.Free;
end;

procedure TIpHTMLPreview.ZoomComboChange(Sender: TObject);
var
  S: string;
begin
  with ZoomCombo do
    S := Items[ItemIndex];
  Zoom := StrToInt(copy(S, 1, length(S) - 1));
end;

procedure TIpHTMLPreview.FormCreate(Sender: TObject);
begin
  ZoomCombo.ItemIndex := 4;
  FZoom := 100;
  Scratch := TBitmap.Create;
  Scratch.Width := SCRATCH_WIDTH;
  Scratch.Height := SCRATCH_HEIGHT;

  // localization
  Self.Caption := rsIpHTMLPreviewPrintPreview;
  btnPrint.Caption := rsIpHTMLPreviewPrint;
  Label3.Caption := rsIpHTMLPreviewZoom;
  btnClose.Caption := rsIpHTMLPreviewClose;
  Label1.Caption := rsIpHTMLPreviewPage;
  Label2.Caption := rsIpHTMLPreviewOf;
  btnSelectPrinter.Caption := rsIpHTMLPreviewSelectPrinter
end;

procedure TIpHTMLPreview.SetZoom(const Value: Integer);
var
  Scale1, Scale2, Scale0: double;
  {$IFDEF IP_LAZARUS}
  ClientHeightDbl, ClientWidthDbl: double;
  {$ENDIF}
begin
  FZoom := Value;
  {$IFDEF IP_LAZARUS}
  ClientHeightDbl := ClientHeight;
  ClientWidthDbl := ClientWidth;
  if Printer.PageHeight>0 then
    Scale1 := ClientHeightDbl/Printer.PageHeight
  else
    Scale1 := ClientHeightDbl/500;
  if Printer.PageWidth>0 then
    Scale2 := ClientWidthDbl/ Printer.PageWidth
  else
    Scale2 := ClientWidthDbl/ 500;
  {$ELSE}
  Scale1 := ClientHeight/ Printer.PageHeight; //JMN
  Scale2 := ClientWidth/ Printer.PageWidth; //JMN
  {$ENDIF}
  if Scale1 < Scale2 then
    Scale0 := Scale1
  else
    Scale0 := Scale2;
  FScale := Scale0 * FZoom / 100;
  ResizeCanvas;
  AlignPaintBox;
end;

procedure TIpHTMLPreview.ResizeCanvas;
begin
  ScrollBox1.HorzScrollBar.Position := 0;
  ScrollBox1.VertScrollBar.Position := 0;
  {$IFDEF IP_LAZARUS}
  if Printer.PageHeight>0 then
    PaperPanel.Height := round(Printer.PageHeight * Scale)
  else
    PaperPanel.Height := round(500 * Scale);
  if Printer.PageWidth>0 then
    PaperPanel.Width := round(Printer.PageWidth * Scale)
  else
    PaperPanel.Width := round(500 * Scale);
  {$ELSE}
  PaperPanel.Width := round(Printer.PageWidth * Scale);
  PaperPanel.Height := round(Printer.PageHeight * Scale);
  {$ENDIF}
  PaintBox1.Width := round(OwnerPanel.PrintWidth * Scale);
  PaintBox1.Height := round(OwnerPanel.PrintHeight * Scale);
  PaintBox1.Left := round(OwnerPanel.PrintTopLeft.x * Scale);
  PaintBox1.Top := round(OwnerPanel.PrintTopLeft.y * Scale);
  AlignPaintBox;
end;

procedure TIpHTMLPreview.AlignPaintBox;
begin
  if PaperPanel.Width < ClientWidth then
    PaperPanel.Left := ClientWidth div 2 - (PaperPanel.Width div 2)
  else
    PaperPanel.Left := 0;
  if PaperPanel.Height < ClientHeight then
    PaperPanel.Top := ClientHeight div 2 - (PaperPanel.Height div 2)
  else
    PaperPanel.Top := 0;
end;

procedure TIpHTMLPreview.FormResize(Sender: TObject);
begin
  if OwnerPanel<>nil then
    SetZoom(Zoom); {force recalc of preview sizes}
end;

end.
