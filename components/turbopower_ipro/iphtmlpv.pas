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

{$IFNDEF Html_Print}
  {$ERROR requires -dHTML_Print}
{$ENDIF}

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
  StdCtrls, ExtCtrls, Spin, IpHtml, IpConst;

type

  { TIpHTMLPreview }

  TIpHTMLPreview = class(TForm)
    btnFitHeight: TButton;
    btnFitWidth: TButton;
    btnFit: TButton;
    btnSelectPrinter: TButton;
    Label3: TLabel;
    PaperPanel: TPanel;
    PaintBox1: TPaintBox;
    edtZoom: TSpinEdit;
    ToolbarPanel: TPanel;
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
    procedure btnFirstClick(Sender: TObject);
    procedure btnFitClick(Sender: TObject);
    procedure btnFitHeightClick(Sender: TObject);
    procedure btnFitWidthClick(Sender: TObject);
    procedure btnLastClick(Sender: TObject);
    procedure btnNextClick(Sender: TObject);
    procedure btnPrevClick(Sender: TObject);
    procedure btnPrintClick(Sender: TObject);
    procedure btnSelectPrinterClick(Sender: TObject);
    procedure edtPageChange(Sender: TObject);
    procedure edtZoomChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure PaintBox1Paint(Sender: TObject);
  private
    SourceRect: TRect;
    Scratch: TBitmap;
    FScale: double;
    FZoom: Integer;
    FZoomToFit: Integer;
    FLockZoomUpdate: Integer;
    procedure SetCurPage(const Value: Integer);
    procedure SetZoom(const Value: Integer);
  protected
    procedure AlignPaintBox;
    procedure Render;
    procedure ResizeCanvas;
//    procedure ScaleSourceRect;
    procedure UpdateBtnStates;
  public
    FCurPage: Integer;
    HTML : TIpHtml;
    PageRect: TRect;
    OwnerPanel: TIpHtmlInternalPanel;
    procedure RenderPage(PageNo: Integer);
    property CurPage: Integer read FCurPage write SetCurPage;
    property Scale: double read FScale;
    property Zoom: Integer read FZoom write SetZoom;
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
  ZOOM_FACTOR = 1.1;

function ScaleRect(ARect: TRect; AFactor: Double): TRect;
begin
  Result.Left := round(ARect.Left * AFactor);
  Result.Top := round(ARect.Top * AFactor);
  Result.Right := round(ARect.Right * AFactor);
  Result.Bottom := round(ARect.Bottom * AFactor);
end;

procedure TIpHTMLPreview.AlignPaintBox;
var
  sb: Integer;
begin
  sb := GetSystemMetrics(SM_CXVSCROLL);
  if PaperPanel.Width < ClientWidth - sb then
    PaperPanel.Left := (ClientWidth - sb - PaperPanel.Width) div 2
  else
    PaperPanel.Left := 0;

  sb := GetSystemMetrics(SM_CXHSCROLL);
  if PaperPanel.Height < ClientHeight - sb - ToolbarPanel.Height then
    PaperPanel.Top := (ClientHeight - sb - ToolbarPanel.Height - PaperPanel.Height) div 2
  else
    PaperPanel.Top := 0;
end;

procedure TIpHTMLPreview.btnFirstClick(Sender: TObject);
begin
  CurPage := 1;
end;

procedure TIpHTMLPreview.btnFitClick(Sender: TObject);
begin
  SetZoom(ZOOM_TO_FIT);
end;

procedure TIpHTMLPreview.btnFitHeightClick(Sender: TObject);
begin
  SetZoom(ZOOM_TO_FIT_HEIGHT);
end;

procedure TIpHTMLPreview.btnFitWidthClick(Sender: TObject);
begin
  SetZoom(ZOOM_TO_FIT_WIDTH);
end;

procedure TIpHTMLPreview.btnLastClick(Sender: TObject);
begin
  CurPage := OwnerPanel.PageCount;
end;

procedure TIpHTMLPreview.btnNextClick(Sender: TObject);
begin
  CurPage := CurPage + 1;
end;

procedure TIpHTMLPreview.btnPrevClick(Sender: TObject);
begin
  CurPage := CurPage - 1;
end;

procedure TIpHTMLPreview.btnSelectPrinterClick(Sender: TObject);
begin
  if OwnerPanel <> nil then
    if OwnerPanel.SelectPrinterDlg then
      SetZoom(Zoom); {force recalc of preview sizes}
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

procedure TIpHTMLPreview.edtPageChange(Sender: TObject);
begin
  CurPage := StrToInt(edtPage.Text);
end;

procedure TIpHTMLPreview.edtZoomChange(Sender: TObject);
var
  newZoom: Double;
begin
  if (FLockZoomUpdate = 0) and TryStrToFloat(edtZoom.Text, newZoom) then
    SetZoom(Round(newZoom));
end;

procedure TIpHTMLPreview.FormCreate(Sender: TObject);
begin
  FZoom := 100;
  FZoomToFit := ZOOM_TO_FIT;
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
  btnSelectPrinter.Caption := rsIpHTMLPreviewSelectPrinter;
  btnFit.Caption := rsIpHTMLPreviewFitAll;
  btnFitWidth.Caption := rsIpHTMLPreviewFitWidth;
  btnFitHeight.Caption := rsIpHTMLPreviewFitHeight;
end;

procedure TIpHTMLPreview.FormDestroy(Sender: TObject);
begin
  Scratch.Free;
end;

procedure TIpHTMLPreview.FormResize(Sender: TObject);
begin
  if (FZoomToFit <= 0) and (OwnerPanel <> nil) then
    SetZoom(FZoomToFit)  {force recalc of preview sizes}
  else
    AlignPaintbox;
end;

procedure TIpHTMLPreview.FormShow(Sender: TObject);
begin
  UpdateBtnStates;
  RenderPage(CurPage);
end;

procedure TIpHTMLPreview.PaintBox1Paint(Sender: TObject);
begin
  SourceRect := ScaleRect(PaintBox1.Canvas.ClipRect, 1.0/Scale);
  OffsetRect(SourceRect, 0, PageRect.Top);
  Render;
end;

procedure TIpHTMLPreview.Render;
var
  TileTop, TileLeft,
  WindowTop, WindowLeft: Integer;
  R, Rscr: TRect;
begin
  {GDI won't let us create a bitmap for a whole page
   since it would become too big for large resolutions,
   so we have to do banding by hand}

  Screen.Cursor := crHourglass;
  try
    Application.ProcessMessages;
    PaintBox1.Canvas.Brush.Color := clWhite;
    PaintBox1.Canvas.FillRect(PaintBox1.Canvas.ClipRect);
    PaintBox1.Canvas.AntialiasingMode := OwnerPanel.PreviewAntiAliasingMode;
    WindowTop := SourceRect.Top;
    TileTop := 0;
    while WindowTop < SourceRect.Bottom do begin
      WindowLeft := SourceRect.Left;
      TileLeft := 0;
      while WindowLeft < SourceRect.Right do begin
        R.Left := WindowLeft;
        R.Top := WindowTop;
        R.Right := R.Left + SCRATCH_WIDTH + 1;
        R.Bottom := R.Top + SCRATCH_HEIGHT + 1;
        Rscr := R;
        if R.Bottom - SourceRect.Top > OwnerPanel.PrintHeight then begin
          Scratch.Canvas.FillRect(0, 0, R.Right-R.Left, R.Bottom-R.Top);
          R.Bottom := SourceRect.Top + OwnerPanel.PrintHeight;
        end;

        HTML.Render(Scratch.Canvas, R, PageRect.Top, PageRect.Bottom, False, Point(0, 0));

        OffsetRect(RScr, 0, -PageRect.Top);
        Rscr := ScaleRect(Rscr, Scale);
        PaintBox1.Canvas.StretchDraw(Rscr, Scratch);

        inc(WindowLeft, SCRATCH_WIDTH);
        inc(TileLeft, SCRATCH_WIDTH);
      end;
      inc(WindowTop, SCRATCH_HEIGHT);
      inc(TileTop, SCRATCH_HEIGHT);
    end;
  finally
    Screen.Cursor := crDefault;
  end;

(*
  This is an untiled version ...
var
  R: TRect;
begin
  // Render to single "scratch" bitmap which has the original print size and
  // then is stretch-drawn into the preview paintbox.
  Screen.Cursor := crHourglass;
  try
    Application.ProcessMessages;
    Paintbox1.Canvas.Brush.Color := clWhite;
    Paintbox1.Canvas.FillRect(Paintbox1.Canvas.ClipRect);
    PaintBox1.Canvas.AntialiasingMode := OwnerPanel.PreviewAntiAliasingMode;
    Scratch.Clear;
    Scratch.Width := SourceRect.Right - SourceRect.Left;
    Scratch.Height := SourceRect.Bottom - SourceRect.Top;

    // probably not needed
    Scratch.Canvas.Brush.Color := clWhite;
    Scratch.Canvas.FillRect(SourceRect);

    HTML.Render(Scratch.Canvas, SourceRect, PageRect.Top, PageRect.Bottom, False, Point(0, 0));

    R := Paintbox1.Canvas.ClipRect;
    PaintBox1.Canvas.StretchDraw(R, Scratch);
  finally
    Screen.Cursor := crDefault;
  end;
*)
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

procedure TIpHTMLPreview.ResizeCanvas;
begin
  ScrollBox1.HorzScrollBar.Position := 0;
  ScrollBox1.VertScrollBar.Position := 0;
  {$IFDEF IP_LAZARUS}
  if Printer.PageHeight > 0 then
    PaperPanel.Height := round(Printer.PageHeight * Scale)
  else
    PaperPanel.Height := round(500 * Scale);
  if Printer.PageWidth > 0 then
    PaperPanel.Width := round(Printer.PageWidth * Scale)
  else
    PaperPanel.Width := round(500 * Scale);
  {$ELSE}
  PaperPanel.Width := round(Printer.PageWidth * Scale);
  PaperPanel.Height := round(Printer.PageHeight * Scale);
  {$ENDIF}

  PaintBox1.Left := round(OwnerPanel.PrintTopLeft.x * Scale);
  PaintBox1.Top := round(OwnerPanel.PrintTopLeft.y * Scale);
  Paintbox1.Width := PaperPanel.Width - Paintbox1.Left;
  Paintbox1.Height := PaperPanel.Height - Paintbox1.top;

  AlignPaintBox;
end;

procedure TIpHTMLPreview.SetCurPage(const Value: Integer);
begin
  if (Value <> FCurPage) and (Value >= 1) and (Value <= OwnerPanel.PageCount) then
  begin
    FCurPage := Value;
    RenderPage(Value);
    edtPage.Text := IntToStr(CurPage);
    UpdateBtnStates;
  end;
end;

procedure TIpHTMLPreview.SetZoom(const Value: Integer);
var
  ClientHeightDbl, ClientWidthDbl: Double;
  PrnPgHeight, PrnPgWidth: Double;
  scaleW, scaleH: Integer;
  sb: Integer;
begin
  FZoomToFit := Value;

  // Available client area in inches, without scrollbars
  sb := GetSystemMetrics(SM_CXHSCROLL);
  ClientHeightDbl := (ClientHeight - sb - ToolbarPanel.Height) / ScreenInfo.PixelsPerInchY;
  sb := GetSystemMetrics(SM_CXVSCROLL);
  ClientWidthDbl := (ClientWidth - sb)/ ScreenInfo.PixelsPerInchX;

  // Printer page size in inches
  PrnPgHeight := Printer.PageHeight / Printer.YDpi;
  PrnPgWidth := Printer.PageWidth / Printer.XDpi;

  case Value of
    ZOOM_TO_FIT:
      begin
        scaleW := round(ClientWidthDbl / PrnPgWidth * 100);
        scaleH := round(ClientHeightDbl / PrnPgHeight * 100);
        if scaleW < scaleH then FZoom := scaleW else FZoom := scaleH;
      end;
    ZOOM_TO_FIT_WIDTH:
      FZoom := round(ClientWidthDbl / PrnPgWidth  * 100);
    ZOOM_TO_FIT_HEIGHT:
      FZoom := round(ClientHeightDbl / PrnPgHeight  * 100);
    else
      FZoom := Value;
  end;
  inc(FLockZoomUpdate);
  edtZoom.Value := FZoom;
  dec(FLockZoomUpdate);
  FScale := ScreenInfo.PixelsPerInchX / Printer.XDpi * FZoom * 0.01;

  ResizeCanvas;
end;

procedure TIpHtmlPreview.UpdateBtnStates;
begin
  btnFirst.Enabled := (FCurPage > 1);
  btnPrev.Enabled := (FCurPage > 1);
  btnNext.Enabled := (FCurPage < OwnerPanel.PageCount);
  btnLast.Enabled := (FCurPage < OwnerPanel.PageCount);
end;

end.
