{*****

  Copyright (c) 2012 Michał Gawrycki (michal.gawrycki(a.t.)gmsystems.pl
  License: modified LGPL (see 'COPYING.modifiedLGPL.txt' in Lazarus directory)

*****}

unit LR_e_img;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LR_Class, Graphics, LazUtf8Classes;

type
  TfrImageExport = class(TComponent)

  end;

  { TfrImageExportFilter }

  TfrImageExportFilter = class(TfrExportFilter)
  private
    FBmp: TFPImageBitmap;
    FCurPage: Integer;
    FFileName: String;
    FFileExt: String;
    FZoom: Extended;
    FJQuality: TJPEGQualityRange;
    FColor: TColor;
  public
    constructor Create(AStream: TStream); override;
    destructor Destroy; override;
    procedure OnBeginDoc; override;
    procedure OnEndPage; override;
    property Zoom: Extended read FZoom write FZoom;
    property JPEGQuality: TJPEGQualityRange read FJQuality write FJQuality;
    property BackgroundColor: TColor read FColor write FColor;
  end;

implementation

{ TfrImageExportFilter }

constructor TfrImageExportFilter.Create(AStream: TStream);
begin
  inherited Create(AStream);
  FFileName := TFileStreamUtf8(AStream).FileName;
  FFileExt := LowerCase(ExtractFileExt(FFileName));
  FFileName := ChangeFileExt(FFileName, '');
  FZoom := 1;
  FCurPage := 0;
  FJQuality := 75;
  FColor := clWhite;
  if FFileExt = '.jpg' then
    FBmp := TJPEGImage.Create
  else
    if FFileExt = '.png' then
      FBmp := TPortableNetworkGraphic.Create
    else
      FBmp := TBitmap.Create;
end;

destructor TfrImageExportFilter.Destroy;
begin
  FBmp.Free;
  inherited Destroy;
end;

procedure TfrImageExportFilter.OnBeginDoc;
begin
  if FBmp is TJPEGImage then
    TJPEGImage(FBmp).CompressionQuality := FJQuality;
end;

procedure TfrImageExportFilter.OnEndPage;
var
  TmpVisible: Boolean;
begin
  Inc(FCurPage);
  FBmp.Clear;
  FBmp.SetSize(Round(CurReport.EMFPages[FCurPage - 1]^.PrnInfo.Pgw * FZoom),
    Round(CurReport.EMFPages[FCurPage - 1]^.PrnInfo.Pgh * FZoom));
  FBmp.Canvas.Brush.Color := FColor;
  FBmp.Canvas.Brush.Style := bsSolid;
  FBmp.Canvas.FillRect(0, 0, FBmp.Width, FBmp.Height);
  TmpVisible := CurReport.EMFPages[FCurPage - 1]^.Visible;
  CurReport.EMFPages[FCurPage - 1]^.Visible := True;
  CurReport.EMFPages.Draw(FCurPage - 1, FBmp.Canvas, Rect(0, 0, FBmp.Width, FBmp.Height));
  CurReport.EMFPages[FCurPage - 1]^.Visible := TmpVisible;
  if FCurPage = 1 then
    FBmp.SaveToStream(Stream)
  else
    FBmp.SaveToFile(FFileName + '_' + IntToStr(FCurPage) + FFileExt);
end;

initialization
  frRegisterExportFilter(TfrImageExportFilter, 'Bitmap file  (*.bmp)', '*.bmp');
  frRegisterExportFilter(TfrImageExportFilter, 'JPEG file  (*.jpg)', '*.jpg');
  frRegisterExportFilter(TfrImageExportFilter, 'PNG file  (*.png)', '*.png');

end.

