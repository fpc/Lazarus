{
  LazReport matrix for export reports

 Copyright (C) 2014-2015 alexs alexs75.at.yandex.ru

 The module is designed to create an image of the report with the exact
 positioning of objects and subsequent binding to the worksheet

 This library is free software; you can redistribute it and/or modify it
 under the terms of the GNU Library General Public License as published by
 the Free Software Foundation; either version 2 of the License, or (at your
 option) any later version with the following modification:

 As a special exception, the copyright holders of this library give you
 permission to link this library with independent modules to produce an
 executable, regardless of the license terms of these independent modules,and
 to copy and distribute the resulting executable under terms of your choice,
 provided that you also meet, for each linked independent module, the terms
 and conditions of the license of that module. An independent module is a
 module which is not derived from or based on this library. If you modify
 this library, you may extend this exception to your version of the library,
 but you are not obligated to do so. If you do not wish to do so, delete this
 exception statement from your version.

 This program is distributed in the hope that it will be useful, but WITHOUT
 ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
 FITNESS FOR A PARTICULAR PURPOSE. See the GNU Library General Public License
 for more details.

 You should have received a copy of the GNU Library General Public License
 along with this library; if not, write to the Free Software Foundation,
 Inc., 51 Franklin Street - Fifth Floor, Boston, MA 02110-1335, USA.
}

unit le_e_spreadsheet;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LR_ExportMatrix, LR_Class, LR_BarC, fpspreadsheet, Graphics, le_e_spreadsheet_types;

type

  { TlrSpreadSheetExportFilter }

  TlrSpreadSheetExportFilter = class(TfrExportFilter)
  private
    FDataGrouping: TlreDataGrouping;
    FDataGroupingChunks: integer;
    FDeleteEmptyRow: boolean;
    FExportMatrix:TExportMatrix;
    FExportPrintRange: boolean;
    FExportImages: boolean;
    FExportURL: boolean;
    FMergeCell: boolean;
    FOpenAfterExport: boolean;
    FWorkbook:TsWorkbook;
    FWorksheet:TsWorksheet;
    FFileName:string;
    FCurPage:integer;
    FTmpTextWidth: Double;
    FTmpTextHeight: Double;
    FTempMemStreem: TStream;
    procedure ExportColWidth;
    procedure ExportRowHight;
    procedure DoExportPrintRange;
    procedure ExportData1;

    procedure MakeWorksheet;
    function GetTempMemStreem(AClear:boolean):TStream;
    procedure ShowBarCode(View: TfrCustomBarCodeView);
  protected
    function Setup:boolean; override;
    procedure AfterExport; override;
  public
    constructor Create(AStream: TStream); override;
    destructor Destroy; override;
    procedure OnBeginPage; override;
    procedure OnEndPage; override;
    procedure OnBeginDoc; override;
    procedure OnEndDoc; override;
{    procedure ShowBackGround(View: TfrView; x, y, h, w: integer);
    procedure Frame(View: TfrView; x, y, h, w: integer);
    procedure ShowFrame(View: TfrView; x, y, h, w: integer);
    procedure Line(View: TfrView; x1,y1, x2,y2: Integer);
    procedure ShowPicture(View: TfrPictureView; x, y, h, w: integer);
    procedure ShowRoundRect(View: TfrRoundRectView; x, y, h, w: integer);
    procedure ShowShape(View: TfrShapeView; x, y, h, w: integer);
    procedure OnText(X, Y: Integer; const Text: string; View: TfrView); override;
    procedure OnData(x, y: Integer; View: TfrView); override;}
    procedure OnExported(x, y: Integer; View: TfrView); override;
  public
    property DataGrouping:TlreDataGrouping read FDataGrouping write FDataGrouping;
    property DataGroupingChunks:integer read FDataGroupingChunks write FDataGroupingChunks;
    property OpenAfterExport:boolean read FOpenAfterExport write FOpenAfterExport;
    property DeleteEmptyRow:boolean read FDeleteEmptyRow write FDeleteEmptyRow;
    property MergeCell:boolean read FMergeCell write FMergeCell;
    property ExportURL:boolean read FExportURL write FExportURL;
    property ExportPrintRange:boolean read FExportPrintRange write FExportPrintRange;
    property ExportImages:boolean read FExportImages write FExportImages;
  end;

implementation
uses LCLType, le_e_spreadsheet_params, fpsTypes, fpsutils, fpsAllFormats,
  LazUTF8Classes, Forms, Controls, LCLIntf, LazFileUtils, le_e_spreadsheet_consts,
  lrSpreadSheetExp, math, fpsPageLayout;

const
  ssAligns : array [TAlignment] of TsHorAlignment = (haLeft, haRight, haCenter);
  ssLayout : array [TTextLayout] of TsVertAlignment = (vaTop, vaCenter, vaBottom);

  { TlrSpreadSheetExportFilter }

procedure TlrSpreadSheetExportFilter.ExportColWidth;
var
  i: Integer;
begin
  for i:=0 to FExportMatrix.ColumnCount-1 do
    //FWorksheet.WriteColWidth(i, round(FExportMatrix.ColumnWidth[i] * 0.161404639));
    //FWorksheet.WriteColWidth(i, Max(FExportMatrix.ColumnWidth[i] / FTmpTextWidth, 1));
    FWorksheet.WriteColWidth(i, FExportMatrix.ColumnWidth[i] / FTmpTextWidth);
end;

procedure TlrSpreadSheetExportFilter.ExportRowHight;
var
  i: Integer;
begin
  for i:=0 to FExportMatrix.RowCount - 1 do
    FWorksheet.WriteRowHeight(i, FExportMatrix.RowHiht[i] / FTmpTextHeight);
end;

procedure TlrSpreadSheetExportFilter.DoExportPrintRange;
var
  X1, Y1, X2, Y2, R, C: Integer;
  Row: TExportRow;
  Cel: TExportObject;
begin
  X1:=MaxInt;
  Y1:=MaxInt;
  X2:=-1;
  Y2:=-1;
  for R:=0 to FExportMatrix.Rows.Count-1 do
  begin
    Row:=TExportRow(FExportMatrix.Rows[R]);
    for C:=0 to Row.Cells.Count-1 do
    begin
      Cel:=TExportObject(Row.Cells[C]);
      if Assigned(Cel) then
      begin
        if Cel.Col < X1 then
          X1:=Cel.Col;
        if Cel.Row < Y1 then
          Y1:=Cel.Row;

        if (Cel.Col < Cel.MergedCol) then
        begin
          if Cel.MergedCol > X2 then
            X2:=Cel.MergedCol;
        end
        else
        if Cel.Col > X2 then
          X2:=Cel.Col;


        if (Cel.Row < Cel.MergedRow) then
        begin
          if Cel.MergedRow > Y2 then
            Y2:=Cel.MergedRow;
        end
        else
        if Cel.Row > Y2 then
          Y2:=Cel.Row;
      end;
    end;
  end;

  if (X1>0) and (Y1>0) and (X2>X1) and (Y2>Y1) then
  begin
    FWorksheet.PageLayout.AddPrintRange(Y1, X1, Y2, X2);
  end;
end;

function sftofs(AFont:TFont):TsFontStyles;
begin
  Result:=[];
  if fsBold in AFont.Style then
    Result:=Result + [fssBold];

  if fsItalic in AFont.Style then
    Result:=Result + [fssItalic];

  if fsStrikeOut in AFont.Style then
    Result:=Result + [fssStrikeOut];

  if fsUnderline in AFont.Style then
    Result:=Result + [fssUnderline];
end;

procedure TlrSpreadSheetExportFilter.ExportData1;
var
  R: Integer;
  Row: TExportRow;
  C: Integer;
  Cel: TExportObject;
  X: Integer;
  Y: Integer;
  scFrm:TsCellBorders;
  FS: TStream;
  FISF: Extended;
begin
  for R:=0 to FExportMatrix.Rows.Count-1 do
  begin
    Row:=TExportRow(FExportMatrix.Rows[R]);
    for C:=0 to Row.Cells.Count-1 do
    begin
      Cel:=TExportObject(Row.Cells[C]);

      if Assigned(Cel) then
      begin
        X:=Cel.Col;
        Y:=Row.Row;
        if (Cel.ObjType = gtMemo) and (Cel.Text<>'') then
        begin
          FWorksheet.WriteUTF8Text(Y, X, TrimRight(Cel.Texts.Text));
          if Cel.Angle <> 0 then
            FWorksheet.WriteTextRotation(Y, X, rt90DegreeCounterClockwiseRotation);
          FWorksheet.WriteVertAlignment(Y, X, ssLayout[Cel.Layout]);
          FWorksheet.WriteWordwrap(Y, X, Cel.WordWrap);

          FWorksheet.WriteBackgroundColor(Y, X, Cel.FillColor);
        end
        else
        if (Cel.ObjType = gtPicture) and (Assigned(Cel.Picture)) then
        begin
          // this code for export images - not work. in progress
          FS:=GetTempMemStreem(true);
          Cel.Picture.SaveToStream(FS);
          if (FS.Size > 0) and (Cel.Widht > 0) and (Cel.Height > 0) then
          begin
            FS.Position:=0;
            FISF:=2.1;  // empiric value :-(
            FWorksheet.WriteImage(Y, X, FS, 0, 0, Cel.Picture.Width / Cel.Widht / FISF, Cel.Picture.Height / Cel.Height / FISF);
          end;
        end;


        if (Cel.Col < Cel.MergedCol) or (Cel.Row < Cel.MergedRow) then
          FWorksheet.MergeCells(Y, X, Cel.MergedRow, Cel.MergedCol);

        if Assigned(Cel.Font) and (Cel.Font.Size > 0) then
          FWorksheet.WriteFont(Y, X, Cel.Font.Name,  Cel.Font.Size, sftofs(Cel.Font), Cel.Font.Color, fpNormal);

        FWorksheet.WriteHorAlignment(Y, X, ssAligns[Cel.Alignment]);

        scFrm:=[];
        if frbLeft in Cel.Frames then
        begin
          FWorksheet.WriteBorderColor(Y, X, cbEast, Cel.FrameColor);
          scFrm:=scFrm + [cbEast]
        end;
        if frbTop in Cel.Frames then
        begin
          FWorksheet.WriteBorderColor(Y, X, cbNorth, Cel.FrameColor);
          scFrm:=scFrm + [cbNorth]
        end;
        if frbBottom in Cel.Frames then
        begin
          FWorksheet.WriteBorderColor(Y, X, cbSouth, Cel.FrameColor);
          scFrm:=scFrm + [cbSouth]
        end;
        if frbRight in Cel.Frames then
        begin
          FWorksheet.WriteBorderColor(Y, X, cbWest, Cel.FrameColor);
          scFrm:=scFrm + [cbWest]
        end;

        if scFrm <> [] then
          FWorksheet.WriteBorders(Y, X, scFrm);

        if FExportURL and  (Cel.URLInfo <> '') then
          FWorksheet.WriteHyperlink(Y, X, Cel.URLInfo, Cel.URLInfo);
      end;
    end;
  end;
end;

procedure TlrSpreadSheetExportFilter.MakeWorksheet;
var
  S: String;
begin
  FExportMatrix.PrepareData;
  if FDataGrouping = ldgLikeReport then
    S:=Format(sPageName, [FCurPage])
  else
    S:=sReportPageName;

  FWorksheet := FWorkbook.AddWorksheet(S);

  ExportColWidth;
  ExportRowHight;
  ExportData1;
  if FExportPrintRange then
  begin
    DoExportPrintRange;
    FWorksheet.PageLayout.Options := FWorksheet.PageLayout.Options + [poFitPages];
    FWorksheet.PageLayout.FitWidthToPages := 1;     // all columns on one page width
    FWorksheet.PageLayout.FitHeightToPages := 0;
  end;

  FWorksheet:=nil;
  FExportMatrix.Clear;
end;

function TlrSpreadSheetExportFilter.GetTempMemStreem(AClear: boolean): TStream;
begin
  if FTempMemStreem = nil then
    FTempMemStreem:=TMemoryStream.Create
  else
  if AClear then
    TMemoryStream(FTempMemStreem).Clear;

  Result:=FTempMemStreem;
end;

procedure TlrSpreadSheetExportFilter.ShowBarCode(View: TfrCustomBarCodeView);
var
  R: TExportObject;
  FBmp: TBitmap;
  FX, FY: Integer;
  LHeader: ^TBitMapInfoHeader;
  M: TMemoryStream;
const
  F_DPI = 60; //empiric value :-(
{
+  procedure BitmapSaveToFile(const ABitmap: TBitmap; const AFileName: TFileName;
+    const ADPI: Integer);
+  var
+    LMemoryStream: TMemoryStream;
+    LHeader: ^TBitMapInfoHeader;
+  begin
+    LMemoryStream := TMemoryStream.Create;
+    try
+      ABitmap.SaveToStream(LMemoryStream);
+      LHeader := LMemoryStream.Memory + SizeOf(TBitMapFileHeader);
+      LHeader^.biXPelsPerMeter := Ceil(ADPI * 100 / 2.54);
+      LHeader^.biYPelsPerMeter := LHeader^.biXPelsPerMeter;
+      LMemoryStream.SaveToFile(AFileName);
+    finally
+      FreeAndNil(LMemoryStream);
+    end;
+  end;
+}
begin
  R:=FExportMatrix.ExportObject(View);
  FBmp:=TBitmap.Create;
  FBmp.Width:=View.dx;
  FBmp.Height:=View.dy;
  FBmp.Canvas.Brush.Color:=clWhite;
  FBmp.Canvas.FillRect(0, 0, FBmp.Width, FBmp.Height);
  FBmp.Canvas.Pen.Color:=clBlack;
  FBmp.Canvas.Line(1, 1 , 10, 10 );

  FX:=View.X;
  FY:=View.Y;
  View.X:=0;
  View.Y:=0;

  View.Draw(FBmp.Canvas);

  View.X:=FX;
  View.Y:=FY;

  M:=TMemoryStream.Create;
  FBMP.SaveToStream(M);

  LHeader := M.Memory + SizeOf(TBitMapFileHeader);
  LHeader^.biXPelsPerMeter := Ceil(F_DPI * 100 / 2.54);
  LHeader^.biYPelsPerMeter := LHeader^.biXPelsPerMeter;
  M.Position:=0;
  R.NeedPicture;
  M.Position:=0;
  R.Picture.LoadFromStream(M);

  R.ObjType:=gtPicture;
  FBmp.Free;
end;

function TlrSpreadSheetExportFilter.Setup: boolean;
begin
  Result:=inherited Setup;

  if Assigned(lrSpreadSheetExportComponent) and not lrSpreadSheetExportComponent.ShowSetupForm then exit;

  leSpreadsheetParamsForm:=TleSpreadsheetParamsForm.Create(Application);
  leSpreadsheetParamsForm.LikeReportRadioButton.Checked:=FDataGrouping = ldgLikeReport;
  leSpreadsheetParamsForm.AllInOnePageRadioButton.Checked:=FDataGrouping = ldgAllInOnePage;
  leSpreadsheetParamsForm.RowsPerChunkRadioButton.Checked:=FDataGrouping = ldgChunks;
  leSpreadsheetParamsForm.RowsPerChunkEdit.Value:=FDataGroupingChunks;
  leSpreadsheetParamsForm.OpenAfterExportCheckBox.Checked:=FOpenAfterExport;
  leSpreadsheetParamsForm.MergeCellsCheckBox.Checked:=FMergeCell;
  leSpreadsheetParamsForm.DeleteEmptyRowsCheckBox.Checked:=FDeleteEmptyRow;
  leSpreadsheetParamsForm.ExportPicturesCheckBox.Checked:=FExportImages;
  leSpreadsheetParamsForm.ExportURLCheckBox.Checked:=FExportURL;
  leSpreadsheetParamsForm.ExportPrintRangeCheckBox.Checked:=FExportPrintRange;
  leSpreadsheetParamsForm.ExportReportTitleCheckBox.Checked:=btReportTitle in BandTypes;
  leSpreadsheetParamsForm.ExportReportSummaryCheckBox.Checked:=btReportSummary in BandTypes;
  leSpreadsheetParamsForm.ExportPageHeaderCheckBox.Checked:=btPageHeader in BandTypes;
  leSpreadsheetParamsForm.ExportPageFooterCheckBox.Checked:=btPageFooter in BandTypes;

  Result:=leSpreadsheetParamsForm.ShowModal = mrOk;
  if Result then
  begin
    if leSpreadsheetParamsForm.LikeReportRadioButton.Checked then
      FDataGrouping:=ldgLikeReport
    else
    if leSpreadsheetParamsForm.AllInOnePageRadioButton.Checked then
      FDataGrouping:=ldgAllInOnePage
    else
    begin
      FDataGrouping:=ldgChunks;
      FDataGroupingChunks:=leSpreadsheetParamsForm.RowsPerChunkEdit.Value;
    end;
    FExportImages   := leSpreadsheetParamsForm.ExportPicturesCheckBox.Checked;
    FOpenAfterExport:= leSpreadsheetParamsForm.OpenAfterExportCheckBox.Checked;
    FMergeCell      := leSpreadsheetParamsForm.MergeCellsCheckBox.Checked;
    FDeleteEmptyRow := leSpreadsheetParamsForm.DeleteEmptyRowsCheckBox.Checked;
    FExportURL      := leSpreadsheetParamsForm.ExportURLCheckBox.Checked;
    FExportPrintRange:=leSpreadsheetParamsForm.ExportPrintRangeCheckBox.Checked;

    FExportMatrix.MergeCell:=FMergeCell;
    FExportMatrix.DeleteEmptyRow:=FDeleteEmptyRow;

    if leSpreadsheetParamsForm.ExportReportTitleCheckBox.Checked then
      BandTypes := BandTypes + [btReportTitle]
    else
      BandTypes := BandTypes - [btReportTitle];
    if leSpreadsheetParamsForm.ExportReportSummaryCheckBox.Checked then
      BandTypes := BandTypes + [btReportSummary]
    else
      BandTypes := BandTypes - [btReportSummary];
    if leSpreadsheetParamsForm.ExportPageHeaderCheckBox.Checked then
      BandTypes := BandTypes + [btPageHeader]
    else
      BandTypes := BandTypes - [btPageHeader];
    if leSpreadsheetParamsForm.ExportPageFooterCheckBox.Checked then
      BandTypes := BandTypes + [btPageFooter]
    else
      BandTypes := BandTypes - [btPageFooter];
  end;
  leSpreadsheetParamsForm.Free;
end;

procedure TlrSpreadSheetExportFilter.AfterExport;
begin
  inherited AfterExport;
  if FOpenAfterExport and FileExistsUTF8(FFileName) then
    OpenDocument(FFileName);
end;

constructor TlrSpreadSheetExportFilter.Create(AStream: TStream);
begin
  inherited Create(AStream);

  if Assigned(lrSpreadSheetExportComponent) then
  begin
    FDataGrouping:=lrSpreadSheetExportComponent.DataGrouping;
    FDataGroupingChunks:=lrSpreadSheetExportComponent.DataGroupingChunks;
    FOpenAfterExport:=lrSpreadSheetExportComponent.OpenAfterExport;
    FMergeCell:=lrSpreadSheetExportComponent.MergeCell;
    FDeleteEmptyRow:=lrSpreadSheetExportComponent.DeleteEmptyRow;
    FExportURL:=lrSpreadSheetExportComponent.ExportURL;
    FExportPrintRange:=lrSpreadSheetExportComponent.ExportPrintRange;
    FExportImages:=lrSpreadSheetExportComponent.ExportImages;
    BandTypes:=lrSpreadSheetExportComponent.BandTypes;
  end
  else
  begin
    FDataGrouping:=ldgLikeReport;
    FDataGroupingChunks:=50;
    FOpenAfterExport:=true;
    FMergeCell:=true;
    FDeleteEmptyRow:=false;
    FExportURL:=false;
    FExportPrintRange:=false;
    FExportImages:=false;
  end;

  FExportMatrix:=TExportMatrix.Create;
  FExportMatrix.MergeCell:=FMergeCell;
  FExportMatrix.DeleteEmptyRow:=FDeleteEmptyRow;

  if AStream is TFileStreamUTF8 then
    FFileName := TFileStreamUTF8(AStream).FileName
  else
    FFileName := '';
end;

destructor TlrSpreadSheetExportFilter.Destroy;
begin
  FExportMatrix.Free;
  if Assigned(FTempMemStreem) then
    FreeAndNil(FTempMemStreem);
  inherited Destroy;
end;

procedure TlrSpreadSheetExportFilter.OnBeginPage;
var
  P: PfrPageInfo;
begin
  inherited OnBeginPage;
  Inc(FCurPage);

  if (FDataGrouping = ldgAllInOnePage) and (FCurPage>1) then
    FExportMatrix.NewPage;
end;

procedure TlrSpreadSheetExportFilter.OnEndPage;
begin
  inherited OnEndPage;
  if FDataGrouping = ldgLikeReport then
    MakeWorksheet;
end;

procedure TlrSpreadSheetExportFilter.OnBeginDoc;
begin
  inherited OnBeginDoc;
//  FTmpTextWidth:=(TempBmp.Canvas.TextWidth('W') + TempBmp.Canvas.TextWidth('i')) div 2;
//  FTmpTextWidth:=TempBmp.Canvas.TextWidth('Wi') / 2;
{  FTmpTextWidth:=TempBmp.Canvas.TextWidth('I');
  FTmpTextHeight:=TempBmp.Canvas.TextHeight('Wg');}
  FTmpTextWidth:=7;
  FTmpTextHeight:=12;
  FWorkbook := TsWorkbook.Create;
  FCurPage:=0;
end;

function GetFormatFromFileName(const AFileName: TFileName;
  out SheetType: TsSpreadsheetFormat): Boolean;
var
  suffix: String;
begin
  Result := true;
  suffix := Lowercase(ExtractFileExt(AFileName));
  case suffix of
    STR_EXCEL_EXTENSION               : SheetType := sfExcel8;
    STR_OOXML_EXCEL_EXTENSION         : SheetType := sfOOXML;
    STR_OPENDOCUMENT_CALC_EXTENSION   : SheetType := sfOpenDocument;
    STR_COMMA_SEPARATED_EXTENSION     : SheetType := sfCSV;
    STR_HTML_EXTENSION, '.htm'        : SheetType := sfHTML;
    STR_WIKITABLE_PIPES_EXTENSION     : SheetType := sfWikiTable_Pipes;
    STR_WIKITABLE_WIKIMEDIA_EXTENSION : SheetType := sfWikiTable_WikiMedia;
    else                                Result := False;
  end;
end;


procedure TlrSpreadSheetExportFilter.OnEndDoc;
var
  S: String;
  sfFileType: TsSpreadsheetFormat;
begin
  inherited OnEndDoc;

  if FDataGrouping = ldgAllInOnePage then
    MakeWorksheet;

  if not GetFormatFromFileName(FFileName, sfFileType) then
    sfFileType:=sfOpenDocument;

  FWorkbook.WriteToStream(Stream, sfFileType);
  FWorkbook.Free;

  FWorkbook:=nil;
end;

procedure TlrSpreadSheetExportFilter.OnExported(x, y: Integer; View: TfrView);
begin
  if not Assigned(View) or not (View.ParentBandType in BandTypes) then exit;  
  if View is TfrCustomBarCodeView then
    ShowBarCode(View as TfrCustomBarCodeView)
  else
  if (View is TfrMemoView) then
    FExportMatrix.ExportObject(View)
  else
  if (View is TfrPictureView) and FExportImages then
    FExportMatrix.ExportObject(View)
end;

initialization
  frRegisterExportFilter(TlrSpreadSheetExportFilter, 'Microsoft Excel (*.xls)', '*.xls');
  frRegisterExportFilter(TlrSpreadSheetExportFilter, 'Microsoft Excel 2007/2010 (*.xlsx)', '*.xlsx');
  frRegisterExportFilter(TlrSpreadSheetExportFilter, 'OpenOffice/LibreOffice (*.ods)', '*.ods');
end.

