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
 Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
}

unit le_e_spreadsheet;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LR_ExportMatrix, LR_Class, fpspreadsheet, Graphics, le_e_spreadsheet_types;

type

  { TlrSpreadSheetExportFilter }

  TlrSpreadSheetExportFilter = class(TfrExportFilter)
  private
    FDataGrouping: TlreDataGrouping;
    FDataGroupingChunks: integer;
    FDeleteEmptyRow: boolean;
    FExportMatrix:TExportMatrix;
    FMergeCell: boolean;
    FOpenAfterExport: boolean;
    FWorkbook:TsWorkbook;
    FWorksheet:TsWorksheet;
    FFileName:string;
    FCurPage:integer;
{    FTmpTextWidth: Integer;
    FTmpTextHeight: Integer;}
    FTmpTextWidth: Double;
    FTmpTextHeight: Double;
    procedure ExportColWidth;
    procedure ExportRowHight;
    //procedure ExportData;
    procedure ExportData1;

    procedure MakeWorksheet;
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
    procedure ShowBarCode(View: TfrBarCodeView; x, y, h, w: integer);
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
  end;

implementation
uses LCLType, le_e_spreadsheet_params, fpsTypes, fpsutils, LazUTF8Classes, Forms, Controls,
  LCLIntf, LazFileUtils, le_e_spreadsheet_consts, lrSpreadSheetExp, math;

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
{
procedure TlrSpreadSheetExportFilter.ExportData;
var
  R:TExportObject;
  y: Integer;
  x: Integer;
  scFrm:TsCellBorders;

begin
  for y:=0 to FExportMatrix.RowCount do
    for x:=0 to FExportMatrix.ColumnCount do
    begin
      R:=FExportMatrix.Objects[X, Y];
      if Assigned(R) then
      begin
        if R.Text<>'' then
        begin
          FWorksheet.WriteUTF8Text(Y, X, TrimRight(R.Texts.Text));
          if R.Angle <> 0 then
            FWorksheet.WriteTextRotation(Y, X, rt90DegreeCounterClockwiseRotation);
          FWorksheet.WriteVertAlignment(Y, X, ssLayout[R.Layout]);
          FWorksheet.WriteWordwrap(Y, X, R.WordWrap);
        end;
        FWorksheet.WriteBackgroundColor(Y, X, R.FillColor);

        if (R.Col < R.MergedCol) or (R.Row < R.MergedRow) then
          FWorksheet.MergeCells(Y, X, R.MergedRow, R.MergedCol);

        if Assigned(R.Font) then
          FWorksheet.WriteFont(Y, X, R.Font.Name,  R.Font.Size, sftofs(R.Font), R.Font.Color, fpNormal);

        FWorksheet.WriteHorAlignment(Y, X, ssAligns[R.Alignment]);

        scFrm:=[];
        if frbLeft in R.Frames then
        begin
          FWorksheet.WriteBorderColor(Y, X, cbEast, R.FrameColor);
          scFrm:=scFrm + [cbEast]
        end;
        if frbTop in R.Frames then
        begin
          FWorksheet.WriteBorderColor(Y, X, cbNorth, R.FrameColor);
          scFrm:=scFrm + [cbNorth]
        end;
        if frbBottom in R.Frames then
        begin
          FWorksheet.WriteBorderColor(Y, X, cbSouth, R.FrameColor);
          scFrm:=scFrm + [cbSouth]
        end;
        if frbRight in R.Frames then
        begin
          FWorksheet.WriteBorderColor(Y, X, cbWest, R.FrameColor);
          scFrm:=scFrm + [cbWest]
        end;

        if scFrm <> [] then
          FWorksheet.WriteBorders(Y, X, scFrm);
      end;
    end;
end;
}
procedure TlrSpreadSheetExportFilter.ExportData1;
var
  R: Integer;
  Row: TExportRow;
  C: Integer;
  Cel: TExportObject;
  X: Integer;
  Y: Integer;
  scFrm:TsCellBorders;
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
        if Cel.Text<>'' then
        begin
          FWorksheet.WriteUTF8Text(Y, X, TrimRight(Cel.Texts.Text));
          if Cel.Angle <> 0 then
            FWorksheet.WriteTextRotation(Y, X, rt90DegreeCounterClockwiseRotation);
          FWorksheet.WriteVertAlignment(Y, X, ssLayout[Cel.Layout]);
          FWorksheet.WriteWordwrap(Y, X, Cel.WordWrap);
        end;
        FWorksheet.WriteBackgroundColor(Y, X, Cel.FillColor);

        if (Cel.Col < Cel.MergedCol) or (Cel.Row < Cel.MergedRow) then
          FWorksheet.MergeCells(Y, X, Cel.MergedRow, Cel.MergedCol);

        if Assigned(Cel.Font) then
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
  //ExportData;
  ExportData1;
  FWorksheet:=nil;
  FExportMatrix.Clear;
end;

function TlrSpreadSheetExportFilter.Setup: boolean;
begin
  Result:=inherited Setup;

  if Assigned(lrSpreadSheetExportComponent) and not lrSpreadSheetExportComponent.ShowSetupForm then exit;

  leSpreadsheetParamsForm:=TleSpreadsheetParamsForm.Create(Application);
  leSpreadsheetParamsForm.RadioButton4.Checked:=FDataGrouping = ldgLikeReport;
  leSpreadsheetParamsForm.RadioButton5.Checked:=FDataGrouping = ldgAllInOnePage;
  leSpreadsheetParamsForm.RadioButton6.Checked:=FDataGrouping = ldgChunks;
  leSpreadsheetParamsForm.SpinEdit1.Value:=FDataGroupingChunks;
  leSpreadsheetParamsForm.CheckBox4.Checked:=FOpenAfterExport;
  leSpreadsheetParamsForm.CheckBox2.Checked:=FMergeCell;
  leSpreadsheetParamsForm.CheckBox6.Checked:=FDeleteEmptyRow;


  Result:=leSpreadsheetParamsForm.ShowModal = mrOk;
  if Result then
  begin
    if leSpreadsheetParamsForm.RadioButton4.Checked then
      FDataGrouping:=ldgLikeReport
    else
    if leSpreadsheetParamsForm.RadioButton5.Checked then
      FDataGrouping:=ldgAllInOnePage
    else
    begin
      FDataGrouping:=ldgChunks;
      FDataGroupingChunks:=leSpreadsheetParamsForm.SpinEdit1.Value;
    end;
    FOpenAfterExport:=leSpreadsheetParamsForm.CheckBox4.Checked;
    FMergeCell      := leSpreadsheetParamsForm.CheckBox2.Checked;
    FDeleteEmptyRow := leSpreadsheetParamsForm.CheckBox6.Checked;

    FExportMatrix.MergeCell:=FMergeCell;
    FExportMatrix.DeleteEmptyRow:=FDeleteEmptyRow;
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
  end
  else
  begin
    FDataGrouping:=ldgLikeReport;
    FDataGroupingChunks:=50;
    FOpenAfterExport:=true;
    FMergeCell:=true;
    FDeleteEmptyRow:=false;
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
  if not Assigned(View) then exit;
  FExportMatrix.ExportObject(View);
end;

initialization
  frRegisterExportFilter(TlrSpreadSheetExportFilter, 'Microsoft Excel (*.xls)', '*.xls');
  frRegisterExportFilter(TlrSpreadSheetExportFilter, 'Microsoft Excel 2007/2010 (*.xlsx)', '*.xlsx');
  frRegisterExportFilter(TlrSpreadSheetExportFilter, 'OpenOffice/LibreOffice (*.ods)', '*.ods');
end.

