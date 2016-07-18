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

unit lrSpreadSheetExp;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, le_e_spreadsheet_types, LR_Class;

type

  { TlrSpreadSheetExport }

  TlrSpreadSheetExport = class(TComponent)
  private
    FBandTypes: TfrBandTypes;
    FDataGrouping: TlreDataGrouping;
    FDataGroupingChunks: integer;
    FDeleteEmptyRow: boolean;
    FExportImages: boolean;
    FExportPrintRange: boolean;
    FExportURL: boolean;
    FMergeCell: boolean;
    FOpenAfterExport: boolean;
    FShowSetupForm: boolean;
    { Private declarations }
  protected
    { Protected declarations }
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property DataGrouping:TlreDataGrouping read FDataGrouping write FDataGrouping;
    property DataGroupingChunks:integer read FDataGroupingChunks write FDataGroupingChunks;
    property OpenAfterExport:boolean read FOpenAfterExport write FOpenAfterExport;
    property DeleteEmptyRow:boolean read FDeleteEmptyRow write FDeleteEmptyRow;
    property MergeCell:boolean read FMergeCell write FMergeCell;
    property ShowSetupForm:boolean read FShowSetupForm write FShowSetupForm;
    property ExportURL:boolean read FExportURL write FExportURL default false;
    property ExportPrintRange:boolean read FExportPrintRange write FExportPrintRange default false;
    property BandTypes:TfrBandTypes read FBandTypes write FBandTypes;
    property ExportImages:boolean read FExportImages write FExportImages;
  end;

procedure Register;

var
  lrSpreadSheetExportComponent:TlrSpreadSheetExport = nil;

implementation
uses le_e_spreadsheet, le_e_spreadsheet_consts;

{$R lrspreadsheetexp.res}

procedure Register;
begin
  RegisterComponents('LazReport',[TlrSpreadSheetExport]);
end;

{ TlrSpreadSheetExport }

constructor TlrSpreadSheetExport.Create(AOwner: TComponent);
begin
  if Assigned(lrSpreadSheetExportComponent) then
    raise Exception.Create(sOnlyOneComponent);
  inherited Create(AOwner);
  lrSpreadSheetExportComponent:=Self;

  FDataGrouping:=ldgLikeReport;
  FDataGroupingChunks:=50;
  FOpenAfterExport:=true;
  FMergeCell:=true;
  FDeleteEmptyRow:=false;
  FExportURL:=false;
  FExportPrintRange:=false;
  FShowSetupForm:=true;
  FBandTypes:=[btReportTitle..btNone];
end;

destructor TlrSpreadSheetExport.Destroy;
begin
  lrSpreadSheetExportComponent:=nil;
  inherited Destroy;
end;

finalization
  lrSpreadSheetExportComponent:=nil;
end.
