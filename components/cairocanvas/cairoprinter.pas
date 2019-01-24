{ CairoPrinter: A unit for easy access to CairoPrinter backends

  Copyright (C) 2013 Jesús Reyes A. (jesusrmx@yahoo.com.mx)

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

unit CairoPrinter;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, CairoCanvas;

type
  ECairoPrinterException = class(Exception);

  TCairoBackend = (cbPDF, cbPS, cbSVG, cbPNG);
  TCairoStatusItem = (csDoc);
  TCairoStatus = set of TCairoStatusItem;
  TCairoPrinterOption = (coAddExtension);
  TCairoPrinterOptions = set of TCairoPrinterOption;

  { TCairoFilePrinter }

  TCairoFilePrinter = class
  private

    fCairoCanvas: TCairoFileCanvas;
    fCairoBackend: TCairoBackend;
    fFileName: string;
    fStatus: TCairoStatus;
    fStream: TStream;
    fYDPI,fXDPI: Integer;
    fOptions: TCairoPrinterOptions;
    function GetCanvas: TCanvas;
    function GetPaperHeight: Integer;
    function GetPaperWidth: Integer;
    procedure SetCairoBackend(AValue: TCairoBackend);
    procedure SetFilename(AValue: string);
    procedure SetOptions(AValue: TCairoPrinterOptions);
    procedure SetPaperHeight(AValue: Integer);
    procedure SetPaperWidth(AValue: Integer);
    procedure SetStream(AValue: TStream);
    procedure SetXDPI(AValue: Integer);
    procedure SetYDPI(AValue: Integer);

    procedure CheckDoc(Prop: string);

  public

    constructor create;

    procedure BeginDoc;
    procedure EndDoc;
    procedure NewPage;

    property FileName: string read fFileName write SetFilename;

    property CairoBackend: TCairoBackend read fCairoBackend write SetCairoBackend;
    property XDPI: Integer read fXDPI write SetXDPI;
    property YDPI: Integer read fYDPI write SetYDPI;
    property Canvas: TCanvas read GetCanvas;
    property PaperWidth: Integer read GetPaperWidth write SetPaperWidth;
    property PaperHeight: Integer read GetPaperHeight write SetPaperHeight;
    property Options: TCairoPrinterOptions read fOptions write SetOptions;
    property Stream: TStream read fStream write SetStream;
  end;

implementation

function TCairoFilePrinter.GetCanvas: TCanvas;
begin
  result := fCairoCanvas;
end;

function TCairoFilePrinter.GetPaperHeight: Integer;
begin
  result := fCairoCanvas.PageHeight;
end;

function TCairoFilePrinter.GetPaperWidth: Integer;
begin
  result := fCairocanvas.PaperWidth;
end;

procedure TCairoFilePrinter.SetCairoBackend(AValue: TCairoBackend);
begin
  if fCairoBackend=AValue then
    Exit;
  CheckDoc('CairoBackend');
  fCairoBackend := AValue;
end;

procedure TCairoFilePrinter.SetFilename(AValue: string);
begin
  if fFileName=AValue then
    Exit;
  CheckDoc('Filename');
  fFileName:=AValue;
end;

procedure TCairoFilePrinter.SetOptions(AValue: TCairoPrinterOptions);
begin
  if fOptions=AValue then
    Exit;
  CheckDoc('Options');
  fOptions:=AValue;
end;

procedure TCairoFilePrinter.SetPaperHeight(AValue: Integer);
begin
  if AValue=fCairoCanvas.PaperHeight then
    exit;
  fCairoCanvas.PaperHeight:=AValue;
  fCairoCanvas.UpdatePageSize;
end;

procedure TCairoFilePrinter.SetPaperWidth(AValue: Integer);
begin
  if Avalue=fCairoCanvas.PaperWidth then
    exit;
  fCairocanvas.PaperWidth:=AValue;
  fCairoCanvas.UpdatePageSize;
end;

procedure TCairoFilePrinter.SetStream(AValue: TStream);
begin
  if fStream=AValue then
    Exit;
  CheckDoc('Stream');
  fStream:=AValue;
end;

procedure TCairoFilePrinter.SetXDPI(AValue: Integer);
begin
  if fXDPI=AValue then
    exit;
  CheckDoc('XDPI');
  fXDPI := AValue;
end;

procedure TCairoFilePrinter.SetYDPI(AValue: Integer);
begin
  if YDPI=AValue then
    exit;
  CheckDoc('YDPI');
  fYDPI := AValue;
end;

procedure TCairoFilePrinter.CheckDoc(Prop: string);
begin
  if csDoc in fStatus then
    raise ECairoPrinterException.CreateFmt('Error: Cannot modify %s, doc has started', [Prop]);
end;

constructor TCairoFilePrinter.create;
begin
  inherited create;

  fXDPI := 300;
  fYDPI := 300;
  fOptions := [coAddExtension];
end;

procedure TCairoFilePrinter.BeginDoc;
var
  s: string;
begin

  if (fStream=nil) and (fFileName='') then
    raise ECairoPrinterException.Create('Error: Filename not specified');

  case fCairoBackend of
    cbPDF   : fCairoCanvas := TCairoPdfCanvas.Create(nil);
    cbPS    : fCairoCanvas := TCairoPSCanvas.Create(nil);
    cbSVG   : fCairoCanvas := TCairoSVGCanvas.Create(nil);
    cbPNG   : fCairoCanvas := TCairoPNGCanvas.Create(nil);
  end;

  s := fFileName;
  if (coAddExtension in fOptions) and (ExtractFileExt(s)='') then
    case fCairoBackend of
      cbPDF   : s := s + '.pdf';
      cbPS    : s := s + '.ps';
      cbSVG   : s := s + '.svg';
      cbPNG   : s := s + '.png';
    end;

  if fStream<>nil then
    fCairoCanvas.Stream := fStream
  else
    fCairoCanvas.OutputFileName:=s;

  Include(fStatus, csDoc);

  fCairoCanvas.XDPI:=fXDPI;
  fCairoCanvas.YDPI:=fYDPI;
  fCairoCanvas.BeginDoc;
end;

procedure TCairoFilePrinter.EndDoc;
begin
  fCairoCanvas.EndDoc;
  Exclude(fStatus, csDoc);
  fCairoCanvas.Free;
  fCairoCanvas:=nil;
end;

procedure TCairoFilePrinter.NewPage;
begin
  fCairoCanvas.NewPage;
end;


end.

