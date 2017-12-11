{ Copyright (C) 2006 Darius Blaszijk

  This source is free software; you can redistribute it and/or modify it under
  the terms of the GNU General Public License as published by the Free
  Software Foundation; either version 2 of the License, or (at your option)
  any later version.

  This code is distributed in the hope that it will be useful, but WITHOUT ANY
  WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
  FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
  details.

  A copy of the GNU General Public License is available on the World Wide Web
  at <http://www.gnu.org/copyleft/gpl.html>. You can also obtain it by writing
  to the Free Software Foundation, Inc., 51 Franklin Street - Fifth Floor,
  Boston, MA 02110-1335, USA.
}

unit SourcePrinter;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Printers, Graphics, PrintersDlgs, ExtCtrls, GraphType, LazUTF8;

type
  TSourcePrinter = class(TObject)
  private
    FFont: TFont;
    FShowLineNumbers: boolean;
    LineHeight: double;
    LinesPerPage: integer;
    FMargin: integer;
    PageCount: integer;
    PrintDialog: TPrintDialog;

    procedure PrintPage(Text: TStrings; PageNum: integer);
  public
    constructor Create;
    destructor Destroy; override;

    procedure Execute(Text: TStrings);
  published
    property Font: TFont read FFont write FFont;
    property ShowLineNumbers: boolean read FShowLineNumbers write FShowLineNumbers;
    property Margin: integer read FMargin write FMargin;
  end;

implementation

constructor TSourcePrinter.Create;
begin
  FFont := TFont.Create;
  FFont.Name := 'Courier New';
  FFont.Size := 10;
  FFont.Color := clBlack;
  PrintDialog := TPrintDialog.Create(nil);
  ShowLineNumbers := True;
  {$ifdef Linux}
    Margin := 30;
  {$else}
    Margin := 0;
  {$endif}
end;

destructor TSourcePrinter.Destroy;
begin
  FFont.Free;
  PrintDialog.Free;
  inherited Destroy;
end;

procedure TSourcePrinter.PrintPage(Text: TStrings; PageNum: integer);
var
  l: integer;
  s: string;
  LineNum, PrintNum: integer;
begin
  //print all lines on the current page
  for l := 0 to LinesPerPage - 1 do
  begin
    LineNum := Pred(PageNum) * LinesPerPage + l;

    //check if end of text is reached
    if LineNum < Text.Count then
    begin
      PrintNum := PtrUInt(Text.Objects[LineNum]);
      if ShowLineNumbers then begin
        if (PrintNum > 0) then
          s := Format('%4d: ',[PrintNum])
        else
          s := '      ';
        end
      else
        s := '';

      s := s + Text[LineNum];

      Printer.Canvas.TextOut(Margin, Round(LineHeight * l), s);
    end;
  end;
end;

procedure TSourcePrinter.Execute(Text: TStrings);
const
  MIN_LINE_LEN = 10; // Minimum 1
var
  p: integer;
  i, j, l, l2: Integer;
  s, s2: String;
begin
  if PrintDialog.Execute then
  begin
    Printer.Title := 'Printers4LazIDE: Source Code Printer Package';
    Printer.BeginDoc;
    Printer.Canvas.Font := FFont;

    //calculate page dimensions
    LineHeight := Printer.Canvas.TextHeight('X') * 1.2;
    LinesPerPage := Round(Printer.PageHeight / LineHeight - 3);

    // break long lines
    i := 1;
    j := 0;
    s2 := '';
    while j < Text.Count do begin
      Text.Objects[j] := TObject(PtrUInt(i));
      s := Text[j];
      if ShowLineNumbers then s2 := Format('%4d: ',[i]);
      l := Printer.Canvas.TextFitInfo(s2 + s, Printer.PageWidth - 2 * Margin);
      l := l - Length(s2); // s2 has only single byte
      l := UTF8CodepointToByteIndex(PChar(s), length(s), l);
      while (l > MIN_LINE_LEN) and (l < length(s)) do begin
        l2 := l;
        while (l2 > MIN_LINE_LEN) and
              (s[l2] in ['a'..'z', 'A'..'Z', '_', '0'..'1', '#', '$', '%']) and
              (s[l2+1] in ['a'..'z', 'A'..'Z', '_', '0'..'1', '#', '$', '%'])
        do
          dec(l2);
        if l2 <= MIN_LINE_LEN then
          l2 := l;
        // find utf8 start
        while (l2 > 1) and (ord(s[l2]) >= 128) and (ord(s[l2+1]) >= 128) and (ord(s[l2+1]) < 192) do
          dec(l2);
        if l2 = 0 then l2 := UTF8CodepointToByteIndex(PChar(s), length(s), MIN_LINE_LEN);
        Text[j] := copy(s, 1, l2);
        delete(s, 1, l2);
        inc(j);
        Text.InsertObject(j, '', nil);
        l := Printer.Canvas.TextFitInfo(s2 + s, Printer.PageWidth - 2 * Margin);
        l := l - Length(s2);
        l := UTF8CodepointToByteIndex(PChar(s), length(s), l);
      end;
      Text[j] := s;
      inc(i);
      inc(j);
    end;


    PageCount := Text.Count div LinesPerPage;
    if Text.Count mod LinesPerPage <> 0 then
      Inc(PageCount);
    
    try
      //print each page
      for p := 1 to PageCount do
      begin
        PrintPage(Text, p);

        //create a new page
        if p <> PageCount then
          Printer.NewPage;
      end;
      
      Printer.EndDoc;
    except
      on E:Exception do
      begin
        Printer.Abort;
        raise Exception.Create(e.message);
      end;
    end;
  end;
end;

end.

