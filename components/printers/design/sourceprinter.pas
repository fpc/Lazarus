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
  to the Free Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
  MA 02111-1307, USA.
}

unit SourcePrinter;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Printers, Graphics, PrintersDlgs, ExtCtrls, GraphType;

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
  LineNum: integer;
begin
  //print all lines on the current page
  for l := 1 to LinesPerPage do
  begin
    LineNum := Pred(PageNum) * LinesPerPage + l;

    //check if end of text is reached
    if LineNum <= Text.Count then
    begin
      if ShowLineNumbers then
        s := Format('%4d: ',[LineNum])
      else
        s := '      ';

      s := s + Text[Pred(LineNum)];

      Printer.Canvas.TextOut(Margin, Round(LineHeight * l), s);
    end;
  end;
end;

procedure TSourcePrinter.Execute(Text: TStrings);
var
  p: integer;
begin
  if PrintDialog.Execute then
  begin
    Printer.Title := 'Printers4LazIDE: Source Code Printer Package';
    Printer.BeginDoc;
    Printer.Canvas.Font := FFont;

    //calculate page dimensions
    LineHeight := Printer.Canvas.TextHeight('X') * 1.2;
    LinesPerPage := Round(Printer.PageHeight / LineHeight - 3);

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

