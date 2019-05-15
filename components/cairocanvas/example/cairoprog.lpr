program cairoprog;

{$mode objfpc}{$H+}

uses
  Classes, CairoCanvas;

var // This could also be TCairoSvgCanvas, TCairoPngCanvas or TCairoPsCanvas
  PrinterCanvas: TCairoPdfCanvas;
begin
  PrinterCanvas := TCairoPdfCanvas.Create;
  try
    //PrinterCanvas.XDPI := 75; // These have valid default values.
    //PrinterCanvas.YDPI := 75;
    //PrinterCanvas.PaperWidth := 250;
    //PrinterCanvas.PaperHeight := 500;
    PrinterCanvas.BeginDoc;
    PrinterCanvas.OutputFileName := 'CairoCanvas.pdf';
    PrinterCanvas.Ellipse(10, 20, 160, 120);

    // Font properties must be set, does not work otherwise.
    {$IFDEF MSWindows}
    PrinterCanvas.Font.Name := 'Arial Unicode MS';
    {$ELSE}
    PrinterCanvas.Font.Name := 'Arial'; // Font properties must be set, does not work otherwise.
    {$ENDIF}
    PrinterCanvas.Font.Height := 24;

    PrinterCanvas.TextOut(50, 60, 'Page 1');
    PrinterCanvas.TextOut(50, 150, 'abcdefghijklmnopqrstuvwxyzåäö');
    PrinterCanvas.TextOut(50, 220, 'ฉันหิวแล้ว');
    PrinterCanvas.TextOut(50, 290, 'К нам в око́шко застучи́т');
    PrinterCanvas.TextOut(50, 360, 'لا أتَكَلّمُ الْعَرَبيّة');
    PrinterCanvas.NewPage;
    PrinterCanvas.Ellipse(10, 20, 160, 120);
    PrinterCanvas.TextOut(50, 60, 'Page 2');
    PrinterCanvas.TextOut(50, 150, 'Finnish: wxyzåäö1234567890');
    PrinterCanvas.TextOut(50, 220, 'Thai: ฉันหิวแล้ว');
    PrinterCanvas.TextOut(50, 290, 'Russian: К нам в око́шко застучи́т');
    PrinterCanvas.TextOut(50, 360, 'Arabic: لا أتَكَلّمُ الْعَرَبيّة');
    WriteLn('Written file ' + PrinterCanvas.OutputFileName);
    PrinterCanvas.EndDoc;
  finally
    PrinterCanvas.Free;
  end;
end.

