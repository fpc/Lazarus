program cairoprog;

{$mode objfpc}{$H+}

uses
  Classes, Printers, Graphics, CairoCanvas;

var // This could also be TCairoSvgCanvas, TCairoPngCanvas or TCairoPsCanvas
  PrinterPDFCanvas: TCairoPdfCanvas;
  PrinterPSCanvas : TCairoPSCanvas;
  ImgSVG          : TCairoSvgCanvas;
  ImgPNG          : TCairoPngCanvas;
begin
  PrinterPDFCanvas := TCairoPdfCanvas.Create;
  try
    PrinterPDFCanvas.OutputFileName := 'CairoCanvas.pdf'; // OutputFileName must set before BeginDoc; Otherwise use stream property
    PrinterPDFCanvas.BeginDoc;
    // Font properties must be set, does not work otherwise.
    {$IFDEF MSWindows}
    PrinterPDFCanvas.Font.Name := 'Arial Unicode MS';
    {$ELSE}
    PrinterPDFCanvas.Font.Name := 'Arial'; // Font properties must be set, does not work otherwise.
    {$ENDIF}

    PrinterPDFCanvas.Brush.Color:=clYellow;
    PrinterPDFCanvas.Font.Height := 24;

    PrinterPDFCanvas.Ellipse(10, 20, 260, 120);

    PrinterPDFCanvas.TextOut(50, 60, 'Portrait');
    PrinterPDFCanvas.TextOut(50, 150, 'abcdefghijklmnopqrstuvwxyzåäö');
    PrinterPDFCanvas.TextOut(50, 220, 'ฉันหิวแล้ว');
    PrinterPDFCanvas.TextOut(50, 290, 'К нам в око́шко застучи́т');
    PrinterPDFCanvas.TextOut(50, 360, 'لا أتَكَلّمُ الْعَرَبيّة');

    PrinterPDFCanvas.Orientation:=poLandscape; //Orientation must set before new page
    PrinterPDFCanvas.NewPage;
    PrinterPDFCanvas.Ellipse(10, 20, 260, 120);
    PrinterPDFCanvas.Font.Color:=clnavy;
    PrinterPDFCanvas.TextOut(50, 60, 'Landscape');
    PrinterPDFCanvas.Font.Color:=clBlack;
    PrinterPDFCanvas.TextOut(50, 150, 'Finnish: wxyzåäö1234567890');
    PrinterPDFCanvas.TextOut(50, 220, 'Thai: ฉันหิวแล้ว');
    PrinterPDFCanvas.TextOut(50, 290, 'Russian: К нам в око́шко застучи́т');
    PrinterPDFCanvas.TextOut(50, 360, 'Arabic: لا أتَكَلّمُ الْعَرَبيّة');

    WriteLn('Written file ' + PrinterPDFCanvas.OutputFileName);
    PrinterPDFCanvas.EndDoc;
  finally
    PrinterPDFCanvas.Free;
  end;

  PrinterPSCanvas := TCairoPSCanvas.Create;
  try
    PrinterPDFCanvas.OutputFileName := 'CairoCanvas.ps'; // OutputFileName must set before BeginDoc; Otherwise use stream property
    PrinterPSCanvas.BeginDoc;
    // Font properties must be set, does not work otherwise.
    {$IFDEF MSWindows}
    PrinterPSCanvas.Font.Name := 'Arial Unicode MS';
    {$ELSE}
    PrinterPSCanvas.Font.Name := 'Arial'; // Font properties must be set, does not work otherwise.
    {$ENDIF}
    PrinterPSCanvas.Brush.Color:=clYellow;
    PrinterPSCanvas.Font.Height := 24;

    PrinterPSCanvas.Ellipse(10, 20, 260, 120);

    PrinterPSCanvas.TextOut(50, 60, 'Portrait');
    PrinterPSCanvas.TextOut(50, 150, 'abcdefghijklmnopqrstuvwxyzåäö');
    PrinterPSCanvas.TextOut(50, 220, 'ฉันหิวแล้ว');
    PrinterPSCanvas.TextOut(50, 290, 'К нам в око́шко застучи́т');
    PrinterPSCanvas.TextOut(50, 360, 'لا أتَكَلّمُ الْعَرَبيّة');

    PrinterPSCanvas.Orientation:=poLandscape; //Orientation must set before new page
    PrinterPSCanvas.NewPage;
    PrinterPSCanvas.Orientation:=poLandscape; //Orientation must set before new page
    PrinterPSCanvas.Ellipse(10, 20, 260, 120);
    PrinterPSCanvas.Font.Color:=clnavy;
    PrinterPSCanvas.TextOut(50, 60, 'Landscape');
    PrinterPSCanvas.Font.Color:=clBlack;
    PrinterPSCanvas.TextOut(50, 150, 'Finnish: wxyzåäö1234567890');
    PrinterPSCanvas.TextOut(50, 220, 'Thai: ฉันหิวแล้ว');
    PrinterPSCanvas.TextOut(50, 290, 'Russian: К нам в око́шко застучи́т');
    PrinterPSCanvas.TextOut(50, 360, 'Arabic: لا أتَكَلّمُ الْعَرَبيّة');

    WriteLn('Written file ' + PrinterPSCanvas.OutputFileName);
    PrinterPSCanvas.EndDoc;
  finally
    PrinterPSCanvas.Free;
  end;

  ImgSVG := TCairoSvgCanvas.Create;
  try
    ImgSVG.OutputFileName := 'CairoCanvas.svg'; // OutputFileName must set before BeginDoc; Otherwise use stream property
    ImgSVG.BeginDoc;
    // Font properties must be set, does not work otherwise.
    {$IFDEF MSWindows}
    ImgSVG.Font.Name := 'Arial Unicode MS';
    {$ELSE}
    ImgSVG.Font.Name := 'Arial'; // Font properties must be set, does not work otherwise.
    {$ENDIF}
    ImgSVG.Brush.Color:=clYellow;
    ImgSVG.Font.Height := 24;

    ImgSVG.Orientation:=poLandscape; //Orientation must set before new page
    ImgSVG.NewPage;
    ImgSVG.Ellipse(10, 20, 260, 120);
    ImgSVG.Font.Color:=clnavy;
    ImgSVG.TextOut(50, 60, 'Landscape');
    ImgSVG.Font.Color:=clBlack;
    ImgSVG.TextOut(50, 150, 'Finnish: wxyzåäö1234567890');
    ImgSVG.TextOut(50, 220, 'Thai: ฉันหิวแล้ว');
    ImgSVG.TextOut(50, 290, 'Russian: К нам в око́шко застучи́т');
    ImgSVG.TextOut(50, 360, 'Arabic: لا أتَكَلّمُ الْعَرَبيّة');

    ImgSVG.Orientation:=poLandscape; //Orientation must set before new page
    ImgSVG.NewPage;
    ImgSVG.Orientation:=poLandscape; //Orientation must set before new page
    ImgSVG.Ellipse(10, 20, 260, 120);
    ImgSVG.Font.Color:=clnavy;
    ImgSVG.TextOut(50, 60, 'Landscape');
    ImgSVG.Font.Color:=clBlack;
    ImgSVG.TextOut(50, 150, 'Finnish: wxyzåäö1234567890');
    ImgSVG.TextOut(50, 220, 'Thai: ฉันหิวแล้ว');
    ImgSVG.TextOut(50, 290, 'Russian: К нам в око́шко застучи́т');
    ImgSVG.TextOut(50, 360, 'Arabic: لا أتَكَلّمُ الْعَرَبيّة');

    WriteLn('Written file ' + ImgSVG.OutputFileName);
    ImgSVG.EndDoc;
  finally
    ImgSVG.Free;
  end;
  ImgPNG := TCairoPNGCanvas.Create;
  try
    ImgPNG.OutputFileName := 'CairoCanvas.png'; // OutputFileName must set before BeginDoc; Otherwise use stream property
    ImgPNG.BeginDoc;
    // Font properties must be set, does not work otherwise.
    {$IFDEF MSWindows}
    ImgPNG.Font.Name := 'Arial Unicode MS';
    {$ELSE}
    ImgPNG.Font.Name := 'Arial'; // Font properties must be set, does not work otherwise.
    {$ENDIF}
    ImgPNG.Brush.Color:=clYellow;
    ImgPNG.Font.Height := 24;

    ImgPNG.Orientation:=poLandscape; //Orientation must set before new page
    ImgPNG.NewPage;
    ImgPNG.Ellipse(10, 20, 260, 120);
    ImgPNG.Font.Color:=clnavy;
    ImgPNG.TextOut(50, 60, 'Landscape');
    ImgPNG.Font.Color:=clBlack;
    ImgPNG.TextOut(50, 150, 'Finnish: wxyzåäö1234567890');
    ImgPNG.TextOut(50, 220, 'Thai: ฉันหิวแล้ว');
    ImgPNG.TextOut(50, 290, 'Russian: К нам в око́шко застучи́т');
    ImgPNG.TextOut(50, 360, 'Arabic: لا أتَكَلّمُ الْعَرَبيّة');

    WriteLn('Written file ' + ImgPNG.OutputFileName);
    ImgPNG.EndDoc;
  finally
    ImgPNG.Free;
  end;

end.

