unit systemcolorstest;

{$mode objfpc}

interface

uses
  Classes, SysUtils, types, FileUtil, LResources, Forms, Controls, Graphics,
  Dialogs, LCLType, LCLIntf;

type

  { TfrmSystemColors }

  TfrmSystemColors = class(TForm)
    procedure FormPaint(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  frmSystemColors: TfrmSystemColors;

implementation

{ TfrmSystemColors }

procedure TfrmSystemColors.FormPaint(Sender: TObject);
var
  lColor: types.DWORD;
begin
  Canvas.Brush.Style := bsSolid;

  lColor := LCLIntf.GetSysColor(COLOR_SCROLLBAR);
  Canvas.Brush.Color := lColor;
  Canvas.TextOut(20, 20, Format('COLOR_SCROLLBAR=%x', [lColor]));

  lColor := LCLIntf.GetSysColor(COLOR_BACKGROUND);
  Canvas.Brush.Color := lColor;
  Canvas.TextOut(20, 40, Format('COLOR_BACKGROUND=%x', [lColor]));

  lColor := LCLIntf.GetSysColor(COLOR_ACTIVECAPTION);
  Canvas.Brush.Color := lColor;
  Canvas.TextOut(20, 60, Format('COLOR_ACTIVECAPTION=%x', [lColor]));

  lColor := LCLIntf.GetSysColor(COLOR_INACTIVECAPTION);
  Canvas.Brush.Color := lColor;
  Canvas.TextOut(20, 80, Format('COLOR_INACTIVECAPTION=%x', [lColor]));

  lColor := LCLIntf.GetSysColor(COLOR_MENU);
  Canvas.Brush.Color := lColor;
  Canvas.TextOut(20, 100, Format('COLOR_MENU=%x', [lColor]));

  lColor := LCLIntf.GetSysColor(COLOR_WINDOW);
  Canvas.Brush.Color := lColor;
  Canvas.TextOut(20, 120, Format('COLOR_WINDOW=%x', [lColor]));

  lColor := LCLIntf.GetSysColor(COLOR_WINDOWFRAME);
  Canvas.Brush.Color := lColor;
  Canvas.TextOut(20, 140, Format('COLOR_WINDOWFRAME=%x', [lColor]));

  Canvas.Pen.Color := clWhite;
  lColor := LCLIntf.GetSysColor(COLOR_MENUTEXT);
  Canvas.Brush.Color := lColor;
  Canvas.TextOut(20, 160, Format('COLOR_MENUTEXT=%x', [lColor]));

  Canvas.Pen.Color := clBlack;
  lColor := LCLIntf.GetSysColor(COLOR_WINDOWTEXT);
  Canvas.Brush.Color := lColor;
  Canvas.TextOut(20, 180, Format('COLOR_WINDOWTEXT=%x', [lColor]));

  lColor := LCLIntf.GetSysColor(COLOR_CAPTIONTEXT);
  Canvas.Brush.Color := lColor;
  Canvas.TextOut(20, 200, Format('COLOR_CAPTIONTEXT=%x', [lColor]));

  lColor := LCLIntf.GetSysColor(COLOR_ACTIVEBORDER);
  Canvas.Brush.Color := lColor;
  Canvas.TextOut(20, 220, Format('COLOR_ACTIVEBORDER=%x', [lColor]));

  lColor := LCLIntf.GetSysColor(COLOR_INACTIVEBORDER);
  Canvas.Brush.Color := lColor;
  Canvas.TextOut(20, 240, Format('COLOR_INACTIVEBORDER=%x', [lColor]));

  lColor := LCLIntf.GetSysColor(COLOR_APPWORKSPACE);
  Canvas.Brush.Color := lColor;
  Canvas.TextOut(20, 260, Format('COLOR_APPWORKSPACE=%x', [lColor]));

  lColor := LCLIntf.GetSysColor(COLOR_HIGHLIGHT);
  Canvas.Brush.Color := lColor;
  Canvas.TextOut(20, 280, Format('COLOR_HIGHLIGHT=%x', [lColor]));

  lColor := LCLIntf.GetSysColor(COLOR_HIGHLIGHTTEXT);
  Canvas.Brush.Color := lColor;
  Canvas.TextOut(20, 300, Format('COLOR_HIGHLIGHTTEXT=%x', [lColor]));

  // Second column

  lColor := LCLIntf.GetSysColor(COLOR_BTNFACE);
  Canvas.Brush.Color := lColor;
  Canvas.TextOut(240, 20, Format('COLOR_BTNFACE=%x', [lColor]));

  lColor := LCLIntf.GetSysColor(COLOR_BTNSHADOW);
  Canvas.Brush.Color := lColor;
  Canvas.TextOut(240, 40, Format('COLOR_BTNSHADOW=%x', [lColor]));

  lColor := LCLIntf.GetSysColor(COLOR_GRAYTEXT);
  Canvas.Brush.Color := lColor;
  Canvas.TextOut(240, 60, Format('COLOR_GRAYTEXT=%x', [lColor]));

  Canvas.Pen.Color := clWhite;
  lColor := LCLIntf.GetSysColor(COLOR_BTNTEXT);
  Canvas.Brush.Color := lColor;
  Canvas.TextOut(240, 80, Format('COLOR_BTNTEXT=%x', [lColor]));

  lColor := LCLIntf.GetSysColor(COLOR_INACTIVECAPTIONTEXT);
  Canvas.Brush.Color := lColor;
  Canvas.TextOut(240, 100, Format('COLOR_INACTIVECAPTIONTEXT=%x', [lColor]));

  Canvas.Pen.Color := clBlack;
  lColor := LCLIntf.GetSysColor(COLOR_BTNHIGHLIGHT);
  Canvas.Brush.Color := lColor;
  Canvas.TextOut(240, 120, Format('COLOR_BTNHIGHLIGHT=%x', [lColor]));

  Canvas.Pen.Color := clWhite;
  lColor := LCLIntf.GetSysColor(COLOR_3DDKSHADOW);
  Canvas.Brush.Color := lColor;
  Canvas.TextOut(240, 140, Format('COLOR_3DDKSHADOW=%x', [lColor]));

  Canvas.Pen.Color := clBlack;
  lColor := LCLIntf.GetSysColor(COLOR_3DLIGHT);
  Canvas.Brush.Color := lColor;
  Canvas.TextOut(240, 160, Format('COLOR_3DLIGHT=%x', [lColor]));

  Canvas.Pen.Color := clWhite;
  lColor := LCLIntf.GetSysColor(COLOR_INFOTEXT);
  Canvas.Brush.Color := lColor;
  Canvas.TextOut(240, 180, Format('COLOR_INFOTEXT=%x', [lColor]));

  {COLOR_INFOBK = 24;
  // PBD: 25 is unassigned in all the docs I can find
  //      if someone finds what this is supposed to be then fill it in
  //      note defaults below, and cl[ColorConst] in graphics
  COLOR_HOTLIGHT = 26;
  COLOR_GRADIENTACTIVECAPTION = 27;
  COLOR_GRADIENTINACTIVECAPTION = 28;
  COLOR_MENUHILIGHT = 29;
  COLOR_MENUBAR = 30;

  COLOR_FORM = 31;}
end;

initialization
  {$I systemcolorstest.lrs}

end.

