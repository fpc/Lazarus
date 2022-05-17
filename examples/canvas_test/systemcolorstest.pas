unit systemcolorstest;

{$mode objfpc}

interface

uses
  SysUtils, types, LResources, Forms, Graphics, LCLType, LCLIntf;

type

  { TfrmSystemColors }

  TfrmSystemColors = class(TForm)
    procedure FormPaint(Sender: TObject);
  private

  public

  end;

var
  frmSystemColors: TfrmSystemColors;

implementation

{ TfrmSystemColors }

procedure TfrmSystemColors.FormPaint(Sender: TObject);
var
  lColor: types.DWORD;

  procedure SetColors;
  begin
    if lColor < $111111 then Canvas.Font.Color := clWhite
    else Canvas.Font.Color := clBlack;
    Canvas.Brush.Color := lColor;
  end;

begin
  Canvas.Brush.Style := bsSolid;
  Canvas.Font.Color := clBlack;

  lColor := LCLIntf.GetSysColor(COLOR_SCROLLBAR);
  SetColors();
  Canvas.TextOut(20, 20, Format('COLOR_SCROLLBAR=%x', [lColor]));

  lColor := LCLIntf.GetSysColor(COLOR_BACKGROUND);
  SetColors();
  Canvas.TextOut(20, 40, Format('COLOR_BACKGROUND=%x', [lColor]));

  lColor := LCLIntf.GetSysColor(COLOR_ACTIVECAPTION);
  SetColors();
  Canvas.TextOut(20, 60, Format('COLOR_ACTIVECAPTION=%x', [lColor]));

  lColor := LCLIntf.GetSysColor(COLOR_INACTIVECAPTION);
  SetColors();
  Canvas.TextOut(20, 80, Format('COLOR_INACTIVECAPTION=%x', [lColor]));

  lColor := LCLIntf.GetSysColor(COLOR_MENU);
  SetColors();
  Canvas.TextOut(20, 100, Format('COLOR_MENU=%x', [lColor]));

  lColor := LCLIntf.GetSysColor(COLOR_WINDOW);
  SetColors();
  Canvas.TextOut(20, 120, Format('COLOR_WINDOW=%x', [lColor]));

  lColor := LCLIntf.GetSysColor(COLOR_WINDOWFRAME);
  SetColors();
  Canvas.TextOut(20, 140, Format('COLOR_WINDOWFRAME=%x', [lColor]));

  lColor := LCLIntf.GetSysColor(COLOR_MENUTEXT);
  SetColors();
  Canvas.TextOut(20, 160, Format('COLOR_MENUTEXT=%x', [lColor]));

  lColor := LCLIntf.GetSysColor(COLOR_WINDOWTEXT);
  SetColors();
  Canvas.TextOut(20, 180, Format('COLOR_WINDOWTEXT=%x', [lColor]));

  lColor := LCLIntf.GetSysColor(COLOR_CAPTIONTEXT);
  SetColors();
  Canvas.TextOut(20, 200, Format('COLOR_CAPTIONTEXT=%x', [lColor]));

  lColor := LCLIntf.GetSysColor(COLOR_ACTIVEBORDER);
  SetColors();
  Canvas.TextOut(20, 220, Format('COLOR_ACTIVEBORDER=%x', [lColor]));

  lColor := LCLIntf.GetSysColor(COLOR_INACTIVEBORDER);
  SetColors();
  Canvas.TextOut(20, 240, Format('COLOR_INACTIVEBORDER=%x', [lColor]));

  lColor := LCLIntf.GetSysColor(COLOR_APPWORKSPACE);
  SetColors();
  Canvas.TextOut(20, 260, Format('COLOR_APPWORKSPACE=%x', [lColor]));

  lColor := LCLIntf.GetSysColor(COLOR_HIGHLIGHT);
  SetColors();
  Canvas.TextOut(20, 280, Format('COLOR_HIGHLIGHT=%x', [lColor]));

  lColor := LCLIntf.GetSysColor(COLOR_HIGHLIGHTTEXT);
  SetColors();
  Canvas.TextOut(20, 300, Format('COLOR_HIGHLIGHTTEXT=%x', [lColor]));

  // Second column

  lColor := LCLIntf.GetSysColor(COLOR_BTNFACE);
  SetColors();
  Canvas.TextOut(240, 20, Format('COLOR_BTNFACE=%x', [lColor]));

  lColor := LCLIntf.GetSysColor(COLOR_BTNSHADOW);
  SetColors();
  Canvas.TextOut(240, 40, Format('COLOR_BTNSHADOW=%x', [lColor]));

  lColor := LCLIntf.GetSysColor(COLOR_GRAYTEXT);
  SetColors();
  Canvas.TextOut(240, 60, Format('COLOR_GRAYTEXT=%x', [lColor]));

  lColor := LCLIntf.GetSysColor(COLOR_BTNTEXT);
  SetColors();
  Canvas.TextOut(240, 80, Format('COLOR_BTNTEXT=%x', [lColor]));

  lColor := LCLIntf.GetSysColor(COLOR_INACTIVECAPTIONTEXT);
  SetColors();
  Canvas.TextOut(240, 100, Format('COLOR_INACTIVECAPTIONTEXT=%x', [lColor]));

  lColor := LCLIntf.GetSysColor(COLOR_BTNHIGHLIGHT);
  SetColors();
  Canvas.TextOut(240, 120, Format('COLOR_BTNHIGHLIGHT=%x', [lColor]));

  lColor := LCLIntf.GetSysColor(COLOR_3DDKSHADOW);
  SetColors();
  Canvas.TextOut(240, 140, Format('COLOR_3DDKSHADOW=%x', [lColor]));

  lColor := LCLIntf.GetSysColor(COLOR_3DLIGHT);
  SetColors();
  Canvas.TextOut(240, 160, Format('COLOR_3DLIGHT=%x', [lColor]));

  lColor := LCLIntf.GetSysColor(COLOR_INFOTEXT);
  SetColors();
  Canvas.TextOut(240, 180, Format('COLOR_INFOTEXT=%x', [lColor]));

  lColor := LCLIntf.GetSysColor(COLOR_INFOBK);
  SetColors();
  Canvas.TextOut(240, 200, Format('COLOR_INFOBK=%x', [lColor]));

  lColor := LCLIntf.GetSysColor(COLOR_HOTLIGHT);
  SetColors();
  Canvas.TextOut(240, 220, Format('COLOR_HOTLIGHT=%x', [lColor]));

  lColor := LCLIntf.GetSysColor(COLOR_MENUHILIGHT);
  SetColors();
  Canvas.TextOut(240, 240, Format('COLOR_MENUHILIGHT=%x', [lColor]));

  lColor := LCLIntf.GetSysColor(COLOR_FORM);
  SetColors();
  Canvas.TextOut(240, 260, Format('COLOR_MENUBAR=%x', [lColor]));

  lColor := LCLIntf.GetSysColor(COLOR_FORM);
  SetColors();
  Canvas.TextOut(240, 280, Format('COLOR_FORM=%x', [lColor]));
end;

initialization
  {$I systemcolorstest.lrs}

end.

