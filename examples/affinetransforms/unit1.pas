unit Unit1; 

{$mode objfpc}{$H+}

interface

uses
  LCLType, LclIntf, Types,
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  StdCtrls, ExtCtrls, CheckLst, Grids, Menus, sqldb, Themes;

type

  { TForm1 }

  TForm1 = class(TForm)
    Label1: TLabel;
    Label2: TLabel;
    PB: TPaintBox;
    RG: TRadioGroup;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure PBPaint(Sender: TObject);
    procedure RGClick(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
    FImage: TBitmap;
  end; 

var
  Form1: TForm1; 

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
begin
  FImage := TBitmap.Create;
  FImage.LoadFromFile('./buy_64.bmp');
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  FImage.Free;
end;

procedure TForm1.PBPaint(Sender: TObject);
var
  R: TRect;
  P: TSize;
begin
  with PB.Canvas do
  begin
    Brush.Color := clWhite;
    FillRect(ClientRect);
    Font.Color := clGreen;
    Font.Height := 20;
    Pen.Width := 2;
    TextOut(20,300, 'Unmapped!'); // check if font size OK
    case RG.ItemIndex of
      0:
      begin
        SetMapMode(Handle, MM_ANISOTROPIC);
        SetWindowExtEx(Handle, 400, 300, nil);
        SetViewPortExtEx(Handle, PB.ClientWidth, PB.ClientHeight, nil);
        SetViewPortOrgEx(Handle, 0, 0, nil);
      end;
      1:
      begin
        SetMapMode(Handle, MM_ISOTROPIC);
        SetWindowExtEx(Handle, 400, 300, nil);
        SetViewPortExtEx(Handle, PB.ClientWidth, PB.ClientHeight, nil);
        SetViewPortOrgEx(Handle, 0, 0, nil);
      end;
      2:
      begin
        SetMapMode(Handle, MM_LOENGLISH);
        SetViewPortOrgEx(Handle, 0, 300, nil);
      end;
      3:
      begin
        SetMapMode(Handle, MM_HIENGLISH);
        SetViewPortOrgEx(Handle, 0, 300, nil);
      end;
      4:
      begin
        SetMapMode(Handle, MM_LOMETRIC);
        SetViewPortOrgEx(Handle, 0, 300, nil);
      end;
      5:
      begin
        SetMapMode(Handle, MM_HIMETRIC);
        SetViewPortOrgEx(Handle, 0, 300, nil);
      end;
      6:
      begin
        SetMapMode(Handle, MM_TWIPS);
        SetViewPortOrgEx(Handle, 0, 300, nil);
      end;
      7:
      begin
        SetMapMode(Handle, MM_ANISOTROPIC);
        SetWindowExtEx(Handle, -400, 300, nil);
        SetViewPortExtEx(Handle, PB.ClientWidth, PB.ClientHeight, nil);
        SetViewPortOrgEx(Handle, PB.ClientWidth, 0, nil);
      end;
    end;
    GetWindowExtEx(Handle, @P);
    GetViewPortExtEx(Handle, @P);
    Brush.Color := clRed;
    FillRect(10, 10, 200, 200);
    Brush.Color := clYellow;
    Arc(200,10,400,200,20,0,400,300);
    Chord(10,10,200,200, 0,0,300,300);
    Brush.Color := clGreen;
    Ellipse(50,200,250,260);
    Brush.Color := clWhite;
    TextOut(50,50, 'Hello!');
    TextRect(Rect(50,80,200,120), 50,80, 'Clipped!');
    DrawFocusRect(Rect(10,200,400,220)); // normally quite slow on GTK2, disable for the test
    MoveTo(300, 200);
    LineTo(390, 290);
    R := Rect(10,200,100,250);
    Frame3D(R, 3, bvRaised);
    Frame(200,250,250,260);
    FrameRect(200,220,250,240);
    Rectangle(320,20,380,100);
    RoundRect(320,110,380,150,6,6);
    ThemeServices.DrawElement(Handle, ThemeServices.GetElementDetails(tbPushButtonNormal), Rect(80,50,100,75), nil);
    DrawFrameControl(Handle, Rect(50,50,70,75), DFC_BUTTON, DFCS_BUTTONPUSH);
    R := Rect(110,50,140,75);
    DrawEdge(Handle, R, BDR_RAISEDINNER, BF_RECT);
    StretchDraw(Rect(230,100, 304, 164), FImage);
    SetMapMode(Handle, MM_TEXT);
    TextOut(20,20, 'Unmapped!'); // check if font size OK
  end;
end;

procedure TForm1.RGClick(Sender: TObject);
begin
  PB.Invalidate;
end;

end.

