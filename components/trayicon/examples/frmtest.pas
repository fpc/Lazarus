unit frmtest;

{$ifdef fpc}
  {$mode delphi}{$H+}
{$endif}  

interface

uses
  Classes, SysUtils,
{$ifdef LCL}
  LResources,
{$endif}
  Forms, Controls, Graphics, Dialogs, Buttons, StdCtrls;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure HandleClick(Sender: TObject);
  private
    { private declarations }
    procedure DoPaint(Sender: TObject);
  public
    { public declarations }
  end; 

var
  Form1: TForm1; 

{$ifndef LCL}
  {$R frmtest.dfm}
{$endif}

implementation

uses
{$ifdef win32}
  Windows,
{$endif}
  TrayIcon;

{ TForm1 }

procedure TForm1.Button1Click(Sender: TObject);
begin
  SystrayIcon.Show;
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  SystrayIcon.Hide;
end;

procedure TForm1.FormCreate(Sender: TObject);
const
  IDI_ICON1         = 101;
  IDI_ICON2         = 115;
begin
{$ifdef win32}
  SystrayIcon.Icon.Handle := LoadIcon(hInstance, MAKEINTRESOURCE(IDI_ICON1));
{$else}
  SystrayIcon.Icon.LoadFromFile('icon.ico');
{$endif}

  SystrayIcon.ShowToolTip := True;
  SystrayIcon.ToolTip := 'my tool tip';

  SystrayIcon.OnClick := HandleClick;
  SystrayIcon.OnPaint := DoPaint;
end;

procedure TForm1.HandleClick(Sender: TObject);
begin
  Application.MessageBox('Text', 'Caption', 0);
end;

procedure TForm1.DoPaint(Sender: TObject);
var
  MyImage: TPixmap;
begin
  MyImage := TPixmap.Create;
  MyImage.LoadFromFile('icon.xpm');
  SystrayIcon.Canvas.Draw(0, 0, MyImage);
  MyImage.Free;
  WriteLn('Paint');
end;

initialization
{$ifdef LCL}
  {$I frmtest.lrs}
{$endif}

end.

