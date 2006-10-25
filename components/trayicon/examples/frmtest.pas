{
 frmtest.dpr

 *****************************************************************************
 *                                                                           *
 *  This demonstration program is public domain, which means no copyright,   *
 * but also no warranty!                                                     *
 *                                                                           *
 *  This program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
 *                                                                           *
 *****************************************************************************

 Author: Felipe Monteiro de Carvalho
}
unit frmtest;

{$ifdef fpc}
  {$mode delphi}{$H+}
{$endif}

interface

uses
  Classes, SysUtils,
{$ifdef fpc}
  LResources,
{$endif}
  Forms, Controls, Graphics, Dialogs, Buttons, StdCtrls, Menus, TrayIcon;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    PopupMenu: TPopupMenu;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
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

{$ifndef fpc}
  {$R frmtest.dfm}
{$endif}

implementation

{$ifdef Windows}
uses Windows;
{$endif}

{ TForm1 }

procedure TForm1.Button1Click(Sender: TObject);
begin
  SystrayIcon.Show;
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  SystrayIcon.Hide;
end;

procedure TForm1.Button3Click(Sender: TObject);
var
  MyImage, SecondImage: TIcon;
begin
  MyImage := TIcon.Create;
  SecondImage := TIcon.Create;

  MyImage.LoadFromFile('icon.ico');
  SecondImage.Height := 22;
  SecondImage.Width := 22;
  SecondImage.Canvas.Draw(0, 0, MyImage);
  Canvas.Draw(0, 0, SecondImage);

  SecondImage.Free;
  MyImage.Free;
end;

procedure TForm1.FormCreate(Sender: TObject);
const
  IDI_ICON1         = 101;
  IDI_ICON2         = 115;
begin
{$ifdef Windows}
  SystrayIcon.Icon.Handle := LoadIcon(hInstance, MAKEINTRESOURCE(IDI_ICON1));

//  Loading from a file should also work
//  SystrayIcon.Icon.LoadFromFile('icon.ico');
{$else}
  SystrayIcon.Icon.LoadFromFile('icon.ico');
{$endif}

  SystrayIcon.ShowHint := True;
  SystrayIcon.Hint := 'my tool tip';

  SystrayIcon.OnClick := HandleClick;
  
//  SystrayIcon.OnPaint := DoPaint;

  SystrayIcon.PopUpMenu := PopupMenu;
end;

procedure TForm1.HandleClick(Sender: TObject);
begin
  Application.MessageBox('Text', 'Caption', 0);
end;

procedure TForm1.DoPaint(Sender: TObject);
var
  MyImage: TIcon;
begin
  MyImage := TIcon.Create;
  MyImage.LoadFromFile('icon.ico');
  SystrayIcon.Canvas.Draw(0, 0, MyImage);
  MyImage.Free;
  WriteLn('Paint');
end;

initialization
{$ifdef fpc}
  {$I frmtest.lrs}
{$endif}

end.

