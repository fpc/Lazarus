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
  Forms, Controls, Graphics, Dialogs, Buttons, StdCtrls, Menus,
  ExtCtrls;

type

  { TfrmTrayTest }

  TfrmTrayTest = class(TForm)
    btnShow: TButton;
    btnHide: TButton;
    btnPaintTest: TButton;
    btnDisplayMessage: TButton;
    chkOnPaintDrawing: TCheckBox;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    PopupMenu: TPopupMenu;
    SystrayIcon: TTrayIcon;
    procedure btnShowClick(Sender: TObject);
    procedure btnHideClick(Sender: TObject);
    procedure btnPaintTestClick(Sender: TObject);
    procedure chkOnPaintDrawingChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormPaint(Sender: TObject);
    procedure HandleClick(Sender: TObject);
  private
    { private declarations }
    pathMedia: string;
    MyImage: TIcon;
    procedure DoPaint(Sender: TObject);
  public
    { public declarations }
  end;

var
  frmTrayTest: TfrmTrayTest;

{$ifndef fpc}
  {$R frmtest.dfm}
{$endif}

implementation

{$ifdef Windows}
uses Windows;
{$endif}
{$IFDEF Darwin}
uses
{$ifdef ver2_2_0}
  FPCMacOSAll;
{$else}
  MacOSAll;
{$endif}
{$ENDIF}

{ TfrmTrayTest }

procedure TfrmTrayTest.btnShowClick(Sender: TObject);
begin
  SystrayIcon.Visible := True;
end;

procedure TfrmTrayTest.btnHideClick(Sender: TObject);
begin
  SystrayIcon.Visible := False;
end;

procedure TfrmTrayTest.btnPaintTestClick(Sender: TObject);
var
  SecondImage: TIcon;
begin
  SecondImage := TIcon.Create;

  try
    SecondImage.Height := 22;
    SecondImage.Width := 22;
    {$IFDEF FPC}
    SecondImage.Canvas.Draw(0, 0, MyImage);
    {$ENDIF}
    Canvas.Draw(0, 0, SecondImage);
  finally
    SecondImage.Free;
  end;
end;

procedure TfrmTrayTest.chkOnPaintDrawingChange(Sender: TObject);
begin
  Invalidate;
end;

procedure TfrmTrayTest.FormCreate(Sender: TObject);
const
  IDI_ICON1         = 101;
  IDI_ICON2         = 115;
  BundleResourceFolder = '/Contents/Resources/';
{$IFDEF Darwin}
var
  pathRef: CFURLRef;
  pathCFStr: CFStringRef;
  pathStr: shortstring;
{$ENDIF}
begin
  pathMedia := '';

  // Under Mac OS X we need to get the location of the bundle
{$IFDEF Darwin}
  pathRef := CFBundleCopyBundleURL(CFBundleGetMainBundle());
  pathCFStr := CFURLCopyFileSystemPath(pathRef, kCFURLPOSIXPathStyle);
  CFStringGetPascalString(pathCFStr, @pathStr, 255, CFStringGetSystemEncoding());
  CFRelease(pathRef);
  CFRelease(pathCFStr);
  
  pathMedia := pathStr + BundleResourceFolder;
{$ENDIF}

{$ifdef Windows}
  SystrayIcon.Icon.Handle := LoadIcon(hInstance, MAKEINTRESOURCE(IDI_ICON1));

//  Loading from a file should also work
//  SystrayIcon.Icon.LoadFromFile('icon.ico');
{$else}
  SystrayIcon.Icon.LoadFromFile(pathMedia + 'icon.ico');
{$endif}

  SystrayIcon.Hint := 'my tool tip';

  SystrayIcon.OnClick := HandleClick;
  
//  SystrayIcon.OnPaint := DoPaint;

  SystrayIcon.PopUpMenu := PopupMenu;

  // Loads the icon

  MyImage := TIcon.Create;

  MyImage.LoadFromFile(pathMedia + 'icon.ico');
end;

procedure TfrmTrayTest.FormDestroy(Sender: TObject);
begin
    MyImage.Free;
end;

procedure TfrmTrayTest.FormPaint(Sender: TObject);
begin
  if chkOnPaintDrawing.Checked then btnPaintTestClick(Sender);
end;

procedure TfrmTrayTest.HandleClick(Sender: TObject);
begin
  Application.MessageBox('Text', 'Caption', 0);
end;

procedure TfrmTrayTest.DoPaint(Sender: TObject);
begin
  {$IFDEF FPC}
  SystrayIcon.Canvas.Draw(0, 0, MyImage);
  {$ENDIF}
  WriteLn('Paint');
end;

initialization
{$ifdef fpc}
  {$I frmtest.lrs}
{$endif}

end.

