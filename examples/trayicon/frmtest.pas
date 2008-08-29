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
    btnDisplayMessage: TButton;
    chkOnPaintDrawing: TCheckBox;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    PopupMenu: TPopupMenu;
    SystrayIcon: TTrayIcon;
    procedure btnShowClick(Sender: TObject);
    procedure btnHideClick(Sender: TObject);
    procedure chkOnPaintDrawingChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormPaint(Sender: TObject);
    procedure HandleClick(Sender: TObject);
  private
    { private declarations }
    pathMedia: string;
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

  // Under Windows we get the path of the executable
{$IFDEF Windows}
  pathMedia := ExtractFilePath(Application.ExeName);
{$ENDIF}

  IncludeTrailingBackslash(pathMedia);

  SystrayIcon.Hint := 'my tool tip';

  SystrayIcon.OnClick := HandleClick;

  SystrayIcon.PopUpMenu := PopupMenu;
end;

procedure TfrmTrayTest.FormPaint(Sender: TObject);
var
  BaseImage: TIcon;
begin
  if chkOnPaintDrawing.Checked then
  begin
    BaseImage := TIcon.Create;
    try
      // Loads the icon
      BaseImage.LoadFromFile(pathMedia + 'icon.ico');
      Canvas.Draw(0, 0, BaseImage);
    finally
      BaseImage.Free;
    end;
  end;
end;

procedure TfrmTrayTest.HandleClick(Sender: TObject);
begin
  Application.MessageBox('Text', 'Caption', 0);
end;

initialization
{$ifdef fpc}
  {$I frmtest.lrs}
{$endif}

end.

