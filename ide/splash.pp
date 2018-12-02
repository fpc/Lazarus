{
 /***************************************************************************
                               Splash.pp
                               ---------

 ***************************************************************************/

 ***************************************************************************
 *                                                                         *
 *   This source is free software; you can redistribute it and/or modify   *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 *   This code is distributed in the hope that it will be useful, but      *
 *   WITHOUT ANY WARRANTY; without even the implied warranty of            *
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU     *
 *   General Public License for more details.                              *
 *                                                                         *
 *   A copy of the GNU General Public License is available on the World    *
 *   Wide Web at <http://www.gnu.org/copyleft/gpl.html>. You can also      *
 *   obtain it by writing to the Free Software Foundation,                 *
 *   Inc., 51 Franklin Street - Fifth Floor, Boston, MA 02110-1335, USA.   *
 *                                                                         *
 ***************************************************************************
}
unit Splash;

{$mode objfpc}{$H+}

interface

uses
  Buttons,
  Classes,
  Controls,
  ExtCtrls,
  Forms,
  Graphics,
  LResources,
  SysUtils,
  LazConf;

type

  { TSplashForm }

  TSplashForm = class(TForm)
    Image: TImage;
    procedure ApplicationOnIdle(Sender: TObject; var {%H-}Done: boolean);
    procedure ImagePaint(Sender: TObject);
  private
    procedure LoadSplash;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Show;
  end;

var
  SplashForm: TSplashForm;

implementation

uses
  GraphUtil;

{$R *.lfm}
{$R ../images/splash_logo.res}

const
  VersionStyle: TTextStyle =
    (
      Alignment  : taCenter;
      Layout     : tlCenter;
      SingleLine : True;
      Clipping   : True;
      ExpandTabs : False;
      ShowPrefix : False;
      Wordbreak  : False;
      Opaque     : False;
      SystemFont : False;
      RightToLeft: False;
      EndEllipsis: False;
    );
  VersionFontStyle: TFontStyles = [fsBold];
  VersionFontColor: TColor = clBlue;

constructor TSplashForm.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  Application.AddOnIdleHandler(@ApplicationOnIdle);
end;

destructor TSplashForm.Destroy;
begin
  Application.RemoveOnIdleHandler(@ApplicationOnIdle);

  inherited Destroy;

  SplashForm := nil;
end;

procedure TSplashForm.ApplicationOnIdle(Sender: TObject; var Done: boolean);
begin
  Hide;
end;

procedure TSplashForm.ImagePaint(Sender: TObject);
var
  ATextRect: TRect;
begin
  // GetLazarusVersionString is too long => use LazarusVersionStr
  ATextRect := Rect(
    Image.Left,
    Image.Height - Image.Canvas.TextHeight('Hg')*5 div 4,
    Image.Width,
    Image.Height);
  Image.Canvas.Font.Style := VersionFontStyle;
  Image.Canvas.Font.Color := VersionFontColor;
  Image.Canvas.TextRect(ATextRect, ATextRect.Left, ATextRect.Top, LazarusVersionStr, VersionStyle);
end;

procedure TSplashForm.LoadSplash;
begin
  Image.Picture.LoadFromResourceName(hInstance, 'splash_logo', TPortableNetworkGraphic);
  Width := round(Height * Image.Picture.Width / Image.Picture.Height);
  AntiAliasedStretchBitmap(Image.Picture.Bitmap, Width, Height);
end;

procedure TSplashForm.Show;
begin
  inherited;

  LoadSplash;
end;

end.
