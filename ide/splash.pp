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
 *   Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.        *
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
  protected
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

var
  SplashForm: TSplashForm;

implementation

{$R *.lfm}

const
  VersionPos: TPoint = (X:0; Y:281);
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

  Image.Picture.LoadFromLazarusResource('splash_logo');

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
  ATextRect.TopLeft := VersionPos;
  ATextRect.BottomRight := Point(Image.Picture.Width, Image.Picture.Height);
  Image.Canvas.Font.Style := VersionFontStyle;
  Image.Canvas.Font.Color := VersionFontColor;
  Image.Canvas.TextRect(ATextRect, VersionPos.X, VersionPos.Y, LazarusVersionStr, VersionStyle);
end;

initialization
  {$I ../images/splash_logo.lrs}

end.
