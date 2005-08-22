{  $Id$  }
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
  Classes, Controls, Forms, Buttons, SysUtils, StdCtrls, ExtCtrls, LResources,
  LCLIntf{must be used before graphics}, Graphics;

type
  TSplashForm = class(TForm)
    procedure ApplicationOnIdle(Sender: TObject; var Done: Boolean);
  private
    FPixmap : TPixmap;
    FTimer : TTimer;
    procedure HideFormTimer(Sender : TObject);
  protected
    procedure Click; override;
  public
    procedure Paint; override;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure StartTimer;
  end;

var
  SplashForm: TSplashForm;


implementation


constructor TSplashForm.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  BorderStyle  := bsNone;
  FormStyle := fsStayOnTop;
  Caption := 'Lazarus';

  FPixmap := TPixmap.Create;
  FPixmap.LoadFromLazarusResource('splash_logo');
  Width := FPixmap.Width;
  Height := FPixmap.Height;
  Position:= poScreenCenter;

  FTimer := TTimer.Create(self);
  with FTimer do
  begin
    Interval := 500;
    OnTimer := @HideFormTimer;
    Enabled := False;
  end;

  Application.OnIdle:=@ApplicationOnIdle;
end;

destructor TSplashForm.Destroy;
begin
  FPixmap.Free;
  FPixmap:=nil;
  FTimer.Free;
  FTimer:=nil;
  if Application.OnIdle=@ApplicationOnIdle then
    Application.OnIdle:=nil;
  inherited Destroy;
  SplashForm:=nil;
end;

procedure TSplashForm.Click; 
begin
  Hide;
  if FTimer<>nil then begin
    FTimer.Enabled := False;
    //Release resources
    FTimer.Free;
    FTimer:=nil;
    FPixmap.Free;
    FPixmap:=nil;
  end;
end;

procedure TSplashForm.ApplicationOnIdle(Sender: TObject; var Done: Boolean);
begin
  Hide;
end;

procedure TSplashForm.HideFormTimer(Sender : TObject);
begin
  Click;
end;

procedure TSplashForm.Paint;
begin
  inherited Paint;
  if FPixmap <>nil
  then Canvas.Copyrect(Bounds(0, 0, Width, Height),FPixmap.Canvas,
                       Rect(0,0, Width, Height));
end;

procedure TSplashForm.StartTimer;
begin
  if FTimer<>nil then
    FTimer.Enabled := True;
end;

initialization
  {$I splash.lrs}

end.

