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
  Buttons,
  Classes,
  Controls,
  ExtCtrls,
  Forms,
  Graphics,
  LResources,
  StdCtrls,
  SysUtils;

type

  { TSplashForm }

  TSplashForm = class(TForm)
    Image: TImage;
    Timer: TTimer;
    procedure ApplicationOnIdle(Sender: TObject; var Done: boolean);
    procedure TimerTimer(Sender: TObject);
  private
  protected
  public
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

  Application.OnIdle := @ApplicationOnIdle;
end;

destructor TSplashForm.Destroy;
begin
  if Application.OnIdle = @ApplicationOnIdle then
    Application.OnIdle := nil;

  inherited Destroy;

  SplashForm := nil;
end;

procedure TSplashForm.ApplicationOnIdle(Sender: TObject; var Done: boolean);
begin
  Hide;
end;

procedure TSplashForm.TimerTimer(Sender: TObject);
begin
  Hide;
end;

procedure TSplashForm.StartTimer;
begin
  Timer.Enabled := True;
end;

initialization
  {$I splash.lrs}

end.
