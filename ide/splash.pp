{  $Id$  }
{
 /***************************************************************************
                               Splash.pp
                             -------------------




 ***************************************************************************/

/***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 ***************************************************************************/
}
unit Splash;

{$mode objfpc}{$H+}

interface

uses
  Classes, Controls, Forms, Buttons, SysUtils, StdCtrls, ExtCtrls, LResources,
  LCLLinux{must be used before graphics}, Graphics;

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
  Caption := 'Lazarus';
  Width := 429;
  Height := 341;
  Position:= poScreenCenter;
  BorderStyle  := bsToolWindow;

  FPixmap := TPixmap.Create;
  FPixmap.LoadFromLazarusResource('splash_logo');
  //FBitmap.Handle := CreatePixmapIndirect(@SPLASH_IMAGE, ColorToRGB(clBtnFace));

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
  then Canvas.Copyrect(Bounds(0, 0, Width, Height)
     ,FPixmap.Canvas, Rect(0,0, Width, Height));
end;

procedure TSplashForm.StartTimer;
begin
  if FTimer<>nil then
    FTimer.Enabled := True;
end;

initialization
  {$I splash.lrs}

end.

{ =============================================================================

  $Log$
  Revision 1.12  2002/05/06 08:58:33  lazarus
  MG: removed unused splash image

  Revision 1.11  2002/05/06 08:50:34  lazarus
  MG: replaced logo, increased version to 0.8.3a and some clientrectbugfix

  Revision 1.10  2002/03/30 21:09:07  lazarus
  MG: hide splash screen on message

  Revision 1.9  2002/03/30 07:29:15  lazarus
  MG: fixed splash screen, fixed parser of resource strings

  Revision 1.8  2002/03/18 11:44:41  lazarus
  MG: TForm.Position will now considered before creating form on 0,0

  Revision 1.7  2001/11/19 21:48:53  lazarus
  MG: fixed splash timer AV, incomplete project loading, application save as

  Revision 1.6  2001/11/19 12:14:24  lazarus
  MG: changed splashform borderstyle

  Revision 1.5  2001/10/15 17:41:31  lazarus
  MG: fixed splashform showing

  Revision 1.4  2001/03/20 16:59:15  lazarus
  MG: fixed many bugs (mem leaks, canvas)

  Revision 1.3  2001/03/19 14:00:47  lazarus
  MG: fixed many unreleased DC and GDIObj bugs

  Revision 1.2  2000/09/10 23:08:30  lazarus
  MWE:
    + Added CreateCompatibeleBitamp function
    + Updated TWinControl.WMPaint
    + Added some checks to avoid gtk/gdk errors
    - Removed no fixed warning from GetDC
    - Removed some output

  Revision 1.1  2000/07/13 10:27:48  michael
  + Initial import

  Revision 1.3  2000/03/30 23:12:17  lazarus
  MWE:
    Moved gtk stuff to ./lcl/interfaces/gtk
    Changed Makefiles

  Revision 1.2  2000/03/19 23:01:42  lazarus
  MWE:
    = Changed splashscreen loading/colordepth
    = Chenged Save/RestoreDC to platform  dependent, since they are
      relative to a DC

}
