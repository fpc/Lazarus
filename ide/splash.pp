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
  BorderStyle  := bsNone;

  FPixmap := TPixmap.Create;
  FPixmap.LoadFromLazarusResource('splash_logo');

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
  Revision 1.17  2002/09/30 20:19:12  lazarus
  MG: fixed flickering of modal forms

  Revision 1.16  2002/09/27 20:52:18  lazarus
  MWE: Applied patch from "Andrew Johnson" <aj_genius@hotmail.com>

  Here is the run down of what it includes -

   -Vasily Volchenko's Updated Russian Localizations

   -improvements to GTK Styles/SysColors
   -initial GTK Palette code - (untested, and for now useless)

   -Hint Windows and Modal dialogs now try to stay transient to
    the main program form, aka they stay on top of the main form
    and usually minimize/maximize with it.

   -fixes to Form BorderStyle code(tool windows needed a border)

   -fixes DrawFrameControl DFCS_BUTTONPUSH to match Win32 better
    when flat

   -fixes DrawFrameControl DFCS_BUTTONCHECK to match Win32 better
    and to match GTK theme better. It works most of the time now,
    but some themes, noteably Default, don't work.

   -fixes bug in Bitmap code which broke compiling in NoGDKPixbuf
    mode.

   -misc other cleanups/ fixes in gtk interface

   -speedbutton's should now draw correctly when flat in Win32

   -I have included an experimental new CheckBox(disabled by
    default) which has initial support for cbGrayed(Tri-State),
    and WordWrap, and misc other improvements. It is not done, it
    is mostly a quick hack to test DrawFrameControl
    DFCS_BUTTONCHECK, however it offers many improvements which
    can be seen in cbsCheck/cbsCrissCross (aka non-themed) state.

   -fixes Message Dialogs to more accurately determine
    button Spacing/Size, and Label Spacing/Size based on current
    System font.
   -fixes MessageDlgPos, & ShowMessagePos in Dialogs
   -adds InputQuery & InputBox to Dialogs

   -re-arranges & somewhat re-designs Control Tabbing, it now
    partially works - wrapping around doesn't work, and
    subcontrols(Panels & Children, etc) don't work. TabOrder now
    works to an extent. I am not sure what is wrong with my code,
    based on my other tests at least wrapping and TabOrder SHOULD
    work properly, but.. Anyone want to try and fix?

   -SynEdit(Code Editor) now changes mouse cursor to match
    position(aka over scrollbar/gutter vs over text edit)

   -adds a TRegion property to Graphics.pp, and Canvas. Once I
    figure out how to handle complex regions(aka polygons) data
    properly I will add Region functions to the canvas itself
    (SetClipRect, intersectClipRect etc.)

   -BitBtn now has a Stored flag on Glyph so it doesn't store to
    lfm/lrs if Glyph is Empty, or if Glyph is not bkCustom(aka
    bkOk, bkCancel, etc.) This should fix most crashes with older
    GDKPixbuf libs.

  Revision 1.15  2002/05/10 06:57:45  lazarus
  MG: updated licenses

  Revision 1.14  2002/05/08 16:47:01  lazarus

       Turned the ApplicationIdle feature back on.  MAH

  Revision 1.13  2002/05/08 14:45:57  lazarus

     New About Dialog Window added; Splash screen modified to stay visible
     longer.  MAH

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
