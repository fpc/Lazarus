{
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
unit HelloForm;

{$mode objfpc}
{$H+}

interface

uses SysUtils, Classes, Forms, Buttons, Controls, Graphics;

type
   THello = class(TForm)
     button1 : TButton;
   public
     constructor Create(AOwner: TComponent); override;
     procedure button1Click(Sender : TObject);
   end;

var
   Hello : THello;

implementation

constructor THello.Create(AOwner: TComponent);
begin
   inherited Create(AOwner);
   Caption := 'Hello World';
   Width := 200;
   Height := 75;
   Left := 200;
   Top := 200;

   button1 := TButton.Create(Self);
   button1.OnClick := @button1click;
   button1.Parent := Self;
   button1.left := (width - 75) div 2 ;
   button1.top := (height - 32) div 2;
   button1.width := 75;
   button1.height := 32;
   button1.caption := 'Close';
   button1.Show;
   
   Self.Constraints.MaxWidth:= 500; 
end;

procedure THello.button1Click(Sender : TObject);
var
  Bitmap : TBitmap;
  S : TFileStream;
  Dir : String;
begin
  Dir := '..' + PathDelim + 'Images' + PathDelim;
  With TPixmap.Create do begin
    LoadFromFile(Dir + 'splash_logo.xpm');
    S := TFileStream.Create(Dir + 'splash_logo.bmp', fmCreate or fmOpenWrite);
    SaveToStream(S);
    S.Free;
  end;
  Bitmap := TBitmap.Create;
  Bitmap.LoadFromFile(Dir + 'splash_logo.bmp');
  S := TFileStream.Create(Dir + 'splash_logo.bmp', fmCreate or fmOpenWrite);
  Bitmap.SaveToStream(S);
  S.Free;
  Bitmap.Free;
  close;
end;

end.

{ =============================================================================

  $Log$
  Revision 1.6  2002/10/29 19:33:42  lazarus
  MG: removed interfaces

  Revision 1.5  2002/10/29 08:22:32  lazarus
  MG: added interfaces unit

  Revision 1.4  2002/09/10 06:49:18  lazarus
  MG: scrollingwincontrol from Andrew

  Revision 1.3  2002/05/10 06:57:50  lazarus
  MG: updated licenses

  Revision 1.2  2002/03/13 22:48:16  lazarus
  Constraints implementation (first cut) and sizig - moving system rework to
  better match Delphi/Kylix way of doing things (the existing implementation
  worked by acident IMHO :-)

  Revision 1.1  2000/07/13 10:28:20  michael
  + Initial import

  Revision 1.5  1999/12/10 00:47:00  lazarus
  MWE:
    Fixed some samples
    Fixed Dialog parent is no longer needed
    Fixed (Win)Control Destruction
    Fixed MenuClick


}
