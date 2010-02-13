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

uses SysUtils, Classes, Forms, Buttons, StdCtrls;

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
begin
  close;
end;

end.

