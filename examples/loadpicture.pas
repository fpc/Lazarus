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
program LoadPicture;

{$mode objfpc}{$H+}

uses
  Interfaces,
  Classes, SysUtils, Forms, Controls, Graphics, ExtCtrls;
  
type
  TLoadBitmapForm = class(TForm)
    Image1: TImage;
  public
    constructor Create(TheOwner: TComponent); override;
  end;

{ TLoadBitmapForm }

constructor TLoadBitmapForm.Create(TheOwner: TComponent);
var
  Filename: String;
begin
  inherited Create(TheOwner);
  
  Filename:=SetDirSeparators('../images/splash_logo.xpm');

  Caption := 'Example: Loading picture from file';
  Width := 429;
  Height := 341;
  Position:= poScreenCenter;

  Image1:=TImage.Create(Self);
  with Image1 do begin
    Name:='Image1';
    Parent:=Self;
    Align:=alClient;
    Picture.LoadFromFile(Filename);
  end;
end;

var
  LoadBitmapForm: TLoadBitmapForm;
begin
  Application.Initialize;
  Application.CreateForm(TLoadBitmapForm,LoadBitmapForm);
  Application.Run;
end.

