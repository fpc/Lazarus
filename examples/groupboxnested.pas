{  $Id$  }
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
program GroupBoxNested;

{$mode objfpc}{$H+}

uses
  Interfaces, Classes, SysUtils, StdCtrls, Forms, Controls;

type
  TForm1 = class(TFORM)
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    GroupBox3: TGroupBox;
  public
    constructor Create(TheOwner: TComponent); override;
  end;

{ TForm1 }

constructor TForm1.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  Name:='Form1';
  Caption:='Nested Groupbox demo';
  
  GroupBox1:=TGroupBox.Create(Self);
  with GroupBox1 do begin
    Name:='GroupBox1';
    SetBounds(0,0,350,350);
    Align:=alClient;
    Parent:=Self;
  end;

  GroupBox2:=TGroupBox.Create(Self);
  with GroupBox2 do begin
    Name:='GroupBox2';
    Align:=alClient;
    Parent:=GroupBox1;
  end;

  GroupBox3:=TGroupBox.Create(Self);
  with GroupBox3 do begin
    Name:='GroupBox3';
    Align:=alClient;
    Parent:=GroupBox2;
  end;
  SetBounds(100,50,300,250);
end;

var Form1 : TForm1;

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.

