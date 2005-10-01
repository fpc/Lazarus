{  $Id$  }
{
 /***************************************************************************
                               NoteBookTest.pp  
                             -------------------




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
{
@abstract(An example application for TNotebook)
@author(NoteBookTest.pp - Marc Weustink <weus@quicknet.nl>)
}
program NotebookTest;
 
{$mode objfpc}{$H+}

uses
  Interfaces, Classes, Controls, Forms, Buttons, SysUtils, StdCtrls,
  Graphics, ExtCtrls, LclProc;

type

  TForm1 = class(TFORM)
    notebook1 : TNotebook;
    Button1: TButton; 
    Button2: TButton; 
    procedure Button1CLick(Sender : TObject);
    procedure Button2CLick(Sender : TObject);
  public
    constructor Create(AOwner: TComponent); override;	
  end;

  
constructor TForm1.Create(AOwner: TComponent);	
begin
  inherited Create(AOwner);
  Caption := 'Notebook Testing';
  Left := 0;
  Top := 0;
  Width := 700; 
  Height := 300;
  Position:= poMainFormCenter;

  Button1 := TButton.Create(Self);
  with Button1 do begin
    Top:= 0;
    Left:= 0;
    Width:= 50;
    Height:= 20;
    Parent:= Self;
    Caption := 'Button';
    OnClick := @Button1Click;
  end;
  
  Button2 := TButton.Create(Self);
  with Button2 do begin
    Top:= 0;
    Left:= 50;
    Width:= 50;
    Height:= 20;
    Parent:= Self;
    Caption := 'Button';
    OnClick := @Button2Click;
  end;

  NoteBook1 := TNoteBook.Create(Self);
  with NoteBook1 do
  begin
    Top:= 25;
    Left:= 0;
    Width:= 650;
    Height:= 250;
    Parent:= Self;
  end;
end;

procedure TForm1.Button1Click(Sender : TObject);
var
  NewPageIndex: integer;
  NewPage: TPage;
  PageLabel: TLabel;
begin
  NewPageIndex := Notebook1.Pages.Add(Format('[Page %d]', [Notebook1.Pages.Count]));
  NewPage := Notebook1.Page[NewPageIndex];
  PageLabel := TLabel.Create(Self);
  with PageLabel do
  begin
    Left := 20;
    Top := 10 + NewPageIndex * 20;
    Width := 500;
    Height := 20;
    Caption := Format('This is page [%d]',[NewPageIndex]);
    Parent := NewPage;
  end;
end;

procedure TForm1.Button2Click(Sender : TObject);
begin
  if Notebook1.PageIndex>=0 then
    Notebook1.Pages[Notebook1.PageIndex] := 'Test';
end;

var
  F1: TForm1;

begin
  DebugLN('------ INIT ------- ');
  Application.Initialize;
  DebugLN('------ CREATE ------- ');
  Application.CreateForm(TForm1, F1);
  DebugLN('------ RUN ------- ');
  Application.Run;
  DebugLN('------ DONE ------- ');
end.
