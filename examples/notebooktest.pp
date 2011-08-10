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

  TForm1 = class(TForm)
    Notebook1 : TNotebook;
    Button1: TButton; 
    Button2: TButton;
    Button3: TButton;
    Label1: TLabel;
    procedure Button1CLick(Sender : TObject);
    procedure Button2CLick(Sender : TObject);
    procedure Button3CLick(Sender : TObject);
  public
    constructor Create(AOwner: TComponent); override;	
  end;

  
constructor TForm1.Create(AOwner: TComponent);	
begin
  inherited CreateNew(AOwner, 1);
  Caption := 'Notebook testing';
  Left := 0;
  Top := 0;
  Width := 700; 
  Height := 300;
  Position:= poMainFormCenter;

  Button1 := TButton.Create(Self);
  with Button1 do begin
    Top:= 0;
    Left:= 0;
    AutoSize := true;
    Parent:= Self;
    Caption := 'Create page';
    OnClick := @Button1Click;
  end;
  
  Button2 := TButton.Create(Self);
  with Button2 do begin
    AnchorSide[akLeft].Side := asrRight;
    AnchorSide[akLeft].Control := Button1;
    AnchorSide[akTop].Side := asrCenter;
    AnchorSide[akTop].Control := Button1;
    AutoSize := true;
    Parent:= Self;
    Caption := 'Back';
    OnClick := @Button2Click;
  end;

  Button3 := TButton.Create(Self);
  with Button3 do begin
    AnchorSide[akLeft].Side := asrRight;
    AnchorSide[akLeft].Control := Button2;
    AnchorSide[akTop].Side := asrCenter;
    AnchorSide[akTop].Control := Button2;
    AutoSize := true;
    Parent:= Self;
    Caption := 'Forward';
    OnClick := @Button3Click;
  end;

  Label1 := TLabel.Create(Self);
  with Label1 do begin
    AnchorSide[akLeft].Side := asrRight;
    BorderSpacing.Left := 6;
    AnchorSide[akLeft].Control := Button3;
    AnchorSide[akTop].Side := asrCenter;
    AnchorSide[akTop].Control := Button3;
    Parent:= Self;
    Caption := '0 pages total';
  end;

  NoteBook1 := TNoteBook.Create(Self);
  with NoteBook1 do
  begin
    AnchorSide[akTop].Side := asrBottom;
    AnchorSide[akTop].Control := Button1;
    Align := alBottom;
    Anchors := Anchors + [akTop];
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
  Label1.Caption := IntToStr(Notebook1.PageCount)+ ' pages total';
end;

procedure TForm1.Button2Click(Sender : TObject);
begin
  if Notebook1.PageIndex>0 then
    Notebook1.PageIndex:=Notebook1.PageIndex-1;
end;

procedure TForm1.Button3Click(Sender : TObject);
begin
  if Notebook1.PageIndex<Notebook1.PageCount-1 then
    Notebook1.PageIndex:=Notebook1.PageIndex+1;
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
