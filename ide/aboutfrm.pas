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
unit AboutFrm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, LResources, StdCtrls,
  Buttons, ExtCtrls;

type
  TAboutForm = class(TForm)
    Memo1: TMEMO;
    Button1: TBUTTON;
    Label1: TLABEL;
    procedure AboutFormResize(Sender: TObject);
  private
    { private declarations }
    FPixmap : TPixmap;
  public
    { public declarations }
    procedure Paint; override;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end; 


function ShowAboutForm: TModalResult;
  

implementation


function ShowAboutForm: TModalResult;
var
  AboutForm: TAboutForm;
begin
  AboutForm:=TAboutForm.Create(Application);
  Result:=AboutForm.ShowModal;
  AboutForm.Free;
end;

{ TAboutForm }

constructor TAboutForm.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FPixmap := TPixmap.Create;
  FPixmap.LoadFromLazarusResource('lazarus_about_logo');
  Label1.Caption := 'Version #: 0.8.5a';
  
  OnResize:=@AboutFormResize;
end;


destructor TAboutForm.Destroy;
begin
  FPixmap.Free;
  FPixmap:=nil;

  inherited Destroy;
end;

procedure TAboutForm.AboutFormResize(Sender: TObject);
begin
  with Memo1 do begin
    Left:=225;
    Top:=10;
    Width:=Self.ClientWidth-Left-Top;
    Height:=Self.ClientHeight-2*Top;
  end;
end;

procedure TAboutForm.Paint;
begin
  inherited Paint;
  if FPixmap <>nil
  then Canvas.Copyrect(Bounds(12, 36, Width, Height)
    ,FPixmap.Canvas, Rect(0,0, Width, Height));
end;



initialization
  {$I aboutfrm.lrs}
  {$I lazarus_about_logo.lrs}

end.

