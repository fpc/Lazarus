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
program synedit1;

{$mode objfpc}{$H+}

uses
  Interfaces, Classes, SysUtils, Forms, Controls, GraphType, Graphics, SynEdit,
  SynHighlighterPas;

type
  TForm1 = class(TForm)
    SynEdit1: TSynEdit;
    PascalHighligher: TSynPasSyn;
    procedure Form1Resize(Sender: TObject);
  private
    procedure LoadText(const Filename: string);
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
  end;

{ TForm1 }

procedure TForm1.Form1Resize(Sender: TObject);
begin
  with SynEdit1 do
    SetBounds(10,10,Parent.ClientWidth-10,Parent.ClientHeight-20);
end;

procedure TForm1.LoadText(const Filename: string);
begin
  SynEdit1.Lines.LoadFromFile(Filename);
end;

constructor TForm1.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  SetBounds(10,20,620,400);
  OnResize:=@Form1Resize;
  
  SynEdit1:=TSynEdit.Create(Self);
  with SynEdit1 do begin
    Parent:=Self;

  end;
  
  PascalHighligher:=TSynPasSyn.Create(Self);
  with PascalHighligher do begin
    CommentAttri.Foreground:=clBlue;
    CommentAttri.Style:=[fsBold];
    KeyAttri.Style:=[fsBold];
    StringAttri.Foreground:=clBlue;
    SymbolAttri.Foreground:=clRed;
  end;
  
  SynEdit1.Highlighter:=PascalHighligher;
  
  LoadText('synedit1.pas');
  OnResize(nil);
end;

destructor TForm1.Destroy;
begin
  inherited Destroy;
end;

var
  Form1: TForm1;

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.

