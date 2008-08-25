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
  Interfaces, Classes, SysUtils, FileUtil, Forms, Controls, GraphType,
  Graphics, SynEdit, SynHighlighterPas;

type
  TForm1 = class(TForm)
    SynEdit1: TSynEdit;
    PascalHighligher: TSynPasSyn;
    procedure Form1Resize(Sender: TObject);
  private
  public
    procedure LoadDefaultText;
    procedure LoadText(const Filename: string);
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
  end;

{ TForm1 }

procedure TForm1.Form1Resize(Sender: TObject);
begin
  with SynEdit1 do
    SetBounds(10,10,Parent.ClientWidth-10,Parent.ClientHeight-20);
end;

procedure TForm1.LoadDefaultText;
var
  E: String;
begin
  E:=#13#10;
  SynEdit1.Lines.Text:=
    '{'+e+
    '  SynEdit Test'+e+
    '}'+e+
    'program synedit1;'+e+
    ''+e+
    '{$mode objfpc}{$H+}'+e+
    ''+e+
    'uses'+e+
    '  Interfaces, Classes, SysUtils, Forms, Controls, GraphType, Graphics, SynEdit,'+e+
    '  SynHighlighterPas;'+e+
    ''+e+
    'type'+e+
    '  TForm1 = class(TForm)'+e+
    '    SynEdit1: TSynEdit;'+e+
    '    PascalHighligher: TSynPasSyn;'+e+
    '    procedure Form1Resize(Sender: TObject);'+e+
    '  private'+e+
    '  public'+e+
    '    procedure LoadDefaultText;'+e+
    '    procedure LoadText(const Filename: string);'+e+
    '    constructor Create(TheOwner: TComponent); override;'+e+
    '    destructor Destroy; override;'+e+
    '  end;'+e+
    ''+e+
    '{ TForm1 }'+e+
    ''+e+
    'procedure TForm1.Form1Resize(Sender: TObject);'+e+
    'begin'+e+
    '  with SynEdit1 do'+e+
    '    SetBounds(10,10,Parent.ClientWidth-10,Parent.ClientHeight-20);'+e+
    'end;'+e+
    ''+e+
    'end.'+e;
end;

procedure TForm1.LoadText(const Filename: string);
begin
  SynEdit1.Lines.LoadFromFile(Filename);
end;

constructor TForm1.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  SetBounds(10,20,980,700);
  OnResize:=@Form1Resize;
  
  SynEdit1:=TSynEdit.Create(Self);
  with SynEdit1 do begin
    Name:='SynEdit1';
    Parent:=Self;
  end;
  
  PascalHighligher:=TSynPasSyn.Create(Self);
  with PascalHighligher do begin
    Name:='PascalHighligher';
    CommentAttri.Foreground:=clBlue;
    CommentAttri.Style:=[fsBold];
    KeyAttri.Style:=[fsBold];
    StringAttri.Foreground:=clBlue;
    SymbolAttri.Foreground:=clRed;
  end;
  
  SynEdit1.Highlighter:=PascalHighligher;
 
  if ParamCount > 0 then
    LoadText(ParamStrUTF8(1))
  else
    LoadDefaultText;
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

