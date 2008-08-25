{ Copyright (C) 2006 Darius Blaszijk

  This source is free software; you can redistribute it and/or modify it under
  the terms of the GNU General Public License as published by the Free
  Software Foundation; either version 2 of the License, or (at your option)
  any later version.

  This code is distributed in the hope that it will be useful, but WITHOUT ANY
  WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
  FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
  details.

  A copy of the GNU General Public License is available on the World Wide Web
  at <http://www.gnu.org/copyleft/gpl.html>. You can also obtain it by writing
  to the Free Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
  MA 02111-1307, USA.
}

unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, SynEdit,
  Buttons, ExtCtrls;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;

    SynEdit1: TSynEdit;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end; 

var
  Form1: TForm1; 

implementation

uses SourcePrinter;

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
begin
  SynEdit1.Lines.LoadFromFile(UTF8ToSys('main.pas'));
end;

procedure TForm1.Button1Click(Sender: TObject);
var
  sp: TSourcePrinter;
begin
  sp := TSourcePrinter.Create;
  sp.Execute(SynEdit1.Lines);
  sp.Free;
end;

initialization
  {$I main.lrs}

end.

