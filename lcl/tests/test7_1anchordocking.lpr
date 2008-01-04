{
 *****************************************************************************
 *                                                                           *
 *  This file is part of the Lazarus Component Library (LCL)                 *
 *                                                                           *
 *  See the file COPYING.LCL, included in this distribution,                 *
 *  for details about the copyright.                                         *
 *                                                                           *
 *  This program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
 *                                                                           *
 *****************************************************************************
 
  LCL Test 7_1

  Showing a form to test several anchor docking layouts.
}
program test7_1anchordocking;

{$mode objfpc}{$H+}

uses
  Interfaces, FPCAdds, LCLProc, LCLType, Classes, Controls, Forms, TypInfo,
  LMessages, StdCtrls, LDockCtrl;

type

  { TForm1 }

  TForm1 = class(TForm)
  public
    constructor Create(TheOwner: TComponent); override;
  end;

constructor TForm1. Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  

end;

{ TForm1 }

var
  Form1: TForm1 = nil;
begin
  Application.Initialize;
  Application.CreateForm(TForm1,Form1);
  Application.Run;
end.

