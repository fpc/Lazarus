{*****************************************************************************
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

  Author: Olivier GUILBAUD

  Abstract:
    Little sample for show how to use PrintersDlgs unit

------------------------------------------------------------------------------}
unit frmselprinter;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, Buttons,
  PrintersDlgs, StdCtrls;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Label1: TLABEL;
    PD: TPrintDialog;
    PSD: TPrinterSetupDialog;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end; 

var
  Form1: TForm1; 

implementation

{ TForm1 }


procedure TForm1.Button2Click(Sender: TObject);
begin
  PSD.Execute;
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  PD.Execute;
end;


initialization
  {$I frmselprinter.lrs}

end.

