{
 wndtray.dpr

 *****************************************************************************
 *                                                                           *
 *  This demonstration program is public domain, witch means no copyright,   *
 * but also no warranty!                                                      *
 *                                                                           *
 *  This program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
 *                                                                           *
 *****************************************************************************

 Author: Felipe Monteiro de Carvalho
}
program wndtray;

{$ifdef fpc}
  {$mode objfpc}{$H+}
{$endif}

uses
{$ifdef fpc}
  Interfaces,
{$endif}
  Forms,
  frmtest in 'frmtest.pas';

{ add your units here }

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfrmTrayTest, frmTrayTest);
  Application.Run;
end.

