{ /***************************************************************************
                    showcompileropts.pas  -  Lazarus IDE unit
                    -----------------------------------------

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

  Author: Mattias Gaertner
  
  Abstract:
    Dialog for showing the compiler options as command line parameters.
}
unit ShowCompilerOpts;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, Buttons,
  StdCtrls;

type
  TShowCompilerOptionsDlg = class(TForm)
    OkButton: TBUTTON;
    CmdLineGroupbox: TGROUPBOX;
    CmdLineMemo: TMEMO;
    procedure OkButtonCLICK(Sender: TObject);
  private
  public
  end;

function ShowCompilerOptionsDialog(const CmdLine: string): TModalResult;

implementation

function ShowCompilerOptionsDialog(const CmdLine: string): TModalResult;
var
  ShowCompilerOptionsDlg: TShowCompilerOptionsDlg;
begin
  Result:=mrOk;
  ShowCompilerOptionsDlg:=TShowCompilerOptionsDlg.Create(nil);
  with ShowCompilerOptionsDlg do begin
    CmdLineMemo.Lines.Text:=CmdLine;
    ShowModal;
    Free;
  end;
end;

{ TShowCompilerOptionsDlg }

procedure TShowCompilerOptionsDlg.OkButtonCLICK(Sender: TObject);
begin
  ModalResult:=mrOk;
end;

initialization
  {$I showcompileropts.lrs}

end.

