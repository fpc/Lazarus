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

  Author: Mattias Gaertner

  Abstract:
    A dialog for showing the progress of a boring calculation.
}
unit ProgressDlg;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, Buttons,
  StdCtrls, ComCtrls, LazarusIDEStrConsts;

type

  { TIDEProgressDialog }

  TIDEProgressDialog = class(TForm)
    AbortButton: TButton;
    DescriptionLabel: TLabel;
    ProgressBar: TProgressBar;
    procedure FormCreate(Sender: TObject);
  private
  public
    procedure ApplicationIdle(Sender: TObject);
  end; 

function ShowProgress(const SomeText: string;
                      Step, MaxStep: integer): boolean;

implementation

function ShowProgress(const SomeText: string; Step, MaxStep: integer): boolean;
begin
  Result:=true;
end;

{ TIDEProgressDialog }

procedure TIDEProgressDialog.FormCreate(Sender: TObject);
begin
  Caption:=lisPDProgress;
  DescriptionLabel.Caption:='...';
  AbortButton.Caption:=lisPDAbort;
  
  Application.AddOnIdleHandler(@ApplicationIdle);
end;

procedure TIDEProgressDialog.ApplicationIdle(Sender: TObject);
begin
  ModalResult:=mrOk;
end;

initialization
  {$I progressdlg.lrs}

end.

