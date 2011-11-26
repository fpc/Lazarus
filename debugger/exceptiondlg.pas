{               ----------------------------------------------
                 exceptiondlg.pas  -  Exception Dialog
                ----------------------------------------------

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
unit ExceptionDlg;

{$mode objfpc}{$H+}

interface

uses
  Classes, Forms, StdCtrls, Buttons, LazarusIDEStrConsts;

type
  
  { TIDEExceptionDlg }

  TIDEExceptionDlg = class(TForm)
    btnBreak: TBitBtn;
    btnContinue: TBitBtn;
    cbIgnoreExceptionType: TCheckBox;
    lblMessage: TLabel;
  private
    { private declarations }
  public
    constructor Create(AOwner: TComponent); override;
    function Execute(AMessage: String; out IgnoreException: Boolean): TModalResult;
  end;

function ExecuteExceptionDialog(AMessage: String; out IgnoreException: Boolean;
                                AskIgnore: Boolean = True): TModalResult;

implementation

{$R *.lfm}

function ExecuteExceptionDialog(AMessage: String; out IgnoreException: Boolean;
  AskIgnore: Boolean = True): TModalResult;
var
  ADialog: TIDEExceptionDlg;
begin
  ADialog := TIDEExceptionDlg.Create(Application);
  try
    ADialog.cbIgnoreExceptionType.Visible := AskIgnore;
    Result := ADialog.Execute(AMessage, IgnoreException);
  finally
    ADialog.Free;
  end;
end;

{ TIDEExceptionDlg }

constructor TIDEExceptionDlg.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Caption := lisExceptionDialog;
  btnBreak.Caption := lisBtnBreak;
  btnContinue.Caption := lisBtnContinue;
  cbIgnoreExceptionType.Caption := lisIgnoreExceptionType;

  btnBreak.LoadGlyphFromLazarusResource('menu_pause');
  btnContinue.LoadGlyphFromLazarusResource('menu_run');
end;

function TIDEExceptionDlg.Execute(AMessage: String; out IgnoreException: Boolean): TModalResult;
begin
  lblMessage.Caption := AMessage;
  Result := ShowModal;
  IgnoreException := cbIgnoreExceptionType.Checked;
end;

end.

