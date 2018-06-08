unit fJcfErrorDisplay;

{(*}
(*------------------------------------------------------------------------------
 Delphi Code formatter source code 

The Original Code is fJcfErrorDisplay, released Sept 2003.
The Initial Developer of the Original Code is Anthony Steele. 
Portions created by Anthony Steele are Copyright (C) 1999-2008 Anthony Steele.
All Rights Reserved. 
Contributor(s): Anthony Steele. 

The contents of this file are subject to the Mozilla Public License Version 1.1
(the "License"). you may not use this file except in compliance with the License.
You may obtain a copy of the License at http://www.mozilla.org/NPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied.
See the License for the specific language governing rights and limitations 
under the License.

Alternatively, the contents of this file may be used under the terms of
the GNU General Public License Version 2 or later (the "GPL") 
See http://www.gnu.org/licenses/gpl.html
------------------------------------------------------------------------------*)
{*)}

{$I JcfGlobal.inc}

interface

{ AFS 22 Sept 2003
  Exception handler form
  that allows the exception text to be copied out }

uses
  SysUtils, Classes, Controls, Forms, StdCtrls, ButtonPanel;

type

  { TExceptionDialog }

  TExceptionDialog = class(TForm)
    BtnPanel: TButtonPanel;
    mExceptionMessage: TMemo;
    procedure FormCreate(Sender: TObject);
  private
  public
    procedure DisplayException(const pE: Exception);
    procedure DisplayErrorMessage(const sMessage: string;
      const psCaption: string; const piY, piX: integer);
  end;

procedure ShowExceptionDialog(const pE: Exception);
procedure ShowErrorMessageDialog(const psMessage: string; const psCaption: string;
  const piY, piX: integer);

implementation

uses
  { local }
  ParseError, JcfStringUtils, JcfFontSetFunctions, jcfuiconsts;

{$ifndef FPC}
  {$R *.dfm}
{$else}
  {$R *.lfm}
{$endif}

procedure ShowExceptionDialog(const pE: Exception);
var
  frm: TExceptionDialog;
begin
  frm := TExceptionDialog.Create(nil);
  try
    frm.DisplayException(pe);
  finally
    frm.Free;
  end;
end;

procedure ShowErrorMessageDialog(const psMessage: string; const psCaption: string;
  const piY, piX: integer);
var
  frm: TExceptionDialog;
begin
  frm := TExceptionDialog.Create(nil);
  try
    frm.DisplayErrorMessage(psMessage, psCaption, piY, piX);
  finally
    frm.Free;
  end;
end;


procedure TExceptionDialog.DisplayException(const pE: Exception);
var
  lcParseError: TEParseError;
begin
  if (pE is TEParseError) then
  begin
    lcParseError := TEParseError(pE);

    Caption := lisEDJCFParseError;
    mExceptionMessage.Text := Format(lisEDNear,[lcParseError.Message,
      lcParseError.TokenMessage]);

    if (lcParseError.XPosition > 0) or (lcParseError.YPosition > 0) then
    begin
      mExceptionMessage.Text := Format(lisEDAtLineCol,[mExceptionMessage.Text +
        NativeLineBreak,IntToStr(lcParseError.YPosition),IntToStr(lcParseError.
        XPosition)]);

      if lcParseError.FileName <> '' then
        mExceptionMessage.Text :=
          Format(lisEDIn,[mExceptionMessage.Text,lcParseError.FileName]);
    end;
  end
  else
  begin
    Caption := Format(lisEDException,[pE.ClassName]);
    mExceptionMessage.Text := Format(lisEDType,[pE.ClassName + NativeLineBreak,
      pE.Message]);
  end;

  ShowModal;
end;

procedure TExceptionDialog.DisplayErrorMessage(const sMessage: string;
  const psCaption: string; const piY, piX: integer);
begin
  if psCaption <> '' then
    Caption := psCaption
  else
    Caption := lisEDError;

  mExceptionMessage.Text := sMessage;
  if (piY > 0) or (piX > 0) then
  begin
    mExceptionMessage.Text := Format(lisEDAtLineCol,[mExceptionMessage.Text +
      NativeLineBreak,IntToStr(piY),IntToStr(piX)]);
  end;

  ShowModal;
end;

procedure TExceptionDialog.FormCreate(Sender: TObject);
begin
  SetObjectFontToSystemFont(Self);
end;

end.
