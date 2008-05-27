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
    A simple dialog for the $PROMPT() tranfer macro function.
    
}
unit MacroPromptDlg;

{$mode objfpc}
{$H+}

{$I ide.inc}

interface

uses
  {$IFDEF IDE_MEM_CHECK}
  MemCheck,
  {$ENDIF}
  Classes, SysUtils, LCLType, Controls, Forms, Buttons, StdCtrls, ComCtrls, 
  Dialogs, LResources, LazarusIDEStrConsts;


type
  TMacroPrompDialog = class(TForm)
    NoteLabel: TLabel;
    DataEdit: TEdit;
    OkButton: TButton;
    CancelButton: TButton;
    procedure MacroPrompDialogResize(Sender: TObject);
    procedure OkButtonClick(Sender: TObject);
    procedure CancelButtonClick(Sender: TObject);
    procedure DataEditKeyDown(Sender: TObject; var Key:Word; Shift:TShiftState);
  public
    constructor Create(AnOwner: TComponent); override;
  end;


function ShowMacroPromptDialog(var InitParam: string): TModalResult;


implementation


function ShowMacroPromptDialog(var InitParam: string): TModalResult;
var MacroPrompDialog: TMacroPrompDialog;
begin
  Result:=mrCancel;
  MacroPrompDialog:=TMacroPrompDialog.Create(nil);
  try
    MacroPrompDialog.DataEdit.Text:=InitParam;
    Result:=MacroPrompDialog.ShowModal;
    if Result=mrOk then 
      InitParam:=MacroPrompDialog.DataEdit.Text;
  finally
    MacroPrompDialog.Free;
  end;
end;

{ TMacroPrompDialog }

constructor TMacroPrompDialog.Create(AnOwner: TComponent);
begin
  inherited Create(AnOwner);
  if LazarusResources.Find(ClassName)=nil then begin
    Width:=300;
    Height:=150;
    Position:=poScreenCenter;
    Caption:=lisMacroPromptEnterData;
    OnResize:=@MacroPrompDialogResize;

    NoteLabel:=TLabel.Create(Self);
    with NoteLabel do begin
      Name:='NoteLabel';
      Parent:=Self;
      SetBounds(8,8,200,25);
      Caption:=lisMacroPromptEnterRunParameters;
      Visible:=true;
    end;
    
    DataEdit:=TEdit.Create(Self);
    with DataEdit do begin
      Name:='DataEdit';
      Parent:=Self;
      SetBounds(8,NoteLabel.Top+NoteLabel.Height+5,Self.ClientWidth-20,25);
      OnKeyDown:=@DataEditKeyDown;
      Visible:=true;
    end;
    
    OkButton:=TButton.Create(Self);
    with OkButton do begin
      Name:='OkButton';
      Parent:=Self;
      SetBounds(Self.ClientWidth-200,Self.ClientHeight-40,80,25);
      Caption:=lisLazBuildOk;
      OnClick:=@OkButtonClick;
      Visible:=true;
    end;
    
    CancelButton:=TButton.Create(Self);
    with CancelButton do begin
      Name:='CancelButton';
      Parent:=Self;
      SetBounds(Self.ClientWidth-100,Self.ClientHeight-40,80,25);
      Caption:=dlgCancel;
      OnClick:=@CancelButtonClick;
      Visible:=true;
    end;
    
  end;
  MacroPrompDialogResize(nil);
  ActiveControl := DataEdit;
end;

procedure TMacroPrompDialog.MacroPrompDialogResize(Sender: TObject);
begin
  with NoteLabel do begin
    SetBounds(8,8,200,25);
  end;

  with DataEdit do begin
    SetBounds(8,NoteLabel.Top+NoteLabel.Height+5,Self.ClientWidth-20,25);
  end;

  with OkButton do begin
    SetBounds(Self.ClientWidth-200,Self.ClientHeight-40,80,25);
  end;

  with CancelButton do begin
    SetBounds(Self.ClientWidth-100,Self.ClientHeight-40,80,25);
  end;
end;

procedure TMacroPrompDialog.OkButtonClick(Sender: TObject);
begin
  ModalResult:=mrOk;
end;

procedure TMacroPrompDialog.CancelButtonClick(Sender: TObject);
begin
  ModalResult:=mrCancel;
end;

procedure TMacroPrompDialog.DataEditKeyDown(Sender: TObject; var Key:Word;
  Shift:TShiftState);
begin
  if (Key=VK_RETURN) then ModalResult:=mrOk;
  if (Key=VK_ESCAPE) then ModalResult:=mrCancel;
end;


end.
