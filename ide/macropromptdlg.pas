{
/***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 ***************************************************************************/

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
  Classes, SysUtils, LCLLinux, Controls, Forms, Buttons, StdCtrls, ComCtrls, 
  Dialogs, LResources;


type
  TMacroPrompDialog = class(TForm)
    NoteLabel: TLabel;
    DataEdit: TEdit;
    OkButton: TButton;
    CancelButton: TButton;
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
  MacroPrompDialog:=TMacroPrompDialog.Create(Application);
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
  
    Caption:='Enter data';
    SetBounds((Screen.Width-300) div 2,(Screen.Height-150) div 2,300,150);
    
    NoteLabel:=TLabel.Create(Self);
    with NoteLabel do begin
      Name:='NoteLabel';
      Parent:=Self;
      SetBounds(8,8,200,25);
      Caption:='Enter run parameters';
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
      Caption:='Ok';
      OnClick:=@OkButtonClick;
      Visible:=true;
    end;
    
    CancelButton:=TButton.Create(Self);
    with CancelButton do begin
      Name:='CancelButton';
      Parent:=Self;
      SetBounds(Self.ClientWidth-100,Self.ClientHeight-40,80,25);
      Caption:='Cancel';
      OnClick:=@CancelButtonClick;
      Visible:=true;
    end;
    
  end;
  DataEdit.SetFocus;
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
