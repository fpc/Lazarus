
{
 /***************************************************************************
                          find_dlg.pp  -  Find dialog
                             -------------------

                   Initial Revision  : Tue Aug 08 14:49 CST 2000


 ***************************************************************************/

/***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 ***************************************************************************/
}
{$H+}
unit find_dlg;

{$mode objfpc}

interface

uses
  classes,LclLinux, stdctrls,forms,buttons,comctrls,
  Controls,graphics,extctrls,Dialogs,VCLGlobals,LMessages;


type

 TFindDialog = class(TForm)
    lblTexttofind : TLabel;
    edtTexttoFind: TEdit;
    btnOK : TButton;
    btnCancel : TButton;
    btnHelp : TButton;
    gbGroupBox : TGroupBox;

    cbCaseSensitive : TCheckbox;
    cbWholeWords : TCheckBox;
    cbRegularExpressions : TCheckBox;

    rgForwardBack : TRadioGroup;
{ event handlers }
    procedure btnOKClicked(Sender : TObject);
    procedure btnCancelClicked(Sender : TObject);
    procedure btnHelpClicked(Sender : TObject);
private
    FFindText : String;
    FOnFind : TNotifyEvent;
protected
public
    constructor Create(AOwner: TComponent); override;
    property OnFind : TNotifyEvent read FonFind write FOnFind;
    property FIndText : String read FFindText write FFindText;
end;

implementation

constructor TFindDialog.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Name := 'FindDialog1';

  Caption := 'Find';
  Setbounds(0,0,450,250);
  Position:= poScreenCenter;

  lblTextToFind := TLabel.Create(self);
  with lblTexttoFind do
    Begin
    parent := Self;
    Left := 10;
    Top := 5;
    Caption := 'Text to find:';
    Name := 'lblTextToFind';
    Visible := True;
    end;

  edtTextToFind := TEdit.Create(self);
  with edtTexttoFind do
    Begin
    parent := Self;
    Left := lblTextToFind.LEft+lblTextToFind.Width+5;
    Width := Self.Width - Left - 5;
    Top := 5;
    Name := 'edtTextToFind';
    Visible := True;
    end;


  gbGroupBox := TGroupBox.Create(self);
  with gbGroupBox do
   begin
    parent := Self;
    Left := 10;
    Top := 35;
    Width :=(Self.Width div 2) - 10;
    Height := (Self.Height div 2) -35;
    Caption := 'Options';
    Name := 'gbGroupBox';
    Visible := True;
   end;

   cbCaseSensitive := TCheckbox.Create(self);
   cbWholeWords := TCheckBox.Create(self);
   cbRegularExpressions := TCheckBox.Create(Self);

   with cbCaseSensitive do
    begin
     parent := gbGroupBox;
     left := 5;
     top := 5;
     Caption := 'Case Sensitive';
     Name := 'cbCaseSensitive';
     visible := True;
    end;

   with cbWholeWords do
    begin
     parent := gbGroupBox;
     left := 5;
     top := 25;
     Caption := 'Whole Words';
     Name := 'cbWholeWords';
     visible := True;
    end;

   with cbRegularExpressions do
    begin
     parent := gbGroupBox;
     left := 5;
     top := 50;
     Caption := 'Regular Expressions';
     Name := 'cbRegularExpressions';
     visible := True;
    end;

  rgForwardBack := TRadioGroup.Create(self);
   with rgForwardBack do
    begin
     parent := self;
     left := (Self.Width div 2) +5;
     top := 35;
     Height := (Self.Height div 2) -35;
     width := (Self.Width div 2) -10;
     Caption := 'Direction';
     Items.Add('Forward');
     Items.Add('Backward');
     Name := 'rgForwardBack';
     visible := True;
     ItemIndex := 0;
    end;

   btnOK := TButton.create(self);
   with btnOK do
     begin
     parent := self;
     left := (Self.Width div 2);
     top := Self.Height -30;
     Height := 25;
     Caption := 'OK';
     ModalResult := mrOK;
     visible := True;
     Name := 'btnOK';
     OnCLick := @BTnOKClicked;
     end;

   btnCancel := TButton.create(self);
   with btnCancel do
     begin
     parent := self;
     left := (Self.Width div 2) + ((Self.Width div 2) div 3);
     top := Self.Height -30;
     Height := 25;
     Caption := 'Cancel';
     ModalResult := mrCancel;
     Name := 'btnCancel';
     visible := True;
     OnCLick := @BTnCancelClicked;
     end;

   btnHelp := TButton.create(self);
   with btnHelp do
     begin
     parent := self;
     left := (Self.Width div 2) + (2*((Self.Width div 2) div 3));
     top := Self.Height -30;
     Height := 25;
     Caption := 'Help';
//     ModalResult := mrHelp;
     Name := 'btnHelp';
     visible := True;
     OnCLick := @BTnHelpClicked;
     end;
end;

procedure TFindDialog.btnOKClicked(Sender : TObject);
Begin
FFIndText := edtTexttoFind.Text;
if Assigned(FOnFind) then FOnFind(self);
end;

procedure TFindDialog.btnCancelClicked(Sender : TObject);
Begin
FFIndText := '';
End;

procedure TFindDialog.btnHelpClicked(Sender : TObject);
Begin
end;


end.
