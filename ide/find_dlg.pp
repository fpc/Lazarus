
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
  Controls,graphics,extctrls;


type

 TFind = class(TFORM)
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
protected
public
    constructor Create(AOwner: TComponent); override; 
end;

var
dlgFind1 : TFind;

implementation

constructor TFind.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  Caption := 'Find';
  Left := 0;
  Top := 0;
  Width := 450;
  height := 395;
  Position:= poScreenCenter;

  lblTextToFind := TLabel.Create(self);
  with lblTexttoFind do
    Begin
    parent := Self;
    Left := 10;
    Top := 5;
    Caption := 'Text to find:';
    Visible := True;
    end;

  edtTextToFind := TEdit.Create(self);
  with edtTexttoFind do
    Begin
    parent := Self;
    Left := lblTextToFind.LEft+lblTextToFind.Width+5;
    Top := 5;
    Visible := True;
    end;


  gbGroupBox := TGroupBox.Create(self);
  with gbGroupBox do
   begin
    parent := Self;
    Left := 10;
    Top := 35;
    Width :=(Self.Width div 2) - 10;
    Height := (Self.Height div 3) -35;
    Caption := 'Options';
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
     visible := True;
    end;

   with cbWholeWords do
    begin
     parent := gbGroupBox;
     left := 5;
     top := 25;
     Caption := 'Whole Words';
     visible := True;
    end;

   with cbRegularExpressions do
    begin
     parent := gbGroupBox;
     left := 5;
     top := 50;
     Caption := 'Regular Expressions';
     visible := True;
    end;

  rgForwardBack := TRadioGroup.Create(self);
   with rgForwardBack do
    begin
     parent := self;
     left := (Self.Width div 2) +5;
     top := 35;
     Height := (Self.Height div 3) -35;
     width := (Self.Width div 2) -10;
     Caption := 'Direction';
     Items.Add('Forward');
     Items.Add('Backward');
     visible := True;
    end;

end;

procedure TFind.btnOKClicked(Sender : TObject);
Begin
end;

procedure TFind.btnCancelClicked(Sender : TObject);
Begin
End;

procedure TFInd.btnHelpClicked(Sender : TObject);
Begin
end;

end.
