unit KeymapSchemeDlg;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, Buttons, LazarusIDEStrConsts;

type
  TChooseKeySchemeDlg = class(TForm)
    OkButton: TBUTTON;
    CancelButton: TBUTTON;
    NoteLabel: TLABEL;
    SchemeRadiogroup: TRADIOGROUP;
    procedure ChooseKeySchemeDlgCREATE(Sender: TObject);
  private
  public
    function GetKeymapScheme: string;
  end;

function ShowChooseKeySchemeDialog(var NewScheme: string): TModalResult;

implementation

function ShowChooseKeySchemeDialog(var NewScheme: string): TModalResult;
var
  ChooseKeySchemeDlg: TChooseKeySchemeDlg;
begin
  ChooseKeySchemeDlg:=TChooseKeySchemeDlg.Create(nil);
  Result:=ChooseKeySchemeDlg.ShowModal;
  if Result=mrOk then
    NewScheme:=ChooseKeySchemeDlg.GetKeymapScheme;
  ChooseKeySchemeDlg.Free;
end;

{ TChooseKeySchemeDlg }

procedure TChooseKeySchemeDlg.ChooseKeySchemeDlgCREATE(Sender: TObject);
begin
  Caption:='Choose Keymapping scheme';
  NoteLabel.Caption:='Note: All keys will be set to the values of the choosen scheme.';
  SchemeRadiogroup.Caption:='Keymapping Scheme';
  OkButton.Caption:='Ok';
  CancelButton.Caption:='Cancel';
end;

function TChooseKeySchemeDlg.GetKeymapScheme: string;
begin
  case SchemeRadiogroup.ItemIndex of
  1: Result:='Classic';
  else Result:='';
  end;
end;

initialization
  {$I keymapschemedlg.lrs}

end.

