unit AddProfileDialog;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  StdCtrls, ButtonPanel, LazarusIDEStrConsts;

type

  { TAddProfileForm }

  TAddProfileForm = class(TForm)
    ButtonPanel: TButtonPanel;
    NameEdit: TEdit;
    ProfileHeaderLabel: TLabel;
    procedure FormCreate(Sender:TObject);
  private

  public

  end; 

var
  AddProfileForm: TAddProfileForm;

implementation

{$R *.lfm}


{ TAddProfileForm }

procedure TAddProfileForm.FormCreate(Sender:TObject);
begin
  ButtonPanel.OKButton.Caption:=lisOk;
  ButtonPanel.CancelButton.Caption:=dlgCancel;
end;

end.

