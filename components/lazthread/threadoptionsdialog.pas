{
 *****************************************************************************
  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************
}

unit ThreadOptionsDialog;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, StdCtrls;

type

  { TThreadOptionsDialog }

  TThreadOptionsDialog = class(TForm)
    CreateUnitButton: TButton;
    ThreadNameLabel: TLabel;
    ThreadNameEdit: TEdit;
    OptionsGroupBox: TGroupBox;
  private
    { private declarations }
  public
    { public declarations }
    constructor Create(AOwner: TComponent);override;
  end; 

implementation

{$R threadoptionsdialog.lfm}

resourcestring
  SThreadDialogTitle       = 'Thread Class Options';
  SOptionsGroupBoxCaption  = 'Options';
  SThreadNameLabelCaption  = 'Thread Class Name';
  SCreateUnitButtonCaption = 'Create Unit';

{ TThreadOptionsDialog }

//--------------------------------------------------------//
constructor TThreadOptionsDialog.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Caption := SThreadDialogTitle;
  OptionsGroupBox.Caption  := SOptionsGroupBoxCaption;
  ThreadNameLabel.Caption  := SThreadNameLabelCaption;
  CreateUnitButton.Caption := SCreateUnitButtonCaption;
end;

end.

