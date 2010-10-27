unit AddProfileDialog;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  StdCtrls, Buttons;

type

  { TAddProfileForm }

  TAddProfileForm = class(TForm)
    CancelButton: TBitBtn;
    NameLabel: TLabel;
    NameEdit: TEdit;
    ProfileHeaderLabel: TLabel;
    OKButton: TBitBtn;
  private

  public

  end; 

var
  AddProfileForm: TAddProfileForm;

implementation

{$R *.lfm}


end.

