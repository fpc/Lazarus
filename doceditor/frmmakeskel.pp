unit frmmakeskel; 

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, Buttons,
  StdCtrls, EditBtn;

type

  { TMakeSkelForm }

  TMakeSkelForm = class(TForm)
    BOK: TButton;
    BCancel: TButton;
    CBDisableResults: TCheckBox;
    CBDisableArguments: TCheckBox;
    CBDisableSeeAlso: TCheckBox;
    CBDisableProtected: TCheckBox;
    CBDisablePrivate: TCheckBox;
    CBDisableErrors: TCheckBox;
    EAdditionalOptions: TEdit;
    EPackage: TEdit;
    FEinputFile: TFileNameEdit;
    FEoutputFIle: TFileNameEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    LEPackage: TLabel;
    procedure CheckEnabled(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end; 

var
  MakeSkelForm: TMakeSkelForm;

implementation

{ TMakeSkelForm }

procedure TMakeSkelForm.CheckEnabled(Sender: TObject);
begin
  if Sender=nil then ;
end;

initialization
  {$I frmmakeskel.lrs}

end.

