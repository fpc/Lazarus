unit frmLink; 

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Buttons;

type
  { TLinkForm }

  TLinkForm = class(TForm)
    BOK: TButton;
    BCancel: TButton;
    CBTarget: TComboBox;
    ELinkText: TEdit;
    LLinkTarget: TLabel;
    LELinkText: TLabel;
  private
    { private declarations }
  public
    { public declarations }
  end; 

var
  LinkForm: TLinkForm;

implementation

initialization
  {$I frmlink.lrs}

end.

