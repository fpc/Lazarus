unit frmabout; 

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, Buttons,
  StdCtrls;

type

  { TAboutForm }

  TAboutForm = class(TForm)
    Button1: TButton;
    LThisApplication: TLabel;
    LCopyRight1: TLabel;
    LCopyRight2: TLabel;
    MCopyRight: TMemo;
  private
    { private declarations }
  public
    { public declarations }
  end; 

var
  AboutForm: TAboutForm;

implementation

initialization
  {$I frmabout.lrs}

end.

