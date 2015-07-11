UNIT about;

{$mode objfpc}{$H+}

INTERFACE

USES
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Buttons;

TYPE
  TAboutBox = CLASS(TForm)
    CloseButton: TBUTTON;
    Label1: TLABEL;
    Label2: TLABEL;
    Warn2Label: TLABEL;
    Warn1Label: TLABEL;
    Warn3Label: TLABEL;
    PROCEDURE Button1CLICK(Sender: TObject);
  END;

VAR
  AboutBox: TAboutBox;

IMPLEMENTATION

{$R about.lfm}

{ TAboutBox }

PROCEDURE TAboutBox.Button1CLICK(Sender: TObject);
BEGIN
  Close;
END;

END.

