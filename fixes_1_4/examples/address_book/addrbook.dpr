program addrbook;

uses
  Forms,
  frmmain in 'frmmain.pas' {MainForm};

{$R *.res}

begin
  Application.Initialize;
  Application.Title := 'Address Book';
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
