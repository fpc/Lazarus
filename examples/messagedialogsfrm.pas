unit MessageDialogsFrm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, LazLogger;

type
  
  { TMainForm }

  TMainForm = class(TForm)
    Button1: TButton;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private

  public

  end;

var
  MainForm: TMainForm;

implementation

{$R *.lfm}

{ TMainForm }

procedure TMainForm.FormCreate(Sender: TObject);
begin
  Button1.Left := (Width - Button1.Width) div 2;
  Button1.Top := (Height - Button1.Height) div 2;
end;

procedure TMainForm.Button1Click(Sender: TObject);
begin
  ShowMessage('First simple test!');
  DebugLn('Go to second dialog');
  MessageDlg('Caption', 'Two buttons now ...', mtError, [mbOK, mbCancel], 0);
  MessageDlg('Warning, not fully implemented', mtWarning, [mbYes, mbNo, mbOK, mbCancel], 0);
  ShowMessageFmt('The show will end now'+LineEnding+'%s'+LineEnding+'Good bye!!!', [MainForm.Caption]);
  close; 
end;

end.

