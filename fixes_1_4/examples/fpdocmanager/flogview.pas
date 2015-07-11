unit fLogView;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls;

type

  { TLogView }

  TLogView = class(TForm)
    edLog: TMemo;
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
  private
    { private declarations }
  public
    { public declarations }
  end; 

var
  LogView: TLogView;

implementation

{$R *.lfm}

{ TLogView }

procedure TLogView.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  edLog.Clear;
  CloseAction := caHide;
end;

end.

