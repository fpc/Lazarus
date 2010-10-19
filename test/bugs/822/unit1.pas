unit Unit1; 

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  LCLType, LCLIntf, LResources, Forms, Controls, Graphics, Dialogs, StdCtrls;

type

  { TForm1 }

  TForm1 = class(TForm)
    ApplicationProperties1: TApplicationProperties;
    Label1: TLabel;
    ScrollBar1: TScrollBar;
    procedure ApplicationProperties1Idle(Sender: TObject; var Done: Boolean);
  private
    { private declarations }
  public
    { public declarations }
  end; 

var
  Form1: TForm1; 

implementation

{ TForm1 }

procedure TForm1.ApplicationProperties1Idle(Sender: TObject; var Done: Boolean);
begin
  Label1.Caption :=
    format('ScrollBar1.Width: %d%sGetSystemMetrics(SM_CXVSCROLL): %d',
          [ScrollBar1.Width, LineEnding, GetSystemMetrics(SM_CXVSCROLL)]);
  if ParamStr(1)='--runtest' then begin
    if ScrollBar1.Width = GetSystemMetrics(SM_CXVSCROLL) then
      writeln('ScrollBar1.Width = GetSystemMetrics(SM_CXVSCROLL)')
    else
      writeln(
        format('ScrollBar1.Width (%d) <> GetSystemMetrics(SM_CXVSCROLL) (%d)',
          [ScrollBar1.Width, GetSystemMetrics(SM_CXVSCROLL)]));
    writeln('ScrollBar1.Height: ', ScrollBar1.Height);
    Close;
  end;
end;

initialization
  {$I unit1.lrs}

end.

