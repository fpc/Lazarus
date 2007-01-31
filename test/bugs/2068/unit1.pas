unit Unit1; 

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, StdCtrls;

type

  { TForm1 }

  TForm1 = class(TForm)
    ApplicationProperties1: TApplicationProperties;
    Edit1: TEdit;
    Memo1: TMemo;
    Debugger: TMemo;
    procedure ApplicationProperties1Idle(Sender: TObject; var Done: Boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure Memo1Change(Sender: TObject);
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
  if ParamStr(1)='--runtest' then begin
    writeln(Debugger.Text);
    Close;
  end;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  debugger.lines.add('FormCreate');
  if edit1.Modified then debugger.Lines.Add(' --> Edit1 is Modified') else
    debugger.Lines.Add(' --> Edit1 is NOT Modified');
  if memo1.Modified then debugger.Lines.Add(' --> Memo1 is Modified') else
    debugger.Lines.Add(' --> Memo1 is NOT Modified');
end;

procedure TForm1.FormShow(Sender: TObject);
begin
  debugger.lines.add('FormShow');
  if edit1.Modified then debugger.Lines.Add(' --> Edit1 is Modified') else
    debugger.Lines.Add(' --> Edit1 is NOT Modified');
  if memo1.Modified then debugger.Lines.Add(' --> Memo1 is Modified') else
    debugger.Lines.Add(' --> Memo1 is NOT Modified');
end;

procedure TForm1.Memo1Change(Sender: TObject);
begin
  debugger.lines.add(
    Format('-- Memo1Change is fired: Sender = %s',[(Sender as TComponent).Name]))
end;

initialization
  {$I unit1.lrs}

end.

