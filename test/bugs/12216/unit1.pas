unit Unit1; 

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  StdCtrls, LCLProc;

type

  { TForm1 }

  TForm1 = class(TForm)
    ApplicationProperties1: TApplicationProperties;
    Button1: TButton;
    procedure ApplicationProperties1Idle(Sender: TObject; var Done: Boolean);
    procedure Button1Click(Sender: TObject);
  private
    { private declarations }
    procedure DoTest;
  public
    { public declarations }
  end; 

var
  Form1: TForm1; 

implementation

var
  InTestRun : boolean;

{ TForm1 }

procedure TForm1.ApplicationProperties1Idle(Sender: TObject; var Done: Boolean);
begin
  if InTestRun then
  begin
    DoTest;
    Close;
  end;
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  DoTest;
end;

procedure TForm1.DoTest;
var
  ListBox : TListBox;
begin
  ListBox := TListBox.Create(nil);
  try
  ListBox.Sorted := true;
  ListBox.Sorted := false;
  DebugLn(['Sorted: ', ListBox.Sorted]);
  ListBox.Items.Insert(0, 'A new item');
  DebugLn('Insert succeeded.');
  except
    on E: Exception do begin
      DebugLn(['Exception occurred: ', E.Message]);
    end;
  end;
  ListBox.Free;
end;

initialization
  InTestRun :=  ParamStr(1)='--runtest';
  {$I unit1.lrs}

end.

