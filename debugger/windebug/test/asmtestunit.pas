unit AsmTestUnit;


{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, StdCtrls, windisas,
  ExtCtrls, LCLProc, Math;

type

  { TForm1 }

  TForm1 = class(TForm)
    chk64Bit: TCheckBox;
    Timer1: TTimer;
    txtOutput: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    txtCode: TMemo;
    procedure Timer1Timer(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end; 

var
  Form1: TForm1; 

implementation

{ TForm1 }

procedure TForm1.Timer1Timer(Sender: TObject);
var
  idx, n: Integer;
  Line, S: String;
  Code: array[0..28] of Byte;
  CodeIdx, B: Byte;
  Value: Int64;
  e: Integer;
  p: Pointer;
begin
  n := txtCode.SelStart;
  if n < 0 then Exit;
  S := Copy(txtCode.Text, 1, n);
  idx := 0;
  for n := 1 to Length(S) do
  begin
    if S[n] = #10 then Inc(idx);
  end;
  Line := txtCode.Lines[idx];
  CodeIdx := 0;
  while (Line <> '') and (CodeIdx < 20) do
  begin
    S := GetPart([], [' ', #9], Line);
    Delete(Line, 1, 1); // strip end ' ' or #9
    if S = '' then Continue;
    B := Min(16, Length(S));
    Val('$' + S, Value, e);
    if e <> 0 then Continue;
    Move(Value, Code[CodeIdx], B div 2);
    Inc(CodeIdx, B div 2);
  end;
  if CodeIdx > 0
  then begin
    p := @Code;
    Disassemble(p, chk64Bit.Checked, S, Line);
    txtOutput.Text := S + ' '+ Line;
  end
//  else txtOutput.Text :='';
end;

initialization
  {$I asmtestunit.lrs}

end.

