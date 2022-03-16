unit AsmTestUnit;


{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, LCLProc, Math, LazStringUtils,
  FpDbgDisasX86, FpDbgClasses, FpDbgUtil;

type

  { TForm1 }

  TForm1 = class(TForm)
    cmdSave: TButton;
    cmdLoad: TButton;
    chk64Bit: TCheckBox;
    Timer1: TTimer;
    txtOutput: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    txtCode: TMemo;
    procedure cmdLoadClick(Sender: TObject);
    procedure cmdSaveClick(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end; 

var
  Form1: TForm1;

const
  TEST_FILE = 'asmcheck.test';

implementation

{$R asmtestunit.lfm}

type
  TFakeProcess = class(TDbgProcess);

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
  Process: TFakeProcess;
  Decoder: TX86AsmDecoder;
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
    // The disassembler only needs the process mode, so we create a fake instance and set the mode flag
    TObject(Process) := TFakeProcess.NewInstance;
    if chk64Bit.Checked
    then Process.SetMode(dm64)
    else Process.SetMode(dm32);

    Decoder := TX86AsmDecoder.Create(Process);
    p := @Code;
    Decoder.Disassemble(p, S, Line);
    txtOutput.Text := S + ' '+ Line;

    Decoder.Free;
    Process.FreeInstance;
  end
//  else txtOutput.Text :='';
end;

procedure TForm1.cmdSaveClick(Sender: TObject);
begin
  txtCode.Lines.SaveToFile(TEST_FILE);
end;

procedure TForm1.cmdLoadClick(Sender: TObject);
begin
  if FileExists(TEST_FILE)
  then txtCode.Lines.LoadFromFile(TEST_FILE);
end;

end.

