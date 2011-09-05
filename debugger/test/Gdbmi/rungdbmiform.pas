unit RunGdbmiForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls, EditBtn, StdCtrls,
  Buttons, CompileHelpers, TestBase, testregistry, fpcunit, GDBMIDebugger, Debugger, LCLIntf,
  CheckLst, CmdLineDebugger, strutils, math;

type

  { TForm1 }

  TForm1 = class(TForm)
    BitBtn1: TBitBtn;
    BtnRun: TButton;
    chkCSF: TCheckBox;
    chkStripEcho: TCheckBox;
    CheckListBox1: TCheckListBox;
    CheckListBox2: TCheckListBox;
    edPasFile: TEdit;
    edPasHistory: TComboBox;
    edBreakFile: TEdit;
    edBreakLine: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Memo1: TMemo;
    Memo2: TMemo;
    OpenDialog1: TOpenDialog;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    Splitter1: TSplitter;
    Splitter2: TSplitter;
    Splitter3: TSplitter;
    procedure BitBtn1Click(Sender: TObject);
    procedure BtnRunClick(Sender: TObject);
    procedure edPasFileChange(Sender: TObject);
    procedure edPasHistoryChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
    CurMemoLine: Integer;
    EchoText, MemoText: string;
  end; 

var
  Form1: TForm1; 

implementation

{$R *.lfm}

type

  { TRunner }

  TRunner = class(TGDBTestCase)
  private
    FTesting: Boolean;
    procedure dobrk(ADebugger: TDebugger; ABreakPoint: TBaseBreakPoint;
      var ACanContinue: Boolean);
  published
    procedure DoDbgOut(Sender: TObject; const AText: String);
    procedure DoRun;
  end;

function EscQ(s: string): String;
begin
  Result := AnsiReplaceStr(s, '"', '""');
end;

{ TRunner }

procedure TRunner.dobrk(ADebugger: TDebugger; ABreakPoint: TBaseBreakPoint;
  var ACanContinue: Boolean);
begin
  ACanContinue := False;
end;

procedure TRunner.DoDbgOut(Sender: TObject; const AText: String);
var s: string;
  i: Integer;
begin
  if not FTesting then exit;

  if Form1.chkStripEcho.Checked then begin
    s := trim(AText);
    if (copy(AText, 1, 1) = '&') then exit;
    if (Form1.EchoText <> '') and ('<'+Form1.EchoText+'>' = s) then exit;
    if (s = '(gdb)')  or (s = '^done') then exit;
  end;
  Form1.EchoText := '';

  if Form1.chkCSF.Checked
  then begin
    s := AText;
    if (copy(s, 1, 2) = '~"') and (copy(s, length(AText), 1) = '"')  then begin
      Delete(s,1,2);
      Delete(s,length(s),1);
    end;
    //S := AnsiReplaceStr(AText, #13, '\r');
    //S := AnsiReplaceStr(AText, #10, '\n');
    Form1.MemoText := Form1.MemoText + EscQ(s) + LineEnding;
    Form1.Memo2.Text := Form1.MemoText;
  end
  else
    Form1.Memo2.Lines.Add(AText);
end;

type THack = class(TCmdLineDebugger) end;

procedure TRunner.DoRun;
var
  TestExeName: string;
  dbg: TGDBMIDebugger;
  i: Integer;
  j: Integer;
  //t: LongWord;
  //S: String;
begin
  i := Form1.CheckListBox1.Items.IndexOf(CompilerInfo.Name);
  if not Form1.CheckListBox1.Checked[i] then exit;
  i := Form1.CheckListBox2.Items.IndexOf(DebuggerInfo.Name);
  if not Form1.CheckListBox2.Checked[i] then exit;

  ClearTestErrors;
  FTesting := False;
  if Form1.chkCSF.Checked
  then begin
    Form1.MemoText := Form1.MemoText + LineEnding  + '"' + EscQ(Parent.Parent.TestName) + '",';
    Form1.Memo2.Text := Form1.MemoText;
  end
  else
    Form1.Memo2.Lines.Add('***** '+ Parent.TestName + ' ' + Parent.Parent.TestName);

  try
    TestCompile(Form1.edPasFile.Text, TestExeName);
  except
    on e: Exception do
      Form1.Memo2.Lines.Add('Compile error: ' + e.Message);
  end;


  try
    dbg := StartGDB(AppDir, TestExeName);
    dbg.OnDbgOutput  := @DoDbgOut;
    dbg.OnBreakPointHit  := @dobrk;

    (* Add breakpoints *)
    with dbg.BreakPoints.Add(Form1.edBreakFile.Text, StrToInt(Form1.edBreakLine.Text)) do begin
      InitialEnabled := True;
      Enabled := True;
    end;

    (* Start debugging *)
    //if dbg.State = dsError then begin
    //  Form1.Memo2.Lines.Add('Failed to start');
    //  exit;
    //end;

    dbg.Run;

    //t:= GetTickCount;
    if Form1.chkCSF.Checked then begin
      Form1.MemoText := Form1.MemoText + '"';
    end;

    for i := 0 to Form1.Memo1.Lines.Count - 1 do begin
      if Trim(Form1.Memo1.Lines[i])<> '' then begin
        FTesting := True;
        Form1.EchoText := Trim(Form1.Memo1.Lines[i]);
        dbg.TestCmd(Form1.EchoText);
        FTesting := False;
      end;
      if Form1.chkCSF.Checked then
        Form1.MemoText := Form1.MemoText + '","';
    end;
    if Form1.chkCSF.Checked then begin
      Form1.MemoText := Form1.MemoText + '"';
      Form1.Memo2.Text := Form1.MemoText;
    end;

    //t := GetTickCount - t;
    //Form1.Memo2.Lines.Add('many '+IntToStr(t));

    //j:=0;
    //t:= GetTickCount;
    //for i := 0 to Form1.Memo1.Lines.Count - 1 do begin
    //  if Trim(Form1.Memo1.Lines[i]) = '' then Continue;
    //  FTesting := True;
    //  THack(dbg).sendcmdLn(Form1.Memo1.Lines[i]);
    //  inc(j);
    //
    //  //dbg.TestCmd(Trim(Form1.Memo1.Lines[i]));
    //  FTesting := False;
    //end;
    //while j > 0 do begin
    //  S := THack(dbg).ReadLine;
    //  Form1.Memo2.Lines.Add(s);
    //  if S = '(gdb) ' then dec(j);
    //end;
    //t := GetTickCount - t;
    //Form1.Memo2.Lines.Add('one  '+IntToStr(t));



    dbg.Stop;
  finally
    dbg.Free;
    CleanGdb;
  end;
  Form1.Memo2.Lines.Add(' ');
end;

{ TForm1 }

procedure TForm1.BtnRunClick(Sender: TObject);
var
  Dummy: TTestResult;
  i: Integer;
begin
  edPasHistory.AddHistoryItem
    (edPasFile.Text + '*' + edBreakFile.Text + '*' + edBreakLine.Text,
     15, True, False);
  edPasHistory.Items.SaveToFile(AppendPathDelim(ExtractFilePath(Paramstr(0))) + 'run_gdbmi_cmds.txt');


  if Memo2.Lines.Count > 0 then begin;
    Memo2.Lines.Add('');
    Memo2.Lines.Add('----- ***** ----- ***** ----- ***** -----');
    Memo2.Lines.Add('');
  end;
  MemoText := Memo2.Text;

  if Form1.chkCSF.Checked then begin
    MemoText := MemoText + LineEnding + '"-","';
    for i := 0 to Form1.Memo1.Lines.Count - 1 do begin
      MemoText := MemoText + EscQ(Trim(Form1.Memo1.Lines[i])) + '","';
    end;
    MemoText := MemoText + '"' + LineEnding;
    Form1.Memo2.Text := MemoText;
  end;

  Dummy := TTestResult.Create;
  GetTestRegistry.Run(Dummy);
  Dummy.Free;

    //for i := 0 to FTests.Count - 1 do
    //RunTest(TTest(FTests[i]), AResult);

end;

procedure TForm1.edPasFileChange(Sender: TObject);
begin

end;

procedure TForm1.edPasHistoryChange(Sender: TObject);
var
  t: TCaption;
  i: SizeInt;
begin
  t := edPasHistory.Text;
  i := pos('*', t)-1;
  if i <= 0 then i := length(t);
  edPasFile.Text := copy(t, 1, i);
  delete(t,1,i+1);

  i := pos('*', t)-1;
  if i <= 0 then i := length(t);
  edBreakFile.Text := copy(t, 1, i);
  delete(t,1,i+1);

  edBreakLine.Text := copy(t, 1, i);
end;

procedure TForm1.FormCreate(Sender: TObject);
var
  l: TCompilerList;
  i, j: Integer;
  l2: TDebuggerList;
begin
  RegisterDbgTest(TRunner);
  if FileExistsUTF8(AppendPathDelim(ExtractFilePath(Paramstr(0))) + 'run_gdbmi_cmds.txt') then
    edPasHistory.Items.LoadFromFile(AppendPathDelim(ExtractFilePath(Paramstr(0))) + 'run_gdbmi_cmds.txt');
  if edPasHistory.Items.Count > 0 then
    edPasHistory.ItemIndex := 0;
  edBreakFile.Text := ExtractFileName(edPasHistory.Text);
  edBreakLine.Text := '1';

  edPasHistoryChange(nil);

  l := GetCompilers;
  for i := 0 to l.Count-1 do begin
    j := CheckListBox1.Items.Add(l.Name[i]);
    CheckListBox1.Checked[j] := True;
  end;
  l2 := GetDebuggers;
  for i := 0 to l2.Count-1 do begin
    j := CheckListBox2.Items.Add(l2.Name[i]);
    CheckListBox2.Checked[j] := True;
  end;
end;

procedure TForm1.BitBtn1Click(Sender: TObject);
begin
  if not OpenDialog1.Execute then exit;
  edPasFile.Text := OpenDialog1.FileName;
  edBreakFile.Text := ExtractFileName(edPasHistory.Text);
  edBreakLine.Text := '1';
end;

end.


