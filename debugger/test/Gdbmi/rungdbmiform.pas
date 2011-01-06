unit RunGdbmiForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls, EditBtn, StdCtrls,
  Buttons, CompileHelpers, TestBase, testregistry, fpcunit, GDBMIDebugger, Debugger, LCLIntf, CmdLineDebugger;

type

  { TForm1 }

  TForm1 = class(TForm)
    BitBtn1: TBitBtn;
    Button1: TButton;
    edPasFile: TComboBox;
    edBreakFile: TEdit;
    edBreakLine: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Memo1: TMemo;
    Memo2: TMemo;
    OpenDialog1: TOpenDialog;
    Panel1: TPanel;
    Panel2: TPanel;
    Splitter1: TSplitter;
    procedure BitBtn1Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
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
  published
    procedure DoDbgOut(Sender: TObject; const AText: String);
    procedure DoRun;
  end;

{ TRunner }

procedure TRunner.DoDbgOut(Sender: TObject; const AText: String);
begin
  if not FTesting then exit;
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
  FTesting := False;
  Form1.Memo2.Lines.Add('***** '+ Parent.TestName + ' ' + Parent.Parent.TestName);
  TestCompile(AppDir + 'WatchesPrg.pas', TestExeName);

  try
    dbg := TGDBMIDebugger.Create(DebuggerInfo.ExeName);
    dbg.OnDbgOutput  := @DoDbgOut;
    ;

    (* Add breakpoints *)
    with dbg.BreakPoints.Add(Form1.edBreakFile.Text, StrToInt(Form1.edBreakLine.Text)) do begin
      InitialEnabled := True;
      Enabled := True;
    end;

    (* Start debugging *)
    dbg.Init;
    if dbg.State = dsError then begin
      Form1.Memo2.Lines.Add('Failed to start');
      exit;
    end;

    dbg.WorkingDir := AppDir;
    dbg.FileName   := TestExeName;
    dbg.Arguments := '';
    dbg.ShowConsole := True;

    dbg.Run;

    //t:= GetTickCount;
    for i := 0 to Form1.Memo1.Lines.Count - 1 do begin
      if Trim(Form1.Memo1.Lines[i]) = '' then Continue;
      FTesting := True;
      dbg.TestCmd(Trim(Form1.Memo1.Lines[i]));
      FTesting := False;
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
  end;
  Form1.Memo2.Lines.Add(' ');
end;

{ TForm1 }

procedure TForm1.Button1Click(Sender: TObject);
var
  Dummy: TTestResult;
begin
  if Memo2.Lines.Count > 0 then begin;
    Memo2.Lines.Add('');
    Memo2.Lines.Add('----- ***** ----- ***** ----- ***** -----');
    Memo2.Lines.Add('');
  end;
  Dummy := TTestResult.Create;
  GetTestRegistry.Run(Dummy);
  Dummy.Free;

    //for i := 0 to FTests.Count - 1 do
    //RunTest(TTest(FTests[i]), AResult);

end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  RegisterDbgTest(TRunner);
  if FileExistsUTF8(AppendPathDelim(ExtractFilePath(Paramstr(0))) + 'run_gdbmi_cmds.txt') then
    edPasFile.Items.LoadFromFile(AppendPathDelim(ExtractFilePath(Paramstr(0))) + 'run_gdbmi_cmds.txt');
  if edPasFile.Items.Count > 0 then
    edPasFile.ItemIndex := 0;
  edBreakFile.Text := ExtractFileName(edPasFile.Text);
  edBreakLine.Text := '1';
end;

procedure TForm1.BitBtn1Click(Sender: TObject);
begin
  if not OpenDialog1.Execute then exit;
  edPasFile.AddHistoryItem(OpenDialog1.FileName, 15, True, False);
  edBreakFile.Text := ExtractFileName(edPasFile.Text);
  edBreakLine.Text := '1';
  edPasFile.Items.SaveToFile(AppendPathDelim(ExtractFilePath(Paramstr(0))) + 'run_gdbmi_cmds.txt');
end;

end.


