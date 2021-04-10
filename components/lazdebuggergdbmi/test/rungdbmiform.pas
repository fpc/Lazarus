unit RunGdbmiForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LazFileUtils, LazLogger, Forms, Controls,
  Graphics, Dialogs, ExtCtrls, EditBtn, StdCtrls, Buttons, TestBase,
  testregistry, fpcunit, GDBMIDebugger, LCLIntf, DbgIntfDebuggerBase, CheckLst,
  Spin, CmdLineDebugger, TTestDbgExecuteables, TestDbgConfig, TestDbgTestSuites,
  TestDbgControl, TestCommonSources, strutils, math, fgl;

type

  { TForm1 }

  TForm1 = class(TForm)
    BitBtn1: TBitBtn;
    BtnRun: TButton;
    chkCSF: TCheckBox;
    chkDeDup: TCheckBox;
    chkDebugln: TCheckBox;
    chkStripEcho: TCheckBox;
    chkWatch: TCheckBox;
    EdDefine: TEdit;
    edUses: TEdit;
    edPasFile: TEdit;
    edPasHistory: TComboBox;
    edBreakFile: TEdit;
    edBreakLine: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Memo1: TMemo;
    Memo2: TMemo;
    OpenDialog1: TOpenDialog;
    Panel1: TPanel;
    Panel2: TPanel;
    SpinHC: TSpinEdit;
    Splitter1: TSplitter;
    Splitter3: TSplitter;
    procedure BitBtn1Click(Sender: TObject);
    procedure BtnRunClick(Sender: TObject);
    procedure edPasFileChange(Sender: TObject);
    procedure edPasHistoryChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { private declarations }
    FMemoAppendText: String;
    FDeferAppend: Boolean;
  public
    { public declarations }
    EchoText: string;
    procedure AppendToMemo2(Txt: String);
  end; 

var
  Form1: TForm1; 

implementation
type TStringMap = specialize TFPGMap<String, String>;
var
  ControlTestRunner: Pointer;
  FResultList: TStringMap;

{$R *.lfm}

type

  { TRunner }

  TRunner = class(TGDBTestCase)
  private
    FTesting: Boolean;
    FRecordDebugln: Boolean;
    procedure dobrk(ADebugger: TDebuggerIntf; ABreakPoint: TBaseBreakPoint;
      var ACanContinue: Boolean);
  protected
    procedure DoAddOutput(const AText: String);
    procedure DoDbgOut(Sender: TObject; S: string; var Handled: Boolean); override;
    procedure DoDebugln(Sender: TObject; S: string; var Handled: Boolean); override;
    procedure DoDbgOutput(Sender: TObject; const AText: String);
  published
    procedure DoRun;
  end;

function EscQ(s: string): String;
begin
  Result := StringReplace(s, '"', '""', [rfReplaceAll]);
end;

{ TRunner }

procedure TRunner.dobrk(ADebugger: TDebuggerIntf; ABreakPoint: TBaseBreakPoint;
  var ACanContinue: Boolean);
begin
  ACanContinue := False;
end;

procedure TRunner.DoAddOutput(const AText: String);
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
    Form1.AppendToMemo2(EscQ(s) + LineEnding);
  end
  else
    Form1.Memo2.Lines.Add(AText);
end;

procedure TRunner.DoDbgOut(Sender: TObject; S: string; var Handled: Boolean);
begin
  inherited DoDbgOut(Sender, S, Handled);
  if not FRecordDebugln then exit;

  DoAddOutput(s);
end;

procedure TRunner.DoDebugln(Sender: TObject; S: string; var Handled: Boolean);
begin
  inherited DoDebugln(Sender, S, Handled);
  if not FRecordDebugln then exit;

  DoAddOutput(s);
end;

procedure TRunner.DoDbgOutput(Sender: TObject; const AText: String);
begin
  if FRecordDebugln then exit;

  DoAddOutput(AText);
end;

type THack = class(TCmdLineDebugger) end;

procedure TRunner.DoRun;
var
  RunAsWatch: Boolean;

  function RemoveHexNumbers(txt: String): String;
  var
    i, j: Integer;
    p, p2: SizeInt;
    s: String;
  begin
    Result := txt;
    i := 1;
    j := 1;
    p := PosEx('0x', Result, i);
    while p > 0 do begin
      i := p+2;

      p2 := p + 2;
      while (p2 <= Length(Result)) and (Result[p2] in ['0'..'9', 'a'..'f', 'A'..'F']) do
        inc(p2);
      if p2 - p > 6 then begin
        s := copy(Result, p, p2-p);
        Result := StringReplace(Result, s, '##$$##HEX'+IntToStr(j)+'##', [rfReplaceAll, rfIgnoreCase]);
      end;

      inc(j);
      p := PosEx('0x', Result, i);
    end;
  end;

  procedure DoOneRun(Name: String; UsesDirs: array of TUsesDir);
  var
    TestExeName, s, s2, R: string;
    dbg: TGDBMIDebugger;
    i, j , hc: Integer;
    Src: TCommonSource;
    RT: TDBGType;
  begin
    ClearTestErrors;
    FTesting := False;

    if Form1.chkCSF.Checked
    then begin
      Form1.AppendToMemo2('"' + EscQ(Parent.TestName) + ' ' + Name + '",');
    end
    else
      Form1.Memo2.Lines.Add('***** '+ Parent.TestSuiteName + ' ' + Parent.TestName + ' ' + Name);

    try
      Src := GetCommonSourceFor(Form1.edPasFile.Text);

      TestCompile(Src, TestExeName, UsesDirs, '', Form1.EdDefine.Text);
    except
      on e: Exception do
        Form1.Memo2.Lines.Add('Compile error: ' + e.Message);
    end;


    Form1.FMemoAppendText := '';
    Form1.FDeferAppend := Form1.chkDeDup.Checked and (FResultList<> nil);
    try
      dbg := StartGDB(AppDir, TestExeName);
      try
        dbg.OnDbgOutput  := @DoDbgOutput;
        dbg.OnBreakPointHit  := @dobrk;

        (* Add breakpoints *)
        i := StrToIntDef(Form1.edBreakLine.Text, 0);
        if i > 0 then
          Debugger.SetBreakPoint(Src.FileName, i)
        else
          Debugger.SetBreakPoint(Src, Form1.edBreakLine.Text);

        (* Start debugging *)
        //if dbg.State = dsError then begin
        //  Form1.Memo2.Lines.Add('Failed to start');
        //  exit;
        //end;

        hc := Form1.SpinHC.Value;
        if hc < 1 then hc := 1;

        while hc > 0 do begin
          dbg.Run;
          dec(hc);
        end;

        //t:= GetTickCount;
        if Form1.chkCSF.Checked then begin
          Form1.AppendToMemo2('"');
        end;

        for i := 0 to Form1.Memo1.Lines.Count - 1 do begin
          if Trim(Form1.Memo1.Lines[i])<> '' then begin
            if FRecordDebugln then begin
              while DebugLogger.CurrentIndentLevel > 0 do
                DebugLogger.DebugLnExit;
            end;

            FTesting := True;
            try
              Form1.EchoText := Trim(Form1.Memo1.Lines[i]);
              if RunAsWatch then begin
                dbg.EvaluateWait(Form1.EchoText, R, RT, [], 3000);
                DoAddOutput(R);
              end
              else
                dbg.TestCmd(Form1.EchoText);
            finally
              FTesting := False;
            end;
          end;
          if Form1.chkCSF.Checked then
            Form1.AppendToMemo2('","');
        end;
        if Form1.chkCSF.Checked then begin
          Form1.AppendToMemo2('"');
        end;


        dbg.Stop;
      finally
        dbg.Free;
        CleanGdb;
      end;
    finally
      if Form1.FDeferAppend then begin
        Form1.FDeferAppend := False;

        s := Parent.TestSuiteName + ' ' + Parent.TestName + ' ' + Name;
        s2 := RemoveHexNumbers(Form1.FMemoAppendText);
        if FResultList.Find(s2, i) then begin
          s := FResultList.Data[i];
          Form1.AppendToMemo2('"'+EscQ('EQUAL TO: '+s)+'"');
        end
        else begin
          FResultList.Add(s2, s);
          Form1.AppendToMemo2(Form1.FMemoAppendText);
        end;
      end;
      Form1.Memo2.Lines.Add(' ');

      Form1.FMemoAppendText := '';
    end;
  end;

var
  AUsesDir: TUsesDir;
  ii: TSymbolType;
begin
  Form1.Caption := 'Running: '+ Parent.TestSuiteName + ' ' + Parent.TestName;

  if SkipTest then exit;
  if not TestControlCanTest(ControlTestRunner) then exit;

  RunAsWatch := Form1.chkWatch.Checked;
  FRecordDebugln := Form1.chkDebugln.Checked;

  if Form1.edUses.Text <> '' then begin

    //with AUsesDir do begin
    //  DirName := Form1.edUses.Text;
    //  ExeId:= '';
    //  SymbolType:= stNone;
    //  ExtraOpts:= '';
    //  NamePostFix:= ''
    //end;
    //DoOneRun('none', [AUsesDir]);

    for ii := low(TSymbolType) to high(TSymbolType) do begin
      if not TestControlCanSymType(ii) then continue;

      if (ii in CompilerInfo.SymbolTypes) and (ii in DebuggerInfo.SymbolTypes)
      then begin
        with AUsesDir do begin
          DirName := Form1.edUses.Text;
          ExeId:= '';
          SymbolType:= ii;
          ExtraOpts:= '';
          NamePostFix:= ''
        end;
        DoOneRun(SymbolTypeNames[ii], [AUsesDir]);
      end;
    end;
//
//    if (stDwarf in CompilerInfo.SymbolTypes) and (stDwarf in DebuggerInfo.SymbolTypes)
//    then begin
//      with AUsesDir do begin
//        DirName := Form1.edUses.Text;
//        ExeId:= '';
//        SymbolType:= stDwarf;
//        ExtraOpts:= '';
//        NamePostFix:= ''
//      end;
//      DoOneRun('stDwarf', [AUsesDir]);
//    end;
//
//    if (stDwarfSet in CompilerInfo.SymbolTypes) and (stDwarfSet in DebuggerInfo.SymbolTypes)
//    then begin
//      with AUsesDir do begin
//        DirName := Form1.edUses.Text;
//        ExeId:= '';
//        SymbolType:= stDwarfSet;
//        ExtraOpts:= '';
//        NamePostFix:= ''
//      end;
//      DoOneRun('stabsSet', [AUsesDir]);
//    end;
//
//    if (stDwarf3 in CompilerInfo.SymbolTypes) and (stDwarf3 in DebuggerInfo.SymbolTypes)
//    then begin
//      with AUsesDir do begin
//        DirName := Form1.edUses.Text;
//        ExeId:= '';
//        SymbolType:= stDwarf3;
//        ExtraOpts:= '';
//        NamePostFix:= ''
//      end;
//      DoOneRun('stDwarf3', [AUsesDir]);
//    end;


  end
  else
    DoOneRun('', []);
end;

{ TForm1 }

procedure TForm1.BtnRunClick(Sender: TObject);
var
  Dummy: TTestResult;
  i: Integer;
begin
  edPasHistory.AddHistoryItem
    (edPasFile.Text + '*' + edBreakFile.Text + '*' + edBreakLine.Text + '*' + edUses.Text  + '*' + EdDefine.Text,
     15, True, False);
  edPasHistory.Items.SaveToFile(AppendPathDelim(ExtractFilePath(Paramstr(0))) + 'run_gdbmi_cmds.txt');

  FResultList:= TStringMap.Create;
  FResultList.Sorted := True;

  if Memo2.Lines.Count > 0 then begin;
    Memo2.Lines.Add('');
    Memo2.Lines.Add('----- ***** ----- ***** ----- ***** -----');
    Memo2.Lines.Add('');
  end;

  if Form1.chkCSF.Checked then begin
    Form1.AppendToMemo2(LineEnding + '"-","');
    for i := 0 to Form1.Memo1.Lines.Count - 1 do begin
      Form1.AppendToMemo2(EscQ(Trim(Form1.Memo1.Lines[i])) + '","');
    end;
    Form1.AppendToMemo2('"' + LineEnding);
  end;

  Dummy := TTestResult.Create;
  GetTestRegistry.Run(Dummy);
  Dummy.Free;

    //for i := 0 to FTests.Count - 1 do
    //RunTest(TTest(FTests[i]), AResult);

  Form1.Caption := 'Done';
  FreeAndNil(FResultList);
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
  if i < 0 then i := length(t);
  edPasFile.Text := copy(t, 1, i);
  delete(t,1,i+1);

  i := pos('*', t)-1;
  if i < 0 then i := length(t);
  edBreakFile.Text := copy(t, 1, i);
  delete(t,1,i+1);

  i := pos('*', t)-1;
  if i < 0 then i := length(t);
  edBreakLine.Text := copy(t, 1, i);
  delete(t,1,i+1);

  i := pos('*', t)-1;
  if i < 0 then i := length(t);
  edUses.Text := copy(t, 1, i);
  delete(t,1,i+1);

  EdDefine.Text := copy(t, 1, i);
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  if FileExistsUTF8(AppendPathDelim(ExtractFilePath(Paramstr(0))) + 'run_gdbmi_cmds.txt') then
    edPasHistory.Items.LoadFromFile(AppendPathDelim(ExtractFilePath(Paramstr(0))) + 'run_gdbmi_cmds.txt');
  if edPasHistory.Items.Count > 0 then
    edPasHistory.ItemIndex := 0;
  edBreakFile.Text := ExtractFileName(edPasHistory.Text);
  edBreakLine.Text := '1';

  edPasHistoryChange(nil);
end;

procedure TForm1.AppendToMemo2(Txt: String);
var
  i: Integer;
begin
  if FDeferAppend then begin
    FMemoAppendText := FMemoAppendText + Txt;
    exit;
  end;

  i := Memo2.Lines.Count;
  if (i = 0)then
    Memo2.Append(Txt)
  else
    Memo2.Lines[i-1] := Memo2.Lines[i-1] + Txt;
end;

procedure TForm1.BitBtn1Click(Sender: TObject);
begin
  if not OpenDialog1.Execute then exit;
  edPasFile.Text := OpenDialog1.FileName;
  edBreakFile.Text := ExtractFileName(edPasHistory.Text);
  edBreakLine.Text := '1';
end;

initialization

  RegisterDbgTest(TRunner);
  ControlTestRunner         := TestControlRegisterTest('Run');

end.


