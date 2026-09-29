unit Main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Math, Forms, Controls, Dialogs, StdCtrls, Spin, ComCtrls,
  LookupStringList;

type

  { TForm1 }

  TForm1 = class(TForm)
    btnUseStringList: TButton;
    btnDedupeMemo: TButton;
    btnDedupeFile: TButton;
    btnGenerate: TButton;
    btnClear: TButton;
    lblWarning: TLabel;
    lblLines: TLabel;
    lblTime: TLabel;
    Memo: TMemo;
    SpinEdit1: TSpinEdit;
    StatusBar1: TStatusBar;
    procedure btnClearClick(Sender: TObject);
    procedure btnDedupeFileClick(Sender: TObject);
    procedure btnGenerateClick(Sender: TObject);
    procedure btnDedupeMemoClick(Sender: TObject);
    procedure btnUseStringListClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    inList :TStringList;
    procedure PrepareDeDup;
    procedure UpdateDuplicates(aDupCount: Integer);
    procedure UpdateTime(aTime: TDateTime);
  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.UpdateDuplicates(aDupCount: Integer);
begin
  lblLines.Caption := 'Duplicates: ' + IntToStr(aDupCount);
end;

procedure TForm1.UpdateTime(aTime: TDateTime);
begin
  lblTime.Caption := 'Time: ' + TimeToStr(aTime);
end;

procedure TForm1.btnGenerateClick(Sender: TObject);
var
  i, j : Integer;
  s : string;
begin
  lblTime.Caption := 'Time: 0';
  lblLines.Caption := 'Duplicates: ?';
  StatusBar1.SimpleText := 'Generating '+SpinEdit1.Value.ToString+' strings.';
  Memo.Clear;
  Application.ProcessMessages;
  Screen.BeginWaitCursor;
  try
    InList.Clear;
    for i := 0 to SpinEdit1.Value - 1 do
    begin
      s := '';
      for j := 0 to 5 do
        //s := s + chr(randomrange(33, 127));
        s := s + chr(randomrange(97, 123));
      InList.Add(s);
    end;
    Memo.Lines.Assign(inList);
  finally
    Screen.EndWaitCursor;
    StatusBar1.SimpleText := 'Ready.';
  end;
end;

procedure TForm1.btnClearClick(Sender: TObject);
begin
  Memo.Clear;
end;

procedure TForm1.PrepareDedup;
begin
  if Trim(Memo.Text) = '' then
    btnGenerateClick(nil);
  lblTime.Caption := 'Time: 0';
  lblLines.Caption := 'Duplicates: ?';
  Application.ProcessMessages;
end;

procedure TForm1.btnDedupeMemoClick(Sender: TObject);
var
  T : TDateTime;
  LSL : TLookupStringList;
begin
  PrepareDeDup;
  StatusBar1.SimpleText:='Deduping by assigning Memo lines to TLookupStringList.';
  Application.ProcessMessages;
  Screen.BeginWaitCursor;
  LSL := TLookupStringList.Create;
  try
    T := Now;
    LSL.Assign(Memo.Lines);
    UpdateDuplicates(Memo.Lines.Count - LSL.Count);
    Memo.Lines.Assign(LSL);
    UpdateTime(Now - T);
  finally
    LSL.Free;
    Screen.EndWaitCursor;
    StatusBar1.SimpleText := 'Ready.';
  end;
end;

procedure TForm1.btnDedupeFileClick(Sender: TObject);
var
  T : TDateTime;
  N : integer;
  LSL : TLookupStringList;
begin
  PrepareDeDup;
  StatusBar1.SimpleText:='Deduping by saving Memo lines to a file, then reading to TLookupStringList.';
  Application.ProcessMessages;
  Screen.BeginWaitCursor;
  LSL := TLookupStringList.Create;
  try
    Memo.Lines.SaveToFile('temp.txt');
    T := Now;
    N := Memo.Lines.Count;
    LSL.LoadFromFile('temp.txt');
    UpdateDuplicates(N - LSL.Count);
    LSL.SaveToFile('temp.txt');
    UpdateTime(Now - T);
    DeleteFile('temp.txt');
  finally
    LSL.Free;
    Screen.EndWaitCursor;
    StatusBar1.SimpleText := 'Ready.';
  end;
end;

procedure TForm1.btnUseStringListClick(Sender: TObject);
var
  T : TDateTime;
  SL : TStringList;
  i: Integer;
  s: String;
begin
  PrepareDeDup;
  StatusBar1.SimpleText:='Deduping by using a normal TStringList.';
  Application.ProcessMessages;
  Screen.BeginWaitCursor;
  SL := TStringList.Create;
  try
    T := Now;
    // By default SL.Duplicate = dupIgnore but it works only with sorted list.
    for i := 0 to Memo.Lines.Count-1 do begin             // Cannot use it here.
      s := Memo.Lines[i];
      if SL.IndexOf(S) < 0 then
        SL.Add(s);
    end;
    UpdateDuplicates(Memo.Lines.Count - SL.Count);
    Memo.Lines.Assign(SL);
    UpdateTime(Now - T);
  finally
    SL.Free;
    Screen.EndWaitCursor;
    StatusBar1.SimpleText := 'Ready.';
  end;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  inList := TStringList.Create;
  Randomize;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  inList.Free;
end;

procedure TForm1.FormShow(Sender: TObject);
begin
  spinedit1.Value := 100000;
end;

end.
