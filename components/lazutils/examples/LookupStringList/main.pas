unit Main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Math, Forms, Controls, Dialogs, StdCtrls, Spin,
  LookupStringList;

type

  { TForm1 }

  TForm1 = class(TForm)
    btnDedupeMemo: TButton;
    btnDedupeFile: TButton;
    btnGenerate: TButton;
    Label1 :TLabel;
    lblLines: TLabel;
    lblTime: TLabel;
    Memo: TMemo;
    SpinEdit1: TSpinEdit;
    procedure btnDedupeFileClick(Sender: TObject);
    procedure btnGenerateClick(Sender: TObject);
    procedure btnDedupeMemoClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    inList :TStringList;
    procedure UpdateDuplicates(aDuplicateCount: string);
    procedure UpdateTime(aTime: TDateTime);
  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.UpdateDuplicates(aDuplicateCount: string);
begin
  lblLines.Caption := 'Duplicated Lines: ' + aDuplicateCount;
end;

procedure TForm1.UpdateTime(aTime: TDateTime);
begin
  lblTime.Caption := 'Time: ' + TimeToStr(aTime);
end;

procedure TForm1.btnGenerateClick(Sender: TObject);
var
  i, j: Integer;
  s :string;
begin
  UpdateDuplicates('?');
  UpdateTime(0);
  Memo.Clear;
  Application.ProcessMessages;
  Screen.BeginWaitCursor;
  try
    InList.Clear;
    for i := 0 to SpinEdit1.Value - 1 do
    begin
      s := '';
      for j := 0 to 5 do
        s := s + chr(randomrange(97, 123));
      InList.Add(s);
    end;
    Memo.Lines.Assign(inList);
  finally
    Screen.EndWaitCursor;
  end;
end;

procedure TForm1.btnDedupeMemoClick(Sender: TObject);
var
  DSL :TLookupStringList;
  T :TDateTime;
begin
  Screen.BeginWaitCursor;
  try
    T := Now;
    DSL := TLookupStringList.Create;
    try
      DSL.Assign(Memo.Lines);
      UpdateDuplicates(IntToStr(Memo.Lines.Count - DSL.Count));
      Memo.Lines.Assign(DSL);
    finally
      DSL.Free;
    end;
    UpdateTime(Now - T);
  finally
    Screen.EndWaitCursor;
  end;
end;

procedure TForm1.btnDedupeFileClick(Sender: TObject);
var
  T :TDateTime;
  N :integer;
  DSL :TLookupStringList;
begin
  lblTime.Caption := 'Time:';
  lblLines.Caption := 'Duplicated lines:';
  Application.ProcessMessages;

  if Trim(Memo.Text) = '' then
  begin
    ShowMessage('Generating data. Please wait.');
    btnGenerateClick(nil);
  end;

  ShowMessage('Saving memo to a file. Please wait.');
  Memo.Lines.SaveToFile('temp.txt');
  ShowMessage('Dedupping the file.');
  T := Now;
  N := Memo.Lines.Count;
  DSL := TLookupStringList.Create;
  try
    DSL.LoadFromFile('temp.txt');
    lblLines.Caption := 'Duplicated Lines: ' + IntToStr(N - DSL.Count);
    DSL.SaveToFile('temp.txt');
    lblTime.Caption := 'Time: ' + TimeToStr(Now - T);
    ShowMessage('Deleting the file.');
    DeleteFile('temp.txt');
  finally
    DSL.Free;
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
