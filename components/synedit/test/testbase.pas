unit TestBase;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, fpcunit, SynEdit, LCLType;

type

  { TTestSynEdit }

  TTestSynEdit = class(TSynEdit)
  public
    procedure TestKeyPress(Key: Word; Shift: TShiftState);
  end;

  { TTestBase }

  TTestBase = class(TTestCase)
  private
    FBaseTestName: String;
    FBaseTestNames: Array of String;
    FForm : TForm;
    FSynEdit : TTestSynEdit;
    procedure SetBaseTestName(const AValue: String);
  protected
    function  LinesToText(Lines: Array of String; Separator: String = LineEnding;
                          SeparatorAtEnd: Boolean = False): String;
  protected
    procedure ReCreateEdit;
    procedure SetLines(Lines: Array of String);
    procedure SetCaret(X, Y: Integer);
    procedure SetCaretAndSel(X1, Y1, X2, Y2: Integer);
    procedure DoKeyPress(Key: Word; Shift: TShiftState);

    procedure TestFail(Name, Func, Expect, Got: String; Result: Boolean = False);
    procedure PushBaseName(Add: String);
    procedure PopPushBaseName(Add: String);
    procedure PopBaseName;
    property  BaseTestName: String read FBaseTestName write SetBaseTestName;
    property  SynEdit: TTestSynEdit read FSynEdit;
    property  Form: TForm read FForm;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  end;


implementation

{ TTestSynEdit }

procedure TTestSynEdit.TestKeyPress(Key: Word; Shift: TShiftState);
var
  c: TUTF8Char;
begin
  KeyDown(Key, Shift);
  c := '';
  if Shift = [] then
    case Key of
      VK_A..VK_Z:  c := chr(Key - VK_A + ord('a'));
      VK_0..VK_9:  c := chr(Key - VK_0 + ord('0'));
      VK_RETURN:   c := #13;
      VK_TAB:      c := #9;
      VK_ESCAPE:   c := #27;
    end
  else
  if Shift = [ssShift] then
    case Key of
      VK_A..VK_Z:  c := chr(Key - VK_A + ord('A'));
    end
  else
  if Shift - [ssShift] = [ssCtrl] then
    case Key of
      VK_A..VK_Z:  c := chr(Key - VK_A + 1);
    end;
  if c <> '' then
    UTF8KeyPress(c);
  KeyUp(Key, Shift);
end;

{ TTestBase }

procedure TTestBase.SetUp;
begin
  inherited SetUp;
  FForm := TForm.Create(nil);
  ReCreateEdit;
  FForm.Show;
end;

procedure TTestBase.TearDown;
begin
  inherited TearDown;
  FreeAndNil(FSynEdit);
  FreeAndNil(FForm);
end;

procedure TTestBase.TestFail(Name, Func, Expect, Got: String; Result: Boolean = False);
begin
  if Result then exit;
  if BaseTestName <> '' then
    Fail(Format('%s: %s (%s)%sExpected: %s%s     Got: %s', [BaseTestName, Name, Func, LineEnding, Expect, LineEnding, Got]))
  else
    Fail(Format('%s (%s)%sExpected: %s%s     Got: %s', [Name, Func, LineEnding, Expect, LineEnding, Got]));
end;

procedure TTestBase.SetBaseTestName(const AValue: String);
begin
  FBaseTestNames := nil;
  PushBaseName(AValue);
end;

function TTestBase.LinesToText(Lines: array of String; Separator: String = LineEnding;
  SeparatorAtEnd: Boolean = False): String;
var
  i: Integer;
begin
  Result := '';
  for i := low(Lines) to high(Lines) do begin
    Result := Result + Lines[i];
    if (i <> high(Lines)) or SeparatorAtEnd then
      Result := Result + Separator;
  end;
end;

procedure TTestBase.ReCreateEdit;
begin
  FreeAndNil(FSynEdit);
  FSynEdit := TTestSynEdit.Create(FForm);
  FSynEdit.Parent := FForm;
  FSynEdit.Top := 0;
  FSynEdit.Left := 0;
  FSynEdit.Width:= 500;
  FSynEdit.Height := FSynEdit.Font.Height * 20 + 2;
end;

procedure TTestBase.SetLines(Lines: array of String);
begin
  SynEdit.Text := LinesToText(Lines);
end;

procedure TTestBase.SetCaret(X, Y: Integer);
begin
  SynEdit.BlockBegin := Point(X, Y);
  SynEdit.LogicalCaretXY := Point(X, Y);
end;

procedure TTestBase.SetCaretAndSel(X1, Y1, X2, Y2: Integer);
begin
  SynEdit.LogicalCaretXY := Point(X2, Y2);
  SynEdit.BlockBegin := Point(X1, Y1);
  SynEdit.BlockEnd   := Point(X2, Y2);
end;

procedure TTestBase.DoKeyPress(Key: Word; Shift: TShiftState);
begin
  SynEdit.TestKeyPress(Key, Shift);
end;

procedure TTestBase.PushBaseName(Add: String);
var
  i: Integer;
begin
  i := length(FBaseTestNames);
  SetLength(FBaseTestNames, i + 1);
  FBaseTestNames[i] := Add;
  FBaseTestName := LinesToText(FBaseTestNames, '; ');
end;

procedure TTestBase.PopPushBaseName(Add: String);
begin
  PopBaseName;
  PushBaseName(Add);
end;

procedure TTestBase.PopBaseName;
begin
  SetLength(FBaseTestNames, length(FBaseTestNames) - 1);
  FBaseTestName := LinesToText(FBaseTestNames, ' ');
end;

end.

