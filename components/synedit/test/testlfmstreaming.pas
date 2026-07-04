unit TestLfmStreaming;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, TestBase, SynEdit, SynGutter, SynGutterBase, SynGutterMarks,
  SynGutterLineNumber, SynGutterChanges, SynGutterCodeFolding, LResources, Forms, ProjResProc,
  testregistry;

type

  TSynTestFrame_A = class(TFrame)
  published
    SynEdit1: TSynEdit;
  end;

  TSynTestForm_1 = class(TForm)
  published
    SynEdit1: TSynEdit;
  end;

  TSynTestForm_2 = class(TForm)
  published
    SynTestFrame_A1: TSynTestFrame_A;
  end;

  { TTestSynEditLfm }

  TTestSynEditLfm = class(TTestBase)
  private
  protected
    procedure SetLfmResource(AName, AData: String);
    function LfmToBinary(TheLfm: array of string): string;
    function CreateForm_1_FromLfm(TheLfm: array of string): TSynTestForm_1;
    function ReCreateForm_1_ViaLfm(var AForm: TSynTestForm_1): TSynTestForm_1;
    function CreateForm_2_FromLfm(TheFormLfm, TheFrameLfm: array of string): TSynTestForm_2;
    function SaveFormToLfm(AForm: TForm): string;
    procedure CheckParts(AName: String; AGutter: TSynGutter; APartClasses: array of TClass);
    procedure CheckPartsNames(AName: String; AGutter: TSynGutter; APartNames: array of string);
    procedure CheckPartWidth(AName: String; AGutter: TSynGutter; APartWidths: array of integer);
  published
    procedure TestGutter;
    procedure TestGutter_Frame;
  end;


implementation

{ TTestSynEditLfm }

function j(s1, s2: array of string): TStringArray;
var
  i,o: Integer;
begin
  Result := nil;
  SetLength(Result, Length(s1) + Length(s2));
  for i := 0 to Length(s1)-1 do Result[i] := s1[i];
  o := Length(s1);
  for i := 0 to Length(s2)-1 do Result[o+i] := s2[i];
end;

function j(s1, s2, s3: array of string): TStringArray;
var
  i,o: Integer;
begin
  Result := nil;
  SetLength(Result, Length(s1) + Length(s2) + Length(s3));
  for i := 0 to Length(s1)-1 do Result[i] := s1[i];
  o := Length(s1);
  for i := 0 to Length(s2)-1 do Result[o+i] := s2[i];
  o := Length(s1)+Length(s2);
  for i := 0 to Length(s3)-1 do Result[o+i] := s3[i];
end;

procedure TTestSynEditLfm.SetLfmResource(AName, AData: String);
var
  R: TLResource;
begin
  R := LazarusResources.Find(AName, 'FORMDATA');
  if R <> nil then
    R.Value := AData
  else
    LazarusResources.Add(AName, 'FORMDATA', AData);
end;

function TTestSynEditLfm.LfmToBinary(TheLfm: array of string): string;
var
  strm, strm2: TStringStream;
begin
  strm := TStringStream.Create;
  strm.WriteAnsiString(AnsiString.Join(LineEnding, TheLfm));
  strm.Position := 0;

  strm2 := TStringStream.Create;
  ProjResProc.LRSObjectTextToBinary(strm, strm2);
  strm.Free;


  strm2.Position := 0;
  Result := strm2.DataString;
  strm2.Free;
end;

function TTestSynEditLfm.CreateForm_1_FromLfm(TheLfm: array of string): TSynTestForm_1;
begin
  SetLfmResource('TSynTestForm_1', LfmToBinary(TheLfm));
  Result := TSynTestForm_1.Create(nil);
end;

function TTestSynEditLfm.ReCreateForm_1_ViaLfm(var AForm: TSynTestForm_1): TSynTestForm_1;
var
  s: String;
begin
  s := SaveFormToLfm(AForm);
  AForm.Free;
  SetLfmResource('TSynTestForm_1', s);

  Result := TSynTestForm_1.Create(nil);
end;

function TTestSynEditLfm.CreateForm_2_FromLfm(TheFormLfm, TheFrameLfm: array of string
  ): TSynTestForm_2;
begin
  SetLfmResource('TSynTestForm_2', LfmToBinary(TheFormLfm));
  SetLfmResource('TSynTestFrame_A', LfmToBinary(TheFrameLfm));

  Result := TSynTestForm_2.Create(nil);
end;

function TTestSynEditLfm.SaveFormToLfm(AForm: TForm): string;
var
  strm: TStringStream;
  Writer: TWriter;
  DestroyDriver: boolean;
  s: AnsiString;
begin
  strm  := TStringStream.Create;
  DestroyDriver:=false;
  Writer := CreateLRSWriter(strm, DestroyDriver);
  Writer.WriteRootComponent(AForm);
  if DestroyDriver then
    Writer.Driver.Free;
  Writer.Free;

  strm.Position := 0;
  Result := strm.DataString;
  strm.Free;

// for text
//  LRSObjectBinaryToText(strm, strm2);
end;

procedure TTestSynEditLfm.CheckParts(AName: String; AGutter: TSynGutter;
  APartClasses: array of TClass);
var
  i: Integer;
begin
  AssertEquals('%s: Parts-count', [AName], Length(APartClasses), AGutter.Parts.Count);
  for i := 0 to Length(APartClasses) - 1 do
    AssertTrue('%s: class of Part[%d] ', [AName, i], AGutter.Parts[i] is APartClasses[i]);
end;

procedure TTestSynEditLfm.CheckPartsNames(AName: String; AGutter: TSynGutter;
  APartNames: array of string);
var
  i: Integer;
begin
  AssertEquals('%s: Parts-count', [AName], Length(APartNames), AGutter.Parts.Count);
  for i := 0 to Length(APartNames) - 1 do
    AssertEquals('%s: name for Part[%d] ', [AName, i], APartNames[i], AGutter.Parts[i].Name);
end;

procedure TTestSynEditLfm.CheckPartWidth(AName: String; AGutter: TSynGutter;
  APartWidths: array of integer);
var
  i, e: Integer;
begin
  AssertEquals('%s: Parts-count', [AName], Length(APartWidths), AGutter.Parts.Count);
  for i := 0 to Length(APartWidths) - 1 do begin
    e := APartWidths[i];
    if e = -1 then
      AssertEquals('%s: autosize for Part[%d] ', [AName, i], True, AGutter.Parts[i].AutoSize)
    else
      AssertEquals('%s: width for Part[%d] ', [AName, i], e, AGutter.Parts[i].Width);
  end;
end;

procedure TTestSynEditLfm.TestGutter;
var
  head, tail: TStringArray;
  f: TSynTestForm_1;
  Syn: TSynEdit;
begin
  head := TStringArray([
    'object Form1: TForm1',
    '  LCLVersion = ''4.9.0.0''',
    '  inline SynEdit1: TSynEdit',
    '    Left = 10',
    '    Height = 250',
    '    Top = 20',
    '    Width = 400',
    '    Name = ''SynEdit1''',
    '    Lines.Strings = (',
    '      ''SynEdit1''',
    '    )'
  ]);
  tail := TStringArray([
    '  end',
    'end'
  ]);

  // width ignored, autosize is on
  f := CreateForm_1_FromLfm(j(head, [
    '    inline SynLeftGutterPartList1: TSynGutterPartList',
    '      object SynGutterMarks1: TSynGutterMarks',
    '        Width = 24',
    '      end',
    '      object SynGutterLineNumber1: TSynGutterLineNumber',
    '        Width = 17',
    '      end',
    '      object SynGutterChanges1: TSynGutterChanges',
    '        Width = 4',
    '      end',
    '      object SynGutterSeparator1: TSynGutterSeparator',
    '        Width = 2',
    '      end',
    '      object SynGutterCodeFolding1: TSynGutterCodeFolding',
    '      end',
    '    end'
  ], tail));

  Syn := f.SynEdit1;
  CheckParts('Left', Syn.Gutter, [TSynGutterMarks, TSynGutterLineNumber, TSynGutterChanges, TSynGutterSeparator, TSynGutterCodeFolding]);
  CheckPartWidth('Left', Syn.Gutter, [-1, -1, -1, -1, -1]);
  CheckParts('Right', Syn.RightGutter, []);

  f := ReCreateForm_1_ViaLfm(f);
  Syn := f.SynEdit1;
  CheckParts('Left', Syn.Gutter, [TSynGutterMarks, TSynGutterLineNumber, TSynGutterChanges, TSynGutterSeparator, TSynGutterCodeFolding]);
  CheckPartWidth('Left', Syn.Gutter, [-1, -1, -1, -1, -1]);
  CheckParts('Right', Syn.RightGutter, []);
  f.Free;

  // change order, and duplicate
  f := CreateForm_1_FromLfm(j(head, [
    '    inline SynLeftGutterPartList1: TSynGutterPartList',
    '      object SynGutterLineNumber1: TSynGutterLineNumber',
    '      end',
    '      object SynGutterMarks1: TSynGutterMarks',
    '      end',
    '      object SynGutterChanges1: TSynGutterChanges',
    '      end',
    '      object SynGutterSeparator1: TSynGutterSeparator',
    '        AutoSize = False',
    '        Width = 70',
    '      end',
    '      object SynGutterCodeFolding2: TSynGutterCodeFolding',
    '      end',
    '      object SynGutterCodeFolding1: TSynGutterCodeFolding',
    '      end',
    '    end'
  ], tail));

  Syn := f.SynEdit1;
  CheckParts('Left', Syn.Gutter, [TSynGutterLineNumber, TSynGutterMarks, TSynGutterChanges, TSynGutterSeparator, TSynGutterCodeFolding, TSynGutterCodeFolding]);
  CheckPartsNames('Left', Syn.Gutter, ['SynGutterLineNumber1', 'SynGutterMarks1', 'SynGutterChanges1', 'SynGutterSeparator1', 'SynGutterCodeFolding2', 'SynGutterCodeFolding1']);
  CheckPartWidth('Left', Syn.Gutter, [-1, -1, -1, 70, -1, -1]);
  CheckParts('Right', Syn.RightGutter, []);

  f := ReCreateForm_1_ViaLfm(f);
  Syn := f.SynEdit1;
  CheckParts('Left', Syn.Gutter, [TSynGutterLineNumber, TSynGutterMarks, TSynGutterChanges, TSynGutterSeparator, TSynGutterCodeFolding, TSynGutterCodeFolding]);
  CheckPartsNames('Left', Syn.Gutter, ['SynGutterLineNumber1', 'SynGutterMarks1', 'SynGutterChanges1', 'SynGutterSeparator1', 'SynGutterCodeFolding2', 'SynGutterCodeFolding1']);
  CheckPartWidth('Left', Syn.Gutter, [-1, -1, -1, 70, -1, -1]);
  CheckParts('Right', Syn.RightGutter, []);
  f.Free;


  // single entry
  f := CreateForm_1_FromLfm(j(head, [
    '    inline SynLeftGutterPartList1: TSynGutterPartList',
    '      object SynGutterLineNumber1: TSynGutterLineNumber',
    '        Width = 17',
    '      end',
    '    end'
  ], tail));

  Syn := f.SynEdit1;
  CheckParts('Left', Syn.Gutter, [TSynGutterLineNumber]);
  CheckParts('Right', Syn.RightGutter, []);

  f := ReCreateForm_1_ViaLfm(f);
  Syn := f.SynEdit1;
  CheckParts('Left', Syn.Gutter, [TSynGutterLineNumber]);
  CheckParts('Right', Syn.RightGutter, []);
  f.Free;


  // zero entry
  // This one should be an lfm without SynLeftGutterPartList1 at all / but test none the less
  f := CreateForm_1_FromLfm(j(head, [
    '    inline SynLeftGutterPartList1: TSynGutterPartList',
    '    end'
  ], tail));

  Syn := f.SynEdit1;
  CheckParts('Left', Syn.Gutter, []);
  CheckParts('Right', Syn.RightGutter, []);

  f := ReCreateForm_1_ViaLfm(f);
  Syn := f.SynEdit1;
  CheckParts('Left', Syn.Gutter, []);
  CheckParts('Right', Syn.RightGutter, []);
  f.Free;


  // no gutter entry = empty
  f := CreateForm_1_FromLfm(j(head, tail));

  Syn := f.SynEdit1;
  CheckParts('Left', Syn.Gutter, []);
  CheckParts('Right', Syn.RightGutter, []);

  f := ReCreateForm_1_ViaLfm(f);
  Syn := f.SynEdit1;
  CheckParts('Left', Syn.Gutter, []);
  CheckParts('Right', Syn.RightGutter, []);
  f.Free;


  (* RIGHT GUTTER *)
  // single entry
  f := CreateForm_1_FromLfm(j(head, [
    '    inline SynRightGutterPartList1: TSynRightGutterPartList',
    '      object SynGutterLineNumber1: TSynGutterLineNumber',
    '      end',
    '    end'
  ], tail));

  Syn := f.SynEdit1;
  CheckParts('Left', Syn.Gutter, []);
  CheckParts('Right', Syn.RightGutter, [TSynGutterLineNumber]);

  f := ReCreateForm_1_ViaLfm(f);
  Syn := f.SynEdit1;
  CheckParts('Left', Syn.Gutter, []);
  CheckParts('Right', Syn.RightGutter, [TSynGutterLineNumber]);
  f.Free;

  // no entry
  f := CreateForm_1_FromLfm(j(head, tail));

  Syn := f.SynEdit1;
  CheckParts('Left', Syn.Gutter, []);
  CheckParts('Right', Syn.RightGutter, []);

  f := ReCreateForm_1_ViaLfm(f);
  Syn := f.SynEdit1;
  CheckParts('Left', Syn.Gutter, []);
  CheckParts('Right', Syn.RightGutter, []);
  f.Free;

  // two same entry
  f := CreateForm_1_FromLfm(j(head, [
    '    inline SynRightGutterPartList1: TSynRightGutterPartList',
    '      object SynGutterLineNumber1: TSynGutterLineNumber',
    '      end',
    '      object SynGutterLineNumber2: TSynGutterLineNumber',
    '      end',
    '    end'
  ], tail));

  Syn := f.SynEdit1;
  CheckParts('Left', Syn.Gutter, []);
  CheckParts('Right', Syn.RightGutter, [TSynGutterLineNumber, TSynGutterLineNumber]);

  f := ReCreateForm_1_ViaLfm(f);
  Syn := f.SynEdit1;
  CheckParts('Left', Syn.Gutter, []);
  CheckParts('Right', Syn.RightGutter, [TSynGutterLineNumber, TSynGutterLineNumber]);
  f.Free;


  (* BOTH GUTTER *)
  // single entry
  f := CreateForm_1_FromLfm(j(head, [
    '    inline SynLeftGutterPartList1: TSynGutterPartList',
    '      object SynGutterLineNumber2: TSynGutterLineNumber',
    '        AutoSize = False',
    '        Width = 42',
    '      end',
    '    end',
    '    inline SynRightGutterPartList1: TSynRightGutterPartList',
    '      object SynGutterLineNumber1: TSynGutterLineNumber',
    '      end',
    '    end'
  ], tail));

  Syn := f.SynEdit1;
  CheckParts('Left', Syn.Gutter, [TSynGutterLineNumber]);
  CheckPartsNames('Left', Syn.Gutter, ['SynGutterLineNumber2']);
  CheckPartWidth('Left', Syn.Gutter, [42]);
  CheckParts('Right', Syn.RightGutter, [TSynGutterLineNumber]);
  CheckPartsNames('Right', Syn.RightGutter, ['SynGutterLineNumber1']);
  CheckPartWidth('Right', Syn.RightGutter, [-1]);

  f := ReCreateForm_1_ViaLfm(f);
  Syn := f.SynEdit1;
  CheckParts('Left', Syn.Gutter, [TSynGutterLineNumber]);
  CheckPartsNames('Left', Syn.Gutter, ['SynGutterLineNumber2']);
  CheckPartWidth('Left', Syn.Gutter, [42]);
  CheckParts('Right', Syn.RightGutter, [TSynGutterLineNumber]);
  CheckPartsNames('Right', Syn.RightGutter, ['SynGutterLineNumber1']);
  CheckPartWidth('Right', Syn.RightGutter, [-1]);
  f.Free;

end;

procedure TTestSynEditLfm.TestGutter_Frame;
var
  head, tail: TStringArray;
  head_f, tail_f, frame_lfm: TStringArray;
  f: TSynTestForm_2;
  Syn: TSynEdit;
begin
  head := TStringArray([
    'object Form1: TForm1',
    '  LCLVersion = ''4.9.0.0''',
    '  inline SynTestFrame_A1: TSynTestFrame_A',
    '    Left = 10',
    '    Top = 20'
  ]);
  tail := TStringArray([
    '  end',
    'end'
  ]);

  head_f := TStringArray([
    'object SynTestFrame_A1: TSynTestFrame_A',
    '  LCLVersion = ''4.9.0.0''',
    '  inline SynEdit1: TSynEdit',
    '    Left = 10',
    '    Height = 250',
    '    Top = 20',
    '    Width = 400',
    '    Name = ''SynEdit1''',
    '    Lines.Strings = (',
    '      ''SynEdit1''',
    '    )'
  ]);
  tail_f := TStringArray([
    '  end',
    'end'
  ]);

  // default in frame
  frame_lfm := j(head_f, [
    '    inline SynLeftGutterPartList1: TSynGutterPartList',
    '      object SynGutterMarks1: TSynGutterMarks',
    '        Width = 24',
    '      end',
    '      object SynGutterLineNumber1: TSynGutterLineNumber',
    '        Width = 17',
    '      end',
    '      object SynGutterChanges1: TSynGutterChanges',
    '        Width = 4',
    '      end',
    '      object SynGutterSeparator1: TSynGutterSeparator',
    '        Width = 2',
    '      end',
    '      object SynGutterCodeFolding1: TSynGutterCodeFolding',
    '      end',
    '    end'
  ], tail);

  f := CreateForm_2_FromLfm(j(head, tail), frame_lfm);


  Syn := f.SynTestFrame_A1.SynEdit1;
  CheckParts('Left', Syn.Gutter, [TSynGutterMarks, TSynGutterLineNumber, TSynGutterChanges, TSynGutterSeparator, TSynGutterCodeFolding]);
  CheckPartWidth('Left', Syn.Gutter, [-1, -1, -1, -1, -1]);
  CheckParts('Right', Syn.RightGutter, []);

  f.Free;


(*
  // default in frame / but only 2 remain in form
  f := CreateForm_2_FromLfm(j(head, [
    '    inherited SynEdit1: TSynEdit',
    '      inherited SynLeftGutterPartList1: TSynGutterPartList',
    '        inherited SynGutterCodeFolding1: TSynGutterCodeFolding[1]',
    '        end',
    '      end',
    '    end'
  ], tail),
  frame_lfm);




  Syn := f.SynTestFrame_A1.SynEdit1;
  CheckParts('Left', Syn.Gutter, [TSynGutterMarks, TSynGutterCodeFolding]);
  //CheckPartWidth('Left', Syn.Gutter, [-1, -1, -1, -1, -1]);
  CheckParts('Right', Syn.RightGutter, []);

  f.Free;
//*)


end;

initialization

  RegisterTest(TTestSynEditLfm);
end.

