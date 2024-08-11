unit frmConsoleOpts;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ButtonPanel;

type

  { TConsoleTestRunnerOptionsForm }
  TDefaultFormat = (dfDefault,dfXML,dfPlainText,dfPlainNoTiming,dfLaTeX);

  TConsoleTestRunnerOptionsForm = class(TForm)
    bpOptions: TButtonPanel;
    cbFormat: TComboBox;
    cbRunAllTests: TCheckBox;
    cbTestInsight: TCheckBox;
    cbCreateTestCase: TCheckBox;
    lblFormat: TLabel;
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
  private
    Function GetCheckBox(aIndex : Integer): TCheckBox;
    function GetCb(aIndex : Integer): Boolean;
    function GetDefaultFormat: TDefaultFormat;
    procedure SetCB(aIndex : Integer; aValue: Boolean);
    procedure SetDefaultFormat(AValue: TDefaultFormat);
    procedure TranslateForm;
    Procedure LoadForm;
    Procedure SaveForm;
  public

    Property RunAllTests : Boolean Index 1 Read GetCB Write SetCB;
    Property CreateTestCase : Boolean Index 2 Read GetCB Write SetCB;
    Property UseTestInsight : Boolean Index 3 Read GetCB Write SetCB;
    Property DefaultFormat : TDefaultFormat Read GetDefaultFormat Write SetDefaultFormat;
  end;

var
  ConsoleTestRunnerOptionsForm: TConsoleTestRunnerOptionsForm;

implementation

uses strtestcaseopts, TypInfo, BaseIDEIntf, LazConfigStorage;

{$R *.lfm}

const
  KeyRunAllTests = 'RunAllTests/Value';
  KeyUseTestInsight = 'UseTestInsight/Value';
  KeyCreateTestCase = 'CreateTestCase/Value';
  KeyDefaultFormat = 'DefaultFormat/Value';



{ TConsoleTestRunnerOptionsForm }

procedure TConsoleTestRunnerOptionsForm.FormCreate(Sender: TObject);
begin
  TranslateForm;
  LoadForm;
end;

procedure TConsoleTestRunnerOptionsForm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  if ModalResult=mrOK then
    SaveForm;
end;

procedure TConsoleTestRunnerOptionsForm.TranslateForm;

begin
  Caption:=sNewFPCUnitProgramToR;
  cbRunAllTests.Caption:=sRunAllTests;
  cbTestInsight.Caption:=sUseTextInsight;
  cbCreateTestCase.Caption:=sCreateFirstTestCase;
  lblFormat.caption:=sDefaultOutputFormat;

  cbFormat.Items[0]:=sDefault;
  cbFormat.Items[1]:=sXML;
  cbFormat.Items[2]:=sPlainText;
  cbFormat.Items[3]:=sPlainTextWithoutTimi;
  cbFormat.Items[4]:=sLaTeX;
end;

procedure TConsoleTestRunnerOptionsForm.LoadForm;

var
  Cfg: TConfigStorage;
  I : integer;

begin
  Cfg:=GetIDEConfigStorage('lazfpcunit.xml',True);
  try
    RunAllTests:=cfg.GetValue(KeyRunAllTests,True);
    UseTestInsight:=cfg.GetValue(KeyUseTestInsight,False);
    CreateTestCase:=cfg.GetValue(KeyCreateTestCase,True);
    I:=GetEnumValue(TypeInfo(TDefaultFormat),Cfg.GetValue(KeyDefaultFormat,'dfPlainText'));
    if I<>-1 then
      DefaultFormat:=TDefaultFormat(I);
  finally
    Cfg.Free;
  end;
end;

procedure TConsoleTestRunnerOptionsForm.SaveForm;

var
  Cfg: TConfigStorage;

begin
  Cfg:=GetIDEConfigStorage('lazfpcunit.xml',True);
  Try
    cfg.SetDeleteValue(KeyRunAllTests,RunAllTests,True);
    cfg.SetDeleteValue(KeyUseTestInsight,UseTestInsight,False);
    cfg.SetDeleteValue(KeyCreateTestCase,CreateTestCase,True);
    Cfg.SetDeleteValue(KeyDefaultFormat,GetEnumName(TypeInfo(TDefaultFormat),Ord(DefaultFormat)),
                                        GetEnumName(TypeInfo(TDefaultFormat),Ord(dfPlainText)));
    Cfg.WriteToDisk;
  finally
    Cfg.Free;
  end;
end;


function TConsoleTestRunnerOptionsForm.GetCheckBox(aIndex: Integer): TCheckBox;

begin
  Case aIndex of
    1 : Result:=cbRunAllTests;
    2 : Result:=cbCreateTestCase;
    3 : Result:=cbTestInsight;
  else
    Result:=Nil;
  end;
end;


function TConsoleTestRunnerOptionsForm.GetCb(aIndex: Integer): Boolean;

begin
  Result:=GetCheckBox(aIndex).Checked;
end;


function TConsoleTestRunnerOptionsForm.GetDefaultFormat: TDefaultFormat;

begin
  if cbFormat.ItemIndex=-1 then
    Result:=dfDefault
  else
    Result:=TDefaultFormat(cbFormat.ItemIndex)
end;


procedure TConsoleTestRunnerOptionsForm.SetCB(aIndex: Integer; aValue: Boolean);

begin
  GetCheckBox(aIndex).Checked:=aValue;
end;


procedure TConsoleTestRunnerOptionsForm.SetDefaultFormat(AValue: TDefaultFormat);

begin
  cbFormat.ItemIndex:=Ord(aValue);
end;

end.

