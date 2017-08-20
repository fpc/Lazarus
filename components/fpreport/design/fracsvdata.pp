{
    This file is part of the Free Component Library.
    Copyright (c) 2017 Michael Van Canneyt, member of the Free Pascal development team

    Frame to configure a report CVS dataset.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit fracsvdata;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, EditBtn, StdCtrls, fpjson, db, designreportdata;

type
  TFrame = TReportDataConfigFrame;
  { TTCSVReportDataFrame }

  TTCSVReportDataFrame = class(TFrame)
    EDelimiter: TEdit;
    EQuoteChar: TEdit;
    FEData: TFileNameEdit;
    Label1: TLabel;
    LDelimiter: TLabel;
    LExplain: TLabel;
    LEFileName: TLabel;
    MFieldNames: TMemo;
    RBFirstLineField: TRadioButton;
    RBUseTheseFieldNames: TRadioButton;
    procedure MFieldNamesEnter(Sender: TObject);
  private

  public
    Procedure GetConfig(aConfig : TJSONObject); override;
    Procedure SetConfig(aConfig : TJSONObject); override;
    Function SaveNotOKMessage: String; override;
  end;

  { TCSVReportDataHandler }

  TCSVReportDataHandler = Class(TDesignReportDataHandler)
    Function CreateDataset(AOwner : TComponent; AConfig : TJSONObject) : TDataset; override;
    Function CreateConfigFrame(AOwner : TComponent) : TReportDataConfigFrame; override;
    Class Function CheckConfig(AConfig: TJSONObject): String; override;
    Class Function DataType : String; override;
    Class Function DataTypeDescription : String; override;
  end;

implementation

uses bufdataset, csvdataset;

{$R *.lfm}

Resourcestring
  SErrNeedFileName = 'Need a CSV file name';
  SErrNeedFieldNames = 'Need at least one field name';
  SErrInvalidFieldName = 'Invalid field name: "%s"';
  SFileNameDoesNotExist = 'Filename does not exist: "%s"';

{ TCSVReportDataHandler }

Const
  keyFileName = 'filename';
  keyFirstLineHasFieldNames = 'firstLineHasFieldNames';
  keyCustomFieldNames = 'customFieldNames';
  keyDelimiter = 'delimiter';
  keyQuoteChar = 'quoteChar';

  DefFirstLineFieldNames = True;
  DefDelimiter = ',';
  DefQuoteChar = '"';

Type

  { TMyCSVDataset }

  TMyCSVDataset = Class(TCSVDataset)
  private
    FCSVFileName: String;
  Protected
    function GetPacketReader(const Format: TDataPacketFormat; const AStream: TStream): TDataPacketReader; override;
    Procedure InternalOpen; override;
  Public
    Property CSVFileName : String Read FCSVFileName Write FCSVFileName;
  end;

{ TMyCSVDataset }

function TMyCSVDataset.GetPacketReader(const Format: TDataPacketFormat; const AStream: TStream): TDataPacketReader;
begin
  Result:=inherited GetPacketReader(Format, AStream);
  if (Result is TCSVDataPacketReader) and (FieldDefs.Count>0) then
     TCSVDataPacketReader(Result).CreateFieldDefs:=FieldDefs;
end;

procedure TMyCSVDataset.InternalOpen;

begin
  FileName:=CSVFileName;
  Inherited;
  FileName:='';
end;

function TCSVReportDataHandler.CreateDataset(AOwner: TComponent; AConfig: TJSONObject): TDataset;

Var
  C : TMyCSVDataset;
  A : TJSONArray;
  I : Integer;

begin
  C:=TMyCSVDataset.Create(AOWner);
  C.CSVOptions.FirstLineAsFieldNames:=AConfig.Get(keyFirstLineHasFieldNames,DefFirstLineFieldNames);
  C.CSVOptions.Delimiter:=AConfig.Get(KeyDelimiter,defDelimiter)[1];
  C.CSVOptions.quoteChar:=AConfig.Get(KeyQuoteChar,defQuoteChar)[1];
  if not C.CSVOptions.FirstLineAsFieldNames then
    begin
    A:=AConfig.Get(keyCustomFieldNames,TJSONArray(Nil));
    If Assigned(A) then
      For I:=0 to A.Count-1 do
        C.FieldDefs.Add(A.Strings[i],ftString,255);
    end;
  C.ReadOnly:=True;
  C.CSVFileName:=AConfig.Get(KeyFileName,'');
  Result:=C;
end;

function TCSVReportDataHandler.CreateConfigFrame(AOwner: TComponent): TReportDataConfigFrame;
begin
  Result:=TTCSVReportDataFrame.Create(AOWner);
end;

class function TCSVReportDataHandler.CheckConfig(AConfig: TJSONObject): String;

Var
  FN : UTF8String;

begin
  Result:='';
  FN:=AConfig.Get(KeyFileName,'');
  if FN='' then
    Result:=SErrNeedFileName
  else if not FileExists(FN) then
    Result:=Format(SFileNameDoesNotExist,[FN]);
end;

class function TCSVReportDataHandler.DataType: String;
begin
  Result:='CSV'
end;

class function TCSVReportDataHandler.DataTypeDescription: String;
begin
  Result:='Comma-separated values text file';
end;

{ TTCSVReportDataFrame }

procedure TTCSVReportDataFrame.MFieldNamesEnter(Sender: TObject);
begin
  RBUseTheseFieldNames.Checked:=True;
end;


procedure TTCSVReportDataFrame.SetConfig(aConfig: TJSONObject);

Var
  A : TJSONArray;
  I : Integer;

begin
  FEData.FileName:=aConfig.Get(keyFileName,'');
  EDelimiter.Text:=aConfig.Get(keyDelimiter,defDelimiter);
  EQuoteChar.Text:=aConfig.Get(keyQuoteChar,defQuoteChar);
  RBFirstLineField.Checked:=aConfig.Get(keyfirstLineHasFieldNames,DefFirstLineFieldNames);
  if not RBFirstLineField.Checked then
    begin
    A:=aConfig.Get(keyCustomFieldNames,TJSONArray(Nil));
    if Assigned(A) then
      begin
      For I:=0 to A.Count-1 do
        MFieldNames.Lines.Add(A.Strings[i]);
      end;
    end;
end;


function TTCSVReportDataFrame.SaveNotOKMessage: String;

Var
  I : Integer;

begin
  Result:='';
  if FEData.FileName='' then
    Result:=SErrNeedFileName
  else If (not RBFirstLineField.Checked) then
    begin
    if (MFieldNames.Lines.Count=0) then
      Result:=SErrNeedFieldNames
    else
      for I:=0 to MFieldNames.Lines.Count-1 do
        begin
        If not IsValidIdent(MFieldNames.Lines[i]) then
          Result:=Format(SErrInvalidFieldName,[MFieldNames.Lines[i]])
        end
    end
end;

procedure TTCSVReportDataFrame.GetConfig(aConfig: TJSONObject);

Var
  A : TJSONArray;
  I : Integer;

begin
  aConfig.Strings[KeyFilename]:=FEData.FileName;
  aConfig.Booleans[KeyFirstLineHasFieldNames]:=RBFirstLineField.Checked;
  aConfig.Strings[keyDelimiter]:=EDelimiter.Text;
  aConfig.Strings[keyQuoteChar]:=EQuoteChar.Text;
  if RBFirstLineField.Checked then
    begin
    i:=aConfig.IndexOfName(keyCustomFieldNames);
    if I<>-1 then
      aConfig.Delete(I);
    end
  else
    begin
    A:=TJSONArray.Create;
    aConfig.arrays[keyCustomFieldNames]:=A;
    For I:=0 to MFieldNames.Lines.Count-1 do
      A.Add(MFieldNames.Lines[i]);
    end;
end;

initialization
  TCSVReportDataHandler.RegisterHandler;
end.

