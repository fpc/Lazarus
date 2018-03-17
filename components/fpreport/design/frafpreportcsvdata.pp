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
unit frafpreportcsvdata;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, EditBtn, StdCtrls, fpjson, db, fpreportdata, fpreportdesignreportdata;

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


implementation

uses fpreportdatacsv;

{$R *.lfm}

Resourcestring
  SErrNeedFieldNames = 'Need at least one field name';
  SErrInvalidFieldName = 'Invalid field name: "%s"';


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
  TCSVReportDataHandler.RegisterConfigClass(TTCSVReportDataFrame);
end.

