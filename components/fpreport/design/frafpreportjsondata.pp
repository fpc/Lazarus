{
    This file is part of the Free Component Library.
    Copyright (c) 2017 Michael Van Canneyt, member of the Free Pascal development team

    Frame to configure a JSON report dataset.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit frafpreportjsondata;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, StdCtrls, EditBtn, Buttons, ActnList, ValEdit, fpjson, fpreportdesignreportdata, db,
  dialogs;

type
  TFrame = TReportDataConfigFrame ;
  { TJSONReportDataConfigFrame }

  TJSONReportDataConfigFrame = class(TFrame)
    ARefresh: TAction;
    ALJSON: TActionList;
    CBArrayBased: TCheckBox;
    EDataPath: TEdit;
    EURL: TEdit;
    FEData: TFileNameEdit;
    ILJSON: TImageList;
    LDataPath: TLabel;
    RBFile: TRadioButton;
    RBURL: TRadioButton;
    SBrefresh: TSpeedButton;
    VLEFields: TValueListEditor;
    procedure ARefreshExecute(Sender: TObject);
    procedure ARefreshUpdate(Sender: TObject);
    procedure EURLEditingDone(Sender: TObject);
    procedure EURLEnter(Sender: TObject);
    procedure FEDataEditingDone(Sender: TObject);
    procedure FEDataEnter(Sender: TObject);
    procedure VLEFieldsValidateEntry(sender: TObject; aCol, aRow: Integer; const OldValue: string; var NewValue: String);
  private

  public
    Procedure GetConfig(aConfig : TJSONObject); override;
    Procedure SetConfig(aConfig : TJSONObject); override;
    Function SaveNotOKMessage : String; override;
  end;

  { TJSONReportDataHandler }


implementation

uses fpreportdatajson;

{$R *.lfm}

{ TJSONReportDataConfigFrame }

procedure TJSONReportDataConfigFrame.ARefreshUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled:=(EURL.Text<>'') or (FEData.FileName<>'');
end;

procedure TJSONReportDataConfigFrame.EURLEditingDone(Sender: TObject);
begin
  if (EURL.Text<>'') then
    ARefreshExecute(Self);
end;

procedure TJSONReportDataConfigFrame.EURLEnter(Sender: TObject);
begin
  RBURL.Checked:=True;
end;

procedure TJSONReportDataConfigFrame.FEDataEditingDone(Sender: TObject);
begin
  if (FEData.FileName<>'') and FileExists(FEData.FileName) then
    ARefreshExecute(Self);
end;

procedure TJSONReportDataConfigFrame.FEDataEnter(Sender: TObject);
begin
  RBFile.Checked:=True;
end;

procedure TJSONReportDataConfigFrame.ARefreshExecute(Sender: TObject);

Var
  J : TJSONData;
  ArrayBased : Boolean;
  P : String;
  R : TRecordDescArray;
  I,II : Integer;

begin
  if RBURL.Checked then
    J:=TJSONReportDataHandler.GetDataFromURL(EURL.Text)
  else
    J:=TJSONReportDataHandler.GetDataFromFile(FEData.FileName);
  if Not DetectJSONStruct(J,EDataPath.Text,P,R,ArrayBased) then
    ShowMessage(SErrNoDataFound)
  else
    begin
    EDataPath.Text:=P;
    CBArrayBased.Checked:=ArrayBased;
    VLEFields.Strings.Clear;
    For I:=0 to Length(R)-1 do
      begin
      II:=VLEFields.Strings.Add(R[I].name+'='+FieldTypeToString(R[i].FieldType,False));
      VLEFields.ItemProps[II].EditStyle:=esPickList;
      VLEFields.ItemProps[II].PickList.CommaText:='string,boolean,integer,float,largeint';
      end;
    end;
end;

procedure TJSONReportDataConfigFrame.VLEFieldsValidateEntry(sender: TObject; aCol, aRow: Integer; const OldValue: string;
  var NewValue: String);

Var
  FT : TFieldType;

begin
  if aCol=1 then
    begin
    if not TryStringToFieldType(NewValue,Ft,True) then
      newvalue:=oldvalue;
    end
  else
    begin
    if Not IsValidIdent(NewValue) then
      NewValue:=OldValue;
    end;
end;

procedure TJSONReportDataConfigFrame.GetConfig(aConfig: TJSONObject);

Var
  M : TJSONArray;
  I : Integer;
  N,V : String;

begin
  if RBFile.Checked then
    begin
    aConfig.Strings[KeyFileName]:=FEData.FileName;
    aConfig.delete(KeyURL);
    end
  else
    begin
    aConfig.Strings[KeyURL]:=EURL.Text;
    aConfig.Delete(KeyFileName);
    end;
  aConfig.Strings[KeyDataPath]:=EDataPath.Text;
  if CBArrayBased.Checked then
    aConfig.Strings[keydataform]:='array'
  else
    aConfig.Strings[keydataform]:='object';
  M:=TJSONArray.Create;
  For I:=0 to VLEFields.Strings.Count-1 do
    begin
    VLEFields.Strings.GetNameValue(I,N,V);
    M.Add(TJSONOBject.Create([KeyFieldName,N,KeyFieldType,V]));
    end;
  aConfig.Objects[keyMetaData]:=TJSONObject.Create([keyFields,M]);
//  Writeln(aConfig.FormatJSON);
end;

procedure TJSONReportDataConfigFrame.SetConfig(aConfig: TJSONObject);

Var
  M : TJSONArray;
  O : TJSONObject;
  I,II : Integer;

begin
  FEData.FileName:=aConfig.Get(KeyFileName,'');
  EURL.Text:=aConfig.Get(KeyURL,'');
  if (FEData.FileName<>'') or (EURL.Text='') then
    RBFile.Checked:=True;
  EDataPath.Text:=aConfig.Get(KeyDataPath,'');
  CBArrayBased.Checked:=aConfig.Get(keydataform,'object')='array';
  M:=aConfig.get(keyMetaData,TJSONarray(Nil));
  if Assigned(M) then
    begin
    VLEFields.Strings.Clear;
    For I:=0 to M.Count-1 do
      begin
      O:=M.Objects[i];
      II:=VLEFields.Strings.Add(O.Get(keyFieldName,'')+'='+O.Get(keyFieldType,''));
      VLEFields.ItemProps[II].EditStyle:=esPickList;
      VLEFields.ItemProps[II].PickList.CommaText:='string,boolean,integer,float,largeint';
      end;
    end;
end;

function TJSONReportDataConfigFrame.SaveNotOKMessage: String;

Var
  I : Integer;
  N,V : String;
  ft : TFieldType;

begin
  Result:='';
  if RBFile.Checked then
    begin
    if (FEData.FileName='') then
      Result:=SErrNeedFileName
    else if Not FileExists(FEData.FileName) then
      Result:=Format(SErrFileNameDoesNotExist,[FEData.FileName])
    end
  else if RBURL.Checked and (EURL.Text='') then
    Result:=SErrNeedURL
  else if VLEFields.Strings.Count=0 then
    Result:=SErrNeedFields
  else
    begin
    I:=0;
    While (Result='') and (I<VLEFields.Strings.Count) do
      begin
      VLEFields.Strings.GetNameValue(I,N,V);
      if (N='') then
        Result:=Format(SErrEmptyFieldsNotAllowed,[I+1])
      else if not TryStringToFieldType(V,ft,False) then
        Result:=Format(SErrUnsupportedJSONFieldType,[V]);
      Inc(I);
      end;
    end;
end;

initialization
  TJSONReportDataHandler.RegisterConfigClass(TJSONReportDataConfigFrame);
end.

