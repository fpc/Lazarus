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
  dialogs, fpjsondataset, Grids;

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

  TJSONReportDataHandler = Class(TDesignReportDataHandler)
    Function CreateDataset(AOwner : TComponent; AConfig : TJSONObject) : TDataset; override;
    Function CreateConfigFrame(AOwner : TComponent) : TReportDataConfigFrame; override;
    Class Function CheckConfig(AConfig: TJSONObject): String; override;
    Class Function DataType : String; override;
    Class Function DataTypeDescription : String; override;
    Class Function GetDataFromFile(aFileName : String) : TJSONData;
    Class Function GetDataFromURL(aURL : String) : TJSONData;
  end;

Type
  TDataForm = (dfObject,dfArray);

  { TMyJSONDataset }

  TMyJSONDataset = class(TBaseJSONDataSet)
  private
    FDataForm: TDataForm;
    FDataPath: String;
    FFileNAme: String;
    FMaxStringFieldSize: Integer;
    FURL: String;
    FJSON : TJSONData;
  Public
    Constructor Create(AOwner : TComponent); override;
    Destructor Destroy; override;
    procedure InternalClose; override;
    Procedure InternalOpen; override;
    Procedure MetaDataToFieldDefs; override;
    Function CreateFieldMapper : TJSONFieldMapper; override;
    property DataForm : TDataForm Read FDataForm Write FDataForm;
    Property MetaData;
    Property FileName : String Read FFileNAme Write FFileName;
    Property URL : String Read FURL Write FURL;
    Property DataPath : String Read FDataPath Write FDataPath;
    Property MaxStringFieldSize : Integer Read FMaxStringFieldSize Write FMaxStringFieldSize;
  end;

Type
  TRecordDesc = Record
    name : string;
    fieldtype : TFieldType;
  end;
  TRecordDescArray = Array of TRecordDesc;

Function DetectJSONStruct(J : TJSONData; StartPath : String; Out APath : String; Out Records : TRecordDescArray; Out ArrayBased : Boolean) : Boolean;
Function TryStringToFieldType(S : String; out Ft : TFieldType; Strict : Boolean) : Boolean;

implementation

uses typinfo,jsonparser,uriparser, fphttpclient;

{$R *.lfm}
Resourcestring
  SErrNeedFileNameOrURL = 'Need a file name or URL';
  SErrNeedFileName = 'Need a file name';
  SErrNeedURL = 'Need a URL';
  SErrNeedFields = 'No fields have been defined';
  SErrFileNameDoesNotExist = 'Filename does not exist: "%s"';
  SErrInvalidProtocol = 'URL has invalid protocol: "%s". Only http and https are supported';
  SErrNotArrayData = 'Data at "%s" does not exist or is not an array.';
  SErrNoDataFound = 'JSON data was found, but no valid data structure was detected.';
  SErrUnsupportedJSONFieldType = 'Unsupported JSON field type: "%s"';
  SErrEmptyFieldsNotAllowed = 'Empty fields are not allowed (field: %d)';


{ TDBFReportDataFrame }

Const
  keyFileName  = 'filename';
  keyMetaData  = 'meta';
  keyURL       = 'url';
  keyDataForm  = 'dataform';
  keyDataPath  = 'path';
  keyFields    = 'fields';
  keyFieldType = 'type';
  keyFieldName = 'name';

Function FieldTypeToString(Ft : TFieldType; Strict : Boolean) : String;

begin
  Case FT of
   ftstring : Result:='string';
   ftBoolean : Result:='boolean';
   ftInteger : Result:='integer';
   ftLargeint : Result:='largeint';
   ftFloat : Result:='float';
 else
   if Strict then
     Raise EDatabaseError.CreateFmt(SErrUnsupportedJSONFieldType,[GetEnumName(TypeInfo(TFieldType),Ord(FT))]);
   result:='string';
 end;
end;

Function TryStringToFieldType(S : String; out Ft : TFieldType; Strict : Boolean) : Boolean;

begin
  Result:=True;
  Case lowercase(s) of
   'string' : ft:=ftstring;
   'boolean': ft:=ftBoolean;
   'integer': ft:=ftInteger;
   'bigint' : ft:=ftLargeint;
   'largeint' : ft:=ftLargeint ;
   'float' : ft:=ftFloat;
  else
    if Strict then
      Result:=False
    else
      ft:=ftString;
  end;
end;

Function DetectJSONStruct(J : TJSONData; StartPath : String; Out APath : String; Out Records : TRecordDescArray; Out ArrayBased : Boolean) : Boolean;

Var
  A : TJSONArray;
  D : TJSONData;
  O : TJSONObject;
  I,C : Integer;

begin
  J:=J.FindPath(StartPath);
  A:=Nil;
  if J is TJSONArray then
    begin
    APath:=StartPath;
    A:=J as TJSONArray;
    end
  else
    begin
    If J is TJSONObject then
      begin
      O:=J as TJSONObject;
      I:=0;
      While (A=Nil) and (I<J.Count) do
        begin
        If J.Items[i].JSONType=jtArray then
          begin
          A:= J.Items[i] as TJSONArray;
          APath:=O.Names[I];
          If StartPath<>'' then
            APath:=StartPath+'.'+APath;
          end;;
        Inc(I);
        end;
      end;
    end;
  Result:=Assigned(A) and (A.Count>0) and (A.Items[0].JSONType in [jtArray,jtObject]);
  if Result then
    begin
    D:=A.items[0];
    if D is TJSONObject then
      O:=D as TJSONObject
    else
      O:=Nil;
    ArrayBased:=O=Nil;
    SetLength(Records,D.Count);
    C:=0;
    for I:=0 to D.Count-1 do
      begin
      Records[C].FieldType:=ftUnknown;
      Case D.Items[C].JSONType of
        jtString : Records[C].FieldType:=ftString;
        jtNumber :
          Case TJSONNumber(D.Items[C]).NumberType of
            ntFloat:  Records[C].fieldtype:=ftFloat;
            ntInteger:  Records[C].fieldtype:=ftInteger;
          else
            Records[C].fieldtype:=ftLargeInt;
          end;
        jtBoolean :  Records[C].fieldtype:=ftBoolean;
        jtNull : Records[C].fieldtype:=ftString;
      end;
      if (Records[C].FieldType<>ftUnknown) then
        begin
        if Assigned(O) then
          Records[C].Name:=O.Names[i]
        else
          Records[C].Name:='Column'+IntToStr(I);
        Inc(C);
        end;
      end;
    SetLength(Records,C);
    end
  else  If J is TJSONObject then
    begin
    // Check members one by one
    O:=J as TJSONObject;
    I:=0;
    While Not result and (I<J.Count) do
      begin
      If J.Items[i].JSONType=jtObject then
        begin
        Result:=DetectJSONStruct(J,O.Names[I],APath,Records,ArrayBased);
        end;
      Inc(I);
      end;
    end;
end;

Type
  TMyJSONObjectFieldMapper = Class(TJSONFieldMapper)
    procedure SetJSONDataForField(Const FieldName : String; FieldIndex : Integer; Row,Data : TJSONData); override;
    Function GetJSONDataForField(Const FieldName : String; FieldIndex : Integer; Row : TJSONData) : TJSONData; override;
    Function CreateRow : TJSONData; override;
  end;

procedure TMyJSONObjectFieldMapper.SetJSONDataForField(const FieldName: String;
  FieldIndex: Integer; Row, Data: TJSONData);

begin
  Raise Exception.Create('Read-only data!');
end;

function TMyJSONObjectFieldMapper.GetJSONDataForField(const FieldName: String;
  FieldIndex: Integer; Row: TJSONData): TJSONData;

Var
  I : integer;

begin
  I:=(Row as TJSONObject).IndexOfName(FieldName);
  if I=-1 then
    Result:=Nil
  else
    Result:=Row.Items[i];
end;

function TMyJSONObjectFieldMapper.CreateRow: TJSONData;
begin
  Result:=TJSONObject.Create;
end;

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

{ TMyJSONDataset }

constructor TMyJSONDataset.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  MaxStringFieldSize:=1024;
  OwnsData:=False;
end;

destructor TMyJSONDataset.Destroy;
begin
  FreeAndNil(FJSON);
  // We own metadata
  Metadata.Free;
  inherited Destroy;
end;

procedure TMyJSONDataset.InternalClose;

begin
  Inherited;
  FreeAndNil(FJSON);
end;

procedure TMyJSONDataset.InternalOpen;

Var
  R : TJSONData;

begin
  FreeAndNil(FJSON);
  if (URL<>'') then
    FJSON:=TJSONReportDataHandler.GetDataFromURL(URL)
  else
    FJSON:=TJSONReportDataHandler.GetDataFromFile(FileName);
  R:=FJSON.FindPath(DataPath);
  if not (R is TJSONArray) then
    Raise EDatabaseError.CreateFmt(SErrNotArrayData,[DataPath]);
  Rows:=R as TJSONArray;
  inherited InternalOpen;
end;


procedure TMyJSONDataset.MetaDataToFieldDefs;

Var
  F : TJSONarray;
  I : Integer;
  O : TJSONObject;
  Ft : TFieldType;

begin
  FieldDefs.Clear;
  F:=Metadata.get(keyFields,TJSONArray(Nil));
  if not Assigned(F) then
    exit;
  For I:=0 to F.Count-1 do
    begin
    O:=F.Objects[i];
    if TryStringToFieldType(O.strings[keyFieldType],ft,false) then
      if ft=ftString then
        FieldDefs.Add(O.strings[keyFieldName],FT,MaxStringFieldSize,False)
      else
        FieldDefs.Add(O.strings[keyFieldName],FT);
    end;
end;

function TMyJSONDataset.CreateFieldMapper: TJSONFieldMapper;
begin
  if DataForm = dfObject then
    begin
    Result:=TMyJSONObjectFieldMapper.Create;
    end
  else
    begin
    Result:=TJSONArrayFieldMapper.Create;
    end
end;

function TJSONReportDataHandler.CreateDataset(AOwner: TComponent; AConfig: TJSONObject): TDataset;

Var
  C : TMyJSONDataset;
  O : TJSONObject;

begin
//  Writeln('Starting dataset',aConfig.FormatJSON());
  C:=TMyJSONDataset.Create(AOWner);
  C.FileName:=AConfig.get(keyFileName,'');
  C.URL:=AConfig.get(keyURL,'');
  O:=AConfig.get(keyMetaData,TJSONObject(Nil));
  if Assigned(O) then
    C.MetaData:=O.Clone as TJSONObject
  else
    Raise EDatabaseError.Create('No metadata');
  if AConfig.get(keyDataForm,'object')='object' then
    C.DataForm:=dfObject
  else
    C.DataForm:=dfArray;
  C.DataPath:=AConfig.get(keyDataPath,'');;
  Result:=C;
end;

function TJSONReportDataHandler.CreateConfigFrame(AOwner: TComponent): TReportDataConfigFrame;
begin
  Result:=TJSONReportDataConfigFrame.Create(AOWner);
end;

class function TJSONReportDataHandler.CheckConfig(AConfig: TJSONObject): String;

Var
  FN,URL : UTF8String;
  URI : TURI;
  O : TJSONObject;
  A : TJSONArray;
  I : Integer;
  Ft : TFieldType;
  V : String;

begin
  Result:='';
  FN:=AConfig.Get(KeyFileName,'');
  if (FN='') then
    begin
    URL:=AConfig.Get(KeyURL,'');
    URI:=parseuri(URL,'http',80,True);
    case lowercase(uri.Protocol) of
     'https' : ;
     'http' : ;
     '' : ;
    else
      Result:=Format(SErrInvalidProtocol,[URI.Protocol]);
    end
    end
  else if FN='' then
    Result:=SErrNeedFileNameOrURL
  else if not FileExists(FN) then
    Result:=Format(SErrFileNameDoesNotExist,[FN])
  else
    begin
    O:=aConfig.get(keyMetaData,TJSONObject(Nil));
    if not Assigned(O) then
      Result:=SErrNeedFields
    else
      begin
      A:=O.get(keyFields,TJSONArray(Nil));
      if (A=Nil) or (A.Count=0) then
        Result:=SErrNeedFields
      else
        begin
        I:=0;
        While (Result='') and (I<A.Count) do
          begin
          if A.Types[i]=jtObject then
            begin
            O:=A.Objects[i];
            if (O.Get(KeyfieldName,'')='') then
              Result:=Format(SErrEmptyFieldsNotAllowed,[I+1])
            else
              begin
              V:=O.Get(KeyFieldType,'');
              if not TryStringToFieldType(V,ft,True) then
                 Result:=Format(SErrUnsupportedJSONFieldType,[V]);
              end;
            end;
          Inc(I);
          end;
        end;
      end;
    end;
end;

class function TJSONReportDataHandler.DataType: String;
begin
  Result:='JSON'
end;

class function TJSONReportDataHandler.DataTypeDescription: String;
begin
  Result:='JSON data';
end;

class function TJSONReportDataHandler.GetDataFromFile(aFileName: String): TJSONData;

Var
  F : TFileStream;
begin
  F:=TFileStream.Create(aFileName,fmOpenRead or fmShareDenyWrite);
  try
    Result:=GetJSON(F);
  finally
    F.Free;
  end;
end;

class function TJSONReportDataHandler.GetDataFromURL(aURL: String): TJSONData;

Var
  S : TStringStream;
  URI : TURI;

begin
  S:=TStringStream.Create('');
  try
    URI:=ParseURI(aURL,False);
    if (URI.protocol='') then
      URI.protocol:='http';
    TFPHTTPClient.SimpleGet(EncodeURI(URI),S);
    S.Position:=0;
    Result:=getJSON(S);
  finally
    S.Free;
  end;
end;

initialization
  TJSONReportDataHandler.RegisterHandler;
end.

