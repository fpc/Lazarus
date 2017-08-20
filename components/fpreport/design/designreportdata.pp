{
    This file is part of the Free Component Library.
    Copyright (c) 2017 Michael Van Canneyt, member of the Free Pascal development team

    Base classes to manage a collection of report data loops.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

unit designreportdata;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, db, forms, fpjson, fpreport;

Type
  EDesignReportData = Class(Exception);

  { TDesignReportDataHandler }

  { TReportDataConfigFrame }

  TReportDataConfigFrame = Class(TFrame)
  Public
    Procedure GetConfig(aConfig : TJSONObject); virtual; abstract;
    Procedure SetConfig(aConfig : TJSONObject); virtual; abstract;
    Function SaveNotOKMessage : String; virtual;
  end;

  TDesignReportDataHandler = Class(TObject)
  Protected
    Class Function TypeList : TStrings;
  Public
    Class Procedure RegisterHandler;
    Class Procedure UnRegisterHandler;
    Class Function GetRegisteredTypes(AList : Tstrings) : Integer;
    Class Function GetTypeHandler(aTypeName : String) : TDesignReportDataHandler;
    // Override this to return a dataset which is owned by AOwner, and configured by AConfig.
    // The dataset must not be opened.
    Function CreateDataset(AOwner : TComponent; AConfig : TJSONObject) : TDataset; virtual; abstract;
    // Override this to return a frame which can be used to configure the dataset.
    Function CreateConfigFrame(AOwner : TComponent) : TReportDataConfigFrame; virtual; abstract;
    // Check if the configuration is valid. Return a string that describes the error(s)
    // If the return is an empty string, the data designer will not close.
    Class Function CheckConfig(AConfig : TJSONObject) : String; virtual;
    Class Function DataType : String; virtual; abstract;
    Class Function DataTypeDescription : String; virtual;
  end;
  TDesignReportDataHandlerClass = Class of TDesignReportDataHandler;

  { TDesignReportData }

  TDesignReportData = Class(TCollectionItem)
  private
    FConfig: TJSONObject;
    FDataType: String;
    FName: String;
    procedure SetConfig(AValue: TJSONObject);
    procedure SetName(AValue: String);
  Public
    Constructor Create(ACollection: TCollection); override;
    Destructor Destroy; override;
    Procedure Assign(Source : TPersistent); override;
    Procedure SaveToJSON(O : TJSONObject); virtual;
    procedure LoadFromJSON(O: TJSONObject); virtual;
    // Clone this
    Function Clone(aNewName : String) : TDesignReportData;
    // Create a dataset.
    Function CreateDataSet(AOwner : TComponent) : TDataset;
    // Create a configuration frame for this data.
    Function CreateConfigFrame(AOwner : TComponent) : TReportDataConfigFrame;
    // Check if the configuration is OK.
    Function Check : String;
  Published
    property Name : String Read FName Write SetName;
    Property DataType : String Read FDataType Write FDataType;
    Property Config : TJSONObject Read FConfig Write SetConfig;
  end;

  { TDesignReportDataCollection }

  TDesignReportDataCollection = Class(TCollection)
  private
    function GetD(Aindex : Integer): TDesignReportData;
    procedure SetD(Aindex : Integer; AValue: TDesignReportData);
  Public
    Function IndexOfName(const aName : String): Integer;
    Function FindDataByName(const aName : String): TDesignReportData;
    Function AddData(const aName : String) : TDesignReportData;
    Procedure SaveToJSON(O : TJSONObject);
    Procedure LoadFromJSON(O : TJSONObject);
    Property Data [Aindex : Integer] : TDesignReportData Read GetD Write SetD; default;
  end;


implementation

{ TDesignReportDataHandler }

Resourcestring
  SErrDuplicateData = 'Duplicate data set name: "%s"';
  SErrInvalidDataName = 'Invalid data set name: "%s"';
  SErrNeedName = 'Data set needs a name';
  SErrNeedDataType = 'Data set needs a type';
  SErrInvalidDataType = 'Invalid data type: "%s"';

Var
  FTypesList : TStrings;
Type

  { THDef }

  THDef = Class(TObject)
    TheClass : TDesignReportDataHandlerClass;
    Constructor Create(aClass : TDesignReportDataHandlerClass);
  end;

{ TReportDataConfigFrame }

function TReportDataConfigFrame.SaveNotOKMessage: String;
begin
  Result:='';
end;

{ THDef }

constructor THDef.Create(aClass: TDesignReportDataHandlerClass);
begin
  TheClass:=AClass;
end;

{ TDesignReportDataCollection }

function TDesignReportDataCollection.GetD(Aindex : Integer): TDesignReportData;
begin
  Result:=Items[Aindex] as TDesignReportData;
end;

procedure TDesignReportDataCollection.SetD(Aindex : Integer; AValue: TDesignReportData);
begin
  Items[Aindex]:=AValue;
end;

function TDesignReportDataCollection.IndexOfName(const aName: String): Integer;
begin
  Result:=Count-1;
  While (Result>=0) and (CompareText(AName,GetD(Result).Name)<>0) do
    Dec(Result);
end;

function TDesignReportDataCollection.FindDataByName(const aName: String): TDesignReportData;

var
  I : Integer;

begin
  I:=indexOfname(aName);
  if I=-1 then
    Result:=Nil
  else
    Result:=GetD(I);
end;

function TDesignReportDataCollection.AddData(const aName: String): TDesignReportData;

begin
  if (IndexOfName(aName)<>-1) then
    raise EReportError.CreateFmt(SErrDuplicateData, [aName]);
  Result:=add as TDesignReportData;
  Result.Name:=aName;
end;

procedure TDesignReportDataCollection.SaveToJSON(O: TJSONObject);

Var
  A : TJSONArray;
  DS : TJSONObject;
  I : Integer;

begin
  A:=TJSONArray.Create;
  O.Add('datasets',a);
  For I:=0 to Count-1 do
    begin
    DS:=TJSONObject.Create;
    A.Add(DS);
    Data[i].SaveToJSON(DS);
    end;
end;

procedure TDesignReportDataCollection.LoadFromJSON(O: TJSONObject);
Var
  A : TJSONArray;
  DS : TDesignReportData;
  I : Integer;

begin
  Clear;
  A:=O.Get('datasets',TJSONArray(Nil));
  if Assigned(A) then
    For I:=0 to A.Count-1 do
      if A.Types[i]=jtObject then
        begin
        DS:=Add as TDesignReportData;
        DS.LoadFromJSON(A.Objects[i]);
        end;
end;

Class function TDesignReportDataHandler.TypeList: TStrings;

Var
  SL : TStringList;

begin
  If (FTypesList=nil) then
    begin
    SL:=TStringList.Create;
    SL.Sorted:=True;
    SL.Duplicates:=dupError;
    SL.OwnsObjects:=True;
    FTypesList:=SL;
    end;
  Result:=FTypesList;
end;



class procedure TDesignReportDataHandler.RegisterHandler;
begin
  TypeList.AddObject(Self.DataType, THDef.Create(Self));
end;

class procedure TDesignReportDataHandler.UnRegisterHandler;

Var
  I : integer;

begin
  I:=TypeList.IndexOf(Self.DataType);
  if I<>-1 then
    TypeList.Delete(I);
end;


class function TDesignReportDataHandler.GetRegisteredTypes(AList: Tstrings): Integer;
begin
  // Don't use assign or addstrings, it will copy the THRefs too, possibly leading to errors
  AList.Text:=TypeList.Text;
  Result:=AList.Count;
end;

class function TDesignReportDataHandler.GetTypeHandler(aTypeName: String): TDesignReportDataHandler;

Var
  I : Integer;
  H : THDef;

begin
  I:=TypeList.IndexOf(ATypeName);
  if (I=-1) then
    Raise EDesignReportData.CreateFmt('Unknown report data type: %s',[aTypeName]);
  H:=THDef(TypeList.Objects[i]);
  Result:=H.TheClass.Create;
end;

class function TDesignReportDataHandler.CheckConfig(AConfig: TJSONObject): String;
begin
  Result:='';
end;

class function TDesignReportDataHandler.DataTypeDescription: String;
begin
  Result:=DataType
end;

{ TDesignReportData }

procedure TDesignReportData.SetConfig(AValue: TJSONObject);
begin
  if FConfig=AValue then Exit;
  FreeAndNil(FConfig);
  FConfig:=AValue.Clone as TJSONObject;
end;

procedure TDesignReportData.SetName(AValue: String);
begin
  if FName=AValue then Exit;
  {$IF FPC_FULLVERSION < 30002}
  if Not IsValidIdent(aValue) then
  {$ELSE}
  if Not IsValidIdent(aValue,True,true) then
  {$ENDIF}
    raise EReportError.CreateFmt(SErrInvalidDataName, [aValue]);
  if (Collection is TFPReportVariables) then
    If ((Collection as TFPReportVariables).FindVariable(AValue)<>Nil) then
      raise EReportError.CreateFmt(SErrDuplicateData, [aValue]);
  FName:=AValue;
end;

procedure TDesignReportData.Assign(Source: TPersistent);

Var
  D : TDesignReportData;
begin
  if (Source is TDesignReportData) then
    begin
    D:=Source as TDesignReportData;
    Config:=D.Config;
    Name:=D.Name;
    DataType:=D.DataType;
    end
  else
    inherited Assign(Source);
end;

procedure TDesignReportData.SaveToJSON(O: TJSONObject);
begin
  O.Add('name',Name);
  O.Add('type',DataType);
  O.Add('config',Config.Clone);
end;

procedure TDesignReportData.LoadFromJSON(O: TJSONObject);

Var
  C : TJSONObject;

begin
  Name:=O.Get('name',Name);
  DataType:=O.Get('type',DataType);
  C:=O.Get('config',TJSONObject(Nil));
  if Assigned(C) then
    Config:=C;
end;

function TDesignReportData.Clone(aNewName: String): TDesignReportData;
begin
  Result:=Collection.Add as TDesignReportData;
  Result.Assign(Self);
  Result.Name:=aNewName;
end;

function TDesignReportData.CreateDataSet(AOwner: TComponent): TDataset;

Var
  H : TDesignReportDataHandler;

begin
  H:=TDesignReportDataHandler.GetTypeHandler(DataType);
  try
    Result:=H.CreateDataset(AOwner,Config);
  finally
    H.Free;
  end;
end;

function TDesignReportData.CreateConfigFrame(AOwner: TComponent): TReportDataConfigFrame;

Var
  H : TDesignReportDataHandler;
begin
  H:=TDesignReportDataHandler.GetTypeHandler(DataType);
  try
    Result:=H.CreateConfigFrame(AOwner);
  finally
    H.Free;
  end;
end;

function TDesignReportData.Check: String;

Var
  H : TDesignReportDataHandler;

begin
  If (Name='') then
    Result:=SErrNeedName
  else if (DataType='') then
    Result:=SErrNeedDataType
  else
    begin
    H:=TDesignReportDataHandler.GetTypeHandler(DataType);
    if H=Nil then
      Result:=Format(SErrInvalidDataType,[DataType])
    else
      Result:=H.CheckConfig(Config);
    end;
end;

constructor TDesignReportData.Create(ACollection: TCollection);
begin
  inherited Create(ACollection);
  FConfig:=TJSONObject.Create;
end;

destructor TDesignReportData.Destroy;
begin
  FreeAndNil(FConfig);
  inherited Destroy;
end;

Finalization
  FreeAndNil(FTypesList)
end.

