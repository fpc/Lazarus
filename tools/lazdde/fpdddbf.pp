{
 Copyright (c) 2007 by Michael Van Canneyt.

 *****************************************************************************
 *                                                                           *
 *  See the file COPYING.modifiedLGPL, included in this distribution,        *
 *  for details about the copyright.                                         *
 *                                                                           *
 *  This program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
 *                                                                           *
 *****************************************************************************
}
unit fpddDbf;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, db,dbf, fpdatadict;
  
Type

  { TDBFDDimporter }

  { TDBFDDEngine }

  TDBFDDEngine = Class(TFPDDEngine)
  Private
    FDBF : TDBF;
  Public
    Procedure Disconnect ; override;
    Function Connect(const AConnectString : String) : Boolean; override;
    Function GetTableList(List : TStrings) : Integer; override;
    Function ImportFields(Table : TDDTableDef) : Integer; override;
    Function ViewTable(Const TableName: String; DatasetOwner : TComponent) : TDataset; override;
    Class function Description : string; override;
    Class function DBType : String; override;
    Class function EngineCapabilities : TFPDDEngineCapabilities; override;
  end;
  
Procedure InitDBFImporter;
Procedure DoneDBFImporter;

implementation

procedure TDBFDDEngine.Disconnect;
begin
  FConnectString:='';
  FConnected:=False;
  FreeAndNil(FDBF);
end;

function TDBFDDEngine.Connect(const AConnectString : String): Boolean;
begin
  FDBF:=TDBF.Create(Self);
  FDBF.FilePath:=AConnectString;
  FConnected:=True;
  FConnectString:=AConnectString;
  Result:=True;
end;

Function TDBFDDEngine.GetTableList(List: TStrings) : Integer;

Var
  Info : TSearchrec;
  FN : String;
 
begin
  Result:=0;
  If Assigned(FDBF) then
    begin
    FN:=IncludeTrailingPathDelimiter(FDBF.FilePath);
    If FindFirst(FN+'*.dbf',0,Info)=0 then
      try
        Repeat
          inc(Result);
          If Assigned(List) then
            List.Add(info.name);
        Until (FindNext(Info)<>0);
      finally
        FindClose(Info);
      end;
    end;
end;

Function TDBFDDEngine.ImportFields(Table: TDDTableDef) : Integer;
begin
  Result:=0;
  if Assigned(FDBF) then
    begin
    FDBF.TableName:=Table.TableName;
    FDBF.Open;
    Try
      Table.ImportFromDataset(FDBF);
    Finally
      FDBF.Close;
    end;
    end;
end;

function TDBFDDEngine.ViewTable(const TableName: String;
  DatasetOwner: TComponent): TDataset;
  
Var
  D : TDBF;
  
begin
  If DatasetOwner=Nil then
   DatasetOwner:=Self;
  D:=TDBF.Create(DatasetOwner);
  D.FilePath:=FDBF.FilePath;
  D.TableName:=TableName;
  Result:=D;
end;

Class function TDBFDDEngine.Description: string;
begin
  Result:='DBase database dictionary importer';
end;

class function TDBFDDEngine.DBType: String;
begin
  Result:='DBase files';
end;

class function TDBFDDEngine.EngineCapabilities: TFPDDEngineCapabilities;
begin
  Result:=[ecImport,ecViewTable];
end;

Procedure InitDBFImporter;

begin
  RegisterDictionaryEngine(TDBFDDEngine);
end;

Procedure DoneDBFImporter;

begin
  UnRegisterDictionaryEngine(TDBFDDEngine);
end;

Initialization
  InitDBFImporter;
Finalization
  DoneDBFImporter;
end.

