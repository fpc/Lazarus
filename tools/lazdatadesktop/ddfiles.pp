{
 Copyright (c) 2007 by Michael Van Canneyt.

 *****************************************************************************
 *                                                                           *
 *  See the file COPYING.modifiedLGPL.txt, included in this distribution,    *
 *  for details about the copyright.                                         *
 *                                                                           *
 *  This program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
 *                                                                           *
 *****************************************************************************
}
unit ddfiles;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, inifiles, inicol;
  
Type
  { TRecentItem }

  TRecentItem = Class(TNamedIniCollectionItem)
  private
    FLastUse: TDateTime;
    FUseCount: Integer;
  Public
    Procedure SaveToIni(Ini: TCustomInifile; ASection : String); override;
    Procedure LoadFromIni(Ini: TCustomInifile; ASection : String); override;
    Procedure Use;
  Published
    Property LastUse : TDateTime Read FLastUse;
    Property UseCount : Integer Read FUseCount;
  end;

  { TRecentDataDict }
  
  TRecentDataDict = Class(TRecentItem)
  private
    FFileName: String;
  Public
    Procedure SaveToIni(Ini: TCustomInifile; ASection : String); override;
    Procedure LoadFromIni(Ini: TCustomInifile; ASection : String); override;
  Published
    Property FileName : String Read FFileName Write FFileName;
  end;

  { TRecentDataDicts }

  TRecentDataDicts = Class(TNamedIniCollection)
  private
    function GetDD(Index: Integer): TRecentDataDict;
    procedure SetDD(Index: Integer; const AValue: TRecentDataDict);
  Public
    Constructor Create(AItemClass: TCollectionItemClass);
    Function IndexOfFileName(AFileName : String) : Integer;
    Function FindFromUserData(UserData : TObject) : TRecentDataDict;
    Function FindFromFileName(AFileName : String) : TRecentDataDict;
    Function FindFromName(AName : String) : TRecentDataDict;
    Function AddDataDict(AFileName : String) : TRecentDataDict;
    Property DataDicts [Index: Integer] : TRecentDataDict Read GetDD Write SetDD; default;
  end;

  { TRecentConnection }

  TRecentConnection = Class(TRecentItem)
  private
    FConnectionString: String;
    FEngineName: String;
  Public
    Procedure SaveToIni(Ini: TCustomInifile; ASection : String); override;
    Procedure LoadFromIni(Ini: TCustomInifile; ASection : String); override;
  Published
    Property EngineName : String Read FEngineName Write FEngineName;
    Property ConnectionString : String Read FConnectionString Write FConnectionString;
  end;
  
  { TRecentConnections }

  TRecentConnections = Class(TNamedIniCollection)
  private
    function GetConnection(Index: Integer): TRecentConnection;
    procedure SetConnection(Index: Integer; const AValue: TRecentConnection);
  Public
    Constructor Create(AItemClass : TCollectionItemClass);
    Function IndexOfConnectionString(Const AConnectionString : String) : Integer;
    Function FindFromUserData(UserData : TObject) : TRecentConnection;
    Function FindFromConnection(Const AConnectionString : String) : TRecentConnection;
    Function FindFromName(Const AName : String) : TRecentConnection;
    Function AddConnection(Const AName: String) : TRecentConnection;
    Property Connections [Index: Integer] : TRecentConnection Read GetConnection Write SetConnection; default;
  end;

implementation

{ ---------------------------------------------------------------------
  TRecentDataDict
  ---------------------------------------------------------------------}

Const
  KeyFileName   = 'FileName';
  KeyLastUsed   = 'LastUsed';
  KeyUseCount   = 'UseCount';
  KeyConnection = 'Connection';
  KeyEngineName = 'EngineName';
  
procedure TRecentDataDict.SaveToIni(Ini: TCustomInifile; ASection: String);
begin
  Inherited;
  With Ini do
    begin
    WriteString(ASection,KeyFileName,self.FFileName);
    end;
end;

procedure TRecentDataDict.LoadFromIni(Ini: TCustomInifile; ASection: String);
begin
  Inherited;
  With Ini do
    begin
    Self.FFileName:=ReadString(ASection,KeyFileName,Self.FFileName);
    end;
end;

{ ---------------------------------------------------------------------
  TRecentDataDicts
  ---------------------------------------------------------------------}

function TRecentDataDicts.GetDD(Index: Integer): TRecentDataDict;
begin
  Result:=TRecentDataDict(Items[Index]);
end;

procedure TRecentDataDicts.SetDD(Index: Integer; const AValue: TRecentDataDict);
begin
  Items[Index]:=AValue;
end;

constructor TRecentDataDicts.Create(AItemClass: TCollectionItemClass);
begin
  inherited Create(AItemClass);
  FPrefix:='DataDict';
  FSectionPrefix:='DD';
end;

function TRecentDataDicts.IndexOfFileName(AFileName: String): Integer;
begin
  Result:=Count-1;
  While (Result>=0) and (Not SameFileName(GetDD(Result).FileName,AFileName)) do
    Dec(Result);
end;

function TRecentDataDicts.AddDataDict(AFileName: String): TRecentDataDict;
begin
  Result:=Add as TRecentDataDict;
  Result.FileName:=AFileName;
end;

function TRecentDataDicts.FindFromUserData(UserData: TObject): TRecentDataDict;

begin
  Result:=TRecentDataDict(FindByUserData(UserData));
end;

function TRecentDataDicts.FindFromFileName(AFileName: String): TRecentDataDict;

Var
  I : integer;

begin
  I:=IndexOfFileName(AFileName);
  If (I=-1) then
    Result:=Nil
  else
    Result:=GetDD(I);
end;

function TRecentDataDicts.FindFromName(AName: String): TRecentDataDict;

begin
  Result:=TRecentDataDict(FindByName(AName));
end;

{ ---------------------------------------------------------------------
  TRecentConnection
  ---------------------------------------------------------------------}


procedure TRecentConnection.SaveToIni(Ini: TCustomInifile; ASection: String);
begin
  Inherited;
  With Ini do
   begin
   WriteString(ASection,KeyConnection,FConnectionString);
   WriteString(ASection,KeyEngineName,FEngineName);
   end;
end;

procedure TRecentConnection.LoadFromIni(Ini: TCustomInifile; ASection: String);
begin
  Inherited;
  With Ini do
    begin
    FConnectionString:=ReadString(ASection,KeyConnection,FConnectionString);
    FEngineName:=ReadString(ASection,KeyEngineName,FEngineName);
    end;
end;

{ ---------------------------------------------------------------------
  TRecentConnections
  ---------------------------------------------------------------------}

function TRecentConnections.GetConnection(Index: Integer): TRecentConnection;
begin
  Result:=TRecentConnection(Items[Index]);
end;

procedure TRecentConnections.SetConnection(Index: Integer;
  const AValue: TRecentConnection);
begin
  Items[Index]:=AValue;
end;

constructor TRecentConnections.Create(AItemClass: TCollectionItemClass);
begin
  inherited Create(AItemClass);
  FPrefix:='Connection';
  FSectionPrefix:='CONN';
end;

function TRecentConnections.IndexOfConnectionString(const AConnectionString: String
  ): Integer;
begin
  Result:=Count-1;
  While (Result>=0) and (CompareText(GetConnection(Result).ConnectionString,AConnectionString)<>0) do
    Dec(Result);
end;

function TRecentConnections.AddConnection(Const AName: String): TRecentConnection;
begin
  Result:=Add as TRecentConnection;
  Result.Name:=AName;
end;

function TRecentConnections.FindFromUserData(UserData: TObject): TRecentConnection;
Var
  I : Integer;

begin
  I:=IndexOfUserData(UserData);
  If (I=-1) then
    Result:=Nil
  Else
    Result:=GetConnection(I);
end;

function TRecentConnections.FindFromConnection(Const AConnectionString: String): TRecentConnection;
Var
  I : Integer;

begin
  I:=IndexOfConnectionString(AConnectionString);
  If (I=-1) then
    Result:=Nil
  Else
    Result:=GetConnection(I);
end;

function TRecentConnections.FindFromName(Const AName: String): TRecentConnection;

begin
  Result:=TRecentConnection(FindByName(AName));
end;


{ TRecentItem }

procedure TRecentItem.SaveToIni(Ini: TCustomInifile; ASection: String);
begin
  With Ini do
    begin
    WriteInteger(ASection,KeyUseCount,FUseCount);
    WriteDateTime(ASection,KeyLastUsed,FLastUse);
    end;
end;

procedure TRecentItem.LoadFromIni(Ini: TCustomInifile; ASection: String);
begin
  With Ini do
    begin
    FUseCount:=ReadInteger(ASection,KeyUseCount,0);
    FLastUse:=ReadDateTime(ASection,KeyLastUsed,0);
    end;
end;

procedure TRecentItem.Use;
begin
  FLastUse:=Now;
  inc(FUseCount);
end;

end.

