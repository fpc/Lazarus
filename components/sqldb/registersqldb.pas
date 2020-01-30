{
 ***************************************************************************
 *                                                                         *
 *   This source is free software; you can redistribute it and/or modify   *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 *   This code is distributed in the hope that it will be useful, but      *
 *   WITHOUT ANY WARRANTY; without even the implied warranty of            *
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU     *
 *   General Public License for more details.                              *
 *                                                                         *
 *   A copy of the GNU General Public License is available on the World    *
 *   Wide Web at <http://www.gnu.org/copyleft/gpl.html>. You can also      *
 *   obtain it by writing to the Free Software Foundation,                 *
 *   Inc., 51 Franklin Street - Fifth Floor, Boston, MA 02110-1335, USA.   *
 *                                                                         *
 ***************************************************************************

  Author: Joost van der Sluis
  
  This unit registers the sqldb components of the FCL.
}
unit registersqldb;

{$mode objfpc}{$H+}

{$DEFINE HASIBCONNECTION}
{$DEFINE HASMYSQL55CONNECTION}
{$DEFINE HASMYSQL4CONNECTION}
{$DEFINE HASPQCONNECTION}
{$DEFINE HASSQLITE3CONNECTION}

{$IF (FPC_FULLVERSION>=30002) or not defined(win64)}
 {$DEFINE HASORACLECONNECTION}
{$ENDIF}

{$IF FPC_FULLVERSION >= 20601}
  // MS SQL Server and Sybase ASE connectors were introduced in the FPC 2.7 development branch,
  //  and backported to 2.6.1. Operating systems should match FPC packages\fcl-db\fpmake.pp	 
  {$IF DEFINED(BEOS) OR DEFINED(HAIKU) OR DEFINED(LINUX) OR DEFINED(FREEBSD) OR DEFINED (NETBSD) OR DEFINED(OPENBSD) OR DEFINED(WIN32) OR DEFINED(WIN64)}	 
    {$DEFINE HASMSSQLCONNECTION}	 
    {$DEFINE HASSYBASECONNECTION}	 
  {$ENDIF}
{$ENDIF}

{$IF FPC_FULLVERSION >= 20602} 
// These were backported to FPC 2.6.2
 {$DEFINE HASFBADMIN}
 {$DEFINE HASPQEVENT}
 {$DEFINE HASFBEVENT}
 {$DEFINE HASLIBLOADER}
{$ENDIF}

{$IF FPC_FULLVERSION >= 20603}
  {$DEFINE HASMYSQL56CONNECTION}
{$ENDIF}
{$IF FPC_FULLVERSION >= 20701}
  {$DEFINE HASMYSQL57CONNECTION}
{$ENDIF}

interface

uses
  Classes, SysUtils, db, sqldb,
  {$IFDEF HASIBCONNECTION}
    ibconnection,
  {$ENDIF}
  {$IFDEF HASMSSQLCONNECTION}
    // mssqlconn provide both MS SQL Server and Sybase ASE connectors.
    mssqlconn,
  {$ENDIF}
  odbcconn,
  {$IFDEF HASPQCONNECTION}
    pqconnection,
    {$IFDEF HASPQEVENT}
    pqteventmonitor,
    {$ENDIF}
  {$ENDIF}
  {$IFDEF HASORACLECONNECTION}
    oracleconnection,
  {$ENDIF}

  {$IFDEF HASMYSQL4CONNECTION}
    mysql40conn, mysql41conn,
  {$ENDIF}
  mysql50conn,
  mysql51conn,
  {$IFDEF HASMYSQL55CONNECTION}
    mysql55conn,
  {$ENDIF}
  {$IFDEF HASMYSQL56CONNECTION}
    mysql56conn,
  {$ENDIF}
  {$IFDEF HASMYSQL57CONNECTION}
    mysql57conn,
  {$ENDIF}
  {$IFDEF HASSQLITE3CONNECTION}
    sqlite3conn,
  {$ENDIF}
  {$IFDEF HASFBADMIN}
    fbadmin,
  {$ENDIF}
  {$IFDEF HASFBEVENT}
    fbeventmonitor,
  {$ENDIF}
  propedits,
  sqlstringspropertyeditordlg,
  controls,
  forms,
  {$IFDEF HASLIBLOADER}
    sqldblib,
  {$ENDIF}
  sqlscript, fpsqltree, fpsqlparser,
  LazarusPackageIntf,
  lazideintf,
  srceditorintf,
  ProjectIntf,
  IDEMsgIntf,
  IDEExternToolIntf,
  CodeCache,
  CodeToolManager;

Type
  { TSQLStringsPropertyEditor }

  TSQLStringsPropertyEditor = class(TStringsPropertyEditor)
  private
    procedure EditSQL;
  public
    procedure Edit; override;
    function CreateEnhancedDlg(s: TStrings): TSQLStringsPropertyEditorDlg; virtual;
    function GetAttributes: TPropertyAttributes; override;
  end;

  { TSQLFirebirdFileNamePropertyEditor }

  TSQLFirebirdFileNamePropertyEditor=class(TFileNamePropertyEditor)
  public
    function GetFilter: String; override;
    function GetInitialDirectory: string; override;
  end;

{$IFDEF HASSQLITE3CONNECTION}

  { TSQLSQLite3FileNamePropertyEditor }

  TSQLSQLite3FileNamePropertyEditor=class(TFileNamePropertyEditor)
  public
    function GetFilter: string; override;
    function GetInitialDirectory: string; override;
  end;

{$ENDIF}

  { TSQLFileDescriptor }

  TSQLFileDescriptor = class(TProjectFileDescriptor)
  public
    constructor Create; override;
    function GetLocalizedName: string; override;
    function GetLocalizedDescription: string; override;
    function GetResourceSource(const {%H-}ResourceName: string): string; override;
    function CreateSource(const {%H-}Filename, {%H-}SourceName,
                          {%H-}ResourceName: string): string; override;
  end;

  { TSQLDBConnectorTypePropertyEditor }

  TSQLDBConnectorTypePropertyEditor = class(TStringPropertyEditor)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure GetValues(Proc: TGetStrProc); override;
    procedure SetValue(const NewValue: ansistring); override;
  end;

{$IFDEF HASLIBLOADER}

  { TSQLDBLibraryLoaderLibraryNamePropertyEditor }

  TSQLDBLibraryLoaderLibraryNamePropertyEditor=class(TFileNamePropertyEditor)
  public
    function GetFilter: String; override;
  end;

{$ENDIF}

  TSQLSyntaxChecker = Class(TComponent)
  private
    FStatementCount,
    FSQLErr : Integer;
    FSFN: String;
    procedure CheckSQLStatement(Sender: TObject; Statement: TStrings; var StopExecution: Boolean);
  Public
    Procedure ShowMessage(Const Msg : String);
    Procedure ShowMessage(Const Fmt : String; Args : Array of const);
    Procedure ShowException(Const Msg : String; E : Exception);
    function CheckSQL(S : TStream): TModalResult;
    function CheckSource(Sender: TObject; var Handled: boolean): TModalResult;
    Property SourceFileName : String Read FSFN;
 end;

procedure Register;

implementation

{$R registersqldb.res}

uses dynlibs;

procedure RegisterUnitSQLdb;
begin
  RegisterComponents('SQLdb',[
    TSQLQuery,
    TSQLTransaction,
    TSQLScript,
    TSQLConnector
{$IFDEF HASMSSQLCONNECTION}                                
    ,TMSSQLConnection
{$ENDIF}
{$IFDEF HASSYBASECONNECTION}                                
    ,TSybaseConnection
{$ENDIF}                              
{$IFDEF HASPQCONNECTION}
    ,TPQConnection
  {$IFDEF HASPQEVENT}
      ,TPQTEventMonitor
  {$ENDIF}
{$ENDIF}
{$IFDEF HASORACLECONNECTION}
    ,TOracleConnection
{$ENDIF}
    ,TODBCConnection
{$IFDEF HASMYSQL4CONNECTION}
    ,TMySQL40Connection
    ,TMySQL41Connection
{$ENDIF}
    ,TMySQL50Connection
    ,TMySQL51Connection
{$IFDEF HASMYSQL55CONNECTION}
    ,TMySQL55Connection
{$ENDIF}
{$IFDEF HASMYSQL56CONNECTION}
    ,TMySQL56Connection
{$ENDIF}
{$IFDEF HASMYSQL57CONNECTION}
    ,TMySQL57Connection
{$ENDIF}
{$IFDEF HASSQLITE3CONNECTION}
    ,TSQLite3Connection
{$ENDIF}
{$IFDEF HASIBCONNECTION}
    ,TIBConnection
{$ENDIF}
{$IFDEF HASFBADMIN}
    ,TFBAdmin
{$ENDIF}
{$IFDEF HASFBEVENT}
    ,TFBEventMonitor
{$ENDIF}
{$IFDEF HASLIBLOADER}
    ,TSQLDBLibraryLoader
{$ENDIF}
    ]);
end;


Resourcestring
  SSQLScript     = 'SQL Script file';
  SSQLScriptDesc = 'Create a new SQL Script file';
  SSQLSource = 'Insert your SQL statements here';

  SFireBirdDatabases = 'Firebird databases';
  SSQLite3Databases = 'SQLite3 databases';
  SInterbaseDatabases = 'Interbase databases';
  SSQLStringsPropertyEditorDlgTitle = 'Editing %s';

  sLibraries = 'Shared libraries';

{ TSQLDBLibraryLoaderConnectionTypePropertyEditor }

function TSQLDBConnectorTypePropertyEditor.GetAttributes: TPropertyAttributes;
begin
  Result := [paMultiSelect, paSortList, paValueList, paRevertable];
end;

procedure TSQLDBConnectorTypePropertyEditor.GetValues(Proc: TGetStrProc);
Var
  L : TStringList;
  I : Integer;
begin
  L:=TStringList.Create;
  try
    GetConnectionList(L);
    for I:=0 to L.Count-1 do
      Proc(L[i]);
  finally
    L.Free;
  end;
end;

procedure TSQLDBConnectorTypePropertyEditor.SetValue(const NewValue: ansistring);
var
  Comp: TPersistent;
  Code: TCodeBuffer;
  ConnDef: TConnectionDef;
  SrcEdit: TSourceEditorInterface;
begin
  if not LazarusIDE.BeginCodeTools then
    Exit;
  SrcEdit := SourceEditorManagerIntf.ActiveEditor;
  if SrcEdit=nil then
    Exit;
  Code := TCodeBuffer(SrcEdit.CodeToolsBuffer);
  if Code = nil then
    Exit;
  Comp := GetComponent(0);
  if Comp is TSQLConnector then
  begin
    ConnDef := GetConnectionDef(NewValue);
    if Assigned(ConnDef) then
      CodeToolBoss.AddUnitToMainUsesSection(Code, ConnDef.UnitName, '');
  end;
  inherited;
end;

{$IFDEF HASLIBLOADER}
{ TSQLDBLibraryLoaderLibraryNamePropertyEditor }

function TSQLDBLibraryLoaderLibraryNamePropertyEditor.GetFilter: String;
begin
  Result := sLibraries+'|*.'+SharedSuffix;
  Result := Result+ '|'+ inherited GetFilter;
end;
{$ENDIF}

{ TSQLFirebirdFileNamePropertyEditor }

function TSQLFirebirdFileNamePropertyEditor.GetFilter: String;
begin
  Result := sFireBirdDatabases+' (*.fb;*.fdb)|*.fb;*.fdb';
  Result := Result + '|' + sInterbaseDatabases  +' (*.gdb)|*.gdb;*.GDB';
  Result:= Result+ '|'+ inherited GetFilter;
end;

function TSQLFirebirdFileNamePropertyEditor.GetInitialDirectory: string;
begin
  Result:= (GetComponent(0) as TSQLConnection).DatabaseName;
  Result:= ExtractFilePath(Result);
end;

{$IFDEF HASSQLITE3CONNECTION}

{ TSQLSQLite3FileNamePropertyEditor }

function TSQLSQLite3FileNamePropertyEditor.GetFilter: string;
begin
  Result := SSQLite3Databases+' (*.db;*.db3;*.sqlite;*.sqlite3)|*.db;*.db3;*.sqlite;*.sqlite3';
  Result:= Result+ '|'+ inherited GetFilter;
end;

function TSQLSQLite3FileNamePropertyEditor.GetInitialDirectory: string;
begin
  Result:= (GetComponent(0) as TSQLConnection).DatabaseName;
  Result:= ExtractFilePath(Result);
end;

{$ENDIF}

{ TSQLStringsPropertyEditor }

procedure TSQLStringsPropertyEditor.EditSQL;
var
  TheDialog:TSQLStringsPropertyEditorDlg;
  Strings  :TStrings;
  Query    :TSQLQuery;
begin
  Strings := TStrings(GetObjectValue);

  TheDialog := CreateEnhancedDlg(Strings);
  try
    TheDialog.Caption := Format(SSQLStringsPropertyEditorDlgTitle, [GetPropInfo^.Name]);
    if (GetComponent(0) is TSQLQuery) then
      begin
      Query := (GetComponent(0) as TSQLQuery);
      TheDialog.Connection  := (Query.DataBase as TSQLConnection);
      TheDialog.Transaction := (Query.Transaction as TSQLTransaction);
      end
    else if (GetComponent(0) is TSQLScript) then
      TheDialog.IsSQLScript:=True;
    if(TheDialog.ShowModal = mrOK)then
      begin
      Strings.Text := TheDialog.SQLEditor.Text;
      Modified;
      end;
  finally
    FreeAndNil(TheDialog);
  end;
end;

procedure TSQLStringsPropertyEditor.Edit;
begin
  try
    EditSQL;
  except
    on E:EDatabaseError do
    begin
      inherited Edit;
    end;
  end;
end;

//------------------------------------------------------------------------------------//
function TSQLStringsPropertyEditor.CreateEnhancedDlg(s: TStrings): TSQLStringsPropertyEditorDlg;
begin
  Result := TSQLStringsPropertyEditorDlg.Create(Application);
  Result.SQLEditor.Text := s.Text;
end;

//------------------------------------------------------------------//
function TSQLStringsPropertyEditor.GetAttributes: TPropertyAttributes;
begin
  Result := [paMultiSelect, paDialog, paRevertable, paReadOnly];
end;

{ TSQLSyntaxChecker }

procedure TSQLSyntaxChecker.CheckSQLStatement(Sender: TObject;
  Statement: TStrings; var StopExecution: Boolean);

Var
  P : TSQLParser;
  S : TMemoryStream;
  E : TSQLElement;

begin
  Inc(FStatementCount);
  S:=TMemoryStream.Create;
  try
    Statement.SaveToStream(S);
    S.Position:=0;
    P:=TSQLParser.Create(S);
    try
      try
        E:=P.Parse;
        E.Free;
        StopExecution:=False;
      except
        On E : Exception do
          begin
          ShowException('',E);
          inc(FSQLErr);
          end;
      end;
    finally
      P.Free;
    end;
  finally
    S.Free;
  end;

end;

procedure TSQLSyntaxChecker.ShowMessage(const Msg: String);
begin
  IDEMessagesWindow.AddCustomMessage(mluImportant,Msg,SourceFileName);
end;

procedure TSQLSyntaxChecker.ShowMessage(const Fmt: String; Args: array of const);
begin
  ShowMessage(Format(Fmt,Args));
end;

procedure TSQLSyntaxChecker.ShowException(const Msg: String; E: Exception);
begin
  If (Msg<>'') then
    ShowMessage(Msg+' : '+E.Message)
  else
    ShowMessage(Msg+' : '+E.Message);
end;

function TSQLSyntaxChecker.CheckSQL(S : TStream): TModalResult;

Var
  SQL : TEventSQLScript;

begin
  SQL:=TEventSQLScript.Create(Self);
  try
    FStatementCount:=0;
    FSQLErr:=0;
    SQL.UseSetTerm:=True;
    SQL.OnSQLStatement:=@CheckSQLStatement;
    SQL.Script.LoadFromStream(S);
    SQL.Execute;
    If (FSQLErr=0) then
      ShowMessage('SQL Syntax OK: %d statements',[FStatementCount])
    else
      ShowMessage('SQL Syntax: %d errors in %d statements',[FSQLErr,FStatementCount]);
  finally
    SQL.free;
  end;
  Result:=mrOK;
end;

function TSQLSyntaxChecker.CheckSource(Sender: TObject; var Handled: boolean
  ): TModalResult;

Var
  AE : TSourceEditorInterface;
  E : String;
  S : TStringStream;

begin
  try
  Handled:=False;
  result:=mrNone;
  AE:=SourceEditorManagerIntf.ActiveEditor;
  If (AE<>Nil) then
    begin
    E:=ExtractFileExt(AE.FileName);
    FSFN:=ExtractFileName(AE.FileName);
    Handled:=CompareText(E,'.sql')=0;
    If Handled then
      begin
      S:=TStringStream.Create(AE.SourceText);
      try
        Result:=CheckSQL(S);
      finally
        S.Free;
      end;
      end;
    end;
  except
    On E : Exception do
      ShowException('Error during syntax check',E);
  end;
end;

Var
  AChecker : TSQLSyntaxChecker;

procedure Register;
begin
{$IFDEF HASIBCONNECTION}
  RegisterPropertyEditor(TypeInfo(AnsiString),
    TIBConnection, 'DatabaseName', TSQLFirebirdFileNamePropertyEditor);
{$ENDIF}
{$IFDEF HASSQLITE3CONNECTION}
  RegisterPropertyEditor(TypeInfo(AnsiString),
    TSQLite3Connection, 'DatabaseName', TSQLSQLite3FileNamePropertyEditor);
{$ENDIF}
  RegisterPropertyEditor(TypeInfo(AnsiString),
    TSQLConnector, 'ConnectorType', TSQLDBConnectorTypePropertyEditor);
{$IFDEF HASLIBLOADER}
  RegisterPropertyEditor(TypeInfo(AnsiString),
    TSQLDBLibraryLoader, 'LibraryName', TSQLDBLibraryLoaderLibraryNamePropertyEditor);
  RegisterPropertyEditor(TypeInfo(AnsiString),
    TSQLDBLibraryLoader, 'ConnectionType', TSQLDBConnectorTypePropertyEditor);
{$endif}
  RegisterPropertyEditor(TypeInfo(AnsiString), TSQLConnection, 'Password', TPasswordStringPropertyEditor);
  RegisterPropertyEditor(TStrings.ClassInfo, TSQLQuery,  'SQL'      , TSQLStringsPropertyEditor);
  RegisterPropertyEditor(TStrings.ClassInfo, TSQLQuery,  'InsertSQL', TSQLStringsPropertyEditor);
  RegisterPropertyEditor(TStrings.ClassInfo, TSQLQuery,  'UpdateSQL', TSQLStringsPropertyEditor);
  RegisterPropertyEditor(TStrings.ClassInfo, TSQLQuery,  'DeleteSQL', TSQLStringsPropertyEditor);
  RegisterPropertyEditor(TStrings.ClassInfo, TSQLQuery,  'RefreshSQL',TSQLStringsPropertyEditor);
  RegisterPropertyEditor(TStrings.ClassInfo, TSQLScript, 'Script'   , TSQLStringsPropertyEditor);
  RegisterProjectFileDescriptor(TSQLFileDescriptor.Create);

  RegisterUnit('sqldb',@RegisterUnitSQLdb);
  AChecker:=TSQLSyntaxChecker.Create(Nil);
  LazarusIDE.AddHandlerOnQuickSyntaxCheck(@AChecker.CheckSource,False);
end;

{ TSQLFileDescriptor }

constructor TSQLFileDescriptor.Create;
begin
  inherited Create;
  Name:='SQL script file';
  DefaultFilename:='sqlscript.sql';
  DefaultResFileExt:='';
  DefaultFileExt:='.sql';
  VisibleInNewDialog:=true;
end;

function TSQLFileDescriptor.GetLocalizedName: string;
begin
  Result:=SSQLScript;
end;

function TSQLFileDescriptor.GetLocalizedDescription: string;
begin
  Result:=SSQLScriptDesc;
end;

function TSQLFileDescriptor.GetResourceSource(const ResourceName: string): string;
begin
  Result:='';
end;

function TSQLFileDescriptor.CreateSource(const Filename, SourceName,
  ResourceName: string): string;
begin
  Result:='/* '+SSQLSource+ '*/';
end;

initialization

finalization
  FreeAndNil(AChecker);
end.
