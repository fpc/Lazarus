{  $Id$  }
{
 *****************************************************************************
 *                                                                           *
 *  See the file COPYING.modifiedLGPL.txt, included in this distribution,        *
 *  for details about the copyright.                                         *
 *                                                                           *
 *  This program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
 *                                                                           *
 *****************************************************************************

  Author: Joost van der Sluis
  
  This unit registers the sqldb components of the FCL.
}
unit registersqldb;

{$mode objfpc}{$H+}
{$IFNDEF win64}
{$DEFINE HASMYSQL4CONNECTION}
{$DEFINE HASORACLECONNECTION}
{$DEFINE HASPQCONNECTION}
{$DEFINE HASSQLITE3CONNECTION}
{$ENDIF}

{$IFDEF VER2_5_1}
{$DEFINE HASMYSQL51CONNECTION}
{$DEFINE HASSQLPARSER}
{$ENDIF}

interface

uses
  Classes, SysUtils, LResources, db, sqldb, ibconnection, odbcconn,
{$IFDEF HASPQCONNECTION}
  pqconnection,
{$ENDIF}
{$IFDEF HASORACLECONNECTION}
  oracleconnection,
{$ENDIF}

{$IFDEF HASMYSQL4CONNECTION}
  mysql40conn, mysql41conn,
{$ENDIF}
  mysql50conn,
{$IFDEF HASMYSQL51CONNECTION}
  mysql51conn,
{$ENDIF}
{$IFDEF HASSQLITE3CONNECTION}
  sqlite3conn,
{$ENDIF}
  propedits,
  sqlstringspropertyeditordlg,
  controls,
  forms,
{$IFDEF HASSQLPARSER}
  sqlscript, fpsqltree, fpsqlparser,
{$ENDIF HASSQLPARSER}
  LazarusPackageIntf,
  lazideintf,
  srceditorintf,
  ProjectIntf,
  idemsgintf;

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

  TSQLFirebirdFileNamePropertyEditor=class(TFileNamePropertyEditor)
  public
    function GetFilter: String; override;
    function GetInitialDirectory: string; override;
  end;

  { TSQLFileDescriptor }

  TSQLFileDescriptor = class(TProjectFileDescriptor)
  public
    constructor Create; override;
    function GetLocalizedName: string; override;
    function GetLocalizedDescription: string; override;
    function GetResourceSource(const ResourceName: string): string; override;
    function CreateSource(const Filename, SourceName,
                          ResourceName: string): string; override;
  end;


{$IFDEF HASSQLPARSER}
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
{$ENDIF HASSQLPARSER}

procedure Register;

implementation

procedure RegisterUnitSQLdb;
begin
  RegisterComponents('SQLdb',[TSQLQuery,
                              TSQLTransaction,
                              TSQLScript,
                              TSQLConnector,
{$IFDEF HASPQCONNECTION}
                              TPQConnection,
{$ENDIF}
{$IFDEF HASORACLECONNECTION}
                              TOracleConnection,
{$ENDIF}
                              TODBCConnection,
{$IFDEF HASMYSQL4CONNECTION}
                              TMySQL40Connection,
                              TMySQL41Connection,
{$ENDIF}
                              TMySQL50Connection,
{$IFDEF HASMYSQL51CONNECTION}
                              TMySQL51Connection,
{$ENDIF}
{$IFDEF HASSQLITE3CONNECTION}
                              TSQLite3Connection,
{$ENDIF}
                              TIBConnection]);
end;


Resourcestring
  SSQLScript     = 'SQL Script file';
  SSQLScriptDesc = 'Create a new SQL Script file';
  SSQLSource = 'Insert your SQL statements here';

  SFireBirdDatabases = 'Firebird databases';
  SInterbaseDatabases = 'Interbase databases';
  SSQLStringsPropertyEditorDlgTitle = 'Editing %s';

{ TDbfFileNamePropertyEditor }

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

{$IFDEF HASSQLPARSER}
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
  IDEMessagesWindow.AddMsg(SourceFileName+' : '+Msg,'',0,Nil);
end;

procedure TSQLSyntaxChecker.ShowMessage(const Fmt: String;
  Args: array of const);
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
  IDEMessagesWindow.BeginBlock(False);
  try
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
  finally
    IDEMessagesWindow.EndBlock;
  end;
end;

Var
  AChecker : TSQLSyntaxChecker;
{$ENDIF HASSQLPARSER}

procedure Register;
begin
  RegisterPropertyEditor(TypeInfo(AnsiString),
    TIBConnection, 'DatabaseName', TSQLFirebirdFileNamePropertyEditor);
  RegisterPropertyEditor(TStrings.ClassInfo, TSQLQuery,  'SQL'      , TSQLStringsPropertyEditor);
  RegisterPropertyEditor(TStrings.ClassInfo, TSQLQuery,  'InsertSQL', TSQLStringsPropertyEditor);
  RegisterPropertyEditor(TStrings.ClassInfo, TSQLQuery,  'DeleteSQL', TSQLStringsPropertyEditor);
  RegisterPropertyEditor(TStrings.ClassInfo, TSQLQuery,  'UpdateSQL', TSQLStringsPropertyEditor);
  RegisterPropertyEditor(TStrings.ClassInfo, TSQLScript, 'Script'   , TSQLStringsPropertyEditor);
  RegisterProjectFileDescriptor(TSQLFileDescriptor.Create);

  RegisterUnit('sqldb',@RegisterUnitSQLdb);
{$IFDEF HASSQLPARSER}
  AChecker:=TSQLSyntaxChecker.Create(Nil);
  LazarusIDE.AddHandlerOnQuickSyntaxCheck(@AChecker.CheckSource,False);
{$ENDIF HASSQLPARSER}
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

function TSQLFileDescriptor.GetResourceSource(const ResourceName: string
  ): string;
begin
  Result:='';
end;

function TSQLFileDescriptor.CreateSource(const Filename, SourceName,
  ResourceName: string): string;
begin
  Result:='/* '+SSQLSource+ '*/';
end;

initialization
 {$i registersqldb.lrs}

{$IFDEF HASSQLPARSER}
finalization
  FreeAndNil(AChecker);
{$ENDIF HASSQLPARSER}
end.
