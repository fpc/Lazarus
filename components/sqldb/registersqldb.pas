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
{$IFDEF HASSQLITE3CONNECTION}
  sqlite3conn,
{$ENDIF}
  propedits,
  sqlstringspropertyeditordlg,
  controls,
  forms,
  LazarusPackageIntf;

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
{$IFDEF HASSQLITE3CONNECTION}
                              TSQLite3Connection,
{$ENDIF}
                              TIBConnection]);
end;


Resourcestring
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
    if(GetComponent(0) is TSQLQuery)then
      begin
      Query := (GetComponent(0) as TSQLQuery);
      TheDialog.Connection  := (Query.DataBase as TSQLConnection);
      TheDialog.Transaction := (Query.Transaction as TSQLTransaction);
      end;
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


procedure Register;
begin
  RegisterPropertyEditor(TypeInfo(AnsiString),
    TIBConnection, 'DatabaseName', TSQLFirebirdFileNamePropertyEditor);
  RegisterPropertyEditor(TStrings.ClassInfo, TSQLQuery, 'SQL'      , TSQLStringsPropertyEditor);
  RegisterPropertyEditor(TStrings.ClassInfo, TSQLQuery, 'InsertSQL', TSQLStringsPropertyEditor);
  RegisterPropertyEditor(TStrings.ClassInfo, TSQLQuery, 'DeleteSQL', TSQLStringsPropertyEditor);
  RegisterPropertyEditor(TStrings.ClassInfo, TSQLQuery, 'UpdateSQL', TSQLStringsPropertyEditor);

  RegisterUnit('sqldb',@RegisterUnitSQLdb);
end;

initialization
 {$i registersqldb.lrs}

end.
