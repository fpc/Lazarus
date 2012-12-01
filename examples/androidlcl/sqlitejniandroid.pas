unit sqlitejniandroid;

{
  This is TSqlite3Dataset, a TDataset descendant class for use with fpc compiler
  Copyright (C) 2004  Luiz Américo Pereira Câmara
  Email: pascalive@bol.com.br

  This library is free software; you can redistribute it and/or modify it
  under the terms of the GNU Library General Public License as published by
  the Free Software Foundation; either version 2 of the License, or (at your
  option) any later version with the following modification:

  As a special exception, the copyright holders of this library give you
  permission to link this library with independent modules to produce an
  executable, regardless of the license terms of these independent modules,and
  to copy and distribute the resulting executable under terms of your choice,
  provided that you also meet, for each linked independent module, the terms
  and conditions of the license of that module. An independent module is a
  module which is not derived from or based on this library. If you modify
  this library, you may extend this exception to your version of the library,
  but you are not obligated to do so. If you do not wish to do so, delete this
  exception statement from your version.

  This program is distributed in the hope that it will be useful, but WITHOUT
  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
  FITNESS FOR A PARTICULAR PURPOSE. See the GNU Library General Public License
  for more details.

  You should have received a copy of the GNU Library General Public License
  along with this library; if not, write to the Free Software Foundation,
  Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
}

{$mode objfpc}
{$H+}
{.$Define DEBUG_SQLITEDS}

interface

uses
  Classes, SysUtils, CustomSqliteDS,
  jni, customdrawnint;

type
  { TSqliteJNIDataset }

  TSqliteJNIDataset = class(TCustomSqliteDataset)
  private
    // Java Classes
    FSqliteClosableClass, FSQLiteDatabaseClass, FSQLiteCursor: JClass;
    // Java Methods
    FSqliteClosable_releaseReference: JMethodID;
    FSqliteDatabase_ExecSQLMethod, FSqliteDatabase_openOrCreateDatabase,
      FSqliteDatabase_getVersion, FSqliteDatabase_query: JMethodID;
    FSqliteCursor_getColumnCount: JMethodID;
    // Java Objects
    AndroidDB: jobject; // SQLiteDatabase
    procedure FindJavaClassesAndMethods;
  protected
    procedure BuildLinkedList; override;
    function GetLastInsertRowId: Int64; override;
    function GetRowsAffected:Integer; override;
    procedure InternalCloseHandle; override;
    function InternalGetHandle: Pointer; override;
    procedure RetrieveFieldDefs; override;
    function SqliteExec(ASQL: PChar; ACallback: TSqliteCdeclCallback; Data: Pointer): Integer; override;
  public
    procedure ExecuteDirect(const ASQL: String); override;
    function QuickQuery(const ASQL: String; const AStrList: TStrings; FillObjects: Boolean): String; override;
    function ReturnString: String; override;
    class function SqliteVersion: String; override;
  end;

implementation

uses
  db, strutils, lclproc;

{
Java code example
Docs: http://developer.android.com/reference/android/database/sqlite/SQLiteDatabase.html

SQLiteDatabase db = openOrCreateDatabase(Preferences.DB_NAME, Context.MODE_PRIVATE, null);
db.execSQL("CREATE TABLE IF NOT EXISTS data (id INTEGER PRIMARY KEY, value VARCHAR)");
Cursor dbResult = db.rawQuery("SELECT value FROM data", null);
// do sometning with cursors
dbResult.close();
db.close(); // releaseReference(); is the same
}

function SqliteCode2Str(Code: Integer): String;
begin
  {case Code of
    SQLITE_OK           :} Result := 'SQLITE_OK';
    {SQLITE_ERROR        : Result := 'SQLITE_ERROR';
    SQLITE_INTERNAL     : Result := 'SQLITE_INTERNAL';
    SQLITE_PERM         : Result := 'SQLITE_PERM';
    SQLITE_ABORT        : Result := 'SQLITE_ABORT';
    SQLITE_BUSY         : Result := 'SQLITE_BUSY';
    SQLITE_LOCKED       : Result := 'SQLITE_LOCKED';
    SQLITE_NOMEM        : Result := 'SQLITE_NOMEM';
    SQLITE_READONLY     : Result := 'SQLITE_READONLY';
    SQLITE_INTERRUPT    : Result := 'SQLITE_INTERRUPT';
    SQLITE_IOERR        : Result := 'SQLITE_IOERR';
    SQLITE_CORRUPT      : Result := 'SQLITE_CORRUPT';
    SQLITE_NOTFOUND     : Result := 'SQLITE_NOTFOUND';
    SQLITE_FULL         : Result := 'SQLITE_FULL';
    SQLITE_CANTOPEN     : Result := 'SQLITE_CANTOPEN';
    SQLITE_PROTOCOL     : Result := 'SQLITE_PROTOCOL';
    SQLITE_EMPTY        : Result := 'SQLITE_EMPTY';
    SQLITE_SCHEMA       : Result := 'SQLITE_SCHEMA';
    SQLITE_TOOBIG       : Result := 'SQLITE_TOOBIG';
    SQLITE_CONSTRAINT   : Result := 'SQLITE_CONSTRAINT';
    SQLITE_MISMATCH     : Result := 'SQLITE_MISMATCH';
    SQLITE_MISUSE       : Result := 'SQLITE_MISUSE';
    SQLITE_NOLFS        : Result := 'SQLITE_NOLFS';
    SQLITE_AUTH         : Result := 'SQLITE_AUTH';
    SQLITE_FORMAT       : Result := 'SQLITE_FORMAT';
    SQLITE_RANGE        : Result := 'SQLITE_RANGE';
    SQLITE_ROW          : Result := 'SQLITE_ROW';
    SQLITE_NOTADB       : Result := 'SQLITE_NOTADB';
    SQLITE_DONE         : Result := 'SQLITE_DONE';
  else
    Result := 'Unknown Return Value';
  end;     }
end;

function GetAutoIncValue(NextValue: Pointer; Columns: Integer; ColumnValues: PPChar; ColumnNames: PPChar): Integer; cdecl;
var
  CodeError, TempInt: Integer;
begin
  TempInt := 0;
  if ColumnValues[0] <> nil then
  begin
    Val(String(ColumnValues[0]), TempInt, CodeError);
    if CodeError <> 0 then
      DatabaseError('TSqlite3Dataset: Error trying to get last autoinc value');
  end;
  Integer(NextValue^) := Succ(TempInt);
  Result := 1;
end;

{ TSqlite3Dataset }

function TSqliteJNIDataset.SqliteExec(ASQL: PChar; ACallback: TSqliteCdeclCallback; Data: Pointer): Integer;
var
  // array for the parameters
  lParams: array[0..2] of JValue;
  lJavaString: JString;
begin
  DebugLn('[TSqliteJNIDataset.SqliteExec] ' + StrPas(ASQL));
  {// void execSQL(String sql)
  // preparations
  lJavaString :=javaEnvRef^^.NewStringUTF(javaEnvRef, ASQL);
  lParams[0].l := lJavaString;

  // Call the method
  javaEnvRef^^.CallVoidMethodA(javaEnvRef, AndroidDB, lExecSQLMethod, @lParams[0]);

  // clean up
  javaEnvRef^^.DeleteLocalRef(javaEnvRef, lJavaString);}
end;

procedure TSqliteJNIDataset.InternalCloseHandle;
begin
  DebugLn('[TSqliteJNIDataset.InternalCloseHandle]');
  // void android.database.sqlite.SQLiteClosable->releaseReference()
  javaEnvRef^^.CallVoidMethod(javaEnvRef, AndroidDB, FSqliteClosable_releaseReference);
  //javaEnvRef^^.DeleteLocalRef(javaEnvRef, AndroidDB);

  //f/sqlite3_close(FSqliteHandle);
  FSqliteHandle := nil;
  //todo:handle return data
end;


function TSqliteJNIDataset.InternalGetHandle: Pointer;
const
  CheckFileSql = 'Select Name from sqlite_master LIMIT 1';
var
  // array for the parameters
  lParams: array[0..2] of JValue;
  lJavaString: JString;
begin
  DebugLn('[TSqliteJNIDataset.InternalGetHandle]');
  FindJavaClassesAndMethods();

  // static SQLiteDatabase openOrCreateDatabase(String path, SQLiteDatabase.CursorFactory factory)
  // preparations
  lJavaString :=javaEnvRef^^.NewStringUTF(javaEnvRef, PChar(FFileName));
  lParams[0].l := lJavaString;
  lParams[1].l := nil;
  // Call the method
  AndroidDB := javaEnvRef^^.CallStaticObjectMethodA(javaEnvRef, FSqliteDatabaseClass, FSqliteDatabase_openOrCreateDatabase, @lParams[0]);
  // clean up
  javaEnvRef^^.DeleteLocalRef(javaEnvRef, lJavaString);
end;

procedure TSqliteJNIDataset.RetrieveFieldDefs;
var
  vm: Pointer;
  ColumnStr: String;
  i, ColumnCount, DataSize: Integer;
  AType: TFieldType;
  //
  dbCursor: JObject;
  lParams: array[0..7] of JValue;
begin
  DebugLn('[TSqliteJNIDataset.RetrieveFieldDefs]');
  FAutoIncFieldNo := -1;
  FieldDefs.Clear;

  // Cursor c = db.query(tableName, null, null, null, null, null, null);
  // public Cursor query (String table, String[] columns, String selection, String[] selectionArgs, String groupBy, String having, String orderBy, String limit)
  lParams[0].l :=javaEnvRef^^.NewStringUTF(javaEnvRef, PChar(TableName));
  lParams[1].l := nil;
  lParams[2].l := nil;
  lParams[3].l := nil;
  lParams[4].l := nil;
  lParams[5].l := nil;
  lParams[6].l := nil;
  lParams[7].l := nil;
  dbCursor := javaEnvRef^^.CallObjectMethodA(javaEnvRef, AndroidDB, FSqliteDatabase_query, @lParams[0]);

  if dbCursor = nil then
  begin
    DebugLn('[TSqliteJNIDataset.RetrieveFieldDefs] dbCursor = nil');
    Exit;
  end;

//    int num = c.getColumnCount();
//    for (int i = 0; i < num; ++i)
    {
      String colname = c.getColumnName(i);
    }


  //FReturnCode := sqlite3_prepare(FSqliteHandle, PChar(FEffectiveSQL), -1, @vm, nil);
  //if FReturnCode <> SQLITE_OK then
  //  DatabaseError(ReturnString, Self);
  //sqlite3_step(vm);
  //ColumnCount := sqlite3_column_count(vm);
  //Prepare the array of pchar2sql functions
{  SetLength(FGetSqlStr, ColumnCount);
  for i := 0 to ColumnCount - 1 do
  begin
    DataSize := 0;
    ColumnStr := UpperCase(String(sqlite3_column_decltype(vm, i)));
    if (ColumnStr = 'INTEGER') or (ColumnStr = 'INT') then
    begin
      if AutoIncrementKey and (UpperCase(String(sqlite3_column_name(vm, i))) = UpperCase(PrimaryKey)) then
      begin
        AType := ftAutoInc;
        FAutoIncFieldNo := i;
      end
      else
        AType := ftInteger;
    end else if Pos('VARCHAR', ColumnStr) = 1 then
    begin
      AType := ftString;
      DataSize := StrToIntDef(Trim(ExtractDelimited(2, ColumnStr, ['(', ')'])), DefaultStringSize);
    end else if Pos('BOOL', ColumnStr) = 1 then
    begin
      AType := ftBoolean;
    end else if Pos('AUTOINC', ColumnStr) = 1 then
    begin
      AType := ftAutoInc;
      if FAutoIncFieldNo = -1 then
        FAutoIncFieldNo := i;
    end else if (Pos('FLOAT', ColumnStr) = 1) or (Pos('NUMERIC', ColumnStr) = 1) then
    begin
      AType := ftFloat;
    end else if (ColumnStr = 'DATETIME') then
    begin
      AType := ftDateTime;
    end else if (ColumnStr = 'DATE') then
    begin
      AType := ftDate;
    end else if (ColumnStr = 'LARGEINT') or (ColumnStr = 'BIGINT') then
    begin
      AType := ftLargeInt;
    end else if (ColumnStr = 'TIME') then
    begin
      AType := ftTime;
    end else if (ColumnStr = 'TEXT') then
    begin
      AType := ftMemo;
    end else if (ColumnStr = 'CURRENCY') then
    begin
      AType := ftCurrency;
    end else if (ColumnStr = 'WORD') then
    begin
      AType := ftWord;
    end else if (ColumnStr = '') then
    begin
      case sqlite3_column_type(vm, i) of
        SQLITE_INTEGER:
          AType := ftInteger;
        SQLITE_FLOAT:
          AType := ftFloat;
      else
	    begin
          AType := ftString;
		  DataSize := DefaultStringSize;
		end;  		
      end;
    end else
    begin
      AType := ftString;
	  DataSize := DefaultStringSize;
    end;
    FieldDefs.Add(String(sqlite3_column_name(vm, i)), AType, DataSize);
    //Set the pchar2sql function
    if AType in [ftString, ftMemo] then
      FGetSqlStr[i] := @Char2SQLStr
    else
      FGetSqlStr[i] := @Num2SQLStr;
    {$ifdef DEBUG_SQLITEDS}
    WriteLn('  Field[', i, '] Name: ', sqlite3_column_name(vm, i));
    WriteLn('  Field[', i, '] Type: ', sqlite3_column_decltype(vm, i));
    {$endif}
  end;
  sqlite3_finalize(vm);}
end;

function TSqliteJNIDataset.GetRowsAffected: Integer;
begin
  DebugLn('[TSqliteJNIDataset.GetRowsAffected]');
  //f/Result := sqlite3_changes(FSqliteHandle);
end;

procedure TSqliteJNIDataset.ExecuteDirect(const ASQL: String);
var
  vm: Pointer;
begin
  DebugLn('[TSqliteJNIDataset.ExecuteDirect]');
  {FReturnCode := sqlite3_prepare(FSqliteHandle, Pchar(ASQL), -1, @vm, nil);
  if FReturnCode <> SQLITE_OK then
    DatabaseError(ReturnString, Self);
  FReturnCode := sqlite3_step(vm);
  sqlite3_finalize(vm);}
end;

procedure TSqliteJNIDataset.FindJavaClassesAndMethods;
begin
  FSQLiteDatabaseClass := javaEnvRef^^.FindClass(javaEnvRef, 'android/database/sqlite/SQLiteDatabase');
  FSQLiteClosableClass := javaEnvRef^^.FindClass(javaEnvRef, 'android/database/sqlite/SQLiteClosable');
  FSQLiteCursor := javaEnvRef^^.FindClass(javaEnvRef, 'android/database/Cursor');

  //
  // Methods from SqliteDatabase
  //
  FSqliteDatabase_ExecSQLMethod := javaEnvRef^^.GetMethodID(javaEnvRef, FSQLiteDatabaseClass, 'execSQL',
    '(Ljava/lang/String;)V');
  FSqliteDatabase_openOrCreateDatabase := javaEnvRef^^.GetStaticMethodID(javaEnvRef, FSQLiteDatabaseClass, 'openOrCreateDatabase',
    '(Ljava/lang/String;Landroid/database/sqlite/SQLiteDatabase$CursorFactory;)Landroid/database/sqlite/SQLiteDatabase;');
  //DebugLn('[TSqliteJNIDataset.FindJavaClassesAndMethods] FSqliteDatabase_openOrCreateDatabase='+IntToHex(Cardinal(FSqliteDatabase_openOrCreateDatabase), 8));
  FSqliteDatabase_getVersion := javaEnvRef^^.GetMethodID(javaEnvRef, FSQLiteDatabaseClass, 'getVersion',
    '()I');
  // public Cursor query (String table, String[] columns, String selection, String[] selectionArgs, String groupBy, String having, String orderBy, String limit)
  FSqliteDatabase_query := javaEnvRef^^.GetMethodID(javaEnvRef, FSQLiteDatabaseClass, 'query',
    '(Ljava/lang/String;[Ljava/lang/String;Ljava/lang/String;[Ljava/lang/String;Ljava/lang/String;Ljava/lang/String;Ljava/lang/String;Ljava/lang/String;)Landroid/database/Cursor;');
  //
  // Methods from FSQLiteCursor
  //
  FSqliteCursor_getColumnCount := javaEnvRef^^.GetMethodID(javaEnvRef, FSQLiteCursor, 'getColumnCount',
    '()I');
end;

procedure TSqliteJNIDataset.BuildLinkedList;
var
  TempItem: PDataRecord;
  vm: Pointer;
  Counter, ColumnCount: Integer;
begin
  DebugLn('[TSqliteJNIDataset.BuildLinkedList]');
{  //Get AutoInc Field initial value
  if FAutoIncFieldNo <> -1 then
    sqlite3_exec(FSqliteHandle, PChar('Select Max(' + FieldDefs[FAutoIncFieldNo].Name +
      ') from ' + FTableName), @GetAutoIncValue, @FNextAutoInc, nil);

  FReturnCode := sqlite3_prepare(FSqliteHandle, PChar(FEffectiveSQL), -1, @vm, nil);
  if FReturnCode <> SQLITE_OK then
    DatabaseError(ReturnString, Self);

  FDataAllocated := True;

  TempItem := FBeginItem;
  FRecordCount := 0;
  ColumnCount := sqlite3_column_count(vm);
  FRowCount := ColumnCount;
  //add extra rows for calculated fields
  if FCalcFieldList <> nil then
    Inc(FRowCount, FCalcFieldList.Count);
  FRowBufferSize := (SizeOf(PPChar) * FRowCount);
  FReturnCode := sqlite3_step(vm);
  while FReturnCode = SQLITE_ROW do
  begin
    Inc(FRecordCount);
    New(TempItem^.Next);
    TempItem^.Next^.Previous := TempItem;
    TempItem := TempItem^.Next;
    GetMem(TempItem^.Row, FRowBufferSize);
    for Counter := 0 to ColumnCount - 1 do
      TempItem^.Row[Counter] := StrNew(sqlite3_column_text(vm, Counter));
    //initialize calculated fields with nil
    for Counter := ColumnCount to FRowCount - 1 do
      TempItem^.Row[Counter] := nil;
    FReturnCode := sqlite3_step(vm);
  end;
  sqlite3_finalize(vm);

  // Attach EndItem
  TempItem^.Next := FEndItem;
  FEndItem^.Previous := TempItem;

  // Alloc temporary item used in append/insert
  GetMem(FCacheItem^.Row, FRowBufferSize);
  for Counter := 0 to FRowCount - 1 do
    FCacheItem^.Row[Counter] := nil;
  // Fill FBeginItem.Row with nil -> necessary for avoid exceptions in empty datasets
  GetMem(FBeginItem^.Row, FRowBufferSize);
  //Todo: see if is better to nullif using FillDWord
  for Counter := 0 to FRowCount - 1 do
    FBeginItem^.Row[Counter] := nil;}
end;

function TSqliteJNIDataset.GetLastInsertRowId: Int64;
begin
  DebugLn('[TSqliteJNIDataset.GetLastInsertRowId]');
  //f/Result := sqlite3_last_insert_rowid(FSqliteHandle);
end;

function TSqliteJNIDataset.ReturnString: String;
begin
  DebugLn('[TSqliteJNIDataset.ReturnString]');
  //f/Result := SqliteCode2Str(FReturnCode) + ' - ' + sqlite3_errmsg(FSqliteHandle);
end;

class function TSqliteJNIDataset.SqliteVersion: String;
var
  intVersion: JInt;
begin
  DebugLn('[TSqliteJNIDataset.SqliteVersion]');
  // public int getVersion ()
  //intVersion := javaEnvRef^^.CallIntMethod(javaEnvRef, AndroidDB, FSqliteClosable_getVersion);
  Result := '3.4'; // IntToStr(intVersion); cant access AndroidDB in class method
  DebugLn('[TSqliteJNIDataset.SqliteVersion] Result='+Result);
end;

function TSqliteJNIDataset.QuickQuery(const ASQL: String; const AStrList: TStrings; FillObjects:Boolean): String;
var
  vm: Pointer;
    
{  procedure FillStrings;
  begin
    while FReturnCode = SQLITE_ROW do
    begin
      AStrList.Add(String(sqlite3_column_text(vm,0)));
      FReturnCode := sqlite3_step(vm);
    end;
  end;
  procedure FillStringsAndObjects;
  begin
    while FReturnCode = SQLITE_ROW do
    begin
      AStrList.AddObject(String(sqlite3_column_text(vm, 0)),
        TObject(PtrInt(sqlite3_column_int(vm, 1))));
      FReturnCode := sqlite3_step(vm);
    end;
  end;    }
begin
  DebugLn('[TSqliteJNIDataset.QuickQuery]');
{  if FSqliteHandle = nil then
    GetSqliteHandle;
  Result := '';
  FReturnCode := sqlite3_prepare(FSqliteHandle,Pchar(ASQL), -1, @vm, nil);
  if FReturnCode <> SQLITE_OK then
    DatabaseError(ReturnString, Self);
    
  FReturnCode := sqlite3_step(vm);
  if (FReturnCode = SQLITE_ROW) and (sqlite3_column_count(vm) > 0) then
  begin
    Result := String(sqlite3_column_text(vm, 0));
    if AStrList <> nil then
    begin   
      if FillObjects and (sqlite3_column_count(vm) > 1) then
        FillStringsAndObjects
      else
        FillStrings;
    end;          
  end;  
  sqlite3_finalize(vm); }
end;

end.

