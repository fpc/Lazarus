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
  Inc., 51 Franklin Street - Fifth Floor, Boston, MA 02110-1335, USA.
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
    FLastInsertRowId: Int64;
    FReturnString: string;
    // Java Classes
    FSqliteClosableClass, FSQLiteDatabaseClass, FDBCursorClass: JClass;
    // Java Methods
    FSqliteClosable_acquireReference, FSqliteClosable_releaseReference: JMethodID;
    FSqliteDatabase_ExecSQLMethod, FSqliteDatabase_openOrCreateDatabase,
      FSqliteDatabase_getVersion, FSqliteDatabase_query,
      FSqliteDatabase_execSQL, FSqliteDatabase_rawQuery,
      FSqliteDatabase_inTransaction: JMethodID;
    FDBCursor_getColumnCount, FDBCursor_getColumnName, FDBCursor_getType,
      FDBCursor_close, FDBCursor_getCount, FDBCursor_getDouble,
      FDBCursor_getLong, FDBCursor_getPosition, FDBCursor_getString,
      FDBCursor_moveToFirst, FDBCursor_moveToNext, FDBCursor_moveToPosition,
      FDBCursor_moveToPrevious: JMethodID;
    // Java Objects
    AndroidDB: jobject; // SQLiteDatabase
    function  SplitSQLStatements(AInput: string): TStrings;
    procedure FindJavaClassesAndMethods;
    procedure RealInternalCloseHandle;
    function  RealInternalGetHandle: Pointer;
  protected
    procedure BuildLinkedList; override;
    function  GetLastInsertRowId: Int64; override;
    function  GetRowsAffected:Integer; override;
    procedure InternalCloseHandle; override;
    function  InternalGetHandle: Pointer; override;
    procedure RetrieveFieldDefs; override;
    function  SqliteExec(ASQL: PChar; ACallback: TSqliteCdeclCallback; Data: Pointer): Integer; override;
    function  CheckJNIException(ARaisePascalException: Boolean): Boolean;
  public
    procedure ExecuteDirect(const ASQL: String); override;
    function  QuickQuery(const ASQL: String; const AStrList: TStrings; FillObjects: Boolean): String; override;
    function  ReturnString: String; override;
    class function SqliteVersion: String; override;
  end;

implementation

uses
  db, strutils, lclproc;

const
  //
  // from android.database.Cursor
  //
  FIELD_TYPE_BLOB = 4;   // Added in API level 11
  FIELD_TYPE_FLOAT = 2;  // Added in API level 11
  FIELD_TYPE_INTEGER = 1;// Added in API level 11
  FIELD_TYPE_NULL = 0;   // Added in API level 11
  FIELD_TYPE_STRING = 3; // Added in API level 11

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

function TSqliteJNIDataset.SplitSQLStatements(AInput: string): TStrings;
var
  i: Integer;
  CurChar: Char;
  CurStr: string;
  InQuote: Boolean = false;
begin
  Result := TStringList.Create;
  for i := 1 to Length(AInput) do
  begin
    CurChar := AInput[i];

    if CurChar = '''' then InQuote := not InQuote;

    // Add a statement when it finishes with a ; outside quotes
    if (CurChar = ';') and (not InQuote) then
    begin
      Result.Add(CurStr);
      CurStr := '';
    end
    else
      CurStr := CurStr + CurChar;
  end;
  // add the last statement even if it doesn't end with a ;
  if CurStr <> '' then Result.Add(CurStr);
end;

procedure TSqliteJNIDataset.FindJavaClassesAndMethods;
begin
  FSQLiteDatabaseClass := javaEnvRef^^.FindClass(javaEnvRef, 'android/database/sqlite/SQLiteDatabase');
  FSQLiteClosableClass := javaEnvRef^^.FindClass(javaEnvRef, 'android/database/sqlite/SQLiteClosable');
  FDBCursorClass := javaEnvRef^^.FindClass(javaEnvRef, 'android/database/Cursor');

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
  // void execSQL(String sql)
  FSqliteDatabase_execSQL := javaEnvRef^^.GetMethodID(javaEnvRef, FSQLiteDatabaseClass, 'execSQL',
    '(Ljava/lang/String;)V');
  // public Cursor rawQuery (String sql, String[] selectionArgs)
  FSqliteDatabase_rawQuery := javaEnvRef^^.GetMethodID(javaEnvRef, FSQLiteDatabaseClass, 'rawQuery',
    '(Ljava/lang/String;[Ljava/lang/String;)Landroid/database/Cursor;');
  // public boolean inTransaction ()
  FSqliteDatabase_inTransaction := javaEnvRef^^.GetMethodID(javaEnvRef, FSQLiteDatabaseClass, 'inTransaction',
    '()Z');
  //
  // Methods from FDBClosable
  //
  FSqliteClosable_acquireReference := javaEnvRef^^.GetMethodID(javaEnvRef, FSQLiteClosableClass, 'acquireReference',
    '()V');
  FSqliteClosable_releaseReference := javaEnvRef^^.GetMethodID(javaEnvRef, FSQLiteClosableClass, 'releaseReference',
    '()V');
  //
  // Methods from FDBCursor
  //
  FDBCursor_getColumnCount := javaEnvRef^^.GetMethodID(javaEnvRef, FDBCursorClass, 'getColumnCount',
    '()I');
  // abstract String getColumnName(int columnIndex)
  FDBCursor_getColumnName := javaEnvRef^^.GetMethodID(javaEnvRef, FDBCursorClass, 'getColumnName',
    '(I)Ljava/lang/String;');
  // public abstract int getType (int columnIndex) // Added in API level 11
  if android_os_Build_VERSION_SDK_INT >= 11 then
  begin
    FDBCursor_getType := javaEnvRef^^.GetMethodID(javaEnvRef, FDBCursorClass, 'getType',
      '(I)I');
  end;
  // abstract void 	close()
  FDBCursor_close := javaEnvRef^^.GetMethodID(javaEnvRef, FDBCursorClass, 'close',
    '()V');
  // public abstract int getCount ()
  FDBCursor_getCount := javaEnvRef^^.GetMethodID(javaEnvRef, FDBCursorClass, 'getCount',
    '()I');
  // public abstract double getDouble (int columnIndex)
  FDBCursor_getDouble := javaEnvRef^^.GetMethodID(javaEnvRef, FDBCursorClass, 'getDouble',
    '(I)D');
  // public abstract long getLong (int columnIndex)
  FDBCursor_getLong := javaEnvRef^^.GetMethodID(javaEnvRef, FDBCursorClass, 'getLong',
    '(I)J');
  // public abstract int getPosition ()
  FDBCursor_getPosition := javaEnvRef^^.GetMethodID(javaEnvRef, FDBCursorClass, 'getPosition',
    '()I');
  // public abstract String getString (int columnIndex)
  FDBCursor_getString := javaEnvRef^^.GetMethodID(javaEnvRef, FDBCursorClass, 'getString',
    '(I)Ljava/lang/String;');
  // public abstract boolean moveToFirst ()
  FDBCursor_moveToFirst := javaEnvRef^^.GetMethodID(javaEnvRef, FDBCursorClass, 'moveToFirst',
    '()Z');
  // public abstract boolean moveToNext ()
  FDBCursor_moveToNext := javaEnvRef^^.GetMethodID(javaEnvRef, FDBCursorClass, 'moveToNext',
    '()Z');
  // public abstract boolean moveToPosition (int position)
  FDBCursor_moveToPosition := javaEnvRef^^.GetMethodID(javaEnvRef, FDBCursorClass, 'moveToPosition',
    '(I)Z');
  // public abstract boolean moveToPrevious ()
  FDBCursor_moveToPrevious := javaEnvRef^^.GetMethodID(javaEnvRef, FDBCursorClass, 'moveToPrevious',
    '()Z');
end;

procedure TSqliteJNIDataset.RealInternalCloseHandle;
{var
  inTransaction: Boolean;}
begin
  DebugLn('[TSqliteJNIDataset.RealInternalCloseHandle]');

  {// Before closing the reference, wait until the last transaction has finished
  inTransaction := javaEnvRef^^.CallBooleanMethod(javaEnvRef, AndroidDB, FSqliteDatabase_inTransaction) = JNI_TRUE;
  while inTransaction do
  begin
    DebugLn('[TSqliteJNIDataset.RealInternalCloseHandle] Waiting 100ms for the transaction to close');
    Sleep(100);
    inTransaction := javaEnvRef^^.CallBooleanMethod(javaEnvRef, AndroidDB, FSqliteDatabase_inTransaction) = JNI_TRUE;
  end;}

  // void android.database.sqlite.SQLiteClosable->releaseReference()
  javaEnvRef^^.CallVoidMethod(javaEnvRef, AndroidDB, FSqliteClosable_releaseReference);
  javaEnvRef^^.DeleteLocalRef(javaEnvRef, AndroidDB);

  //f/sqlite3_close(FSqliteHandle);
  FSqliteHandle := nil;
  //todo:handle return data
end;

function TSqliteJNIDataset.RealInternalGetHandle: Pointer;
const
  CheckFileSql = 'Select Name from sqlite_master LIMIT 1';
var
  // array for the parameters
  lParams: array[0..2] of JValue;
  lJavaString: JString;
begin
  DebugLn('[TSqliteJNIDataset.RealInternalGetHandle]');

  // static SQLiteDatabase openOrCreateDatabase(String path, SQLiteDatabase.CursorFactory factory)
  // preparations
  lJavaString :=javaEnvRef^^.NewStringUTF(javaEnvRef, PChar(FFileName));
  lParams[0].l := lJavaString;
  lParams[1].l := nil;
  // Call the method
  AndroidDB := javaEnvRef^^.CallStaticObjectMethodA(javaEnvRef, FSqliteDatabaseClass, FSqliteDatabase_openOrCreateDatabase, @lParams[0]);
  CheckJNIException(True);
  // clean up
  javaEnvRef^^.DeleteLocalRef(javaEnvRef, lJavaString);

  // Reinforce by getting a reference to the object, not only creating it -> Doesnt seam to make any difference
  //javaEnvRef^^.CallVoidMethod(javaEnvRef, AndroidDB, FSqliteClosable_acquireReference);
  Result := Pointer(AndroidDB);
  DebugLn(Format('[TSqliteJNIDataset.RealInternalGetHandle] AndroidDB=%x', [PtrInt(AndroidDB)]));
end;

procedure TSqliteJNIDataset.BuildLinkedList;
var
  TempItem: PDataRecord;
  Counter, ColumnCount, TrueRowCount: Integer;
  lIsAfterLastRow: Boolean;
  //
  lJavaString: JString;
  lNativeString: PChar;
  dbCursor: JObject;
  lParams: array[0..7] of JValue;
begin
  DebugLn('[TSqliteJNIDataset.BuildLinkedList] FEffectiveSQL='+FEffectiveSQL);
  RealInternalGetHandle();
{  //Get AutoInc Field initial value
  if FAutoIncFieldNo <> -1 then
    sqlite3_exec(FSqliteHandle, PChar('Select Max(' + FieldDefs[FAutoIncFieldNo].Name +
      ') from ' + FTableName), @GetAutoIncValue, @FNextAutoInc, nil);}

  //FReturnCode := sqlite3_prepare(FSqliteHandle, PChar(FEffectiveSQL), -1, @vm, nil);
  //if FReturnCode <> SQLITE_OK then
  //  DatabaseError(ReturnString, Self);
  //
  // public Cursor rawQuery (String sql, String[] selectionArgs)
  lParams[0].l := javaEnvRef^^.NewStringUTF(javaEnvRef, PChar(FEffectiveSQL));
  lParams[1].l := nil;
  dbCursor := javaEnvRef^^.CallObjectMethodA(javaEnvRef, AndroidDB, FSqliteDatabase_rawQuery, @lParams[0]);
  javaEnvRef^^.DeleteLocalRef(javaEnvRef, lParams[0].l);

  FDataAllocated := True;

  TempItem := FBeginItem;
  FRecordCount := 0;
  ColumnCount := javaEnvRef^^.CallIntMethod(javaEnvRef, dbCursor, FDBCursor_getColumnCount);
  TrueRowCount := javaEnvRef^^.CallIntMethod(javaEnvRef, dbCursor, FDBCursor_getCount);
  FRowCount := ColumnCount;
  //add extra rows for calculated fields
  if FCalcFieldList <> nil then
    Inc(FRowCount, FCalcFieldList.Count);
  FRowBufferSize := (SizeOf(PPChar) * FRowCount);
  //FReturnCode := sqlite3_step(vm);
  //while FReturnCode = SQLITE_ROW do
  //begin
  //
  // public abstract boolean moveToNext ()
  DebugLn(Format('[TSqliteJNIDataset.BuildLinkedList] ColCount=%d RowCount=%d', [ColumnCount, TrueRowCount]));
  if TrueRowCount > 0 then
  begin
    lIsAfterLastRow := javaEnvRef^^.CallBooleanMethod(javaEnvRef, dbCursor, FDBCursor_moveToNext) = JNI_FALSE;
    while not lIsAfterLastRow do
    begin
      Inc(FRecordCount);
      DebugLn(Format('[TSqliteJNIDataset.BuildLinkedList] reading row # %d', [FRecordCount]));
      New(TempItem^.Next);
      TempItem^.Next^.Previous := TempItem;
      TempItem := TempItem^.Next;
      GetMem(TempItem^.Row, FRowBufferSize);
      for Counter := 0 to ColumnCount - 1 do
      begin
        DebugLn(Format('[TSqliteJNIDataset.BuildLinkedList] reading row # %d col # %d', [FRecordCount, Counter]));
        // TempItem^.Row[Counter] := StrNew(sqlite3_column_text(vm, Counter));
        // public abstract String getString (int columnIndex)
        lParams[0].i := Counter;
        lJavaString := javaEnvRef^^.CallObjectMethodA(javaEnvRef, dbCursor, FDBCursor_getString, @lParams[0]);
        //DebugLn(Format('[TSqliteJNIDataset.BuildLinkedList] lJavaString=%x', [PtrInt(lJavaString)]));
        if lJavaString <> nil then
        begin
          lNativeString := javaEnvRef^^.GetStringUTFChars(javaEnvRef, lJavaString, nil);
          TempItem^.Row[Counter] := StrNew(lNativeString);
          javaEnvRef^^.ReleaseStringUTFChars(javaEnvRef, lJavaString, lNativeString);
          javaEnvRef^^.DeleteLocalRef(javaEnvRef, lJavaString);
        end
        else
          TempItem^.Row[Counter] := StrNew('');
      end;
      //initialize calculated fields with nil
      for Counter := ColumnCount to FRowCount - 1 do
        TempItem^.Row[Counter] := nil;
      //FReturnCode := sqlite3_step(vm);
      lIsAfterLastRow := javaEnvRef^^.CallBooleanMethod(javaEnvRef, dbCursor, FDBCursor_moveToNext) = JNI_FALSE;
    end;
  end;
  //sqlite3_finalize(vm);
  javaEnvRef^^.CallVoidMethod(javaEnvRef, dbCursor, FDBCursor_close);

  // Attach EndItem
  TempItem^.Next := FEndItem;
  FEndItem^.Previous := TempItem;

  // Alloc temporary item used in append/insert
  GetMem(FSavedEditItem^.Row, FRowBufferSize);
  for Counter := 0 to FRowCount - 1 do
    FSavedEditItem^.Row[Counter] := nil;
  // Fill FBeginItem.Row with nil -> necessary for avoid exceptions in empty datasets
  GetMem(FBeginItem^.Row, FRowBufferSize);
  //Todo: see if is better to nullif using FillDWord
  for Counter := 0 to FRowCount - 1 do
    FBeginItem^.Row[Counter] := nil;

  RealInternalCloseHandle();
end;

function TSqliteJNIDataset.GetLastInsertRowId: Int64;
begin
  Result := FLastInsertRowId;
  DebugLn('[TSqliteJNIDataset.GetLastInsertRowId] Result='+IntToStr(Result));
  //f/Result := sqlite3_last_insert_rowid(FSqliteHandle);
end;

function TSqliteJNIDataset.GetRowsAffected: Integer;
begin
  DebugLn('[TSqliteJNIDataset.GetRowsAffected]');
  //f/Result := sqlite3_changes(FSqliteHandle);
end;

procedure TSqliteJNIDataset.InternalCloseHandle;
begin
  DebugLn('[TSqliteJNIDataset.InternalCloseHandle]');
  // RealInternalCloseHandle();
end;


function TSqliteJNIDataset.InternalGetHandle: Pointer;
begin
  DebugLn('[TSqliteJNIDataset.InternalGetHandle]');
  FindJavaClassesAndMethods();
  Result := Pointer($beef);
end;

procedure TSqliteJNIDataset.RetrieveFieldDefs;
var
  vm: Pointer;
  ColumnName: string;
  i, ColumnCount, RowCount, DataSize: Integer;
  AType: TFieldType;
  //
  lColumnType: JInt;
  lJavaString: JString;
  lNativeString: PChar;
  dbCursor: JObject;
  lParams: array[0..7] of JValue;
begin
  DebugLn('[TSqliteJNIDataset.RetrieveFieldDefs]');
  RealInternalGetHandle();
  FAutoIncFieldNo := -1;
  FieldDefs.Clear;

  //FReturnCode := sqlite3_prepare(FSqliteHandle, PChar(FEffectiveSQL), -1, @vm, nil);
  //if FReturnCode <> SQLITE_OK then
  //  DatabaseError(ReturnString, Self);
  //
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
  if CheckJNIException(False) then
  begin
    DebugLn('[TSqliteJNIDataset.RetrieveFieldDefs] Java Exceptiong calling AndroidDB.query ' + ReturnString);
    DatabaseError(ReturnString, Self);
    Exit;
  end;

  //sqlite3_step(vm);

  //
  // Obtain the number of columns
  //

  // abstract String getColumnName(int columnIndex)
  //    int ColumnCount = c.getColumnCount();
  ColumnCount := javaEnvRef^^.CallIntMethod(javaEnvRef, dbCursor, FDBCursor_getColumnCount);
  RowCount := javaEnvRef^^.CallIntMethod(javaEnvRef, dbCursor, FDBCursor_getCount);
  //DebugLn(Format('[TSqliteJNIDataset.RetrieveFieldDefs] ColumnCount=%d RowCount=%d', [ColumnCount, RowCount]));
  //Prepare the array of pchar2sql functions
  SetLength(FGetSqlStr, ColumnCount);
  for i := 0 to ColumnCount - 1 do
  begin
    //DebugLn('[TSqliteJNIDataset.RetrieveFieldDefs] for i='+IntToStr(i));
    //
    // First get the column name
    //
    // abstract String getColumnName(int columnIndex)
    lParams[0].i := i;
    lJavaString := javaEnvRef^^.CallObjectMethodA(javaEnvRef, dbCursor, FDBCursor_getColumnName, @lParams[0]);
    lNativeString := javaEnvRef^^.GetStringUTFChars(javaEnvRef, lJavaString, nil);
    ColumnName := lNativeString;
    javaEnvRef^^.ReleaseStringUTFChars(javaEnvRef, lJavaString, lNativeString);
    javaEnvRef^^.DeleteLocalRef(javaEnvRef, lJavaString);
    //DebugLn('[TSqliteJNIDataset.RetrieveFieldDefs] ColumnName='+ColumnName);

    //
    // Now obtain the data size and type
    //
    DataSize := 0;

    // Before Android 3.0 there is no way to know the type of the field, so just suppose it is string
    if (android_os_Build_VERSION_SDK_INT < 11) or (RowCount <= 0) then
    begin
      AType := ftString;
      DataSize := DefaultStringSize;
    end
    else
    // In Android 3.0 we can use Cursor.getType
    // but it throws a CursorIndexOutOfBoundsException if the cursor is in position -1
    // so if the database has no rows, getType doesn't work o.O
    begin
      // getType won't work if we don't first move to a row
      javaEnvRef^^.CallBooleanMethod(javaEnvRef, dbCursor, FDBCursor_moveToFirst);

      // public abstract int getType (int columnIndex) // Added in API level 11
      lParams[0].i := i;
      lColumnType := javaEnvRef^^.CallIntMethodA(javaEnvRef, dbCursor, FDBCursor_getType, @lParams[0]);
      //DebugLn('[TSqliteJNIDataset.RetrieveFieldDefs] dbCursor.getType()='+IntToStr(lColumnType));

      case lColumnType of
        FIELD_TYPE_BLOB:
        begin
          AType := ftString;
          DataSize := DefaultStringSize;
        end;
        FIELD_TYPE_FLOAT:
        begin
          AType := ftFloat;
        end;
        FIELD_TYPE_INTEGER:
        begin
          {if AutoIncrementKey and (UpperCase(String(sqlite3_column_name(vm, i))) = UpperCase(PrimaryKey)) then
          begin
            AType := ftAutoInc;
            FAutoIncFieldNo := i;
          end
          else}
            AType := ftInteger;
        end;
        FIELD_TYPE_NULL:
        begin
          AType := ftString;
          DataSize := DefaultStringSize;
        end;
        FIELD_TYPE_STRING:
        begin
          AType := ftString;
          DataSize := DefaultStringSize;
        end;
      end;
    end;

    FieldDefs.Add(ColumnName, AType, DataSize);
    //Set the pchar2sql function
    if AType in [ftString, ftMemo] then
      FGetSqlStr[i] := @Char2SQLStr
    else
      FGetSqlStr[i] := @Num2SQLStr;
    {$ifdef DEBUG_SQLITEDS}
    DebugLn('  Field[', i, '] Name: ', sqlite3_column_name(vm, i));
    DebugLn('  Field[', i, '] Type: ', sqlite3_column_decltype(vm, i));
    {$endif}
  end;
  //sqlite3_finalize(vm);
  javaEnvRef^^.CallVoidMethod(javaEnvRef, dbCursor, FDBCursor_close);
  RealInternalCloseHandle();
end;

function TSqliteJNIDataset.SqliteExec(ASQL: PChar; ACallback: TSqliteCdeclCallback; Data: Pointer): Integer;
var
  // array for the parameters
  lParams: array[0..2] of JValue;
  lJavaString: JString;
  lSQLStrings: TStrings;
  i: Integer;
begin
  DebugLn(Format('[TSqliteJNIDataset.SqliteExec] AndroidDB=%x ACallback=%x Data=%x ASQL=%s',
    [PtrInt(AndroidDB), PtrInt(ACallback), PtrInt(Data), StrPas(ASQL)]));

  // If we don't renew our AndroidDB, we get crashes =/
  //I/lclapp  (  901): [TSqliteJNIDataset.SqliteExec] AndroidDB=410A5178 ACallback=0 Data=0 ASQL=BEGIN;INSERT INTO TestTable (FirstFieldStr,SecondFieldInt) VALUES ('fe1
  //I/lclapp  (  901): ',NULL);COMMIT;
  // W/dalvikvm(  901): JNI WARNING: 0x410a5178 is not a valid JNI reference
  // W/dalvikvm(  901):              in Lcom/pascal/lcltest/LCLActivity;.LCLOnTouch:(FFI)I (CallVoidMethodA)
  RealInternalGetHandle();

  // Split the SQL and execute each part
  // because SqliteDatabase.execSQL can execute only 1 single SQL statement, not multiple ones
  lSQLStrings := SplitSQLStatements(StrPas(ASQL));

  for i := 0 to lSQLStrings.Count-1 do
  begin
    DebugLn('[TSqliteJNIDataset.SqliteExec] Executing SQL part: ' + lSQLStrings.Strings[i]);
    // void execSQL(String sql)
    // preparations
    lJavaString := javaEnvRef^^.NewStringUTF(javaEnvRef, PChar(lSQLStrings.Strings[i]));
    lParams[0].l := lJavaString;
    // Call the method
    javaEnvRef^^.CallVoidMethodA(javaEnvRef, AndroidDB, FSqliteDatabase_execSQL, @lParams[0]);
    // clean up
    javaEnvRef^^.DeleteLocalRef(javaEnvRef, lJavaString);
  end;

  // Call the callback
  if ACallback <> nil then
  begin
    DebugLn('[TSqliteJNIDataset.SqliteExec] Calling callback');
    ACallback(Data, 0, nil, nil);
  end;

  RealInternalCloseHandle();
  lSQLStrings.Free;

  DebugLn('[TSqliteJNIDataset.SqliteExec] END');
end;

function TSqliteJNIDataset.CheckJNIException(ARaisePascalException: Boolean): Boolean;
var
  exceptionObj: jthrowable;
  javaLangClass: jclass;
  javaLangClass_getName: JMethodID;
  lJavaString: JString;
  lNativeString: PChar;
begin
  Result := False;
  FReturnString := '';
  // There seams to be no way to get any information about the exception in JNI =(
  //DebugLn('[TSqliteJNIDataset.CheckJNIException] START');

  if javaEnvRef^^.ExceptionCheck(javaEnvRef) = JNI_FALSE then
  begin
    Exit;
  end;

  DebugLn('[TSqliteJNIDataset.CheckJNIException] Exception found');
  Result := True;
  FReturnString := 'JNI Exception! See adb logcat for more details.';
  exceptionObj := javaEnvRef^^.ExceptionOccurred(javaEnvRef);
  javaEnvRef^^.ExceptionDescribe(javaEnvRef);
  javaEnvRef^^.ExceptionClear(javaEnvRef);
  if ARaisePascalException then raise Exception.Create(FReturnString);

  // Code for reading info from the exception object has failed for me.
  {DebugLn('[TSqliteJNIDataset.PrepareReturnString] A exceptionObj='+IntToHex(Cardinal(exceptionObj), 8));
  javaLangClass := javaEnvRef^^.FindClass(javaEnvRef, 'java/lang/Class');
  javaLangClass_getName := javaEnvRef^^.GetMethodID(javaEnvRef, javaLangClass, 'getName', '()Ljava/lang/String;');
  DebugLn('[TSqliteJNIDataset.PrepareReturnString] B');
  lJavaString := javaEnvRef^^.CallObjectMethod(javaEnvRef, exceptionObj, javaLangClass_getName); // <--- crashes here
  DebugLn('[TSqliteJNIDataset.PrepareReturnString] C lJavaString='+IntToHex(Cardinal(lJavaString), 8));
  lNativeString := javaEnvRef^^.GetStringUTFChars(javaEnvRef, lJavaString, nil);
  DebugLn('[TSqliteJNIDataset.PrepareReturnString] D');
  FReturnString := StrPas(lNativeString);
  javaEnvRef^^.ReleaseStringUTFChars(javaEnvRef, lJavaString, lNativeString);
  javaEnvRef^^.DeleteLocalRef(javaEnvRef, lJavaString);
  javaEnvRef^^.DeleteLocalRef(javaEnvRef, exceptionObj);
  DebugLn('[TSqliteJNIDataset.PrepareReturnString] FReturnString=' + FReturnString);}
end;

procedure TSqliteJNIDataset.ExecuteDirect(const ASQL: String);
var
  // array for the parameters
  lParams: array[0..2] of JValue;
  lJavaString: JString;
begin
  DebugLn('[TSqliteJNIDataset.ExecuteDirect] ' + ASQL);
  RealInternalGetHandle();

  // void execSQL(String sql)
  // preparations
  lJavaString :=javaEnvRef^^.NewStringUTF(javaEnvRef, PChar(ASQL));
  lParams[0].l := lJavaString;
  // Call the method
  javaEnvRef^^.CallVoidMethodA(javaEnvRef, AndroidDB, FSqliteDatabase_execSQL, @lParams[0]);
  // clean up
  javaEnvRef^^.DeleteLocalRef(javaEnvRef, lJavaString);
  // Check for exceptions
  CheckJNIException(True);

  RealInternalCloseHandle();

  {FReturnCode := sqlite3_prepare(FSqliteHandle, Pchar(ASQL), -1, @vm, nil);
  if FReturnCode <> SQLITE_OK then
    DatabaseError(ReturnString, Self);
  FReturnCode := sqlite3_step(vm);
  sqlite3_finalize(vm);}
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

function TSqliteJNIDataset.ReturnString: String;
begin
  Result := FReturnString;
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

end.

