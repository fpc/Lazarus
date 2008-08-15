{$mode objfpc}
{$H+}

unit dbtests;

Interface

Uses
{$ifndef ver1_0}
  mysql4,
{$else}
  mysql,
{$endif}  
  testu;

{ ---------------------------------------------------------------------
  High-level access
  ---------------------------------------------------------------------}

Function GetTestID(Name : string) : Integer;
Function GetOSID(Name : String) : Integer;
Function GetCPUID(Name : String) : Integer;
Function GetCategoryID(Name : String) : Integer;
Function GetVersionID(Name : String) : Integer;
Function GetRunID(OSID, CPUID, VERSIONID : Integer; Date : TDateTime) : Integer;
Function AddRun(OSID, CPUID, VERSIONID, CATEGORYID : Integer; Date : TDateTime) : Integer;
Function AddTest(Name : String; AddSource : Boolean) : Integer;
Function UpdateTest(ID : Integer; Info : TConfig; Source : String) : Boolean;
Function AddTestResult(TestID,RunID,TestRes : Integer;
                       OK, Skipped : Boolean;
                       Log : String) : Integer;
Function RequireTestID(Name : String): Integer;
Function CleanTestRun(ID : Integer) : Boolean;

{ ---------------------------------------------------------------------
    Low-level DB access.
  ---------------------------------------------------------------------}


Type
  TQueryResult = PMYSQL_RES;

Function  ConnectToDatabase(DatabaseName,Host,User,Password : String) : Boolean;
Procedure DisconnectDatabase;
Function  RunQuery (Qry : String; Var res : TQueryResult) : Boolean ;
Procedure FreeQueryResult (Res : TQueryResult);
Function  GetResultField (Res : TQueryResult; Id : Integer) : String;
Function  IDQuery(Qry : String) : Integer;
Function  EscapeSQL( S : String) : String;
Function SQLDate(D : TDateTime) : String;

var
  RelSrcDir,
  TestSrcDir : string;

Implementation

Uses
  SysUtils;

{ ---------------------------------------------------------------------
    Low-level DB access.
  ---------------------------------------------------------------------}


Var
  Connection : TMYSQL;


Function ConnectToDatabase(DatabaseName,Host,User,Password : String) : Boolean;

Var
  S : String;

begin
  Verbose(V_DEBUG,'Connection params : '+DatabaseName+' '+Host+' '+User+' '+Password);
{$ifdef ver1_0}  
  Result:=mysql_connect(@Connection,PChar(Host),PChar(User),PChar(Password))<>Nil;
{$else}  
  mysql_init(@Connection);
  Result:=mysql_real_connect(@Connection,PChar(Host),PChar(User),PChar(Password),Nil,0,Nil,0)<>Nil;
{$endif}  
  If Not Result then
    begin
    S:=Strpas(mysql_error(@connection));
    Verbose(V_ERROR,'Failed to connect to database : '+S);
    end
  else
    begin
    Result:=Mysql_select_db(@Connection,Pchar(DatabaseName))>=0;
    If Not result then
      begin
      S:=StrPas(mysql_error(@connection));
      DisconnectDatabase;
      Verbose(V_Error,'Failed to select database : '+S);
      end;
    end;
end;

Procedure DisconnectDatabase;

begin
  mysql_close(@Connection);
end;

Function RunQuery (Qry : String; Var res : TQueryResult) : Boolean ;

begin
  Verbose(V_DEBUG,'Running query:'+Qry);
  Result:=mysql_query(@Connection,PChar(qry))=0;
  If Not Result then
    Verbose(V_WARNING,'Query : '+Qry+'Failed : '+Strpas(mysql_error(@connection)))
  else
    Res:=Mysql_store_result(@connection);
end;

Function GetResultField (Res : TQueryResult; Id : Integer) : String;

Var
  Row : TMYSQL_ROW;

begin
  if Res=Nil then
    Result:=''
  else
    begin
    Row:=mysql_fetch_row(Res);
    If (Row=Nil) or (Row[ID]=Nil) then
      Result:=''
    else
      Result:=strpas(Row[ID]);
    end;
  Verbose(V_DEBUG,'Field value '+Result);
end;

Procedure FreeQueryResult (Res : TQueryResult);

begin
  mysql_free_result(Res);
end;

Function IDQuery(Qry : String) : Integer;

Var
  Res : TQueryResult;

begin
  Result:=-1;
  If RunQuery(Qry,Res) then
    begin
    Result:=StrToIntDef(GetResultField(Res,0),-1);
    FreeQueryResult(Res);
    end;
end;

Function EscapeSQL( S : String) : String;


begin
  Result:=StringReplace(S,'"','\"',[rfReplaceAll]);
  Verbose(V_DEBUG,'EscapeSQL : "'+S+'" -> "'+Result+'"');
end;


Function SQLDate(D : TDateTime) : String;

begin
  Result:=FormatDateTime('YYYY/MM/DD hh:nn:ss',D);
end;

{ ---------------------------------------------------------------------
  High-level access
  ---------------------------------------------------------------------}


Function GetTestID(Name : string) : Integer;

Const
  SFromName = 'SELECT T_ID FROM TESTS WHERE (T_NAME="%s")';

begin
  Result:=IDQuery(Format(SFromName,[Name]));
end;

Function GetOSID(Name : String) : Integer;

Const
  SFromName = 'SELECT TO_ID FROM TESTOS WHERE (TO_NAME="%s")';

begin
  Result:=IDQuery(Format(SFromName,[Name]));
end;

Function GetVersionID(Name : String) : Integer;

Const
  SFromName = 'SELECT TV_ID FROM TESTVERSION WHERE (TV_VERSION="%s")';

begin
  Result:=IDQuery(Format(SFromName,[Name]));
end;

Function GetCPUID(Name : String) : Integer;

Const
  SFromName = 'SELECT TC_ID FROM TESTCPU WHERE (TC_NAME="%s")';

begin
  Result:=IDQuery(Format(SFromName,[Name]));
end;

Function GetCategoryID(Name : String) : Integer;

Const
  SFromName = 'SELECT TCAT_ID FROM TESTCATEGORY WHERE (TCAT_NAME="%s")';

begin
  Result:=IDQuery(Format(SFromName,[Name]));
end;

Function GetRunID(OSID, CPUID, VERSIONID : Integer; Date : TDateTime) : Integer;


Const
  SFromIDS = 'SELECT TU_ID FROM TESTRUN WHERE '+
             ' (TU_OS_FK=%d) '+
             ' AND (TU_CPU_FK=%d) '+
             ' AND (TU_VERSION_FK=%d) '+
             ' AND (TU_DATE="%s")';

begin
  Result:=IDQuery(Format(SFromIDS,[OSID,CPUID,VERSIONID,SQLDate(Date)]));
end;

Function AddRun(OSID, CPUID, VERSIONID, CATEGORYID : Integer; Date : TDateTime) : Integer;

Const
  SInsertRun = 'INSERT INTO TESTRUN '+
               '(TU_OS_FK,TU_CPU_FK,TU_VERSION_FK,TU_CATEGORY_FK,TU_DATE)'+
               ' VALUES '+
               '(%d,%d,%d,%d,"%s")';

Var
  Res : TQueryResult;

begin
  If RunQuery(Format(SInsertRun,[OSID,CPUID,VERSIONID,CATEGORYID,SQLDate(Date)]),Res) then
    Result:=mysql_insert_id(@connection)
  else
    Result:=-1;
end;

function posr(c : Char; const s : AnsiString) : integer;
var
  i : integer;
begin
  i := length(s);
  while (i>0) and (s[i] <> c) do dec(i);
  Result := i;
end;

function GetUnitTestConfig(const fn : string; var r : TConfig) : Boolean;
var
  Path       : string;
  ClassName  : string;
  MethodName : string;
  slashpos   : integer;
  FileName   : string;
  s          : string;
  t          : text;
begin
  Result := False;
  FillChar(r,sizeof(r),0);
  if pos('.',fn) > 0 then exit; // This is normally not a unit-test
  slashpos := posr('/',fn);
  if slashpos < 1 then exit;
  MethodName := copy(fn,slashpos+1,length(fn));
  Path := copy(fn,1,slashpos-1);
  slashpos := posr('/',Path);
  if slashpos > 0 then
    begin
    ClassName := copy(Path,slashpos+1,length(Path));
    Path := copy(Path,1,slashpos-1);
    end
  else
    begin
    ClassName := Path;
    path := '.';
    end;
  if upper(ClassName[1])<>'T' then exit;
  FileName := TestSrcDir+RelSrcDir+Path+DirectorySeparator+copy(lowercase(ClassName),2,length(classname));
  if FileExists(FileName+'.pas') then
    FileName := FileName + '.pas'
  else if FileExists(FileName+'.pp') then
    FileName := FileName + '.pp'
  else exit;
  
  Verbose(V_Debug,'Reading '+FileName);
  assign(t,FileName);
  {$I-}
   reset(t);
  {$I+}
  if ioresult<>0 then
   begin
     Verbose(V_Error,'Can''t open '+FileName);
     exit;
   end;
  while not eof(t) do
   begin
     readln(t,s);

     if s<>'' then
      begin
        TrimB(s);
        if SameText(copy(s,1,9),'PROCEDURE') then
         begin
           if pos(';',s)>11 then
            begin
              s := copy(s,11,pos(';',s)-11);
              TrimB(s);
              if SameText(s,ClassName+'.'+MethodName) then
               begin
                 Result := True;
                 r.Note:= 'unittest';
               end;
            end;
         end;
      end;
   end;
  close(t);
end;

Function AddTest(Name : String; AddSource : Boolean) : Integer;

Const
  SInsertTest = 'INSERT INTO TESTS (T_NAME,T_ADDDATE)'+
                ' VALUES ("%s",NOW())';

Var
  Info : TConfig;
  Res  : TQueryResult;

begin
  Result:=-1;
  If (FileExists(TestSrcDir+RelSrcDir+Name) and
     GetConfig(TestSrcDir+RelSrcDir+Name,Info)) or
     GetUnitTestConfig(Name,Info) then
    begin
    If RunQuery(Format(SInsertTest,[Name]),Res) then
      begin
      Result:=GetTestID(Name);
      If Result=-1 then
        Verbose(V_WARNING,'Could not find newly added test!')
      else
        If AddSource then
          UpdateTest(Result,Info,GetFileContents(Name))
        else
          UpdateTest(Result,Info,'');
      end
    end
  else
    Verbose(V_ERROR,'Could not find test "'+Name+'" or info about this test.');
end;

Const
  B : Array[Boolean] of String = ('-','+');

Function UpdateTest(ID : Integer; Info : TConfig; Source : String) : Boolean;

Const
  SUpdateTest = 'Update TESTS SET '+
                ' T_CPU="%s", T_OS="%s", T_VERSION="%s",'+
                ' T_GRAPH="%s", T_INTERACTIVE="%s", T_RESULT=%d,'+
                ' T_FAIL="%s", T_RECOMPILE="%s", T_NORUN="%s",'+
                ' T_NEEDLIBRARY="%s", T_KNOWNRUNERROR=%d,'+
                ' T_KNOWN="%s", T_NOTE="%s", T_OPTS = "%s"'+
                ' %s '+
                'WHERE'+
                ' T_ID=%d';


Var
  Qry : String;
  Res : TQueryResult;

begin
  If Source<>'' then
    begin
    Source:=EscapeSQL(Source);
    Source:=', T_SOURCE="'+Source+'"';
    end;
  With Info do
    Qry:=Format(SUpdateTest,[EscapeSQL(NeedCPU),'',EscapeSQL(MinVersion),
                             B[usesGraph],B[IsInteractive],ResultCode,
                             B[ShouldFail],B[NeedRecompile],B[NoRun],
                             B[NeedLibrary],KnownRunError,
                             B[IsKnownCompileError],EscapeSQL(Note),EscapeSQL(NeedOptions),
                             Source,
                             ID
     ]);
  Result:=RunQuery(Qry,res)
end;

Function AddTestResult(TestID,RunID,TestRes : Integer;
                       OK, Skipped : Boolean;
                       Log : String) : Integer;

Const
  SInsertRes='Insert into TESTRESULTS '+
             '(TR_TEST_FK,TR_TESTRUN_FK,TR_OK,TR_SKIP,TR_RESULT,TR_LOG) '+
             ' VALUES '+
             '(%d,%d,"%s","%s",%d,"%s") ';

Var
  Qry : String;
  Res : TQueryResult;

begin
  Result:=-1;
  Qry:=Format(SInsertRes,
              [TestID,RunID,B[OK],B[Skipped],TestRes,EscapeSQL(Log)]);
  If RunQuery(Qry,Res) then
    Result:=mysql_insert_id(@connection);
end;

Function RequireTestID(Name : String): Integer;

begin
  Result:=GetTestID(Name);
  If Result=-1 then
    Result:=AddTest(Name,FileExists(Name));
  If Result=-1 then
    Verbose(V_WARNING,'Could not find or create entry for test '+Name);
end;

Function CleanTestRun(ID : Integer) : Boolean;

Const
  SDeleteRun = 'DELETE FROM TESTRESULTS WHERE TR_TESTRUN_FK=%d';

Var
 Res : TQueryResult;

begin
  Result:=RunQuery(Format(SDeleteRun,[ID]),Res);
end;

end.
