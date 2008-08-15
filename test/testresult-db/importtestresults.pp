{
    This file is part of the Free Pascal test suite.
    Copyright (c) 2002 by the Free Pascal development team.

    This program generates a digest
    of the last tests run.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{$mode objfpc}
{$h+}

program importtestresults;

uses
  sysutils,teststr,testu,tresults,dbtests;


Var
  StatusCount : Array[TTestStatus] of Integer;
  UnknownLines : integer;


Procedure ExtractTestFileName(Var Line : string);

Var I : integer;

begin
  I:=Pos(' ',Line);
  If (I<>0) then
    Line:=Copy(Line,1,I-1);
end;

Function Analyse(Var Line : string; Var Status : TTestStatus) : Boolean;

Var
  TS : TTestStatus;

begin
  Result:=False;
  For TS:=FirstStatus to LastStatus do
    begin
    Result:=Pos(StatusText[TS],Line)=1;
    If Result then
      begin
      Status:=TS;
      Delete(Line,1,Length(StatusText[TS]));
      ExtractTestFileName(Line);
      Break;
      end;
    end;
end;

Type

TConfigOpt = (
  coDatabaseName,
  soHost,
  coUserName,
  coPassword,
  coLogFile,
  coOS,
  coCPU,
  coCategory,
  coVersion,
  coDate,
  coSubmitter,
  coMachine,
  coComment,
  coTestSrcDir,
  coRelSrcDir,
  coVerbose
 );

Const

ConfigStrings : Array [TConfigOpt] of string = (
  'databasename',
  'host',
  'username',
  'password',
  'logfile',
  'os',
  'cpu',
  'widgetset',
  'category',
  'version',
  'date',
  'submitter',
  'machine',
  'comment',
  'testsrcdir',
  'relsrcdir',
  'verbose'
);

ConfigOpts : Array[TConfigOpt] of char
           = ('d','h','u','p','l','o','c','w','a','v','t','s','m','C','S','r','V');

Var
  TestOS,
  TestCPU,
  TestVersion,
  TestCategory,
  DatabaseName,
  HostName,
  UserName,
  Password,
  ResultsFileName,
  Submitter,
  Machine,
  Comment : String;
  TestDate : TDateTime;

Procedure SetOpt (O : TConfigOpt; Value : string);
var
  year,month,day,min,hour : word;
begin
  Case O of
    coDatabaseName : DatabaseName:=Value;
    soHost         : HostName:=Value;
    coUserName     : UserName:=Value;
    coPassword     : Password:=Value;
    coLogFile      : ResultsFileName:=Value;
    coOS           : TestOS:=Value;
    coCPU          : TestCPU:=Value;
    coCategory     : TestCategory:=Value;
    coVersion      : TestVersion:=Value;
    coDate         : 
      begin
        { Formated like YYYYMMDDhhmm }
	if Length(value)=12 then
	  begin
	    year:=StrToInt(Copy(value,1,4));
	    month:=StrToInt(Copy(value,5,2));
	    day:=StrToInt(Copy(Value,7,2));
	    hour:=StrToInt(Copy(Value,9,2));
	    min:=StrToInt(Copy(Value,11,2));
	    TestDate:=EncodeDate(year,month,day)+EncodeTime(hour,min,0,0);
	  end
	else
	  Verbose(V_Error,'Error in date format, use YYYYMMDDhhmm');  
      end;
    coSubmitter    : Submitter:=Value;
    coMachine      : Machine:=Value;
    coComment      : Comment:=Value;
    coVerbose      : DoVerbose:=true;
    coTestSrcDir   :
      begin
        TestSrcDir:=Value;
	if (TestSrcDir<>'') and (TestSrcDir[length(TestSrcDir)]<>'/') then
	  TestSrcDir:=TestSrcDir+'/';
      end;
    coRelSrcDir   :
      begin
        RelSrcDir:=Value;
	if (RelSrcDir<>'') and (RelSrcDir[length(RelSrcDir)]<>'/') then
	  RelSrcDir:=RelSrcDir+'/';
	if (RelSrcDir<>'') and (RelSrcDir[1]='/') then
	  RelSrcDir:=copy(RelSrcDir,2,length(RelSrcDir)-1);
      end;
  end;
end;

Function ProcessOption(S: String) : Boolean;

Var
  N : String;
  I : Integer;
  co : TConfigOpt;

begin
  Verbose(V_DEBUG,'Processing option: '+S);
  I:=Pos('=',S);
  Result:=(I<>0);
  If Result then
    begin
    N:=Copy(S,1,I-1);
    Delete(S,1,I);
    For co:=low(TConfigOpt) to high(TConfigOpt) do
      begin
      Result:=CompareText(ConfigStrings[co],N)=0;
      If Result then
        Break;
      end;
    end;
 If Result then
   SetOpt(co,S)
 else
   Verbose(V_ERROR,'Unknown option : '+n+S);
end;

Procedure ProcessConfigfile(FN : String);

Var
  F : Text;
  S : String;
  I : Integer;

begin
  // Set the default value for old digests without RelSrcDir to the rtl/compiler
  // testsuite
  RelSrcDir:='tests/';
  If Not FileExists(FN) Then
    Exit;
  Verbose(V_DEBUG,'Parsing config file: '+FN);
  Assign(F,FN);
  {$i-}
  Reset(F);
  If IOResult<>0 then
    Exit;
  {$I+}
  While not(EOF(F)) do
    begin
    ReadLn(F,S);
    S:=trim(S);
    I:=Pos('#',S);
    If I<>0 then
      S:=Copy(S,1,I-1);
    If (S<>'') then
      ProcessOption(S);
    end;
  Close(F);
end;

Procedure ProcessCommandLine;

Var
  I : Integer;
  O : String;
  c,co : TConfigOpt;
  Found : Boolean;

begin
  I:=1;
  While I<=ParamCount do
    begin
    O:=Paramstr(I);
    Found:=Length(O)=2;
    If Found then
      For co:=low(TConfigOpt) to high(TConfigOpt) do
        begin
        Found:=(O[2]=ConfigOpts[co]);
        If Found then
          begin
          c:=co;
          Break;
          end;
        end;
    If Not Found then
      Verbose(V_ERROR,'Illegal command-line option : '+O)
    else
      begin
      Found:=(I<ParamCount);
      If Not found then
        Verbose(V_ERROR,'Option requires argument : '+O)
      else
        begin
        inc(I);
        O:=Paramstr(I);
        SetOpt(c,o);
        end;
      end;
    Inc(I);
    end;
end;

Var
  TestCPUID : Integer;
  TestOSID  : Integer;
  TestVersionID  : Integer;
  TestCategoryID : Integer;
  TestRunID : Integer;

Procedure GetIDs;

begin
  TestCPUID := GetCPUId(TestCPU);
  If TestCPUID=-1 then
    Verbose(V_Error,'NO ID for CPU "'+TestCPU+'" found.');
  TestOSID  := GetOSID(TestOS);
  If TestOSID=-1 then
    Verbose(V_Error,'NO ID for OS "'+TestOS+'" found.');
  TestCategoryID := GetCategoryID(TestCategory);
  If TestCategoryID=-1 then
    begin
//    Verbose(V_Error,'NO ID for Category "'+TestCategory+'" found.');
    TestCategoryID:=1;
    end;
  TestVersionID  := GetVersionID(TestVersion);
  If TestVersionID=-1 then
    Verbose(V_Error,'NO ID for version "'+TestVersion+'" found.');
  If (Round(TestDate)=0) then
    Testdate:=Now;
  TestRunID:=GetRunID(TestOSID,TestCPUID,TestVersionID,TestDate);
  If (TestRunID=-1) then
    begin
    TestRunID:=AddRun(TestOSID,TestCPUID,TestVersionID,TestCategoryID,TestDate);
    If TestRUnID=-1 then
      Verbose(V_Error,'Could not insert new testrun record!');
    end
  else
    CleanTestRun(TestRunID);
end;

Function GetLog(FN : String) : String;

begin
  FN:=ChangeFileExt(FN,'.log');
  If FileExists(FN) then
    Result:=GetFileContents(FN)
  else
    Result:='';
end;

Function GetExecuteLog(FN : String) : String;

begin
  FN:=ChangeFileExt(FN,'.elg');
  If FileExists(FN) then
    Result:=GetFileContents(FN)
  else
    Result:='';
end;

Procedure Processfile (FN: String);

var
  logfile : text;
  line : string;
  TS : TTestStatus;
  ID : integer;
  Testlog : string;

begin
  Assign(logfile,FN);
{$i-}
  reset(logfile);
  if ioresult<>0 then
    Verbose(V_Error,'Unable to open log file'+ResultsFileName);
{$i+}
  while not eof(logfile) do
    begin
    readln(logfile,line);
    If analyse(line,TS) then
      begin
      Verbose(V_NORMAL,'Analysing result for test '+Line);
      Inc(StatusCount[TS]);
      If Not ExpectRun[TS] then
        begin
        ID:=RequireTestID(Line);
        If (ID<>-1) then
          begin
          If Not (TestOK[TS] or TestSkipped[TS]) then
            begin
              TestLog:=GetExecuteLog(Line);
              if pos(failed_to_compile,TestLog)=1 then
                TestLog:=GetLog(Line);
            end  
          else
            TestLog:='';
          AddTestResult(ID,TestRunID,Ord(TS),TestOK[TS],TestSkipped[TS],TestLog);
          end;
        end
      end
    else
      Inc(UnknownLines);
    end;
  close(logfile);
end;

procedure ProcessResultsFile;
begin
end;

procedure UpdateTestRun;

  var
     i : TTestStatus;
     qry : string;
     res : TQueryResult;

  begin
    qry:='UPDATE TESTRUN SET ';
    for i:=low(TTestStatus) to high(TTestStatus) do
      qry:=qry+format('%s=%d, ',[SQLField[i],StatusCount[i]]);
    qry:=qry+format('TU_SUBMITTER="%s", TU_MACHINE="%s", TU_COMMENT="%s", TU_DATE="%s"',[Submitter,Machine,Comment,SqlDate(TestDate)]);
    qry:=qry+' WHERE TU_ID='+format('%d',[TestRunID]);
    RunQuery(Qry,res)
  end;


begin
  ProcessConfigFile('dbdigest.cfg');
  ProcessCommandLine;
  If ResultsFileName<>'' then
    begin
    ConnectToDatabase(DatabaseName,HostName,UserName,Password);
    GetIDs;
    ProcessResultsFile(ResultsFileName);
    UpdateTestRun;
    end
  else
    Verbose(V_ERROR,'Missing log file name');
end.
