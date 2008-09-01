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
  sysutils,teststr,testu,tresults,dbtests,
  dom, XMLRead, dateutils;


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
  coWidgetset,
  coFPCVersion,
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
  'fpcversion',
  'date',
  'submitter',
  'machine',
  'comment',
  'testsrcdir',
  'relsrcdir',
  'verbose'
);

ConfigOpts : Array[TConfigOpt] of char
           = ('d','h','u','p','l','o','c','w', 'v','t','s','m','C','S','r','V');

Var
  TestOS,
  TestCPU,
  TestFPCVersion,
  TestLazVersion,
  TestWidgetSet,
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
    coFPCVersion   : TestFPCVersion:=Value;
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
    O:=ParamStr(I);
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
        O:=ParamStr(I);
        SetOpt(c,o);
        end;
      end;
    Inc(I);
    end;
end;

Var
  Doc: TXMLDocument;
  TestCPUID : Integer;
  TestOSID  : Integer;
  TestFPCVersionID : Integer;
  TestLazVersionID : Integer;
  TestWidgetSetID : Integer;
  TestRunID : Integer;
  TestCount, ErrorCount, FailureCount, IgnoreCount: integer;

Procedure GetIDs;
var
  EnvNode: TDOMElement;

  function GetEnvValue(const Name: string): string;
  var
    Node: TDomNode;
  begin
    Node := EnvNode.FindNode(Name);
    if not assigned(Node) then
      Verbose(V_Error,'No environment element for '+Name);
    Result := Node.TextContent;
    Verbose(V_Debug,format('Environment: retrieved %s=%s', [Name, Result]));
  end;

  function TryGetEnvValue(const Name, Default: string): string;
  var
    Node: TDomNode;
  begin
    Result:=Default;
    Node := EnvNode.FindNode(Name);
    if not assigned(Node) then
      Verbose(V_DEBUG,'No environment element for '+Name)
    else
      Result := Node.TextContent;
    Verbose(V_Debug,format('Environment: retrieved %s=%s', [Name, Result]));
  end;

  function GetDate: TDateTime;
  var
    Node: TDomNode;
  begin
    Result := 0;
    Node := Doc.FirstChild.FindNode('DateTimeRan') as TDomElement;
    if assigned(Node) then
      Result := ScanDateTime('YYYY-MM-DD hh:nn:ss', Node.TextContent)
    else
      Verbose(V_Debug,'No DateTimeRan node');
  end;

begin
  EnvNode := Doc.FirstChild.FindNode('Environment') as TDomElement;
  if not Assigned(EnvNode) then
    Verbose(V_Error,'No environment element');
  TestCPU := GetEnvValue('CPU');
  TestCPUID := GetCPUId(TestCPU);
  If TestCPUID=-1 then
    Verbose(V_Error,'NO ID for CPU "'+TestCPU+'" found.');

  TestOS := GetEnvValue('OS');
  TestOSID  := GetOSID(TestOS);
  If TestOSID=-1 then
    Verbose(V_Error,'NO ID for OS "'+TestOS+'" found.');

  TestFPCVersion := GetEnvValue('FPCVersion');
  TestFPCVersionID  := GetFPCVersionID(TestFPCVersion);
  If TestFPCVersionID=-1 then
    Verbose(V_Error,'NO ID for fpc version "'+TestFPCVersion+'" found.');

  TestLazVersion := GetEnvValue('LazVersion');
  TestLazVersionID  := GetLazVersionID(TestLazVersion);
  If TestLazVersionID=-1 then
    Verbose(V_Error,'NO ID for lazarus version "'+TestLazVersion+'" found.');

  TestWidgetSet := GetEnvValue('WidgetSet');
  TestWidgetSetID  := GetWidgetSetID(TestWidgetSet);
  If TestWidgetSetID=-1 then
    Verbose(V_Error,'NO ID for fpc version "'+TestLazVersion+'" found.');

  TestDate := GetDate;

  Submitter:=TryGetEnvValue('Submitter', Submitter);
  Machine:=TryGetEnvValue('Machine', Machine);

  If (Round(TestDate)=0) then
    Testdate:=Now;
  TestRunID:=GetRunID(TestOSID,TestCPUID,TestFPCVersionID,TestLazVersionID, TestWidgetSetID, TestDate);
  If (TestRunID=-1) then
    begin
    TestRunID:=AddRun(TestOSID,TestCPUID,TestFPCVersionID,TestLazVersionID,TestWidgetSetID,TestDate);
    If TestRUnID=-1 then
      Verbose(V_Error,'Could not insert new testrun record!');
    end
  else
    CleanTestRun(TestRunID);
end;

procedure GetCounts;
  Function GetIntValue(name: string): integer;
  var
    Node: TDomNode;
  begin
    Result := -2;
    Node := Doc.FirstChild.FindNode(name);
    if not assigned(Node) then
      Verbose(V_Error,'No element for '+Name);
    Result := StrToIntDef(Node.TextContent,-1);
    Verbose(V_Debug,format('Environment: retrieved %s=%d', [Name, Result]));
  end;
begin
  TestCount := GetIntValue('NumberOfRunTests');
  ErrorCount := GetIntValue('NumberOfErrors');
  FailureCount := GetIntValue('NumberOfFailures');
  IgnoreCount := GetIntValue('NumberOfIgnoredTests');
end;

Function GetLog(FN : String) : String;

begin
  FN:=ChangeFileExt(FN,'.log');
  If FileExists (FN) then
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
//      If Not ExpectRun[TS] then
        //begin
        //ID:=RequireTestID(Line);
        //If (ID<>-1) then
          //begin
          //If Not (TestOK[TS] or TestSkipped[TS]) then
            //begin
              //TestLog:=GetExecuteLog(Line);
              //if pos(failed_to_compile,TestLog)=1 then
                //TestLog:=GetLog(Line);
            //end
          //else
            //TestLog:='';
          //AddTestResult(ID,TestRunID,Ord(TS),TestOK[TS],TestSkipped[TS],TestLog);
          //end;
        //end
      end
    else
      Inc(UnknownLines);
    end;
  close(logfile);
end;

procedure ProcessTestListing;

  procedure ProcessNodes(AParentNode: TDomNode; APath: string); forward;

  procedure ProcessTestSuite(ATestSuiteElement: TDOMElement; APath: string);
  var
    Name: string;
  begin
    Name := ATestSuiteElement.GetAttribute('Name');
    Verbose(V_NORMAL,'Analysing result for testsuite '+Name + ' at '+ APath);
    ProcessNodes(ATestSuiteElement, APath + '|' + Name);
  end;

  procedure ProcessTest(ATestElement: TDOMElement; APath: string);
  var
    Name, FullName: string;
    TestResult, TestLog: string;
    TS: TTestStatus;
    ID: LongInt;
  begin
    Name := ATestElement.GetAttribute('Name');
    TestResult := ATestElement.GetAttribute('Result');
    TestLog := '';
    Verbose(V_NORMAL,'Analysing result for test '+Name);
    FullName := APath + '|' + Name;
    ID := RequireTestID(FullName);
    TS:=GetTestStatus(TestResult);
    Verbose(V_Debug,'Test result: '+TestResult+' TestStatus: '+IntToStr(ord(TS)));
    if TS in [stFailed, stError] then
      TestLog := ATestElement.FindNode('Message').TextContent;
    AddTestResult(ID,TestRunID,Ord(TS),TestOK[TS],TestSkipped[TS],TestLog);
  end;

  procedure ProcessNodes(AParentNode: TDomNode; APath: string);

  var
    Node: TDomNode;
    Element: TDomElement absolute Node;
  begin
    Node := AParentNode.FirstChild;
    while assigned(Node) do
    begin
      if node is TDOMElement then
      begin
        if Element.TagName='TestSuite' then
          ProcessTestSuite(Element, APath)
        else if Element.TagName='Test' then
          ProcessTest(Element, APath);
      end;
      Node := Node.NextSibling;
    end;
  end;

var
  ListingNode: TDomNode;
begin
  ListingNode := Doc.FirstChild.FindNode('TestListing');
  if assigned(ListingNode) then
    ProcessNodes(ListingNode, '');
end;

procedure ProcessResultsFile(FN: String);

begin
  Verbose(V_Debug,'Start ProcessResultsFile');
  if not FileExists(FN) then
    Verbose(V_Error,'Results file "'+ResultsFileName+'" does not exist.');

  ReadXMLFile(Doc, FN);
  GetIDs;
  GetCounts;
  ProcessTestListing;
  Doc.Free;
  Verbose(V_Debug,'End ProcessResultsFile');
end;

procedure UpdateTestRun;

  var
     qry : string;
     res : TQueryResult;

  begin
    qry:='UPDATE TESTRUN SET ';
    //for i:=low(TTestStatus) to high(TTestStatus) do
      //qry:=qry+format('%s=%d, ',[SQLField[i],StatusCount[i]]);
    qry:=qry+format('TU_TESTCOUNT=%d, TU_ERRORCOUNT=%d, TU_FAILURECOUNT=%d, TU_IGNORECOUNT=%d,',
                [TestCount, ErrorCount, FailureCount, IgnoreCount]);
    qry:=qry+format(' TU_SUBMITTER="%s", TU_MACHINE="%s", TU_COMMENT="%s", TU_DATE="%s"',
                [Submitter,Machine,Comment,SqlDate(TestDate)]);
    qry:=qry+' WHERE TU_ID='+format('%d',[TestRunID]);
    RunQuery(Qry,res)
  end;


begin
  DoVerbose := true;
  ProcessConfigFile('dbdigest.cfg');
  ProcessCommandLine;
  If ResultsFileName<>'' then
    begin
    ConnectToDatabase(DatabaseName,HostName,UserName,Password);
    ProcessResultsFile(ResultsFileName);
    UpdateTestRun;
    end
  else
    Verbose(V_ERROR,'Missing log file name');
end.
