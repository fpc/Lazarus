{$mode objfpc}
{$h+}
unit utests;

interface

uses cgiapp,sysutils,mysql50conn,sqldb,whtml,dbwhtml,db,
     tresults,
     Classes,ftFont,fpimage,fpimgcanv,fpWritePng,fpcanvas;

const
  TestsuiteURLPrefix='http://svn2.freepascal.org/laztestsuite/';
  TestsuiteCGIURL = TestsuiteURLPrefix+'cgi-bin/testsuite.cgi';
  ViewVCURL='http://svn.freepascal.org/cgi-bin/viewvc.cgi/trunk/tests/';

Type

  { TTestSuite }

  TTestSuite = Class(TCgiApplication)
  Private
    FHTMLWriter : THtmlWriter;
    FComboBoxProducer : TComboBoxProducer;
    FDB : TSQLConnection;
    FTrans : TSQLTransaction;
    FRunID,
    FCompareRunID,
    FTestFileID,
    FTestFileName,
    FFpcVersion,
    FLazVersion,
    FWidgetSet,
    FCPU,
    FOS  : String;
    FDate : TDateTime;
    FDebug,
    FNoSkipped,
    FOnlyFailed : Boolean;
    FRunSkipCount,
    FRunFailedCount,
    FRunCount : Integer;
    FAction,
    FLimit : Integer;
    FTestLastDays : Integer;
    FNeedEnd : boolean;
    Procedure GetOverviewRowAttr(Sender : TObject; Var BGColor : String;
                                   Var Align : THTMLAlign; Var VAlign : THTMLValign;
                                   Var CustomAttr : String) ;
    Procedure GetRunRowAttr(Sender : TObject; Var BGColor : String;
                            Var Align : THTMLAlign; Var VAlign : THTMLValign;
                            Var CustomAttr : String) ;
    Procedure FormatFailedOverview(Sender : TObject; Var CellData : String);
    Procedure FormatTestRunOverview(Sender : TObject; Var CellData : String);
    Procedure FormatFileDetails(Sender: TObject; var CellData: String);
    Procedure FormatTestResult(Sender: TObject; var CellData: String);
    Procedure DoDrawPie(Img : TFPCustomImage; Skipped,Failed,Total : Integer);
  Public
    Function CreateDataset(Qry : String) : TSQLQuery;
    Function CreateTableProducer(DS : TDataset) :TTableProducer;
    Procedure DefaultTableFromQuery(Qry,ALink : String; IncludeRecordCount : Boolean);
    Procedure ComboBoxFromQuery(Const ComboName,Qry : String);
    Procedure ComboBoxFromQuery(Const ComboName,Qry,Value : String);
    Function  GetSingleTon(Const Qry : String) : String;
    Function GetOSName(ID : String) : String;
    Function GetCPUName(ID : String) : String;
    Function GetWidgetSetName(ID : String) : String;
    Function GetFPCVersionName(ID : String) : String;
    Function GetLazVersionName(ID : String) : String;
    Function GetTestFileName(ID : String) : String;
    Function InitCGIVars : Integer;
    Procedure DoRun; override;
    Procedure EmitOverviewForm;
    Procedure ShowRunResults;
    Procedure ShowRunComparison;
    Procedure ShowOneTest;
    Function ConnectToDB : Boolean;
    procedure DisconnectFromDB;
    Procedure EmitTitle(ATitle : String);
    Procedure EmitEnd;
    Procedure ShowRunOverview;
    Procedure CreateRunPie;
    Function  ShowRunData : Boolean;
  end;

implementation


Const
{$i utests.cfg}

{ if utests.cfg is missing, create one with the following contents:
  DefDatabase = 'TESTSUITE';
  DefHost     = '';
  DefDBUser   = ''; // fill this in when compiling.
  DefPassword = ''; // fill this in, too.
}

Const
  SDetailsURL = TestsuiteCGIURL + '?action=1&run1id=%s';

Procedure TTestSuite.DoRun;

begin
  Try
    Try
      Case InitCGIVars of
        0 : EmitOverviewForm;
        1 :
          if Length(FCompareRunID) = 0 then
            ShowRunResults
          else
            ShowRunComparison;
        2 : CreateRunPie;
        3 : ShowOneTest;
{$ifdef TEST}        
        98 :
          begin
            EmitOverviewForm;
            Writeln(stdout,'<PRE>');
            FreeMem(pointer($ffffffff));
            Writeln(stdout,'</PRE>');
          end;
        99 : 
          begin
            EmitOverviewForm;
            Writeln(stdout,'<PRE>');
            Dump_stack(stdout,get_frame);
            Writeln(stdout,'</PRE>');
          end;
{$endif TEST}
        end;
    finally
      EmitEnd; 
      DisConnectFromDB;
    end;
  Finally
    Terminate;
  end;
end;


Function TTestSuite.InitCGIVars : Integer;

Var
  S : String;

begin
  FHtmlWriter:=THTMLWriter.Create(Response);
  FComboBoxProducer:=TComboBoxProducer.Create(Self);
  DateSeparator:='/';
  Result:=0;
  S:=RequestVariables['action'];
  if Length(S) = 0 then
    S:=RequestVariables['TESTACTION'];
  FAction:=StrToIntDef(S,0);
  S:=RequestVariables['limit'];
  if Length(S) = 0 then
    S:=RequestVariables['TESTLIMIT'];
  FLimit:=StrToIntDef(S,50);
  FFpcVersion:=RequestVariables['fpcversion'];
  if Length(FFpcVersion) = 0 then
    FFpcVersion:=RequestVariables['TESTFPCVERSION'];
  FLazVersion:=RequestVariables['lazversion'];
  if Length(FLazVersion) = 0 then
    FFpcVersion:=RequestVariables['TESTLAZVERSION'];
  FCPU:=RequestVariables['cpu'];
  if Length(FCPU) = 0 then
    FCPU:=RequestVariables['TESTCPU'];
  FOS:=RequestVariables['os'];
  if Length(FOS) = 0 then
    FOS:=RequestVariables['TESTOS'];
  FWidgetSet:=RequestVariables['widgetset'];
  if Length(FWidgetSet) = 0 then
    FWidgetSet:=RequestVariables['TESTWIDGETSET'];
  FRunID:=RequestVariables['run1id'];
  if Length(FRunID) = 0 then
    FRunID:=RequestVariables['TESTRUN'];
  S:=RequestVariables['lastdays'];
  if Length(S) = 0 then
    S:=RequestVariables['TESTLASTDAYS'];
  FTestLastDays:=StrToIntDef(S,31);
  S:=RequestVariables['date'];
  if Length(S) = 0 then
    S:=RequestVariables['TESTDATE'];
  if Length(S) > 0 then
    try
      FDate:=StrToDate(S);
    except
      FDate:=0;
    end;
  S:=RequestVariables['failedonly'];
  if Length(S) = 0 then
    S:=RequestVariables['TESTFAILEDONLY'];
  FOnlyFailed:=(S='1');
  S:=RequestVariables['noskipped'];
  if Length(S) = 0 then
    S:=RequestVariables['TESTNOSKIPPED'];
  FNoSkipped:=(S='1');
  FCompareRunID:=RequestVariables['run2id'];
  FTestFileID:=RequestVariables['testfileid'];
  FTestFileName:=RequestVariables['testfilename'];
  FRunCount:=StrToIntDef(RequestVariables['PIETOTAL'],0);
  FRunSkipCount:=StrToIntDef(RequestVariables['PIESKIPPED'],0);
  FRunFailedCount:=StrToIntDef(RequestVariables['PIEFAILED'],0);
  S:=RequestVariables['DEBUGCGI'];
  FDebug:=(S='1');
  Result:=FAction;
end;

Function TTestSuite.ConnectToDB : Boolean;

begin
  Result:=False;
  FDB:=TMySQl50Connection.Create(Self);
  FDB.HostName:=DefHost;
  FDB.DatabaseName:=DefDatabase;
  FDB.UserName:=DefDBUser;
  FDB.Password:=DefPassword;
  FTrans := TSQLTransaction.Create(nil);
  FTrans.DataBase := FDB;
  FDB.Transaction := FTrans;
  FDB.Connected:=True;
  Result:=True;
end;

procedure TTestSuite.DisconnectFromDB;

begin
  If Assigned(FDB) then
    begin
    if (FDB.Connected) then
      FDB.Connected:=False;
    FreeAndNil(FDB);
    FreeAndNil(FTrans);
    end;
end;

Procedure TTestSuite.ComboBoxFromQuery(Const ComboName,Qry: String);

begin
  ComboBoxFromQuery(ComboName,Qry,'')
end;

Procedure TTestSuite.ComboBoxFromQuery(Const ComboName,Qry,Value : String);

Var
  Q : TSQLQuery;

begin
  Q:=TSQLQuery.Create(Self);
  try
    Q.Database:=FDB;
    Q.Transaction:=FTrans;
    Q.SQL.Text:=Qry;
    Q.Open;
    FComboboxProducer.Dataset:=Q;
    FComboBoxProducer.ValueField:=Q.Fields[0].FieldName;
    FComboBoxProducer.DataField:=Q.Fields[1].FieldName;
    FComboBoxProducer.Value:=Value;
    FComboBoxProducer.InputName:=ComboName;
    FComboBoxProducer.CreateComboBox(Response);
  Finally
    Q.Free;
  end;
end;

Function TTestSuite.GetSingleton(Const Qry : String) : String;

Var
  Q : TSQLQuery;

begin
  Result:='';
  if FDEbug then
    begin
      system.Writeln('Query=',Qry);
      system.flush(output);
    end;
  Q:=TSQLQuery.Create(Self);
  try
    Q.Database:=FDB;
    Q.Transaction:=FTrans;
    Q.SQL.Text:=Qry;
    Q.Open;
    Try
      if FDebug and (Q.FieldCount<>1) then
        begin
          system.Writeln('GetSingleton number of fields is not 1, but ',
            Q.FieldCount);
          system.flush(output);
        end;
      If Not (Q.EOF and Q.BOF) then
        Result:=Q.Fields[0].AsString;
    Finally
      Q.Close;
    end;
  finally
    Q.Free;
  end;
end;
Procedure TTestSuite.EmitTitle(ATitle : String);

begin
  AddResponseLn('<HTML>');
  AddResponseLn('<TITLE>'+ATitle+'</TITLE>');
  AddResponseLn('<BODY>');
  FNeedEnd:=true;
end;

Procedure TTestSuite.EmitOverviewForm;

begin
  ConnectToDB;
  ContentType:='text/html';
  EmitContentType;
  EmitTitle(Title);
  With FHTMLWriter do
    begin
    HeaderStart(1);
    Write('View Test suite results');
    HeaderEnd(1);
    Write('Please specify search criteria:');
    ParagraphStart;
    FormStart(TestsuiteCGIURL,'');
    TableStart(2,true);
    RowStart;
      CellStart;
        Write('Operating system:');
      CellNext;
        ComboBoxFromQuery('os','SELECT TO_ID,TO_NAME FROM TESTOS ORDER BY TO_NAME',FOS);
      CellEnd;
    RowNext;
      CellStart;
        Write('Processor:');
      CellNext;
        ComboBoxFromQuery('cpu','SELECT TC_ID,TC_NAME FROM TESTCPU ORDER BY TC_NAME',FCPU);
      CellEnd;
    RowNext;
      CellStart;
        Write('FPC Version');
      CellNext;
        ComboBoxFromQuery('fpcversion','SELECT TFV_ID,TFV_VERSION FROM TESTFPCVERSION ORDER BY TFV_VERSION DESC',FFpcVersion);
      CellEnd;
    RowNext;
      CellStart;
        Write('Lazarus Version');
      CellNext;
        ComboBoxFromQuery('lazversion','SELECT TLV_ID,TLV_VERSION FROM TESTLAZVERSION ORDER BY TLV_VERSION DESC',FLazVersion);
      CellEnd;
    RowNext;
      CellStart;
        Write('Widget set');
      CellNext;
        ComboBoxFromQuery('widgetset','SELECT TW_ID,TW_NAME FROM TESTWIDGETSET ORDER BY TW_NAME',FWidgetSet);
      CellEnd;
    RowNext;
      CellStart;
        Write('Date');
      CellNext;
        If (FDate=0) then
          EmitInput('date','')
        else
          EmitInput('date',DateToStr(FDate));
      CellEnd;
    RowNext;
      CellStart;
        Write('Only failed tests');
      CellNext;
        EmitCheckBox('failedonly','1',FonlyFailed);
      CellEnd;
    RowNext;
      CellStart;
        Write('Hide skipped tests');
      CellNext;
        EmitCheckBox('noskipped','1',FNoSkipped);
      CellEnd;
    RowEnd;
    TableEnd;
    ParaGraphStart;
    EmitSubmitButton('','Search');
    EmitResetButton('','Reset form');
    FormEnd;
    end;
  ShowRunOverview;
end;

procedure TTestSuite.EmitEnd;
begin  
  if not FNeedEnd then
    exit;
  AddResponseLn('</BODY>');
  AddResponseLn('</HTML>');
end;

procedure TTestSuite.GetOverviewRowAttr(Sender: TObject; var BGColor: String;
  var Align: THTMLAlign; var VAlign: THTMLValign; var CustomAttr: String);
begin
  If ((Sender as TTAbleProducer).CurrentRow mod 2=0) then
    BGColor:='#EEEEEE'
end;


Function TTestSuite.CreateDataset(Qry : String) : TSQLQuery;

begin
  Result:=TSQLQuery.Create(Self);
  With Result do
    begin
    Database:=FDB;
    Transaction := FTrans;
    SQL.Text:=Qry;
    end;
end;

Function TTestSuite.CreateTableProducer(DS : TDataset) :TTableProducer;

begin
  Result:=TTableProducer.Create(Self);
  Result.Dataset:=DS;
end;

Procedure TTestSuite.DefaultTableFromQuery(Qry,Alink : String; IncludeRecordCount : Boolean);

Var
  Q : TSQLQuery;

begin
  If FDebug then
    Writeln('Query : '+Qry);
  Q:=CreateDataset(Qry);
  With Q do
    try
      Open;
      Try
        With CreateTableProducer(Q) do
          Try
            Border:=True;
            If (Alink<>'') then
              begin
              CreateColumns(Nil);
              If TableColumns.Count>0 then
                (TableColumns.Items[0] as TTableColumn).ActionURL:=ALink;
              end;
            CreateTable(Response);
          Finally
            Free;
          end;
        If IncludeRecordCount then
          FHTMLWriter.DumpLn(Format('<p>Record count: %d </p>',[Q.RecordCount]));
      Finally
        Close;
      end;
    finally
      Free;
    end;
end;

Procedure TTestSuite.ShowRunOverview;
Const
  SOverview = 'SELECT TU_ID as ID,TU_DATE as Date,TC_NAME as CPU,TO_NAME as OS,'+
               'TW_NAME as `Widget Set`,'+
               'TFV_VERSION as `FPC Version`, TLV_VERSION as `Lazarus Version`,'+
               ' COUNT(TR_ID) as Count,'+
               '(TU_TESTCOUNT - TU_FAILURECOUNT - TU_ERRORCOUNT) AS OK,'+
               '(TU_FAILURECOUNT + TU_ERRORCOUNT) as Failed,'+
               'TU_TESTCOUNT as Total,'+
               'TU_SUBMITTER as Submitter, TU_MACHINE as Machine, TU_COMMENT as Comment '+
              'FROM TESTRESULTS,TESTRUN,TESTCPU,TESTOS,TESTWIDGETSET,TESTFPCVERSION,TESTLAZVERSION '+
              'WHERE '+
               '(TC_ID=TU_CPU_FK) AND '+
               '(TO_ID=TU_OS_FK) AND '+
               '(TW_ID=TU_WS_FK) AND '+
               '(TFV_ID=TU_FPC_VERSION_FK) AND '+
               '(TLV_ID=TU_LAZ_VERSION_FK) AND '+
               '(TR_TESTRUN_FK=TU_ID) '+
               '%s '+
              'GROUP BY TU_ID '+
              'ORDER BY TU_ID DESC LIMIT %d';


Var
  S,A,Qry : String;
  Q : TSQLQuery;

begin
   S:='';
   If (FCPU<>'') and (GetCPUName(FCPU)<>'All') then
     S:=S+' AND (TU_CPU_FK='+FCPU+')';
   if (FOS<>'') and (GetOSName(FOS)<>'All') then
     S:=S+' AND (TU_OS_FK='+FOS+')';
   if (FWidgetSet<>'') and (GetWidgetSetName(FWidgetSet)<>'All') then
     S:=S+' AND (TU_WS_FK='+FWidgetSet+')';
   If (FFpcVersion<>'') and (GetFPCVersionName(FFpcVersion)<>'All')  then
     S:=S+' AND (TU_FPC_VERSION_FK='+FFpcVersion+')';
   If (FLazVersion<>'') and (GetLazVersionName(FLazVersion)<>'All')  then
     S:=S+' AND (TU_LAZ_VERSION_FK='+FLazVersion+')';
   If (Round(FDate)<>0) then
     S:=S+' AND (TU_DATE="'+FormatDateTime('YYYY/MM/DD',FDate)+'")';
   If FOnlyFailed then
     S:=S+' AND (TR_OK="-")';
   A:=SDetailsURL;
   If FOnlyFailed then
     A:=A+'&failedonly=1';
   If FNoSkipped then
     A:=A+'&noskipped=1';
  Qry:=Format(SOverview,[S,FLimit]);
  If FDebug then
    Writeln('Query : '+Qry);
  Q:=CreateDataset(Qry);
  With Q do
    try
      Open;
      Try
        With CreateTableProducer(Q) do
          Try
            Border:=True;
            OnGetRowAttributes:=@GetOverViewRowAttr;
            CreateColumns(Nil);
            TableColumns.ColumnByName('ID').ActionURL:=A;
            TableColumns.ColumnByNAme('Failed').OnGetCellContents:=@FormatFailedOverview;
            CreateTable(Response);
          Finally
            Free;
          end;
        FHTMLWriter.DumpLn(Format('<p>Record count: %d</p>',[Q.RecordCount]));
      Finally
        Close;
      end;
    finally
      Free;
    end;
end;


Function TTestSuite.GetOSName(ID : String) : String;

begin
  if (ID<>'') then
    Result:=GetSingleTon('SELECT TO_NAME FROM TESTOS WHERE TO_ID='+ID);
end;

Function TTestSuite.GetTestFileName(ID : String) : String;

begin
  if (ID<>'') then
    Result:=GetSingleTon('SELECT T_NAME FROM TESTS WHERE T_ID='+ID);
end;

Function TTestSuite.GetCPUName(ID : String) : String;

begin
  if (ID<>'') then
    Result:=GetSingleTon('SELECT TC_NAME FROM TESTCPU WHERE TC_ID='+ID);
end;

function TTestSuite.GetWidgetSetName(ID: String): String;
begin
  if (ID<>'') then
    Result:=GetSingleTon('SELECT TW_NAME FROM TESTWIDGETSET WHERE TW_ID='+ID);
end;

Function TTestSuite.GetFPCVersionName(ID : String) : String;

begin
  if (ID<>'') then
    Result:=GetSingleton('SELECT TFV_VERSION FROM TESTFPCVERSION WHERE TFV_ID='+ID);
end;

function TTestSuite.GetLazVersionName(ID: String): String;
begin
  if (ID<>'') then
    Result:=GetSingleton('SELECT TLV_VERSION FROM TESTLAZVERSION WHERE TLV_ID='+ID);
end;

Function TTestSuite.ShowRunData : Boolean;

Const
  SGetRunData = 'SELECT TU_ID,TU_DATE,TC_NAME,TO_NAME,TW_NAME,' +
                'TU_SUBMITTER,TU_MACHINE,TU_COMMENT,TFV_VERSION,TLV_VERSION '+
                ' FROM TESTRUN,TESTCPU,TESTOS,TESTFPCVERSION,TESTLAZVERSION,TESTWIDGETSET '+
                'WHERE '+
                ' (TC_ID=TU_CPU_FK) AND '+
                ' (TO_ID=TU_OS_FK) AND '+
                ' (TW_ID=TU_WS_FK) AND '+
                ' (TFV_ID=TU_FPC_VERSION_FK) AND '+
                ' (TLV_ID=TU_LAZ_VERSION_FK) AND '+
                ' (TW_ID=TU_WS_FK) AND '+
                ' (TU_ID=%s)';


Var
  Q1,Q2 : TSQLQuery;
  F : TField;
  Date1, Date2: TDateTime;
begin
  Result:=(FRunID<>'');
  If Result then
    begin
    Q1:=CreateDataset(Format(SGetRunData,[FRunID]));
    if Length(FCompareRunID) > 0 then
      Q2:=CreateDataset(Format(SGetRunData,[FCompareRunID]))
    else
      Q2:=nil;
    Try
      Q1.Open;
      if Q2 <> nil then
        Q2.Open;
      Result:=Not (Q1.EOF and Q1.BOF);
      If Result then
        With FHTMLWriter do
          begin
          FormStart(TestsuiteCGIURL,'get');
          EmitHiddenVar('action', '1');
          TableStart(3,true);
          RowStart;
            CellStart;
              Write('Run ID:');
            CellNext;
              EmitInput('run1id',FRunID);
            CellNext;
              EmitInput('run2id',FCompareRunID);
            CellEnd;
          RowNext;
            CellStart;
              Write('Processor:');
            CellNext;
              Write(Q1.FieldByName('TC_NAME').AsString);
            CellNext;
              if Q2 <> nil then
                Write(Q2.FieldByName('TC_NAME').AsString);
            CellEnd;
          RowNext;
            CellStart;
              Write('Operating system:');
            CellNext;
              Write(Q1.FieldByName('TO_NAME').AsString);
            CellNext;
              if Q2 <> nil then
                Write(Q2.FieldByName('TO_NAME').AsString);
            CellEnd;
          RowNext;
            CellStart;
              Write('Widget set:');
            CellNext;
              Write(Q1.FieldByName('TW_NAME').AsString);
            CellNext;
              if Q2 <> nil then
                Write(Q2.FieldByName('TW_NAME').AsString);
            CellEnd;
          RowNext;
            CellStart;
              Write('FPC Version:');
            CellNext;
              Write(Q1.FieldByNAme('TFV_VERSION').AsString);
            CellNext;
              if Q2 <> nil then
                Write(Q2.FieldByNAme('TFV_VERSION').AsString);
            CellEnd;
          RowNext;
            CellStart;
              Write('Lazarus Version:');
            CellNext;
              Write(Q1.FieldByNAme('TLV_VERSION').AsString);
            CellNext;
              if Q2 <> nil then
                Write(Q2.FieldByNAme('TLV_VERSION').AsString);
            CellEnd;
          RowNext;
            CellStart;
              Write('Comment:');
            CellNext;
              Write(Q1.FieldByName('TU_COMMENT').AsString);
            CellNext;
              if Q2 <> nil then
                Write(Q2.FieldByName('TU_COMMENT').AsString);
            CellEnd;
          RowNext;
            CellStart;
              Write('Machine:');
            CellNext;
              Write(Q1.FieldByName('TU_MACHINE').AsString);
            CellNext;
              if Q2 <> nil then
                Write(Q2.FieldByName('TU_MACHINE').AsString);
            CellEnd;
          RowNext;
            CellStart;
              Write('Submitter:');
            CellNext;
              Write(Q1.FieldByName('TU_SUBMITTER').AsString);
            CellNext;
              if Q2 <> nil then
                Write(Q2.FieldByName('TU_SUBMITTER').AsString);
            CellEnd;
          RowNext;
            CellStart;
              Write('Date:');
            CellNext;
              F := Q1.FieldByName('TU_DATE');
              Date1 := F.AsDateTime;
              Write(F.AsString);
            CellNext;
              if Q2 <> nil then
                begin
                F := Q2.FieldByName('TU_DATE');
                Date2 := F.AsDateTime;
                Write(F.AsString);
                end;
            CellEnd;
          RowEnd;
          TableEnd;
          ParagraphStart;
          EmitCheckBox('noskipped','1',FNoSkipped);
          Write(' Hide skipped tests');
	  ParagraphEnd;
	  ParagraphStart;
          EmitCheckBox('failedonly','1',FonlyFailed);
          Write(' Hide successful tests');
          ParagraphEnd;
          ParaGraphStart;
          EmitSubmitButton('','Show/Compare');
          EmitResetButton('','Reset form');
          ParagraphEnd;
          FormEnd;
          { give warning if dates reversed }
          if (Q2 <> nil) and (Date1 > Date2) then
            begin
            ParagraphStart;
            Write('Warning: testruns are not compared in chronological order.');
            ParagraphEnd;
            end;
          end;
    Finally
      Q1.Close;
      Q1.Free;
      if Q2 <> nil then
        begin
        Q2.Close;
        Q2.Free;
        end;
    end;
    end;
end;

Procedure TTestSuite.ShowRunResults;

Var
  S : String;
  Qry : String;
  Q : TSQLQuery;
  FL : String;
  DelayedResponse: TMemoryStream;

begin
  ConnectToDB;
  ContentType:='text/html';
  EmitContentType;
  EmitTitle(Title+' : Search Results');
  With FHTMLWriter do
    begin
    HeaderStart(1);
    Write('Test suite results for run '+FRunID);
    HeaderEnd(1);
    HeaderStart(2);
    Write('Test run data : ');
    HeaderEnd(2);
    If ShowRunData then
      begin
      S:='SELECT T_ID as Id,T_NAME as Filename,TR_SKIP as Skipped'
        +',TR_OK as OK,TR_RESULT as Result'
        +' FROM TESTRESULTS,TESTS'
        +' WHERE (TR_TEST_FK=T_ID) AND (TR_TESTRUN_FK='+FRunID+') ';
        
      If FOnlyFailed then
        S:=S+' AND (TR_OK="-")';
      If FNoSkipped then
        S:=S+' AND (TR_SKIP="-")';
      S:=S+' ORDER BY TR_ID ';
      Qry:=S;
      If FDebug then
        begin
        Writeln('Query : '+Qry);
        Flush(stdout);
      end;
      DelayedResponse := TMemoryStream.Create;
      FRunCount:=0;
      FRunSkipCount:=0;
      FRunFailedCount:=0;
      Q:=CreateDataset(Qry);
      With Q do
        try
          Open;
          Try
            With CreateTableProducer(Q) do
              Try
                Border:=True;
                FL:='Id,Filename';
                If Not FNoSkipped then
                  FL:=FL+',Skipped';
                If Not FOnlyFailed then
                  FL:=FL+',OK';
                FL:=FL+',Result';
                CreateColumns(FL);
                OnGetRowAttributes:=@GetRunRowAttr;
                TableColumns.ColumnByNAme('Filename').OnGetCellContents:=
                  @FormatFileDetails;
                TableColumns.ColumnByNAme('Result').OnGetCellContents:=
                  @FormatTestResult;
                //(TableColumns.Items[0] as TTableColumn).ActionURL:=ALink;
                CreateTable(DelayedResponse);
              Finally
                Free;
              end;
            DumpLn(Format('<p>Record count: %d </p>',[Q.RecordCount]));
          Finally
            Close;
          end;
        finally
          Free;
        end;
      If Not (FRunCount=0) and not (FNoSkipped or FOnlyFailed) then
        begin
        ParaGraphStart;
        TagStart('IMG',Format('Src="'+TestsuiteCGIURL+'?action=2&pietotal=%d&piefailed=%d&pieskipped=%d"',[FRunCount,FRunFailedCount,FRunSkipCount]));
        end;
      HeaderStart(2);
      Write('Detailed test run results:');

      FL:='';
      If FOnlyFailed or FNoSkipped then
        begin
        FL:='';
        If FOnlyFailed then
          FL:='successful';
        if FNoSkipped then
          begin
          If (FL<>'') then
            FL:=FL+' and ';
          FL:=FL+'skipped';
          end;
        Write(' ('+FL+' tests are hidden)');
        end;
      HeaderEnd(2);
      ParaGraphStart;
      DelayedResponse.Position:=0;
      Response.CopyFrom(DelayedResponse, DelayedResponse.Size);
      DelayedResponse.Free;
      end
    else
      Write('No data for test run with ID: '+FRunID);
    end;
end;

Procedure TTestSuite.ShowOneTest;

Var
  S : String;
  Qry : String;
  Q : TSQLQuery;
  i : longint;
  FieldName,FieldValue,
  Log,Source : String;
  Res : Boolean;
begin
  ConnectToDB;
  ContentType:='text/html';
  EmitContentType;
  if FTestFileID='' then
    FTestFileID:=GetSingleton('SELECT T_ID FROM TESTS WHERE T_NAME LIKE ''%'+
     FTestFileName+'%''');
  if FTestFileID<>'' then
    FTestFileName:=GetTestFileName(FTestFileID);
  EmitTitle(Title+' : File '+FTestFileName+' Results');
  With FHTMLWriter do
    begin
    HeaderStart(1);
    Write('Test suite results for test file '+FTestFileName);
    HeaderEnd(1);
    HeaderStart(2);
    Write('Test run data : ');
    HeaderEnd(2);
    if FRunID<>'' then
      begin
        Res:=ShowRunData;
        Res:=true;
      end
    else 
      begin
        // This is useless as it is now
        // It should be integrated into a form probably PM 
        Write('Only failed tests');
        EmitCheckBox('failedonly','1',FonlyFailed);
        Write('Hide skipped tests');
        EmitCheckBox('noskipped','1',FNoSkipped);
        Res:=true;
      end;
    If Res then
      begin
      HeaderStart(2);
      Write('Test file "'+FTestFileName+'" information:');
      HeaderEnd(2);
      ParaGraphStart;
      if FTestFileID<>'' then
        S:='SELECT * FROM TESTS WHERE T_ID='+FTestFileID
      else
        S:='SELECT * FROM TESTS WHERE T_NAME='+FTestFileName;
      Q:=CreateDataSet(S);
      With Q do
        Try
          Open;
          Try
            For i:=0 to FieldCount-1 do
              begin
                FieldValue:=Fields[i].AsString;
                FieldName:=Fields[i].DisplayName;
                
                if (FieldValue<>'') and (FieldValue<>'-') and 
                   (FieldName<>'T_NAME') and (FieldName<>'T_SOURCE') then
                  begin
                    if (FieldValue='+') then
                      Write('Flag ');
                    Write(FieldName);
                    Write(' ');
                    if FieldValue='+' then
                      Write(' set')
                    else
                      Write(FieldValue);
                    DumpLn('<BR>');
                  end;
              end;
           
          Finally
            Close;
          end;
        Finally
          Free;
        end;
      ParaGraphEnd;  
      HeaderStart(2);
      Write('Detailed test run results:');

      HeaderEnd(2);
      ParaGraphStart;
      S:='SELECT TR_ID,TR_TESTRUN_FK,TR_TEST_FK,TR_OK, TR_SKIP,TR_RESULT '
      //S:='SELECT * '
        +' FROM TESTRESULTS '
        +' WHERE  (TR_TEST_FK='+FTestFileID+')';
      If FOnlyFailed then
        S:=S+' AND (TR_OK="-")';
      if Fcomparerunid<>'' then
        S:=S+' AND ((TR_TESTRUN_FK='+Frunid+') OR '+
             '(TR_TESTRUN_FK='+Fcomparerunid+'))'
      else if Frunid<>'' then
        S:=S+' AND (TR_TESTRUN_FK='+Frunid+')'
      else
         S:=S+' ORDER BY TR_TESTRUN_FK DESC LIMIT '+IntToStr(FLimit);
      Qry:=S;
      If FDebug then
      begin
        Writeln('Query : '+Qry);
        Flush(stdout);
      end;
      FRunCount:=0;
      FRunSkipCount:=0;
      FRunFailedCount:=0;
      Q:=CreateDataset(Qry);
      With Q do
        try
          Open;
          Try
            With CreateTableProducer(Q) do
              Try
                Border:=True;
                //FL:='TR_ID,TR_TESTRUN_FK,T_NAME,T_CPU,T_VERSION';
                CreateColumns(Nil);
                TableColumns.ColumnByNAme('TR_TESTRUN_FK').OnGetCellContents:=
                  @FormatTestRunOverview;
                //OnGetRowAttributes:=@GetRunRowAttr;
                TableColumns.ColumnByNAme('TR_RESULT').OnGetCellContents:=
                  @FormatTestResult;
                //(TableColumns.Items[0] as TTableColumn).ActionURL:=ALink;
                CreateTable(Response);
              Finally
                Free;
              end;
           DumpLn(Format('<p>Record count: %d </p>',[Q.RecordCount]));
          Finally
            Close;
          end;
        finally
          Free;
        end;
             //If FDebug then
            if FRunId<>'' then
              begin
                log:=''; 
                Try
                log:=getsingleton('select TR_LOG from TESTRESULTS where (TR_TEST_FK='+ftestfileid
                     +') and (TR_TESTRUN_FK='+frunid+')');
                if Log<>'' then
                  begin
                    HeaderStart(2);
                    Write('Log of '+FRunId+':');
                    HeaderEnd(2);
                    PreformatStart;
                    system.Write(EscapeText(Log));
                    system.flush(output);
                    PreformatEnd;
                  end;
                Finally
                  if Log='' then
                    begin
                      HeaderStart(2);
                      Write('No log of '+FRunId+'.');
                      HeaderEnd(2);
                    end;  
                end;  
              end;  
            if FCompareRunId<>'' then
              begin
                log:=''; 
                Try
                log:=getsingleton('select TR_LOG from TESTRESULTS where (TR_TEST_FK='+ftestfileid
                     +') and (TR_TESTRUN_FK='+fcomparerunid+')');
                if Log<>'' then
                  begin
                    HeaderStart(2);
                    Write('Log of '+FCompareRunId+':');
                    HeaderEnd(2);
                    PreformatStart;
                    system.Write(Log);
                    system.flush(output);
                    PreformatEnd;
                  end;
                Finally
                  if Log='' then
                    begin
                      HeaderStart(2);
                      Write('No log of '+FCompareRunId+'.');
                      HeaderEnd(2);
                    end;  
                end;  
              end;  
            if FDebug then
              Write('After Log.');
            Source:='';
            Try  
            Source:=getsingleton('select T_SOURCE from TESTS where T_ID='+ftestfileid);
            if Source<>'' then
              begin
                HeaderStart(2);
                Write('Source:');
                HeaderEnd(2);
                PreformatStart;
                system.Write(Source);
                system.flush(output);
                PreformatEnd;
              end;
            Finally 
            if Source='' then
              begin
                HeaderStart(3);
                DumpLn('<P>No Source in TestSuite DataBase.</P>');
                DumpLn('Link to SVN view of '+
                  '<A HREF="'+ViewVCURL+FTestFileName+'?view=markup'+
                  '" TARGET="_blank"> '+FTestFileName+'</A> source. ');
                HeaderEnd(3);
              end
            else
              begin
                HeaderStart(3);
                DumpLn('Link to SVN view of '+
                  '<A HREF="'+ViewVCURL+FTestFileName+'?view=markup'+
                  '" TARGET="_blank"> '+FTestFileName+'</A> source. ');
                HeaderEnd(3);
              end;  
            end;  
             if FDebug then
              Write('After Source.');
    end
    else
      Write(Format('No data for test file with ID: %s',[FTestFileID]));
      
    end;
end;

Procedure TTestSuite.ShowRunComparison;

Var
  S : String;
  Qry : String;
  Q : TSQLQuery;
  FL : String;

begin
  ConnectToDB;
  ContentType:='text/html';
  EmitContentType;
  EmitTitle(Title+' : Compare 2 runs');
  With FHTMLWriter do
    begin
    HeaderStart(1);
    Write('Test suite results for run '+FRunID+' vs. '+FCompareRunID);
    HeaderEnd(1);
    HeaderStart(2);
    Write('Test run data: ');
    HeaderEnd(2);
    If ShowRunData then
      begin
      HeaderStart(2);
      Write('Detailed test run results:');

      FL:='';
      If FOnlyFailed or FNoSkipped then
        begin
        FL:='';
        If FOnlyFailed then
          FL:='successful';
        if FNoSkipped then
          begin
          If (FL<>'') then
            FL:=FL+' and ';
          FL:=FL+'skipped';
          end;
        Write(' ('+FL+' tests are hidden)');
        end;
      HeaderEnd(2);
      ParaGraphStart;
      Q:=CreateDataset('');
      Q.SQL.Text:='CREATE TEMPORARY TABLE tr1 like TESTRESULTS;';
      Q.ExecSQL;
      Q.SQL.Text:='CREATE TEMPORARY TABLE tr2 like TESTRESULTS;';
      Q.ExecSQL;
      Q.SQL.Text:='INSERT INTO tr1 SELECT * FROM TESTRESULTS '+
        'WHERE TR_TESTRUN_FK='+FRunID+';';
      Q.ExecSQL;
      Q.SQL.Text:='INSERT INTO tr2 SELECT * FROM TESTRESULTS '+
        'WHERE TR_TESTRUN_FK='+FCompareRunID+';';
      Q.ExecSQL;
      S:='SELECT T_ID as Id,T_NAME as Filename,tr1.TR_SKIP as Run1_Skipped,'
         +'tr2.TR_SKIP as Run2_Skipped,tr1.TR_OK as Run1_OK,'
         +'tr2.TR_OK as Run2_OK, tr1.TR_Result as Run1_Result,'
         +'tr2.TR_RESULT as Run2_Result '
         +'FROM TESTS, tr2 LEFT JOIN tr1 USING (TR_TEST_FK) '
         +'WHERE ((tr1.TR_SKIP IS NULL) or'
         +' (tr2.TR_SKIP IS NULL) or '
         +' (%s (tr1.TR_Result<>tr2.TR_Result)))'
         +'and (T_ID=tr2.TR_TEST_FK)';
      If FNoSkipped then
        begin
        Qry:='(((tr1.TR_SKIP="+") and (tr2.TR_OK="-") and (tr2.TR_SKIP="-")) or '
           +'((tr1.TR_OK="-") and (tr1.TR_SKIP="-") and (tr2.TR_SKIP="+")) or '
           +'((tr1.TR_SKIP="-") and (tr2.TR_SKIP="-"))) and ';
        end
      else
        Qry:='';
      Qry:=Format(S,[Qry]);
      If FDebug then
        begin
        Writeln('Query: '+Qry);
        Flush(stdout);
        end;
      FRunCount:=0;
      FRunSkipCount:=0;
      FRunFailedCount:=0;
      Q.SQL.Text:=Qry;
      With Q do
        try
          Open;
          Try
            With CreateTableProducer(Q) do
              Try
                Border:=True;
                FL:='Filename,Run1_OK,Run2_OK';
                If Not FNoSkipped then
                  FL:=FL+',Run1_Skipped,Run2_Skipped';
                FL:=FL+',Run1_Result,Run2_Result';
                CreateColumns(FL);
                OnGetRowAttributes:=@GetRunRowAttr;
                TableColumns.ColumnByNAme('Run1_Result').OnGetCellContents:=
                  @FormatTestResult;
                TableColumns.ColumnByNAme('Run2_Result').OnGetCellContents:=
                  @FormatTestResult;
                TableColumns.ColumnByNAme('Filename').OnGetCellContents:=
                 @FormatFileDetails;
                //(TableColumns.Items[0] as TTableColumn).ActionURL:=ALink;
                CreateTable(Response);
              Finally
                Free;
              end;
            Writeln('<p>Record count: ',Q.RecordCount,'</p>');
          Finally
            Close;
          end;
        finally
          Free;
        end;
      If Not (FRunCount=0) and not (FNoSkipped or FOnlyFailed) then
        begin
        ParaGraphStart;
        TagStart('IMG',Format('Src="'+TestsuiteCGIURL+'?action=2&pietotal=%d&piefailed=%d&pieskipped=%d"',[FRunCount,FRunFailedCount,FRunSkipCount]));
        end;
      end
    else
      Write('No data for test run with ID: '+FRunID);
    end;
end;

procedure TTestSuite.GetRunRowAttr(Sender: TObject; var BGColor: String;
  var Align: THTMLAlign; var VAlign: THTMLValign; var CustomAttr: String);

Var
  P : TTableProducer;
  Skip1Field, Skip2Field, Run1Field, Run2Field : TField;
begin
  P:=(Sender as TTAbleProducer);
  Inc(FRunCount);
  If (FOnlyFailed and FNoSkipped) then
    begin
    If (P.CurrentRow Mod 2)=0 then
      BGColor:='#EEEEEE'
    end
  else
    begin
    Skip1Field := P.Dataset.FindField('Skipped');
    if Skip1Field = nil then
      begin
      Skip1Field := P.Dataset.FindField('Run1_Skipped');
      Skip2Field := P.Dataset.FindField('Run2_Skipped');
      end
    else
      Skip2Field := nil;
    Run1Field := P.Dataset.FindField('OK');
    if Run1Field = nil then
      Run1Field := P.Dataset.FindField('Run1_OK');
    Run2Field := P.Dataset.FindField('OK');
    if Run2Field = nil then
      Run2Field := P.Dataset.FindField('Run2_OK');
    If (not FNoSkipped) and ((Skip1Field.AsString='+')
        or ((Skip2Field <> nil) and (Skip2Field.AsString = '+'))) then
      begin
      Inc(FRunSkipCount);
      BGColor:='yellow';    // Yellow
      end
    else If Run2Field.AsString='+' then
      begin
      if Run1Field.AsString='' then
        BGColor:='#68DFB8'
      else if Run1Field.ASString<>'+' then
        BGColor:='#98FB98';    // pale Green
      end  
    else if Run2Field.AsString='-' then
      begin
      Inc(FRunFailedCount);
      if Run1Field.AsString='' then
        BGColor:='#FF82AB'    // Light red
      else if Run1Field.AsString<>'-' then
        BGColor:='#FF225B';
      end;
    end;
end;

procedure TTestSuite.FormatFailedOverview(Sender: TObject; var CellData: String);

Var
  S: String;
  P : TTableProducer;

begin
  P:=(Sender as TTableProducer);
  S:=Format(SDetailsURL,[P.DataSet.FieldByName('ID').AsString]);
  S:=S+'&failedonly=1&noskipped=1';
  CellData:=Format('<A HREF="%s">%s</A>',[S,CellData]);
end;

procedure TTestSuite.FormatTestRunOverview(Sender: TObject; var CellData: String);

Var
  S: String;
  P : TTableProducer;

begin
  P:=(Sender as TTableProducer);
  S:=Format(SDetailsURL,[P.DataSet.FieldByName('TR_TESTRUN_FK').AsString]);
  if FOnlyFailed then
    S:=S+'&failedonly=1';
  if FNoSkipped then
    S:=S+'&noskipped=1';
  CellData:=Format('<A HREF="%s">%s</A>',[S,CellData]);
end;

procedure TTestSuite.FormatFileDetails(Sender: TObject; var CellData: String);

Var
  S: String;
  P : TTableProducer;

begin
  P:=(Sender as TTableProducer);
  if FCompareRunID<>'' then
    S:=Format(TestSuiteCGIURL + '?action=3&run1id=%s&run2id=%s&testfileid=%s',
       [FRunID,FCompareRunID,P.DataSet.FieldByName('Id').AsString])
  else 
    S:=Format(TestSuiteCGIURL + '?action=3&run1id=%s&testfileid=%s',
       [FRunID,P.DataSet.FieldByName('Id').AsString]);
  CellData:=Format('<A HREF="%s">%s</A>',[S,CellData]);
end;

procedure TTestSuite.FormatTestResult(Sender: TObject; var CellData: String);

Var
  Res : longint;
  Error:word;
  TS : TTestStatus;
begin
  Val(CellData,Res,Error);
  if (Error=0) and (Res>=longint(FirstStatus)) and
     (Res<=longint(LastStatus)) then
    begin   
      TS:=TTestStatus(Res);
      CellData:=StatusText[TS];
    end;
end;

Procedure TTestSuite.CreateRunPie;

Var
  I : TFPMemoryImage;
  M : TMemoryStream;

begin
  ftFont.InitEngine;
  FontMgr.SearchPath:='/usr/share/fonts/liberation';
  I:=TFPMemoryImage.Create(320,320);
  try
    If FRunCount=0 Then
      Raise Exception.Create('Invalid parameters passed to script: No total count');
    DoDrawPie(I,FRunSkipCount,FRunFailedCount,FRunCount);
    M:=TMemoryStream.Create;
    Try
      With TFPWriterPNG.Create do
        try
          UseAlpha:=True;
          ImageWrite(M,I);
        Finally
          Free;
        end;
      ContentType:='image/png';
      EmitContentType;
      M.Position:=0;
      Response.CopyFrom(M,M.Size);
    Finally
      M.Free;
    end;
  Finally
    I.Free;
  end;
end;

Procedure TTestSuite.DoDrawPie(Img : TFPCustomImage; Skipped,Failed,Total : Integer);

Var
  Cnv : TFPImageCanvas;
  W,H,FH,CR,ra : Integer;
  A1,A2,FR,SR,PR : Double;
  R : TRect;
  F : TFreeTypeFont;

  Procedure AddPie(X,Y,R : Integer; AStart,AStop : Double; Col : TFPColor);

  Var
    DX,Dy : Integer;

  begin
    DX:=Round(R*Cos(AStart));
    DY:=Round(R*Sin(AStart));
    Cnv.Line(X,Y,X+DX,Y-DY);
    DX:=Round(Ra*Cos(AStop));
    DY:=Round(Ra*Sin(AStop));
    Cnv.Line(X,Y,X+DX,Y-Dy);
    DX:=Round(R/2*Cos((AStart+AStop)/2));
    DY:=Round(R/2*Sin((AStart+AStop)/2));
    Cnv.Brush.FpColor:=Col;
    Cnv.FloodFill(X+DX,Y-DY);
  end;

  Function FractionAngle(F,T : Integer): Double;

  begin
    Result:=(2*Pi*(F/T))
  end;



begin
  F:=TFreeTypeFont.Create;
  With F do
    begin
    {$IFDEF windows}
    Name:='arial';
    {$ELSE}
    Name:='LiberationSans-Regular';
    {$ENDIF}
    FontIndex:=0;
    Size:=12;
    FPColor:=colred;
    AntiAliased:=False;
    Resolution:=96;
    end;
  // Writeln('Creating image');
  Cnv:=TFPImageCanvas.Create(Img);
  // Writeln('Getting width and height');
  W:=Img.Width;
  H:=Img.Height;
  // Writeln('Transparant');
  cnv.Brush.Style:=bsSolid;
  cnv.Brush.FPColor:=colTransparent;
  cnv.Pen.FPColor:=colWhite;
  Cnv.Rectangle(0,0,W,H);
  // Writeln('Setting font');
  Cnv.Font:=F;
  // Writeln('Getting textwidth ');
  FH:=CNV.GetTextHeight('A');
  If FH=0 then
    FH:=14; // 3 * 14;
  Inc(FH,4);
  R.Top:=FH*4;
  R.Left:=0;
  R.Bottom:=H-1;
  CR:=H-R.Top;
  If W>CR then
    R.Right:=CR
  else
    R.Right:=W;
  Ra:=CR div 2;

  Cnv.Pen.FPColor:=colBlack;
  Cnv.Ellipse(R);

  FR:=Failed/Total;
  SR:=Skipped/Total;
  PR:=1-(FR+SR);
  cnv.font.FPColor:=colred;
  Cnv.Textout(1,FH,Format('%d Failed (%3.1f%%)',[Failed,Fr*100]));
  cnv.font.FPColor:=colYellow;
  Cnv.Textout(1,FH*2,Format('%d Skipped (%3.1f%%)',[Skipped,SR*100]));
  cnv.font.FPColor:=colGreen;
  Cnv.Textout(1,FH*3,Format('%d Passed (%3.1f%%)',[Total-Skipped-Failed,PR*100]));

  A1:=(Pi*2*(failed/total));
  AddPie(Ra,R.Top+Ra,Ra,0,A1,ColRed);
  A2:=A1+(Pi*2*(Skipped/Total));
  AddPie(Ra,R.Top+Ra,Ra,A1,A2,ColYellow);
  AddPie(Ra,R.Top+Ra,Ra,A2,Pi*2,ColGreen);
end;


end.
