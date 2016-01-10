unit fraquery;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, SynHighlighterSQL, SynEdit, LResources, Forms,
  DB, LCLType, Controls, ComCtrls, StdCtrls, ActnList, Dialogs, ExtCtrls, Menus,
  fpDatadict, fradata, lazdatadeskstr, sqlscript;

type
   TExecuteMode = (emSingle,emSelection,emScript,emSelectionScript);
   TScriptMode = (smStopNextError,smStopNoErrors,smAbort);
   TBusyMode = (bmIdle,bmSingle,bmScript);

  { TQueryFrame }

  TQueryFrame = class(TFrame)
    ACloseQuery: TAction;
    ACreateCode: TAction;
    AExecuteSelectionScript: TAction;
    AExecuteScript: TAction;
    AExecuteSelection: TAction;
    AExecuteSingle: TAction;
    AExport: TAction;
    ASaveSQL: TAction;
    ALoadSQL: TAction;
    ANextQuery: TAction;
    APreviousQuery: TAction;
    AExecute: TAction;
    ALQuery: TActionList;
    ILQuery: TImageList;
    MIExecuteSelectionScript: TMenuItem;
    MIExecuteScript: TMenuItem;
    MIExecuteSelection: TMenuItem;
    MIExecuteSingle: TMenuItem;
    MResult: TMemo;
    ODSQL: TOpenDialog;
    PCResult: TPageControl;
    FMSQL: TSynEdit;
    PMExecute: TPopupMenu;
    SDSQL: TSaveDialog;
    SQuery: TSplitter;
    SQLSyn: TSynSQLSyn;
    TBExecute: TToolButton;
    TBSep1: TToolButton;
    TBPrevious: TToolButton;
    TBClose: TToolButton;
    TBNext: TToolButton;
    TBSep2: TToolButton;
    TBLoadSQL: TToolButton;
    TBSaveSQL: TToolButton;
    TBSep3: TToolButton;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    TSResult: TTabSheet;
    TSData: TTabSheet;
    ToolBar1: TToolBar;
    procedure AExecuteExecute(Sender: TObject);
    procedure AExecuteScriptExecute(Sender: TObject);
    procedure AExecuteSelectionExecute(Sender: TObject);
    procedure AExecuteSelectionScriptExecute(Sender: TObject);
    procedure AExecuteSingleExecute(Sender: TObject);
    procedure BExecClick(Sender: TObject);
    procedure CloseQueryClick(Sender: TObject);
    procedure HaveNextQuery(Sender: TObject);
    procedure HavePreviousQuery(Sender: TObject);
    procedure HaveSQLSelection(Sender: TObject);
    procedure LoadQueryClick(Sender: TObject);
    procedure NextQueryClick(Sender: TObject);
    procedure OnMemoKey(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure PreviousQueryClick(Sender: TObject);
    procedure SaveQueryClick(Sender: TObject);
    procedure ExportDataClick(Sender: TObject);
    procedure CreateCodeClick(Sender: TObject);
    Procedure NotBusy(Sender: TObject);
    Procedure DataShowing(Sender: TObject);
  private
    { private declarations }
    FEngine: TFPDDEngine;
    FQueryHistory : TStrings;
    FCurrentQuery : Integer;
    FBusy : TBusyMode;
    FData : TDataFrame;
    FScript : TEventSQLScript;
    FScriptMode : TScriptMode;
    FErrorCount,
    FStatementCount : Integer;
    FAbortScript : Boolean;
    procedure ClearResults;
    function CountStatements(const S: String): Integer;
    function DetermineExecuteMode: TExecuteMode;
    // Script events
    procedure DoCommit(Sender: TObject);
    procedure DoDirective(Sender: TObject; Directive, Argument: AnsiString;  var StopExecution: Boolean);
    procedure DoSQLStatement(Sender: TObject; Statement: TStrings; var StopExecution: Boolean);
    // Execute SQL
    procedure DoExecuteQuery(Const Qry: String; ACount : Integer = 0);
    procedure LocalizeFrame;
    function SelectionHint: Boolean;
    procedure SetTableNames;
  public
  Protected
    procedure SetEngine(const AValue: TFPDDEngine);
    Function GetDataset: TDataset;
    Procedure CreateControls; virtual;
  Public
    Constructor Create(AOwner : TComponent); override;
    Destructor Destroy; override;
    Function ExecuteQuery(Const Qry: String; ACount : Integer = 0) : Boolean;
    procedure ExecuteScript(AScript: String);
    procedure SaveQuery(AFileName: String);
    procedure LoadQuery(AFileName: String);
    Function AddToHistory(Qry : String) : Integer;
    Function NextQuery : Integer;
    Function PreviousQuery : Integer;
    Procedure CloseDataset;
    Procedure FreeDataset;
    Procedure ExportData;
    Procedure CreateCode;
    Procedure ActivatePanel;
    Property Dataset : TDataset Read GetDataset;
    Property Engine : TFPDDEngine Read FEngine Write SetEngine;
    Property QueryHistory : TStrings Read FQueryHistory;
    Property CurrentQuery : Integer Read FCurrentQuery;
    Property Busy : TBusyMode Read FBusy;
    { public declarations }
  end;

implementation

uses strutils, sqldb, fpdataexporter, fpcodegenerator;

{$r *.lfm}


constructor TQueryFrame.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FQueryHistory:=TStringList.Create;
  FCurrentQuery:=-1;
  CreateControls;
  LocalizeFrame;
end;

destructor TQueryFrame.Destroy;
begin
  FreeAndNil(FQueryHistory);
  inherited Destroy;
end;

procedure TQueryFrame.SetEngine(const AValue: TFPDDEngine);
begin
  if FEngine=AValue then exit;
  If Assigned(Dataset) then
    begin
    CloseDataset;
    FreeDataset;
    end;
  FEngine:=AValue;
  SetTableNames;
end;

procedure TQueryFrame.SetTableNames;

begin
  SQLSyn.TableNames.BeginUpdate;
  try
    SQLSyn.TableNames.Clear;
    if (FEngine=Nil) or Not (FEngine.Connected) then
       exit;
    FEngine.GetTableList(SQLSyn.TableNames);
  finally
    SQLSyn.TableNames.EndUpdate;
  end;
end;

procedure TQueryFrame.ExportDataClick(Sender: TObject);
begin
  ExportData;
end;

procedure TQueryFrame.CreateCodeClick(Sender: TObject);
begin
  CreateCode;
end;

function TQueryFrame.GetDataset: TDataset;
begin
  Result:=FData.Dataset;
end;

procedure TQueryFrame.LocalizeFrame;

begin
  // Localize
  AExecute.Caption:=SExecute;
  AExecute.Hint:=SHintExecute;
  APreviousQuery.Caption:=SPrevious;
  APreviousQuery.Hint:=SHintPrevious;
  ANextQuery.Caption:=SNext;
  ANextQuery.Hint:=SHintNext;
  ALoadSQL.Caption:=SLoad;
  ALoadSQL.Hint:=SHintLoad;
  ASaveSQL.Caption:=SSave;
  ASaveSQL.Hint:=SHintSave;
  ACloseQuery.Caption:=SClose;
  ACloseQuery.Hint:=SHintClose;
  AExport.Caption:=SExport;
  AExport.Hint:=SHintExport;
  ACreateCode.Caption:=SCreateCode;
  ACreateCode.Hint:=SHintCreateCode;
  ODSQL.Filter:=SSQLFilters;
  SDSQL.Filter:=SSQLFilters;
end;

procedure TQueryFrame.CreateControls;

begin
  FData:=TDataFrame.Create(Self);
  FData.Parent:=TSData;
  FData.Align:=alClient;
  FData.Visible:=True;
  FData.ShowExtraButtons:=False;
  MResult.Lines.Clear;
  MResult.Append(SReadyForSQL);
  FScript:=TEventSQLScript.Create(Self);
  FScript.UseDefines:=True;
  FScript.UseSetTerm:=True;
  FScript.UseCommit:=True;
  FScript.OnSQLStatement:=@DoSQLStatement;
  FScript.OnDirective:=@DoDirective;
  FScript.OnCommit:=@DoCommit;
end;

{ ---------------------------------------------------------------------
  Callbacks
  ---------------------------------------------------------------------}

procedure TQueryFrame.OnMemoKey(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  If (Key=VK_E) and (Shift=[ssCtrl]) then
    begin
    AExecute.Execute;
    Key:=0;
    end;
end;

procedure TQueryFrame.ClearResults;

Var
  DS : TDataset;

begin
  MResult.Clear;
  DS:=Dataset;
  If Assigned(DS) then
    CloseDataset;
end;

function TQueryFrame.CountStatements(Const S : String) : Integer;

Var
  I : integer;

begin
  Result:=1;
  For I:=2 To Length(S) do
    If S[I-1]=';' then
      inc(Result);
end;

function TQueryFrame.DetermineExecuteMode: TExecuteMode;

begin
  if SelectionHint then
    begin
    Result:=emSelection;
    if CountStatements(Trim(FMSQL.SelText))>1 then
      Result:=emSelectionScript
    end
  else
    begin
    Result:=emSingle;
    if (FMSQL.Lines.Count>300) then
      Result:=emScript
    else
      if CountStatements(Trim(FMSQL.Lines.Text))>1 then
        result:=emScript
    end;
end;

procedure TQueryFrame.DoSQLStatement(Sender: TObject; Statement: TStrings;
  var StopExecution: Boolean);

Var
  RetryStatement : Boolean;
begin
  Application.ProcessMessages;
  StopExecution:=False;
  RetryStatement:=False;
  Inc(FStatementCount);
  Repeat
    If not ExecuteQuery(Statement.Text,FStatementCount) then
      begin
      If not RetryStatement then
        Inc(FErrorCount);
      if (FScriptMode=smStopNextError) then
        Case QuestionDlg(SErrInScript,SErrInScriptChoice,mtWarning,[
            mrYes,SStopOnNextError,
            mrYesToAll,SStopNoError,
            mrAbort,SAbortScript,
            mrRetry,SRetryStatement
          ],0) of
          mrYesToAll : FScriptMode:=smStopNoErrors;
          mrAbort : StopExecution:=True;
          mrRetry : RetryStatement:=True;
        else
          FScriptMode:=smStopNextError;
        end;
      end;
  until StopExecution or Not RetryStatement;
  if FAbortScript then
    StopExecution:=True;
  Application.ProcessMessages;
end;

procedure TQueryFrame.DoCommit(Sender: TObject);
begin
  MResult.Append(SErrCommitNotSupported);
end;

procedure TQueryFrame.DoDirective(Sender: TObject; Directive, Argument: AnsiString; var StopExecution: Boolean);
begin
  MResult.Append(Format(SErrUnknownDirective,[Directive,Argument]));
  StopExecution:=False;
  // Not yet implemented
end;

procedure TQueryFrame.BExecClick(Sender : TObject);

begin

end;

procedure TQueryFrame.AExecuteExecute(Sender: TObject);

begin
  ClearResults;
  Case DetermineExecuteMode of
    emSingle          : AExecuteSingle.Execute;
    emSelection       : AExecuteSelection.Execute;
    emScript          : AExecuteScript.Execute;
    emSelectionScript : AExecuteSelectionScript.Execute;
  end;
end;

procedure TQueryFrame.AExecuteScriptExecute(Sender: TObject);
begin
  ClearResults;
  ExecuteScript(Trim(FMSQL.Lines.Text));
end;

procedure TQueryFrame.AExecuteSelectionExecute(Sender: TObject);
begin
  ClearResults;
  ExecuteQuery(Trim(FMSQL.SelText));
end;

procedure TQueryFrame.AExecuteSelectionScriptExecute(Sender: TObject);
begin
  ClearResults;
  ExecuteScript(Trim(FMSQL.SelText));
end;

procedure TQueryFrame.AExecuteSingleExecute(Sender: TObject);
begin
  ClearResults;
  ExecuteQuery(Trim(FMSQL.Lines.Text));
end;

procedure TQueryFrame.CloseQueryClick(Sender : TObject);

begin
  CloseDataset;
end;

procedure TQueryFrame.NotBusy(Sender : TObject);

begin
  (Sender as TAction).Enabled:=FBusy=bmIdle;
end;

procedure TQueryFrame.DataShowing(Sender : TObject);

Var
  DS : TDataset;

begin
  DS:=Dataset;
  (Sender as TAction).Enabled:=Assigned(DS) and DS.Active;
end;

procedure TQueryFrame.HaveNextQuery(Sender : TObject);

begin
  (Sender as TAction).Enabled:=(FCurrentQuery<FQueryHistory.Count-1);
end;

procedure TQueryFrame.HavePreviousQuery(Sender : TObject);

begin
  (Sender as TAction).Enabled:=(FCurrentQuery>0);
end;

function TQueryFrame.SelectionHint: Boolean;

Var
  S : String;

begin
  S:=Trim(FMSQL.SelText);
  Result:=WordCount(S,[#10,#13,#9,' '])>1;
end;

procedure TQueryFrame.HaveSQLSelection(Sender: TObject);
begin
  (Sender as TAction).Enabled:=SelectionHint;
end;

procedure TQueryFrame.NextQueryClick(Sender : TObject);

begin
  NextQuery;
end;

procedure TQueryFrame.PreviousQueryClick(Sender : TObject);

begin
  PreviousQuery;
end;

procedure TQueryFrame.LoadQueryClick(Sender : TObject);

begin
  With ODSQL do
    begin
    Options:=[ofFileMustExist];
    If Execute then
      LoadQuery(FileName);
    end;
end;

procedure TQueryFrame.SaveQueryClick(Sender : TObject);

begin
  With SDSQL.Create(Self) do
    begin
    If Execute then
      SaveQuery(FileName);
    end;
end;

{ ---------------------------------------------------------------------
  Actual commands
  ---------------------------------------------------------------------}

procedure TQueryFrame.LoadQuery(AFileName: String);

begin
  FMSQL.Lines.LoadFromFile(AFileName);
end;

function TQueryFrame.AddToHistory(Qry: String): Integer;

Var
  I : Integer;

begin
  I:=FQueryHistory.IndexOf(Qry);
  If (I=-1) then
    FCurrentQuery:=FQueryHistory.Add(Qry)
  else
    begin
    FQueryHistory.Move(I,FQueryHistory.Count-1);
    FCurrentQuery:=FQueryHistory.Count-1;
    end;
  Result:=FCurrentQuery;
end;

function TQueryFrame.NextQuery: Integer;
begin
  If FCurrentQuery<FQueryHistory.Count-1 then
    begin
    Inc(FCurrentQuery);
    FMSQL.Lines.Text:=FQueryHistory[FCurrentQuery];
    end;
  Result:=FCurrentQuery;
end;

function TQueryFrame.PreviousQuery: Integer;
begin
  If (FCurrentQuery>0) then
    begin
    Dec(FCurrentQuery);
    FMSQL.Lines.Text:=FQueryHistory[FCurrentQuery];
    end;
  Result:=FCurrentQuery;
end;


procedure TQueryFrame.SaveQuery(AFileName: String);

begin
  FMSQL.Lines.SaveToFile(AFileName);
end;

procedure TQueryFrame.DoExecuteQuery(Const Qry : String; ACount : Integer = 0);

Var
  DS : TDataset;
  S,RowsAff : String;
  N : Integer;
  TS,TE : TDateTime;

begin
  RowsAff:='';
  TS:=Now;
  if ACount<>0 then
    MResult.Append(Format(SExecutingSQLStatementCount,[DateTimeToStr(TS),ACount]))
  else
    MResult.Append(Format(SExecutingSQLStatement,[DateTimeToStr(TS)]));
  MResult.Append(Qry);
  If Not assigned(FEngine) then
    Raise Exception.Create(SErrNoEngine);
  S:=ExtractDelimited(1,Trim(Qry),[' ',#9,#13,#10]);
  If (CompareText(S,'SELECT')<>0) then
    begin
    N:=FEngine.RunQuery(Qry);
    TE:=Now;
    If ecRowsAffected in FEngine.EngineCapabilities then
      RowsAff:=Format(SRowsAffected,[N]);
    TSData.TabVisible:=False;
    PCResult.ActivePage:=TSResult;
    end
  else
    begin
    DS:=Dataset;
    If Assigned(DS) then
      FEngine.SetQueryStatement(Qry,DS)
    else
      begin
      DS:=FEngine.CreateQuery(Qry,Self);
      FData.Dataset:=DS;
      end;
    TSData.TabVisible:=true;
    PCResult.ActivePage:=TSData;
    FData.Visible:=True;
    DS.Open;
    TE:=Now;
    RowsAff:=Format(SRecordsFetched,[DS.RecordCount]);
    end;
  MResult.Append(Format(SSQLExecutedOK,[DateTimeToStr(TE)]));
{$IFDEF VER2_6}
  MResult.Append(Format(SExecutionTime,[FormatDateTime('hh:nn:ss.zzz',TE-TS)]));
{$ELSE}
  MResult.Append(Format(SExecutionTime,[FormatDateTime('hh:nn:ss.zzz',TE-TS,[fdoInterval])]));
{$ENDIF}
  if (RowsAff<>'') then
    MResult.Append(RowsAff);
  AddToHistory(Qry);
  ACloseQuery.Update;
end;


function TQueryFrame.ExecuteQuery(Const Qry: String; ACount : Integer = 0): Boolean;

Var
  Msg : String;

begin
  Result:=False;
  if ACount>0 then
    FBusy:=bmScript
  else
    FBusy:=bmSingle;
  Try
    try
      DoExecuteQuery(Qry,ACount);
      Result:=True;
    except
{$IFNDEF VER2_6}
      on Ed : ESQLDatabaseError do
        begin
        Msg:=Ed.Message;
        if Ed.ErrorCode<>0 then
          Msg:=Msg+sLineBreak+Format(SSQLErrorCode,[Ed.ErrorCode]);
        if (Ed.SQLState<>'') then
          Msg:=Msg+sLineBreak+Format(SSQLStatus,[Ed.SQLState]);
        end;
{$ENDIF}
      On E : EDatabaseError do
        begin
        Msg:=E.Message;
        end;
    end;
    if (Msg<>'') then
      begin
      PCResult.ActivePage:=TSResult;
      MResult.Append(SErrorExecutingSQL);
      MResult.Append(Msg);
      end;
  Finally
    if ACount<=0 then
      FBusy:=bmIdle;
  end;
end;

procedure TQueryFrame.ExecuteScript(AScript : String);

begin
  FStatementCount:=0;
  FErrorCount:=0;
  FScriptMode:=smStopNextError;

  FBusy:=bmScript;
  try
    FScript.Script.Text:=AScript;
    FScript.Execute;
    If Fscript.Aborted then
      MResult.Append(Format(SScriptAborted,[FStatementCount]))
    else
      MResult.Append(Format(SScriptCompleted,[FStatementCount]));
    if FErrorCount>0 then
      MResult.Append(Format(SScriptErrorCount ,[FErrorCount]));
  finally
    FBusy:=bmIdle;
  end;
end;


procedure TQueryFrame.CloseDataset;
begin
  if FBusy=bmScript then
    FAbortScript:=True
  else
    begin
    fBusy:=bmSingle;
    Try
      FData.Dataset.Close;
      FData.Visible:=False;
      ACloseQuery.Update;
    Finally
      FBusy:=bmIdle;
    end;
    end;
end;

procedure TQueryFrame.FreeDataset;

Var
  D : TDataset;

begin
  D:=FData.Dataset;
  FData.Dataset:=Nil;
  D.Free;
end;



procedure TQueryFrame.ExportData;

begin
  With TFPDataExporter.Create(Dataset) do
    try
      Execute;
    finally
      Free;
    end;
end;

procedure TQueryFrame.CreateCode;
begin
  With TFPCodeGenerator.Create(Dataset) do
    try
      SQL:=FMSQL.Lines;
      DataSet:=Self.Dataset;
      Execute;
    Finally
      Free;
    end;
end;

procedure TQueryFrame.ActivatePanel;
begin
  If SQLSyn.TableNames.Count=0 then
    SetTableNames;
end;

end.

