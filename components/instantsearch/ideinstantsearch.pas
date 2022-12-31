unit ideinstantsearch;

{$mode ObjFPC}{$H+}
{$modeswitch typehelpers}

interface

uses
  Classes, SysUtils, idemcindexer, lazideintf, idemsgintf, LazConfigStorage, BaseIDEIntf, projectintf,
  ExtCtrls;

Const
  RTLTree = 'RTL';
  FCLTree = 'FCL';
  CompilerTree = 'Compiler';
  LCLTree = 'LCL';
  LazTree = 'Lazarus';

  // Disconnect after timeout. in seconds
  DefaultIdleDisconnectTimeOut = 30;
  DefaultMaxStartSearchLength = 255;
  DefaultMaxResultCount = 100;
  // Number of seconds to wait after project was opened before start indexing in case of pmiOnOpen
  DefaultOpenDelay = 5;
  // Number of seconds to wait after project was opened before marking it for indexing.
  DefaultTimedDelay = 5 * 60;

  // Source tree ID in project custom data
  SInstantSearchID = 'InstantSearchID';
  // Source tree ID in project custom data
  SInstantSearchProjFileName = 'InstantSearchProjFileName';

Type
  TIndexProjectMoment = (
    ipmOnOpen,    // after open project, index project
    ipmManual,    // User manually starts indexing operation
    ipmFirstsave  // Indexing starts when first save happens.
  );

  TIndexProjectStrategy = (
    ipsAll,   // All projects are marked for indexing.
    ipsTimed, // A project is marked for indexing if it is open after TimedDelay
    ipsManual // a project is marked for indexing by user
  );

  { TIndexProjectMomentHelper }

  TIndexProjectMomentHelper = Type helper for TIndexProjectMoment
    Function ToString : string;
  end;

  { TIndexProjectStrategyHelper }

  TIndexProjectStrategyHelper = Type helper for TIndexProjectStrategy
    Function ToString : string;
  end;

  { TSourceTreeDefinition }

  TSourceTreeDefinition = Class(TCollectionItem)
  private
    FAllFiles: Boolean;
    FBaseDir: String;
    FEnabled: Boolean;
    FExtensions: String;
    FName: String;
    FRecurse: Boolean;
    FSystem: Boolean;
  Public
    procedure Assign(aSource : TPersistent); override;
  Published
    Property System : Boolean Read FSystem Write FSystem;
    Property Name : String Read FName Write FName;
    Property BaseDir : String Read FBaseDir Write FBaseDir;
    Property Recurse : Boolean Read FRecurse Write FRecurse;
    Property AllFiles : Boolean Read FAllFiles Write FAllFiles;
    Property Extensions : String Read FExtensions Write FExtensions;
    Property Enabled : Boolean Read FEnabled Write FEnabled;
  end;

  { TSourceTreeDefinitionList }

  TSourceTreeDefinitionList = class(TOwnedCollection)
  private
    function GetDef(aIndex : Integer): TSourceTreeDefinition;
    procedure SetDef(aIndex : Integer; AValue: TSourceTreeDefinition);
  Protected
    procedure Update(Item: TCollectionItem); override;
  Public
    Function AddTree(const aName : String): TSourceTreeDefinition;
    Function IndexOfName(const aName : String) : Integer;
    Function FindByName(const aName : String): TSourceTreeDefinition;
    Property Definitions[aIndex : Integer] : TSourceTreeDefinition Read GetDef Write SetDef; default;
  end;

  { TIDEInstantSearchManager }

  TIDEInstantSearchManager = class(TComponent)
  Private
    FConfigured: Boolean;
    FIdleDisconnectTimeOut: Integer;
    FIndexer: TManticoreSearchSources;
    FIndexProjectDelay: Cardinal;
    FMaxStartSearchLength: Integer;
    FMinSearchLength: Integer;
    FIndexProjectMoment: TIndexProjectMoment;
    FIndexProjectStrategy: TIndexProjectStrategy;
    FOnIndexStart: TNotifyEvent;
    FOnSourceTreesChanged: TNotifyEvent;
    FProjectTreeName: string;
    FSearchProject: boolean;
    FServerTrees: TStringList;
    FSourceTrees: TSourceTreeDefinitionList;
    FIndexThread : TThread;
    FOnIndexDone : TNotifyEvent;
    FMarkProject : TLazProject;
    FMarkProjectTimer : TTimer;
    FStartIndexTimer : TTimer;
    class var _instance : TIDEInstantSearchManager;
    procedure DoOnIndexDone(Sender: TObject);
    procedure DoManticoreLog(Sender: TObject; aKind: TMCLogKind; const aMessage: String);
    procedure DoRefreshTimer(Sender: TObject);
    procedure GetProjectFiles(aProject: TLazProject; aList: TStrings);
    procedure SetMinSearchLength(AValue: Integer);
    procedure SetRefreshTimer;
    procedure SetServerTrees(AValue: TStringList);
    procedure SetSourceTrees(AValue: TSourceTreeDefinitionList);
    procedure DoMarkProjectIndexed(Sender: TObject);
  Protected
    procedure CallIndexDone;
    procedure DoStartIndexing;
    Procedure SourceTreesChanged; virtual;
  Public
    Class constructor Init;
    Class Destructor Done;
    Class Property Instance : TIDEInstantSearchManager Read _Instance;
    Constructor Create(aOwner : TComponent); override;
    Destructor Destroy; override;
    // Load/Save config
    function GetDefaultConfigFileName: String;
    Procedure LoadConfig(const aFileName : String);
    Procedure SaveConfig(const aFileName : string);
    Procedure Load;
    Procedure Save;
    // Start timer to mark project as indexable
    procedure StartMarkProjectTimer(aProject: TLazProject);
    // Refresh system trees
    procedure RefreshSystemTrees(Force : Boolean);
    // Assign a tree ID to a project
    function AssignProjectTreeID(aProject: TLazProject): String;
    // Index files of project
    Procedure IndexProjectFiles(aProject : TLazProject);
    // Rescan FPC directory trees
    procedure RescanFPCDir;
    // Clear mark project timer
    procedure ClearMarkProjectTimer;
    // Mark a project indexable
    procedure MarkProjectIndexed(aProject: TLazProject);
    // Do search, taking into account active trees.
    function Search(const aTerm: String): TMCSearchResultArray;
    // Create default trees. Only call in or after register, it needs IDE macros.
    procedure CreateDefaultTrees;
    // Are all properties to connect set ?
    function CanConnect : Boolean;
    // Test connection. If aIndexList is assigned, fill list with names of existing trees.
    function TestConnect(aIndexList : TStrings) : Boolean;
    // Returns empty string if we can search. Returns what needs to be configured.
    Function CanSearch : string;
    // Start indexing operation for tree.
    function IndexTree(aTree : TSourceTreeDefinition) : Boolean;
    // Start indexing operation on a list of trees.
    function IndexTrees(aList: TSourceTreeDefinitionList; aFreeList : Boolean): Boolean;
    // Active project tree name
    Property ProjectTreeName : string Read FProjectTreeName Write FProjectTreeName;
    // Search project or not. If yes, projecttreeName Will be added to the list of tree to search in.
    Property SearchProject : boolean Read FSearchProject Write FSearchProject;
    // When to index a project when it is marked for indexing
    Property IndexProjectMoment : TIndexProjectMoment Read FIndexProjectMoment Write FIndexProjectMoment default ipmManual;
    // When to mark a project for indexing
    Property IndexProjectStrategy : TIndexProjectStrategy Read FIndexProjectStrategy Write FIndexProjectStrategy default ipsManual;
    // when IndexProjectStrategy is ipsTimed, Time (minutes) to wait when a project is opened
    Property IndexProjectDelay : Cardinal Read FIndexProjectDelay Write FIndexProjectDelay default DefaultTimedDelay;
    // Check whether one of the source tree indexes must be refreshed, and start an index operation
    procedure CheckRefresh(aExisting: TStrings; aForceRefresh : Boolean);
    // Is an index job currently running ?
    Function IsIndexing : Boolean;
    // Global indexer instance, use only for searching.
    Property Indexer : TManticoreSearchSources Read FIndexer;
    // Minimum number of characters needed before a search can be started.
    Property MinSearchLength : Integer Read FMinSearchLength Write SetMinSearchLength default DefaultMCMinInfixLen;
    // When checking clipboard content, maximum length for text to be accepted as search text
    Property MaxStartSearchLength : Integer Read FMaxStartSearchLength Write FMaxStartSearchLength default DefaultMaxStartSearchLength;
    // Our server trees.
    Property SourceTrees : TSourceTreeDefinitionList Read FSourceTrees Write SetSourceTrees;
    // Source trees on server
    Property ServerTrees : TStringList Read FServerTrees Write SetServerTrees;
    // Get notification when index operation is done.
    Property OnIndexStart : TNotifyEvent Read FOnIndexStart Write FOnIndexStart;
    // Get notification when index operation is done.
    Property OnIndexDone : TNotifyEvent Read FOnIndexDone Write FOnIndexDone ;
    // Get notification when source trees change
    Property OnSourceTreesChanged : TNotifyEvent Read FOnSourceTreesChanged Write FOnSourceTreesChanged;
    // Configured.
    Property Configured : Boolean Read FConfigured Write FConfigured;
    // Timeout before disconnecting. In seconds
    Property IdleDisconnectTimeOut : Integer Read FIdleDisconnectTimeOut Write FIdleDisconnectTimeOut;
  end;

Function IDEInstantSearchManager : TIDEInstantSearchManager;

implementation

uses TypInfo, Strutils, IDEExternToolIntf, MacroIntf, IDEOptionsIntf, instantsearchstrings;

function IDEInstantSearchManager: TIDEInstantSearchManager;
begin
  Result:=TIDEInstantSearchManager.Instance;
end;

Type

  { TInstantSearchIndexThread }

  TInstantSearchIndexThread = class(TThread)
  Private
    FIndexer : TManticoreSearchSources;
  Protected
    procedure DoLog(aKind: TMCLogKind; const aMessage: string); overload;
    procedure DoLog(aKind: TMCLogKind; const aFmt: String;  const aArgs: array of const); overload;
    Property Indexer : TManticoreSearchSources Read FIndexer;
  Public
    Constructor Create(aIndexer : TManticoreSearchSources; aOnTerminate : TNotifyEvent);
  end;

  TInstantSearchIndexTreeThread = class(TInstantSearchIndexThread)
  private
    FTrees : TSourceTreeDefinitionList;
    procedure DoCheckTerminate(Sender: TObject; const aFileName: String;  var aContinue: Boolean);
    function IndexTree(aTree: TSourceTreeDefinition): Integer;
  Public
    Constructor Create(aIndexer : TManticoreSearchSources; aTrees : TSourceTreeDefinitionList; aOnTerminate : TNotifyEvent);
    Destructor Destroy; override;
    Procedure Execute; override;
  end;

  { TIndexFilesData }

  TIndexFilesData = class
  private
    FName : String;
    FBaseDir: String;
    Flist: TStrings;
    FTree: string;
  Public
    Constructor Create(aName,aTree,aBaseDir : String; aList : TStrings);
    Destructor destroy; override;
    Property Name : string Read FName;
    Property Tree : string Read FTree;
    Property BaseDir : String Read FBaseDir;
    Property List : TStrings Read Flist;
  end;

  { TInstantSearchIndexFilesThread }

  TInstantSearchIndexFilesThread = class(TInstantSearchIndexThread)
  Private
    FData : TIndexFilesData;
  Protected
    Property Data : TIndexFilesData Read FData;
  Public
    Constructor Create(aIndexer : TManticoreSearchSources; const aData : TIndexFilesdata; aOnTerminate : TNotifyEvent);
    Destructor Destroy; override;
    Procedure Execute; override;
  end;

{ TIndexFilesData }

constructor TIndexFilesData.Create(aName, aTree, aBaseDir: String;
  aList: TStrings);
begin
  FName:=aName;
  FTree:=aTree;
  FBaseDir:=IncludeTrailingPathDelimiter(aBaseDir);
  FList:=aList;
end;

destructor TIndexFilesData.destroy;
begin
  FreeAndNil(Flist);
  inherited destroy;
end;

{ TInstantSearchIndexFilesThread }

constructor TInstantSearchIndexFilesThread.Create(
  aIndexer: TManticoreSearchSources; const aData: TIndexFilesdata;
  aOnTerminate: TNotifyEvent);

begin
  FData:=aData;
  Inherited Create(aIndexer,aOnTerminate);
end;

destructor TInstantSearchIndexFilesThread.Destroy;
begin
  FreeAndNil(FData);
  inherited Destroy;
end;

procedure TInstantSearchIndexFilesThread.Execute;
var
  aCount,I : Integer;
  FN,RelFN,Msg : string;

begin
  aCount:=0;
  try
    if not Indexer.Connected then
      Indexer.Connect;
    DoLog(mlkProgress,lrsStartIndexingProject,[Data.Name,Data.BaseDir]);
    FIndexer.DeleteTree(Data.Tree);
    For I:=0 to Data.List.Count-1 do
      begin
      inc(aCount);
      FN:=Data.list[i];
      if Data.BaseDir='' then
        RelFN:=FN
      else
        RelFN:=ExtractRelativePath(Data.BaseDir,FN);
      Indexer.IndexSourceFile(Data.Tree,RelFN,FN);
      if Terminated then
        break;
      end;
    if Terminated then
      Msg:=lrsIndexingProjectTerminated
    else
      Msg:=lrsFinishedIndexingProject;
    DoLog(mlkProgress,Msg,[Data.Name,aCount])
  except
    On E : exception do
      DoLog(mlkError,'Exception %s while indexing project %s : %s',[E.ClassName,Data.Name,E.Message]);
  end;
end;

{ TInstantSearchIndexThread }

constructor TInstantSearchIndexThread.Create(aIndexer: TManticoreSearchSources; aOnTerminate: TNotifyEvent);
begin
  FIndexer:=aIndexer;
  OnTerminate:=aOnTerminate;
  Inherited Create(False);
end;

destructor TInstantSearchIndexTreeThread.Destroy;
begin
  FreeAndNil(FTrees);
  inherited Destroy;
end;

Procedure TInstantSearchIndexThread.DoLog(aKind : TMCLogKind; Const aMessage : string);

begin
  if Assigned(FIndexer) and Assigned(FIndexer.Onlog) then
    FIndexer.Onlog(Self,aKind,aMessage);
end;

Procedure TInstantSearchIndexThread.DoLog(aKind : TMCLogKind; Const aFmt : String; Const aArgs : Array of const);

begin
  DoLog(aKind,Format(aFmt,aArgs));
end;

{ TInstantSearchIndexTreeThread }

Constructor TInstantSearchIndexTreeThread.Create(aIndexer : TManticoreSearchSources; aTrees : TSourceTreeDefinitionList; aOnTerminate : TNotifyEvent);

begin
  FTrees:=aTrees;
  Inherited Create(aIndexer,aOnTerminate);
end;

Function TInstantSearchIndexTreeThread.IndexTree(aTree : TSourceTreeDefinition) : Integer;

Var
  Options : TMCIndexOptions;

begin
  Findexer.Extensions:=aTree.Extensions.Split([';'],TStringSplitOptions.ExcludeEmpty);
  if not FIndexer.Connected then
    FIndexer.Connect;
  Options:=[ioStoreRelativeNames];
  if aTree.AllFiles then
    Include(Options,ioAllFiles);
  if aTree.Recurse then
    Include(Options,ioRecurse);
  FIndexer.DeleteTree(aTree.Name);
  Result:=FIndexer.IndexSources(aTree.Name,aTree.BaseDir,Options,@DoCheckTerminate);
end;

procedure TInstantSearchIndexTreeThread.DoCheckTerminate(Sender: TObject;
  const aFileName: String; var aContinue: Boolean);
begin
  if Terminated then
    aContinue:=False;
end;


procedure TInstantSearchIndexTreeThread.Execute;

var
  I,aCount : Integer;
  aTree : TSourceTreeDefinition;
  aName : string;
begin
  For I:=0 to FTrees.Count-1 do
    try
      aTree:=FTrees[i];
      aName:=aTree.Name;
      DoLog(mlkProgress,lrsStartIndexingTree,[aName,aTree.BaseDir]);
      aCount:=IndexTree(aTree);
      DoLog(mlkProgress,lrsFinishedIndexingTree,[aName,aCount]);
      if Terminated then
        Break;
    except
      On E : exception do
        DoLog(mlkError,'Exception %s while indexing tree %s : %s',[E.ClassName,aName,E.Message]);
    end;
end;


{ TIndexProjectStrategyHelper }

function TIndexProjectStrategyHelper.ToString: string;
begin
  Case self of
    ipsAll : Result:=lrsAllProjects;
    ipsTimed  : Result:=lrsTimed;
    ipsManual : Result:=lrsManual;
  else
    Result:='?';
  end;
end;

{ TIndexProjectMomentHelper }

function TIndexProjectMomentHelper.ToString: string;
begin
  Case self of
    ipmOnOpen : Result:=lrsOnOpen;
    ipmManual : Result:=lrsManual;
    ipmFirstsave : Result:=lrsOnFirstSave;
  else
    Result:='?';
  end;
end;

{ TIDEInstantSearchManager }

procedure TIDEInstantSearchManager.SetMinSearchLength(AValue: Integer);
begin
  if FMinSearchLength=AValue then Exit;
  if aValue<2 then
    Raise EManticoreSearch.Create(lrsErrorMinLengthIs2);
  FMinSearchLength:=AValue;
  FIndexer.MinInfixLen:=AValue;
end;

procedure TIDEInstantSearchManager.CallIndexDone;

begin
  // Must be called in main thread
  If Assigned(FonIndexDone) then
    FonIndexDone(Self);
  DoManticoreLog(self,mlkProgress,lrsIndexingOperationFinished);
end;

procedure TIDEInstantSearchManager.DoMarkProjectIndexed(Sender: TObject);


begin
  if assigned(FMarkProject) and (FMarkProject=LazarusIDE.ActiveProject) then
    begin
    MarkProjectIndexed(FMarkProject);
    end;
end;

procedure TIDEInstantSearchManager.SourceTreesChanged;
begin
  if assigned(FOnSourceTreesChanged) then
    FOnSourceTreesChanged(Self);
end;

procedure TIDEInstantSearchManager.DoOnIndexDone(Sender: TObject);
begin
  if Sender=FIndexThread then
    FIndexThread:=Nil;
  if assigned(FonIndexDone) then
    TThread.Synchronize(TThread.CurrentThread,@CallIndexDone);
end;

procedure TIDEInstantSearchManager.DoManticoreLog(Sender: TObject;
  aKind: TMCLogKind; const aMessage: String);

Var
  mlu : TMessageLineUrgency;

begin
  // Writeln(aKind,': ',aMessage);
  if (aKind in [mlkError,mlkProgress]) and Assigned(IDEMessagesWindow) then
     begin
     if aKind=mlkError then
       mlu:=mluError
     else
       mlu:=mluProgress;
     IDEMessagesWindow.AddCustomMessage(mlu,lrsInstantSearch+': '+aMessage,'',0,0,lrsInstantSearch);
     end;
end;

procedure TIDEInstantSearchManager.SetServerTrees(AValue: TStringList);
begin
  if FServerTrees=AValue then Exit;
  FServerTrees.Assign(AValue);
end;

procedure TIDEInstantSearchManager.SetSourceTrees(
  AValue: TSourceTreeDefinitionList);
begin
  if FSourceTrees=AValue then Exit;
  FSourceTrees.Assign(AValue);
end;

class constructor TIDEInstantSearchManager.Init;
begin
  _Instance:=TIDEInstantSearchManager.Create(Nil);
end;

class destructor TIDEInstantSearchManager.Done;
begin
  FreeAndNil(_Instance);
end;

constructor TIDEInstantSearchManager.Create(aOwner: TComponent);


begin
  Inherited;
  IdleDisconnectTimeOut:=DefaultIdleDisconnectTimeOut;
  FMinSearchLength:=DefaultMCMinInfixLen;
  FIndexer:=TManticoreSearchSources.Create(Nil);
  FIndexer.OnLog:=@DoManticoreLog;
  FIndexer.Extensions:=['pp','pas','lpr','inc'];
  FIndexer.Limit:=DefaultMaxResultCount;
  FServerTrees:=TStringList.Create;
  FSourceTrees:=TSourceTreeDefinitionList.Create(Self,TSourceTreeDefinition);
  FMaxStartSearchLength:=DefaultMaxStartSearchLength;
  FIndexProjectMoment:=ipmManual;
  FIndexProjectStrategy:=ipsManual;
  FIndexProjectDelay:=DefaultTimedDelay;
end;

procedure TIDEInstantSearchManager.CreateDefaultTrees;

  function GetCurrentFPCSourceDirectory: string;
  begin
    Result:='$(FPCSrcDir)';
    if not IDEMacros.SubstituteMacros(Result) then
      raise Exception.Create('unable to retrieve FPCSrcDir');
  end;

Var
  FPCDir : String;

begin
  FPCDir:=IncludeTrailingPathDelimiter(GetCurrentFPCSourceDirectory);
  With FSourceTrees.AddTree(RTLTree) do
    begin
    BaseDir:=FPCDir+'rtl';
    Recurse:=True;
    System:=true;
    Extensions:='pp;pas;inc';
    end;
  With FSourceTrees.AddTree(FCLTree) do
    begin
    BaseDir:=FPCDir+'packages';
    Recurse:=True;
    System:=true;
    Extensions:='pp;pas;inc';
    end;
  With FSourceTrees.AddTree(CompilerTree) do
    begin
    BaseDir:=FPCDir+'compiler';
    Recurse:=True;
    System:=true;
    Extensions:='pp;pas;inc';
    end;
  With FSourceTrees.AddTree(LCLTree) do
    begin
    BaseDir:=IncludeTrailingPathDelimiter(IDEEnvironmentOptions.GetParsedLazarusDirectory)+'lcl'+PathDelim;
    Recurse:=True;
    System:=true;
    Extensions:='pp;pas;inc';
    end;
end;



procedure TIDEInstantSearchManager.LoadConfig(const aFileName: String);

var
  Cfg: TConfigStorage;
  List,S,aEnableds : String;

  aTree : TSourceTreeDefinition;
  I : integer;

begin
  List:='';
  Cfg:=GetIDEConfigStorage(aFilename,True);
  try
    Configured:=Cfg.GetValue('Search/Configured',Configured);
    MaxStartSearchLength:=Cfg.GetValue('Search/MaxStartSearchLength',MaxStartSearchLength);
    Findexer.Limit:=Cfg.GetValue('Indexer/Limit',Findexer.Limit);
    MinSearchLength:=Cfg.GetValue('Indexer/MinSearchLength',MinSearchLength);
    Findexer.Extensions:=Cfg.GetValue('Indexer/Extensions','').Split(';',TStringSplitOptions.ExcludeEmpty);
    Findexer.HostName:=Cfg.GetValue('Indexer/Hostname',Findexer.HostName);
    Findexer.Port:=Cfg.GetValue('Indexer/Port',Findexer.Port);
    IdleDisconnectTimeOut:=Cfg.GetValue('Indexer/IdleDisconnect',IdleDisconnectTimeOut);
    aEnableds:=Cfg.GetValue('Indexer/EnabledSystemTrees','');
    S:=GetEnumName(TypeInfo(TMCTransport),Ord(Findexer.Transport));
    S:=Cfg.GetValue('Indexer/Transport',S);
    I:=GetEnumValue(TypeInfo(TMCTransport),S);
    if I<>-1 then
      Findexer.Transport:=TMCTransport(I);
    S:=GetEnumName(TypeInfo(TMCMySQLClientVersion),Ord(Findexer.MySQLVersion));
    S:=Cfg.GetValue('Indexer/MySQLVersion',S);
    I:=GetEnumValue(TypeInfo(TMCMySQLClientVersion),S);
    if I<>-1 then
      Findexer.MySQLVersion:=TMCMySQLClientVersion(I);
    Cfg.GetValue('Trees/Names',List);
    For I:=SourceTrees.Count-1 downto 0 do
      if Not SourceTrees[i].System then
         SourceTrees.Delete(I);
    For I:=SourceTrees.Count-1 downto 0 do
      With SourceTrees[i] do
        Enabled:=Pos(';'+Name+';',aEnableds)>0;

    for S in SplitString(List,';') do
      if S<>'' then
        begin
        ATree:=SourceTrees.AddTree(S);
        Cfg.ReadObject('Trees/list/'+aTree.Name+'/',aTree);
        end;
  finally
    Cfg.Free;
  end;

end;

procedure TIDEInstantSearchManager.SaveConfig(const aFileName: string);

var
  Cfg: TConfigStorage;
  S,ext : String;
  I : Integer;
  aTree : TSourceTreeDefinition;

begin
  S:='';
  Cfg:=GetIDEConfigStorage(aFilename,True);
  try
    Cfg.SetValue('Search/Configured',Configured);
    Cfg.SetValue('Search/MaxStartSearchLength',MaxStartSearchLength);
    Cfg.SetValue('Indexer/MinSearchLength',MinSearchLength);
    Cfg.SetValue('Indexer/Limit',Findexer.Limit);
    Cfg.SetValue('Indexer/Transport',GetEnumName(TypeInfo(TMCTransport),Ord(Findexer.Transport)));
    Cfg.SetValue('Indexer/IdleDisconnect',IdleDisconnectTimeOut);
    S:=GetEnumName(TypeInfo(TMCMySQLClientVersion),Ord(Findexer.MySQLVersion));
    Cfg.SetValue('Indexer/MySQLVersion',S);
    S:='';
    For ext in Findexer.Extensions do
      begin
      if S<>'' then
        S:=S+';';
      S:=S+Ext;
      end;
    Cfg.SetValue('Indexer/Extensions',S);
    Cfg.SetValue('Indexer/Hostname',Findexer.HostName);
    Cfg.SetValue('Indexer/Port',Findexer.Port);
    S:='';
    For I:=0 to SourceTrees.Count-1 do
      With SourceTrees[i] do
        if System and Enabled then
          begin
          S:=S+';'+SourceTrees[i].Name;
          end;
    if (S<>'') then
      S:=S+';';
    Cfg.SetValue('Indexer/EnabledSystemTrees',S);
    S:='';
    For I:=0 to SourceTrees.Count-1 do
      if not SourceTrees[i].System then
        begin
        if S<>'' then
          S:=S+';';
        S:=S+SourceTrees[i].Name;
        end;
    Cfg.SetValue('Trees/Names',S);
    Cfg.DeletePath('Trees/List');
    For I:=0 to SourceTrees.Count-1 do
      begin
      aTree:=SourceTrees[i];
      if not aTree.System then
        Cfg.WriteObject('Trees/list/'+aTree.Name+'/',SourceTrees[I]);
      end;
    Cfg.WriteToDisk;
  finally
    Cfg.Free;
  end;
end;

function TIDEInstantSearchManager.GetDefaultConfigFileName : String;

begin
  Result:=IncludeTrailingPathDelimiter(LazarusIDE.GetPrimaryConfigPath)+'instantsearch.xml';
end;

procedure TIDEInstantSearchManager.CheckRefresh(aExisting : TStrings; aForceRefresh : Boolean);

Var
  i : Integer;
  Msg,aName : string;
  aTrees : TSourceTreeDefinitionList;

begin

  aTrees:=TSourceTreeDefinitionList.Create(Self,TSourceTreeDefinition);
  try
    for I:=0 to FSourceTrees.Count-1 do
      begin
      aName:=FSourceTrees[i].Name;
      if aForceRefresh or (aExisting.IndexOf(aName)=-1) then
        begin
        if not aForceRefresh then
          begin
          Msg:=Format('Source tree %s has no records (searched %d trees), refreshing',[aName,aExisting.Count]);
          IDEMessagesWindow.AddCustomMessage(mluHint,lrsInstantSearch+': '+Msg,'',0,0,lrsInstantSearch);
          end;
        aTrees.AddTree(aName).Assign(FSourceTrees[i]);
        end;
      end;
  except
    FreeAndNil(aTrees);
    raise;
  end;
  if aTrees.Count>0 then
    IndexTrees(aTrees,True)
  else
    FreeAndNil(aTrees);
end;

procedure TIDEInstantSearchManager.RefreshSystemTrees(Force : Boolean);

Var
  L : TStrings;

begin
  L:=TStringList.Create;
  try
    Indexer.ListTrees(L);
    CheckRefresh(L,Force);
  finally
    L.Free;
  end;
end;

procedure TIDEInstantSearchManager.DoRefreshTimer(Sender : TObject);

begin
  FStartIndexTimer.Enabled:=False;
  FreeAndNil(FStartIndexTimer);
  RefreshSystemTrees(False);
end;

procedure TIDEInstantSearchManager.SetRefreshTimer;

begin
  FStartIndexTimer:=TTimer.Create(Self);
  FStartIndexTimer.Enabled:=False;
  FStartIndexTimer.Interval:=DefaultOpenDelay;
  FStartIndexTimer.OnTimer:=@DoRefreshTimer;
  FStartIndexTimer.Enabled:=True;
end;

procedure TIDEInstantSearchManager.Load;

Var
  L : TStrings;

begin
  LoadConfig(GetDefaultConfigFileName);
  L:=TStringList.Create;
  try
    if TestConnect(L) then
      begin
      ServerTrees.Assign(L);
      if L.IndexOf(Indexer.IndexName)<>-1 then
         SetRefreshTimer;
      end;
  finally
    L.Free;
  end;

end;

procedure TIDEInstantSearchManager.Save;
begin
  SaveConfig(GetDefaultConfigFileName);
end;

function TIDEInstantSearchManager.AssignProjectTreeID(aProject : TLazProject) : String;

var
  CurrentProjectFileName : String;

begin
  Result:=TGUID.NewGuid.ToString(True);
  CurrentProjectFileName:=aProject.ProjectInfoFile;
  aProject.CustomSessionData[SInstantSearchID]:=Result;
  aProject.CustomSessionData[SInstantSearchProjFileName]:=CurrentProjectFileName;
  aProject.Modified:=True;
end;

procedure TIDEInstantSearchManager.GetProjectFiles(aProject: TLazProject; aList : TStrings);

Var
  FN : String;
  I : Integer;
begin
  for I:=0 to aProject.FileCount-1 do
    if aProject.Files[i].IsPartOfProject then
      begin
      FN:=aProject.Files[i].GetFullFilename;
      if Indexer.AllowExtension(ExtractFileExt(FN)) then
        aList.Add(FN);
      end
end;

procedure TIDEInstantSearchManager.IndexProjectFiles(aProject: TLazProject);

Var
  ProjName : string;
  ProjTree : String;
  ProjTreeFileName : String;
  ProjectDir,CurrentProjectFileName : String;
  lIndexer : TManticoreSearchSources;
  Data : TIndexFilesData;
  L : TStrings;

begin
  ProjTree:=aProject.CustomSessionData[SInstantSearchID];
  ProjTreeFileName:=aProject.CustomSessionData[SInstantSearchProjFileName];
  CurrentProjectFileName:=aProject.ProjectInfoFile;
  ProjectDir:=ExtractFilePath(CurrentProjectFileName);
  if (ProjTree='') or (ProjTreeFileName<>CurrentProjectFileName) then
    ProjTree:=AssignProjectTreeID(aProject);
  L:=TStringList.Create;
  try
    GetProjectFiles(aProject,L);
    ProjName:=aProject.Title;
    if ProjName='' then
      ProjName:=ChangeFileExt(ExtractFileName(CurrentProjectFileName),'');
    Data:=TIndexFilesData.Create(ProjName,projtree,ProjectDir,L);
    L:=nil;
    lIndexer:=Indexer.Clone(Self);
    FIndexThread:=TInstantSearchIndexFilesThread.Create(lIndexer,Data,@DoOnIndexDone);
  finally
    L.Free;
  end;
end;

procedure TIDEInstantSearchManager.RescanFPCDir;

Var
  L : TSourceTreeDefinitionList;
  aTree : TSourceTreeDefinition;

begin
  L:=TSourceTreeDefinitionList.Create(Self,TSourceTreeDefinition);
  aTree:=SourceTrees.FindByName(FCLTree);
  if assigned(aTree) then
    L.AddTree(aTree.Name).Assign(aTree);
  aTree:=SourceTrees.FindByName(RTLTree);
  if assigned(aTree) then
    L.AddTree(aTree.Name).Assign(aTree);
  aTree:=SourceTrees.FindByName(CompilerTree);
  if assigned(aTree) then
    L.AddTree(aTree.Name).Assign(aTree);
  IndexTrees(L,True);
end;

procedure TIDEInstantSearchManager.ClearMarkProjectTimer;
begin
  FMarkProject:=nil;
  if Assigned(FMarkProjectTimer) then
    begin
    FMarkProjectTimer.Enabled:=False;
    FreeAndNil(FMarkProjectTimer);
    end;
end;

procedure TIDEInstantSearchManager.StartMarkProjectTimer(aProject : TLazProject);

begin
  FMarkProject:=aProject;
  FMarkProjectTimer:=TTimer.Create(Self);
  FMarkProjectTimer.Enabled:=False;
  FMarkProjectTimer.Interval:=IndexProjectDelay*1000;
  FMarkProjectTimer.OnTimer:=@DoMarkProjectIndexed;
  FMarkProjectTimer.Enabled:=True;
end;



procedure TIDEInstantSearchManager.MarkProjectIndexed(aProject: TLazProject);

Var
  FN : String;

begin
  FN:=ChangeFileExt(ExtractFileName(aProject.ProjectInfoFile),'');
  IDEMessagesWindow.AddCustomMessage(mluVerbose,lrsInstantSearch+Format(lrsMarkingIndexable,[FN]),aProject.ProjectInfoFile,0,0,lrsInstantSearch);
  AssignProjectTreeID(aProject);
  IndexProjectFiles(aProject);
end;


function  TIDEInstantSearchManager.Search(const aTerm: String) : TMCSearchResultArray;

Var
  I,aCount : Integer;
  aTree : TSourceTreeDefinition;
  aTrees : Array of string;

  Procedure AddTree(const aName : String);
  begin
    aTrees[aCount]:=aName;
    inc(aCount);
  end;

begin
  aTrees:=[];
  SetLength(aTrees,Sourcetrees.Count+1);
  aCount:=0;
  if SearchProject and (ProjectTreeName<>'') then
    addTree(ProjectTreeName);
  for I:=0 to SourceTrees.Count-1 do
    begin
    aTree:=SourceTrees[i];
    if aTree.Enabled then
      addTree(aTree.Name);
    end;
   SetLength(aTrees,aCount);
   Result:=Indexer.Search('*'+aTerm+'*',aTrees);
end;

destructor TIDEInstantSearchManager.Destroy;
begin
  if Assigned(FIndexThread) then
    begin
    FIndexThread.Terminate;
    While Assigned(FIndexThread) do
      begin
      Sleep(10);
      // Can't use application.processmessages
      CheckSynchronize;
      end;
    end;
  FreeAndNil(FIndexer);
  FreeAndNil(FSourceTrees);
  FreeAndNil(FServerTrees);
  FreeAndNil(FStartIndexTimer);
  FreeAndNil(FMarkProjectTimer);
  inherited Destroy;
end;

function TIDEInstantSearchManager.CanConnect: Boolean;
begin
  Result:=(Indexer.IndexName<>'')
end;

function TIDEInstantSearchManager.TestConnect(aIndexList: TStrings): Boolean;

Var
  aList : Tstrings;

begin
  Result:=False;
  aList:=aIndexList;
  if aList=Nil then
    aList:=TstringList.Create;
  try
    try
      Indexer.Connect;
      Indexer.ListIndexes(aList);
      Result:=True;
    except
      On E : Exception do
        DoManticoreLog(Self,mlkInfo,Format(lrsCannotConnectToManticore,[E.ClassName,E.Message]));
    end;
  finally
    if aList<>aIndexList then
      FreeAndNil(aList);
  end;
end;

function TIDEInstantSearchManager.CanSearch: string;

begin
  if Indexer.Transport=mctNone then
    Exit(lrsNoTransport);
  if (Indexer.Transport=mctMysql) and (Indexer.MySQLVersion=mcvNone) then
    Exit(lrsNoMySQLVersion);
  if SourceTrees.Count=0 then
    Exit(lrsNoSearchTrees);
  If (ServerTrees.Count=0) then
    Exit(lrsNoServerTrees);
end;

procedure TIDEInstantSearchManager.DoStartIndexing;

begin
  if Assigned(FOnIndexStart) then
    FOnIndexStart(Self);
end;

function TIDEInstantSearchManager.IndexTrees(aList: TSourceTreeDefinitionList;
  aFreeList: Boolean): Boolean;

Var
  lTrees : TSourceTreeDefinitionList;
  lIndexer : TManticoreSearchSources;

begin
  Result:=False;
  if IsIndexing then
     begin
     if AFreeList then
       aList.Free;
     exit;
     end;
  lTrees:=Nil;
  try
    if aFreeList then
      lTrees:=aList
    else
      begin
      lTrees:=TSourceTreeDefinitionList.Create(Nil,aList.ItemClass);
      lTrees.Assign(aList);
      end;
    lIndexer:=Indexer.Clone(Self);
    FIndexThread:=TInstantSearchIndexTreeThread.Create(lIndexer,lTrees,@DoOnIndexDone);
    DoStartIndexing;
    Result:=True;
  except
    On E : Exception do
      begin
      FreeAndNil(lTrees);
      DoManticoreLog(Self,mlkInfo,Format(lrsCannotConnectToManticore,[E.ClassName,E.Message]));
      end;
  end;
end;

function TIDEInstantSearchManager.IndexTree(aTree: TSourceTreeDefinition
  ): Boolean;

Var
  aList : TSourceTreeDefinitionList;

begin
  aList:=TSourceTreeDefinitionList.Create(Self,TSourceTreeDefinition);
  aList.AddTree(aTree.Name).Assign(aTree);
  Result:=IndexTrees(aList,True);
end;

function TIDEInstantSearchManager.IsIndexing: Boolean;
begin
  Result:=Assigned(FIndexThread);
end;

{ TSourceTreeDefinitionList }

function TSourceTreeDefinitionList.GetDef(aIndex : Integer
  ): TSourceTreeDefinition;
begin
  Result:=TSourceTreeDefinition(Items[aIndex]);
end;

procedure TSourceTreeDefinitionList.SetDef(aIndex : Integer;
  AValue: TSourceTreeDefinition);
begin
  Items[aIndex]:=aValue;
end;

procedure TSourceTreeDefinitionList.Update(Item: TCollectionItem);
begin
  if Owner is TIDEInstantSearchManager then
    (Owner as TIDEInstantSearchManager).SourceTreesChanged;
end;

function TSourceTreeDefinitionList.AddTree(const aName: String
  ): TSourceTreeDefinition;
begin
  Result:=Add as TSourceTreeDefinition;
  Result.Name:=aName;
end;

function TSourceTreeDefinitionList.IndexOfName(const aName: String): Integer;
begin
  Result:=Count-1;
  While (Result>=0) and Not SameText(GetDef(Result).Name,aName) do
    Dec(Result);
end;

function TSourceTreeDefinitionList.FindByName(const aName: String
  ): TSourceTreeDefinition;

Var
  Idx : Integer;

begin
  Result:=nil;
  Idx:=IndexOfName(aName);
  if Idx<>-1 then
    Result:=GetDef(Idx);
end;

{ TSourceTreeDefinition }

procedure TSourceTreeDefinition.Assign(aSource: TPersistent);

Var
  aSTD :  TSourceTreeDefinition absolute aSource;

begin
  if aSource is TSourceTreeDefinition then
    begin
    FBaseDir:=aStd.BaseDir;
    FName:=aStd.Name;
    FRecurse:=aStd.Recurse;
    FExtensions:=aStd.Extensions;
    FSystem:=aStd.System;
    FAllFiles:=aStd.AllFiles;
    FEnabled:=aStd.Enabled;
    end
  else
    inherited Assign(aSource);
end;

end.

