unit CompilerTargetInfo;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  // LazUtils
  laz2_DOM, laz2_XMLRead, LazFileUtils,
  // CodeTools
  DefineTemplates;

type

  { TFPCControllerInfo - one MCU (-Wp value) and the OS targets it is valid under }
  TFPCControllerInfo = class
  public
    ControllerName: string;   // -Wp value
    ControllerUnit: string;
    OSes: TStringList;        // <ostarget shortname> children: OS targets this MCU is valid under
    constructor Create;
    destructor Destroy; override;
  end;

  { TFPCTargetOSInfo - one <ostarget> }
  TFPCTargetOSInfo = class
  public
    ShortName: string;
    LongName: string;
    UnderDevelopment: boolean; // experimental="1"
  end;

  { TFPCTargetInfoCPU - everything the per-CPU "-ix" reports for one CPU }
  TFPCTargetInfoCPU = class
  private
    FCPU: string;
    FOSes: TFPList;              // of TFPCTargetOSInfo (owned)
    FControllers: TFPList;       // of TFPCControllerInfo (owned)
    FInstructionSets: TStringList;
    FFPUSets: TStringList;
    function GetOS(Index: integer): TFPCTargetOSInfo;
    function GetController(Index: integer): TFPCControllerInfo;
  public
    constructor Create(const aCPU: string);
    destructor Destroy; override;
    function OSCount: integer;
    function ControllerCount: integer;
    function FindOS(const aTargetOS: string): TFPCTargetOSInfo;   // case-insensitive, nil if none
    function OSUsesControllers(const aTargetOS: string): boolean; // any controller valid under this OS
    function ControllerFlagProvided: boolean; // True if the compiler maps controllers to OSes
    procedure GetControllerNamesForOS(const aTargetOS: string; aList: TStrings); // sorted, OS-filtered
    property CPU: string read FCPU;
    property OSes[Index: integer]: TFPCTargetOSInfo read GetOS;
    property Controllers[Index: integer]: TFPCControllerInfo read GetController;
    property InstructionSets: TStringList read FInstructionSets;
    property FPUSets: TStringList read FFPUSets;
  end;

  { TFPCTargetInfoCache - session-level, lazy, keyed on (compiler, cpu) }
  TFPCTargetInfoCache = class
  private
    FEntries: TFPList;  // of TFPCTargetInfoEntry
    function FindEntry(const aCompiler, aCPU: string): Pointer;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    // Lazy. Returns cached per-CPU info, querying the compiler on first request.
    // Returns nil when the query failed (missing cross-compiler, no -ix, malformed) -
    // that nil is cached so it isn't retried every time.
    function GetInfo(const aCompilerFilename, aCPU: string): TFPCTargetInfoCPU;
  end;

// Session singleton (created on first use, freed on finalization).
function TargetInfoCache: TFPCTargetInfoCache;

// CPUs the configured fpc can actually build for (its native target plus the crosses configured in
// its fpc.cfg). Fast path is the driver query "fpc -ix" (<cputargets>: native + crosses in one
// call); if that yields nothing (older fpc without the query), falls back to probing each known CPU
// with "-P<cpu> -iTP". Cached per compiler for the session.
function GetConfiguredTargetCPUs(const aCompilerFilename: string; aList: TStrings): integer;

implementation

type
  TFPCTargetInfoEntry = class
  public
    Compiler: string;
    CPU: string;
    Info: TFPCTargetInfoCPU; // nil = queried and unavailable
  end;

{ helpers }

function AttrStr(aNode: TDOMNode; const aName: string): string;
var
  El: TDOMElement;
begin
  Result:='';
  if aNode is TDOMElement then
  begin
    El:=TDOMElement(aNode);
    if El.hasAttribute(aName) then
      Result:=string(El.GetAttribute(aName));
  end;
end;

procedure AddNamedChildren(aSection: TDOMNode; const aChildTag: string; aList: TStrings);
var
  Child: TDOMNode;
  s: string;
begin
  Child:=aSection.FirstChild;
  while Child<>nil do
  begin
    if (Child.NodeType=ELEMENT_NODE) and SameText(Child.NodeName,aChildTag) then
    begin
      s:=AttrStr(Child,'name');
      if (s<>'') and (aList.IndexOf(s)<0) then
        aList.Add(s);
    end;
    Child:=Child.NextSibling;
  end;
end;

function ParseIXXML(const XMLText: string; Info: TFPCTargetInfoCPU): boolean;
var
  Stream: TStringStream;
  Doc: TXMLDocument;
  InfoNode, Section, Item, Child: TDOMNode;
  OSInfo: TFPCTargetOSInfo;
  Ctrl: TFPCControllerInfo;
  s: string;
begin
  Result:=false;
  if XMLText='' then exit;
  Doc:=nil;
  Stream:=TStringStream.Create(XMLText);
  try
    try
      ReadXMLFile(Doc,Stream);
    except
      exit; // malformed XML -> treat as unavailable
    end;
    if (Doc=nil) or (Doc.DocumentElement=nil) then exit;
    InfoNode:=Doc.DocumentElement.FindNode('info');
    if InfoNode=nil then exit;

    Section:=InfoNode.FirstChild;
    while Section<>nil do
    begin
      if Section.NodeType=ELEMENT_NODE then
        case LowerCase(Section.NodeName) of
        'cpuinstructionsets':
          AddNamedChildren(Section,'cpuinstructionset',Info.InstructionSets);
        'fpuinstructionsets':
          // note: the fpu section reuses the <cpuinstructionset> element name
          AddNamedChildren(Section,'cpuinstructionset',Info.FPUSets);
        'ostargets':
          begin
            Item:=Section.FirstChild;
            while Item<>nil do
            begin
              if (Item.NodeType=ELEMENT_NODE) and SameText(Item.NodeName,'ostarget') then
              begin
                OSInfo:=TFPCTargetOSInfo.Create;
                OSInfo.ShortName:=AttrStr(Item,'shortname');
                OSInfo.LongName:=AttrStr(Item,'name');
                OSInfo.UnderDevelopment:=AttrStr(Item,'experimental')='1';
                Info.FOSes.Add(OSInfo);
              end;
              Item:=Item.NextSibling;
            end;
          end;
        'controllertypes':
          begin
            Item:=Section.FirstChild;
            while Item<>nil do
            begin
              if (Item.NodeType=ELEMENT_NODE) and SameText(Item.NodeName,'controllertype') then
              begin
                Ctrl:=TFPCControllerInfo.Create;
                Ctrl.ControllerName:=AttrStr(Item,'name');
                Ctrl.ControllerUnit:=AttrStr(Item,'controllerunit');
                // nested <ostarget shortname=> children: the OS targets this MCU is valid under
                Child:=Item.FirstChild;
                while Child<>nil do
                begin
                  if (Child.NodeType=ELEMENT_NODE) and SameText(Child.NodeName,'ostarget') then
                  begin
                    s:=AttrStr(Child,'shortname');
                    if (s<>'') and (Ctrl.OSes.IndexOf(s)<0) then
                      Ctrl.OSes.Add(s);
                  end;
                  Child:=Child.NextSibling;
                end;
                if Ctrl.ControllerName<>'' then
                  Info.FControllers.Add(Ctrl)
                else
                  Ctrl.Free; // skip sentinel entries with empty name
              end;
              Item:=Item.NextSibling;
            end;
          end;
        end;
      Section:=Section.NextSibling;
    end;
    Result:=true;
  finally
    Stream.Free;
    Doc.Free;
  end;
end;

{ TFPCControllerInfo }

constructor TFPCControllerInfo.Create;
begin
  OSes:=TStringList.Create; // CaseSensitive stays False -> OS shortname matching is case-insensitive
end;

destructor TFPCControllerInfo.Destroy;
begin
  OSes.Free;
  inherited Destroy;
end;

{ TFPCTargetInfoCPU }

constructor TFPCTargetInfoCPU.Create(const aCPU: string);
begin
  FCPU:=aCPU;
  FOSes:=TFPList.Create;
  FControllers:=TFPList.Create;
  FInstructionSets:=TStringList.Create;
  FFPUSets:=TStringList.Create;
end;

destructor TFPCTargetInfoCPU.Destroy;
var i: integer;
begin
  for i:=0 to FOSes.Count-1 do TObject(FOSes[i]).Free;
  for i:=0 to FControllers.Count-1 do TObject(FControllers[i]).Free;
  FOSes.Free;
  FControllers.Free;
  FInstructionSets.Free;
  FFPUSets.Free;
  inherited Destroy;
end;

function TFPCTargetInfoCPU.GetOS(Index: integer): TFPCTargetOSInfo;
begin
  Result:=TFPCTargetOSInfo(FOSes[Index]);
end;

function TFPCTargetInfoCPU.GetController(Index: integer): TFPCControllerInfo;
begin
  Result:=TFPCControllerInfo(FControllers[Index]);
end;

function TFPCTargetInfoCPU.OSCount: integer;
begin
  Result:=FOSes.Count;
end;

function TFPCTargetInfoCPU.ControllerCount: integer;
begin
  Result:=FControllers.Count;
end;

function TFPCTargetInfoCPU.FindOS(const aTargetOS: string): TFPCTargetOSInfo;
var i: integer;
begin
  Result:=nil;
  for i:=0 to FOSes.Count-1 do
    if SameText(OSes[i].ShortName,aTargetOS) or SameText(OSes[i].LongName,aTargetOS) then
      exit(OSes[i]);
end;

function TFPCTargetInfoCPU.OSUsesControllers(const aTargetOS: string): boolean;
// True when at least one controller lists this OS among its <ostarget> children.
var i: integer;
begin
  Result:=false;
  if aTargetOS='' then exit;
  for i:=0 to FControllers.Count-1 do
    if TFPCControllerInfo(FControllers[i]).OSes.IndexOf(aTargetOS)>=0 then
      exit(true);
end;

function TFPCTargetInfoCPU.ControllerFlagProvided: boolean;
// True when the compiler maps controllers to OSes (any controller carries <ostarget> children) -
// i.e. it speaks the dialect, so its per-OS answer is authoritative and the static list is not
// needed. Older compilers that emit no mapping return False, so the caller keeps its static gate.
var i: integer;
begin
  Result:=false;
  for i:=0 to FControllers.Count-1 do
    if TFPCControllerInfo(FControllers[i]).OSes.Count>0 then
      exit(true);
end;

procedure TFPCTargetInfoCPU.GetControllerNamesForOS(const aTargetOS: string; aList: TStrings);
// Controller names valid under the given OS shortname, sorted.
var i: integer; Ctrl: TFPCControllerInfo;
begin
  if aTargetOS='' then exit;
  for i:=0 to FControllers.Count-1 do
  begin
    Ctrl:=Controllers[i];
    if (Ctrl.ControllerName<>'') and (Ctrl.OSes.IndexOf(aTargetOS)>=0) then
      aList.Add(Ctrl.ControllerName);
  end;
  if aList is TStringList then TStringList(aList).Sort;
end;

{ TFPCTargetInfoCache }

constructor TFPCTargetInfoCache.Create;
begin
  FEntries:=TFPList.Create;
end;

destructor TFPCTargetInfoCache.Destroy;
begin
  Clear;
  FEntries.Free;
  inherited Destroy;
end;

procedure TFPCTargetInfoCache.Clear;
var i: integer; E: TFPCTargetInfoEntry;
begin
  for i:=0 to FEntries.Count-1 do
  begin
    E:=TFPCTargetInfoEntry(FEntries[i]);
    E.Info.Free;
    E.Free;
  end;
  FEntries.Clear;
end;

function TFPCTargetInfoCache.FindEntry(const aCompiler, aCPU: string): Pointer;
var i: integer; E: TFPCTargetInfoEntry;
begin
  Result:=nil;
  for i:=0 to FEntries.Count-1 do
  begin
    E:=TFPCTargetInfoEntry(FEntries[i]);
    if SameText(E.Compiler,aCompiler) and SameText(E.CPU,aCPU) then
      exit(E);
  end;
end;

function TFPCTargetInfoCache.GetInfo(const aCompilerFilename, aCPU: string): TFPCTargetInfoCPU;
var
  E: TFPCTargetInfoEntry;
  XMLLines: TStringList;
  NewInfo: TFPCTargetInfoCPU;
begin
  E:=TFPCTargetInfoEntry(FindEntry(aCompilerFilename,aCPU));
  if E<>nil then
    exit(E.Info); // cached (may be nil = known unavailable)

  NewInfo:=nil;
  if (aCompilerFilename<>'') and (aCPU<>'') then
  begin
    XMLLines:=TStringList.Create;
    try
      if RunFPCInfoXML(aCompilerFilename,'-P'+aCPU,XMLLines) then
      begin
        NewInfo:=TFPCTargetInfoCPU.Create(aCPU);
        if not ParseIXXML(XMLLines.Text,NewInfo) then
          FreeAndNil(NewInfo);
      end;
    finally
      XMLLines.Free;
    end;
  end;

  E:=TFPCTargetInfoEntry.Create;
  E.Compiler:=aCompilerFilename;
  E.CPU:=aCPU;
  E.Info:=NewInfo;
  FEntries.Add(E);
  Result:=NewInfo;
end;

{ singleton + availability }

var
  FTargetInfoCache: TFPCTargetInfoCache = nil;
  FAvailCPUCompiler: string = '';
  FAvailCPUList: TStringList = nil;

function TargetInfoCache: TFPCTargetInfoCache;
begin
  if FTargetInfoCache=nil then
    FTargetInfoCache:=TFPCTargetInfoCache.Create;
  Result:=FTargetInfoCache;
end;

procedure QueryConfiguredTargetCPUs(const aCompilerFilename: string; aList: TStrings);
// Fallback for compilers without the -ix driver query: ask fpc, for each known CPU, whether it can
// target it (a cross configured in fpc.cfg) via "-P<cpu> -iTP" run from the compiler's own dir, and
// keep <cpu> only if fpc reports that target processor back (otherwise it fell back to the native).
var
  CPU: string;
  Params, ToolOut: TStringList;
begin
  aList.Clear;
  if aCompilerFilename='' then exit;
  for CPU in FPCProcessorNames do
  begin
    Params:=TStringList.Create;
    try
      Params.Add('-P'+CPU);
      Params.Add('-iTP');
      ToolOut:=RunTool(aCompilerFilename,Params,ExtractFilePath(aCompilerFilename),true);
    finally
      Params.Free;
    end;
    if ToolOut<>nil then
      try
        if (ToolOut.Count>0) and SameText(Trim(ToolOut[0]),CPU) then
          aList.Add(CPU);
      finally
        ToolOut.Free;
      end;
  end;
end;

function GetCPUTargetsFromIXC(const aCompilerFilename: string; aList: TStrings): boolean;
// Driver query "fpc -ix": fills aList from <fpcoutput><cputargets><cputarget name=> - the complete
// CPU-target list (native + fpc.cfg crosses) in one call, answered by the fpc driver itself. Returns
// True when it yielded targets; False (older fpc that lacks the query, or an empty list) lets the
// caller fall back to the per-CPU -iTP probe.
var
  Params, ToolOut: TStringList;
  Stream: TStringStream;
  Doc: TXMLDocument;
  Section, Item: TDOMNode;
  CPU: string;
begin
  Result:=false;
  aList.Clear;
  if aCompilerFilename='' then exit;
  ToolOut:=nil;
  Params:=TStringList.Create;
  try
    Params.Add('-ix');
    ToolOut:=RunTool(aCompilerFilename,Params,ExtractFilePath(aCompilerFilename),true);
  finally
    Params.Free;
  end;
  if ToolOut=nil then exit;
  try
    Doc:=nil;
    Stream:=TStringStream.Create(ToolOut.Text);
    try
      try
        ReadXMLFile(Doc,Stream);
      except
        exit; // not XML (old fpc rejecting -ix) -> caller falls back
      end;
      if (Doc=nil) or (Doc.DocumentElement=nil) then exit;
      Section:=Doc.DocumentElement.FindNode('cputargets');
      if Section=nil then exit; // query not supported -> caller falls back
      Item:=Section.FirstChild;
      while Item<>nil do
      begin
        if (Item.NodeType=ELEMENT_NODE) and SameText(Item.NodeName,'cputarget') then
        begin
          CPU:=LowerCase(AttrStr(Item,'name'));
          if (CPU<>'') and (aList.IndexOf(CPU)<0) then
            aList.Add(CPU);
        end;
        Item:=Item.NextSibling;
      end;
      Result:=aList.Count>0;
    finally
      Doc.Free;
      Stream.Free;
    end;
  finally
    ToolOut.Free;
  end;
end;

function GetConfiguredTargetCPUs(const aCompilerFilename: string; aList: TStrings): integer;
// Cached wrapper. Prefer the "fpc -ix" driver query (one call: native + crosses); if it yields
// nothing, fall back to probing each known CPU with "-P<cpu> -iTP".
begin
  if FAvailCPUList=nil then
    FAvailCPUList:=TStringList.Create;
  if not SameText(FAvailCPUCompiler,aCompilerFilename) then
  begin
    if not GetCPUTargetsFromIXC(aCompilerFilename,FAvailCPUList) then
      QueryConfiguredTargetCPUs(aCompilerFilename,FAvailCPUList);
    FAvailCPUCompiler:=aCompilerFilename;
  end;
  aList.Assign(FAvailCPUList);
  Result:=aList.Count;
end;

finalization
  FreeAndNil(FTargetInfoCache);
  FreeAndNil(FAvailCPUList);
end.
