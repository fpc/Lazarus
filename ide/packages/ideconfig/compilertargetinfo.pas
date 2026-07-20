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

  { TFPCControllerInfo - one MCU (-Wp value). CPU/FPU/memory are '' / 0 until FPC MR5
    adds them to the <controllertype> element; the parser fills whatever is present. }
  TFPCControllerInfo = class
  public
    ControllerName: string;   // -Wp value
    ControllerUnit: string;
    CPU: string;              // instruction set (-Cp)
    FPU: string;
    FlashBase, FlashSize, SRAMBase, SRAMSize: int64;
  end;

  { TFPCTargetOSInfo - one <ostarget> }
  TFPCTargetOSInfo = class
  public
    ShortName: string;
    LongName: string;
    UnderDevelopment: boolean; // experimental="1"
    HasControllers: boolean;   // hascontrollers="1"  (whether the MCU picker applies)
    HasControllersProvided: boolean; // the hascontrollers attribute was present at all
  end;

  { TFPCTargetInfoCPU - everything -ix reports for one CPU }
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
    function OSUsesControllers(const aTargetOS: string): boolean; // reads the controllers flag
    function ControllerFlagProvided: boolean; // True if the compiler emitted hascontrollers at all
    procedure GetControllerNames(aList: TStrings);                // sorted
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

// CPUs the configured fpc can actually target: its native target plus the cross compilers
// configured in its fpc.cfg. Fast path is a single front-line "fpc -ix" whose <crosscputargets>
// section lists the crosses (by design the section is crosses-only, so the IDE adds the native
// itself via -iTP); if that section is absent, falls back to probing each known CPU with
// "-P<cpu> -iTP". Cached per compiler for the session.
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

function AttrExists(aNode: TDOMNode; const aName: string): boolean;
begin
  Result:=(aNode is TDOMElement) and TDOMElement(aNode).hasAttribute(aName);
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
  InfoNode, Section, Item: TDOMNode;
  OSInfo: TFPCTargetOSInfo;
  Ctrl: TFPCControllerInfo;
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
                OSInfo.HasControllers:=AttrStr(Item,'hascontrollers')='1';
                OSInfo.HasControllersProvided:=AttrExists(Item,'hascontrollers');
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
                Ctrl.CPU:=AttrStr(Item,'cpu');   // present once FPC MR5 lands
                Ctrl.FPU:=AttrStr(Item,'fpu');
                Ctrl.FlashBase:=StrToInt64Def(AttrStr(Item,'flashbase'),0); // "$..." parses natively
                Ctrl.FlashSize:=StrToInt64Def(AttrStr(Item,'flashsize'),0);
                Ctrl.SRAMBase :=StrToInt64Def(AttrStr(Item,'srambase'),0);
                Ctrl.SRAMSize :=StrToInt64Def(AttrStr(Item,'sramsize'),0);
                Info.FControllers.Add(Ctrl);
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
var O: TFPCTargetOSInfo;
begin
  O:=FindOS(aTargetOS);
  Result:=(O<>nil) and O.HasControllers;
end;

function TFPCTargetInfoCPU.ControllerFlagProvided: boolean;
// True when the compiler emitted the hascontrollers attribute on any <ostarget> - i.e. it
// speaks the flag dialect, so its per-OS answer is authoritative and the static list is not needed.
var i: integer;
begin
  Result:=false;
  for i:=0 to FOSes.Count-1 do
    if TFPCTargetOSInfo(FOSes[i]).HasControllersProvided then
      exit(true);
end;

procedure TFPCTargetInfoCPU.GetControllerNames(aList: TStrings);
var i: integer;
begin
  for i:=0 to FControllers.Count-1 do
    aList.Add(Controllers[i].ControllerName);
  TStringList(aList).Sort;
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
// Ask the configured front-line fpc, for each known CPU, whether it can target it - i.e.
// whether a cross compiler for it is configured in fpc.cfg. No assumptions about ppc names
// or paths: run "<fpc> -P<cpu> -iTP" from the compiler's own directory and keep <cpu> only
// if fpc reports that target processor back (otherwise it fell back to the native one).
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

function GetNativeTargetCPU(const aCompilerFilename: string): string;
// The compiler's own native CPU, asked via "-iTP" from its directory. Lowercased to match the
// cross names. Used to seed the CPU list, since <crosscputargets> lists only the fpc.cfg crosses.
var
  Params, ToolOut: TStringList;
begin
  Result:='';
  if aCompilerFilename='' then exit;
  Params:=TStringList.Create;
  try
    Params.Add('-iTP');
    ToolOut:=RunTool(aCompilerFilename,Params,ExtractFilePath(aCompilerFilename),true);
  finally
    Params.Free;
  end;
  if ToolOut<>nil then
    try
      if ToolOut.Count>0 then
        Result:=LowerCase(Trim(ToolOut[0]));
    finally
      ToolOut.Free;
    end;
end;

function GetCrossCpuTargetsFromIX(const aCompilerFilename: string; aList: TStrings): boolean;
// Runs front-line "fpc -ix" once. If it carries a <crosscputargets> section (by design this lists
// only the fpc.cfg cross CPUs, via the "name" attribute), the IDE owns the merge: aList is seeded
// with the native target (from -iTP) and the crosses are appended. Returns True when the section
// is present, so the caller uses this fast path; returns False only when the section is absent
// (older fpc), letting the caller fall back to per-CPU -iTP probing.
var
  XMLLines: TStringList;
  Stream: TStringStream;
  Doc: TXMLDocument;
  InfoNode, Section, Item: TDOMNode;
  CPU, Native: string;
begin
  Result:=false;
  aList.Clear;
  if aCompilerFilename='' then exit;
  XMLLines:=TStringList.Create;
  try
    if not RunFPCInfoXML(aCompilerFilename,'',XMLLines) then exit;
    Doc:=nil;
    Stream:=TStringStream.Create(XMLLines.Text);
    try
      try
        ReadXMLFile(Doc,Stream);
      except
        exit;
      end;
      if (Doc=nil) or (Doc.DocumentElement=nil) then exit;
      InfoNode:=Doc.DocumentElement.FindNode('info');
      if InfoNode=nil then exit;
      Section:=InfoNode.FindNode('crosscputargets');
      if Section=nil then exit; // older fpc without the section -> caller falls back
      // New-format fpc. Seed with the native target, then append the fpc.cfg crosses.
      Native:=GetNativeTargetCPU(aCompilerFilename);
      if (Native<>'') and (aList.IndexOf(Native)<0) then
        aList.Add(Native);
      Item:=Section.FirstChild;
      while Item<>nil do
      begin
        if (Item.NodeType=ELEMENT_NODE) and SameText(Item.NodeName,'crosscputarget') then
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
    XMLLines.Free;
  end;
end;

function GetConfiguredTargetCPUs(const aCompilerFilename: string; aList: TStrings): integer;
// Cached wrapper. Prefer a single front-line "fpc -ix" when it advertises a <crosscputargets>
// section; otherwise fall back to probing each CPU with "-P<cpu> -iTP".
begin
  if FAvailCPUList=nil then
    FAvailCPUList:=TStringList.Create;
  if not SameText(FAvailCPUCompiler,aCompilerFilename) then
  begin
    if not GetCrossCpuTargetsFromIX(aCompilerFilename,FAvailCPUList) then
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
