{

    FPDoc  -  Free Pascal Documentation Tool
    Copyright (C) 2000 - 2003 by
      Areca Systems GmbH / Sebastian Guenther, sg@freepascal.org

    * Skeleton XML description file generator

    See the file COPYING, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}

(* --- Version 1.0 ---
The TFPDocMaker class shall support the following functionality:
- Project generation from a commandline.
- FPDoc documentation generation, optionally syntax check only.
- MakeSkel skeleton generation or update.

Everything else is done in a separate documentation manager.
The documentation manager maintains its own projects
and creates temporary TFPDocProjects and TFPDocPackages on demand.
*)

(* Version 0.0 - requires patched FPDoc units!
The TFPDocMaker class supports the following functionality:
- documentation generation (FPDoc),
  - for all units in a package
  - for a selected unit (optionally syntax check only)
- project generation
  - from input and description directories
  - from a commandline
- skeleton generation
  - for all units in a package
  - for selected unit (MakeSkel)
- documentation sync with source (MakeSkel UpdateMode)
  - for all units in a package
    - output into one or more files
  - for selected unit
- skeleton and sync at once
*)
unit umakeskel;

interface

{$mode objfpc}
{$h+}

uses
  SysUtils, Classes, Gettext,
  dGlobals, PasTree, PParser,PScanner,
  IniFiles,
  mkfpdoc, fpdocproj;

resourcestring
  STitle = 'MakeSkel - FPDoc skeleton XML description file generator';
  SVersion = 'Version %s [%s]';
  SCopyright = '(c) 2000 - 2003 Areca Systems GmbH / Sebastian Guenther, sg@freepascal.org';
  SCmdLineHelp = 'See documentation for usage.';
  SCmdLineInvalidOption = 'Ignoring unknown option "%s"';
  SNoPackageNameProvided = 'Please specify a package name with --package=<name>';
  SOutputMustNotBeDescr = 'Output file must be different from description filenames.';
  SCreatingNewNode = 'Creating documentation for new node : %s';
  SNodeNotReferenced = 'Documentation node "%s" no longer used';
  SDone = 'Done.';
//from fpdocxmlopts
  SErrInvalidRootNode = 'Invalid options root node: Got "%s", expected "docproject"';
  SErrNoPackagesNode = 'No "packages" node found in docproject';

type
  TCmdLineAction = (actionHelp, actionConvert);

(* Extended INI file
*)

  { TConfigFile }

  TConfigFile = class(TIniFile)
  public
    function IsDirty: boolean;
    procedure Flush;
    procedure WriteSectionValues(const Section: string; Strings: TStrings);
  end;

(* EngineOptions plus MakeSkel options.
  Used in the commandline parsers, passed to the Engine.
  Project.Options are ignored by TFDocMaker.(?)
*)

  { TCmdOptions }

  TCmdOptions = class(TEngineOptions)
  public
    WriteDeclaration,
    UpdateMode,
    SortNodes,
    DisableOverride,
    DisableErrors,
    DisableSeealso,
    DisableArguments,
    DisableProtected,
    DisablePrivate,
    DisableFunctionResults: Boolean;
    EmitClassSeparator: Boolean;
    Verbose,
    Modified: boolean;
    procedure Assign(Source: TPersistent); override;
    procedure LoadConfig(cf: TConfigFile; AProfile: string);
    procedure SaveConfig(cf: TConfigFile; AProfile: string);
    procedure BackendToPairs(Dest: TStrings);
    procedure BackendFromPairs(Source: TStrings);
  end;

  { TSkelEngine }

  TSkelEngine = class(TFPDocEngine)
  Private
    FEmittedList, 
    FNodeList,
    FModules : TStringList;
    FOptions: TCmdOptions;
    Procedure  DoWriteUnReferencedNodes(N : TDocNode; NodePath : String);
    procedure SetOptions(AValue: TCmdOptions);
  public
    Destructor Destroy; override;
    Function MustWriteElement(El : TPasElement; Full : Boolean) : Boolean;
    Function WriteElement(Var F : Text; El : TPasElement; ADocNode : TDocNode) : Boolean;
    function FindModule(const AName: String): TPasModule; override;
    function CreateElement(AClass: TPTreeElement; const AName: String;
      AParent: TPasElement; AVisibility :TPasMemberVisibility;
      const ASourceFilename: String; ASourceLinenumber: Integer): TPasElement; override;
    procedure WriteUnReferencedNodes;
    Procedure WriteNodes(Var F : Text; AModule : TPasModule; List : TStrings);
    Procedure DocumentFile(Var F : Text; Const AFileName,ATarget,ACPU : String);
    Property NodeList : TStringList Read FNodeList;
    Property EmittedList : TStringList Read FEmittedList;
    property Options: TCmdOptions read FOptions write SetOptions;
  end;

  THandleOption = function(const Cmd, Arg: string): boolean;

  TCreatorAction = (
    caDefault,
    caDryRun,
    caUsage, //explicit or on all errors?
    caInvalid,
    caWriteProject
  );

  { TFPDocMaker }
(* MakeSkel functionality as a class.
*)
  TFPDocMaker = class(TFPDocCreator)
  private
    FDescrDir: string;
    FInputDir: string;
    FOnOption: THandleOption;
    FOptions: TCmdOptions;
    function GetDescrDir: string;
    function GetInputDir: string;
    procedure SetDescrDir(AValue: string);
    procedure SetInputDir(AValue: string);
    procedure SetOnOption(AValue: THandleOption);
    procedure SetOptions(AValue: TCmdOptions);
  protected
    FCmdAction: TCreatorAction;
    FDryRun: boolean;
    FPackage: TFPDocPackage;
    FProjectFile: boolean;
    FWriteProjectFile: string;
    FTranslated: string;
    procedure SetCmdAction(AValue: TCreatorAction);
    procedure SetDryRun(AValue: boolean);
    procedure SetPackage(AValue: TFPDocPackage);
    procedure SetWriteProjectFile(AValue: string);
    function ParseCommon(var Cmd, Arg: string): TCreatorAction;
  public
    Function  DocumentPackage(Const APackageName,AOutputName: string; InputFiles, DescrFiles : TStrings) : String;
    procedure CreateUnitDocumentation(const AUnit: string; ParseOnly: Boolean);
  public
    ImportDir: string;
    SelectedUnit: string;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure AddDirToFileList(List: TStrings; const ADirName, AMask: String);
    procedure AddToFileList(List: TStrings; const FileName: String);
    function  UnitSpec(AUnit: string): string;
    function  ImportName(AIndex: integer): string;
    procedure LogToStdOut(Sender: TObject; const msg: string);
    procedure LogToStdErr(Sender: TObject; const msg: string);
  //parsing
    function  ParseFPDocOption(const S: string):  TCreatorAction;
    function  ParseUpdateOption(const S: string):  TCreatorAction;
    function  CheckSkelOptions: string;
    function  CleanXML(const FileName: string): boolean;
    function  SelectedPackage: TFPDocPackage;
    property Package: TFPDocPackage read SelectedPackage write SetPackage;
    property CmdAction: TCreatorAction read FCmdAction write SetCmdAction;
    property DryRun: boolean read FDryRun write SetDryRun;
    property ReadProject: boolean read FProjectFile;
    property WriteProjectFile: string read FWriteProjectFile write SetWriteProjectFile;
    property OnOption: THandleOption read FOnOption write SetOnOption;
    property InputDir: string read GetInputDir write SetInputDir;
    property DescrDir: string read GetDescrDir write SetDescrDir;
    property CmdOptions: TCmdOptions read FOptions write SetOptions;
  end;

//Extract next commandline option from a string
Function GetNextWord(Var s : string) : String;

//Get package name from Imports spec
function ExtractImportName(const s: string): string;
//Get Unit filename from Inputs or Descriptions
function UnitFile(AList: TStrings; AIndex: integer): string;
//Get Unit name from Inputs or Descriptions
function ExtractUnitName(AList: TStrings; AIndex: integer): string;
function ExtractUnitName(s: string): string;

implementation

uses
  dom,
  dWriter;

(* Extract (remove!) next commandline option from a string.
  Handle quoted arguments, but do not unquote.
  Option may be partially quoted, e.g. -opt="arg with blanks"
*)
Function GetNextWord(Var s : string) : String;
Const
  WhiteSpace = [' ',#9,#10,#13];
var
  i,j: integer;
  quoted: boolean;
begin
  I:=1;
  quoted := False;
  While (I<=Length(S)) and (S[i] in WhiteSpace) do
    Inc(I);
  J:=I;
{
    While (J<=Length(S)) and (not (S[J] in WhiteSpace)) do
      Inc(J);
}
  While (J<=Length(S)) do begin
    if (s[j] = '"') then begin
      if quoted then
        break;
      quoted := True;
    end else if not quoted and (S[J] in WhiteSpace) then
      break;
    Inc(J);
  end;
  if (I<=Length(S)) then
    Result:=Copy(S,I,J-I);
  Delete(S,1,J);
end;

function ExtractImportName(const s: string): string;
var
  i: integer;
begin
  Result := s;
  i := Pos(',', Result);
  if i > 1 then
    SetLength(Result, i-1);
  Result := ChangeFileExt(ExtractFileName(Result), '');
end;

function ExtractUnitName(s: string): string;
begin
  Result := ChangeFileExt(ExtractFileName(s), '');
end;

(* Unit name from Inputs[i] or Descriptions[i]
  Package name from Imports?
*)
function ExtractUnitName(AList: TStrings; AIndex: integer): string;
begin
  Result := UnitFile(AList, AIndex);
  if Result <> '' then
    Result := ChangeFileExt(ExtractFileName(Result), '');
end;

(* Extract a file reference from Inputs or Descriptions list.
  Check for existing list and item.
*)
function UnitFile(AList: TStrings; AIndex: integer): string;
var
  s: string;
begin
  if assigned(AList) and (AIndex < AList.Count) then begin
    s := AList[AIndex];
    while s <> '' do begin
      Result := GetNextWord(s);
      if (Result <> '') and (Result[1] <> '-') then
        exit; //found a non-option
    end;
  end;
  Result := ''; //should never happen!
end;

type

  TNodePair = Class(TObject)
  Private
    FEl : TPasElement;
    FNode : TDocNode;
  Public
    Constructor Create(AnElement : TPasElement; ADocNode : TDocNode);
    Property Element : TPasElement Read FEl;
    Property DocNode : TDocNode Read FNode;
  end;

{ TConfigFile }

function TConfigFile.IsDirty: boolean;
begin
  Result := Dirty;
end;

procedure TConfigFile.Flush;
begin
  if Dirty then
    UpdateFile; //only if dirty
end;

procedure TConfigFile.WriteSectionValues(const Section: string; Strings: TStrings);
var
  i: integer;
begin
//add missing: write Strings as a section
  if (Strings = nil) or (Strings.Count = 0) then
    exit; //nothing to write
  for i := 0 to Strings.Count - 1 do begin
    WriteString(Section, Strings.Names[i], Strings.ValueFromIndex[i]);
    //WriteString(Section, Strings[i], ''); //???
  end;
end;

{ TCmdOptions }

procedure TCmdOptions.Assign(Source: TPersistent);
var
  s: TCmdOptions absolute Source;
begin
  inherited Assign(Source); //writes to the local copy!
  if Source is TCmdOptions then begin
    WriteDeclaration := s.WriteDeclaration;
    DisableOverride := s.DisableOverride;
    DisableErrors:=s.DisableErrors;
    DisableSeealso:=s.DisableSeealso;
    DisableArguments:=s.DisableArguments;
    DisableFunctionResults := s.DisableFunctionResults;
    ShowPrivate := s.ShowPrivate;
    DisableProtected:=s.DisableProtected;
    SortNodes := s.SortNodes;
    Verbose:=s.Verbose;
  end;
end;

const SecOpts = 'default';

procedure TCmdOptions.LoadConfig(cf: TConfigFile; AProfile: string);
var
  s, sec: string;
begin
//MakeSkel
  WriteDeclaration := cf.ReadBool(SecOpts, 'WriteDeclaration', True);
  DisableOverride := cf.ReadBool(SecOpts, 'DisableOverride', False);
  DisableErrors := cf.ReadBool(SecOpts, 'DisableErrors', False);
  DisableSeealso := cf.ReadBool(SecOpts, 'DisableSeealso', False);
  DisableArguments := cf.ReadBool(SecOpts, 'DisableArguments', False);
  DisableFunctionResults := cf.ReadBool(SecOpts, 'DisableFunctionResults', False);
  ShowPrivate := cf.ReadBool(SecOpts, 'ShowPrivate', True);
  DisableProtected := cf.ReadBool(SecOpts, 'DisableProtected', False);
  SortNodes := cf.ReadBool(SecOpts, 'SortNodes', False);
//Engine
  StopOnParseError :=  cf.ReadBool(SecOpts, 'StopOnParseError', False);
  WarnNoNode :=  cf.ReadBool(SecOpts, 'WarnNoNode', True);
  InterfaceOnly :=  cf.ReadBool(SecOpts, 'InterfaceOnly', True);
  if AProfile = '' then
    AProfile := SecOpts;
  OSTarget := cf.ReadString(AProfile, 'OSTarget', DefOSTarget);
  CPUTarget := cf.ReadString(AProfile, 'CPUTarget', DefCPUTarget);
  Language := cf.ReadString(AProfile, 'Language', '');
  Backend :=  cf.ReadString(AProfile, 'Backend', 'html');
  MoDir :=  cf.ReadString(AProfile, 'MoDir', '');
  HideProtected :=  cf.ReadBool(AProfile, 'HideProtected', False);
  ShowPrivate :=  cf.ReadBool(AProfile, 'ShowPrivate', False);
  DontTrim := cf.ReadBool(AProfile, 'DontTrim', False);
//Backend
  s := cf.ReadString(AProfile, 'BackendOptions', '');
  BackendOptions.CommaText := s;
//finally
  Modified := False;
end;

procedure TCmdOptions.SaveConfig(cf: TConfigFile; AProfile: string);
begin
//MakeSkel
  cf.WriteBool(SecOpts, 'WriteDeclaration', WriteDeclaration);
  cf.WriteBool(SecOpts, 'DisableOverride', DisableOverride);
  cf.WriteBool(SecOpts, 'DisableErrors', DisableErrors);
  cf.WriteBool(SecOpts, 'DisableSeealso', DisableSeealso);
  cf.WriteBool(SecOpts, 'DisableArguments', DisableArguments);
  cf.WriteBool(SecOpts, 'DisableFunctionResults', DisableFunctionResults);
  cf.WriteBool(SecOpts, 'DisablePrivate', DisablePrivate);
  cf.WriteBool(SecOpts, 'DisableProtected', DisableProtected);
  cf.WriteBool(SecOpts, 'SortNodes', SortNodes);
//Engine
  cf.WriteBool(SecOpts, 'StopOnParseError', StopOnParseError);
  cf.WriteBool(SecOpts, 'WarnNoNode', WarnNoNode);
  cf.WriteBool(SecOpts, 'DontTrim', DontTrim);
  if AProfile = '' then
    AProfile := SecOpts;
  cf.WriteString(AProfile, 'OSTarget', OSTarget);
  cf.WriteString(AProfile, 'CPUTarget', CPUTarget);
  cf.WriteString(AProfile, 'Language', Language);
  cf.WriteString(AProfile, 'Backend', Backend);
  cf.WriteString(AProfile, 'MoDir', MoDir);
  cf.WriteBool(AProfile, 'HideProtected', HideProtected);
  cf.WriteBool(AProfile, 'ShowPrivate', ShowPrivate);
  cf.WriteBool(AProfile, 'InterfaceOnly', InterfaceOnly);
//Backend
  if BackendOptions.Count > 0 then
    cf.WriteString(AProfile, 'BackendOptions', BackendOptions.CommaText);
//finally
  Modified := False;
end;

procedure TCmdOptions.BackendToPairs(Dest: TStrings);
var
  i, n: integer;
begin
  Dest.Clear;
  n := BackendOptions.Count div 2;
  if n = 0 then
    exit;
  Dest.Capacity := n;
  for i := 0 to n-1 do begin
    Dest.Add(BackendOptions[i*2] + '=' + BackendOptions[i*2 + 1]);
  end;
end;

procedure TCmdOptions.BackendFromPairs(Source: TStrings);
var
  i: integer;
begin
  BackendOptions.Clear;
  BackendOptions.Capacity:=Source.Count * 2;
  for i := 0 to Source.Count - 1 do begin
    BackendOptions.Add(Source.Names[i]);
    BackendOptions.Add(Source.ValueFromIndex[i]);
  end;
  Modified := True; //todo: only if really changed?
end;

Constructor TNodePair.Create(AnElement : TPasElement; ADocNode : TDocNode);

begin
  Fel:=Anelement;
  FNode:=ADocNode;
end;

function TSkelEngine.FindModule(const AName: String): TPasModule; 

Var
  I : Integer;

begin
  Result:=Inherited FindModule(AName);
  If (Result=Nil) then
    begin // Create dummy list and search in that.
    If (FModules=Nil) then
      begin
      FModules:=TStringList.Create;
      FModules.Sorted:=True;
      end;
    I:=FModules.IndexOf(AName);
    IF (I=-1) then
      begin
      Result:=TPasModule.Create(AName,Nil);
      FModules.AddObject(AName,Result);
      end
    else
      Result:=FModules.Objects[i] as TPasModule;  
    end;  
end;

Destructor TSkelEngine.Destroy; 

Var
  I : Integer;

begin
  If Assigned(FModules) then 
    begin
    For I:=0 to FModules.Count-1 do
      FModules.Objects[i].Free;
    FreeAndNil(FModules);    
    end;
end;

Function TSkelEngine.MustWriteElement(El : TPasElement; Full : Boolean) : Boolean;

Var
  ParentVisible:Boolean;
  PT,PP : TPasElement;
begin
  ParentVisible:=True;
  If (El is TPasArgument) or (El is TPasResultElement) then
    begin
    PT:=El.Parent;
    // Skip ProcedureType or PasFunctionType
    If (PT<>Nil) then
      begin
      if (PT is TPasProcedureType) or (PT is TPasFunctionType) then
        PT:=PT.Parent;
      If (PT<>Nil) and ((PT is TPasProcedure) or (PT is TPasProcedure))   then
        PP:=PT.Parent
      else
        PP:=Nil;
      If (PP<>Nil) and (PP is TPasClassType) then
        begin
        ParentVisible:=((not Options.DisablePrivate or (PT.Visibility<>visPrivate)) and
                       (not Options.DisableProtected or (PT.Visibility<>visProtected)));
        end;
      end;
    end;
  Result:=Assigned(El.Parent) and (Length(El.Name) > 0) and
          (ParentVisible and (not Options.DisableArguments or (El.ClassType <> TPasArgument))) and
          (ParentVisible and (not Options.DisableFunctionResults or (El.ClassType <> TPasResultElement))) and
          (not Options.DisablePrivate or (el.Visibility<>visPrivate)) and
          (not Options.DisableProtected or (el.Visibility<>visProtected));
  If Result and Full then
    begin
    Result:=(Not Assigned(FEmittedList) or (FEmittedList.IndexOf(El.FullName)=-1));
    If Options.DisableOverride and (El is TPasProcedure) then
      Result:=Not TPasProcedure(El).IsOverride;
    end;  
end;


function TSkelEngine.CreateElement(AClass: TPTreeElement; const AName: String;
  AParent: TPasElement; AVisibility : TPasMemberVisibility;
  const ASourceFilename: String; ASourceLinenumber: Integer): TPasElement;

Var
  DN : TDocNode;

begin
  Result := AClass.Create(AName, AParent);
  Result.Visibility:=AVisibility;
  if AClass.InheritsFrom(TPasModule) then
    CurModule := TPasModule(Result);
  // Track this element
  If Options.UpdateMode then
    begin
    DN:=FindDocNode(Result);    
    If Assigned(DN) then
      DN.IncRefCount;
    end
  else
    DN:=Nil;  
  // See if we need to write documentation for it
  If MustWriteElement(Result,False) then
    FNodeList.AddObject(Result.PathName,TNodePair.Create(Result,DN));
end;

Function TSkelEngine.WriteElement(Var F : Text;El : TPasElement; ADocNode : TDocNode) : Boolean;

  Function WriteOnlyShort(APasElement : TPasElement) : Boolean;

  begin
    Result:=(APasElement.ClassType=TPasArgument) or
            (APasElement.ClassType=TPasResultElement) or
            (APasElement.ClassType=TPasEnumValue);
  end;

  Function IsTypeVarConst(APasElement : TPasElement) : Boolean;

  begin
    With APasElement do
      Result:=(InheritsFrom(TPasType) and not InheritsFrom(TPasClassType)) or
              (InheritsFrom(TPasResString)) or
              (InheritsFrom(TPasVariable));
  end;
  
  Function NeedDeclaration(El : TPasElement) : boolean;
  
  begin
    Result:=IsTypeVarConst(El) 
            or WriteOnlyShort(El) 
            or EL.InheritsFrom(TPasProcedure) 
  end;
    
begin
  // Check again, this time with full declaration.
  Result:=MustWriteElement(El,True);
  If Result and Options.UpdateMode then
     Result:=(ADocNode=Nil);
  If Not Result Then
    Exit;
  If Options.UpdateMode then
    DoLog(Format(ScreatingNewNode,[el.PathName]));
  FEmittedList.Add(El.FullName); // So we don't emit again.
  WriteLn(f);
  if Options.EmitClassSeparator and (El.ClassType = TPasClassType) then
    begin
    WriteLn(f, '<!--');
    WriteLn(f, '  ********************************************************************');
    WriteLn(f, '    ', El.PathName);
    WriteLn(f, '  ********************************************************************');
    WriteLn(f, '-->');
    WriteLn(f);
    end;
  If Not (Options.WriteDeclaration and NeedDeclaration(El)) then
    Writeln(F,'<!-- ', El.ElementTypeName,' Visibility: ',VisibilityNames[El.Visibility], ' -->')
  else  
    begin
    Writeln(F,'<!-- ',El.ElementTypeName,' Visibility: ',VisibilityNames[El.Visibility]);
    Writeln(F,'     Declaration: ',El.GetDeclaration(True),' -->');
    end;
  WriteLn(f,'<element name="', El.FullName, '">');
  WriteLn(f, '<short></short>');
  if Not WriteOnlyShort(El) then
    begin
    WriteLn(f, '<descr>');
    WriteLn(f, '</descr>');
    if not (Options.DisableErrors or IsTypeVarConst(El)) then
      begin
      WriteLn(f, '<errors>');
      WriteLn(f, '</errors>');
      end;
    if not Options.DisableSeealso then
      begin
      WriteLn(f, '<seealso>');
      WriteLn(f, '</seealso>');
      end;
    end;
  WriteLn(f, '</element>');
end;

Procedure  TSkelEngine.DoWriteUnReferencedNodes(N : TDocNode; NodePath : String);

begin
  If (N<>Nil) then
    begin
    If (NodePath<>'') then
      NodePath:=NodePath+'.';
    DoWriteUnReferencedNodes(N.FirstChild,NodePath+N.Name);
    While (N<>Nil) do
      begin
      if (N.RefCount=0) and (N.Node<>Nil) and (Not N.TopicNode) then
        DoLog(Format(SNodeNotReferenced,[NodePath+N.Name]));
      N:=N.NextSibling;
      end;
    end;
end;

procedure TSkelEngine.SetOptions(AValue: TCmdOptions);
begin
  if FOptions=AValue then Exit;
  FOptions:=AValue;
end;

procedure TSkelEngine.WriteUnReferencedNodes;

begin
  DoWriteUnReferencedNodes(RootDocNode,'');
end;

Procedure TSkelEngine.WriteNodes(Var F : Text; AModule : TPasModule; List : TStrings);

Var
  P : TNodePair;
  I : integer;

begin
  WriteLn(f);
  WriteLn(f, '<!--');
  WriteLn(f, '  ====================================================================');
  WriteLn(f, '    ', Amodule.Name);
  WriteLn(f, '  ====================================================================');
  WriteLn(f, '-->');
  WriteLn(f);
  WriteLn(f, '<module name="', AModule.Name, '">');
  if not Options.UpdateMode then
    begin
    WriteLn(f, '<short></short>');
    WriteLn(f, '<descr>');
    WriteLn(f, '</descr>');
    end;
  Try 
    For I:=0 to List.Count-1 do
      begin
      P:=List.Objects[i] as TNodePair;
      If (P.Element<>AModule) then
        WriteElement(F,P.Element,P.DocNode);
      end;
  Finally
    WriteLn(f, '');
    WriteLn(f, '</module> <!-- ', AModule.Name, ' -->');
    WriteLn(f, '');
  end;
end;

Procedure TSkelEngine.DocumentFile(Var F : Text; Const AFileName,ATarget,ACPU : String);

Var
  Module : TPasModule;
  I : Integer;
  N : TDocNode;
     
begin
  FNodeList:=TStringList.Create;
  Try
    FEmittedList:=TStringList.Create;
    FEmittedList.Sorted:=True;
    try
      Module:=ParseSource(Self,AFileName,ATarget,ACPU);
      If Options.UpdateMode then
        begin
        N:=FindDocNode(Module);
        If Assigned(N) then
           N.IncRefCount;
         end;
      If Options.SortNodes then
        FNodelist.Sorted:=True;   
      WriteNodes(F,Module,FNodeList);  
      If Options.UpdateMode then
        WriteUnReferencedNodes;
    Finally
      FEmittedList.Free;
    end;  
  Finally  
    For I:=0 to FNodeList.Count-1 do
      FNodeList.Objects[i].Free;
    FNodeList.Free;  
  end;  
end;

{ ---------------------------------------------------------------------
  Main program. Document all units.    
  ---------------------------------------------------------------------}

{ TFPDocMaker }

constructor TFPDocMaker.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FOptions := TCmdOptions.Create;
end;

destructor TFPDocMaker.Destroy;
begin
  FreeAndNil(FOptions);
  inherited Destroy;
end;

function TFPDocMaker.SelectedPackage: TFPDocPackage;
begin
  Result:=FPackage;
  if (FPackage=Nil) or (FPackage.Name='') then
    begin
    DoLog(SNeedPackageName);
    //Usage(1); - in application
    end;
end;

procedure TFPDocMaker.SetOnOption(AValue: THandleOption);
begin
  if FOnOption=AValue then Exit;
  FOnOption:=AValue;
end;

procedure TFPDocMaker.SetDescrDir(AValue: string);
begin
  if FDescrDir=AValue then Exit;
  FDescrDir:=AValue;
  AddDirToFileList(SelectedPackage.Descriptions, AValue, '*.xml');
end;

function TFPDocMaker.GetDescrDir: string;
begin
  if FDescrDir = '' then begin
    if SelectedPackage.Descriptions.Count > 0 then begin
      Result := FPackage.Descriptions[0];
      FDescrDir := ExtractFilePath(Result); //include separator
    end;
  end;
  Result := FDescrDir;
end;

function TFPDocMaker.UnitSpec(AUnit: string): string;
var
  i: integer;
  w: string;
begin
  for i := 0 to SelectedPackage.Inputs.Count - 1 do begin
    w := ExtractUnitName(FPackage.Inputs, i);
    if CompareText(w, AUnit) = 0 then begin
      Result := FPackage.Inputs[i];
      exit;
    end;
  end;
  Result := '';
end;

function TFPDocMaker.ImportName(AIndex: integer): string;
begin
  Result := ExtractImportName(SelectedPackage.Imports[AIndex]);
end;

function TFPDocMaker.GetInputDir: string;
var
  W: string;
begin
  if (FInputDir = '') and (SelectedPackage.Inputs.Count > 0) then begin
    Result := FPackage.Inputs[0];
    while Result <> '' do begin
      w := GetNextWord(Result);
      if (w <> '') and (w[1] <> '-') then begin
        FInputDir := ExtractFilePath(W); //include separator
        break;
      end;
    end;
  end;
  Result := FInputDir;
end;

procedure TFPDocMaker.SetInputDir(AValue: string);
begin
  if FInputDir=AValue then Exit;
  FInputDir:=AValue;
  AddDirToFileList(SelectedPackage.Inputs, AValue, '*.pp');
  AddDirToFileList(SelectedPackage.Inputs, AValue, '*.pas');
end;

procedure TFPDocMaker.SetOptions(AValue: TCmdOptions);
begin
  //if FOptions=AValue then Exit;
  FOptions.Assign(AValue);  //the local MakeSkel options
  Options.Assign(AValue);   //the FPDoc Engine options
  Verbose := AValue.Verbose; //not in Options
end;

(* Check the options, return errors as message strings.
*)
function TFPDocMaker.CheckSkelOptions: string;

Const
{$IFDEF Unix}
  MoFileTemplate = '/usr/local/share/locale/%s/LC_MESSAGES/makeskel.mo';
{$ELSE}
  MoFileTemplate ='intl/makeskel.%s.mo';
{$ENDIF}

Var
  MOFilename: string;

begin
  Result := '';
//translate strings - only once?
  If (Options.Language<>FTranslated) then begin
    MOFilename:=Format(MOFileTemplate,[Options.Language]);
    if FileExists(MOFilename) then
      gettext.TranslateResourceStrings(MoFileName)
    else begin
      Result := ('NOTE: unable to find translation file ' + MOFilename);
      exit;
    end;
    // Translate internal documentation strings
    TranslateDocStrings(Options.Language);
    FTranslated:=Options.Language;
  end;
  // Action is to create the XML skeleton
  if (Package.Name = '') and (CmdAction<>caUsage) then begin
    Result := (SNoPackageNameProvided);
    exit;
  end;
  if CmdOptions.UpdateMode
  and (SelectedPackage.Descriptions.IndexOf(Package.Output)<>-1) then begin
    Result := (SOutputMustNotBeDescr);
    exit;
  end;
end;

procedure TFPDocMaker.SetCmdAction(AValue: TCreatorAction);
begin
  if FCmdAction=AValue then Exit;
  FCmdAction:=AValue;
end;

procedure TFPDocMaker.SetDryRun(AValue: boolean);
begin
  if FDryRun=AValue then Exit;
  FDryRun:=AValue;
end;

procedure TFPDocMaker.SetPackage(AValue: TFPDocPackage);
begin
  if FPackage=AValue then Exit;
  FPackage:=AValue;
end;

procedure TFPDocMaker.SetWriteProjectFile(AValue: string);
begin
  if FWriteProjectFile=AValue then Exit;
  FWriteProjectFile:=AValue;
end;

procedure TFPDocMaker.AddDirToFileList(List: TStrings; const ADirName, AMask: String);

Var
  Info : TSearchRec;
  D : String;

begin
  if (ADirName<>'') and not DirectoryExists(ADirName) then
     DoLog('Directory '+ADirName+' does not exist')
  else
    begin
    if (ADirName='.') or (ADirName='') then
      D:=''
    else
      D:=IncludeTrailingPathDelimiter(ADirName);
    If (FindFirst(D+AMask,0,Info)=0) then
      try
        Repeat
          If (Info.Attr and faDirectory)=0 then
            List.Add(D+Info.name);
        Until FindNext(Info)<>0;
      finally
        FindClose(Info);
      end;
    end;
end;

procedure TFPDocMaker.AddToFileList(List: TStrings; const FileName: String);
var
  f: Text;
  s: String;
begin
  if Copy(FileName, 1, 1) = '@' then
  begin
    AssignFile(f, Copy(FileName, 2, Length(FileName)));
    Reset(f);
    while not EOF(f) do
    begin
      ReadLn(f, s);
      List.Add(s);
    end;
    Close(f);
  end else
    List.Add(FileName);
end;

function TFPDocMaker.ParseCommon(var Cmd, Arg: string):  TCreatorAction;
var
  i: Integer;
begin
  if (Cmd = '-h') or (Cmd = '--help') then begin
    //Usage(0)
    CmdAction := caUsage;
    exit(caUsage);
  end;
{$IFDEF v0}
  if Cmd = '--makeskel' then
    Options.CreateSkeleton := True
  else
{$ELSE}
{$ENDIF}
  if Cmd = '--update' then
    CmdOptions.UpdateMode := True
  else if (Cmd = '-n') or (Cmd = '--dry-run') then
    begin
    DryRun:=True;
    CmdAction := caDryRun;
    end
//project options
  else if Cmd = '--hide-protected' then
    Options.HideProtected := True
  else if Cmd = '--warn-no-node' then
    Options.WarnNoNode := True
  else if Cmd = '--show-private' then
    Options.ShowPrivate := True  //DoDi: was False???
  else if Cmd = '--stop-on-parser-error' then
    Options.StopOnParseError := True
  else if Cmd = '--dont-trim' then
    Options.DontTrim := True
  else if Cmd = '--parse-impl' then
    Options.InterfaceOnly:=false //is default really True???
  else begin
  //split option
    i := Pos('=', Cmd);
    if i > 0 then begin
      Arg := Copy(Cmd, i + 1, Length(Cmd));
      SetLength(Cmd, i - 1);
      if (Arg <> '') and (Arg[1] = '"') then begin
      //remove quotes
        Arg := StringReplace(Arg, '"', '', [rfReplaceAll]);
      end;
    end else begin
      SetLength(Arg, 0);
      exit(caInvalid); //options without values unhandled here!
    end;
  //more options
    Result := caDefault; //assume succ
    if (Cmd = '--project') or (Cmd='-p') then begin
      FProjectFile:=True; //means: project loaded
      WriteProjectFile := Arg; //do *not* normally overwrite!
      LoadProjectFile(Arg);
    end else if (Cmd = '--descr') then begin
      if FileExists(Arg) then
        AddToFileList(SelectedPackage.Descriptions, Arg)
    end else if (Cmd = '--descr-dir') then
      DescrDir:=Arg
    else if (Cmd = '-i') or (Cmd = '--input') then
      AddToFileList(SelectedPackage.Inputs, Arg)
    else if (Cmd = '--input-dir') then
      InputDir:=Arg
    else if Cmd = '--package' then begin
      If FProjectFile then
        FPackage:=Packages.FindPackage(Arg)
      else begin
        if FPackage = nil then
          FPackage := (Packages.Add) as TFPDocPackage;
        FPackage.Name:=Arg;
      end
    end else if Cmd = '--ostarget' then
      Options.OSTarget := Arg
    else if Cmd = '--cputarget' then
      Options.CPUTarget := Arg
    else if (Cmd = '-l') or (Cmd = '--lang') then
      Options.Language := Arg
  {$IFDEF new}
    else if (Cmd = '--common-options') then
      SelectedPackage.CommonOptions:=Arg
  {$ELSE}
  {$ENDIF}
    else if Cmd = '--mo-dir' then
      Options.modir := Arg
    else if (Cmd = '-o') or (Cmd = '--output') then
      SelectedPackage.Output := Arg
    else if (Cmd = '--unit') then //-u= UpdateMode
      SelectedUnit:= Arg
    else if (Cmd = '-v') or (Cmd = '--verbose') then
      Verbose:=true
    else if Cmd = '--write-project' then begin
      CmdAction := caWriteProject;
      WriteProjectFile:=Arg
    end
  //else no match
    else
      Result := caInvalid;
  end;
end;

function TFPDocMaker.ParseFPDocOption(const S: string):  TCreatorAction;
//procedure TFPDocAplication.Parseoption(Const S : String);
var
  Cmd, Arg: String;
begin
  Cmd:=S;
  Arg := ''; //make compiler happy
  Result := ParseCommon(Cmd, Arg);
  if Result <> caInvalid then
    exit;
  Result := caDefault; //assume succ
  if Cmd = '--content' then
    SelectedPackage.ContentFile := Arg
  else if Cmd = '--import' then
    SelectedPackage.Imports.Add(Arg)
//this should not be a project option
  else if (Cmd = '-f') or (Cmd = '--format') then
    begin
    Arg:=UpperCase(Arg);
    If FindWriterClass(Arg)=-1 then
      WriteLn(StdErr, Format(SCmdLineInvalidFormat, [Arg]))
    else
      Options.BackEnd:=Arg;
    end
  else
    begin
    Options.BackendOptions.Add(Cmd);
    Options.BackendOptions.Add(Arg);
    end;
end;

procedure TFPDocMaker.LogToStdOut(Sender: TObject; const msg: string);
begin
  WriteLn(msg);
end;

procedure TFPDocMaker.LogToStdErr(Sender: TObject; const msg: string);
begin
  WriteLn(stderr, msg);
end;

(* Write *all* updates into AOutputName (=DescrFile for Create, UpdFile for Update).
*)
Function TFPDocMaker.DocumentPackage(Const APackageName,AOutputName: string; InputFiles, DescrFiles : TStrings) : String;
Var
  F : Text;
  I,J : Integer;
  Engine: TSkelEngine;
begin
  Result:='';
  AssignFile(f, AOutputName);
  Rewrite(f);
  Try
    WriteLn(f, '<?xml version="1.0" encoding="ISO-8859-1"?>');
    WriteLn(f, '<fpdoc-descriptions>');
    WriteLn(f, '<package name="', APackageName, '">');
    I:=0;
    While (Result='') And (I<InputFiles.Count) do
      begin
      Engine := TSkelEngine.Create;
    //configure engine
      Engine.OnLog:=Self.OnLog;
      Engine.ScannerLogEvents:=Self.ScannerLogEvents;
      Engine.ParserLogEvents:=Self.ParserLogEvents;
      Engine.Options := CmdOptions;
      Try
        Engine.SetPackageName(APackageName);
        if CmdOptions.UpdateMode then
          For J:=0 to DescrFiles.Count-1 do
            Engine.AddDocFile(DescrFiles[J]);
        Try
          Engine.DocumentFile(F,InputFiles[i],Options.OSTarget,Options.CPUTarget);
        except
          on E:Exception do
          begin
            Result:='Error while documenting: '+E.message;
          end;
        end;
      Finally
        Engine.Free;
      end;
      Inc(I);
      end;
  Finally
    WriteLn(f, '</package>');
    WriteLn(f, '</fpdoc-descriptions>');
    Close(f);
  end;
end;

procedure TFPDocMaker.CreateUnitDocumentation(const AUnit: string; ParseOnly: Boolean);
var
  il: TStringList;
  spec: string;
begin
  if AUnit <> '' then begin
  //selected unit only
    spec := UnitSpec(AUnit);
    il := TStringList.Create;
    il.Assign(Package.Inputs);
    Package.Inputs.Clear;
    Package.Inputs.Add(spec);
    try
      inherited CreateDocumentation(Package, ParseOnly);
    finally
      Package.Inputs.Assign(il);
      il.Free;
    end;
  end else begin
    CreateDocumentation(Package,ParseOnly);
  end;
end;

(* Return True and (try) kill file if no "<element" found.
*)
function TFPDocMaker.CleanXML(const FileName: string): boolean;
var
  f: TextFile;
  s: string;
begin
  AssignFile(f, FileName);
  Reset(f);
  try
    while not EOF(f) do begin
      ReadLn(f, s);
      if Pos('<element ', s) > 0 then
        exit(False); //file not empty
    end;
  finally
    CloseFile(f);
  end;
//nothing found, delete the file
  if DeleteFile(FileName) then
    DoLog('File ' + FileName + ' has no elements. Deleted.')
  else
    DoLog('File ' + FileName + ' has no elements. Delete failed.');
  Result := True;
end;


function TFPDocMaker.ParseUpdateOption(const s: String):  TCreatorAction;
var
  Cmd, Arg: String;
begin
  Cmd:=S;
  Arg := ''; //make compiler happy
  Result := ParseCommon(Cmd, Arg);
  if Result <> caInvalid then
    exit;
  Result := caDefault; //assume succ
  if s = '--disable-arguments' then
    CmdOptions.DisableArguments := True
  else if s = '--disable-errors' then
    CmdOptions.DisableErrors := True
  else if s = '--disable-function-results' then
    CmdOptions.DisableFunctionResults := True
  else if s = '--disable-seealso' then
    CmdOptions.DisableSeealso := True
  else if s = '--disable-private' then
    CmdOptions.DisablePrivate := True
  else if s = '--disable-override' then
    CmdOptions.DisableOverride := True
  else if s = '--disable-protected' then
    begin
    CmdOptions.DisableProtected := True;
    CmdOptions.DisablePrivate :=True;
    end
  else if (s = '--emitclassseparator') or (s='--emit-class-separator') then
    CmdOptions.EmitClassSeparator := True
  else if (s = '--emit-declaration') then
    CmdOptions.WriteDeclaration := True
  else if (s = '--sort-nodes') then
    CmdOptions.SortNodes := True
  else if (Cmd = '-i') or (Cmd = '--input') then
    AddToFileList(SelectedPackage.Inputs, Arg)
  else if not assigned(OnOption) or not OnOption(Cmd, Arg) then begin
    DoLog(SCmdLineInvalidOption, [s]);
    CmdAction := caInvalid;
    Result := caInvalid;
  end;
end;

end.

