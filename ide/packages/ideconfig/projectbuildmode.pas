{
***************************************************************************
*                                                                         *
*   This source is free software; you can redistribute it and/or modify   *
*   it under the terms of the GNU General Public License as published by  *
*   the Free Software Foundation; either version 2 of the License, or     *
*   (at your option) any later version.                                   *
*                                                                         *
*   This code is distributed in the hope that it will be useful, but      *
*   WITHOUT ANY WARRANTY; without even the implied warranty of            *
*   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU     *
*   General Public License for more details.                              *
*                                                                         *
*   A copy of the GNU General Public License is available on the World    *
*   Wide Web at <http://www.gnu.org/copyleft/gpl.html>. You can also      *
*   obtain it by writing to the Free Software Foundation,                 *
*   Inc., 51 Franklin Street - Fifth Floor, Boston, MA 02110-1335, USA.   *
*                                                                         *
***************************************************************************

 Abstract:
   Buildmodes used by a project.
}
unit ProjectBuildMode;

{$mode objfpc}{$H+}

interface

uses
  // RTL + FCL
  Classes, SysUtils,
  // LazUtils
  LazFileCache, LazMethodList, LazLoggerBase, Laz2_XMLCfg,
  // BuildIntf
  ProjectIntf, CompOptsIntf,
  // IdeConfig
  IDEProcs, CompOptsModes, ModeMatrixOpts, CompilerOptions;

type

  { TProjectBuildMode }

  TProjectBuildMode = class(TLazProjectBuildMode)
  private
    FCompilerOptions: TBaseCompilerOptions;// TProjectCompilerOptions;
  protected
    function GetLazCompilerOptions: TLazCompilerOptions; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function LazProject: TLazProject;
    procedure Clear;
    function Equals(Src: TProjectBuildMode): boolean; reintroduce;
    function CreateDiff(Other: TProjectBuildMode;
                        Tool: TCompilerDiffTool = nil): boolean;
    procedure Assign(Src: TProjectBuildMode); reintroduce;
    procedure LoadFromXMLConfig(XMLConfig: TXMLConfig; const Path: string);
    procedure SaveMacroValuesAtOldPlace(XMLConfig: TXMLConfig; const Path: string);
    procedure SaveToXMLConfig(XMLConfig: TXMLConfig; const Path: string;
                              IsDefault, ALegacyList: Boolean; var Cnt: integer);
    function GetCaption: string; override;
  public
    // copied by Assign, compared by Equals, cleared by Clear
    property CompilerOptions: TBaseCompilerOptions read FCompilerOptions;
  end;

  { TProjectBuildModes }

  TProjectBuildModes = class(TLazProjectBuildModes)
  private
    FAssigning: Boolean;
    FSessionMatrixOptions: TBuildMatrixOptions;
    FSharedMatrixOptions: TBuildMatrixOptions;
    FManyBuildModes: TStringList;  // User selection of many modes.
    fSavedChangeStamp: int64;
    fItems: TFPList;
    FLazProject: TLazProject;
    fChangedHandlers: TMethodList;
    // Variables used by Project's LoadFromXMLConfig and SaveToXMLConfig
    FXMLConfig: TXMLConfig;
    FGlobalMatrixOptions: TBuildMatrixOptions;
    function GetItems(Index: integer): TProjectBuildMode;
    function GetModified: boolean;
    procedure ItemChanged(Sender: TObject);
    procedure SetModified(const AValue: boolean);
    // Used by LoadFromXMLConfig
    procedure AddMatrixMacro(const MacroName, MacroValue, ModeIdentifier: string; InSession: boolean);
    procedure LoadSessionEnabledNonSessionMatrixOptions(const Path: string);
    procedure LoadOtherCompilerOpts(const Path: string; FromIndex, ToIndex: Integer; InSession: boolean);
    procedure LoadMacroValues(const Path: string; CurMode: TProjectBuildMode);
    procedure LoadAllMacroValues(const Path: string; Cnt: Integer);
    procedure LoadOldFormat(const Path: string);
    procedure LoadActiveBuildMode(const Path: string);
  protected
    function GetLazBuildModes(Index: integer): TLazProjectBuildMode; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Clear;
    function IsEqual(OtherModes: TProjectBuildModes): boolean;
    procedure Assign(Source: TPersistent; WithModified: boolean); overload;
    procedure Delete(Index: integer);
    function IndexOf(Identifier: string): integer;
    function IndexOf(aMode: TProjectBuildMode): integer;
    function Find(Identifier: string): TProjectBuildMode;
    function Add(Identifier: string): TProjectBuildMode;
    procedure Move(FromIndex, ToIndex: integer);
    function Count: integer; override;
    procedure IncreaseChangeStamp;
    procedure AddOnChangedHandler(const Handler: TNotifyEvent);
    procedure RemoveOnChangedHandler(const Handler: TNotifyEvent);
    function IsModified(InSession: boolean): boolean;
    function GetSessionModes: TStringList;
    function IsSessionMode(const ModeIdentifier: string): boolean;
    function IsSharedMode(const ModeIdentifier: string): boolean;
    procedure RenameMatrixMode(const OldName, NewName: string);
    function CreateExtraModes(aCurMode: TProjectBuildMode): TProjectBuildMode;
    // load, save
    procedure LoadProjOptsFromXMLConfig(XMLConfig: TXMLConfig; const Path: string);
    procedure LoadSessionFromXMLConfig(XMLConfig: TXMLConfig; const Path: string;
                                       LoadAllOptions: boolean);
    procedure SaveProjOptsToXMLConfig(XMLConfig: TXMLConfig; const Path: string;
                                      SaveSession, ALegacyList: boolean);
    procedure SaveSessionOptsToXMLConfig(XMLConfig: TXMLConfig; const Path: string;
                                         SaveSession, ALegacyList: boolean);
    // Used by Project's SaveToXMLConfig
    procedure SaveSessionData(const Path: string);
    procedure SaveSharedMatrixOptions(const Path: string);
  public
    property Items[Index: integer]: TProjectBuildMode read GetItems; default;
    property ChangeStamp: integer read FChangeStamp;
    property LazProject: TLazProject read FLazProject write FLazProject;
    property Assigning: Boolean read FAssigning;
    property Modified: boolean read GetModified write SetModified;
    property SharedMatrixOptions: TBuildMatrixOptions read FSharedMatrixOptions;
    property SessionMatrixOptions: TBuildMatrixOptions read FSessionMatrixOptions;
    property ManyBuildModes: TStringList read FManyBuildModes;
    property ChangedHandlers: TMethodList read fChangedHandlers;
    property GlobalMatrixOptions: TBuildMatrixOptions read FGlobalMatrixOptions
                                                     write FGlobalMatrixOptions;
  end;

const
  // Names for extra buildmodes which may be created automatically.
  DebugModeName = 'Debug';
  ReleaseModeName = 'Release';

var
  CompilerOptClass: TBaseCompilerOptionsClass;

implementation

{ TProjectBuildMode }

function TProjectBuildMode.GetLazCompilerOptions: TLazCompilerOptions;
begin
  Result:=FCompilerOptions;
end;

constructor TProjectBuildMode.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FCompilerOptions:=CompilerOptClass.Create(LazProject);
  FCompilerOptions.AddOnChangedHandler(@OnItemChanged);
  //FCompilerOptions.FBuildMode:=Self;
end;

destructor TProjectBuildMode.Destroy;
begin
  FreeAndNil(FCompilerOptions);
  inherited Destroy;
end;

function TProjectBuildMode.LazProject: TLazProject;
begin
  if Owner is TProjectBuildModes then
    Result:=TProjectBuildModes(Owner).LazProject
  else
    Result:=Nil;
end;

procedure TProjectBuildMode.Clear;
begin
  CompilerOptions.Clear;
end;

function TProjectBuildMode.Equals(Src: TProjectBuildMode): boolean;
begin
  Result:=CompilerOptions.IsEqual(Src.CompilerOptions);
end;

function TProjectBuildMode.CreateDiff(Other: TProjectBuildMode;
  Tool: TCompilerDiffTool): boolean;
begin
  // Note: if there is a Tool all steps must be evaluated, if not exit on first diff
  //if Tool<>nil then debugln(['TProjectBuildMode.CreateDiff ']);
  Result:=CompilerOptions.CreateDiff(Other.CompilerOptions,Tool);
  if (Tool=nil) and Result then exit;
end;

procedure TProjectBuildMode.Assign(Src: TProjectBuildMode);
begin
  if Equals(Src) then exit;
  InSession:=Src.InSession;
  CompilerOptions.Assign(Src.CompilerOptions);
end;

procedure TProjectBuildMode.LoadFromXMLConfig(XMLConfig: TXMLConfig;
  const Path: string);
begin
  FIdentifier:=XMLConfig.GetValue('Identifier','');
  FCompilerOptions.LoadFromXMLConfig(XMLConfig,Path+'CompilerOptions/');
end;

procedure TProjectBuildMode.SaveMacroValuesAtOldPlace(XMLConfig: TXMLConfig; const Path: string);
var
  Cnt: Integer;
  Modes: TProjectBuildModes;
begin
  // for older IDE (<1.1) save the macros at the old place
  Assert(Assigned(Owner), 'SaveMacroValuesAtOldPlace: Owner not assigned.');
  Modes := Owner as TProjectBuildModes;
  Cnt:=Modes.SessionMatrixOptions.SaveAtOldXMLConfig(XMLConfig, Path, Identifier);
  Cnt+=Modes.SharedMatrixOptions.SaveAtOldXMLConfig(XMLConfig, Path, Identifier);
  XMLConfig.SetDeleteValue(Path+'Count',Cnt,0);
end;

procedure TProjectBuildMode.SaveToXMLConfig(XMLConfig: TXMLConfig;
  const Path: string; IsDefault, ALegacyList: Boolean; var Cnt: integer);
var
  SubPath: String;
begin
  SubPath:=Path+'BuildModes/'+XMLConfig.GetListItemXPath('Item', Cnt, ALegacyList, True)+'/';
  inc(Cnt);
  XMLConfig.SetDeleteValue(SubPath+'Name',Identifier,'');
  if IsDefault then
    XMLConfig.SetDeleteValue(SubPath+'Default',True,false)
  else begin
    SaveMacroValuesAtOldPlace(XMLConfig, SubPath+'MacroValues/');
    CompilerOptions.SaveToXMLConfig(XMLConfig,SubPath+'CompilerOptions/');
  end;
end;

function TProjectBuildMode.GetCaption: string;
var
  i: Integer;
begin
  Result:=Identifier;
  // Is this filtering really needed?   [question by Juha]
  for i:=length(Result) downto 1 do
    if Result[i] in ['&',#0..#31,#127] then
      System.Delete(Result,i,1);
end;

{ TProjectBuildModes }

function TProjectBuildModes.GetItems(Index: integer): TProjectBuildMode;
begin
  Result:=TProjectBuildMode(fItems[Index]);
end;

function TProjectBuildModes.GetModified: boolean;
begin
  Result:=fSavedChangeStamp<>FChangeStamp;
end;

procedure TProjectBuildModes.ItemChanged(Sender: TObject);
begin
  {$IFDEF VerboseIDEModified}
  debugln(['TProjectBuildModes.OnItemChanged ',DbgSName(Sender)]);
  {$ENDIF}
  IncreaseChangeStamp;
end;

procedure TProjectBuildModes.SetModified(const AValue: boolean);
var
  i: Integer;
begin
  if AValue then
    IncreaseChangeStamp
  else begin
    for i:=0 to Count-1 do
      Items[i].Modified:=false;
    SharedMatrixOptions.Modified:=false;
    SessionMatrixOptions.Modified:=false;
    fSavedChangeStamp:=FChangeStamp;
  end;
end;

constructor TProjectBuildModes.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  fChangedHandlers:=TMethodList.Create;
  fItems:=TFPList.Create;
  FChangeStamp:=LUInvalidChangeStamp;
  fSavedChangeStamp:=FChangeStamp;
  FSharedMatrixOptions:=TBuildMatrixOptions.Create;
  FSharedMatrixOptions.OnChanged:=@ItemChanged;
  FSessionMatrixOptions:=TBuildMatrixOptions.Create;
  FSessionMatrixOptions.OnChanged:=@ItemChanged;
  FManyBuildModes:=TStringList.Create;
end;

destructor TProjectBuildModes.Destroy;
begin
  FreeAndNil(fChangedHandlers);
  Clear;
  FreeAndNil(FManyBuildModes);
  FreeAndNil(FSharedMatrixOptions);
  FreeAndNil(FSessionMatrixOptions);
  FreeAndNil(fItems);
  inherited Destroy;
end;

procedure TProjectBuildModes.Clear;
begin
  while Count>0 do
    Delete(Count-1);
  SharedMatrixOptions.Clear;
  SessionMatrixOptions.Clear;
  //fChangedHandlers.Clear;
end;

function TProjectBuildModes.IsEqual(OtherModes: TProjectBuildModes): boolean;
var
  i: Integer;
begin
  Result:=true;
  if OtherModes.Count<>Count then exit;
  for i:=0 to Count-1 do
    if not Items[i].Equals(OtherModes[i]) then exit;
  if not SharedMatrixOptions.Equals(OtherModes.SharedMatrixOptions) then exit;
  if not SessionMatrixOptions.Equals(OtherModes.SessionMatrixOptions) then exit;
  Result:=false;
end;

procedure TProjectBuildModes.Assign(Source: TPersistent; WithModified: boolean);
var
  OtherModes: TProjectBuildModes;
  i: Integer;
  CurMode: TProjectBuildMode;
begin
  if Source is TProjectBuildModes then begin
    FAssigning:=True;
    OtherModes:=TProjectBuildModes(Source);
    Clear;
    for i:=0 to OtherModes.Count-1 do
    begin
      CurMode:=Add(OtherModes[i].Identifier);
      CurMode.Assign(OtherModes[i]);
      if WithModified then
        CurMode.Modified:=OtherModes[i].Modified;
    end;
    SharedMatrixOptions.Assign(OtherModes.SharedMatrixOptions);
    SessionMatrixOptions.Assign(OtherModes.SessionMatrixOptions);
    ManyBuildModes.Assign(OtherModes.ManyBuildModes);
    ChangedHandlers.Assign(OtherModes.ChangedHandlers);
    if WithModified then
      Modified:=OtherModes.Modified;
    FAssigning:=False;
  end else
    inherited Assign(Source);
end;

procedure TProjectBuildModes.Delete(Index: integer);
var
  Item: TProjectBuildMode;
begin
  Item:=Items[Index];
  fItems.Delete(Index);
  Item.Free;
  {$IFDEF VerboseIDEModified}
  debugln(['TProjectBuildModes.Delete ']);
  {$ENDIF}
  IncreaseChangeStamp;
end;

function TProjectBuildModes.IndexOf(Identifier: string): integer;
begin
  Result:=Count-1;
  while (Result>=0)
  and (SysUtils.CompareText(Identifier,Items[Result].Identifier)<>0) do
    dec(Result);
end;

function TProjectBuildModes.IndexOf(aMode: TProjectBuildMode): integer;
begin
  Result:=fItems.IndexOf(aMode);
end;

function TProjectBuildModes.Find(Identifier: string): TProjectBuildMode;
var
  i: LongInt;
begin
  i:=IndexOf(Identifier);
  if i>=0 then
    Result:=Items[i]
  else
    Result:=nil;
end;

function TProjectBuildModes.Add(Identifier: string): TProjectBuildMode;
begin
  Result:=TProjectBuildMode.Create(Self);
  Result.FIdentifier:=Identifier;
  if LazProject<>nil then
    Result.CompilerOptions.BaseDirectory:=LazProject.Directory;
  Result.AddOnChangedHandler(@ItemChanged);
  fItems.Add(Result);
end;

procedure TProjectBuildModes.Move(FromIndex, ToIndex: integer);
begin
  fItems.Move(FromIndex,ToIndex);
end;

function TProjectBuildModes.Count: integer;
begin
  Result:=fItems.Count;
end;

procedure TProjectBuildModes.IncreaseChangeStamp;
begin
  LUIncreaseChangeStamp(FChangeStamp);
  if fChangedHandlers<>nil then fChangedHandlers.CallNotifyEvents(Self);
end;

procedure TProjectBuildModes.AddOnChangedHandler(const Handler: TNotifyEvent);
begin
  fChangedHandlers.Add(TMethod(Handler));
end;

procedure TProjectBuildModes.RemoveOnChangedHandler(const Handler: TNotifyEvent);
begin
  fChangedHandlers.Remove(TMethod(Handler));
end;

function TProjectBuildModes.IsModified(InSession: boolean): boolean;
var
  i: Integer;
begin
  Result:=true;
  if InSession then begin
    if SessionMatrixOptions.Modified then exit;
  end else begin
    if SharedMatrixOptions.Modified then exit;
  end;
  for i:=0 to Count-1 do
    if (Items[i].InSession=InSession) and Items[i].Modified then
      exit;
  Result:=false;
end;

function TProjectBuildModes.GetSessionModes: TStringList;
var
  i: Integer;
  BuildMode: TProjectBuildMode;
begin
  Result:=TStringList.Create;
  for i:=0 to Count-1 do begin
    BuildMode:=Items[i];
    if BuildMode.InSession then
      Result.Add(BuildMode.Identifier);
  end;
end;

function TProjectBuildModes.IsSessionMode(const ModeIdentifier: string): boolean;
var
  i: Integer;
  BuildMode: TProjectBuildMode;
begin
  for i:=0 to Count-1 do begin
    BuildMode:=Items[i];
    if SysUtils.CompareText(BuildMode.Identifier,ModeIdentifier)=0 then
      exit(BuildMode.InSession);
  end;
  Result:=false;
end;

function TProjectBuildModes.IsSharedMode(const ModeIdentifier: string): boolean;
var
  i: Integer;
  BuildMode: TProjectBuildMode;
begin
  for i:=0 to Count-1 do begin
    BuildMode:=Items[i];
    if SysUtils.CompareText(BuildMode.Identifier,ModeIdentifier)=0 then
      exit(not BuildMode.InSession);
  end;
  Result:=false;
end;

procedure TProjectBuildModes.RenameMatrixMode(const OldName, NewName: string);
begin
  SharedMatrixOptions.RenameMode(OldName,NewName);
  SessionMatrixOptions.RenameMode(OldName,NewName);
end;

function TProjectBuildModes.CreateExtraModes(aCurMode: TProjectBuildMode): TProjectBuildMode;
// Create Debug and Release buildmodes. Return the created debug mode.
// Params: aCurMode - existing mode to copy settings from.

  procedure AssignAndSetBooleans(aMode: TProjectBuildMode; IsDebug: Boolean);
  begin
    // Clone from current mode
    if Assigned(aCurMode) then
      aMode.Assign(aCurMode);
    // Change only some options
    with aMode.CompilerOptions do
    begin
      // Smart linking
      SmartLinkUnit:=not IsDebug;
      LinkSmart:=not IsDebug;
      // Checks
      IOChecks:=IsDebug;
      RangeChecks:=IsDebug;
      OverflowChecks:=IsDebug;
      StackChecks:=IsDebug;
      IncludeAssertionCode:=IsDebug;
      VerifyObjMethodCall:=IsDebug;
      // Debug flags
      GenerateDebugInfo:=IsDebug;
      if not IsDebug then
        UseExternalDbgSyms := False;
      RunWithoutDebug:=not IsDebug;
      UseHeaptrc:=IsDebug;
      TrashVariables:=IsDebug;
      StripSymbols:=not IsDebug;
    end;
  end;

var
  RelMode: TProjectBuildMode;
begin
  // Create Debug mode
  Result:=Add(DebugModeName);
  AssignAndSetBooleans(Result, True);
  Result.CompilerOptions.OptimizationLevel:=1;    // Low optimization
  Result.CompilerOptions.DebugInfoType:=dsDwarf3; // Debug
  // Create Release mode
  RelMode:=Add(ReleaseModeName);
  AssignAndSetBooleans(RelMode, False);
  RelMode.CompilerOptions.OptimizationLevel:=3;  // High but safe optimization, -O4 is dangerous
  RelMode.CompilerOptions.DebugInfoType:=dsAuto; // No Debug
  RelMode.CompilerOptions.UseValgrind:=false;
  RelMode.CompilerOptions.GenGProfCode:=false;
end;

// Methods for LoadFromXMLConfig

procedure TProjectBuildModes.AddMatrixMacro(const MacroName, MacroValue, ModeIdentifier: string;
  InSession: boolean);
var
  MatrixOptions: TBuildMatrixOptions;
  MatrixOption: TBuildMatrixOption;
begin
  MatrixOption:=SharedMatrixOptions.FindMacro(MacroName,MacroValue);
  if MatrixOption=nil then
    MatrixOption:=SessionMatrixOptions.FindMacro(MacroName,MacroValue);
  if MatrixOption<>nil then begin
    // Macro already exists => enable mode for this macro
    MatrixOption.EnableMode(ModeIdentifier);
  end else begin
    // Macro does not yet exist => create
    if InSession then
      MatrixOptions:=SessionMatrixOptions
    else
      MatrixOptions:=SharedMatrixOptions;
    MatrixOption:=MatrixOptions.Add(bmotIDEMacro,'*');
    MatrixOption.MacroName:=MacroName;
    MatrixOption.Value:=MacroValue;
    MatrixOption.Modes:=ModeIdentifier;
  end;
end;

procedure TProjectBuildModes.LoadSessionEnabledNonSessionMatrixOptions(const Path: string);
var
  i, Cnt: integer;
  SubPath: String;
  ModeID, OptionID: String;
begin
  // disable all matrix options in session modes
  if FGlobalMatrixOptions<>nil then
    FGlobalMatrixOptions.DisableModes(@IsSessionMode);
  SharedMatrixOptions.DisableModes(@IsSessionMode);
  // load
  Cnt:=FXMLConfig.GetValue(Path+'Count',0);
  for i:=1 to Cnt do begin
    SubPath:=Path+'Item'+IntToStr(i)+'/';
    ModeID:=FXMLConfig.GetValue(SubPath+'Mode','');
    if (ModeID='') or (not IsSessionMode(ModeID)) then begin
      debugln(['LoadSessionEnabledNonSessionMatrixOptions not a session Mode="',dbgstr(ModeID),'" at ',SubPath]);
      continue;
    end;
    OptionID:=FXMLConfig.GetValue(SubPath+'Option','');
    if OptionID='' then begin
      debugln(['LoadSessionEnabledNonSessionMatrixOptions invalid option at ',SubPath]);
      continue;
    end;
    if Assigned(FGlobalMatrixOptions) then
      FGlobalMatrixOptions.EnableModeIfOptionFound(ModeID, OptionID);
    if Assigned(SharedMatrixOptions) then
      SharedMatrixOptions.EnableModeIfOptionFound(ModeID, OptionID);
  end;
end;

procedure TProjectBuildModes.LoadOtherCompilerOpts(const Path: string;
  FromIndex, ToIndex: Integer; InSession: boolean);
// Iterate rest of the modes.
var
  i: Integer;
  Ident, SubPath: String;
  CurMode: TProjectBuildMode;
  LegacyList: Boolean;
begin
  LegacyList := FXMLConfig.IsLegacyList(Path);
  for i:=FromIndex to ToIndex do
  begin
    SubPath:=Path+FXMLConfig.GetListItemXPath('Item', i-1, LegacyList, True)+'/';
    Ident:=FXMLConfig.GetValue(SubPath+'Name','');
    CurMode:=Add(Ident);                     // add another mode
    CurMode.InSession:=InSession;
    CurMode.CompilerOptions.LoadFromXMLConfig(FXMLConfig, SubPath+'CompilerOptions/');
  end;
end;

procedure TProjectBuildModes.LoadMacroValues(const Path: string; CurMode: TProjectBuildMode);
var
  i, Cnt: Integer;
  SubPath, MacroName, MacroValue: String;
begin
  // load macro values of old IDE (<1.1)
  Cnt:=FXMLConfig.GetValue(Path+'Count',0);
  //debugln(['LoadMacroValues Cnt=',Cnt]);
  for i:=1 to Cnt do begin
    SubPath:=Path+'Macro'+IntToStr(i)+'/';
    MacroName:=FXMLConfig.GetValue(SubPath+'Name','');
    if not IsValidIdent(MacroName) then continue;
    MacroValue:=FXMLConfig.GetValue(SubPath+'Value','');
    //debugln(['LoadMacroValues Mode="',CurMode.Identifier,'" ',MacroName,'="',MacroValue,'" session=',CurMode.InSession]);
    AddMatrixMacro(MacroName,MacroValue,CurMode.Identifier,CurMode.InSession);
  end;
end;

procedure TProjectBuildModes.LoadAllMacroValues(const Path: string; Cnt: Integer);
var
  i: Integer;
  SubPath: String;
  IsLegacyList: Boolean;
begin
  // First default mode.
  LoadMacroValues(Path+'MacroValues/', Items[0]);
  IsLegacyList := FXMLConfig.IsLegacyList(Path+'BuildModes/');
  // Iterate rest of the modes.
  for i:=2 to Cnt do
  begin
    SubPath:=Path+'BuildModes/'+FXMLConfig.GetListItemXPath('Item', i-1, IsLegacyList, True);
    LoadMacroValues(SubPath+'MacroValues/', Items[i-1]);
  end;
end;

procedure TProjectBuildModes.LoadOldFormat(const Path: string);
var
  Ident, CompOptsPath, MacroValsPath: String;
  CurMode: TProjectBuildMode;
begin
  // no build modes => an old file format
  CompOptsPath:='CompilerOptions/';
  // due to a bug in an old version, the XML path can be 'CompilerOptions/' or ''
  if (LazProject.FileVersion<3)
  and (FXMLConfig.GetValue('SearchPaths/CompilerPath/Value','')<>'') then
    CompOptsPath:='';
  MacroValsPath:=Path+'MacroValues/';
  CurMode:=Items[0];
  LoadMacroValues(MacroValsPath,CurMode);
  if FXMLConfig.GetValue(CompOptsPath+'Version/Value', 0)<10 then begin
    // LCLWidgetType was not a macro but a property of its own
    Ident := FXMLConfig.GetValue(CompOptsPath+'LCLWidgetType/Value', '');
    if (Ident<>'') and (SysUtils.CompareText(Ident,'default')<>0) then
      AddMatrixMacro('LCLWidgetType',Ident,'default',false);
  end;
  CurMode.CompilerOptions.LoadFromXMLConfig(FXMLConfig,CompOptsPath);
end;

procedure TProjectBuildModes.LoadActiveBuildMode(const Path: string);
var
  CurMode: TProjectBuildMode;
begin
  CurMode:=Find(FXMLConfig.GetValue(Path+'BuildModes/Active','default'));
  if CurMode=nil then
    CurMode:=Items[0];
  LazProject.ActiveBuildModeID:=CurMode.Identifier;
  // Many BuildModes selection, a comma separated list.
  FManyBuildModes.CommaText:=FXMLConfig.GetValue(Path+'ManyBuildModesSelection/Value','');
end;

procedure TProjectBuildModes.LoadProjOptsFromXMLConfig(XMLConfig: TXMLConfig; const Path: string);
// Load for project
var
  Cnt: Integer;
  IsLegacyList: Boolean;
begin
  FXMLConfig := XMLConfig;

  IsLegacyList := FXMLConfig.IsLegacyList(Path+'BuildModes/');
  Cnt:=FXMLConfig.GetListItemCount(Path+'BuildModes/', 'Item', IsLegacyList);
  if Cnt>0 then begin
    // Project default mode is stored at the old XML path for backward compatibility.
    // Testing the 'Default' XML attribute is not needed because the first mode
    // is always default.
    Items[0].Identifier:=FXMLConfig.GetValue(Path+'BuildModes/'+XMLConfig.GetListItemXPath('Item', 0, IsLegacyList, True)+'/Name', '');
    Items[0].CompilerOptions.LoadFromXMLConfig(FXMLConfig, 'CompilerOptions/');
    LoadOtherCompilerOpts(Path+'BuildModes/', 2, Cnt, False);
    LoadAllMacroValues(Path+'MacroValues/', Cnt);
  end
  else
    LoadOldFormat(Path);

  LoadActiveBuildMode(Path);
end;

procedure TProjectBuildModes.LoadSessionFromXMLConfig(XMLConfig: TXMLConfig;
  const Path: string; LoadAllOptions: boolean);
// Load for session
var
  Cnt: Integer;
  IsLegacyList: Boolean;
begin
  FXMLConfig := XMLConfig;

  if LoadAllOptions then
    // load matrix options
    SessionMatrixOptions.LoadFromXMLConfig(FXMLConfig, Path+'BuildModes/SessionMatrixOptions/');

  IsLegacyList := FXMLConfig.IsLegacyList(Path+'BuildModes/');
  Cnt:=FXMLConfig.GetListItemCount(Path+'BuildModes/', 'Item', IsLegacyList);
  if Cnt>0 then begin
    // Add a new mode for session compiler options.
    LoadOtherCompilerOpts(Path+'BuildModes/', 1, Cnt, True);
    LoadAllMacroValues(Path+'MacroValues/', Cnt);
  end;

  if LoadAllOptions then
    // load what matrix options are enabled in session build modes
    LoadSessionEnabledNonSessionMatrixOptions(Path+'BuildModes/SessionEnabledMatrixOptions/');

  LoadActiveBuildMode(Path);
end;

// Methods for SaveToXMLConfig

procedure TProjectBuildModes.SaveSessionData(const Path: string);
var
  SubPath: String;
  i, Cnt: Integer;
begin
  // Many BuildModes selection, a comma separated list.
  FXMLConfig.SetDeleteValue(Path+'ManyBuildModesSelection/Value', FManyBuildModes.CommaText, '');
  // save what mode is currently active in the session
  FXMLConfig.SetDeleteValue(Path+'BuildModes/Active',LazProject.ActiveBuildModeID,'default');
  // save matrix options of session
  SessionMatrixOptions.SaveToXMLConfig(FXMLConfig, Path+'BuildModes/SessionMatrixOptions/',nil);

  // save what matrix options are enabled in session build modes
  Cnt:=0;
  SubPath:=Path+'BuildModes/SessionEnabledMatrixOptions/';
  for i:=0 to Count-1 do
    if Items[i].InSession then
      SharedMatrixOptions.SaveSessionEnabled(FXMLConfig, SubPath, Items[i].Identifier, Cnt);
  if Assigned(FGlobalMatrixOptions) then
    for i:=0 to Count-1 do
      if Items[i].InSession then
        FGlobalMatrixOptions.SaveSessionEnabled(FXMLConfig, SubPath, Items[i].Identifier, Cnt);
  FXMLConfig.SetDeleteValue(SubPath+'Count',Cnt,0);
end;

procedure TProjectBuildModes.SaveSharedMatrixOptions(const Path: string);
begin
  SharedMatrixOptions.SaveToXMLConfig(FXMLConfig, Path+'BuildModes/SharedMatrixOptions/',@IsSharedMode);
end;

function TProjectBuildModes.GetLazBuildModes(Index: integer): TLazProjectBuildMode;
begin
  Result:=TLazProjectBuildMode(fItems[Index]);
end;

// SaveToXMLConfig itself
procedure TProjectBuildModes.SaveProjOptsToXMLConfig(XMLConfig: TXMLConfig;
  const Path: string; SaveSession, ALegacyList: boolean);
var
  i, Cnt: Integer;
begin
  FXMLConfig := XMLConfig;
  // Save the default mode under an old xml path to let old IDEs open new projects
  // Note: the 0.9.29 reader already supports fetching the default build
  //       mode from the BuildModes, so in one or two releases we can switch
  //Items[0].SaveDefaultCompilerOpts(FXMLConfig, Path);
  Items[0].SaveMacroValuesAtOldPlace(XMLConfig,Path+'MacroValues/');
  Items[0].CompilerOptions.SaveToXMLConfig(XMLConfig,'CompilerOptions/'); // no Path!

  Cnt:=0;
  for i:=0 to Count-1 do
    if SaveSession or not Items[i].InSession then
      Items[i].SaveToXMLConfig(FXMLConfig, Path, i=0, ALegacyList, Cnt);
  FXMLConfig.SetListItemCount(Path+'BuildModes/',Cnt,ALegacyList);
end;

procedure TProjectBuildModes.SaveSessionOptsToXMLConfig(XMLConfig: TXMLConfig;
  const Path: string; SaveSession, ALegacyList: boolean);
var
  i, Cnt: Integer;
begin
  FXMLConfig := XMLConfig;
  Cnt:=0;
  for i:=0 to Count-1 do
    if Items[i].InSession and SaveSession then
      Items[i].SaveToXMLConfig(FXMLConfig, Path, false, ALegacyList, Cnt);
  FXMLConfig.SetListItemCount(Path+'BuildModes/',Cnt,ALegacyList);
end;


end.

