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

}
unit ModeMatrixOpts;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, contnrs,
  // LazUtils
  LazConfigStorage, Laz2_XMLCfg, LazLoggerBase, LazUTF8,
  // Codetools
  FileProcs, KeywordFuncLists, CodeToolsCfgScript,
  // IDE
  LazarusIDEStrConsts;

const
  BuildMatrixProjectName = '#project';
  BuildMatrixIDEName = '#ide';
type
  TBuildMatrixOptionType = (
    bmotCustom,  // append fpc parameters in Value
    bmotOutDir,  // override output directory -FU of target
    bmotIDEMacro // MacroName and Value
    );
  TBuildMatrixOptionTypes = set of TBuildMatrixOptionType;

const
  BuildMatrixOptionTypeNames: array[TBuildMatrixOptionType] of string = (
    'Custom',
    'OutDir',
    'IDEMacro'
    );

type
  TBuildMatrixGroupType = (
    bmgtEnvironment,
    bmgtProject,
    bmgtSession
    );
  TBuildMatrixGroupTypes = set of TBuildMatrixGroupType;
const
  bmgtAll = [low(TBuildMatrixGroupType)..high(TBuildMatrixGroupType)];

type
  TStrToBoolEvent = function(const Identifier: string): boolean of object;

  TBuildMatrixOptions = class;

  { TBuildMatrixOption }

  TBuildMatrixOption = class(TPersistent)
  private
    FID: string;
    FList: TBuildMatrixOptions;
    FMacroName: string;
    FModes: string;
    FTargets: string;
    FTyp: TBuildMatrixOptionType;
    FValue: string;
    procedure SetMacroName(AValue: string);
    procedure SetModes(AValue: string);
    procedure SetTargets(AValue: string);
    procedure SetTyp(AValue: TBuildMatrixOptionType);
    procedure SetValue(AValue: string);
  public
    procedure Assign(Source: TPersistent); override;
    constructor Create(aList: TBuildMatrixOptions);
    destructor Destroy; override;
    function FitsTarget(const Target: string): boolean;
    function FitsMode(const Mode: string): boolean;
    property List: TBuildMatrixOptions read FList;
    property ID: string read FID write FID;
    property Targets: string read FTargets write SetTargets;
    property Modes: string read FModes write SetModes; // modes separated by line breaks, case insensitive
    property Typ: TBuildMatrixOptionType read FTyp write SetTyp;
    property MacroName: string read FMacroName write SetMacroName;
    property Value: string read FValue write SetValue;
    function Equals(Obj: TObject): boolean; override;
    function GetModesSeparatedByComma(const SaveModes: TStrToBoolEvent): string;
    procedure SetModesFromCommaSeparatedList(aList: string);
    procedure DisableModes(const DisableModeEvent: TStrToBoolEvent);
    procedure EnableMode(aMode: string);
    procedure RenameMode(const OldMode, NewMode: string);
    procedure LoadFromConfig(Cfg: TConfigStorage);
    procedure SaveToConfig(Cfg: TConfigStorage; const SaveModes: TStrToBoolEvent);
    procedure LoadFromXMLConfig(Cfg: TXMLConfig; const aPath: string);
    procedure SaveToXMLConfig(Cfg: TXMLConfig; const aPath: string; const SaveModes: TStrToBoolEvent);
    function AsString: string;
  end;

  { TBuildMatrixOptions }

  TBuildMatrixOptions = class(TPersistent)
  private
    FChangeStep: int64;
    fSavedChangeStep: int64;
    fClearing: boolean;
    fItems: TObjectList; // list of TBuildMatrixOption
    FOnChanged: TNotifyEvent;
    FOnChangesd: TNotifyEvent;
    function GetItems(Index: integer): TBuildMatrixOption;
    function GetModified: boolean;
    procedure SetModified(AValue: boolean);
  public
    procedure Assign(Source: TPersistent); override;
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    function Count: integer;
    property Items[Index: integer]: TBuildMatrixOption read GetItems; default;
    function IndexOf(Option: TBuildMatrixOption): integer;
    function Add(Typ: TBuildMatrixOptionType = bmotCustom; Targets: string = '*'): TBuildMatrixOption;
    procedure Delete(Index: integer);
    procedure DisableModes(const IsModeEvent: TStrToBoolEvent);
    procedure RenameMode(const OldMode, NewMode: string);

    // equals, modified
    property ChangeStep: int64 read FChangeStep;
    procedure IncreaseChangeStep;
    function Equals(Obj: TObject): boolean; override;
    property OnChanged: TNotifyEvent read FOnChanged write FOnChangesd;
    property Modified: boolean read GetModified write SetModified;

    // load, save
    procedure LoadFromConfig(Cfg: TConfigStorage);
    procedure SaveToConfig(Cfg: TConfigStorage; const SaveMode: TStrToBoolEvent);
    procedure LoadFromXMLConfig(Cfg: TXMLConfig; const aPath: string);
    procedure SaveToXMLConfig(Cfg: TXMLConfig; const aPath: string;
      const SaveMode: TStrToBoolEvent);
    function SaveAtOldXMLConfig(Cfg: TXMLConfig; const Path, ModeIdent: string): integer;
    procedure SaveSessionEnabled(Cfg: TXMLConfig; const Path, ModeIdent: string; var Cnt: integer);

    // queries
    procedure AppendCustomOptions(Target, ActiveMode: string; var Options: string);
    procedure GetOutputDirectory(Target, ActiveMode: string; var OutDir: string);
    function FindOption(const ID: string): TBuildMatrixOption;
    function FindMacro(const MacroName, MacroValue: string): TBuildMatrixOption;
    procedure EnableModeIfOptionFound(ModeID, OptionID: String);
  end;

  EMMMacroSyntaxException = class(Exception)
  end;


function BuildMatrixTargetFits(Target, Targets: string): boolean;
function BuildMatrixTargetFitsPattern(Target, Pattern: PChar): boolean;
function CheckBuildMatrixTargetsSyntax(const Targets: String): String;
function BuildMatrixModeFits(Mode, ModesSeparatedByLineBreaks: string): boolean;
function Str2BuildMatrixOptionType(const s: string): TBuildMatrixOptionType;
function CreateBuildMatrixOptionGUID: string;

function SplitMatrixMacro(MacroAssignment: string;
  out MacroName, MacroValue: string; ExceptionOnError: boolean): boolean;
procedure ApplyBuildMatrixMacros(Options: TBuildMatrixOptions; Target, ActiveMode: string;
  CfgVars: TCTCfgScriptVariables);

implementation

function BuildMatrixTargetFits(Target, Targets: string): boolean;
{ case insensitive
  * = all
  a = fits a and A
  a* = fits all starting with a
  a? = fits all two letter names starting with a

  Comma and minus:
    Fits if there is at least one positive match and no negative match
  a,b = fits a or b
  -a = if target is a, stop immediately with 'false'
  -ab,a* = fits all beginning with a except for ab
  a*,-ab = fits all beginning with a, the -ab is ignored
}
var
  p: PChar;
  Negated: Boolean;
begin
  Result:=false;
  if (Targets='') or (Target='') then exit;
  p:=PChar(Targets);
  repeat
    if p^='-' then begin
      Negated:=true;
      inc(p);
    end else
      Negated:=false;
    if BuildMatrixTargetFitsPattern(PChar(Target),p) then begin
      if Negated then begin
        exit(false);
      end else begin
        Result:=true;
      end;
    end;
    while not (p^ in [',',#0]) do
      inc(p);
    while p^=',' do
      inc(p);
  until p^=#0;
end;

function BuildMatrixTargetFitsPattern(Target, Pattern: PChar): boolean;
// Pattern ends at #0 or comma
// ? means one arbitrary character
// * means any arbitrary characters, even none
begin
  Result:=false;
  if (Target=nil) or (Target^=#0) or (Pattern=nil) or (Pattern^ in [#0,',']) then
    exit;
  repeat
    case Pattern^ of
    #0,',':
      begin
        // end of pattern reached
        Result:=Target^=#0;
        exit;
      end;
    '?':
      begin
        // one arbitrary character
        if Target^=#0 then
          exit;
        inc(Pattern);
        inc(Target);
      end;
    '*':
      begin
        repeat
          inc(Pattern);
        until Pattern^<>'*';
        if Pattern^ in [#0,','] then
          exit(true);
        // behind the * comes a none * => check recursively all combinations
        while Target^<>#0 do begin
          if BuildMatrixTargetFitsPattern(Target,Pattern) then
            exit(true);
          inc(Target);
        end;
        exit;
      end;
    'a'..'z','A'..'Z':
      begin
        if UpChars[Pattern^]<>UpChars[Target^] then
          exit;
        inc(Pattern);
        inc(Target)
      end;
    else
      if Pattern^<>Target^ then
        exit;
      inc(Pattern);
      inc(Target);
    end;
  until false;
end;

function CheckBuildMatrixTargetsSyntax(const Targets: String): String;
var
  p: PChar;

  procedure WarnInvalidChar;
  begin
    Result:=Format(lisMMInvalidCharacterAt, [dbgstr(p^), IntToStr(p-PChar(
      Targets)+1)]);
  end;

begin
  Result:='';
  if Targets='' then exit;
  p:=PChar(Targets);
  repeat
    case p^ of
    #0:
      if p-PChar(Targets)=length(Targets) then
        break
      else begin
        WarnInvalidChar;
        exit;
      end;
    #1..#32,#127:
      begin
        WarnInvalidChar;
        exit;
      end;
    end;
    inc(p);
  until false;
end;

function BuildMatrixModeFits(Mode, ModesSeparatedByLineBreaks: string): boolean;
var
  p: PChar;
  m: PChar;
begin
  Result:=false;
  if Mode='' then exit;
  if ModesSeparatedByLineBreaks='' then exit;
  p:=PChar(ModesSeparatedByLineBreaks);
  while p^<>#0 do begin
    while p^ in [#1..#31] do inc(p);
    m:=PChar(Mode);
    while (UpChars[p^]=UpChars[m^]) and (p^>=' ') do begin
      inc(p);
      inc(m);
    end;
    if (m^=#0) and (p^ in [#10,#13,#0]) then
      exit(true);
    while p^>=' ' do inc(p);
  end;
end;

function Str2BuildMatrixOptionType(const s: string): TBuildMatrixOptionType;
begin
  for Result:=low(TBuildMatrixOptionType) to high(TBuildMatrixOptionType) do
    if SysUtils.CompareText(BuildMatrixOptionTypeNames[Result],s)=0 then exit;
  Result:=bmotCustom;
end;

function CreateBuildMatrixOptionGUID: string;
var
  i: Integer;
begin
  SetLength(Result,12);
  for i:=1 to length(Result) do
    Result[i]:=chr(ord('0')+random(10));
end;

function SplitMatrixMacro(MacroAssignment: string; out MacroName,
  MacroValue: string; ExceptionOnError: boolean): boolean;

  procedure E(Msg: string);
  begin
    raise EMMMacroSyntaxException.Create(Msg);
  end;

var
  p: PChar;
  StartP: PChar;
begin
  Result:=false;
  MacroName:='';
  MacroValue:='';
  if MacroAssignment='' then begin
    if ExceptionOnError then
      E(lisMMMissingMacroName);
    exit;
  end;
  p:=PChar(MacroAssignment);
  if not IsIdentStartChar[p^] then begin
    if ExceptionOnError then
      E(Format(lisMMExpectedMacroNameButFound, [dbgstr(p^)]));
    exit;
  end;
  StartP:=p;
  repeat
    inc(p);
  until not IsIdentChar[p^];
  MacroName:=copy(MacroAssignment,1,p-StartP);
  if (p^<>':') or (p[1]<>'=') then begin
    if ExceptionOnError then
      E(Format(lisMMExpectedAfterMacroNameButFound, [dbgstr(p^)]));
    exit;
  end;
  inc(p,2);
  StartP:=p;
  repeat
    if (p^=#0) and (p-PChar(MacroAssignment)=length(MacroAssignment)) then break;
    if p^ in [#0..#31,#127] then begin
      if ExceptionOnError then
        E(Format(lisMMInvalidCharacterInMacroValue, [dbgstr(p^)]));
      exit;
    end;
    inc(p);
  until false;
  MacroValue:=copy(MacroAssignment,StartP-PChar(MacroAssignment)+1,p-StartP);
  Result:=true;
end;

procedure ApplyBuildMatrixMacros(Options: TBuildMatrixOptions;
  Target, ActiveMode: string; CfgVars: TCTCfgScriptVariables);
var
  i: Integer;
  Option: TBuildMatrixOption;
begin
  if (Options=nil) or (CfgVars=nil) then exit;
  for i:=0 to Options.Count-1 do begin
    Option:=Options[i];
    if Option.Typ<>bmotIDEMacro then continue;
    if not Option.FitsMode(ActiveMode) then continue;
    if not Option.FitsTarget(Target) then continue;
    //debugln(['ApplyBuildMatrixMacros Option.MacroName="',Option.MacroName,'" Value="',Option.Value,'"']);
    CfgVars.Values[Option.MacroName]:=Option.Value;
  end;
end;

{ TBuildMatrixOptions }

function TBuildMatrixOptions.GetItems(Index: integer): TBuildMatrixOption;
begin
  Result:=TBuildMatrixOption(fItems[Index]);
end;

function TBuildMatrixOptions.GetModified: boolean;
begin
  Result:=fSavedChangeStep<>FChangeStep;
end;

procedure TBuildMatrixOptions.SetModified(AValue: boolean);
begin
  if AValue then
    IncreaseChangeStep
  else
    fSavedChangeStep:=FChangeStep;
end;

procedure TBuildMatrixOptions.Assign(Source: TPersistent);
var
  aSource: TBuildMatrixOptions;
  i: Integer;
  Item: TBuildMatrixOption;
begin
  if Source is TBuildMatrixOptions then
  begin
    aSource:=TBuildMatrixOptions(Source);
    Clear;
    for i:=0 to aSource.Count-1 do begin
      Item:=TBuildMatrixOption.Create(Self);
      Item.Assign(aSource[i]);
    end;
  end else
    inherited Assign(Source);
end;

constructor TBuildMatrixOptions.Create;
begin
  FChangeStep:=CTInvalidChangeStamp64;
  fItems:=TObjectList.create(true);
end;

destructor TBuildMatrixOptions.Destroy;
begin
  Clear;
  FreeAndNil(fItems);
  inherited Destroy;
end;

procedure TBuildMatrixOptions.Clear;
begin
  if fItems.Count=0 then exit;
  fClearing:=true;
  fItems.Clear;
  fClearing:=false;
  IncreaseChangeStep;
end;

procedure TBuildMatrixOptions.DisableModes(const IsModeEvent: TStrToBoolEvent);
var
  i: Integer;
begin
  for i:=0 to Count-1 do
    Items[i].DisableModes(IsModeEvent);
end;

procedure TBuildMatrixOptions.RenameMode(const OldMode, NewMode: string);
var
  i: Integer;
begin
  for i:=0 to Count-1 do
    Items[i].RenameMode(OldMode,NewMode);
end;

function TBuildMatrixOptions.Count: integer;
begin
  Result:=fItems.Count;
end;

function TBuildMatrixOptions.IndexOf(Option: TBuildMatrixOption): integer;
begin
  Result:=fItems.IndexOf(Option);
end;

function TBuildMatrixOptions.Add(Typ: TBuildMatrixOptionType; Targets: string
  ): TBuildMatrixOption;
begin
  Result:=TBuildMatrixOption.Create(Self);
  Result.Targets:=Targets;
  Result.Typ:=Typ;
end;

procedure TBuildMatrixOptions.Delete(Index: integer);
begin
  Items[Index].Free;
end;

procedure TBuildMatrixOptions.IncreaseChangeStep;
begin
  CTIncreaseChangeStamp64(FChangeStep);
  if Assigned(OnChanged) then
    OnChanged(Self);
end;

function TBuildMatrixOptions.Equals(Obj: TObject): boolean;
var
  Src: TBuildMatrixOptions;
  i: Integer;
begin
  Result:=false;
  if Self=Obj then exit;
  if not (Obj is TBuildMatrixOptions) then exit;
  Src:=TBuildMatrixOptions(Obj);
  if Src.Count<>Count then exit;
  for i:=0 to Count-1 do
    if not Src[i].Equals(Items[i]) then exit;
  Result:=true;
end;

procedure TBuildMatrixOptions.LoadFromConfig(Cfg: TConfigStorage);
var
  Cnt: Integer;
  i: Integer;
  Option: TBuildMatrixOption;
begin
  Clear;
  Cnt:=Cfg.GetValue('Count',0);
  for i:=1 to Cnt do begin
    Option:=TBuildMatrixOption.Create(Self);
    Cfg.AppendBasePath('Item'+IntToStr(i));
    Option.LoadFromConfig(Cfg);
    Cfg.UndoAppendBasePath;
  end;
end;

procedure TBuildMatrixOptions.SaveToConfig(Cfg: TConfigStorage;
  const SaveMode: TStrToBoolEvent);
var
  i: Integer;
begin
  Cfg.SetDeleteValue('Count',Count,0);
  for i:=0 to Count-1 do begin
    Cfg.AppendBasePath('Item'+IntToStr(i+1));
    Items[i].SaveToConfig(Cfg,SaveMode);
    Cfg.UndoAppendBasePath;
  end;
end;

procedure TBuildMatrixOptions.LoadFromXMLConfig(Cfg: TXMLConfig;
  const aPath: string);
var
  Cnt: Integer;
  i: Integer;
  Option: TBuildMatrixOption;
begin
  Clear;
  Cnt:=Cfg.GetValue(aPath+'Count',0);
  //debugln(['TBuildMatrixOptions.LoadFromXMLConfig Cnt=',Cnt]);
  for i:=1 to Cnt do begin
    Option:=TBuildMatrixOption.Create(Self);
    Option.LoadFromXMLConfig(Cfg,aPath+'Item'+IntToStr(i)+'/');
  end;
  //debugln(['TBuildMatrixOptions.LoadFromXMLConfig Count=',Count]);
end;

procedure TBuildMatrixOptions.SaveToXMLConfig(Cfg: TXMLConfig;
  const aPath: string; const SaveMode: TStrToBoolEvent);
var
  i: Integer;
begin
  //debugln(['TBuildMatrixOptions.SaveToXMLConfig ',aPath]);
  Cfg.SetDeleteValue(aPath+'Count',Count,0);
  for i:=0 to Count-1 do
    Items[i].SaveToXMLConfig(Cfg,aPath+'Item'+IntToStr(i+1)+'/',SaveMode);
end;

function TBuildMatrixOptions.SaveAtOldXMLConfig(Cfg: TXMLConfig;
  const Path, ModeIdent: string): integer;
var
  i: Integer;
  MatrixOption: TBuildMatrixOption;
  SubPath: String;
begin
  Result:=0;
  for i:=0 to Count-1 do
  begin
    MatrixOption:=Items[i];
    if (MatrixOption.Typ=bmotIDEMacro)
    and MatrixOption.FitsTarget(BuildMatrixProjectName)
    and MatrixOption.FitsMode(ModeIdent) then
    begin
      inc(Result);
      SubPath:=Path+'Macro'+IntToStr(i+1)+'/';
      Cfg.SetDeleteValue(SubPath+'Name',MatrixOption.MacroName,'');
      Cfg.SetDeleteValue(SubPath+'Value',MatrixOption.Value,'');
    end;
  end;
end;

procedure TBuildMatrixOptions.SaveSessionEnabled(Cfg: TXMLConfig;
  const Path, ModeIdent: string; var Cnt: integer);
var
  MatrixOption: TBuildMatrixOption;
  SubPath: String;
  i: Integer;
begin
  for i:=0 to Count-1 do begin
    MatrixOption:=Items[i];
    //debugln(['SaveSessionEnabled ',MatrixOption.AsString]);
    if not MatrixOption.FitsMode(ModeIdent) then continue;
    inc(Cnt);
    SubPath:=Path+'Item'+IntToStr(Cnt)+'/';
    //debugln(['SaveSessionEnabled ModeID="',CurMode.Identifier,'" OptionID="',MatrixOption.ID,'" ',MatrixOption.AsString]);
    Cfg.SetDeleteValue(SubPath+'Mode',ModeIdent,'');
    Cfg.SetDeleteValue(SubPath+'Option',MatrixOption.ID,'');
  end;
end;

procedure TBuildMatrixOptions.AppendCustomOptions(Target, ActiveMode: string;
  var Options: string);
var
  i: Integer;
  Option: TBuildMatrixOption;
  Value: String;
begin
  for i:=0 to Count-1 do begin
    Option:=Items[i];
    if Option.Typ<>bmotCustom then continue;
    Value:=Trim(Option.Value);
    if Value='' then continue;
    if not Option.FitsTarget(Target) then continue;
    if not Option.FitsMode(ActiveMode) then continue;
    if Options<>'' then Options+=' ';
    Options+=Value;
  end;
end;

procedure TBuildMatrixOptions.GetOutputDirectory(Target, ActiveMode: string;
  var OutDir: string);
var
  i: Integer;
  Option: TBuildMatrixOption;
begin
  for i:=0 to Count-1 do begin
    Option:=Items[i];
    if Option.Typ<>bmotOutDir then continue;
    if not Option.FitsTarget(Target) then continue;
    if not Option.FitsMode(ActiveMode) then continue;
    OutDir:=Option.Value;
  end;
end;

function TBuildMatrixOptions.FindOption(const ID: string): TBuildMatrixOption;
var
  i: Integer;
begin
  for i:=0 to Count-1 do begin
    Result:=Items[i];
    if Result.ID=ID then exit;
  end;
  Result:=nil;
end;

function TBuildMatrixOptions.FindMacro(const MacroName, MacroValue: string): TBuildMatrixOption;
var
  i: Integer;
begin
  i:=Count-1;
  while i>=0 do
  begin
    Result:=Items[i];
    if (Result.Typ=bmotIDEMacro)
    and (Result.Targets='*')
    and (Result.MacroName=MacroName)
    and (Result.Value=MacroValue)
    then
      exit;
    dec(i);
  end;
  Result:=nil;
end;

procedure TBuildMatrixOptions.EnableModeIfOptionFound(ModeID, OptionID: String);
var
  Opt: TBuildMatrixOption;
begin
  Opt:=FindOption(OptionID);
  if Assigned(Opt) then
    Opt.EnableMode(ModeID);
end;

{ TBuildMatrixOption }

procedure TBuildMatrixOption.SetMacroName(AValue: string);
begin
  if FMacroName=AValue then Exit;
  FMacroName:=AValue;
  List.IncreaseChangeStep;
end;

procedure TBuildMatrixOption.SetModes(AValue: string);
begin
  if FModes=AValue then exit;
  FModes:=AValue;
  List.IncreaseChangeStep;
end;

procedure TBuildMatrixOption.SetTargets(AValue: string);
begin
  if FTargets=AValue then Exit;
  FTargets:=AValue;
  List.IncreaseChangeStep;
end;

procedure TBuildMatrixOption.SetTyp(AValue: TBuildMatrixOptionType);
begin
  if FTyp=AValue then Exit;
  FTyp:=AValue;
  List.IncreaseChangeStep;
end;

procedure TBuildMatrixOption.SetValue(AValue: string);
begin
  if FValue=AValue then Exit;
  FValue:=AValue;
  List.IncreaseChangeStep;
end;

procedure TBuildMatrixOption.Assign(Source: TPersistent);
var
  aSource: TBuildMatrixOption;
begin
  if Source is TBuildMatrixOption then
  begin
    aSource:=TBuildMatrixOption(Source);
    ID:=aSource.ID;
    Targets:=aSource.Targets;
    Modes:=aSource.Modes;
    Typ:=aSource.Typ;
    MacroName:=aSource.MacroName;
    Value:=aSource.Value;
  end else
    inherited Assign(Source);
end;

constructor TBuildMatrixOption.Create(aList: TBuildMatrixOptions);
begin
  FID:=CreateBuildMatrixOptionGUID;
  FList:=aList;
  if List<>nil then
    List.fItems.Add(Self);
end;

destructor TBuildMatrixOption.Destroy;
begin
  List.fItems.Remove(Self);
  FList:=nil;
  inherited Destroy;
end;

function TBuildMatrixOption.FitsTarget(const Target: string): boolean;
begin
  Result:=BuildMatrixTargetFits(Target,Targets);
end;

function TBuildMatrixOption.FitsMode(const Mode: string): boolean;
begin
  Result:=BuildMatrixModeFits(Mode,Modes);
end;

function TBuildMatrixOption.Equals(Obj: TObject): boolean;
var
  Src: TBuildMatrixOption;
begin
  Result:=false;
  if Obj=Self then exit;
  if not (Obj is TBuildMatrixOption) then exit;
  Src:=TBuildMatrixOption(Obj);
  if Src.Targets<>Targets then exit;
  if Src.Modes<>Modes then exit;
  if Src.Typ<>Typ then exit;
  if Src.MacroName<>MacroName then exit;
  if Src.Value<>Value then exit;
  Result:=true;
end;

function TBuildMatrixOption.GetModesSeparatedByComma(
  const SaveModes: TStrToBoolEvent): string;
var
  p, StartP: PChar;
  l: SizeInt;
  CurMode: string;
  i: Integer;
begin
  Result:='';
  if Modes='' then exit;
  p:=PChar(Modes);
  while p^<>#0 do begin
    StartP:=p;
    while not (p^ in [#0,#10,#13]) do inc(p);
    l:=p-StartP;
    while p^ in [#10,#13] do inc(p);
    if l=0 then continue; // skip empty strings
    SetLength(CurMode,l);
    System.Move(StartP^,CurMode[1],l);
    if Assigned(SaveModes) and not SaveModes(CurMode) then continue;
    // convert a single comma to double comma
    for i:=length(CurMode) downto 1 do
      if CurMode[i]=',' then
        System.Insert(',',CurMode,i);
    if Result<>'' then
      Result+=',';
    Result+=CurMode;
  end;
  //debugln(['TBuildMatrixOption.GetModesSeparatedByComma ',dbgstr(Modes),' -> ',dbgstr(Result)]);
end;

procedure TBuildMatrixOption.SetModesFromCommaSeparatedList(aList: string);
var
  p: Integer;
begin
  //debugln(['TBuildMatrixOption.SetModesFromCommaSeparatedList START aList=',aList]);
  p:=1;
  while p<=length(aList) do begin
    if aList[p]=',' then begin
      if (p<length(aList)) and (aList[p+1]=',') then begin
        // double comma is normal character = single comma
        system.Delete(aList,p,1);
        inc(p);
      end else begin
        // single comma is separator
        ReplaceSubstring(aList,p,1,LineEnding);
        inc(p,length(LineEnding));
      end;
    end else begin
      inc(p);
    end;
  end;
  Modes:=aList;
  //debugln(['TBuildMatrixOption.SetModesFromCommaSeparatedList END Modes=',dbgstr(Modes)]);
end;

procedure TBuildMatrixOption.DisableModes(const DisableModeEvent: TStrToBoolEvent);
var
  CurModes: String;
  p: PChar;
  StartP: PChar;
  CurMode: String;
  StartPos: integer;
begin
  CurModes:=Modes;
  p:=PChar(CurModes);
  while p^<>#0 do begin
    StartP:=p;
    while not (p^ in [#0,#10,#13]) do inc(p);
    StartPos:=StartP-PChar(CurModes)+1;
    CurMode:=copy(CurModes,StartPos,p-StartP);
    while p^ in [#10,#13] do inc(p);
    if DisableModeEvent(CurMode) then begin
      System.Delete(CurModes,StartPos,p-StartP);
      p:=Pointer(CurModes)+StartPos-1;
    end;
  end;
  Modes:=CurModes;
end;

procedure TBuildMatrixOption.EnableMode(aMode: string);
begin
  if FitsMode(aMode) then exit;
  if Modes<>'' then
    aMode:=LineEnding+aMode;
  Modes:=Modes+aMode;
end;

procedure TBuildMatrixOption.RenameMode(const OldMode, NewMode: string);
var
  CurModes: String;
  p: PChar;
  StartP: PChar;
  StartPos: SizeInt;
  CurMode: String;
begin
  CurModes:=Modes;
  p:=PChar(CurModes);
  while p^<>#0 do begin
    StartP:=p;
    while not (p^ in [#0,#10,#13]) do inc(p);
    StartPos:=StartP-PChar(CurModes)+1;
    CurMode:=copy(CurModes,StartPos,p-StartP);
    if CompareText(CurMode,OldMode)=0 then begin
      ReplaceSubstring(CurModes,StartPos,p-StartP,NewMode);
      p:=Pointer(CurModes)+StartPos-1+length(NewMode);
    end;
    while p^ in [#10,#13] do inc(p);
  end;
  Modes:=CurModes;
end;

procedure TBuildMatrixOption.LoadFromConfig(Cfg: TConfigStorage);
begin
  ID:=Cfg.GetValue('ID','');
  if ID='' then ID:=CreateBuildMatrixOptionGUID;
  Targets:=Cfg.GetValue('Targets','*');
  SetModesFromCommaSeparatedList(Cfg.GetValue('Modes',''));
  Typ:=Str2BuildMatrixOptionType(Cfg.GetValue('Type',''));
  MacroName:=Cfg.GetValue('MacroName','');
  Value:=Cfg.GetValue('Value','');
end;

procedure TBuildMatrixOption.SaveToConfig(Cfg: TConfigStorage;
  const SaveModes: TStrToBoolEvent);
begin
  Cfg.SetDeleteValue('ID',ID,'');
  Cfg.SetDeleteValue('Targets',Targets,'*');
  Cfg.SetDeleteValue('Modes',GetModesSeparatedByComma(SaveModes),'');
  Cfg.SetDeleteValue('Type',BuildMatrixOptionTypeNames[Typ],BuildMatrixOptionTypeNames[bmotCustom]);
  Cfg.SetDeleteValue('MacroName',MacroName,'');
  Cfg.SetDeleteValue('Value',Value,'');
end;

procedure TBuildMatrixOption.LoadFromXMLConfig(Cfg: TXMLConfig;
  const aPath: string);
begin
  ID:=Cfg.GetValue(aPath+'ID','');
  if ID='' then ID:=CreateBuildMatrixOptionGUID;
  Targets:=Cfg.GetValue(aPath+'Targets','*');
  SetModesFromCommaSeparatedList(Cfg.GetValue(aPath+'Modes',''));
  Typ:=Str2BuildMatrixOptionType(Cfg.GetValue(aPath+'Type',''));
  MacroName:=Cfg.GetValue(aPath+'MacroName','');
  Value:=Cfg.GetValue(aPath+'Value','');
end;

procedure TBuildMatrixOption.SaveToXMLConfig(Cfg: TXMLConfig;
  const aPath: string; const SaveModes: TStrToBoolEvent);
begin
  Cfg.SetDeleteValue(aPath+'ID',ID,'');
  Cfg.SetDeleteValue(aPath+'Targets',Targets,'*');
  Cfg.SetDeleteValue(aPath+'Modes',GetModesSeparatedByComma(SaveModes),'');
  Cfg.SetDeleteValue(aPath+'Type',BuildMatrixOptionTypeNames[Typ],BuildMatrixOptionTypeNames[bmotCustom]);
  Cfg.SetDeleteValue(aPath+'MacroName',MacroName,'');
  Cfg.SetDeleteValue(aPath+'Value',Value,'');
end;

function TBuildMatrixOption.AsString: string;
begin
  Result:='ID="'+ID+'" '+BuildMatrixOptionTypeNames[Typ]
    +' Value="'+Value+'"'
    +' Modes="'+dbgstr(Modes)+'"';
end;

end.

