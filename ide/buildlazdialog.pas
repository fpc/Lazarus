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
 *   Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.        *
 *                                                                         *
 ***************************************************************************

  Author: Mattias Gaertner
  Converted to lfm by: Matthijs Willemstein

  Abstract:
    Defines the TBuildLazarusOptions which stores the settings for the
    "Build Lazarus" function of the IDE.
    TConfigureBuildLazarusDlg is used to edit TBuildLazarusOptions.

    The BuildLazarus function will build the lazarus parts.
}
unit BuildLazDialog;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Math, LCLProc, Forms, Controls, LCLType, LCLIntf,
  Graphics, GraphType, StdCtrls, ExtCtrls, Buttons, FileUtil, Dialogs,
  LResources,  Laz_XMLCfg,
  IDEExternToolIntf,
  LazarusIDEStrConsts, TransferMacros, LazConf, IDEProcs, DialogProcs,
  IDEWindowIntf, InputHistory, ExtToolDialog, ExtToolEditDlg,
  CompilerOptions;

type
  { TBuildLazarusItem }

  TMakeMode = (
    mmNone,
    mmBuild,
    mmCleanBuild
    );
  TMakeModes = set of TMakeMode;

  TBuildLazarusFlag = (
    blfWithoutLinkingIDE, // skip linking stage of IDE
    blfOnlyIDE,           // skip all but IDE
    blfDontClean,         // ignore clean up
    blfWithStaticPackages,// build with IDE static design time packages
    blfUseMakeIDECfg
    );
  TBuildLazarusFlags = set of TBuildLazarusFlag;

  TBuildLazarusItem = class
  private
    FCommands: array[TMakeMode] of string;
    FDefaultMakeMode: TMakeMode;
    FDescription: string;
    FDirectory: string;
    FMakeMode: TMakeMode;
    FName: string;
    function GetCommands(Mode: TMakeMode): string;
    procedure SetCommands(Mode: TMakeMode; const AValue: string);
    procedure SetDefaultMakeMode(const AValue: TMakeMode);
    procedure SetDescription(const AValue: string);
    procedure SetDirectory(const AValue: string);
    procedure SetMakeMode(const AValue: TMakeMode);
    procedure SetName(const AValue: string);
  public
    constructor Create;
    constructor Create(const NewName, NewDescription, NewDirectory: string;
                       const NewMakeMode: TMakeMode);
    destructor Destroy; override;
    procedure Clear;
    procedure Assign(Source: TBuildLazarusItem);
    property Name: string read FName write SetName;
    property Description: string read FDescription write SetDescription;
    property Directory: string read FDirectory write SetDirectory;
    property MakeMode: TMakeMode read FMakeMode write SetMakeMode;
    property DefaultMakeMode: TMakeMode read FDefaultMakeMode write SetDefaultMakeMode;
    property Commands[Mode: TMakeMode]: string read GetCommands write SetCommands;
  end;


  { TBuildLazarusOptions }

  TBuildLazarusOptions = class
  private
    FCleanAll: boolean;
    FGlobals: TGlobalCompilerOptions;
    FItemCodeTools: TBuildLazarusItem;
    FItemExamples: TBuildLazarusItem;
    FItemIDE: TBuildLazarusItem;
    FItemIDEIntf: TBuildLazarusItem;
    FItemJITForm: TBuildLazarusItem;
    FItemLCL: TBuildLazarusItem;
    FItemPkgReg: TBuildLazarusItem;
    FItemStarter: TBuildLazarusItem;
    FItemSynEdit: TBuildLazarusItem;
    FExtraOptions: string;
    FRestartAfterBuild: boolean;
    FConfirmBuild: boolean;
    FTargetCPU: string;
    FTargetDirectory: string;
    fTargetOS: string;
    fLCLPlatform: TLCLPlatform;
    fStaticAutoInstallPackages: TStringList;
    FWithStaticPackages: boolean;
    FItems: TList; // list of TBuildLazarusItem
    function GetCount: integer;
    function GetItems(Index: integer): TBuildLazarusItem;
    procedure SetRestartAfterBuild(const AValue: boolean);
    procedure SetConfirmBuild(const AValue: boolean);
    procedure SetTargetCPU(const AValue: string);
    procedure SetTargetDirectory(const AValue: string);
    procedure SetTargetOS(const AValue: string);
    procedure SetWithStaticPackages(const AValue: boolean);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    procedure CreateDefaults;
    function IndexOf(Item: TBuildLazarusItem): integer;
    procedure Load(XMLConfig: TXMLConfig; const Path: string);
    procedure Save(XMLConfig: TXMLConfig; const Path: string);
    procedure Assign(Source: TBuildLazarusOptions);
    procedure SetBuildAll;
    function FindName(const Name: string): TBuildLazarusItem;
    function CompiledUnitExt(FPCVersion, FPCRelease: integer): string;
  public
    property Count: integer read GetCount;
    property Items[Index: integer]: TBuildLazarusItem read GetItems;
    property ItemLCL: TBuildLazarusItem read FItemLCL;
    property ItemSynEdit: TBuildLazarusItem read FItemSynEdit;
    property ItemCodeTools: TBuildLazarusItem read FItemCodeTools;
    property ItemPkgReg: TBuildLazarusItem read FItemPkgReg;
    property ItemIDEIntf: TBuildLazarusItem read FItemIDEIntf;
    property ItemJITForm: TBuildLazarusItem read FItemJITForm;
    property ItemIDE: TBuildLazarusItem read FItemIDE;
    property ItemStarter: TBuildLazarusItem read FItemStarter;
    property ItemExamples: TBuildLazarusItem read FItemExamples;
    property CleanAll: boolean read FCleanAll write FCleanAll;
    property ExtraOptions: string read FExtraOptions write FExtraOptions;
    property TargetOS: string read fTargetOS write SetTargetOS;
    property TargetCPU: string read FTargetCPU write SetTargetCPU;
    property LCLPlatform: TLCLPlatform read fLCLPlatform write fLCLPlatform;
    property StaticAutoInstallPackages: TStringList
                                                read fStaticAutoInstallPackages;
    property TargetDirectory: string read FTargetDirectory
                                     write SetTargetDirectory;
    property WithStaticPackages: boolean read FWithStaticPackages
                                         write SetWithStaticPackages;
    property RestartAfterBuild: boolean read FRestartAfterBuild
                                        write SetRestartAfterBuild;
    property ConfirmBuild: boolean read FConfirmBuild write SetConfirmBuild;
    property Globals: TGlobalCompilerOptions read FGlobals;
  end;

  { TConfigureBuildLazarusDlg }

  TConfigureBuildLazarusDlg = class(TForm)
    BuildAllButton: TButton;
    CompileButton: TButton;
    CancelButton: TButton;
    CleanAllCheckBox: TCheckBox;
    ConfirmBuildCheckBox: TCheckBox;
    ImageList: TImageList;
    ItemsListBox: TListBox;
    LCLInterfaceRadioGroup: TRadioGroup;
    SaveSettingsButton: TButton;
    OptionsEdit: TEdit;
    OptionsLabel: TLabel;
    RestartAfterBuildCheckBox: TCheckBox;
    TargetCPUComboBox: TComboBox;
    TargetCPULabel: TLabel;
    TargetDirectoryButton: TButton;
    TargetDirectoryComboBox: TComboBox;
    TargetDirectoryLabel: TLabel;
    TargetOSEdit: TEdit;
    TargetOSLabel: TLabel;
    WithStaticPackagesCheckBox: TCheckBox;
    procedure BuildAllButtonClick(Sender: TObject);
    procedure CancelButtonClick(Sender: TObject);
    procedure CompileButtonClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ItemsListBoxDrawItem(Control: TWinControl; Index: Integer;
      ARect: TRect; State: TOwnerDrawState);
    procedure ItemsListBoxMouseDown(Sender: TOBject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure ItemsListBoxShowHint(Sender: TObject; HintInfo: PHintInfo);
    procedure SaveSettingsButtonClick(Sender: TObject);
    procedure TargetDirectoryButtonClick(Sender: TObject);
  private
    ImageIndexNone: Integer;
    ImageIndexBuild: Integer;
    ImageIndexCleanBuild: Integer;
    Options: TBuildLazarusOptions;
    function GetMakeModeAtX(const X: Integer; var MakeMode: TMakeMode): boolean;
    function MakeModeToInt(MakeMode: TMakeMode): integer;
    function IntToMakeMode(i: integer): TMakeMode;
  public
    procedure Load(SourceOptions: TBuildLazarusOptions);
    procedure Save(DestOptions: TBuildLazarusOptions);
  end;

function ShowConfigureBuildLazarusDlg(
  Options: TBuildLazarusOptions): TModalResult;

function BuildLazarus(Options: TBuildLazarusOptions;
  ExternalTools: TExternalToolList; Macros: TTransferMacroList;
  const PackageOptions, CompilerPath, MakePath: string;
  Flags: TBuildLazarusFlags): TModalResult;
function CreateBuildLazarusOptions(Options: TBuildLazarusOptions;
  ItemIndex: integer; Macros: TTransferMacroList;
  const PackageOptions: string; Flags: TBuildLazarusFlags;
  var ExtraOptions: string): TModalResult;
function SaveIDEMakeOptions(Options: TBuildLazarusOptions;
  Macros: TTransferMacroList;
  const PackageOptions: string; Flags: TBuildLazarusFlags): TModalResult;
function GetMakeIDEConfigFilename: string;
function GetTranslatedMakeModes(MakeMode: TMakeMode): string;

implementation

const
  DefaultIDEMakeOptionFilename = 'idemake.cfg';
  MakeModeNames: array[TMakeMode] of string = (
      'None', 'Build', 'Clean+Build'    );
  DefaultTargetDirectory = ''; // empty will be replaced by '$(ConfDir)/bin';

function GetTranslatedMakeModes(MakeMode: TMakeMode): string;
begin
  case MakeMode of
  mmNone: Result:=lisLazBuildNone;
  mmBuild: Result:=lisLazBuildBuild;
  mmCleanBuild: Result:=lisLazBuildCleanBuild;
  else
    Result:='???';
  end;
end;

function StrToMakeMode(const s: string): TMakeMode;
begin
  for Result:=Succ(mmNone) to High(TMakeMode) do
    if CompareText(s,MakeModeNames[Result])=0 then exit;
  Result:=mmNone;
end;

function StrToLCLPlatform(const s: string): TLCLPlatform;
begin
  for Result:=Low(TLCLPlatform) to High(TLCLPlatform) do
    if CompareText(s,LCLPlatformNames[Result])=0 then exit;
  Result:=lpGtk;
end;

function ShowConfigureBuildLazarusDlg(
  Options: TBuildLazarusOptions): TModalResult;
// mrOk=save
// mrYes=save and compile
var ConfigBuildLazDlg: TConfigureBuildLazarusDlg;
begin
  Result:=mrCancel;
  ConfigBuildLazDlg:=TConfigureBuildLazarusDlg.Create(nil);
  try
    ConfigBuildLazDlg.Load(Options);
    Result:=ConfigBuildLazDlg.ShowModal;
    if Result in [mrOk,mrYes] then
      ConfigBuildLazDlg.Save(Options);
  finally
    ConfigBuildLazDlg.Free;
  end;
end;

function BuildLazarus(Options: TBuildLazarusOptions;
  ExternalTools: TExternalToolList; Macros: TTransferMacroList;
  const PackageOptions, CompilerPath, MakePath: string;
  Flags: TBuildLazarusFlags): TModalResult;
var
  Tool: TExternalToolOptions;
  i: Integer;
  CurItem: TBuildLazarusItem;
  ExtraOptions: String;
  CurMakeMode: TMakeMode;
begin
  Result:=mrCancel;
  Tool:=TExternalToolOptions.Create;
  try
    // setup external tool
    Tool.Filename:=MakePath;
    Tool.EnvironmentOverrides.Values['LCL_PLATFORM']:=
      LCLPlatformNames[Options.LCLPlatform];
    Tool.EnvironmentOverrides.Values['LANG']:= 'en_US';
    if blfOnlyIDE in Flags then
      Tool.EnvironmentOverrides.Values['USESVN2REVISIONINC']:= '0';
    if CompilerPath<>'' then
      Tool.EnvironmentOverrides.Values['PP']:=CompilerPath;
    if not FileExists(Tool.Filename) then begin
      Tool.Filename:=FindDefaultMakePath;
      if not FileExists(Tool.Filename) then begin
        MessageDlg(lisMakeNotFound,
          Format(lisTheProgramMakeWasNotFoundThisToolIsNeededToBuildLa, ['"',
            '"', #13, #13])
          ,mtError,[mbCancel],0);
        exit;
      end;
    end;
    Tool.ScanOutputForFPCMessages:=true;
    Tool.ScanOutputForMakeMessages:=true;

    // clean up
    if Options.CleanAll
    and ([blfDontClean,blfOnlyIDE]*Flags=[]) then begin
      // clean lazarus source directories
      Tool.Title:=lisCleanLazarusSource;
      Tool.WorkingDirectory:='$(LazarusDir)';
      Tool.CmdLineParams:='cleanlaz';
      // append target OS
      if Options.TargetOS<>'' then
        Tool.CmdLineParams:=Tool.CmdLineParams+' OS_TARGET='+Options.TargetOS;
      // append target CPU
      if Options.TargetCPU<>'' then
        Tool.CmdLineParams:=Tool.CmdLineParams+' CPU_TARGET='+Options.TargetCPU;
      Result:=ExternalTools.Run(Tool,Macros);
      if Result<>mrOk then exit;
    end;

    // build every item
    for i:=0 to Options.Count-1 do begin
      // build item
      CurItem:=Options.Items[i];
      // calculate make mode
      CurMakeMode:=CurItem.MakeMode;
      if (blfOnlyIDE in Flags) then begin
        if (CurItem=Options.ItemIDE) then begin
          CurMakeMode:=mmCleanBuild;
        end else
          CurMakeMode:=mmNone;
      end;
      if CurMakeMode=mmNone then continue;
      if (blfDontClean in Flags) and (CurMakeMode=mmCleanBuild) then
        CurMakeMode:=mmBuild;
      Tool.Title:=CurItem.Description;
      if (CurItem=Options.ItemIDE) and (blfWithoutLinkingIDE in Flags) then
        Tool.Title:=lisCompileIDEWithoutLinking;
      Tool.WorkingDirectory:='$(LazarusDir)'+PathDelim+CurItem.Directory;
      Tool.CmdLineParams:=CurItem.Commands[CurMakeMode];
      // append extra options
      ExtraOptions:='';
      Result:=CreateBuildLazarusOptions(Options,i,Macros,PackageOptions,Flags,
                                        ExtraOptions);
      if Result<>mrOk then exit;
      if ExtraOptions<>'' then
        Tool.EnvironmentOverrides.Values['OPT'] := ExtraOptions;
      // add -w option to print leaving/entering messages
      Tool.CmdLineParams:=Tool.CmdLineParams+' -w';
      // append target OS
      if Options.TargetOS<>'' then
        Tool.CmdLineParams:=Tool.CmdLineParams+' OS_TARGET='+Options.TargetOS;
      // append target CPU
      if Options.TargetCPU<>'' then
        Tool.CmdLineParams:=Tool.CmdLineParams+' CPU_TARGET='+Options.TargetCPU;
      // don't run svn2revisioninc when building the IDE for the second time
      if not (blfWithoutLinkingIDE in Flags) then
        Tool.CmdLineParams:=Tool.CmdLineParams+' USESVN2REVISIONINC=0';
      // run
      Result:=ExternalTools.Run(Tool,Macros);
      if Result<>mrOk then exit;
    end;
    Result:=mrOk;
  finally
    Tool.Free;
  end;
end;

function CreateBuildLazarusOptions(Options: TBuildLazarusOptions;
  ItemIndex: integer; Macros: TTransferMacroList;
  const PackageOptions: string; Flags: TBuildLazarusFlags;
  var ExtraOptions: string): TModalResult;

  function RemoveProfilerOption(const ExtraOptions: string): string;
  var
    p, StartPos: integer;
  begin
    Result:=ExtraOptions;
    // delete profiler option
    p:=Pos('-pg',Result);
    if (p>0)
    and ((p+3>length(Result)) or (Result[p+3]=' ')) // option end
    and ((p=1) or (Result[p-1]=' ')) then begin
      // profiler option found
      StartPos:=p;
      while (StartPos>1) and (Result[StartPos-1]=' ') do
        dec(StartPos);
      System.Delete(Result,StartPos,p-StartPos+3);
    end;
  end;

  procedure AppendExtraOption(const AddOption: string; EncloseIfSpace: boolean);
  begin
    if AddOption='' then exit;
    if ExtraOptions<>'' then ExtraOptions:=ExtraOptions+' ';
    if EncloseIfSpace and (Pos(' ',AddOption)>0) then
      ExtraOptions:=ExtraOptions+'"'+AddOption+'"'
    else
      ExtraOptions:=ExtraOptions+AddOption;
  end;

  procedure AppendExtraOption(const AddOption: string);
  begin
    AppendExtraOption(AddOption,true);
  end;

var
  CurItem: TBuildLazarusItem;
  MakeIDECfgFilename: String;
  NewTargetFilename: String;
  NewTargetDirectory: String;
  NewUnitDirectory: String;
  DefaultTargetOS: string;
  DefaultTargetCPU: string;
begin
  Result:=mrOk;
  CurItem:=Options.Items[ItemIndex];

  // create extra options
  ExtraOptions:=Options.ExtraOptions;

  if CurItem=Options.ItemJITForm then begin
    // remove profiler option for JIT form
    ExtraOptions:=RemoveProfilerOption(ExtraOptions);
  end else if CurItem=Options.ItemIDE then begin
    // check for special IDE config file
    if (blfUseMakeIDECfg in Flags) then begin
      MakeIDECfgFilename:=GetMakeIDEConfigFilename;
      if (FileExists(MakeIDECfgFilename)) then begin
        if pos(' ', MakeIDECfgFilename)>0 then
          ExtraOptions:='@"'+MakeIDECfgFilename+'"'
        else
          ExtraOptions:='@'+MakeIDECfgFilename;
        exit;
      end;
    end;
    // check if linking should be skipped
    if blfWithoutLinkingIDE in Flags then begin
      AppendExtraOption('-Cn');
    end;

    // set target filename and target directory:
    // 1. the user has set a target directory
    // 2. For crosscompiling the IDE it needs a different directory
    // 3. If lazarus is installed as root/administrator, the lazarus executable
    //    is readonly and needs a different name and directory
    //    (e.g. ~/.lazarus/bin/lazarus).
    // 4. Platforms like windows locks executables, so lazarus can not replace
    //    itself. They need a different name (e.g. lazarus.new.exe).
    //    The target directory is writable, the lazarus.o file can be created.
    // 5. If the user uses the startlazarus utility, then we need a backup.
    //    Under non locking platforms 'make' cleans the lazarus executable, so
    //    the IDE will rename the old file first (e.g. to lazarus.old).
    //    Renaming is not needed.
    // Otherwise: Don't touch the target filename.

    NewTargetFilename:='';
    NewUnitDirectory:='';
    NewTargetDirectory:='';
    if (Options.TargetDirectory<>'') then begin
      // Case 1. the user has set a target directory
      NewTargetDirectory:=Options.TargetDirectory;
      if not Macros.SubstituteStr(NewTargetDirectory) then begin
        debugln('CreateBuildLazarusOptions macro aborted Options.TargetDirectory=',Options.TargetDirectory);
        Result:=mrAbort;
        exit;
      end;
      NewTargetDirectory:=CleanAndExpandDirectory(NewTargetDirectory);
      debugln('CreateBuildLazarusOptions Options.TargetDirectory=',NewTargetDirectory);
      Result:=ForceDirectoryInteractive(NewTargetDirectory,[]);
      if Result<>mrOk then exit;
    end else begin
      // no user defined target directory
      // => find it automatically

      DefaultTargetOS:={$I %FPCTARGETOS%};
      DefaultTargetCPU:={$I %FPCTARGETCPU%};
      if ((Options.TargetOS<>'')
          and (CompareText(Options.TargetOS,DefaultTargetOS)<>0))
      or ((Options.TargetCPU<>'')
          and (CompareText(Options.TargetCPU,DefaultTargetCPU)<>0)) then
      begin
        // Case 2. crosscompiling the IDE
        // create directory <primary config dir>/bin/<TargetCPU>-<TargetOS>
        NewTargetDirectory:=AppendPathDelim(GetPrimaryConfigPath)+'bin'
                            +PathDelim+Options.TargetCPU+'-'+Options.TargetOS;
        Macros.SubstituteStr(NewUnitDirectory);
        debugln('CreateBuildLazarusOptions Options.TargetOS=',Options.TargetOS,' Options.TargetCPU=',Options.TargetCPU,' DefaultOS=',DefaultTargetOS,' DefaultCPU=',DefaultTargetCPU);
        Result:=ForceDirectoryInteractive(NewTargetDirectory,[]);
        if Result<>mrOk then exit;
      end else begin
        // -> normal compile for this platform

        // get lazarus directory
        if Macros<>nil then begin
          NewTargetDirectory:='$(LazarusDir)';
          Macros.SubstituteStr(NewTargetDirectory);
        end;

        if (NewTargetDirectory<>'') and DirPathExists(NewTargetDirectory) then
        begin
          if not DirectoryIsWritableCached(NewTargetDirectory) then begin
            // Case 3. the lazarus directory is not writable
            // create directory <primary config dir>/bin/
            NewTargetDirectory:=AppendPathDelim(GetPrimaryConfigPath)+'bin';
            NewUnitDirectory:=AppendPathDelim(GetPrimaryConfigPath)+'units'
                            +PathDelim+Options.TargetCPU+'-'+Options.TargetOS;
            debugln('CreateBuildLazarusOptions LazDir readonly NewTargetDirectory=',NewTargetDirectory);
            Result:=ForceDirectoryInteractive(NewTargetDirectory,[]);
            if Result<>mrOk then exit;
          end else begin
            // the lazarus directory is writable
            if OSLocksExecutables then begin
              // Case 4. the current executable is locked
              // => use a different output name
              NewTargetFilename:='lazarus.new'+GetExecutableExt(Options.TargetOS);
              debugln('CreateBuildLazarusOptions exe locked NewTargetFilename=',NewTargetFilename);
            end else begin
              // Case 5. or else: => just compile to current directory
              NewTargetDirectory:='';
            end;
          end;
        end else begin
          // lazarus dir is not valid (probably someone is experimenting)
          // -> just compile to current directory
          NewTargetDirectory:='';
        end;
      end;
    end;

    if NewUnitDirectory<>'' then
      // FPC interpretes '\ ' as an escape for a space in a path,
      // so make sure the directory doesn't end with the path delimeter.
      AppendExtraOption('-FU'+ChompPathDelim(NewTargetDirectory));

    if NewTargetDirectory<>'' then
      // FPC interpretes '\ ' as an escape for a space in a path,
      // so make sure the directory doesn't end with the path delimeter.
      AppendExtraOption('-FE'+ChompPathDelim(NewTargetDirectory));

    if NewTargetFilename<>'' then begin
      // FPC automatically changes the last extension (append or replace)
      // For example under linux, where executables don't need any extension
      // fpc removes the last extension of the -o option.
      // Trick fpc:
      if GetExecutableExt(Options.TargetOS)='' then
        NewTargetFilename:=NewTargetFilename+'.dummy';
      AppendExtraOption('-o'+NewTargetFilename);
    end;

    // add package options for IDE
    AppendExtraOption(PackageOptions,false);
  end;
end;

function SaveIDEMakeOptions(Options: TBuildLazarusOptions;
  Macros: TTransferMacroList;
  const PackageOptions: string; Flags: TBuildLazarusFlags): TModalResult;

  function BreakOptions(const OptionString: string): string;
  var
    StartPos: Integer;
    EndPos: Integer;
    c: Char;
    CurLine: String;
  begin
    Result:='';
    // write each option into a line of its own
    StartPos:=1;
    repeat
      while (StartPos<=length(OptionString)) and (OptionString[StartPos]=' ') do
        inc(StartPos);
      EndPos:=StartPos;
      while EndPos<=length(OptionString) do begin
        c:=OptionString[EndPos];
        case c of
        ' ': break;

        '''','"','`':
          begin
            repeat
              inc(EndPos);
              if (OptionString[EndPos]=c) then begin
                inc(EndPos);
                break;
              end;
            until (EndPos>length(OptionString));
          end;

        else
          inc(EndPos);
        end;
      end;
      if (EndPos>StartPos) then begin
        CurLine:=Trim(copy(OptionString,StartPos,EndPos-StartPos));
        if (length(CurLine)>2) and (CurLine[1] in ['''','"','`'])
        and (CurLine[1]=CurLine[length(CurLine)]) then begin
          // whole line enclosed in quotation marks
          // in fpc config this is forbidden and gladfully unncessary
          CurLine:=copy(CurLine,2,length(CurLine)-2);
        end;
        Result:=Result+CurLine+LineEnding;
      end;
      StartPos:=EndPos;
    until StartPos>length(OptionString);
  end;

var
  ExtraOptions: String;
  Filename: String;
  fs: TFileStream;
  OptionsAsText: String;
begin
  ExtraOptions:='';
  Result:=CreateBuildLazarusOptions(Options,Options.IndexOf(Options.ItemIDE),
                                    Macros,PackageOptions,Flags,
                                    ExtraOptions);
  if Result<>mrOk then exit;
  Filename:=GetMakeIDEConfigFilename;
  try
    InvalidateFileStateCache;
    fs:=TFileStream.Create(Filename,fmCreate);
    try
      if ExtraOptions<>'' then begin
        OptionsAsText:=BreakOptions(ExtraOptions);
        fs.Write(OptionsAsText[1],length(OptionsAsText));
      end;
    finally
      fs.Free;
    end;
  except
    on E: Exception do begin
      Result:=MessageDlg('Error writing file',
        'Unable to write file "'+Filename+'":'#13
        +E.Message,
        mtError,[mbCancel,mbAbort],0);
      exit;
    end;
  end;
  Result:=mrOk;
end;

function GetMakeIDEConfigFilename: string;
begin
  Result:=AppendPathDelim(GetPrimaryConfigPath)+DefaultIDEMakeOptionFilename;
end;

{ TConfigureBuildLazarusDlg }

procedure TConfigureBuildLazarusDlg.FormCreate(Sender: TObject);
var
  LCLInterface: TLCLPlatform;
begin
  Caption := Format(lisConfigureBuildLazarus, ['"', '"']);
  ImageIndexNone := 0;
  ImageIndexBuild := 1;
  ImageIndexCleanBuild := 2;
  Options := TBuildLazarusOptions.Create;
  CleanAllCheckBox.Caption := lisLazBuildCleanAll;
  BuildAllButton.Caption := Format(lisLazBuildSetToBuildAll, ['"', '"']);
  OptionsLabel.Caption := lisLazBuildOptions;
  LCLInterfaceRadioGroup.Caption := lisLazBuildLCLInterface;
  for LCLInterface := Low(TLCLPlatform) to High(TLCLPlatform) do begin
    LCLInterfaceRadioGroup.Items.Add(LCLPlatformNames[LCLInterface]);
  end;
  WithStaticPackagesCheckBox.Caption := lisLazBuildWithStaticPackages;
  RestartAfterBuildCheckBox.Caption := lisLazBuildRestartAfterBuild;
  ConfirmBuildCheckBox.Caption := lisLazBuildConfirmBuild;
  CompileButton.Caption := lisMenuBuild;
  SaveSettingsButton.Caption := lisLazBuildSaveSettings;
  CancelButton.Caption := lisLazBuildCancel;
  TargetOSLabel.Caption := lisLazBuildTargetOS;
  TargetCPULabel.Caption := lisLazBuildTargetCPU;
  TargetDirectoryLabel.Caption := lisLazBuildTargetDirectory;

  IDEDialogLayoutList.ApplyLayout(Self, 500, 500);

  Load(Options);
end;

procedure TConfigureBuildLazarusDlg.FormDestroy(Sender: TObject);
begin
  Options.Free;
end;

procedure TConfigureBuildLazarusDlg.ItemsListBoxDrawItem(Control: TWinControl; Index: Integer;
  ARect: TRect; State: TOwnerDrawState);
var
  ButtonState: integer;
  x: Integer;
  ButtonWidth: Integer;
  ButtonHeight: Integer;
  ButtonRect: TRect;
  InnerButtonRect: TRect;
  CurItem: TBuildLazarusItem;
  CurStr: String;
  TxtH: Integer;
  CurRect: TRect;
  ImgIndex: Integer;
  CurIcon: TBitmap;
  Mask: TBitMap;
  mm: TMakeMode;
  IconWidth: Integer;
  IconHeight: Integer;
  IconRect: TRect;
  DestRect: TRect;
begin
  if (Index<0) or (Index>=Options.Count) then exit;
  CurItem:=Options.Items[Index];
  CurStr:=CurItem.Description;
  TxtH:=ItemsListBox.Canvas.TextHeight(CurStr);
  CurRect:=ARect;
  ItemsListBox.Canvas.FillRect(CurRect);
  // draw the buttons
  x:=0;
  ButtonWidth:=ImageList.Width+4;
  ButtonHeight:=ButtonWidth;
  for mm:=Low(TMakeMode) to High(TMakeMode) do begin
    // draw button
    ButtonRect.Left:=x;
    ButtonRect.Top:=ARect.Top+((ARect.Bottom-ARect.Top-ButtonWidth) div 2);
    ButtonRect.Right:=x+ButtonWidth;
    ButtonRect.Bottom:=ButtonRect.Top+ButtonHeight;
    ButtonState:=DFCS_BUTTONPUSH;
    if CurItem.MakeMode=mm then begin
      inc(ButtonState,DFCS_PUSHED);
      ItemsListBox.Canvas.Brush.Color := clHighlight;
    end
    else
      ItemsListBox.Canvas.Brush.Color := clBtnFace;
    // draw button background
    InnerButtonRect := ButtonRect;
    inc(InnerButtonRect.Top); inc(InnerButtonRect.Left);
    dec(InnerButtonRect.Bottom); dec(InnerButtonRect.Right);
    ItemsListBox.Canvas.FillRect(InnerButtonRect);
    DrawFrameControl(
      ItemsListBox.Canvas.GetUpdatedHandle([csBrushValid,csPenValid]),
      ButtonRect, DFC_BUTTON, ButtonState);
    // draw icon
    case mm of
    mmBuild: ImgIndex:=ImageIndexBuild;
    mmCleanBuild: ImgIndex:=ImageIndexCleanBuild;
    else ImgIndex:=ImageIndexNone;
    end;
    ImageList.GetInternalImage(ImgIndex,CurIcon,Mask,IconRect);
    if CurIcon<>nil then begin
      IconWidth:=IconRect.Right-IconRect.Left;
      IconHeight:=IconRect.Bottom-IconRect.Top;
      DestRect.Left:=ARect.Left+x+((ButtonWidth-IconWidth) div 2);
      DestRect.Top:=ARect.Top+((ARect.Bottom-ARect.Top-IconHeight) div 2);
      DestRect.Right:=DestRect.Left+IconWidth;
      DestRect.Bottom:=DestRect.Top+IconHeight;
      StretchMaskBlt(ItemsListBox.Canvas.Handle,
        DestRect.Left, DestRect.Top, IconWidth, IconHeight,
        CurIcon.Canvas.Handle, 0, 0, IconWidth, IconHeight,
        CurIcon.MaskHandle, 0, 0, SRCCOPY);
    end;
    inc(x,ButtonWidth);
  end;
  // draw description
  ItemsListBox.Canvas.TextOut(x+2,
          ARect.Top+(ARect.Bottom-ARect.Top-TxtH) div 2,
          CurStr);
  // draw make mode text
  x:=ItemsListBox.ClientWidth-90;
  ItemsListBox.Canvas.TextOut(x+2,
          ARect.Top+(ARect.Bottom-ARect.Top-TxtH) div 2,
          MakeModeNames[CurItem.MakeMode]);
end;

procedure TConfigureBuildLazarusDlg.ItemsListBoxMouseDown(Sender: TOBject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  NewMakeMode: TMakeMode;
  i: Integer;
begin
  if not GetMakeModeAtX(X, NewMakeMode) then exit;
  i:=ItemsListBox.GetIndexAtY(Y);
  if (i<0) or (i>=Options.Count) then exit;
  Options.Items[i].MakeMode:=NewMakeMode;
  ItemsListBox.Invalidate;
end;

procedure TConfigureBuildLazarusDlg.ItemsListBoxShowHint(Sender: TObject; HintInfo: PHintInfo);
var
  MakeMode: TMakeMode;
  i: Integer;
begin
  with HintInfo^ do begin
    HintStr:='';
    if not GetMakeModeAtX(CursorPos.X, MakeMode) then exit;
    i:=ItemsListBox.GetIndexAtY(CursorPos.Y);
    if (i<0) or (i>=Options.Count) then exit;
    HintStr := MakeModeNames[MakeMode];
  end;
end;

procedure TConfigureBuildLazarusDlg.SaveSettingsButtonClick(Sender: TObject);
begin
  Save(Options);
  ModalResult:=mrOk;
end;

procedure TConfigureBuildLazarusDlg.TargetDirectoryButtonClick(Sender: TObject);
var
  AFilename: String;
  DirDialog: TSelectDirectoryDialog;
begin
  DirDialog:=TSelectDirectoryDialog.Create(nil);
  try
    DirDialog.Options:=DirDialog.Options+[ofPathMustExist];
    DirDialog.Title:='Choose output directory of the IDE executable '
                    +'(lazarus'+GetExecutableExt(Options.TargetOS)+')';
    if DirDialog.Execute then begin
      AFilename:=CleanAndExpandDirectory(DirDialog.Filename);
      TargetDirectoryComboBox.AddHistoryItem(AFilename,10,true,true);
    end;
  finally
    DirDialog.Free;
  end;
end;

function TConfigureBuildLazarusDlg.GetMakeModeAtX(const X: Integer;
  var MakeMode: TMakeMode): boolean;
var
  ButtonWidth: Integer;
  i: integer;
begin
  Result:=True;
  ButtonWidth:=ImageList.Width+4;
  i:=X div ButtonWidth;
  case i of
  0: MakeMode:=mmNone;
  1: MakeMode:=mmBuild;
  2: MakeMode:=mmCleanBuild;
  else Result:=False;
  end;
end;

function TConfigureBuildLazarusDlg.MakeModeToInt(MakeMode: TMakeMode): integer;
begin
  case MakeMode of
    mmBuild:      Result:=1;
    mmCleanBuild: Result:=2;
  else            Result:=0;
  end;
end;

function TConfigureBuildLazarusDlg.IntToMakeMode(i: integer): TMakeMode;
begin
  case i of
    1: Result:=mmBuild;
    2: Result:=mmCleanBuild;
  else Result:=mmNone;
  end;
end;

procedure TConfigureBuildLazarusDlg.Load(SourceOptions: TBuildLazarusOptions);
var
  i: Integer;
begin
  Options.Assign(SourceOptions);

  CleanAllCheckBox.Checked:=Options.CleanAll;

  // items
  ItemsListBox.Items.BeginUpdate;
  i:=0;
  while i<Options.Count do begin
    if i<ItemsListBox.Items.Count then begin
      ItemsListBox.Items[i]:=Options.Items[i].Description
    end else
      ItemsListBox.Items.Add(Options.Items[i].Description);
    inc(i);
  end;
  while ItemsListBox.Items.Count>i do
    ItemsListBox.Items.Delete(ItemsListBox.Items.Count-1);
  ItemsListBox.Items.EndUpdate;

  OptionsEdit.Text:=Options.ExtraOptions;
  LCLInterfaceRadioGroup.ItemIndex:=ord(Options.LCLPlatform);
  WithStaticPackagesCheckBox.Checked:=Options.WithStaticPackages;
  RestartAfterBuildCheckBox.Checked:=Options.RestartAfterBuild;
  ConfirmBuildCheckBox.Checked:=Options.ConfirmBuild;
  TargetOSEdit.Text:=Options.TargetOS;
  TargetDirectoryComboBox.Text:=Options.TargetDirectory;
  TargetCPUComboBox.Text:=Options.TargetCPU;
  
  Invalidate;
end;

procedure TConfigureBuildLazarusDlg.Save(DestOptions: TBuildLazarusOptions);
begin
  if DestOptions=nil then exit;

  Options.CleanAll:=CleanAllCheckBox.Checked;
  Options.ExtraOptions:=OptionsEdit.Text;
  Options.LCLPlatform:=TLCLPlatform(LCLInterfaceRadioGroup.ItemIndex);
  Options.WithStaticPackages:=WithStaticPackagesCheckBox.Checked;
  Options.RestartAfterBuild:=RestartAfterBuildCheckBox.Checked;
  Options.ConfirmBuild:=ConfirmBuildCheckBox.Checked;
  Options.TargetOS:=TargetOSEdit.Text;
  Options.TargetCPU:=TargetCPUComboBox.Text;
  Options.TargetDirectory:=TargetDirectoryComboBox.Text;

  DestOptions.Assign(Options);
end;

procedure TConfigureBuildLazarusDlg.CancelButtonClick(Sender: TObject);
begin
  ModalResult:=mrCancel;
end;

procedure TConfigureBuildLazarusDlg.CompileButtonClick(Sender: TObject);
begin
  Save(Options);
  ModalResult:=mrYes;
end;

procedure TConfigureBuildLazarusDlg.FormClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  IDEDialogLayoutList.SaveLayout(Self);
end;

procedure TConfigureBuildLazarusDlg.BuildAllButtonClick(Sender: TObject);
begin
  Save(Options);
  Options.SetBuildAll;
  Load(Options);
end;

{ TBuildLazarusItem }

function TBuildLazarusItem.GetCommands(Mode: TMakeMode): string;
begin
  Result:=FCommands[Mode];
end;

procedure TBuildLazarusItem.SetCommands(Mode: TMakeMode; const AValue: string);
begin
  FCommands[Mode]:=AValue;
end;

procedure TBuildLazarusItem.SetDefaultMakeMode(const AValue: TMakeMode);
begin
  if FDefaultMakeMode=AValue then exit;
  FDefaultMakeMode:=AValue;
end;

procedure TBuildLazarusItem.SetDescription(const AValue: string);
begin
  if FDescription=AValue then exit;
  FDescription:=AValue;
end;

procedure TBuildLazarusItem.SetDirectory(const AValue: string);
begin
  if FDirectory=AValue then exit;
  FDirectory:=AValue;
end;

procedure TBuildLazarusItem.SetMakeMode(const AValue: TMakeMode);
begin
  if FMakeMode=AValue then exit;
  FMakeMode:=AValue;
end;

procedure TBuildLazarusItem.SetName(const AValue: string);
begin
  if FName=AValue then exit;
  FName:=AValue;
end;

constructor TBuildLazarusItem.Create;
begin
  Clear;
end;

constructor TBuildLazarusItem.Create(const NewName, NewDescription,
  NewDirectory: string; const NewMakeMode: TMakeMode);
begin
  Clear;
  Name:=NewName;
  Description:=NewDescription;
  Directory:=NewDirectory;
  MakeMode:=NewMakeMode;
  DefaultMakeMode:=MakeMode;
end;

destructor TBuildLazarusItem.Destroy;
begin
  Clear;
  inherited Destroy;
end;

procedure TBuildLazarusItem.Clear;
begin
  FCommands[mmNone]:='';
  FCommands[mmBuild]:='all';
  FCommands[mmCleanBuild]:='clean all';
  FDirectory:='';
  FName:='';
  FMakeMode:=mmNone;
end;

procedure TBuildLazarusItem.Assign(Source: TBuildLazarusItem);
var
  mm: TMakeMode;
begin
  if (Source=nil) or (Source=Self) then exit;
  Name:=Source.Name;
  Description:=Source.Description;
  Directory:=Source.Directory;
  MakeMode:=Source.MakeMode;
  DefaultMakeMode:=Source.DefaultMakeMode;
  for mm:=Low(TMakeMode) to High(TMakeMode) do
    Commands[mm]:=Source.Commands[mm];
end;

{ TBuildLazarusOptions }

function TBuildLazarusOptions.GetCount: integer;
begin
  Result:=FItems.Count;
end;

function TBuildLazarusOptions.GetItems(Index: integer): TBuildLazarusItem;
begin
  Result:=TBuildLazarusItem(FItems[Index]);
end;

procedure TBuildLazarusOptions.SetRestartAfterBuild(const AValue: boolean);
begin
  if FRestartAfterBuild=AValue then exit;
  FRestartAfterBuild:=AValue;
end;

procedure TBuildLazarusOptions.SetConfirmBuild(const AValue: boolean);
begin
  if FConfirmBuild=AValue then exit;
  FConfirmBuild:=AValue;
end;

procedure TBuildLazarusOptions.SetTargetCPU(const AValue: string);
begin
  if FTargetCPU=AValue then exit;
  FTargetCPU:=AValue;
  FGlobals.TargetCPU:=TargetCPU;
end;

procedure TBuildLazarusOptions.SetTargetDirectory(const AValue: string);
begin
  if FTargetDirectory=AValue then exit;
  FTargetDirectory:=AValue;
end;

procedure TBuildLazarusOptions.SetTargetOS(const AValue: string);
begin
  if fTargetOS=AValue then exit;
  fTargetOS:=AValue;
  FGlobals.TargetOS:=TargetOS;
end;

procedure TBuildLazarusOptions.SetWithStaticPackages(const AValue: boolean);
begin
  if FWithStaticPackages=AValue then exit;
  FWithStaticPackages:=AValue;
end;

constructor TBuildLazarusOptions.Create;
begin
  inherited Create;
  FGlobals:=TGlobalCompilerOptions.Create;
  FItems:=TList.Create;
  FStaticAutoInstallPackages:=TStringList.Create;
  Clear;
  CreateDefaults;
end;

destructor TBuildLazarusOptions.Destroy;
begin
  Clear;
  FStaticAutoInstallPackages.Free;
  FItems.Free;
  FGlobals.Free;
  inherited Destroy;
end;

procedure TBuildLazarusOptions.Clear;
var
  i: Integer;
begin
  FCleanAll:=true;
  FExtraOptions:='';
  FTargetDirectory:=DefaultTargetDirectory;
  TargetOS:='';
  TargetCPU:='';
  fLCLPlatform:=StrToLCLPlatform(GetDefaultLCLWidgetType);

  // auto install packages
  fStaticAutoInstallPackages.Clear;

  // items
  for i:=0 to FItems.Count-1 do begin
    Items[i].Free;
  end;
  FItems.Clear;

  FItemLCL:=nil;
  FItemSynEdit:=nil;
  FItemCodeTools:=nil;
  FItemPkgReg:=nil;
  FItemIDEIntf:=nil;
  FItemJITForm:=nil;
  FItemIDE:=nil;
  FItemStarter:=nil;
  FItemExamples:=nil;
end;

procedure TBuildLazarusOptions.CreateDefaults;
begin
  // LCL
  FItemLCL:=TBuildLazarusItem.Create(
    'LCL',lisBuildLCL,'lcl',mmCleanBuild);
  FItems.Add(FItemLCL);

  // SynEdit
  FItemSynEdit:=TBuildLazarusItem.Create(
    'SynEdit',lisBuildSynEdit,'components/synedit',mmBuild);
  FItems.Add(FItemSynEdit);

  // CodeTools
  FItemCodeTools:=TBuildLazarusItem.Create(
    'CodeTools',lisBuildCodeTools,'components/codetools',mmBuild);
  FItems.Add(FItemCodeTools);

  // package registration units
  FItemPkgReg:=TBuildLazarusItem.Create(
    'PackageRegistration',lisBuildPkgReg,'packager/registration',
    mmBuild);
  FItems.Add(FItemPkgReg);

  // IDE Interface
  FItemIDEIntf:=TBuildLazarusItem.Create(
    'IDEIntf',lisBuildIDEIntf,'ideintf',mmBuild);
  FItems.Add(FItemIDEIntf);

  // JITForm
  FItemJITForm:=TBuildLazarusItem.Create(
    'JITForm',lisBuildJITForm,'designer/jitform',mmBuild);
  FItems.Add(FItemJITForm);

  // IDE
  FItemIDE:=TBuildLazarusItem.Create('IDE',lisBuildIDE,'',mmBuild);
  FItemIDE.Commands[mmBuild]:='ide';
  FItemIDE.Commands[mmCleanBuild]:='cleanide ide';
  FItems.Add(FItemIDE);

  // Starter
  FItemStarter:=TBuildLazarusItem.Create('Starter',lisBuildStarter,'',mmBuild);
  FItemStarter.Commands[mmBuild]:='starter';
  FItemStarter.Commands[mmCleanBuild]:='starter';
  FItems.Add(FItemStarter);

  // Examples
  FItemExamples:=TBuildLazarusItem.Create(
    'Examples',lisBuildExamples,'examples',mmBuild);
  FItems.Add(FItemExamples);
end;

function TBuildLazarusOptions.IndexOf(Item: TBuildLazarusItem): integer;
begin
  Result:=Count-1;
  while (Result>=0) and (Items[Result]<>Item) do dec(Result);
end;

procedure TBuildLazarusOptions.Load(XMLConfig: TXMLConfig; const Path: string);
var
  i: Integer;
begin
  Clear;
  CreateDefaults;
  for i:=0 to Count-1 do begin
    Items[i].MakeMode:=StrToMakeMode(XMLConfig.GetValue(
      Path+'Build'+Items[i].Name+'/Value',
      MakeModeNames[Items[i].MakeMode]));
  end;
  FCleanAll:=XMLConfig.GetValue(Path+'CleanAll/Value',false);
  FExtraOptions:=XMLConfig.GetValue(Path+'ExtraOptions/Value','');
  TargetOS:=XMLConfig.GetValue(Path+'TargetOS/Value','');
  TargetCPU:=XMLConfig.GetValue(Path+'TargetCPU/Value','');
  fLCLPlatform:=StrToLCLPlatform(XMLConfig.GetValue(Path+'LCLPlatform/Value',
                                 GetDefaultLCLWidgetType));
  FTargetDirectory:=AppendPathDelim(SetDirSeparators(
                  XMLConfig.GetValue(Path+'TargetDirectory/Value',
                                     DefaultTargetDirectory)));
  FRestartAfterBuild:=XMLConfig.GetValue(Path+'RestartAfterBuild/Value',true);
  FConfirmBuild:=XMLConfig.GetValue(Path+'ConfirmBuild/Value',true);
  FWithStaticPackages:=XMLConfig.GetValue(Path+'WithStaticPackages/Value',true);

  // auto install packages
  LoadStringList(XMLConfig,fStaticAutoInstallPackages,
                 Path+'StaticAutoInstallPackages/');
end;

procedure TBuildLazarusOptions.Save(XMLConfig: TXMLConfig; const Path: string);
var
  i: Integer;
begin
  for i:=0 to Count-1 do begin
    XMLConfig.SetDeleteValue(
      Path+'Build'+Items[i].Name+'/Value',
      MakeModeNames[Items[i].MakeMode],
      MakeModeNames[Items[i].DefaultMakeMode]);
  end;

  XMLConfig.SetDeleteValue(Path+'CleanAll/Value',FCleanAll,true);
  XMLConfig.SetDeleteValue(Path+'ExtraOptions/Value',FExtraOptions,'');
  XMLConfig.SetDeleteValue(Path+'TargetOS/Value',TargetOS,'');
  XMLConfig.SetDeleteValue(Path+'TargetCPU/Value',TargetCPU,'');
  XMLConfig.SetDeleteValue(Path+'LCLPlatform/Value',
                           LCLPlatformNames[fLCLPlatform],
                           GetDefaultLCLWidgetType);
  XMLConfig.SetDeleteValue(Path+'TargetDirectory/Value',
                           FTargetDirectory,DefaultTargetDirectory);
  XMLConfig.SetDeleteValue(Path+'RestartAfterBuild/Value',FRestartAfterBuild,
                           true);
  XMLConfig.SetDeleteValue(Path+'ConfirmBuild/Value',FConfirmBuild,
                           true);
  XMLConfig.SetDeleteValue(Path+'WithStaticPackages/Value',FWithStaticPackages,
                           true);

  // auto install packages
  SaveStringList(XMLConfig,fStaticAutoInstallPackages,
                 Path+'StaticAutoInstallPackages/');
end;

procedure TBuildLazarusOptions.Assign(Source: TBuildLazarusOptions);
var
  i: Integer;
  SrcItem: TBuildLazarusItem;
  NewItem: TBuildLazarusItem;
begin
  if (Source=nil) or (Source=Self) then exit;
  Clear;
  CleanAll:=Source.CleanAll;
  ExtraOptions:=Source.ExtraOptions;
  TargetOS:=Source.TargetOS;
  TargetCPU:=Source.TargetCPU;
  LCLPlatform:=Source.LCLPlatform;
  TargetDirectory:=Source.TargetDirectory;
  WithStaticPackages:=Source.WithStaticPackages;
  RestartAfterBuild:=Source.RestartAfterBuild;
  ConfirmBuild:=Source.ConfirmBuild;
  fStaticAutoInstallPackages.Assign(Source.fStaticAutoInstallPackages);
  for i:=0 to Source.Count-1 do begin
    SrcItem:=Source.Items[i];
    NewItem:=TBuildLazarusItem.Create;
    NewItem.Assign(SrcItem);
    FItems.Add(NewItem);
  end;
  FItemLCL:=FindName('LCL');
  FItemSynEdit:=FindName('SynEdit');
  FItemCodeTools:=FindName('CodeTools');
  FItemPkgReg:=FindName('PkgReg');
  FItemIDEIntf:=FindName('IDEIntf');
  FItemJITForm:=FindName('JITForm');
  FItemIDE:=FindName('IDE');
  FItemStarter:=FindName('Starter');
  FItemExamples:=FindName('Examples');
end;

procedure TBuildLazarusOptions.SetBuildAll;
var
  i: Integer;
begin
  FCleanAll:=true;
  for i:=0 to Count-1 do Items[i].MakeMode:=mmBuild;
end;

function TBuildLazarusOptions.FindName(const Name: string): TBuildLazarusItem;
var
  i: Integer;
begin
  Result:=nil;
  for i:=0 to Count-1 do if CompareText(Name,Items[i].Name)=0 then begin
    Result:=Items[i];
    exit;
  end;
end;

function TBuildLazarusOptions.CompiledUnitExt(FPCVersion, FPCRelease: integer
  ): string;
begin
  Result:=GetDefaultCompiledUnitExt(FPCVersion,FPCRelease);
  if (CompareText(TargetOS,'win32')=0)
  and (FPCVersion=1) and (FPCRelease=0) then
    Result:='.ppw'
  else
    Result:='.ppu';
end;

initialization
  {$I buildlazdialog.lrs}

end.

