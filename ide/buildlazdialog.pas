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
  Quickoptions added by: Giuliano Colla

  Abstract:
    Defines the TBuildLazarusOptions which stores the settings for the
    "Build Lazarus" function of the IDE.
    TConfigureBuildLazarusDlg is used to edit TBuildLazarusOptions.

    The BuildLazarus function will build the lazarus parts.

    Building occurs only with options defined in the Advanced Page.
    The Quick Page is just used to set options there.
    Therefore advanced users can ignore the Quick Page, while beginners
    can see the effect of the choice made by looking at the Advanced Page.
}
unit BuildLazDialog;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LCLProc, LConvEncoding, Forms, Controls, LCLType, LCLIntf,
  Graphics, GraphType, StdCtrls, ExtCtrls, Buttons, FileUtil, Dialogs, Types,
  Laz_XMLCfg, InterfaceBase, Themes, ComCtrls,
  LazarusIDEStrConsts, TransferMacros, LazConf, IDEProcs, DialogProcs,
  IDEWindowIntf, IDEMsgIntf, InputHistory, ExtToolDialog, ExtToolEditDlg,
  EnvironmentOpts,
  {$IFDEF win32}
  CodeToolManager, // added for windres workaround
  {$ENDIF}
  ApplicationBundle, CompilerOptions, IDEContextHelpEdit;

type

  { TBuildLazarusItem }

  TMakeMode =
  (
    mmNone,
    mmBuild,
    mmCleanBuild
  );
  TMakeModes = set of TMakeMode;

  TBuildLazarusFlag = (
    blfWithoutCompilingIDE, // skip compiling stage of IDE
    blfWithoutLinkingIDE, // skip linking stage of IDE
    blfOnlyIDE,           // skip all but IDE
    blfDontClean,         // ignore clean up
    blfWithStaticPackages,// build with IDE static design time packages
    blfUseMakeIDECfg      // use idemake.cfg
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
    FAdvanced: boolean;
    FCleanAll: boolean;
    FGlobals: TGlobalCompilerOptions;
    FItemExamples: TBuildLazarusItem;
    FItemIDE: TBuildLazarusItem;
    FItemIDEIntf: TBuildLazarusItem;
    FItemLCL: TBuildLazarusItem;
    FItemPkgReg: TBuildLazarusItem;
    FItemSynEdit: TBuildLazarusItem;
    FExtraOptions: string;
    FRestartAfterBuild: boolean;
    FConfirmBuild: boolean;
    FQuickBuildOption:integer;
    FTargetCPU: string;
    FTargetDirectory: string;
    fTargetOS: string;
    fLCLPlatform: TLCLPlatform;
    fTargetPlatform: TLCLPlatform; // holds selection for LCL Build
    fIDEPlatform: TLCLPlatform;  // holds selection for IDE Build
    fStaticAutoInstallPackages: TStringList;
    FUpdateRevisionInc: boolean;
    FWithStaticPackages: boolean;
    FItems: TList; // list of TBuildLazarusItem
    function GetCount: integer;
    function GetItems(Index: integer): TBuildLazarusItem;
    procedure SetTargetCPU(const AValue: string);
    procedure SetTargetOS(const AValue: string);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    procedure CreateDefaults;
    function IndexOf(Item: TBuildLazarusItem): integer;
    procedure Load(XMLConfig: TXMLConfig; const Path: string);
    procedure Save(XMLConfig: TXMLConfig; const Path: string);
    procedure Assign(Source: TBuildLazarusOptions);
    procedure SetMakeMode(AMode: TMakeMode);
    function FindName(const Name: string): TBuildLazarusItem;
    function CompiledUnitExt(FPCVersion, FPCRelease: integer): string;
  public
    property Count: integer read GetCount;
    property Items[Index: integer]: TBuildLazarusItem read GetItems;
    property ItemLCL: TBuildLazarusItem read FItemLCL;
    property ItemSynEdit: TBuildLazarusItem read FItemSynEdit;
    property ItemPkgReg: TBuildLazarusItem read FItemPkgReg;
    property ItemIDEIntf: TBuildLazarusItem read FItemIDEIntf;
    property ItemIDE: TBuildLazarusItem read FItemIDE;
    property ItemExamples: TBuildLazarusItem read FItemExamples;
    property Advanced: boolean read FAdvanced write FAdvanced;
    property CleanAll: boolean read FCleanAll write FCleanAll;
    property ExtraOptions: string read FExtraOptions write FExtraOptions;
    property TargetOS: string read fTargetOS write SetTargetOS;
    property TargetCPU: string read FTargetCPU write SetTargetCPU;
    property LCLPlatform: TLCLPlatform read fLCLPlatform write fLCLPlatform;
    property IDEPlatform: TLCLPlatform read fIDEPlatform write fIDEPlatform;
    property TargetPlatform: TLCLPlatform read fTargetPlatform write fTargetPlatform;
    property StaticAutoInstallPackages: TStringList
                                                read fStaticAutoInstallPackages;
    property TargetDirectory: string read FTargetDirectory
                                     write FTargetDirectory;
    property WithStaticPackages: boolean read FWithStaticPackages
                                         write FWithStaticPackages;
    property UpdateRevisionInc: boolean read FUpdateRevisionInc write FUpdateRevisionInc;
    property RestartAfterBuild: boolean read FRestartAfterBuild
                                        write FRestartAfterBuild;
    property ConfirmBuild: boolean read FConfirmBuild write FConfirmBuild;
    property QuickBuildOption:integer read FQuickBuildOption write FQuickBuildOption;
    property Globals: TGlobalCompilerOptions read FGlobals;
  end;

  { TConfigureBuildLazarusDlg }

  TConfigureBuildLazarusDlg = class(TForm)
    HelpButton: TBitBtn;
    CancelButton: TBitBtn;
    CompileButton: TBitBtn;
    Panel2: TPanel;
    SaveSettingsButton: TBitBtn;
    TargetDirectoryButton: TButton;
    AppLCLInterfaceComboBox: TComboBox;
    IDELCLInterfaceComboBox: TComboBox;
    QuickLCLInterfaceComboLabel: TLabel;
    AppLCLLabel: TLabel;
    IDELCLLabel: TLabel;
    QuickBuildOptionsRadioGroup: TRadioGroup;
    RestartAfterBuildCheckBox: TCheckBox;
    ConfirmBuildCheckBox: TCheckBox;
    CleanAllCheckBox: TCheckBox;
    TargetDirectoryComboBox: TComboBox;
    TargetCPUComboBox: TComboBox;
    OptionsEdit: TEdit;
    TargetOsEdit: TEdit;
    OptionsLabel: TLabel;
    TargetOSLabel: TLabel;
    TargetDirectoryLabel: TLabel;
    TargetCPULabel: TLabel;
    ItemsListBox: TListBox;
    UpdateRevisionIncCheckBox: TCheckBox;
    WithStaticPackagesCheckBox: TCheckBox;
    ItemListHeader: THeaderControl;
    Notebook: TPageControl;
    QuickBuildOptionsPage: TTabSheet;
    AdvancedBuildOptionsPage: TTabSheet;
    CBLDBtnPanel: TPanel;
    LCLInterfaceRadioGroup: TRadioGroup;
    procedure AdvancedBuildOptionsPageResize(Sender: TObject);
    procedure AppLCLInterfaceComboBoxChange(Sender: TObject);
    procedure CompileButtonClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure HelpButtonClick(Sender: TObject);
    procedure ItemListHeaderResize(Sender: TObject);
    procedure ItemListHeaderSectionClick(HeaderControl: TCustomHeaderControl;
      Section: THeaderSection);
    procedure ItemsListBoxDrawItem(Control: TWinControl; Index: Integer;
      ARect: TRect; State: TOwnerDrawState);
    procedure ItemsListBoxMouseDown(Sender: TOBject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure ItemsListBoxShowHint(Sender: TObject; HintInfo: PHintInfo);
    procedure NotebookPageChanged(Sender: TObject);
    procedure QuickBuildOptionsRadioGroupClick(Sender: TObject);
    procedure SaveSettingsButtonClick(Sender: TObject);
    procedure TargetDirectoryButtonClick(Sender: TObject);
    procedure TargetOsEditEditingDone(Sender: TObject);
  private
    FAdvanced: Boolean;
    FOptions: TBuildLazarusOptions;
    function GetMakeModeAtX(const X: Integer; out MakeMode: TMakeMode): boolean;
    function MakeModeToInt(MakeMode: TMakeMode): integer;
    function IntToMakeMode(i: integer): TMakeMode;
    procedure SetAdvanced(AValue: boolean);
  public
    procedure Load(SourceOptions: TBuildLazarusOptions);
    procedure Save(DestOptions: TBuildLazarusOptions);
    property  Advanced: Boolean read FAdvanced write SetAdvanced;
    property  Options: TBuildLazarusOptions read FOptions;
  end;

function ShowConfigureBuildLazarusDlg(
  AOptions: TBuildLazarusOptions): TModalResult;

function BuildLazarus(Options: TBuildLazarusOptions;
  ExternalTools: TExternalToolList; Macros: TTransferMacroList;
  const PackageOptions, CompilerPath, MakePath: string;
  Flags: TBuildLazarusFlags): TModalResult;
function CreateBuildLazarusOptions(Options: TBuildLazarusOptions;
  ItemIndex: integer; Macros: TTransferMacroList;
  const PackageOptions: string; Flags: TBuildLazarusFlags;
  var ExtraOptions: string; out UpdateRevisionInc: boolean;
  out OutputDirRedirected: boolean): TModalResult;
function SaveIDEMakeOptions(Options: TBuildLazarusOptions;
  Macros: TTransferMacroList;
  const PackageOptions: string; Flags: TBuildLazarusFlags): TModalResult;
function GetMakeIDEConfigFilename: string;
function GetTranslatedMakeModes(MakeMode: TMakeMode): string;

implementation

{$R *.lfm}

uses
  IDEImagesIntf;

const
  DefaultIDEMakeOptionFilename = 'idemake.cfg';
  MakeModeNames: array[TMakeMode] of string = (
      'None', 'Build', 'Clean+Build'    );
  DefaultTargetDirectory = ''; // empty will be replaced by '$(ConfDir)/bin';
  ButtonSize = 24;

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

function ShowConfigureBuildLazarusDlg(
  AOptions: TBuildLazarusOptions): TModalResult;
// mrOk=save
// mrYes=save and compile
var
  ConfigBuildLazDlg: TConfigureBuildLazarusDlg;
  OldExtraOptions: String;
begin
  Result := mrCancel;
  ConfigBuildLazDlg := TConfigureBuildLazarusDlg.Create(nil);
  try
    ConfigBuildLazDlg.Load(AOptions);
    Result := ConfigBuildLazDlg.ShowModal;
    if Result in [mrOk,mrYes]
    then begin
      //DebugLn('ShowConfigureBuildLazarusDialog');
      OldExtraOptions:=AOptions.ExtraOptions;
      AOptions.Assign(ConfigBuildLazDlg.Options);
      if OldExtraOptions<>AOptions.ExtraOptions then
        IncreaseCompilerParseStamp;
    end;
  finally
    ConfigBuildLazDlg.Free;
  end;
end;

function BuildLazarus(Options: TBuildLazarusOptions;
  ExternalTools: TExternalToolList; Macros: TTransferMacroList;
  const PackageOptions, CompilerPath, MakePath: string;
  Flags: TBuildLazarusFlags): TModalResult;

  function CheckDirectoryWritable(Dir: string): boolean;
  begin
    if DirectoryIsWritableCached(Dir) then exit(true);
    Result:=false;
    MessageDlg(lisBuildingLazarusFailed,
      Format(lisThisSetOfOptionsToBuildLazarusIsNotSupportedByThis, [#13, '"',
        Dir, '"', #13]),
      mtError,[mbCancel],0);
  end;

var
  Tool: TExternalToolOptions;
  i: Integer;
  CurItem: TBuildLazarusItem;
  ExtraOptions, LinkerAddition: String;
  CurMakeMode: TMakeMode;
  WorkingDirectory: String;
  OutputDirRedirected, UpdateRevisionInc: boolean;
begin
  Result:=mrCancel;

  if (blfOnlyIDE in Flags) and (blfWithoutLinkingIDE in Flags)
  and (blfWithoutCompilingIDE in Flags) then
    exit(mrOk); // only IDE, but skip both parts -> nothing to do
  
  Tool:=TExternalToolOptions.Create;
  try
    // setup external tool
    Tool.Filename:=MakePath;
    Tool.EnvironmentOverrides.Values['LCL_PLATFORM']:=
      LCLPlatformDirNames[Options.LCLPlatform];
    Tool.EnvironmentOverrides.Values['LANG']:= 'en_US';
    if CompilerPath<>'' then
      Tool.EnvironmentOverrides.Values['PP']:=CompilerPath;
    if (Tool.Filename<>'') and (not FileExistsUTF8(Tool.Filename)) then
      Tool.Filename:=FindDefaultExecutablePath(Tool.Filename);
    if (Tool.Filename='') or (not FileExistsUTF8(Tool.Filename)) then begin
      Tool.Filename:=FindDefaultMakePath;
      if (Tool.Filename='') or (not FileExistsUTF8(Tool.Filename)) then begin
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
      WorkingDirectory:=EnvironmentOptions.LazarusDirectory;
      if not CheckDirectoryWritable(WorkingDirectory) then exit(mrCancel);

      // clean lazarus source directories
      Tool.Title:=lisCleanLazarusSource;
      Tool.WorkingDirectory:=WorkingDirectory;
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

      WorkingDirectory:=TrimFilename(EnvironmentOptions.LazarusDirectory
                                     +PathDelim+CurItem.Directory);

      // calculate make mode
      CurMakeMode:=CurItem.MakeMode;
      if (blfOnlyIDE in Flags) then begin
        if (CurItem<>Options.ItemIDE) then
          CurMakeMode:=mmNone;
      end;
      if (CurItem=Options.ItemIDE) then
      begin
        if (blfWithoutCompilingIDE in Flags) and (blfWithoutLinkingIDE in Flags)
        then
          CurMakeMode:=mmNone
        // build the IDE when blfOnlyIDE is set, eg. when installing packages
        // even if that build node is disabled in configure build lazarus dialog
        else if (blfOnlyIDE in Flags) and (CurMakeMode=mmNone) then
          CurMakeMode := mmBuild;
      end;

      if CurMakeMode=mmNone then continue;

      if (blfDontClean in Flags) and (CurMakeMode=mmCleanBuild) then
        CurMakeMode:=mmBuild;
      Tool.Title:=CurItem.Description;
      if (CurItem=Options.ItemIDE) and (blfWithoutLinkingIDE in Flags) then
        Tool.Title:=lisCompileIDEWithoutLinking;
      Tool.WorkingDirectory:=WorkingDirectory;
      Tool.CmdLineParams:=CurItem.Commands[CurMakeMode];
      // append extra options
      ExtraOptions:='';
      Result:=CreateBuildLazarusOptions(Options,i,Macros,PackageOptions,Flags,
                                        ExtraOptions,UpdateRevisionInc,
                                        OutputDirRedirected);
      if Result<>mrOk then exit;

      if (not OutputDirRedirected)
      and (not CheckDirectoryWritable(WorkingDirectory)) then
        exit(mrCancel);

      // add Linker options for wigdet set
      LinkerAddition := LCLWidgetLinkerAddition[Options.LCLPlatform];
      if LinkerAddition <> '' then
      begin
        if ExtraOptions <> '' then
          ExtraOptions := ExtraOptions + ' ' + LinkerAddition
        else
          ExtraOptions := LinkerAddition;
      end;
      
      if ExtraOptions<>'' then
        Tool.EnvironmentOverrides.Values['OPT'] := ExtraOptions;
      if not UpdateRevisionInc then
        Tool.EnvironmentOverrides.Values['USESVN2REVISIONINC'] := '0';
      // add -w option to print leaving/entering messages
      Tool.CmdLineParams:=Tool.CmdLineParams+' -w';
      // append target OS
      if Options.TargetOS<>'' then
        Tool.CmdLineParams:=Tool.CmdLineParams+' OS_TARGET='+Options.TargetOS;
      // append target CPU
      if Options.TargetCPU<>'' then
        Tool.CmdLineParams:=Tool.CmdLineParams+' CPU_TARGET='+Options.TargetCPU;
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
  var ExtraOptions: string; out UpdateRevisionInc: boolean;
  out OutputDirRedirected: boolean): TModalResult;

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
    //DebugLn(['AppendExtraOption ',ExtraOptions]);
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
  NewTargetOS: String;
  NewTargetCPU: String;
  CrossCompiling: Boolean;
  CurTargetFilename: String;
  BundleDir: String;
begin
  Result:=mrOk;
  UpdateRevisionInc:=Options.UpdateRevisionInc;
  OutputDirRedirected:=false;
  CurItem:=Options.Items[ItemIndex];

  // create extra options
  ExtraOptions:=Options.ExtraOptions;

  if CurItem=Options.ItemIDE then begin
    // check for special IDE config file
    if (blfUseMakeIDECfg in Flags) then begin
      MakeIDECfgFilename:=GetMakeIDEConfigFilename;
      //DebugLn(['CreateBuildLazarusOptions MAKE MakeIDECfgFilename=',MakeIDECfgFilename,' ',FileExistsUTF8(MakeIDECfgFilename)]);
      if (FileExistsUTF8(MakeIDECfgFilename)) then begin
        // If a file name contains spaces, a file name whould need to be quoted.
        // Using a single quote is not possible, it is used already in the
        // makefile to group all options in OPT='bla bla'.
        // using " implicates that make uses a shell to execute the command of
        // that line. But using shells (i.e. command.com, cmd.exe, etc) is so
        // fragile (see bug 11362), that is better to avoid this.
        // Therefore we use a short 8.3 file and path name, so we don't need to
        // use quotes at all.
        // On platforms other than windows, ExtractShortPathName is implemented
        // too and simply returns the passed file name, so there is no need
        // for $IFDEF.
        if pos(' ',MakeIDECfgFilename)>0 then
          MakeIDECfgFilename:=ExtractShortPathNameUTF8(MakeIDECfgFilename);
        AppendExtraOption('@'+MakeIDECfgFilename);
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
    DefaultTargetOS:=GetDefaultTargetOS;
    DefaultTargetCPU:=GetDefaultTargetCPU;
    NewTargetOS:=Options.TargetOS;
    NewTargetCPU:=Options.TargetCPU;
    if NewTargetOS='' then NewTargetOS:=DefaultTargetOS;
    if NewTargetCPU='' then NewTargetCPU:=DefaultTargetCPU;
    CrossCompiling:=(CompareText(NewTargetOS,DefaultTargetOS)<>0) or (CompareText(NewTargetCPU,DefaultTargetCPU)<>0);
    DebugLn(['CreateBuildLazarusOptions NewTargetOS=',NewTargetOS,' NewTargetCPU=',NewTargetCPU]);
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
      if OSLocksExecutables and not CrossCompiling then begin
        // Allow for the case where this corresponds to the current executable
        NewTargetFilename:='lazarus'+GetExecutableExt(NewTargetOS);
        if FileExistsUTF8(AppendPathDelim(NewTargetDirectory)+NewTargetFilename) then
          NewTargetFilename:='lazarus.new'+GetExecutableExt(NewTargetOS)
      end;
    end else begin
      // no user defined target directory
      // => find it automatically

      if CrossCompiling then
      begin
        // Case 2. crosscompiling the IDE
        // create directory <primary config dir>/bin/<TargetCPU>-<TargetOS>
        NewTargetDirectory:=AppendPathDelim(GetPrimaryConfigPath)+'bin'
                            +PathDelim+NewTargetOS+'-'+NewTargetCPU;
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
            UpdateRevisionInc:=false;
            NewTargetDirectory:=AppendPathDelim(GetPrimaryConfigPath)+'bin';
            NewUnitDirectory:=AppendPathDelim(GetPrimaryConfigPath)+'units'
                            +PathDelim+NewTargetCPU+'-'+NewTargetOS;
            debugln('CreateBuildLazarusOptions LazDir readonly NewTargetDirectory=',NewTargetDirectory);
            Result:=ForceDirectoryInteractive(NewTargetDirectory,[]);
            if Result<>mrOk then exit;
          end else begin
            // the lazarus directory is writable
            if OSLocksExecutables then begin
              // Case 4. the current executable is locked
              // => use a different output name
              NewTargetFilename:='lazarus.new'+GetExecutableExt(NewTargetOS);
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

    OutputDirRedirected:=NewTargetDirectory<>'';

    // create apple bundle if needed
    //debugln(['CreateBuildLazarusOptions NewTargetDirectory=',NewTargetDirectory]);
    if (Options.LCLPlatform in [lpCarbon,lpCocoa])
    and (NewTargetDirectory<>'')
    and (DirectoryIsWritableCached(NewTargetDirectory)) then begin
      CurTargetFilename:=NewTargetFilename;
      if CurTargetFilename='' then
        CurTargetFilename:='lazarus'+GetExecutableExt(NewTargetOS);
      if not FilenameIsAbsolute(CurTargetFilename) then
        CurTargetFilename:=NewTargetDirectory+PathDelim+CurTargetFilename;
      BundleDir:=ChangeFileExt(CurTargetFilename,'.app');
      //debugln(['CreateBuildLazarusOptions checking bundle ',BundleDir]);
      if not FileExistsCached(BundleDir) then begin
        //debugln(['CreateBuildLazarusOptions CurTargetFilename=',CurTargetFilename]);
        Result:=CreateApplicationBundle(CurTargetFilename, 'Lazarus');
        if not (Result in [mrOk,mrIgnore]) then begin
          debugln(['CreateBuildLazarusOptions CreateApplicationBundle failed']);
          IDEMessagesWindow.AddMsg('Error: failed to create application bundle '+BundleDir,NewTargetDirectory,-1);
          exit;
        end;
        Result:=CreateAppBundleSymbolicLink(CurTargetFilename);
        if not (Result in [mrOk,mrIgnore]) then begin
          debugln(['CreateBuildLazarusOptions CreateAppBundleSymbolicLink failed']);
          IDEMessagesWindow.AddMsg('Error: failed to create application bundle symlink to '+CurTargetFilename,NewTargetDirectory,-1);
          exit;
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
      if GetExecutableExt(NewTargetOS)='' then
        NewTargetFilename:=NewTargetFilename+'.dummy';
      AppendExtraOption('-o'+NewTargetFilename);
    end;

    // add package options for IDE
    //DebugLn(['CreateBuildLazarusOptions blfUseMakeIDECfg=',blfUseMakeIDECfg in FLags,' ExtraOptions="',ExtraOptions,'" ',PackageOptions]);
    if not (blfUseMakeIDECfg in Flags) then
      AppendExtraOption(PackageOptions,false);
  end;
  //DebugLn(['CreateBuildLazarusOptions ',CurItem.Name,' ',ExtraOptions]);
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
  UpdateRevisionInc: boolean;
  OutputDirRedirected: boolean;
begin
  ExtraOptions:='';
  Result:=CreateBuildLazarusOptions(Options,Options.IndexOf(Options.ItemIDE),
                                    Macros,PackageOptions,Flags,
                                    ExtraOptions,UpdateRevisionInc,
                                    OutputDirRedirected);
  if Result<>mrOk then exit;
  Filename:=GetMakeIDEConfigFilename;
  try
    InvalidateFileStateCache;
    fs:=TFileStream.Create(UTF8ToSys(Filename),fmCreate);
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
      Result:=MessageDlg(lisLazBuildErrorWritingFile,
        Format(lisLazBuildUnableToWriteFile, [Filename, #13])
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

  ItemListHeader.Images := IDEImages.Images_16;
  with ItemListHeader.Sections.Add do
  begin
    Width := ButtonSize;
    MinWidth := Width;
    MaxWidth := Width;
    ImageIndex := IDEImages.LoadImage(16, 'menu_close');
  end;
  with ItemListHeader.Sections.Add do
  begin
    Width := ButtonSize;
    MinWidth := Width;
    MaxWidth := Width;
    ImageIndex := IDEImages.LoadImage(16, 'menu_build');
  end;
  with ItemListHeader.Sections.Add do
  begin
    Width := ButtonSize;
    MinWidth := Width;
    MaxWidth := Width;
    ImageIndex := IDEImages.LoadImage(16, 'menu_build_clean');
  end;
  with ItemListHeader.Sections.Add do
  begin
    Width := ItemListHeader.Width - 90 - 3 * ButtonSize;
    MinWidth := Width;
    MaxWidth := Width;
    Text := lisLazBuildABOPart;
  end;
  with ItemListHeader.Sections.Add do
  begin
    Width := 90;
    MinWidth := Width;
    MaxWidth := Width;
    Text := lisLazBuildABOAction;
  end;

  FOptions := TBuildLazarusOptions.Create;
  CleanAllCheckBox.Caption := lisLazBuildCleanAll;
  OptionsLabel.Caption := lisLazBuildOptions;
  LCLInterfaceRadioGroup.Caption := lisLazBuildLCLInterface;
  QuickLCLInterfaceComboLabel.Caption := lisLazBuildLCLInterface;
  QuickBuildOptionsRadioGroup.Caption := lisLazBuildBuildOptions;
  QuickBuildOptionsPage.Caption := lisLazBuildQuickBuildOptions;
  with QuickBuildOptionsRadioGroup do
  begin
    Items[0]:=lisLazBuildQBOBuildLCL;
    Items[1]:=lisLazBuildQBOBuildIDEwPackages;
    Items[2]:=lisLazBuildQBOBuildIDEwithoutPackages;
    Items[3]:=lisLazBuildQBOBuildAll;
    Items[4]:=lisLazBuildQBOCleanUpBuildAll;
    Items[5]:=lisLazBuildQBOBuildOther;
  end;
  AppLCLLabel.Caption:=lisLazBuildQBOAppLCLTarget;

  AdvancedBuildOptionsPage.Caption := lisLazBuildAdvancedBuildOptions;

  for LCLInterface := Low(TLCLPlatform) to High(TLCLPlatform) do
  begin
    LCLInterfaceRadioGroup.Items.Add(LCLPlatformDisplayNames[LCLInterface]);
    AppLCLInterfaceComboBox.Items.Add(LCLPlatformDisplayNames[LCLInterface]);
    IDELCLInterfaceComboBox.Items.Add(LCLPlatformDisplayNames[LCLInterface]);
  end;
  
  WithStaticPackagesCheckBox.Caption := lisLazBuildWithStaticPackages;
  UpdateRevisionIncCheckBox.Caption := lisUpdateRevisionInc;
  RestartAfterBuildCheckBox.Caption := lisLazBuildRestartAfterBuild;
  ConfirmBuildCheckBox.Caption := lisLazBuildConfirmBuild;
  CompileButton.Caption := lisMenuBuild;
  SaveSettingsButton.Caption := lisLazBuildSaveSettings;
  CancelButton.Caption := lisLazBuildCancel;
  HelpButton.Caption := lisMenuHelp;
  TargetOSLabel.Caption := lisLazBuildTargetOS;
  TargetCPULabel.Caption := lisLazBuildTargetCPU;
  TargetDirectoryLabel.Caption := lisLazBuildTargetDirectory;

  IDEDialogLayoutList.ApplyLayout(Self, 520, 570);

  CompileButton.LoadGlyphFromLazarusResource('menu_build');
  SaveSettingsButton.LoadGlyphFromStock(idButtonSave);
  if SaveSettingsButton.Glyph.Empty then
    SaveSettingsButton.LoadGlyphFromLazarusResource('laz_save');
  Load(FOptions);
end;

procedure TConfigureBuildLazarusDlg.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FOptions);
end;

procedure TConfigureBuildLazarusDlg.FormShow(Sender: TObject);
begin

end;

procedure TConfigureBuildLazarusDlg.HelpButtonClick(Sender: TObject);
begin
  ShowContextHelpForIDE(Self);
end;

procedure TConfigureBuildLazarusDlg.ItemListHeaderResize(Sender: TObject);
begin
  if ItemListHeader.Sections.Count >= 3 then
    ItemListHeader.Sections[3].Width := ItemListHeader.Width - 90 - 3 * ButtonSize;
end;

procedure TConfigureBuildLazarusDlg.ItemListHeaderSectionClick(
  HeaderControl: TCustomHeaderControl; Section: THeaderSection);
begin
  if Section.Index in [0..2] then
  begin
    Save(Options);
    Options.SetMakeMode(IntToMakeMode(Section.Index));
    Load(Options);
  end;
end;

procedure TConfigureBuildLazarusDlg.ItemsListBoxDrawItem(Control: TWinControl;
  Index: Integer; ARect: TRect; State: TOwnerDrawState);
var
  ButtonState: TThemedButton;
  ButtonDetails: TThemedElementDetails;
  x: Integer;
  ButtonRect: TRect;
  CurItem: TBuildLazarusItem;
  CurStr: String;
  TxtH: Integer;
  CurRect: TRect;
  mm: TMakeMode;
  RadioSize: TSize;
begin
  if (Index<0) or (Index>=Options.Count) then exit;
  CurItem:=Options.Items[Index];
  CurStr:=CurItem.Description;
  TxtH:=ItemsListBox.Canvas.TextHeight(CurStr);
  CurRect:=ARect;
  ItemsListBox.Canvas.Brush.Style:=bsSolid;
  ItemsListBox.Canvas.FillRect(CurRect);
  // draw the buttons
  x:=0;
  for mm:=Low(TMakeMode) to High(TMakeMode) do
  begin
    // draw button
    ButtonRect.Left:=x;
    ButtonRect.Top:=ARect.Top+((ARect.Bottom-ARect.Top-ButtonSize) div 2);
    ButtonRect.Right:=x+ButtonSize;
    ButtonRect.Bottom:=ButtonRect.Top + ButtonSize;

    if CurItem.MakeMode = mm then // checked
      ButtonState := tbRadioButtonCheckedNormal
    else
      ButtonState := tbRadioButtonUncheckedNormal;

    ButtonDetails := ThemeServices.GetElementDetails(ButtonState);
    if ThemeServices.HasTransparentParts(ButtonDetails) then
      ItemsListBox.Canvas.FillRect(ButtonRect);

    RadioSize := ThemeServices.GetDetailSize(ButtonDetails);
    if (RadioSize.cx <> -1) and (RadioSize.cy <> -1) then
    begin
      ButtonRect.Left := (ButtonRect.Left + ButtonRect.Right - RadioSize.cx) div 2;
      ButtonRect.Right := ButtonRect.Left + RadioSize.cx;
      ButtonRect.Top := (ButtonRect.Top + ButtonRect.Bottom - RadioSize.cy) div 2;
      ButtonRect.Bottom := ButtonRect.Top + RadioSize.cy;
    end;

    ThemeServices.DrawElement(ItemsListBox.Canvas.GetUpdatedHandle([csBrushValid,csPenValid]), ButtonDetails, ButtonRect);
    Inc(x, ButtonSize);
  end;

  // draw description
  ItemsListBox.Canvas.Brush.Style := bsClear;
  ItemsListBox.Canvas.TextRect(Rect(x, ARect.Top, ItemsListBox.ClientWidth - 90, ARect.Bottom), x + 2,
          ARect.Top + (ARect.Bottom - ARect.Top - TxtH) div 2,
          CurStr);
  // draw make mode text
  x := ItemsListBox.ClientWidth - 90;
  ItemsListBox.Canvas.TextRect(Rect(x, ARect.Top, ItemsListBox.ClientWidth, ARect.Bottom), x + 2,
          ARect.Top + (ARect.Bottom - ARect.Top - TxtH) div 2,
          GetTranslatedMakeModes(CurItem.MakeMode));
end;

procedure TConfigureBuildLazarusDlg.ItemsListBoxMouseDown(Sender: TOBject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  NewMakeMode: TMakeMode;
  i: Integer;
begin
  if not GetMakeModeAtX(X, NewMakeMode) then
    exit;
  i := ItemsListBox.ItemAtPos(Point(X,Y),true);
  if (i < 0) or (i >= Options.Count) then
    exit;
  Options.Items[i].MakeMode:=NewMakeMode;
  ItemsListBox.Invalidate;
  QuickBuildOptionsRadioGroup.ItemIndex := 5;
end;

procedure TConfigureBuildLazarusDlg.ItemsListBoxShowHint(Sender: TObject; HintInfo: PHintInfo);
var
  MakeMode: TMakeMode;
  i: Integer;
begin
  with HintInfo^ do begin
    HintStr:='';
    if not GetMakeModeAtX(CursorPos.X, MakeMode) then exit;
    i:=ItemsListBox.ItemAtPos(CursorPos,true);
    if (i<0) or (i>=Options.Count) then exit;
    HintStr := MakeModeNames[MakeMode];
  end;
end;

procedure TConfigureBuildLazarusDlg.TargetDirectoryButtonClick(Sender: TObject);
var
  AFilename: String;
  DirDialog: TSelectDirectoryDialog;
begin
  DirDialog:=TSelectDirectoryDialog.Create(nil);
  try
    DirDialog.Options:=DirDialog.Options+[ofPathMustExist];
    DirDialog.Title:=lisLazBuildABOChooseOutputDir+'(lazarus'+GetExecutableExt(Options.TargetOS)+')';
    if DirDialog.Execute then begin
      AFilename:=CleanAndExpandDirectory(DirDialog.Filename);
      TargetDirectoryComboBox.AddHistoryItem(AFilename,10,true,true);
    end;
  finally
    DirDialog.Free;
  end;
end;

procedure TConfigureBuildLazarusDlg.TargetOsEditEditingDone(Sender: TObject);
begin
  TargetOsEdit.Text:=lowercase(TargetOsEdit.Text);
  DebugLn(['TConfigureBuildLazarusDlg.TargetOsEditEditingDone ',TargetOsEdit.Text]);
end;

function TConfigureBuildLazarusDlg.GetMakeModeAtX(const X: Integer;
  out MakeMode: TMakeMode): boolean;
var
  i: integer;
begin
  Result:=True;
  MakeMode:=mmNone;
  i := X div ButtonSize;
  case i of
    0: MakeMode:=mmNone;
    1: MakeMode:=mmBuild;
    2: MakeMode:=mmCleanBuild;
  else
    Result:=False;
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
  AppLCLInterfaceComboBox.ItemIndex:=ord(Options.TargetPlatform);
  IDELCLInterfaceComboBox.ItemIndex:=ord(Options.IDEPlatform);
  WithStaticPackagesCheckBox.Checked:=Options.WithStaticPackages;
  UpdateRevisionIncCheckBox.Checked:=Options.UpdateRevisionInc;
  RestartAfterBuildCheckBox.Checked:=Options.RestartAfterBuild;
  ConfirmBuildCheckBox.Checked:=Options.ConfirmBuild;
  QuickBuildOptionsRadioGroup.ItemIndex:=Options.QuickBuildOption;
  TargetOSEdit.Text:=Options.TargetOS;
  TargetDirectoryComboBox.Text:=Options.TargetDirectory;
  TargetCPUComboBox.Text:=Options.TargetCPU;

  Advanced := Options.Advanced;
  if not Advanced then
    QuickBuildOptionsRadioGroupClick(Self);

  Invalidate;
end;

procedure TConfigureBuildLazarusDlg.Save(DestOptions: TBuildLazarusOptions);
begin
  if DestOptions=nil then exit;

  Options.Advanced := Advanced;
  Options.CleanAll:=CleanAllCheckBox.Checked;
  Options.ExtraOptions:=OptionsEdit.Text;
  Options.LCLPlatform:=TLCLPlatform(LCLInterfaceRadioGroup.ItemIndex);
  if AppLCLInterfaceComboBox.ItemIndex = -1 then
    Options.TargetPlatform:= Options.LCLPlatform
  else
    Options.TargetPlatform:= TLCLPlatform(AppLCLInterfaceComboBox.ItemIndex);
  if IDELCLInterfaceComboBox.ItemIndex = -1 then
    Options.IDEPlatform := Options.LCLPlatform
  else
    Options.IDEPlatform := TLCLPlatform(IDELCLInterfaceComboBox.ItemIndex);
  Options.WithStaticPackages:=WithStaticPackagesCheckBox.Checked;
  Options.UpdateRevisionInc:=UpdateRevisionIncCheckBox.Checked;
  Options.RestartAfterBuild:=RestartAfterBuildCheckBox.Checked;
  Options.ConfirmBuild:=ConfirmBuildCheckBox.Checked;
  Options.QuickBuildOption:=QuickBuildOptionsRadioGroup.itemindex;
  Options.TargetOS:=TargetOSEdit.Text;
  Options.TargetCPU:=TargetCPUComboBox.Text;
  Options.TargetDirectory:=TargetDirectoryComboBox.Text;

  DestOptions.Assign(Options);
end;

procedure TConfigureBuildLazarusDlg.SetAdvanced(AValue: boolean);
begin
  if AValue = FAdvanced then Exit;
  FAdvanced := AValue;

  if FAdvanced
  then Notebook.PageIndex := 1
  else Notebook.PageIndex := 0;
end;

procedure TConfigureBuildLazarusDlg.AppLCLInterfaceComboBoxChange(
  Sender: TObject);
var
  Combo: TComboBox;
begin
  Combo := Sender as TComboBox;
  LCLInterfaceRadioGroup.ItemIndex := Combo.ItemIndex;
end;

procedure TConfigureBuildLazarusDlg.AdvancedBuildOptionsPageResize(
  Sender: TObject);
begin
  ItemListHeader.Width:=AdvancedBuildOptionsPage.ClientWidth div 2;
end;

procedure TConfigureBuildLazarusDlg.CompileButtonClick(Sender: TObject);
begin
  Save(Options);
  ModalResult:=mrYes;
end;

procedure TConfigureBuildLazarusDlg.SaveSettingsButtonClick(Sender: TObject);
begin
  Save(Options);
  ModalResult:=mrOk;
end;

procedure TConfigureBuildLazarusDlg.FormClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  IDEDialogLayoutList.SaveLayout(Self);
end;

procedure TConfigureBuildLazarusDlg.NotebookPageChanged(Sender: TObject);
begin
  FAdvanced := Notebook.ActivePage = AdvancedBuildOptionsPage;
end;

procedure TConfigureBuildLazarusDlg.QuickBuildOptionsRadioGroupClick(
  Sender: TObject);

  procedure SetOptions(ASet, AReset: TMakeMode; ASetItem: TBuildLazarusItem);
  var
    i: Integer;
    Item: TBuildLazarusItem;
  begin
    for i := 0 to Options.Count -1 do
    begin
      Item := Options.Items[i];
      if Item = ASetItem
      then Item.MakeMode := ASet
      else Item.MakeMode := AReset;
    end;
  end;

begin

  if QuickBuildOptionsRadioGroup.ItemIndex = 0
  then begin     // Build LCL
    if AppLCLInterfaceComboBox.ItemIndex > -1
    then LCLInterfaceRadioGroup.ItemIndex := AppLCLInterfaceComboBox.ItemIndex;
    IDELCLInterfaceComboBox.Enabled := False;
    AppLCLInterfaceComboBox.Enabled := True;
  end
  else begin     // Build IDE
    if IDELCLInterfaceComboBox.ItemIndex > -1
    then LCLInterfaceRadioGroup.ItemIndex := IDELCLInterfaceComboBox.ItemIndex;
    AppLCLInterfaceComboBox.Enabled := False;
    IDELCLInterfaceComboBox.Enabled := True;
  end;

  case QuickBuildOptionsRadioGroup.ItemIndex of
    0 : begin  // Build LCL
      SetOptions(mmBuild, mmNone, Options.ItemLCL);
    end;
    1 : begin // Build IDE with Packages
      SetOptions(mmBuild, mmNone, Options.ItemIDE);
      WithStaticPackagesCheckBox.Checked := True;
    end;
    2 : begin // Build IDE w/out Packages
      SetOptions(mmBuild, mmNone, Options.ItemIDE);
      WithStaticPackagesCheckBox.Checked := False;
    end;
    3 : begin // Build All
      SetOptions(mmNone, mmBuild, nil);
      WithStaticPackagesCheckBox.Checked := True;
    end;
   4 : begin // Clean + Build All
      SetOptions(mmNone, mmCleanBuild, nil);
      WithStaticPackagesCheckBox.Checked := True;
    end;
  end;
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

procedure TBuildLazarusOptions.SetTargetCPU(const AValue: string);
begin
  if FTargetCPU=AValue then exit;
  FTargetCPU:=AValue;
  FGlobals.TargetCPU:=TargetCPU;
end;

procedure TBuildLazarusOptions.SetTargetOS(const AValue: string);
begin
  if fTargetOS=AValue then exit;
  fTargetOS:=AValue;
  FGlobals.TargetOS:=TargetOS;
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
  fLCLPlatform:=GetDefaultLCLWidgetType;

  // auto install packages
  fStaticAutoInstallPackages.Clear;

  // items
  for i:=0 to FItems.Count-1 do begin
    Items[i].Free;
  end;
  FItems.Clear;

  FItemLCL:=nil;
  FItemSynEdit:=nil;
  FItemPkgReg:=nil;
  FItemIDEIntf:=nil;
  FItemIDE:=nil;
  FItemExamples:=nil;
end;

procedure TBuildLazarusOptions.CreateDefaults;
begin
  // LCL
  FItemLCL:=TBuildLazarusItem.Create(
    'LCL',lisLCL,'lcl',mmCleanBuild);
  FItems.Add(FItemLCL);

  // package registration units
  FItemPkgReg:=TBuildLazarusItem.Create(
    'PackageRegistration',lisPkgReg,'packager'+PathDelim+'registration',
    mmBuild);
  FItems.Add(FItemPkgReg);

  // IDE Interface
  FItemIDEIntf:=TBuildLazarusItem.Create(
    'IDEIntf',lisIDEIntf,'ideintf',mmBuild);
  FItems.Add(FItemIDEIntf);

  // SynEdit
  FItemSynEdit:=TBuildLazarusItem.Create(
    'SynEdit',lisSynEdit,'components'+PathDelim+'synedit',mmBuild);
  FItems.Add(FItemSynEdit);

  // IDE
  FItemIDE:=TBuildLazarusItem.Create('IDE',lisIDE,'',mmBuild);
  FItemIDE.Commands[mmBuild]:='ide';
  FItemIDE.Commands[mmCleanBuild]:='cleanide ide';
  FItems.Add(FItemIDE);

  // Examples
  FItemExamples:=TBuildLazarusItem.Create(
    'Examples',lisExamples,'examples',mmBuild);
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
  LCLPlatformStr: string;
begin
  Clear;
  CreateDefaults;
  for i:=0 to Count-1 do begin
    Items[i].MakeMode:=StrToMakeMode(XMLConfig.GetValue(
      Path+'Build'+Items[i].Name+'/Value',
      MakeModeNames[Items[i].MakeMode]));
  end;
  FAdvanced:=XMLConfig.GetValue(Path+'Advanced/Value',false);
  FCleanAll:=XMLConfig.GetValue(Path+'CleanAll/Value',false);
  FExtraOptions:=XMLConfig.GetValue(Path+'ExtraOptions/Value','');
  TargetOS:=XMLConfig.GetValue(Path+'TargetOS/Value','');
  TargetCPU:=XMLConfig.GetValue(Path+'TargetCPU/Value','');
  LCLPlatformStr:= XMLConfig.GetValue(Path+'LCLPlatform/Value','');
  if LCLPlatformStr='' then
    fLCLPlatform:=GetDefaultLCLWidgetType
  else
    fLCLPlatform:=DirNameToLCLPlatform(LCLPlatformStr);
  LCLPlatformStr:= XMLConfig.GetValue(Path+'TargetLCLPlatform/Value','');
  if LCLPlatformStr='' then
    fTargetPlatform:=GetDefaultLCLWidgetType
  else
    fTargetPlatform:=DirNameToLCLPlatform(LCLPlatformStr);

  LCLPlatformStr:= XMLConfig.GetValue(Path+'IDELCLPlatform/Value','');
  if LCLPlatformStr='' then
    fIDEPlatform:=GetDefaultLCLWidgetType
  else
    fIDEPlatform:=DirNameToLCLPlatform(LCLPlatformStr);
  FTargetDirectory:=AppendPathDelim(SetDirSeparators(
                  XMLConfig.GetValue(Path+'TargetDirectory/Value',
                                     DefaultTargetDirectory)));
  FRestartAfterBuild:=XMLConfig.GetValue(Path+'RestartAfterBuild/Value',true);
  FConfirmBuild:=XMLConfig.GetValue(Path+'ConfirmBuild/Value',true);
  FQuickBuildOption:=XMLConfig.GetValue(Path+'QuickBuild/Value',0);
  FWithStaticPackages:=XMLConfig.GetValue(Path+'WithStaticPackages/Value',true);
  FUpdateRevisionInc:=XMLConfig.GetValue(Path+'UpdateRevisionInc/Value',true);

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

  XMLConfig.SetDeleteValue(Path+'Advanced/Value',FAdvanced,false);
  XMLConfig.SetDeleteValue(Path+'CleanAll/Value',FCleanAll,false);
  XMLConfig.SetDeleteValue(Path+'ExtraOptions/Value',FExtraOptions,'');
  XMLConfig.SetDeleteValue(Path+'TargetOS/Value',TargetOS,'');
  XMLConfig.SetDeleteValue(Path+'TargetCPU/Value',TargetCPU,'');
  XMLConfig.SetDeleteValue(Path+'LCLPlatform/Value',
                           LCLPlatformDirNames[fLCLPlatform],
                           LCLPlatformDirNames[GetDefaultLCLWidgetType]);
  XMLConfig.SetDeleteValue(Path+'TargetLCLPlatform/Value',
                           LCLPlatformDirNames[fTargetPlatform],
                           LCLPlatformDirNames[GetDefaultLCLWidgetType]);
  XMLConfig.SetDeleteValue(Path+'IDELCLPlatform/Value',
                           LCLPlatformDirNames[fIDEPlatform],
                           LCLPlatformDirNames[GetDefaultLCLWidgetType]);
  XMLConfig.SetDeleteValue(Path+'TargetDirectory/Value',
                           FTargetDirectory,DefaultTargetDirectory);
  XMLConfig.SetDeleteValue(Path+'RestartAfterBuild/Value',FRestartAfterBuild,
                           true);
  XMLConfig.SetDeleteValue(Path+'ConfirmBuild/Value',FConfirmBuild,
                           true);
  XMLConfig.SetDeleteValue(Path+'QuickBuild/Value',FQuickBuildOption,0);
  XMLConfig.SetDeleteValue(Path+'WithStaticPackages/Value',FWithStaticPackages,
                           true);
  XMLConfig.SetDeleteValue(Path+'UpdateRevisionInc/Value',FUpdateRevisionInc,
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
  Advanced:=Source.Advanced;
  CleanAll:=Source.CleanAll;
  ExtraOptions:=Source.ExtraOptions;
  TargetOS:=Source.TargetOS;
  TargetCPU:=Source.TargetCPU;
  LCLPlatform:=Source.LCLPlatform;
  TargetPlatform:=Source.TargetPlatform;
  IDEPlatform:= Source.IDEPlatform;
  TargetDirectory:=Source.TargetDirectory;
  WithStaticPackages:=Source.WithStaticPackages;
  UpdateRevisionInc:=Source.UpdateRevisionInc;
  RestartAfterBuild:=Source.RestartAfterBuild;
  ConfirmBuild:=Source.ConfirmBuild;
  QuickBuildOption:=Source.QuickBuildOption;
  fStaticAutoInstallPackages.Assign(Source.fStaticAutoInstallPackages);
  for i:=0 to Source.Count-1 do begin
    SrcItem:=Source.Items[i];
    NewItem:=TBuildLazarusItem.Create;
    NewItem.Assign(SrcItem);
    FItems.Add(NewItem);
  end;
  FItemLCL:=FindName('LCL');
  FItemPkgReg:=FindName('PkgReg');
  FItemIDEIntf:=FindName('IDEIntf');
  FItemSynEdit:=FindName('SynEdit');
  FItemIDE:=FindName('IDE');
  FItemExamples:=FindName('Examples');
end;

procedure TBuildLazarusOptions.SetMakeMode(AMode: TMakeMode);
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
    Items[i].MakeMode := AMode;
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

end.

