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
  Classes, SysUtils, Math, Forms, Controls, LCLType, LCLIntf, Graphics,
  GraphType, StdCtrls, ExtCtrls, Buttons, FileCtrl, Dialogs, LResources,
  Laz_XMLCfg, LazarusIDEStrConsts, TransferMacros, LazConf, IDEProcs,
  InputHistory, ExtToolDialog, ExtToolEditDlg, CompilerOptions;

type
  { TBuildLazarusItem }

  TMakeMode = (
    mmNone,
    mmBuild,
    mmCleanBuild
    );
  TMakeModes = set of TMakeMode;
    
  TBuildLazarusFlag = (
    blfWithoutLinkingIDE,
    blfOnlyIDE,
    blfQuick,
    blfWithStaticPackages,
    blfUseMakeIDECfg,
    blfDontClean
    );
  TBuildLazarusFlags = set of TBuildLazarusFlag;
  
  TBuildLazarusItem = class
  private
    fCommands: array[TMakeMode] of string;
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
    fCleanAll: boolean;
    FGlobals: TGlobalCompilerOptions;
    FItemCodeTools: TBuildLazarusItem;
    FItemExamples: TBuildLazarusItem;
    FItemIDE: TBuildLazarusItem;
    FItemIDEIntf: TBuildLazarusItem;
    FItemJITForm: TBuildLazarusItem;
    FItemLCL: TBuildLazarusItem;
    FItemPkgReg: TBuildLazarusItem;
  {$IFDEF UseStartLazarus}
    FItemStarter: TBuildLazarusItem;
  {$ENDIF}
    FItemSynEdit: TBuildLazarusItem;
    fExtraOptions: string;
    FTargetDirectory: string;
    fTargetOS: string;
    fLCLPlatform: TLCLPlatform;
    fStaticAutoInstallPackages: TStringList;
    FWithStaticPackages: boolean;
    fItems: TList; // list of TBuildLazarusItem
    function GetCount: integer;
    function GetItems(Index: integer): TBuildLazarusItem;
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
    {$IFDEF UseStartLazarus}
    property ItemStarter: TBuildLazarusItem read FItemStarter;
    {$ENDIF}
    property ItemExamples: TBuildLazarusItem read FItemExamples;
    property CleanAll: boolean read fCleanAll write fCleanAll;
    //property MakeFilename: string read fMakeFilename write fMakeFilename;
    property ExtraOptions: string read fExtraOptions write fExtraOptions;
    property TargetOS: string read fTargetOS write SetTargetOS;
    property LCLPlatform: TLCLPlatform read fLCLPlatform write fLCLPlatform;
    property StaticAutoInstallPackages: TStringList
                                                read fStaticAutoInstallPackages;
    property TargetDirectory: string read FTargetDirectory
                                     write SetTargetDirectory;
    property WithStaticPackages: boolean read FWithStaticPackages
                                         write SetWithStaticPackages;
    property Globals: TGlobalCompilerOptions read FGlobals;
  end;
  
  
  { TConfigureBuildLazarusDlg }

  TConfigureBuildLazarusDlg = class(TForm)
    CleanAllCheckBox: TCheckBox;
    BuildAllButton: TButton;
    ItemsListBox: TListBox;
    WithStaticPackagesCheckBox: TCheckBox;
    OptionsLabel: TLabel;
    OptionsEdit: TEdit;
    LCLInterfaceRadioGroup: TRadioGroup;
    TargetOSLabel: TLabel;
    TargetOSEdit: TEdit;
    TargetDirectoryLabel: TLabel;
    TargetDirectoryComboBox: TComboBox;
    TargetDirectoryButton: TButton;
    OkButton: TButton;
    CancelButton: TButton;
    ImageList: TImageList;
    procedure BuildAllButtonClick(Sender: TObject);
    procedure ConfigureBuildLazarusDlgResize(Sender: TObject);
    procedure ItemsListBoxDrawItem(Control: TWinControl; Index: Integer;
                                   ARect: TRect; State: TOwnerDrawState);
    procedure ItemsListBoxMouseDown(Sender: TOBject; Button: TMouseButton;
                                    Shift: TShiftState; X, Y: Integer);
    procedure OkButtonClick(Sender: TObject);
    procedure CancelButtonClick(Sender: TObject);
    procedure TargetDirectoryButtonClick(Sender: TObject);
  private
    Options: TBuildLazarusOptions;
    ImageIndexNone: integer;
    ImageIndexBuild: integer;
    ImageIndexCleanBuild: integer;
    function MakeModeToInt(MakeMode: TMakeMode): integer;
    function IntToMakeMode(i: integer): TMakeMode;
    procedure SetupComponents;
  public
    procedure Load(SourceOptions: TBuildLazarusOptions);
    procedure Save(DestOptions: TBuildLazarusOptions);
    constructor Create(AnOwner: TComponent); override;
    destructor Destroy; override;
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
      'None', 'Build', 'Clean+Build'
    );

  DefaultTargetDirectory = ''; //'$(ConfDir)/bin';

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
    if AnsiCompareText(s,MakeModeNames[Result])=0 then exit;
  Result:=mmNone;
end;

function StrToLCLPlatform(const s: string): TLCLPlatform;
begin
  for Result:=Low(TLCLPlatform) to High(TLCLPlatform) do
    if AnsiCompareText(s,LCLPlatformNames[Result])=0 then exit;
  Result:=lpGtk;
end;

function ShowConfigureBuildLazarusDlg(
  Options: TBuildLazarusOptions): TModalResult;
var ConfigBuildLazDlg: TConfigureBuildLazarusDlg;
begin
  Result:=mrCancel;
  ConfigBuildLazDlg:=TConfigureBuildLazarusDlg.Create(Application);
  try
    ConfigBuildLazDlg.Load(Options);
    Result:=ConfigBuildLazDlg.ShowModal;
    if Result=mrOk then
      ConfigBuildLazDlg.Save(Options);
  finally
    ConfigBuildLazDlg.Free;
  end;
  Result:=mrOk;
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
    if CompilerPath<>'' then
      Tool.EnvironmentOverrides.Values['PP']:=CompilerPath;
    if not FileExists(Tool.Filename) then begin
      Tool.Filename:=FindDefaultMakePath;
      if not FileExists(Tool.Filename) then begin
        MessageDlg('Make not found',
          'The program "make" was not found.'#13
          +'This tool is needed to build lazarus.'#13
          ,mtError,[mbCancel],0);
        exit;
      end;
    end;
    Tool.ScanOutputForFPCMessages:=true;
    Tool.ScanOutputForMakeMessages:=true;
    
    // clean up
    if Options.CleanAll
    and ([blfQuick,blfOnlyIDE]*Flags=[]) then begin
      // clean lazarus source directories
      Tool.Title:=lisCleanLazarusSource;
      Tool.WorkingDirectory:='$(LazarusDir)';
      Tool.CmdLineParams:='cleanall';
      // append target OS
      if Options.TargetOS<>'' then
        Tool.CmdLineParams:=Tool.CmdLineParams+' OS_TARGET='+Options.TargetOS;
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
      if (blfQuick in Flags) and (CurMakeMode=mmCleanBuild) then
        CurMakeMode:=mmBuild;
      if CurMakeMode=mmNone then continue;
      if (CurMakeMode=mmCleanBuild) and (blfDontClean in Flags) then
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
        Tool.CmdLineParams:=Tool.CmdLineParams+' OPT="'+ExtraOptions+'"';
      // append target OS
      if Options.TargetOS<>'' then
        Tool.CmdLineParams:=Tool.CmdLineParams+' OS_TARGET='+Options.TargetOS;
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
  
  procedure AppendExtraOption(const AddOption: string);
  begin
    if AddOption='' then exit;
    if ExtraOptions<>'' then ExtraOptions:=ExtraOptions+' ';
    ExtraOptions:=ExtraOptions+AddOption;
  end;

var
  CurItem: TBuildLazarusItem;
  MakeIDECfgFilename: String;
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
        ExtraOptions:='@'+MakeIDECfgFilename;
        exit;
      end;
    end;
    // check if linking should be skipped
    if blfWithoutLinkingIDE in Flags then begin
      AppendExtraOption('-Cn');
    end;
    {$IFDEF UseStartLazarus}
      //TODO: Maybe only if running executable is lazarus and
      //      we are not cross compiling.
    AppendExtraOption('-olazarus.new'+GetDefaultExecutableExt);
    {$ENDIF}

    // add package options for IDE
    AppendExtraOption(PackageOptions);
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
              inc(c);
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
      if EndPos>StartPos then
        Result:=Result+copy(OptionString,StartPos,EndPos-StartPos)+LineEnding;
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

constructor TConfigureBuildLazarusDlg.Create(AnOwner: TComponent);
begin
  inherited Create(AnOwner);
  Options:=TBuildLazarusOptions.Create;
  Width:=480;
  Height:=435;
  Position:=poScreenCenter;
  Caption:=Format(lisConfigureBuildLazarus, ['"', '"']);
  OnResize:=@ConfigureBuildLazarusDlgResize;

  SetupComponents;
  OnResize(nil);
end;

destructor TConfigureBuildLazarusDlg.Destroy;
begin
  Options.Free;
  inherited Destroy;
end;

procedure TConfigureBuildLazarusDlg.BuildAllButtonClick(Sender: TObject);
begin
  Save(Options);
  Options.SetBuildAll;
  Load(Options);
end;

procedure TConfigureBuildLazarusDlg.ConfigureBuildLazarusDlgResize(
  Sender: TObject);
var
  x: Integer;
  y: Integer;
  w: Integer;
begin
  x:=10;
  y:=10;
  w:=ClientWidth-150;
  with CleanAllCheckBox do begin
    SetBounds(x,y,w,Height);
    inc(y,Height+5);
  end;
  with BuildAllButton do begin
    SetBounds(x,y,200,Height);
    inc(y,Height+5);
  end;
  with ItemsListBox do begin
    SetBounds(x,y,w,Max(30,Parent.ClientHeight-200));
    inc(y,Height+5);
  end;
  with OptionsLabel do begin
    SetBounds(x,y+3,80,Height);
    inc(x,Width+3);
  end;
  with OptionsEdit do begin
    SetBounds(x,y,Parent.ClientWidth-x-10,Height);
    x:=OptionsLabel.Left;
    inc(y,Height+3);
  end;
  with TargetOSLabel do begin
    SetBounds(x,y+3,80,Height);
    inc(x,Width+3);
  end;
  with TargetOSEdit do begin
    SetBounds(x,y,Parent.ClientWidth-x-10,Height);
    x:=OptionsLabel.Left;
    inc(y,Height+3);
  end;
  with TargetDirectoryLabel do begin
    SetBounds(x,y+3,120,Height);
    inc(x,Width+3);
  end;
  with TargetDirectoryComboBox do begin
    SetBounds(x,y,Parent.ClientWidth-x-10-25,Height);
    inc(x,Width+2);
  end;
  with TargetDirectoryButton do begin
    SetBounds(x,y,Parent.ClientWidth-x-10,Height);
    x:=TargetDirectoryLabel.Left;
    inc(y,Height+3);
  end;

  inc(x,w+10);
  y:=ItemsListBox.Top;
  w:=ClientWidth-10-x;
  with LCLInterfaceRadioGroup do begin
    SetBounds(x,y,w,150);
    inc(y,Height+60);
  end;
  with WithStaticPackagesCheckBox do begin
    SetBounds(x,y,w,Height);
    inc(y,Height+40);
  end;

  with OkButton do
    SetBounds(Parent.ClientWidth-180,Parent.ClientHeight-38,80,25);
  with CancelButton do
    SetBounds(Parent.ClientWidth-90,OkButton.Top,
              OkButton.Width,OkButton.Height);
end;

procedure TConfigureBuildLazarusDlg.ItemsListBoxDrawItem(Control: TWinControl;
  Index: Integer; ARect: TRect; State: TOwnerDrawState);
var
  ButtonState: integer;
  x: Integer;
  ButtonWidth: Integer;
  ButtonHeight: Integer;
  ButtonRect: TRect;
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
    if CurItem.MakeMode=mm then
      inc(ButtonState,DFCS_PUSHED);
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
      ItemsListBox.Canvas.CopyRect(DestRect,CurIcon.Canvas,IconRect);
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

procedure TConfigureBuildLazarusDlg.ItemsListBoxMouseDown(Sender: TOBject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  ButtonWidth: Integer;
  NewMakeMode: TMakeMode;
  i: Integer;
begin
  ButtonWidth:=ImageList.Width+4;
  i:=X div ButtonWidth;
  case i of
  0: NewMakeMode:=mmNone;
  1: NewMakeMode:=mmBuild;
  2: NewMakeMode:=mmCleanBuild;
  else exit;
  end;
  i:=ItemsListBox.GetIndexAtY(Y);
  if (i<0) or (i>=Options.Count) then exit;
  Options.Items[i].MakeMode:=NewMakeMode;
  ItemsListBox.Invalidate;
end;

procedure TConfigureBuildLazarusDlg.OkButtonClick(Sender: TObject);
begin
  ModalResult:=mrOk;
end;

procedure TConfigureBuildLazarusDlg.CancelButtonClick(Sender: TObject);
begin
  ModalResult:=mrCancel;
end;

procedure TConfigureBuildLazarusDlg.TargetDirectoryButtonClick(Sender: TObject);
var
  OpenDialog: TOpenDialog;
  AFilename: String;
begin
  OpenDialog:=TOpenDialog.Create(Application);
  try
    OpenDialog.Options:=OpenDialog.Options+[ofPathMustExist];
    OpenDialog.Title:='Choose output directory of the IDE executable (lazarus)';
    if OpenDialog.Execute then begin
      AFilename:=CleanAndExpandDirectory(OpenDialog.Filename);
      TargetDirectoryComboBox.AddHistoryItem(AFilename,10,true,true);
    end;
  finally
    OpenDialog.Free;
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
  TargetOSEdit.Text:=Options.TargetOS;
end;

procedure TConfigureBuildLazarusDlg.Save(DestOptions: TBuildLazarusOptions);
begin
  if DestOptions=nil then exit;
  
  Options.CleanAll:=CleanAllCheckBox.Checked;
  Options.ExtraOptions:=OptionsEdit.Text;
  Options.LCLPlatform:=TLCLPlatform(LCLInterfaceRadioGroup.ItemIndex);
  Options.WithStaticPackages:=WithStaticPackagesCheckBox.Checked;
  Options.TargetOS:=TargetOSEdit.Text;
  
  DestOptions.Assign(Options);
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

procedure TConfigureBuildLazarusDlg.SetupComponents;

  procedure AddResImg(const ResName: string);
  var Pixmap: TPixmap;
  begin
    Pixmap:=TPixmap.Create;
    Pixmap.TransparentColor:=clWhite;
    Pixmap.LoadFromLazarusResource(ResName);
    ImageList.Add(Pixmap,nil)
  end;

var
  LCLInterface: TLCLPlatform;
begin
  ImageList:=TImageList.Create(Self);
  with ImageList do begin
    Width:=20;
    Height:=20;
    Name:='ImageList';
    ImageIndexNone:=Count;
    AddResImg('menu_stepover');
    ImageIndexBuild:=Count;
    AddResImg('menu_build');
    ImageIndexCleanBuild:=Count;
    AddResImg('menu_buildall');
  end;

  CleanAllCheckBox:=TCheckBox.Create(Self);
  with CleanAllCheckBox do begin
    Parent:=Self;
    Name:='CleanAllCheckBox';
    Caption:=lisLazBuildCleanAll;
  end;

  BuildAllButton:=TButton.Create(Self);
  with BuildAllButton do begin
    Name:='BuildAllButton';
    Parent:=Self;
    Caption:=Format(lisLazBuildSetToBuildAll, ['"', '"']);
    OnClick:=@BuildAllButtonClick;
  end;
  
  ItemsListBox:=TListBox.Create(Self);
  with ItemsListBox do begin
    Name:='ItemsListBox';
    Parent:=Self;
    Style:= lbOwnerDrawFixed;
    OnMouseDown:=@ItemsListBoxMouseDown;
    OnDrawItem:=@ItemsListBoxDrawItem;
    ItemHeight:=ImageList.Height+6;
  end;

  OptionsLabel:=TLabel.Create(Self);
  with OptionsLabel do begin
    Name:='OptionsLabel';
    Parent:=Self;
    Caption:=lisLazBuildOptions;
  end;

  OptionsEdit:=TEdit.Create(Self);
  with OptionsEdit do begin
    Name:='OptionsEdit';
    Parent:=Self;
  end;

  TargetOSLabel:=TLabel.Create(Self);
  with TargetOSLabel do begin
    Name:='TargetOSLabel';
    Parent:=Self;
    Caption:=lisLazBuildTargetOS;
  end;

  TargetOSEdit:=TEdit.Create(Self);
  with TargetOSEdit do begin
    Name:='TargetOSEdit';
    Parent:=Self;
  end;
  
  TargetDirectoryLabel:=TLabel.Create(Self);
  with TargetDirectoryLabel do begin
    Name:='TargetDirectoryLabel';
    Parent:=Self;
    Caption:=lisLazBuildTargetDirectory;
    Enabled:=false;
  end;

  TargetDirectoryComboBox:=TComboBox.Create(Self);
  with TargetDirectoryComboBox do begin
    Name:='TargetDirectoryComboBox';
    Parent:=Self;
    Enabled:=false;
    Text:='';
  end;

  TargetDirectoryButton:=TButton.Create(Self);
  with TargetDirectoryButton do begin
    Name:='TargetDirectoryButton';
    Parent:=Self;
    Caption:='...';
    OnClick:=@TargetDirectoryButtonClick;
    Enabled:=false;
  end;

  LCLInterfaceRadioGroup:=TRadioGroup.Create(Self);
  with LCLInterfaceRadioGroup do begin
    Name:='LCLInterfaceRadioGroup';
    Parent:=Self;
    Caption:=lisLazBuildLCLInterface;
    for LCLInterface:=Low(TLCLPlatform) to High(TLCLPlatform) do begin
      Items.Add(LCLPlatformNames[LCLInterface]);
    end;
  end;

  WithStaticPackagesCheckBox:=TCheckBox.Create(Self);
  with WithStaticPackagesCheckBox do begin
    Name:='WithStaticPackagesCheckBox';
    Parent:=Self;
    Caption:=lisLazBuildWithStaticPackages;
    {$IFDEF DisablePkgs}
    Visible:=false;
    {$ENDIF}
  end;

  OkButton:=TButton.Create(Self);
  with OkButton do begin
    Parent:=Self;
    Name:='OkButton';
    Caption:=lisLazBuildOk;
    OnClick:=@OkButtonClick;
    Default:=true;
  end;

  CancelButton:=TButton.Create(Self);
  with CancelButton do begin
    Parent:=Self;
    Name:='CancelButton';
    Caption:=lisLazBuildCancel;
    OnClick:=@CancelButtonClick;
    Cancel:=true;
  end;

end;

{ TBuildLazarusOptions }

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

  XMLConfig.SetDeleteValue(Path+'CleanAll/Value',fCleanAll,true);
  XMLConfig.SetDeleteValue(Path+'ExtraOptions/Value',fExtraOptions,'');
  XMLConfig.SetDeleteValue(Path+'TargetOS/Value',TargetOS,'');
  XMLConfig.SetDeleteValue(Path+'LCLPlatform/Value',
                           LCLPlatformNames[fLCLPlatform],
                           GetDefaultLCLWidgetType);
  XMLConfig.SetDeleteValue(Path+'TargetDirectory/Value',
                           FTargetDirectory,DefaultTargetDirectory);
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
  LCLPlatform:=Source.LCLPlatform;
  TargetDirectory:=Source.TargetDirectory;
  WithStaticPackages:=Source.WithStaticPackages;
  fStaticAutoInstallPackages.Assign(Source.fStaticAutoInstallPackages);
  for i:=0 to Source.Count-1 do begin
    SrcItem:=Source.Items[i];
    NewItem:=TBuildLazarusItem.Create;
    NewItem.Assign(SrcItem);
    fItems.Add(NewItem);
  end;
  FItemLCL:=FindName('LCL');
  FItemSynEdit:=FindName('SynEdit');
  FItemCodeTools:=FindName('CodeTools');
  FItemPkgReg:=FindName('PkgReg');
  FItemIDEIntf:=FindName('IDEIntf');
  FItemJITForm:=FindName('JITForm');
  FItemIDE:=FindName('IDE');
  {$IFDEF UseStartLazarus}
  FItemStarter:=FindName('Starter');
  {$ENDIF}
  FItemExamples:=FindName('Examples');
end;

procedure TBuildLazarusOptions.SetBuildAll;
var
  i: Integer;
begin
  fCleanAll:=true;
  for i:=0 to Count-1 do Items[i].MakeMode:=mmBuild;
end;

function TBuildLazarusOptions.FindName(const Name: string): TBuildLazarusItem;
var
  i: Integer;
begin
  Result:=nil;
  for i:=0 to Count-1 do if AnsiCompareText(Name,Items[i].Name)=0 then begin
    Result:=Items[i];
    exit;
  end;
end;

function TBuildLazarusOptions.CompiledUnitExt(FPCVersion, FPCRelease: integer
  ): string;
begin
  Result:=GetDefaultCompiledUnitExt(FPCVersion,FPCRelease);
  if (AnsiCompareText(TargetOS,'win32')=0)
  and (FPCVersion=1) and (FPCRelease=0) then
    Result:='.ppw'
  else
    Result:='.ppu';
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
  fCleanAll:=XMLConfig.GetValue(Path+'CleanAll/Value',true);
  fExtraOptions:=XMLConfig.GetValue(Path+'ExtraOptions/Value','');
  TargetOS:=XMLConfig.GetValue(Path+'TargetOS/Value','');
  fLCLPlatform:=StrToLCLPlatform(XMLConfig.GetValue(Path+'LCLPlatform/Value',
                                 GetDefaultLCLWidgetType));
  FTargetDirectory:=AppendPathDelim(SetDirSeparators(
                  XMLConfig.GetValue(Path+'TargetDirectory/Value',
                                     DefaultTargetDirectory)));
  FWithStaticPackages:=XMLConfig.GetValue(Path+'WithStaticPackages/Value',true);

  // auto install packages
  LoadStringList(XMLConfig,fStaticAutoInstallPackages,
                 Path+'StaticAutoInstallPackages/');
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

function TBuildLazarusOptions.GetCount: integer;
begin
  Result:=FItems.Count;
end;

function TBuildLazarusOptions.GetItems(Index: integer): TBuildLazarusItem;
begin
  Result:=TBuildLazarusItem(fItems[Index]);
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
  fItems:=TList.Create;
  fStaticAutoInstallPackages:=TStringList.Create;
  Clear;
  CreateDefaults;
end;

destructor TBuildLazarusOptions.Destroy;
begin
  Clear;
  fStaticAutoInstallPackages.Free;
  fItems.Free;
  FGlobals.Free;
  inherited Destroy;
end;

procedure TBuildLazarusOptions.Clear;
var
  i: Integer;
begin
  fCleanAll:=true;
  fExtraOptions:='';
  FTargetDirectory:=DefaultTargetDirectory;
  TargetOS:='';
  fLCLPlatform:=StrToLCLPlatform(GetDefaultLCLWidgetType);

  // auto install packages
  fStaticAutoInstallPackages.Clear;

  // items
  for i:=0 to FItems.Count-1 do begin
    Items[i].Free;
  end;
  fItems.Clear;

  FItemLCL:=nil;
  FItemSynEdit:=nil;
  FItemCodeTools:=nil;
  FItemPkgReg:=nil;
  FItemIDEIntf:=nil;
  FItemJITForm:=nil;
  FItemIDE:=nil;
  {$IFDEF UseStartLazarus}
  FItemStarter:=nil;
  {$ENDIF}
  FItemExamples:=nil;
end;

procedure TBuildLazarusOptions.CreateDefaults;
begin
  // LCL
  FItemLCL:=TBuildLazarusItem.Create(
    'LCL',lisBuildLCL,'lcl',mmCleanBuild);
  fItems.Add(FItemLCL);

  // SynEdit
  FItemSynEdit:=TBuildLazarusItem.Create(
    'SynEdit',lisBuildSynEdit,'components/synedit',mmBuild);
  fItems.Add(FItemSynEdit);

  // CodeTools
  FItemCodeTools:=TBuildLazarusItem.Create(
    'CodeTools',lisBuildCodeTools,'components/codetools',mmBuild);
  fItems.Add(FItemCodeTools);

  // package registration units
  FItemPkgReg:=TBuildLazarusItem.Create(
    'PackageRegistration',lisBuildPkgReg,'packager/registration',
    mmBuild);
  fItems.Add(FItemPkgReg);

  // IDE Interface
  FItemIDEIntf:=TBuildLazarusItem.Create(
    'IDEIntf',lisBuildIDEIntf,'ideintf',mmBuild);
  fItems.Add(FItemIDEIntf);

  // JITForm
  FItemJITForm:=TBuildLazarusItem.Create(
    'JITForm',lisBuildJITForm,'designer/jitform',mmBuild);
  fItems.Add(FItemJITForm);

  // IDE
  FItemIDE:=TBuildLazarusItem.Create('IDE',lisBuildIDE,'',mmBuild);
  FItemIDE.Commands[mmBuild]:='ide';
  FItemIDE.Commands[mmCleanBuild]:='cleanide ide';
  fItems.Add(FItemIDE);

  {$IFDEF UseStartLazarus}
  // Starter
  FItemStarter:=TBuildLazarusItem.Create('Starter',lisBuildStarter,'',mmBuild);
  FItemStarter.Commands[mmBuild]:='starter';
  FItemStarter.Commands[mmCleanBuild]:='starter';
  fItems.Add(FItemStarter);
  {$ENDIF}

  // Examples
  FItemExamples:=TBuildLazarusItem.Create(
    'Examples',lisBuildExamples,'examples',mmBuild);
  fItems.Add(FItemExamples);
end;

function TBuildLazarusOptions.IndexOf(Item: TBuildLazarusItem): integer;
begin
  Result:=Count-1;
  while (Result>=0) and (Items[Result]<>Item) do dec(Result);
end;

{ TBuildLazarusItem }

function TBuildLazarusItem.GetCommands(Mode: TMakeMode): string;
begin
  Result:=fCommands[Mode];
end;

procedure TBuildLazarusItem.SetCommands(Mode: TMakeMode; const AValue: string);
begin
  fCommands[Mode]:=AValue;
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
  fCommands[mmNone]:='';
  fCommands[mmBuild]:='all';
  fCommands[mmCleanBuild]:='clean all';
  FDirectory:='';
  fName:='';
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

end.


