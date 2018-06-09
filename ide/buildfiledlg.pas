{ Dialog to configure Build/Run file
}
unit BuildFileDlg;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  // LCL
  LCLProc, LCLType, Forms, Controls, Graphics, ComCtrls, Dialogs, StdCtrls, ButtonPanel,
  // LazUtils
  LazFileUtils,
  // CodeTools
  BasicCodeTools,
  // IdeIntf
  IDEHelpIntf, MacroDefIntf, LazIDEIntf, IDEUtils,
  // IDE
  IDEProcs, InputHistory, LazarusIDEStrConsts, EnvironmentOpts, TransferMacros;

type

  { TMacroSelectionBox }
  
  TMacroSelectionBox = class(TGroupBox)
    procedure ListBoxClick(Sender: TObject);
  private
    FMacroList: TTransferMacroList;
    FOnAddMacro: TNotifyEvent;
    ListBox: TListBox;
    AddButton: TButton;
    procedure AddButtonClick(Sender: TObject);
    procedure SetMacroList(const AValue: TTransferMacroList);
    procedure FillListBox;
  public
    constructor Create(TheOwner: TComponent); override;
    function GetSelectedMacro(var MacroAsCode: string): TTransferMacro;
    property MacroList: TTransferMacroList read FMacroList write SetMacroList;
    property OnAddMacro: TNotifyEvent read FOnAddMacro write FOnAddMacro;
  end;


  { TBuildFileDialog }

  TBuildFileDialog = class(TForm)
    RunBeforeBuildCheckbox: TCheckBox;
    BuildBrowseWorkDirButton: TButton;
    BuildCommandGroupbox: TGroupBox;
    BuildCommandMemo: TMemo;
    BuildScanForFPCMsgCheckbox: TCheckBox;
    BuildScanForMakeMsgCheckbox: TCheckBox;
    BuildWorkDirCombobox: TComboBox;
    BuildWorkingDirGroupbox: TGroupBox;
    BuildPage: TTabSheet;
    ButtonPanel: TButtonPanel;
    GeneralPage: TTabSheet;
    Notebook1: TPageControl;
    OverrideBuildProjectCheckbox: TCheckBox;
    OverrideRunProjectCheckbox: TCheckBox;
    RunBrowseWorkDirButton: TButton;
    RunCommandGroupbox: TGroupBox;
    RunCommandMemo: TMemo;
    RunPage: TTabSheet;
    RunWorkDirCombobox: TComboBox;
    RunWorkDirGroupbox: TGroupBox;
    RunShowOutputCheckBox: TCheckBox;
    WhenFileIsActiveGroupbox: TGroupBox;
    BuildMacroSelectionBox: TMacroSelectionBox;
    RunMacroSelectionBox: TMacroSelectionBox;
    procedure BuildBrowseWorkDirButtonCLICK(Sender: TObject);
    procedure BuildFileDialogCreate(Sender: TObject);
    procedure BuildFileDialogKeyDown(Sender: TObject; var Key: Word;
                                     {%H-}Shift: TShiftState);
    procedure BuildMacroSelectionBoxAddMacro(Sender: TObject);
    procedure HelpButtonClick(Sender: TObject);
    procedure OkButtonClick(Sender: TObject);
    procedure RunMacroSelectionBoxAddMacro(Sender: TObject);
  private
    FDirectiveList: TStrings;
    FFilename: string;
    FMacroList: TTransferMacroList;
    function GetBuildFileIfActive: boolean;
    function GetRunFileIfActive: boolean;
    procedure SetBuildFileIfActive(const AValue: boolean);
    procedure SetDirectiveList(const AValue: TStrings);
    procedure SetFilename(const AValue: string);
    procedure SetMacroList(const AValue: TTransferMacroList);
    procedure SetRunFileIfActive(const AValue: boolean);
    procedure UpdateCaption;
    procedure ReadDirectiveList;
    procedure WriteDirectiveList;
  public
    property DirectiveList: TStrings read FDirectiveList write SetDirectiveList;
    property BuildFileIfActive: boolean read GetBuildFileIfActive write SetBuildFileIfActive;
    property RunFileIfActive: boolean read GetRunFileIfActive write SetRunFileIfActive;
    property Filename: string read FFilename write SetFilename;
    property MacroList: TTransferMacroList read FMacroList write SetMacroList;
  end;

const
  IDEDirDefaultBuildCommand = '$(CompPath) $(EdFile)';
  IDEDirBuildScanFlagDefValues = [idedbsfFPC,idedbsfMake];
  IDEDirDefaultRunCommand = '$MakeExe($(EdFile))';
  IDEDirRunFlagDefValues = [idedrfBuildBeforeRun];

var
  IDEDirectiveSpecialChars: string;

function IndexOfIDEDirective(DirectiveList: TStrings;
                             const DirectiveName: string): integer;
function GetIDEStringDirective(DirectiveList: TStrings;
                             const DirectiveName, DefaultValue: string): string;
function GetIDEDirectiveFlag(const DirectiveValue, FlagName: string;
                            DefaultValue: boolean): boolean;
procedure SetIDEDirective(DirectiveList: TStrings; const DirectiveName: string;
                          const NewValue, DefaultValue: string);
function StringToIDEDirectiveValue(const s: string): string;
function IDEDirectiveValueToString(const s: string): string;
function IDEDirectiveNameToDirective(const DirectiveName: string): TIDEDirective;
  
// build scan flags
function IDEDirBuildScanFlagNameToFlag(const FlagName: string): TIDEDirBuildScanFlag;
function GetIDEDirBuildScanFromString(const s: string): TIDEDirBuildScanFlags;
function GetIDEDirBuildScanStrFromFlags(Flags: TIDEDirBuildScanFlags): string;

// run flags
function IDEDirRunFlagNameToFlag(const FlagName: string
  ): TIDEDirRunFlag;
function GetIDEDirRunFlagFromString(const s: string): TIDEDirRunFlags; overload;
function GetIDEDirRunFlagFromString(const s: string;
                      DefaultFlags: TIDEDirRunFlags): TIDEDirRunFlags; overload;
function GetIDEDirRunFlagStrFromFlags(Flags: TIDEDirRunFlags): string;

  

implementation

{$R *.lfm}

procedure AddFlagStr(var FlagStr: string; const FlagName: string;
  Value: boolean);
var
  s: String;
begin
  s:=FlagName;
  if FlagStr<>'' then s:=' '+s;
  if Value then
    s:=s+'+'
  else
    s:=s+'-';
  FlagStr:=FlagStr+s;
end;

function IndexOfIDEDirective(DirectiveList: TStrings;
                             const DirectiveName: string): integer;
var
  i: Integer;
  CurDirective: string;
  DirectiveNameLen: Integer;
begin
  Result:=-1;
  if (DirectiveList=nil) or (DirectiveName='') then exit;
  DirectiveNameLen:=length(DirectiveName);
  for i:=0 to DirectiveList.Count-1 do begin
    CurDirective:=DirectiveList[i];
    if length(CurDirective)>4+DirectiveNameLen then begin
      if CompareText(@CurDirective[3],DirectiveNameLen,
                     @DirectiveName[1],DirectiveNameLen,
                     false)=0
      then begin
        Result:=i;
        exit;
      end;
    end;
  end;
end;

function GetIDEStringDirective(DirectiveList: TStrings;
  const DirectiveName, DefaultValue: string): string;
var
  CurDirective: string;
  DirectiveNameLen: Integer;
  Index: Integer;
begin
  Result:=DefaultValue;
  Index:=IndexOfIDEDirective(DirectiveList,DirectiveName);
  if Index<0 then exit;
  DirectiveNameLen:=length(DirectiveName);
  CurDirective:=DirectiveList[Index];
  Result:=IDEDirectiveValueToString(copy(CurDirective,4+DirectiveNameLen,
               length(CurDirective)-4-DirectiveNameLen));
end;

function GetIDEDirectiveFlag(const DirectiveValue,
  FlagName: string; DefaultValue: boolean): boolean;
// Example: 'FPC+ Make off   BUILD  on  FPC-'

  function ReadNextWord(var ReadPos: integer;
    out WordStart, WordEnd: integer): boolean;
  begin
    Result:=false;
    // skip space
    while (ReadPos<=length(DirectiveValue))
    and (DirectiveValue[ReadPos]=' ') do
      inc(ReadPos);
    // read word
    WordStart:=ReadPos;
    while (ReadPos<=length(DirectiveValue))
    and (DirectiveValue[ReadPos]in ['a'..'z','A'..'Z']) do
      inc(ReadPos);
    WordEnd:=ReadPos;
    Result:=WordStart<WordEnd;
  end;

var
  ReadPos: Integer;
  WordStart, WordEnd,ValueStart, ValueEnd: integer;
  CurValue: Boolean;
begin
  Result:=DefaultValue;
  if (FlagName='') or (DirectiveValue='') then exit;
  ReadPos:=1;
  repeat
    if not ReadNextWord(ReadPos,WordStart,WordEnd) then exit;
    // read value
    if ReadPos>length(DirectiveValue) then begin
      // missing value
      exit;
    end;
    case DirectiveValue[ReadPos] of
    '+','-':
      begin
        CurValue:=DirectiveValue[ReadPos]='+';
        inc(ReadPos);
      end;
    ' ':
      begin
        if not ReadNextWord(ReadPos,ValueStart,ValueEnd) then exit;
        if CompareText(@DirectiveValue[ValueStart],ValueEnd-ValueStart,
                       'ON',2,false)=0
        then
          CurValue:=true
        else if CompareText(@DirectiveValue[ValueStart],ValueEnd-ValueStart,
                            'OFF',3,false)=0
        then
          CurValue:=false
        else
          // syntax error
          exit;
      end;
    else
      // syntax error
      exit;
    end;
    if CompareText(@DirectiveValue[WordStart],WordEnd-WordStart,
                   @FlagName[1],length(FlagName),false)=0
    then begin
      Result:=CurValue;
      exit;
    end;
  until false;
end;

procedure SetIDEDirective(DirectiveList: TStrings; const DirectiveName: string;
  const NewValue, DefaultValue: string);
var
  Index: Integer;
  NewEntry: String;
begin
  if (DirectiveName='') or (DirectiveList=nil) then exit;
  //DebugLn(['SetIDEDirective ',DirectiveName,' NewValue="',NewValue,'" DefaultValue="',DefaultValue,'"']);
  Index:=IndexOfIDEDirective(DirectiveList,DirectiveName);
  if NewValue=DefaultValue then begin
    // value is default -> remove entry
    while Index>=0 do begin
      DirectiveList.Delete(Index);
      Index:=IndexOfIDEDirective(DirectiveList,DirectiveName);
    end;
    exit;
  end else begin
    // value is not default
    NewEntry:='{%'+DirectiveName+' '+StringToIDEDirectiveValue(NewValue)+'}';
    if Index<0 then
      Index:=DirectiveList.Add(NewEntry)
    else
      DirectiveList[Index]:=NewEntry;
  end;
end;

function StringToIDEDirectiveValue(const s: string): string;
var
  NewLength: Integer;
  i: Integer;
  ResultPos: Integer;
  SpecialIndex: Integer;
begin
  NewLength:=length(s);
  for i:=1 to length(s) do
    if Pos(s[i],IDEDirectiveSpecialChars)>0 then
      inc(NewLength);
  if NewLength=length(s) then begin
    Result:=s;
    exit;
  end;
  SetLength(Result,NewLength);
  ResultPos:=1;
  for i:=1 to length(s) do begin
    SpecialIndex:=Pos(s[i],IDEDirectiveSpecialChars);
    if SpecialIndex>0 then begin
      Result[ResultPos]:='%';
      inc(ResultPos);
      Result[ResultPos]:=chr(ord('0')+SpecialIndex);
      inc(ResultPos);
    end else begin
      Result[ResultPos]:=s[i];
      inc(ResultPos);
    end;
  end;
  if ResultPos<>NewLength+1 then
    RaiseException('Internal error');
end;

function IDEDirectiveValueToString(const s: string): string;
var
  NewLength: Integer;
  i: Integer;
  ResultPos: Integer;
  SpecialIndex: Integer;
begin
  NewLength:=length(s);
  for i:=1 to length(s) do
    if (s[i]='%') and (i<length(s)) then
      dec(NewLength);
  if NewLength=length(s) then begin
    Result:=s;
    exit;
  end;
  SetLength(Result,NewLength);
  ResultPos:=1;
  i:=1;
  while i<=length(s) do begin
    if (s[i]='%') and (i<length(s)) then begin
      inc(i);
      SpecialIndex:=ord(s[i])-ord('0');
      inc(i);
      if (SpecialIndex<1) or (SpecialIndex>length(IDEDirectiveSpecialChars))
      then
        Result[ResultPos]:='?'
      else
        Result[ResultPos]:=IDEDirectiveSpecialChars[SpecialIndex];
      inc(ResultPos);
    end else begin
      Result[ResultPos]:=s[i];
      inc(ResultPos);
      inc(i);
    end;
  end;
  if ResultPos<>NewLength+1 then
    RaiseException('Internal error');
end;

function IDEDirectiveNameToDirective(const DirectiveName: string): TIDEDirective;
begin
  for Result:=Low(TIDEDirective) to High(TIDEDirective) do
    if CompareText(IDEDirectiveNames[Result],DirectiveName)=0 then exit;
  Result:=idedNone;
end;

function IDEDirBuildScanFlagNameToFlag(const FlagName: string): TIDEDirBuildScanFlag;
begin
  for Result:=Low(TIDEDirBuildScanFlag) to High(TIDEDirBuildScanFlag) do
    if CompareText(IDEDirBuildScanFlagNames[Result],FlagName)=0 then
      exit;
  Result:=idedbsfNone;
end;

function GetIDEDirBuildScanFromString(const s: string): TIDEDirBuildScanFlags;
var
  f: TIDEDirBuildScanFlag;
begin
  Result:=[];
  for f:=Low(TIDEDirBuildScanFlag) to High(TIDEDirBuildScanFlag) do begin
    if f=idedbsfNone then continue;
    if GetIDEDirectiveFlag(s,IDEDirBuildScanFlagNames[f],
                           f in IDEDirBuildScanFlagDefValues)
    then
      Include(Result,f);
  end;
end;

function GetIDEDirBuildScanStrFromFlags(Flags: TIDEDirBuildScanFlags): string;
var
  f: TIDEDirBuildScanFlag;
begin
  Result:='';
  for f:=Low(TIDEDirBuildScanFlag) to High(TIDEDirBuildScanFlag) do begin
    if f=idedbsfNone then continue;
    if (f in Flags)<>(f in IDEDirBuildScanFlagDefValues) then
      AddFlagStr(Result,IDEDirBuildScanFlagNames[f],f in Flags);
  end;
end;

function IDEDirRunFlagNameToFlag(const FlagName: string): TIDEDirRunFlag;
begin
  for Result:=Low(TIDEDirRunFlag) to High(TIDEDirRunFlag) do
    if CompareText(IDEDirRunFlagNames[Result],FlagName)=0 then
      exit;
  Result:=idedrfNone;
end;

function GetIDEDirRunFlagFromString(const s: string): TIDEDirRunFlags;
begin
  Result:=GetIDEDirRunFlagFromString(s,IDEDirRunFlagDefValues);
end;

function GetIDEDirRunFlagFromString(const s: string;
  DefaultFlags: TIDEDirRunFlags): TIDEDirRunFlags;
var
  f: TIDEDirRunFlag;
begin
  Result:=[];
  for f:=Low(TIDEDirRunFlag) to High(TIDEDirRunFlag) do begin
    if f=idedrfNone then continue;
    if GetIDEDirectiveFlag(s,IDEDirRunFlagNames[f],f in DefaultFlags)
    then
      Include(Result,f);
  end;
end;

function GetIDEDirRunFlagStrFromFlags(Flags: TIDEDirRunFlags): string;
var
  f: TIDEDirRunFlag;
begin
  Result:='';
  for f:=Low(TIDEDirRunFlag) to High(TIDEDirRunFlag) do begin
    if f=idedrfNone then continue;
    if (f in Flags)<>(f in IDEDirRunFlagDefValues) then
      AddFlagStr(Result,IDEDirRunFlagNames[f],f in Flags);
  end;
end;

{ TBuildFileDialog }

procedure TBuildFileDialog.BuildFileDialogKeyDown(Sender: TObject;
  var Key: Word; Shift: TShiftState);
begin
  if Key=VK_ESCAPE then ModalResult:=mrCancel;
end;

procedure TBuildFileDialog.BuildMacroSelectionBoxAddMacro(Sender: TObject);
var
  MacroCode: string;
  Macro: TTransferMacro;
begin
  MacroCode:='';
  Macro:=BuildMacroSelectionBox.GetSelectedMacro(MacroCode);
  if Macro=nil then exit;
  BuildCommandMemo.SelText:=MacroCode;
end;

procedure TBuildFileDialog.HelpButtonClick(Sender: TObject);
begin
  LazarusHelp.ShowHelpForIDEControl(Self);
end;

procedure TBuildFileDialog.OkButtonClick(Sender: TObject);
begin
  WriteDirectiveList;
  ModalResult:=mrOk;
end;

procedure TBuildFileDialog.RunMacroSelectionBoxAddMacro(Sender: TObject);
var
  MacroCode: string;
  Macro: TTransferMacro;
begin
  MacroCode:='';
  Macro:=RunMacroSelectionBox.GetSelectedMacro(MacroCode);
  if Macro=nil then exit;
  RunCommandMemo.SelText:=MacroCode;
end;

procedure TBuildFileDialog.BuildFileDialogCreate(Sender: TObject);
begin
  Notebook1.PageIndex:=0;
  
  BuildMacroSelectionBox:=TMacroSelectionBox.Create(Self);
  with BuildMacroSelectionBox do begin
    Name:='BuildMacroSelectionBox';
    Caption:=lisEdtExtToolMacros;
    OnAddMacro:=@BuildMacroSelectionBoxAddMacro;
    AnchorToNeighbour(akTop,0,BuildScanForMakeMsgCheckbox);
    BorderSpacing.Around:=6;
    Align:=alClient;
    Parent:=BuildPage;
  end;

  RunMacroSelectionBox:=TMacroSelectionBox.Create(Self);
  with RunMacroSelectionBox do begin
    Name:='RunMacroSelectionBox';
    Caption:=lisEdtExtToolMacros;
    OnAddMacro:=@RunMacroSelectionBoxAddMacro;
    AnchorToNeighbour(akTop,0,RunCommandGroupbox);
    BorderSpacing.Around:=6;
    Align:=alClient;
    Parent:=RunPage;
  end;

  GeneralPage.Caption:=lisOptions;
  WhenFileIsActiveGroupbox.Caption:=lisBFWhenThisFileIsActiveInSourceEditor;
  OverrideBuildProjectCheckbox.Caption:=
    lisBFOnBuildProjectExecuteTheBuildFileCommandInstead;
  OverrideRunProjectCheckbox.Caption:=
    lisBFOnRunProjectExecuteTheRunFileCommandInstead;

  BuildPage.Caption:=lisBuildCaption;
  BuildWorkingDirGroupbox.Caption:=lisBFWorkingDirectoryLeaveEmptyForFilePath;
  BuildCommandGroupbox.Caption:=lisBFBuildCommand;
  BuildScanForFPCMsgCheckbox.Caption:=lisCOScanForFPCMessages;
  BuildScanForMakeMsgCheckbox.Caption:=lisCOScanForMakeMessages;

  RunPage.Caption:=lisRun;
  RunBeforeBuildCheckbox.Caption:=lisBFAlwaysBuildBeforeRun;
  RunShowOutputCheckBox.Caption:=lisShowOutput;
  RunWorkDirGroupbox.Caption:=lisBFWorkingDirectoryLeaveEmptyForFilePath;
  RunCommandGroupbox.Caption:=lisBFRunCommand;

  ButtonPanel.HelpButton.OnClick := @HelpButtonClick;
  ButtonPanel.OKButton.OnClick := @OKButtonClick;
end;

procedure TBuildFileDialog.BuildBrowseWorkDirButtonCLICK(Sender: TObject);
var
  OpenDialog: TSelectDirectoryDialog;
  NewFilename: String;
  ComboBox: TComboBox;
begin
  OpenDialog:=TSelectDirectoryDialog.Create(Self);
  try
    InputHistories.ApplyFileDialogSettings(OpenDialog);
    if Sender=BuildBrowseWorkDirButton then
      OpenDialog.Title:=lisWorkingDirectoryForBuilding
    else if Sender=RunBrowseWorkDirButton then
      OpenDialog.Title:=lisWorkingDirectoryForRun
    else
      exit;
    OpenDialog.Filename:='';
    OpenDialog.InitialDir:=ExtractFilePath(Filename);
    if OpenDialog.Execute then begin
      NewFilename:=TrimFilename(OpenDialog.Filename);
      if Sender=BuildBrowseWorkDirButton then
        ComboBox:=BuildWorkDirCombobox
      else if Sender=RunBrowseWorkDirButton then
        ComboBox:=RunWorkDirCombobox;
      SetComboBoxText(ComboBox,NewFilename,cstFilename);
    end;
    InputHistories.StoreFileDialogSettings(OpenDialog);
  finally
    OpenDialog.Free;
  end;
end;

procedure TBuildFileDialog.SetDirectiveList(const AValue: TStrings);
begin
  if FDirectiveList=AValue then exit;
  FDirectiveList:=AValue;
  ReadDirectiveList;
end;

procedure TBuildFileDialog.SetFilename(const AValue: string);
begin
  if FFilename=AValue then exit;
  FFilename:=AValue;
  UpdateCaption;
end;

procedure TBuildFileDialog.SetMacroList(const AValue: TTransferMacroList);
begin
  if FMacroList=AValue then exit;
  FMacroList:=AValue;
  BuildMacroSelectionBox.MacroList:=MacroList;
  RunMacroSelectionBox.MacroList:=MacroList;
end;

procedure TBuildFileDialog.SetBuildFileIfActive(const AValue: boolean);
begin
  OverrideBuildProjectCheckbox.Checked:=AValue;
end;

function TBuildFileDialog.GetBuildFileIfActive: boolean;
begin
  Result:=OverrideBuildProjectCheckbox.Checked;
end;

function TBuildFileDialog.GetRunFileIfActive: boolean;
begin
  Result:=OverrideRunProjectCheckbox.Checked;
end;

procedure TBuildFileDialog.SetRunFileIfActive(const AValue: boolean);
begin
  OverrideRunProjectCheckbox.Checked:=AValue;
end;

procedure TBuildFileDialog.UpdateCaption;
begin
  Caption:=Format(lisConfigureBuild, [Filename]);
end;

procedure TBuildFileDialog.ReadDirectiveList;
var
  BuildWorkingDir: String;
  BuildCommand: String;
  BuildScanForFPCMsg: Boolean;
  BuildScanForMakeMsg: Boolean;
  RunWorkingDir: String;
  RunCommand: String;
  BuildScanStr: String;
  BuildScan: TIDEDirBuildScanFlags;
  RunFlags: TIDEDirRunFlags;
begin
  // get values from directive list
  // build
  BuildWorkingDir:=GetIDEStringDirective(DirectiveList,
                                       IDEDirectiveNames[idedBuildWorkingDir],
                                       '');
  BuildCommand:=GetIDEStringDirective(DirectiveList,
                                    IDEDirectiveNames[idedBuildCommand],
                                    IDEDirDefaultBuildCommand);
  BuildScanStr:=GetIDEStringDirective(DirectiveList,
                                 IDEDirectiveNames[idedBuildScan],'');
  BuildScan:=GetIDEDirBuildScanFromString(BuildScanStr);
  BuildScanForFPCMsg:=idedbsfFPC in BuildScan;
  BuildScanForMakeMsg:=idedbsfMake in BuildScan;

  // run
  RunFlags:=GetIDEDirRunFlagFromString(
               GetIDEStringDirective(DirectiveList,
                                     IDEDirectiveNames[idedRunFlags],''));
  RunWorkingDir:=GetIDEStringDirective(DirectiveList,
                                       IDEDirectiveNames[idedRunWorkingDir],'');
  RunCommand:=GetIDEStringDirective(DirectiveList,
                                  IDEDirectiveNames[idedRunCommand],
                                  IDEDirDefaultRunCommand);

  // set values to dialog
  BuildWorkDirCombobox.Text:=BuildWorkingDir;
  BuildCommandMemo.Lines.Text:=BuildCommand;
  BuildScanForFPCMsgCheckbox.Checked:=BuildScanForFPCMsg;
  BuildScanForMakeMsgCheckbox.Checked:=BuildScanForMakeMsg;
  RunBeforeBuildCheckbox.Checked:=idedrfBuildBeforeRun in RunFlags;
  RunShowOutputCheckBox.Checked:=idedrfMessages in RunFlags;
  RunWorkDirCombobox.Text:=RunWorkingDir;
  RunCommandMemo.Lines.Text:=RunCommand;
end;

procedure TBuildFileDialog.WriteDirectiveList;
var
  BuildWorkingDir: String;
  BuildCommand: String;
  BuildScanForFPCMsg: Boolean;
  BuildScanForMakeMsg: Boolean;
  BuildScan: TIDEDirBuildScanFlags;
  RunWorkingDir: String;
  RunCommand: String;
  RunFlags: TIDEDirRunFlags;
begin
  // get values from dialog
  // build
  BuildWorkingDir:=SpecialCharsToSpaces(BuildWorkDirCombobox.Text,true);
  BuildCommand:=SpecialCharsToSpaces(BuildCommandMemo.Lines.Text,true);
  BuildScanForFPCMsg:=BuildScanForFPCMsgCheckbox.Checked;
  BuildScanForMakeMsg:=BuildScanForMakeMsgCheckbox.Checked;
  BuildScan:=[];
  if BuildScanForFPCMsg then Include(BuildScan,idedbsfFPC);
  if BuildScanForMakeMsg then Include(BuildScan,idedbsfMake);

  // run
  RunFlags:=[];
  if RunBeforeBuildCheckbox.Checked then Include(RunFlags,idedrfBuildBeforeRun);
  if RunShowOutputCheckBox.Checked then Include(RunFlags,idedrfMessages);
  RunWorkingDir:=SpecialCharsToSpaces(RunWorkDirCombobox.Text,true);
  RunCommand:=SpecialCharsToSpaces(RunCommandMemo.Lines.Text,true);
  
  // set values to directivelist
  //DebugLn(['TBuildFileDialog.WriteDirectiveList ']);
  SetIDEDirective(DirectiveList,IDEDirectiveNames[idedBuildWorkingDir],
                  BuildWorkingDir,'');
  SetIDEDirective(DirectiveList,IDEDirectiveNames[idedBuildCommand],
                  BuildCommand,IDEDirDefaultBuildCommand);
  SetIDEDirective(DirectiveList,IDEDirectiveNames[idedBuildScan],
                  GetIDEDirBuildScanStrFromFlags(BuildScan),'');
  SetIDEDirective(DirectiveList,IDEDirectiveNames[idedRunWorkingDir],
                  RunWorkingDir,'');
  SetIDEDirective(DirectiveList,IDEDirectiveNames[idedRunCommand],
                  RunCommand,IDEDirDefaultRunCommand);
  SetIDEDirective(DirectiveList,IDEDirectiveNames[idedRunFlags],
                  GetIDEDirRunFlagStrFromFlags(RunFlags),'');
end;

{ TMacroSelectionBox }

procedure TMacroSelectionBox.ListBoxClick(Sender: TObject);
begin
  AddButton.Enabled:=(Listbox.ItemIndex>=0);
end;

procedure TMacroSelectionBox.AddButtonClick(Sender: TObject);
begin
  if Assigned(OnAddMacro) then OnAddMacro(Self);
end;

procedure TMacroSelectionBox.SetMacroList(const AValue: TTransferMacroList);
begin
  if FMacroList=AValue then exit;
  FMacroList:=AValue;
  FillListBox;
end;

procedure TMacroSelectionBox.FillListBox;
var
  i: Integer;
  Macro: TTransferMacro;
begin
  ListBox.Items.BeginUpdate;
  ListBox.Items.Clear;
  if MacroList=nil then exit;
  for i:=0 to MacroList.Count-1 do begin
    Macro:=MacroList[i];
    if Macro.MacroFunction=nil then begin
      Listbox.Items.Add('$('+Macro.Name+') - '+Macro.Description);
    end else begin
      Listbox.Items.Add('$'+Macro.Name+'() - '+Macro.Description);
    end;
  end;
  ListBox.Items.EndUpdate;
end;

constructor TMacroSelectionBox.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  
  AddButton:=TButton.Create(Self);
  with AddButton do begin
    Name:='AddButton';
    Caption:=lisAdd;
    OnClick:=@AddButtonClick;
    Enabled:=false;
    AutoSize:=true;
    Anchors:=[akTop,akRight];
    Top:=0;
    BorderSpacing.Around := 6;
    AnchorParallel(akTop,0,Self);
    AnchorParallel(akRight,0,Self);
    Parent:=Self;
  end;
  
  ListBox:=TListBox.Create(Self);
  with ListBox do begin
    Name:='ListBox';
    OnClick:=@ListBoxClick;
    Align:=alLeft;
    BorderSpacing.Around := 6;
    AnchorToNeighbour(akRight, 0, AddButton);
    Parent:=Self;
  end;
end;

function TMacroSelectionBox.GetSelectedMacro(
  var MacroAsCode: string): TTransferMacro;
var
  i: integer;
begin
  Result:=nil;
  MacroAsCode:='';
  if MacroList=nil then exit;
  i:=Listbox.ItemIndex;
  if i<0 then exit;
  Result:=MacroList[i];
  if Result.MacroFunction=nil then
    MacroAsCode:='$('+Result.Name+')'
  else
    MacroAsCode:='$'+Result.Name+'()';
end;

initialization
  IDEDirectiveSpecialChars:='{}*%';

end.

