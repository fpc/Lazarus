{***************************************************************************
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
    Frame to edit compiler config file, target and syntax mode
    (project+packages).
}
unit compiler_config_target;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, strutils,
  // LCL
  Controls, Dialogs, Graphics, StdCtrls,
  // LazUtils
  LazFileUtils, LazStringUtils, LazUTF8, LazLoggerBase, FPCAdds,
  // CodeTools
  DefineTemplates, CodeToolManager,
  // BuildIntf
  IDEOptionsIntf, MacroIntf,
  // IdeIntf
  IDEOptEditorIntf, IDEDialogs, IDEIntfUtils, IDEIntfStrConsts,
  // IdeUtils
  InputHistory,
  // IdeConfig
  RecentListProcs, TransferMacros, CompilerOptions, IdeConfStrConsts,
  EnvironmentOpts, ParsedCompilerOpts, CompilerTargetInfo, MiscOptions,
  // IDE
  LazarusIDEStrConsts, PackageDefs, Project, compiler_parsing_options;

type

  { TCompilerConfigTargetFrame }

  TCompilerConfigTargetFrame = class(TAbstractIDEOptionsEditor)
    chkConfigFile: TCheckBox;
    chkCustomConfigFile: TCheckBox;
    chkWriteConfigFile: TCheckBox;
    chkWin32GraphicApp: TCheckBox;
    edtCustomConfigPath: TEdit;
    edtWriteConfigFilePath: TEdit;
    grbTargetOptions: TGroupBox;
    grbConfigFile: TGroupBox;
    grbTargetPlatform: TGroupBox;
    CurrentWidgetTypeLabel: TLabel;
    lblTargetCPU: TLabel;
    lblTargetOS: TLabel;
    lblTargetProc: TLabel;
    lblSubtarget: TLabel;
    LCLWidgetTypeLabel: TLabel;
    TargetCPUComboBox: TComboBox;
    TargetOSComboBox: TComboBox;
    TargetProcComboBox: TComboBox;
    SubtargetComboBox: TComboBox;
    chkOnlyAvailable: TCheckBox;
    lblController: TLabel;
    ControllerComboBox: TComboBox;
    procedure chkOnlyAvailableClick(Sender: TObject);
    procedure chkCustomConfigFileClick(Sender: TObject);
    procedure chkWriteConfigFileClick(Sender: TObject);
    procedure TargetOSComboBoxSelect(Sender: TObject);
    procedure TargetCPUComboBoxSelect(Sender: TObject);
    procedure TargetProcComboBoxSelect(Sender: TObject);
    procedure ControllerComboBoxSelect(Sender: TObject);
    procedure LCLWidgetTypeLabelClick(Sender: TObject);
    procedure LCLWidgetTypeLabelMouseEnter(Sender: TObject);
    procedure LCLWidgetTypeLabelMouseLeave(Sender: TObject);
  private
    FDialog: TAbstractOptionsEditorDialog;
    FCompOptions: TBaseCompilerOptions;
    FIsPackage: boolean;
    FUpdating: boolean; // reentrancy guard while one combo forces another
    function EnsureQueryCompilerTrusted: boolean;
    procedure SelectComboOrDefault(aCombo: TComboBox; const aText: string);
    procedure RevertInvalidSubOptions;
    procedure UpdateByTargetOS(aTargetOS: string);
    procedure UpdateByTargetCPU(aTargetCPU: string);
    procedure FillSubTargetComboBox(UseSubTarget: string);
    function OnlyAvailable: boolean;
    function GetQueryCompilerFilename: string;
    function GetSelectedTargetCPU: string;
    function GetSelectedTargetOS: string;
    function CurrentCPUInfo: TFPCTargetInfoCPU;
    function IsControllerOS(aInfo: TFPCTargetInfoCPU; const aTargetOS: string): boolean;
    procedure RefillCPUList;
    procedure RefillOSList;
    procedure RefillControllerList(const KeepController: string; KeepIfMissing: boolean);
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    function Check: Boolean; override;
    function GetTitle: string; override;
    procedure UpdateWidgetSet(AValue: string = '');
    procedure Setup(ADialog: TAbstractOptionsEditorDialog); override;
    procedure ReadSettings(AOptions: TAbstractIDEOptions); override;
    procedure WriteSettings(AOptions: TAbstractIDEOptions); override;
    class function SupportedOptionsClass: TAbstractIDEOptionsClass; override;
  end;

implementation

{$R *.lfm}

function CaptionToOS(const OS: string): string;
begin
  Result:=LowerCase(OS);
end;

function CaptionToCPU(const CPU: string): string;
begin
  Result:=LowerCase(CPU);
end;

function ProcessorToCaption(const aProcessor: string): string;
// Special treatment for i386 CPUs, others go untouched
begin
  if aProcessor = '' then
    Result := '('+lisDefault+')'
  else if CompareText(aProcessor, '80386') = 0 then
    Result := '386/486 (-Cp80386)'
  else if CompareText(aProcessor, 'pentium') = 0 then
    Result := 'Pentium/Pentium MMX (-CpPENTIUM)'
  else if CompareText(aProcessor, 'pentium2') = 0 then
    Result := 'Pentium Pro/Pentium II/C6x86/K6 (-CpPENTIUM2)'
  else if CompareText(aProcessor, 'pentium3') = 0 then
    Result := 'Pentium III (-CpPENTIUM3)'
  else if CompareText(aProcessor, 'pentium4') = 0 then
    Result := 'Pentium IV (-CpPENTIUM4)'
  else if CompareText(aProcessor, 'pentiumm') = 0 then
    Result := 'Pentium M (-CpPENTIUMM)'
  else
    Result := aProcessor;
end;

function CaptionToProcessor(const aCaption: string): string;
// Special treatment for i386 CPUs, others go untouched
begin
  if aCaption = '('+lisDefault+')' then
    Result := ''
  else if Pos('-Cp80386', aCaption) > 0 then
    Result := '80386'
  else if Pos('-CpPENTIUMM', aCaption) > 0 then
    Result := 'pentiumm'
  else if Pos('-CpPENTIUM4', aCaption) > 0 then
    Result := 'pentium4'
  else if Pos('-CpPENTIUM3', aCaption) > 0 then
    Result := 'pentium3'
  else if Pos('-CpPENTIUM2', aCaption) > 0 then
    Result := 'pentium2'
  else if Pos('-CpPENTIUM', aCaption) > 0 then
    Result := 'pentium'
  else
    Result := aCaption;
end;

{ TCompilerConfigTargetFrame }

constructor TCompilerConfigTargetFrame.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
end;

destructor TCompilerConfigTargetFrame.Destroy;
begin
  inherited Destroy;
end;

function TCompilerConfigTargetFrame.Check: Boolean;
var
  NewDontUseConfigFile, NewCustomConfigFile: Boolean;
  NewConfigFilePath, AdditionalConfig: String;
begin
  //debugln(['TCompilerConfigTargetFrame.ReadSettings ',dbgs(Pointer(FCompOptions)),' ',FCompOptions=Project1.CompilerOptions]);

  NewDontUseConfigFile := not chkConfigFile.Checked;
  NewCustomConfigFile := chkCustomConfigFile.Checked;
  NewConfigFilePath := edtCustomConfigPath.Text;

  if ((NewDontUseConfigFile <> FCompOptions.DontUseConfigFile) or
    (NewCustomConfigFile <> FCompOptions.CustomConfigFile) or
    (NewConfigFilePath <> FCompOptions.ConfigFilePath)) and (not NewDontUseConfigFile) and
    NewCustomConfigFile then
  begin
    // config file options changed
    // and both additional and standard config files are used
    AdditionalConfig := ExtractFilename(edtCustomConfigPath.Text);
    if (CompareFileNames(AdditionalConfig, 'fpc.cfg') = 0) then
    begin
      if IDEMessageDialog(lisCOAmbiguousAdditionalCompilerConfigFile,
        Format(lisCOClickOKIfAreSureToDoThat,
        [BreakString(lisCOWarningTheAdditionalCompilerConfigFileHasTheSameNa,
        60, 0), LineEnding+LineEnding]), mtWarning, [mbOK, mbCancel]) <> mrOk then
      begin
        Result := False;
        exit;
      end;
    end;
  end;

  Result := True;
end;

function TCompilerConfigTargetFrame.GetTitle: string;
begin
  Result := dlgConfigAndTarget;
end;

procedure TCompilerConfigTargetFrame.UpdateWidgetSet(AValue: string);
// Use the value if it is given. Otherwise read IDE macro LCLWidgetType's value.
// This can be called from ModeMatrix with a new value before it is saved.
begin
  if AValue = '' then begin
    AValue := '$(LCLWidgetType)';
    if not IDEMacros.SubstituteMacros(AValue) then
      AValue := '';
  end;
  //debugln(['TCompilerConfigTargetFrame.UpdateWidgetSet ',AValue]);
  CurrentWidgetTypeLabel.Caption := Format(lisCurrentLCLWidgetSet, [AValue]);
end;

procedure TCompilerConfigTargetFrame.UpdateByTargetOS(aTargetOS: string);
begin
  //debugln(['UpdateByTargetOS TargetOS=', aTargetOS]);
  if aTargetOS = '' then
  begin
    aTargetOS := GetCompiledTargetOS;
    //debugln(['UpdateByTargetOS Substituted TargetOS=', aTargetOS]);
  end;
  if StartsText('Win', aTargetOS) then
    chkWin32GraphicApp.Caption := dlgWindowsGUIApp + ' (-WG)'
  else
    chkWin32GraphicApp.Caption := dlgWindowsGUIApp + ' (-WG, '+lisOptionValueIgnored+')';
end;

procedure TCompilerConfigTargetFrame.UpdateByTargetCPU(aTargetCPU: string);
var
  ParsingFrame: TCompilerParsingOptionsFrame;
  sl: TStringListUTF8Fast;
  i: Integer;
  Info: TFPCTargetInfoCPU;
  KeepProc: string;
begin
  if aTargetCPU = '' then
  begin
    aTargetCPU := '$(TargetCPU)';
    if not GlobalMacroList.SubstituteStr(aTargetCPU) then
      raise Exception.CreateFmt(lisCannotSubstituteMacroS, [aTargetCPU]);
  end;

  // Update selection list for target processor (from the -ix cache in available mode, else static)
  KeepProc := TargetProcComboBox.Text;
  sl:=TStringListUTF8Fast.Create;
  Info:=CurrentCPUInfo;
  if OnlyAvailable and (Info<>nil) and (Info.InstructionSets.Count>0) then
    sl.AddStrings(Info.InstructionSets)
  else
    GetTargetProcessors(aTargetCPU,sl);
  sl.Sort;
  sl.Insert(0,'('+lisDefault+')');
  for i:=0 to sl.Count-1 do
    sl[i]:=ProcessorToCaption(sl[i]);
  TargetProcComboBox.Items.Assign(sl);
  sl.Free;
  // restore only if still valid for this CPU; otherwise revert to (Default) (do not insert)
  SelectComboOrDefault(TargetProcComboBox,KeepProc);

  // Update selection list for assembler style
  ParsingFrame := TCompilerParsingOptionsFrame(FDialog.FindEditor(TCompilerParsingOptionsFrame));
  Assert(Assigned(ParsingFrame));
  ParsingFrame.grpAsmStyle.Visible := IsCPUX86(aTargetCPU);
end;

function TCompilerConfigTargetFrame.OnlyAvailable: boolean;
begin
  Result := chkOnlyAvailable.Checked;
end;

function TCompilerConfigTargetFrame.GetQueryCompilerFilename: string;
// The compiler that will build this project (may be a custom trunk/cross build with extra
// controllers); fall back to the IDE default. -P<cpu> dispatches the fpc wrapper.
begin
  Result := '';
  if FCompOptions <> nil then
    Result := FCompOptions.ParsedOpts.GetParsedValue(pcosCompilerPath);
  if Result = '' then
    Result := EnvironmentOptions.GetParsedCompilerFilename;
end;

function TCompilerConfigTargetFrame.GetSelectedTargetCPU: string;
begin
  if TargetCPUComboBox.ItemIndex <= 0 then
    Result := ''
  else
    Result := CaptionToCPU(TargetCPUComboBox.Text);
end;

function TCompilerConfigTargetFrame.GetSelectedTargetOS: string;
begin
  if TargetOSComboBox.ItemIndex <= 0 then
    Result := ''
  else
    Result := CaptionToOS(TargetOSComboBox.Text);
end;

function TCompilerConfigTargetFrame.CurrentCPUInfo: TFPCTargetInfoCPU;
// nil in static mode, for the default CPU, or when the compiler/-ix is unavailable.
var
  CPU: string;
begin
  Result := nil;
  if not OnlyAvailable then Exit;
  CPU := GetSelectedTargetCPU;
  if CPU = '' then Exit;
  Result := TargetInfoCache.GetInfo(GetQueryCompilerFilename, CPU);
end;

function TCompilerConfigTargetFrame.IsControllerOS(aInfo: TFPCTargetInfoCPU;
  const aTargetOS: string): boolean;
// When the compiler maps controllers to their OS targets (the <controllertype><ostarget> children)
// it is the source of truth; the static embedded/freertos list is only a fallback for compilers
// that don't emit that mapping.
var
  OS: string;
begin
  if (aInfo <> nil) and aInfo.ControllerFlagProvided then
    Result := aInfo.OSUsesControllers(aTargetOS)   // compiler is authoritative
  else
  begin
    OS := lowercase(aTargetOS);
    Result := (OS = 'embedded') or (OS = 'freertos'); // legacy fallback
  end;
end;

function TCompilerConfigTargetFrame.EnsureQueryCompilerTrusted: boolean;
// Before running the query compiler (which may be a project-supplied fpc.exe override), verify it
// against Lazarus's trusted-compiler list, mirroring buildmodesmanager.CompilerPathNeedsTrust. The
// unparsed path is used deliberately - resolving macros is itself an attack surface. If the compiler
// is untrusted the user is prompted (this time / always / use default), and the caller reverts the
// checkbox if the prompt is cancelled. Nothing runs the compiler while the dialog is open.
var
  UnparsedPath: string;
begin
  Result := True;
  if FIsPackage or (FCompOptions = nil) then Exit;
  UnparsedPath := FCompOptions.CompilerPath;
  // safe cases: empty or the default macro resolve to the IDE default compiler
  if (UnparsedPath = '') or SameText(UnparsedPath, DefaultCompilerPath) then Exit;
  if CompareFilenames(UnparsedPath, EnvironmentOptions.GetParsedCompilerFilename) = 0 then Exit;
  if EnvironmentOptions.IsCompilerTrusted(UnparsedPath) then Exit;
  // custom, untrusted compiler -> ask
  Result := False;
  case IDEQuestionDialog(lisTrustCompilerCaption,
         Format(lisTheProjectWantsToUseTheCompiler,
                [Project1.GetTitleOrName, LineEnding+LineEnding, UnparsedPath,
                 LineEnding+LineEnding, LineEnding+LineEnding]),
         mtConfirmation,
         [mrYes, lisTrustCompilerThisTime,
          mrAll, lisTrustCompilerAlways,
          mrIgnore, lisUseDefaultCompiler,
          mrCancel, lisCancel], '') of
    mrYes: // trust for this session only
      begin
        EnvironmentOptions.AddSessionTrustedCompiler(UnparsedPath);
        Result := True;
      end;
    mrAll: // trust permanently
      begin
        EnvironmentOptions.AddTrustedCompiler(UnparsedPath);
        EnvironmentOptions.Save(False);
        Result := True;
      end;
    mrIgnore: // fall back to the IDE default compiler for this options set
      begin
        FCompOptions.CompilerPath := DefaultCompilerPath;
        Result := True;
      end;
  end;
end;

procedure TCompilerConfigTargetFrame.SelectComboOrDefault(aCombo: TComboBox;
  const aText: string);
// Select aText only if it is a genuine item of aCombo (case-insensitive); otherwise fall back to
// (Default) at index 0. Unlike SetComboBoxText, this never inserts an unknown value - so a
// selection that is not valid for the current CPU/OS is reverted instead of being silently kept.
var
  idx: integer;
begin
  idx := LazStringUtils.IndexInStringList(aCombo.Items, cstCaseInsensitive, aText);
  if idx < 0 then idx := 0;
  aCombo.ItemIndex := idx;
end;

procedure TCompilerConfigTargetFrame.RevertInvalidSubOptions;
// Net that runs after any dropdown change: snap any fixed-list subordinate combo whose selection is
// no longer a valid item back to (Default), and keep Target Processor / Target Controller mutually
// exclusive. The refills already restore-or-default each combo (see SelectComboOrDefault); this
// guards the cross-combo invariants regardless of which control changed.
begin
  if (TargetProcComboBox.ItemIndex < 0)
  or (LazStringUtils.IndexInStringList(TargetProcComboBox.Items, cstCaseInsensitive,
        TargetProcComboBox.Text) < 0) then
    TargetProcComboBox.ItemIndex := 0;
  if (ControllerComboBox.ItemIndex < 0)
  or (LazStringUtils.IndexInStringList(ControllerComboBox.Items, cstCaseInsensitive,
        ControllerComboBox.Text) < 0) then
    ControllerComboBox.ItemIndex := 0;
  // mutual exclusivity: never both non-default at once (a controller implies its own -Cp)
  if (ControllerComboBox.ItemIndex > 0) and (TargetProcComboBox.ItemIndex > 0) then
    TargetProcComboBox.ItemIndex := 0;
end;

procedure TCompilerConfigTargetFrame.TargetProcComboBoxSelect(Sender: TObject);
// Target Processor and Target Controller are mutually exclusive: choosing a processor clears any
// controller back to (Default).
begin
  if FUpdating then Exit;
  if TargetProcComboBox.ItemIndex > 0 then
  begin
    FUpdating := True;
    try
      ControllerComboBox.ItemIndex := 0;
    finally
      FUpdating := False;
    end;
  end;
  RevertInvalidSubOptions;
end;

procedure TCompilerConfigTargetFrame.ControllerComboBoxSelect(Sender: TObject);
// Mutually exclusive with Target Processor: choosing a controller clears any processor to (Default).
begin
  if FUpdating then Exit;
  if ControllerComboBox.ItemIndex > 0 then
  begin
    FUpdating := True;
    try
      TargetProcComboBox.ItemIndex := 0;
    finally
      FUpdating := False;
    end;
  end;
  RevertInvalidSubOptions;
end;

procedure TCompilerConfigTargetFrame.RefillCPUList;
// In "available" mode narrow the list to the CPUs the configured fpc can actually target
// (its native target plus the cross compilers in its fpc.cfg), asked of fpc itself - no
// assumptions about ppc names/paths. Otherwise the full static list.
var
  sl, Avail: TStringListUTF8Fast;
  KeepCPU, KeepNorm: string;
  s: ShortString;
  UseStatic: boolean;
begin
  KeepCPU := TargetCPUComboBox.Text;
  sl := TStringListUTF8Fast.Create;
  try
    sl.Add('(' + lisDefault + ')');
    UseStatic := True;
    if OnlyAvailable then
    begin
      Avail := TStringListUTF8Fast.Create;
      try
        if GetConfiguredTargetCPUs(GetQueryCompilerFilename, Avail) > 0 then
        begin
          sl.AddStrings(Avail);
          UseStatic := False;
        end;
      finally
        Avail.Free;
      end;
    end;
    if UseStatic then
    begin
      for s in FPCProcessorNames do sl.Add(s);
      for s in Pas2jsProcessorNames do sl.Add(s);
    end
    else
    begin
      // keep the current selection even if detection somehow missed it
      KeepNorm := CaptionToCPU(KeepCPU);
      if (KeepNorm <> '') and (sl.IndexOf(KeepNorm) < 0) then
        sl.Add(KeepNorm);
    end;
    TargetCPUComboBox.Items.Assign(sl);
    SetComboBoxText(TargetCPUComboBox, KeepCPU, cstCaseInsensitive);
    if TargetCPUComboBox.ItemIndex < 0 then TargetCPUComboBox.ItemIndex := 0;
  finally
    sl.Free;
  end;
end;

procedure TCompilerConfigTargetFrame.RefillOSList;
var
  sl: TStringListUTF8Fast;
  KeepOS: string;
  s: ShortString;
  Info: TFPCTargetInfoCPU;
  i: integer;
  UseStatic: boolean;
begin
  KeepOS := TargetOSComboBox.Text;
  sl := TStringListUTF8Fast.Create;
  try
    sl.Add('(' + lisDefault + ')');
    UseStatic := True;
    Info := CurrentCPUInfo;
    if OnlyAvailable and (Info <> nil) and (Info.OSCount > 0) then
    begin
      for i := 0 to Info.OSCount - 1 do
        sl.Add(lowercase(Info.OSes[i].ShortName));
      UseStatic := False;
    end;
    if UseStatic then
    begin
      for s in FPCOperatingSystemCaptions do sl.Add(s);
      for s in Pas2jsPlatformNames do sl.Add(s);
    end;
    TargetOSComboBox.Items.Assign(sl);
    // restore only if still valid for this CPU; otherwise revert to (Default) (do not insert)
    SelectComboOrDefault(TargetOSComboBox, KeepOS);
  finally
    sl.Free;
  end;
end;

procedure TCompilerConfigTargetFrame.RefillControllerList(const KeepController: string;
  KeepIfMissing: boolean);
var
  sl, tmp: TStringListUTF8Fast;
  Info: TFPCTargetInfoCPU;
  DefCap: string;
  InList: boolean;
begin
  DefCap := '(' + lisDefault + ')';
  Info := CurrentCPUInfo;
  sl := TStringListUTF8Fast.Create;
  try
    sl.Add(DefCap);
    if OnlyAvailable and (Info <> nil) and IsControllerOS(Info, GetSelectedTargetOS) then
    begin
      tmp := TStringListUTF8Fast.Create;
      try
        Info.GetControllerNamesForOS(GetSelectedTargetOS, tmp);
        sl.AddStrings(tmp);
      finally
        tmp.Free;
      end;
    end;
    InList := (KeepController = '') or (KeepController = DefCap) or (sl.IndexOf(KeepController) >= 0);
    if (not InList) and KeepIfMissing then
    begin
      sl.Add(KeepController);
      InList := True;
    end;
    ControllerComboBox.Items.Assign(sl);
    if InList and (KeepController <> '') and (KeepController <> DefCap) then
      SetComboBoxText(ControllerComboBox, KeepController, cstCaseInsensitive)
    else
      ControllerComboBox.ItemIndex := 0;
  finally
    sl.Free;
  end;
end;

procedure TCompilerConfigTargetFrame.chkOnlyAvailableClick(Sender: TObject);
// Full refresh, preserving still-valid selections. When switching the query on, verify the query
// compiler is trusted first; if the user declines, switch the checkbox back off (no ambiguity) and
// fall through to a normal static refresh.
begin
  if chkOnlyAvailable.Checked and not EnsureQueryCompilerTrusted then
  begin
    chkOnlyAvailable.OnClick := nil;
    chkOnlyAvailable.Checked := False;
    chkOnlyAvailable.OnClick := @chkOnlyAvailableClick;
  end;
  RefillCPUList;
  RefillOSList;
  UpdateByTargetCPU(GetSelectedTargetCPU);
  RefillControllerList(ControllerComboBox.Text, False);
  RevertInvalidSubOptions;
end;

procedure TCompilerConfigTargetFrame.FillSubTargetComboBox(UseSubTarget: string);
var
  sl: TStringListUTF8Fast;
  aCache: TFPCUnitSetCache;
  Cfg: TPCTargetConfigCache;
  CfgFiles: TPCConfigFileStateList;
  i, j: Integer;
  aFilename, Dir, SubTarget, CfgFilename, Prefix: String;
  SearchedDirs, Files: TStrings;
begin
  sl:=TStringListUTF8Fast.Create;
  try
    sl.Assign(InputHistories.HistoryLists.GetList('Subtarget',true,rltCaseInsensitive));
    // represent "no subtarget" as the (Default) sentinel rather than an empty entry
    for i := sl.Count-1 downto 0 do
      if sl[i] = '' then sl.Delete(i);

    // search for possible subtargets
    // fpc searches subtarget configs in the same directories it searches for normal configs
    // codetools has the list of searched cfg files as reported by fpc
    aCache:=CodeToolBoss.GetUnitSetForDirectory('');
    if aCache<>nil then begin
      Cfg:=aCache.GetConfigCache(false);
      if Cfg<>nil then begin
        CfgFiles:=Cfg.ConfigFiles;
        if CfgFiles<>nil then begin
          SearchedDirs:=TStringListUTF8Fast.Create;
          Files:=TStringListUTF8Fast.Create;
          try
            // iterate all cfg files reported by fpc
            for i:=0 to CfgFiles.Count-1 do begin
              CfgFilename:=CfgFiles[i].Filename;
              aFilename:=ExtractFileNameOnly(CfgFilename);
              if StartsStr('.fpc',aFilename) then
                Prefix:='.fpc-'
              else
                Prefix:='fpc-';
              Dir:=ExtractFilePath(CfgFilename);
              if SearchedDirs.IndexOf(Dir)>=0 then continue;
              SearchedDirs.Add(Dir);
              Files.Clear;
              // search for prefix<subtarget>.cfg files
              CodeToolBoss.DirectoryCachePool.GetListing(Dir,Files,false);
              if Files<>nil then begin
                for j:=0 to Files.Count-1 do begin
                  aFilename:=Files[j];
                  if CompareFileExt(aFilename,'cfg')<>0 then continue;
                  if not AnsiStartsStr(Prefix,aFilename) then continue;
                  SubTarget:=lowercase(copy(ExtractFileNameOnly(aFilename),length(Prefix)+1,length(aFilename)));
                  if sl.IndexOf(SubTarget)>=0 then continue;
                  sl.Add(SubTarget);
                end;
              end;
            end;
          finally
            SearchedDirs.Free;
            Files.Free;
          end;
        end;
      end;
    end;
    //debugln(['TCompilerConfigTargetFrame.FillSubTargetComboBox UseSubTarget="',UseSubTarget,'" Candidates=[',sl.Text,']']);
    sl.Insert(0,'('+lisDefault+')'); // (Default) at the top = emit no -t
    with SubtargetComboBox do begin
      Items.BeginUpdate;
      Items.Assign(sl);
      if UseSubTarget = '' then
        SetComboBoxText(SubtargetComboBox,'('+lisDefault+')',cstCaseInsensitive)
      else
        SetComboBoxText(SubtargetComboBox,UseSubTarget,cstCaseInsensitive);
      if ItemIndex < 0 then ItemIndex := 0;
      Items.EndUpdate;
    end;
    //debugln(['TCompilerConfigTargetFrame.FillSubTargetComboBox SubtargetComboBox: Text="',SubtargetComboBox.Text,'" Index=',SubtargetComboBox.ItemIndex,' Items=[',SubtargetComboBox.Items.Text,']']);
  finally
    sl.Free;
  end;
end;

procedure TCompilerConfigTargetFrame.Setup(ADialog: TAbstractOptionsEditorDialog);
var
  s: ShortString;
  List: TStringList;
begin
  List:=TStringList.Create;
  try
    //debugln(['TCompilerConfigTargetFrame.Setup ']);
    FDialog := ADialog;
    // Config
    grbConfigFile.Caption := dlgConfigFiles;
    chkConfigFile.Caption := dlgUseFpcCfg + ' ('+lisIfNotChecked+' -n)';
    chkWriteConfigFile.Caption := lisWriteConfigInsteadOfCommandLineParameters+' (@)';
    edtWriteConfigFilePath.Text := '';
    chkCustomConfigFile.Caption := dlgUseCustomConfig + ' (@)';
    edtCustomConfigPath.Text := '';

    // Target platform
    grbTargetPlatform.Caption := dlgTargetPlatform;
    lblTargetOS.Caption := dlgTargetOS + ' (-T)';
    List.Clear;
    List.Add('(' + lisDefault + ')');
    for s in FPCOperatingSystemCaptions do
      List.Add(s);
    for s in Pas2jsPlatformNames do
      List.Add(s);
    with TargetOSComboBox do
    begin
      Items.Assign(List);
      ItemIndex := 0;
    end;

    // Target CPU
    lblTargetCPU.Caption := dlgTargetCPUFamily + ' (-P)';
    List.Clear;
    List.Add('(' + lisDefault + ')');
    for s in FPCProcessorNames do
      List.Add(s);
    for s in Pas2jsProcessorNames do
      List.Add(s);
    with TargetCPUComboBox do
    begin
      Items.Assign(List);
      ItemIndex := 0;
    end;

    // Target processor
    lblTargetProc.Caption := dlgTargetProc+' (-Cp)';
    // Target-specific options
    grbTargetOptions.Caption := dlgTargetSpecificOptions;
    chkWin32GraphicApp.Caption := dlgWindowsGUIApp + ' (-WG)';
    // WidgetSet
    LCLWidgetTypeLabel.Caption := lisSelectAnotherLCLWidgetSet;

    // Subtarget
    lblSubtarget.Caption := lisSubtarget+' (-t)';

    // Controller / available-configs toggle
    lblController.Caption := lisTargetController + ' (-Wp)';
    chkOnlyAvailable.Caption := lisQueryCompilerForTargets;
    chkOnlyAvailable.Hint := lisQueryCompilerForTargetsHint;
    chkOnlyAvailable.ShowHint := True;
  finally
    List.Free;
  end;
end;

procedure TCompilerConfigTargetFrame.ReadSettings(AOptions: TAbstractIDEOptions);
var
  i: Integer;
  PkgDep: TPkgDependency;
begin
  FCompOptions:=AOptions as TBaseCompilerOptions;
  FIsPackage:=FCompOptions is TPkgCompilerOptions;
  //debugln(['TCompilerConfigTargetFrame.ReadSettings ',dbgs(Pointer(FCompOptions)),' ',FCompOptions=Project1.CompilerOptions]);

  with FCompOptions do
  begin
    chkConfigFile.Checked := not DontUseConfigFile;
    chkWriteConfigFile.Checked := WriteConfigFile;
    edtWriteConfigFilePath.Enabled:= WriteConfigFile;
    edtWriteConfigFilePath.Text := WriteConfigFilePath;
    chkCustomConfigFile.Checked := CustomConfigFile;
    edtCustomConfigPath.Enabled := chkCustomConfigFile.Checked;
    edtCustomConfigPath.Text := ConfigFilePath;
    if fIsPackage then begin
      grbTargetPlatform.Visible:=false;
      TargetOSComboBox.ItemIndex := 0;
      TargetOSComboBox.Text := 'default';
      TargetCPUComboBox.ItemIndex := 0;
      TargetCPUComboBox.Text := 'default';
      TargetProcComboBox.Text := 'default';
      SubtargetComboBox.Text := 'default';
      ControllerComboBox.Text := 'default';
      CurrentWidgetTypeLabel.Visible:=false;
      LCLWidgetTypeLabel.Visible:=false;
    end else begin
      grbTargetPlatform.Visible:=true;
      // Target OS
      i := TargetOSComboBox.Items.IndexOf(TargetOS);
      if i < 0 then
        i := 0;  // 0 is default
      TargetOSComboBox.ItemIndex := i;
      // Target CPU family
      i := TargetCPUComboBox.Items.IndexOf(TargetCPU);
      if i < 0 then
        i := 0;  // 0 is default
      TargetCPUComboBox.ItemIndex := i;
      // Target Processor
      UpdateByTargetCPU(TargetCPU);
      UpdateByTargetOS(TargetOS);
      TargetProcComboBox.Text := ProcessorToCaption(TargetProcessor);
      // SubTarget
      FillSubTargetComboBox(Subtarget);

      // Controller (MCU) - the query toggle is a sticky IDE-wide preference (defaults off).
      // If it was persisted on but the project's query compiler is untrusted this session, prompt
      // once (safe here - nothing runs the compiler yet) and turn it off if the user declines.
      chkOnlyAvailable.OnClick := nil;
      chkOnlyAvailable.Checked := MiscellaneousOptions.QueryCompilerForTargets;
      if chkOnlyAvailable.Checked and not EnsureQueryCompilerTrusted then
        chkOnlyAvailable.Checked := False;
      chkOnlyAvailable.OnClick := @chkOnlyAvailableClick;
      if OnlyAvailable then
      begin
        RefillCPUList;
        RefillOSList;
        UpdateByTargetCPU(GetSelectedTargetCPU);
      end;
      RefillControllerList(Controller, True);

      PkgDep:=TProjectCompilerOptions(AOptions).LazProject.FindDependencyByName('LCL');
      CurrentWidgetTypeLabel.Visible:=Assigned(PkgDep);
      LCLWidgetTypeLabel.Visible:=Assigned(PkgDep);
    end;
    chkWin32GraphicApp.Checked := Win32GraphicApp;
    chkWin32GraphicApp.Enabled := NeedsLinkerOpts;
  end;

  UpdateWidgetSet;
end;

procedure TCompilerConfigTargetFrame.WriteSettings(AOptions: TAbstractIDEOptions);
var
  CurOptions: TBaseCompilerOptions;
  NewTargetOS: string;
  NewTargetCPU: string;
begin
  //debugln(['TCompilerConfigTargetFrame.WriteSettings ',DbgSName(AOptions)]);
  CurOptions:=AOptions as TBaseCompilerOptions;
  with CurOptions do
  begin
    DontUseConfigFile := not chkConfigFile.Checked;
    WriteConfigFile := chkWriteConfigFile.Checked;
    WriteConfigFilePath := edtWriteConfigFilePath.Text;
    CustomConfigFile := chkCustomConfigFile.Checked;
    ConfigFilePath := edtCustomConfigPath.Text;
    if not fIsPackage then
    begin
      NewTargetOS := TargetOSComboBox.Text;
      if TargetOSComboBox.Items.IndexOf(NewTargetOS) <= 0 then
        NewTargetOS := '';
      TargetOS := CaptionToOS(NewTargetOS);
      NewTargetCPU := TargetCPUComboBox.Text;
      if TargetCPUComboBox.Items.IndexOf(NewTargetCPU) <= 0 then
        NewTargetCPU := '';
      TargetCPU := CaptionToCPU(NewTargetCPU);
      TargetProcessor := CaptionToProcessor(TargetProcComboBox.Text);
      if (SubtargetComboBox.Text = '') or (SubtargetComboBox.Text = '('+lisDefault+')') then
        Subtarget := ''
      else
        Subtarget := lowercase(SubtargetComboBox.Text);
      if ControllerComboBox.ItemIndex <= 0 then
        Controller := ''
      else
        Controller := ControllerComboBox.Text;
    end;
    Win32GraphicApp := chkWin32GraphicApp.Checked;
  end;
  // persist the query toggle as a sticky IDE-wide preference. This dialog does not own
  // MiscellaneousOptions, so write the file out ourselves when it changes.
  if MiscellaneousOptions.QueryCompilerForTargets <> chkOnlyAvailable.Checked then
  begin
    MiscellaneousOptions.QueryCompilerForTargets := chkOnlyAvailable.Checked;
    MiscellaneousOptions.Save;
  end;
end;

procedure TCompilerConfigTargetFrame.chkCustomConfigFileClick(Sender: TObject);
begin
  edtCustomConfigPath.Enabled := chkCustomConfigFile.Checked;
end;

procedure TCompilerConfigTargetFrame.chkWriteConfigFileClick(Sender: TObject);
begin
  edtWriteConfigFilePath.Enabled := chkWriteConfigFile.Checked;
end;

procedure TCompilerConfigTargetFrame.TargetOSComboBoxSelect(Sender: TObject);
var
  cb: TComboBox;
  s: TCaption;
begin
  cb := Sender as TComboBox;
  if cb.ItemIndex = 0 then
    s :=''
  else
    s := cb.Text;
  UpdateByTargetOS(s);
  RefillControllerList(ControllerComboBox.Text, False);
  RevertInvalidSubOptions;
end;

procedure TCompilerConfigTargetFrame.TargetCPUComboBoxSelect(Sender: TObject);
var
  cb: TComboBox;
  s: String;
begin
  cb := Sender as TComboBox;
  if cb.ItemIndex = 0 then
    s :=''
  else
    s := cb.Text;
  UpdateByTargetCPU(s);
  RefillOSList;
  RefillControllerList(ControllerComboBox.Text, False);
  RevertInvalidSubOptions;
end;

procedure TCompilerConfigTargetFrame.LCLWidgetTypeLabelClick(Sender: TObject);
begin
  // Make sure the "Additions And Overrides" page is visible, then move there.
  FDialog.ResetFilter;
  FDialog.OpenEditor(GroupCompiler,CompilerOptionsAdditionsAndOverrides);
end;

procedure TCompilerConfigTargetFrame.LCLWidgetTypeLabelMouseEnter(Sender: TObject);
begin
  (Sender as TLabel).Font.Underline := True;
  (Sender as TLabel).Font.Color := clRed;
end;

procedure TCompilerConfigTargetFrame.LCLWidgetTypeLabelMouseLeave(Sender: TObject);
begin
  (Sender as TLabel).Font.Underline := False;
  (Sender as TLabel).Font.Color := clBlue;
end;

class function TCompilerConfigTargetFrame.SupportedOptionsClass: TAbstractIDEOptionsClass;
begin
  Result := TBaseCompilerOptions;
end;

initialization
  RegisterIDEOptionsEditor(GroupCompiler, TCompilerConfigTargetFrame,
    CompilerOptionsConfigTarget);
  RegisterIDEOptionsEditor(GroupPkgCompiler, TCompilerConfigTargetFrame,
    CompilerOptionsConfigTarget);

end.

