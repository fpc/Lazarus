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
  LazFileUtils, LazStringUtils,
  // CodeTools
  DefineTemplates,
  // IdeIntf
  IDEOptionsIntf, IDEOptEditorIntf, MacroIntf, IDEDialogs,
  // IDE
  CompilerOptions, LazarusIDEStrConsts, TransferMacros, PackageDefs, Project,
  compiler_parsing_options;

type

  { TCompilerConfigTargetFrame }

  TCompilerConfigTargetFrame = class(TAbstractIDEOptionsEditor)
    chkConfigFile: TCheckBox;
    chkCustomConfigFile: TCheckBox;
    chkWin32GraphicApp: TCheckBox;
    edtConfigPath: TEdit;
    grbTargetOptions: TGroupBox;
    grbConfigFile: TGroupBox;
    grbTargetPlatform: TGroupBox;
    CurrentWidgetTypeLabel: TLabel;
    lblTargetCPU: TLabel;
    lblTargetOS: TLabel;
    lblTargetProc: TLabel;
    LCLWidgetTypeLabel: TLabel;
    TargetCPUComboBox: TComboBox;
    TargetOSComboBox: TComboBox;
    TargetProcComboBox: TComboBox;
    procedure chkCustomConfigFileClick(Sender: TObject);
    procedure TargetOSComboBoxSelect(Sender: TObject);
    procedure TargetCPUComboBoxSelect(Sender: TObject);
    procedure LCLWidgetTypeLabelClick(Sender: TObject);
    procedure LCLWidgetTypeLabelMouseEnter(Sender: TObject);
    procedure LCLWidgetTypeLabelMouseLeave(Sender: TObject);
  private
    FDialog: TAbstractOptionsEditorDialog;
    FCompOptions: TBaseCompilerOptions;
    FIsPackage: boolean;
    procedure UpdateByTargetOS(aTargetOS: string);
    procedure UpdateByTargetCPU(aTargetCPU: string);
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
  NewDontUseConfigFile: Boolean;
  NewCustomConfigFile: Boolean;
  NewConfigFilePath: String;
  AdditionalConfig: String;
begin
  //debugln(['TCompilerConfigTargetFrame.ReadSettings ',dbgs(Pointer(FCompOptions)),' ',FCompOptions=Project1.CompilerOptions]);

  NewDontUseConfigFile := not chkConfigFile.Checked;
  NewCustomConfigFile := chkCustomConfigFile.Checked;
  NewConfigFilePath := edtConfigPath.Text;

  if ((NewDontUseConfigFile <> FCompOptions.DontUseConfigFile) or
    (NewCustomConfigFile <> FCompOptions.CustomConfigFile) or
    (NewConfigFilePath <> FCompOptions.ConfigFilePath)) and (not NewDontUseConfigFile) and
    NewCustomConfigFile then
  begin
    // config file options changed
    // and both additional and standard config files are used
    AdditionalConfig := ExtractFilename(edtConfigPath.Text);
    if (CompareFileNames(AdditionalConfig, 'fpc.cfg') = 0) or
      (CompareFileNames(AdditionalConfig, 'ppc386.cfg') = 0) then
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
  if aTargetOS = '' then
  begin
    aTargetOS := '$(TargetOS)';
    if not GlobalMacroList.SubstituteStr(aTargetOS) then
      raise Exception.CreateFmt(lisCannotSubstituteMacroS, [aTargetOS]);
  end;

  if AnsiStartsText('Win', aTargetOS) then
    chkWin32GraphicApp.Caption := dlgWin32GUIApp + ' (-WG)'
  else
    chkWin32GraphicApp.Caption := dlgWin32GUIApp + ' (-WG, '+
      lisOptionValueIgnored+')';
end;

procedure TCompilerConfigTargetFrame.UpdateByTargetCPU(aTargetCPU: string);
var
  ParsingFrame: TCompilerParsingOptionsFrame;
  sl: TStringList;
  i: Integer;
begin
  if aTargetCPU = '' then
  begin
    aTargetCPU := '$(TargetCPU)';
    if not GlobalMacroList.SubstituteStr(aTargetCPU) then
      raise Exception.CreateFmt(lisCannotSubstituteMacroS, [aTargetCPU]);
  end;

  // Update selection list for target processor
  sl:=TStringList.Create;
  GetTargetProcessors(aTargetCPU,sl);
  sl.Sort;
  sl.Insert(0,'('+lisDefault+')');
  for i:=0 to sl.Count-1 do
    sl[i]:=ProcessorToCaption(sl[i]);
  TargetProcComboBox.Items.Assign(sl);
  sl.Free;
  TargetProcComboBox.ItemIndex := 0;

  // Update selection list for assembler style
  ParsingFrame := TCompilerParsingOptionsFrame(FDialog.FindEditor(TCompilerParsingOptionsFrame));
  Assert(Assigned(ParsingFrame));
  ParsingFrame.grpAsmStyle.Visible := (aTargetCPU='i386') or (aTargetCPU='x86_64');
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
    chkCustomConfigFile.Caption := dlgUseCustomConfig + ' (@)';
    edtConfigPath.Text := '';

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

    // Target CPU
    lblTargetProc.Caption := dlgTargetProc+' (-Cp)';
    // Target-specific options
    grbTargetOptions.Caption := dlgTargetSpecificOptions;
    chkWin32GraphicApp.Caption := dlgWin32GUIApp + ' (-WG)';
    // WidgetSet
    LCLWidgetTypeLabel.Caption := lisSelectAnotherLCLWidgetSet;
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
    chkCustomConfigFile.Checked := CustomConfigFile;
    edtConfigPath.Enabled := chkCustomConfigFile.Checked;
    edtConfigPath.Text := ConfigFilePath;
    if fIsPackage then begin
      grbTargetPlatform.Visible:=false;
      TargetOSComboBox.ItemIndex := 0;
      TargetOSComboBox.Text := 'default';
      TargetCPUComboBox.ItemIndex := 0;
      TargetCPUComboBox.Text := 'default';
      TargetProcComboBox.Text := 'default';
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
    CustomConfigFile := chkCustomConfigFile.Checked;
    ConfigFilePath := edtConfigPath.Text;
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
    end;
    Win32GraphicApp := chkWin32GraphicApp.Checked;
  end;
end;

procedure TCompilerConfigTargetFrame.chkCustomConfigFileClick(Sender: TObject);
begin
  edtConfigPath.Enabled := chkCustomConfigFile.Checked;
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

