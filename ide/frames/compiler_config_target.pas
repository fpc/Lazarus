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
 *   Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.        *
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
  Classes, SysUtils, strutils, FileUtil, Controls, Dialogs, StdCtrls, LCLProc,
  IDEOptionsIntf, IDEDialogs, CompilerOptions, LazarusIDEStrConsts,
  TransferMacros, PackageDefs, compiler_parsing_options;

type

  { TCompilerConfigTargetFrame }

  TCompilerConfigTargetFrame = class(TAbstractIDEOptionsEditor)
    chkConfigFile: TCheckBox;
    chkCustomConfigFile: TCheckBox;
    chkWin32GraphicApp: TCheckBox;
    edtConfigPath: TEdit;
    grbTargetOptions: TGroupBox;
    grpConfigFile: TGroupBox;
    grpTargetPlatform: TGroupBox;
    lblTargetCPU: TLabel;
    lblTargetOS: TLabel;
    lblTargetProc: TLabel;
    TargetCPUComboBox: TComboBox;
    TargetOSComboBox: TComboBox;
    TargetProcComboBox: TComboBox;
    procedure chkCustomConfigFileClick(Sender: TObject);
    procedure TargetOSComboBoxSelect(Sender: TObject);
    procedure TargetCPUComboBoxSelect(Sender: TObject);
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
    Result := '386/486 (-Op80386)'
  else if CompareText(aProcessor, 'pentium') = 0 then
    Result := 'Pentium/Pentium MMX (-OpPENTIUM)'
  else if CompareText(aProcessor, 'pentium2') = 0 then
    Result := 'Pentium Pro/Pentium II/C6x86/K6 (-OpPENTIUM2)'
  else if CompareText(aProcessor, 'pentium3') = 0 then
    Result := 'Pentium III (-OpPENTIUM3)'
  else if CompareText(aProcessor, 'pentium4') = 0 then
    Result := 'Pentium IV (-OpPENTIUM4)'
  else if CompareText(aProcessor, 'pentiumm') = 0 then
    Result := 'Pentium M (-OpPENTIUMM)'
  else
    Result := aProcessor;
end;

function CaptionToProcessor(const aCaption: string): string;
// Special treatment for i386 CPUs, others go untouched
begin
  if aCaption = '('+lisDefault+')' then
    Result := ''
  else if Pos('-Op80386', aCaption) > 0 then
    Result := '80386'
  else if Pos('-OpPENTIUMM', aCaption) > 0 then
    Result := 'pentiumm'
  else if Pos('-OpPENTIUM4', aCaption) > 0 then
    Result := 'pentium4'
  else if Pos('-OpPENTIUM3', aCaption) > 0 then
    Result := 'pentium3'
  else if Pos('-OpPENTIUM2', aCaption) > 0 then
    Result := 'pentium2'
  else if Pos('-OpPENTIUM', aCaption) > 0 then
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

procedure TCompilerConfigTargetFrame.UpdateByTargetOS(aTargetOS: string);
var
  DbgMsg: String;
begin
  DbgMsg := '';
  if aTargetOS = '' then
  begin
    aTargetOS := '$(TargetOS)';
    if not GlobalMacroList.SubstituteStr(aTargetOS) then
      raise Exception.CreateFmt('Cannot substitute macro "%s".', [aTargetOS]);
    DbgMsg := ' (got by using $(TargetOS) macro)';
  end;
  DebugLn(['TCompilerConfigTargetFrame.UpdateTargetSpecific: TargetOS=',aTargetOS,DbgMsg]);
  // Now hide/show the whole GroupBox because there is only one setting.
  grbTargetOptions.Visible := AnsiStartsText('Win', aTargetOS);
end;

procedure TCompilerConfigTargetFrame.UpdateByTargetCPU(aTargetCPU: string);
var
  IsIntel: Boolean;

  procedure Arm(aList: TStrings);
  begin
    aList.Add('ARMV3');
    aList.Add('ARMV4');
    aList.Add('ARMV5');
    aList.Add('ARMV6');
    aList.Add('ARMV7');
    aList.Add('ARMV7M');
    aList.Add('CORTEXM3');
  end;

  procedure Intel_i386(aList: TStrings);
  begin
    IsIntel := True;
    aList.Add(ProcessorToCaption('80386'));
    aList.Add(ProcessorToCaption('Pentium'));
    aList.Add(ProcessorToCaption('Pentium2'));
    aList.Add(ProcessorToCaption('Pentium3'));
    aList.Add(ProcessorToCaption('Pentium4'));
    aList.Add(ProcessorToCaption('PentiumM'));
  end;

  procedure Intel_x86_64(aList: TStrings);
  begin
    IsIntel := True;
    aList.Add('ATHLON64');
  end;

  procedure PowerPc(aList: TStrings);
  begin
    aList.Add('604');
    aList.Add('750');
    aList.Add('7400');
    aList.Add('970');
  end;

  procedure Sparc(aList: TStrings);
  begin
    aList.Add('SPARC V7');
    aList.Add('SPARC V8');
    aList.Add('SPARC V9');
  end;

  procedure Mips(aList: TStrings);
  begin
    aList.Add('mips1');
    aList.Add('mips2');
    aList.Add('mips3');
    aList.Add('mips4');
    aList.Add('mips5');
    aList.Add('mips32');
    aList.Add('mips32r2');
  end;

var
  DbgMsg: String;
  ParsingFrame: TCompilerParsingOptionsFrame;
begin
  IsIntel := False;
  DbgMsg := '';
  if aTargetCPU = '' then
  begin
    aTargetCPU := '$(TargetCPU)';
    if not GlobalMacroList.SubstituteStr(aTargetCPU) then
      raise Exception.CreateFmt('Cannot substitute macro "%s".', [aTargetCPU]);
    DbgMsg := ' (got by using $(TargetCPU) macro)';
  end;
  DebugLn(['TCompilerConfigTargetFrame.UpdateTargetProcessorList: TargetCPU=',aTargetCPU,DbgMsg]);

  // Update selection list for target processor
  TargetProcComboBox.Clear;
  TargetProcComboBox.Items.Add('('+lisDefault+')');
  case aTargetCPU of
    'arm'    : Arm(TargetProcComboBox.Items);
    'i386'   : Intel_i386(TargetProcComboBox.Items);
    'm68k'   : begin end;
    'powerpc': PowerPc(TargetProcComboBox.Items);
    'sparc'  : Sparc(TargetProcComboBox.Items);
    'x86_64' : Intel_x86_64(TargetProcComboBox.Items);
    'mipsel' : Mips(TargetProcComboBox.Items);
    'mips'   : Mips(TargetProcComboBox.Items);
    'jvm'    : begin end;
  end;
  TargetProcComboBox.ItemIndex := 0;

  // Update selection list for assembler style
  ParsingFrame := TCompilerParsingOptionsFrame(FDialog.FindEditor(TCompilerParsingOptionsFrame));
  Assert(Assigned(ParsingFrame));
  ParsingFrame.grpAsmStyle.Visible := IsIntel;
end;

procedure TCompilerConfigTargetFrame.Setup(ADialog: TAbstractOptionsEditorDialog);
begin
  FDialog := ADialog;

  // Config
  grpConfigFile.Caption := dlgConfigFiles;
  chkConfigFile.Caption := dlgUseFpcCfg + ' (If not checked: -n)';
  chkCustomConfigFile.Caption := dlgUseCustomConfig + ' (@)';
  edtConfigPath.Text := '';

  // Target platform
  grpTargetPlatform.Caption := dlgTargetPlatform;
  lblTargetOS.Caption := dlgTargetOS + ' (-T)';
  with TargetOSComboBox do
  begin
    Items.Add('(' + lisDefault + ')');
    Items.Add('Darwin');
    Items.Add('FreeBSD');
    Items.Add('Linux');
    Items.Add('NetBSD');
    Items.Add('OpenBSD');
    Items.Add('Solaris');
    Items.Add('Win32');
    Items.Add('Win64');
    Items.Add('WinCE');
    Items.Add('aix');
    Items.Add('amiga');
    Items.Add('android');
    Items.Add('atari');
    Items.Add('beos');
    Items.Add('embedded');
    Items.Add('emx');
    Items.Add('gba');
    Items.Add('go32v2');
    Items.Add('haiku');
    Items.Add('iphonesim');
    Items.Add('java');
    Items.Add('macos');
    Items.Add('morphos');
    Items.Add('nds');
    Items.Add('netware');
    Items.Add('netwlibc');
    Items.Add('os2');
    Items.Add('palmos');
    Items.Add('qnx');
    Items.Add('symbian');
    Items.Add('watcom');
    Items.Add('wdosx');
    ItemIndex := 0;
  end;

  lblTargetCPU.Caption := dlgTargetCPUFamily + ' (-P)';
  with TargetCPUComboBox do
  begin
    Items.Add('(' + lisDefault + ')');
    Items.Add('arm');
    Items.Add('i386');
    Items.Add('m68k');
    Items.Add('powerpc');
    Items.Add('sparc');
    Items.Add('x86_64');
    Items.Add('mipsel');
    Items.Add('mips');
    Items.Add('jvm');
    ItemIndex := 0;
  end;

  lblTargetProc.Caption := dlgTargetProc;

  // Target options
  grbTargetOptions.Caption := dlgTargetSpecificOptions;
  chkWin32GraphicApp.Caption := dlgWin32GUIApp + ' (-WG)';
end;

procedure TCompilerConfigTargetFrame.ReadSettings(AOptions: TAbstractIDEOptions);
var
  i: Integer;
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
      grpTargetPlatform.Visible:=false;
      TargetOSComboBox.ItemIndex := 0;
      TargetOSComboBox.Text := 'default';
      TargetCPUComboBox.ItemIndex := 0;
      TargetCPUComboBox.Text := 'default';
      TargetProcComboBox.Text := 'default';
    end else begin
      grpTargetPlatform.Visible:=true;
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
    end;
    chkWin32GraphicApp.Checked := Win32GraphicApp;
    chkWin32GraphicApp.Enabled := NeedsLinkerOpts;
  end;
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

