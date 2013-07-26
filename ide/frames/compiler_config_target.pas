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
  Classes, SysUtils, FileUtil, Controls, Dialogs, StdCtrls, LCLProc, IDEOptionsIntf,
  IDEDialogs, CompilerOptions, LazarusIDEStrConsts, PackageDefs;

type

  { TCompilerConfigTargetFrame }

  TCompilerConfigTargetFrame = class(TAbstractIDEOptionsEditor)
    chkConfigFile: TCheckBox;
    chkCustomConfigFile: TCheckBox;
    edtConfigPath: TEdit;
    grpConfigFile: TGroupBox;
    grpTargetPlatform: TGroupBox;
    lblTargetCPU: TLabel;
    lblTargetOS: TLabel;
    lblTargetProcessorProc: TLabel;
    TargetCPUComboBox: TComboBox;
    TargetOSComboBox: TComboBox;
    TargetProcessorProcComboBox: TComboBox;
    procedure chkCustomConfigFileClick(Sender: TObject);
  private
    FCompOptions: TBaseCompilerOptions;
    FIsPackage: boolean;
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

function ProcessorToCaption(const Processor: string): string;
begin
  if CompareText(Processor, '80386') = 0 then
    Result := '386/486 (-Op80386)'
  else if CompareText(Processor, 'pentium') = 0 then
    Result := 'Pentium/Pentium MMX (-OpPENTIUM)'
  else if CompareText(Processor, 'pentium2') = 0 then
    Result := 'Pentium Pro/Pentium II/C6x86/K6 (-OpPENTIUM2)'
  else if CompareText(Processor, 'pentium3') = 0 then
    Result := 'Pentium III (-OpPENTIUM3)'
  else if CompareText(Processor, 'pentium4') = 0 then
    Result := 'Pentium IV (-OpPENTIUM4)'
  else if CompareText(Processor, 'pentiumm') = 0 then
    Result := 'Pentium M (-OpPENTIUMM)'
  else
    Result := '(' + lisDefault + ')';
end;

function CaptionToProcessor(const Caption: string): string;
begin
  if Pos('-Op80386', Caption) > 0 then
    Result := '80386'
  else if Pos('-OpPENTIUMM', Caption) > 0 then
    Result := 'pentiumm'
  else if Pos('-OpPENTIUM4', Caption) > 0 then
    Result := 'pentium4'
  else if Pos('-OpPENTIUM3', Caption) > 0 then
    Result := 'pentium3'
  else if Pos('-OpPENTIUM2', Caption) > 0 then
    Result := 'pentium2'
  else if Pos('-OpPENTIUM', Caption) > 0 then
    Result := 'pentium'
  else
    Result := '';
end;


{ TCompilerConfigTargetFrame }

procedure TCompilerConfigTargetFrame.chkCustomConfigFileClick(Sender: TObject);
begin
  edtConfigPath.Enabled := chkCustomConfigFile.Checked;
end;

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

procedure TCompilerConfigTargetFrame.Setup(ADialog: TAbstractOptionsEditorDialog);
begin
  // Config
  grpConfigFile.Caption := dlgConfigFiles;
  chkConfigFile.Caption := dlgUseFpcCfg + ' (If not checked: -n)';
  chkCustomConfigFile.Caption := dlgUseCustomConfig + ' (@)';
  edtConfigPath.Text := '';
  // Target
  grpTargetPlatform.Caption := dlgTargetPlatform;
  lblTargetOS.Caption := dlgTargetOS + ' (-T)';

  with TargetOSComboBox do
  begin
    with Items do
    begin
      Add('(' + lisDefault + ')');
      Add('Darwin');
      Add('FreeBSD');
      Add('Linux');
      Add('NetBSD');
      Add('OpenBSD');
      Add('Solaris');
      Add('Win32');
      Add('Win64');
      Add('WinCE');
      Add('aix');
      Add('amiga');
      Add('android');
      Add('atari');
      Add('beos');
      Add('embedded');
      Add('emx');
      Add('gba');
      Add('go32v2');
      Add('haiku');
      Add('iphonesim');
      Add('java');
      Add('macos');
      Add('morphos');
      Add('nds');
      Add('netware');
      Add('netwlibc');
      Add('os2');
      Add('palmos');
      Add('qnx');
      Add('symbian');
      Add('watcom');
      Add('wdosx');
    end;
    ItemIndex := 0;
  end;

  lblTargetCPU.Caption := dlgTargetCPUFamily + ' (-P)';

  with TargetCPUComboBox do
  begin
    with Items do
    begin
      Add('(' + lisDefault + ')');
      Add('arm');
      Add('i386');
      Add('m68k');
      Add('powerpc');
      Add('sparc');
      Add('x86_64');
      Add('mipsel');
      Add('mips');
      Add('jvm');
    end;
    ItemIndex := 0;
  end;

  lblTargetProcessorProc.Caption := dlgTargetProc;

  with TargetProcessorProcComboBox do
  begin
    with Items do
    begin
      Clear;
      Add(ProcessorToCaption(''));
      Add(ProcessorToCaption('80386'));
      Add(ProcessorToCaption('Pentium'));
      Add(ProcessorToCaption('Pentium2'));
      Add(ProcessorToCaption('Pentium3'));
      Add(ProcessorToCaption('Pentium4'));
      Add(ProcessorToCaption('PentiumM'));
    end;
    ItemIndex := 0;
  end;
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
      TargetProcessorProcComboBox.Text := 'default';
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
      TargetProcessorProcComboBox.Text := ProcessorToCaption(TargetProcessor);
    end;
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
      TargetProcessor := CaptionToProcessor(TargetProcessorProcComboBox.Text);
    end;
  end;
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

