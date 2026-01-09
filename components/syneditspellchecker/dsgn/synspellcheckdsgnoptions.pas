{
 *****************************************************************************
  This file is part of the SynEditSpellCheckerDsgn package from the Lazarus IDE.

  This content of this file is licensed: Modified LGPL-2
  Or at the users choice: Modified LGPL-3
  See the file COPYING.modifiedLGPL.txt, included in the Lazarus distribution,
  for details about the license.

  Alternatively, the contents of this file may be used under the terms of the
  Mozilla Public License Version 1.1 or 2.0 http://www.mozilla.org/MPL/

  A copy used under either License can have the other Licenses removed from this
  header. A note should be added that the original file is available with the
  above choice of License.
  Used units may have to be amended according to the choice.
 *****************************************************************************
}
unit SynSpellCheckDsgnOptions;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes,
  // LazUtils
  Laz2_XMLCfg, LazFileUtils,
  // IdeIntf
  IDEOptionsIntf, LazIDEIntf,
  // SynSpellChecker
  SynSpellDictionary, SynSpellCheckWordBreaker, SynEditMouseCmds;

type

  { TSynSpellOptions }

  TSynSpellOptions = class(TAbstractIDEEnvironmentOptions)
  private const
    SYN_SPELL_OPT_FILENAME = 'SpellCheckOptions.xml';
  strict private
    XMLConfig: TRttiXMLConfig;

    procedure CreateXmlConf;
  private
    FASpellOpts: TSynSpellDictionaryASpell;
    FCheckerOpts: TSynSpellWordCheckerSourceCode;
    FEnablePopupMenu: boolean;
    FMouseButton: TSynMouseButton;
    FMouseShift: TShiftState;
    FMouseShiftMask: TShiftState;
    procedure SetASpellOpts(AValue: TSynSpellDictionaryASpell);
    procedure SetCheckerOpts(AValue: TSynSpellWordCheckerSourceCode);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Load;
    procedure Save;
  published
    property ASpellOpts: TSynSpellDictionaryASpell read FASpellOpts write SetASpellOpts;
    property CheckerOpts: TSynSpellWordCheckerSourceCode read FCheckerOpts write SetCheckerOpts;

    property EnablePopupMenu: boolean read FEnablePopupMenu write FEnablePopupMenu default True;
    property MouseButton: TSynMouseButton read FMouseButton write FMouseButton default mbXRight;
    property MouseShift: TShiftState read FMouseShift write FMouseShift default [];
    property MouseShiftMask: TShiftState read FMouseShiftMask write FMouseShiftMask default [ssShift, ssCtrl, ssAlt];
  end;
  TSynSpellOptionsClass = class of TSynSpellOptions;

var
  SynSpellOptions: TSynSpellOptions;

implementation

{ TSynSpellOptions }

procedure TSynSpellOptions.CreateXmlConf;
var
  ConfFileName: String;
begin
  if XMLConfig <> nil then
    exit;

  ConfFileName := AppendPathDelim(LazarusIDE.GetPrimaryConfigPath) + SYN_SPELL_OPT_FILENAME;
  try
    if not FileExistsUTF8(ConfFileName) then
      XMLConfig := TRttiXMLConfig.CreateClean(ConfFileName)
    else
      XMLConfig := TRttiXMLConfig.Create(ConfFileName);
    XMLConfig.CheckPropertyDefault := True;
    XMLConfig.WriteAllNestedObjects := True;
  except
    on E: Exception do
    begin
      //DebugLn('WARNING: unable to read ', ConfFileName, ' ', E.Message);
      XMLConfig := Nil;
    end;
  end;
end;

procedure TSynSpellOptions.SetASpellOpts(AValue: TSynSpellDictionaryASpell);
begin
  FASpellOpts.Assign(AValue);
end;

procedure TSynSpellOptions.SetCheckerOpts(AValue: TSynSpellWordCheckerSourceCode);
begin
  if FCheckerOpts = AValue then Exit;
  FCheckerOpts.Assign(AValue);
end;

constructor TSynSpellOptions.Create;
begin
  FASpellOpts := TSynSpellDictionaryASpell.Create;
  FCheckerOpts := TSynSpellWordCheckerSourceCode.Create;
  FEnablePopupMenu := True;
  FMouseButton := mbXRight;
  FMouseShift := [];
  FMouseShiftMask := [ssShift, ssCtrl, ssAlt];
  inherited Create;
end;

destructor TSynSpellOptions.Destroy;
begin
  inherited Destroy;
  XMLConfig.Free;
  FASpellOpts.ReleaseReference;
  FCheckerOpts.Free;
end;

procedure TSynSpellOptions.Load;
begin
  CreateXmlConf;
  XMLConfig.ReadObject('SpellOpts/', Self);
end;

procedure TSynSpellOptions.Save;
begin
  XMLConfig.WriteObject('SpellOpts/', Self);
  XMLConfig.Flush;
end;

initialization
  SynSpellOptions := TSynSpellOptions.Create;

finalization
  SynSpellOptions.Free;

end.

