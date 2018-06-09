unit EMSIdeOptions;

{$mode objfpc}{$H+}

interface

uses
  SysUtils,
  // LCL
  StdCtrls,
  // IdeIntf
  IDEOptionsIntf, IDEOptEditorIntf, SrcEditorIntf,
  // MacroScript
  EMScriptMacro, EMSStrings;

type

  { TEMSIdeOptionsFrame }

  TEMSIdeOptionsFrame = class(TAbstractIDEOptionsEditor)
    btnActivate: TButton;
    lblStatus: TLabel;
    procedure btnActivateClick(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
    function GetTitle: String; override;
    procedure Setup(ADialog: TAbstractOptionsEditorDialog); override;
    procedure ReadSettings(AOptions: TAbstractIDEOptions); override;
    procedure WriteSettings(AOptions: TAbstractIDEOptions); override;
    class function SupportedOptionsClass: TAbstractIDEOptionsClass; override;
  end;

implementation

{$R *.lfm}

{ EMSIdeOptionsFrame }

procedure TEMSIdeOptionsFrame.btnActivateClick(Sender: TObject);
var
  cfg: TEMSConfig;
begin
  cfg := GetEMSConf;
  cfg.SelfTestFailed := 0;
  cfg.SelfTestActive := False;
  cfg.Save;
  ReadSettings(nil);
end;

function TEMSIdeOptionsFrame.GetTitle: String;
begin
  Result := EMSStatusTitle;
end;

procedure TEMSIdeOptionsFrame.Setup(ADialog: TAbstractOptionsEditorDialog);
begin
  btnActivate.Caption := EMSBtnTestAgain;
end;

procedure TEMSIdeOptionsFrame.ReadSettings(AOptions: TAbstractIDEOptions);
var
  cfg: TEMSConfig;
begin
  if not EMSSupported then begin
    lblStatus.Caption := EMSNotSupported;
    btnActivate.Enabled := True;
    exit;
  end;

  cfg := GetEMSConf;

  if cfg.SelfTestFailed >= EMSVersion then begin
    lblStatus.Caption := EMSNotActive;
    btnActivate.Enabled := True;
  end
  else

  if EditorMacroPlayerClass = TEMSEditorMacro then begin
    lblStatus.Caption := EMSActive;
    btnActivate.Enabled := False;
  end

  else
  begin
    lblStatus.Caption := EMSPending;
    btnActivate.Enabled := False;
  end;

end;

procedure TEMSIdeOptionsFrame.WriteSettings(AOptions: TAbstractIDEOptions);
begin
  //
end;

class function TEMSIdeOptionsFrame.SupportedOptionsClass: TAbstractIDEOptionsClass;
begin
  Result := TEMSConfig;
end;

end.

