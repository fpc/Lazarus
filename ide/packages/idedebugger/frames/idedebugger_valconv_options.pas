unit IdeDebugger_ValConv_Options;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Forms, Controls, IDEOptEditorIntf, IDEOptionsIntf,
  DbgIntfDebuggerBase, IdeDebuggerStringConstants,
  IdeDbgValueConverterSettingsFrame, IdeDebuggerOpts,
  IdeDebuggerBackendValueConv;

type

  { TIdeDbgValConvOptionsFrame }

  TIdeDbgValConvOptionsFrame = class(TAbstractIDEOptionsEditor)
    DbgValConvFrame1: TIdeDbgValConvFrame;
  private
    FValConvList: TIdeDbgValueConvertSelectorList;
  public
    destructor Destroy; override;
    function GetTitle: String; override;
    procedure Setup({%H-}ADialog: TAbstractOptionsEditorDialog); override;
    procedure ReadSettings({%H-}AOptions: TAbstractIDEOptions); override;
    procedure WriteSettings({%H-}AOptions: TAbstractIDEOptions); override;
    class function SupportedOptionsClass: TAbstractIDEOptionsClass; override;
  end;

  procedure Register;

implementation

procedure Register;
begin
  RegisterIDEOptionsEditor(GroupDebugger, TIdeDbgValConvOptionsFrame, DbgOptionsBackConverter);
end;

{$R *.lfm}

{ TIdeDbgValConvOptionsFrame }

destructor TIdeDbgValConvOptionsFrame.Destroy;
begin
  inherited Destroy;
  FValConvList.Free;
end;

function TIdeDbgValConvOptionsFrame.GetTitle: String;
begin
  Result := dlgBackConvOptDebugOptions;
end;

procedure TIdeDbgValConvOptionsFrame.Setup(
  ADialog: TAbstractOptionsEditorDialog);
begin
  DbgValConvFrame1.Setup;
end;

procedure TIdeDbgValConvOptionsFrame.ReadSettings(
  AOptions: TAbstractIDEOptions);
begin
  if FValConvList = nil then
    FValConvList := TIdeDbgValueConvertSelectorList.Create;
  FValConvList.Assign(DebuggerOptions.BackendConverterConfig);
  FValConvList.Changed := False;
  DbgValConvFrame1.ValConvList := FValConvList;
end;

procedure TIdeDbgValConvOptionsFrame.WriteSettings(
  AOptions: TAbstractIDEOptions);
begin
  DbgValConvFrame1.SaveCurrent;
  if FValConvList.Changed then begin
    DebuggerOptions.BackendConverterConfig.Assign(FValConvList);
    DebuggerOptions.BackendConverterConfig.Changed := True;

    if DebugBossManager <> nil then
      DebugBossManager.DoBackendConverterChanged;
  end;
end;

class function TIdeDbgValConvOptionsFrame.SupportedOptionsClass: TAbstractIDEOptionsClass;
begin
  Result := TDebuggerOptions;
end;

end.

