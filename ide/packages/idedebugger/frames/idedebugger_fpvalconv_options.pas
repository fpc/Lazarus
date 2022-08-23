unit IdeDebugger_FpValConv_Options;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, IDEOptEditorIntf, IDEOptionsIntf,
  FpDebugValueConvertors, IdeDebuggerStringConstants,
  IdeFpDbgValueConverterSettingsFrame, IdeDebuggerOpts,
  IdeDebuggerFpDbgValueConv;

type

  { TIdeDbgFpValConvOptionsFrame }

  TIdeDbgFpValConvOptionsFrame = class(TAbstractIDEOptionsEditor)
    FpDbgValConvFrame1: TFpDbgValConvFrame;
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
  RegisterIDEOptionsEditor(GroupDebugger, TIdeDbgFpValConvOptionsFrame, DbgOptionsFpDbgOpts);
end;

{$R *.lfm}

{ TIdeDbgFpValConvOptionsFrame }

destructor TIdeDbgFpValConvOptionsFrame.Destroy;
begin
  inherited Destroy;
  FValConvList.Free;
end;

function TIdeDbgFpValConvOptionsFrame.GetTitle: String;
begin
  Result := dlgFpConvOptFpDebugOptions;
end;

procedure TIdeDbgFpValConvOptionsFrame.Setup(
  ADialog: TAbstractOptionsEditorDialog);
begin
  FpDbgValConvFrame1.Setup;
end;

procedure TIdeDbgFpValConvOptionsFrame.ReadSettings(
  AOptions: TAbstractIDEOptions);
begin
  if FValConvList = nil then
    FValConvList := TIdeDbgValueConvertSelectorList.Create;
  FValConvList.Assign(DebuggerOptions.FpDbgConverterConfig);
  FValConvList.Changed := False;
  FpDbgValConvFrame1.ValConvList := FValConvList;
end;

procedure TIdeDbgFpValConvOptionsFrame.WriteSettings(
  AOptions: TAbstractIDEOptions);
begin
  FpDbgValConvFrame1.SaveCurrent;
  if FValConvList.Changed then begin
    DebuggerOptions.FpDbgConverterConfig.Assign(FValConvList);
    DebuggerOptions.FpDbgConverterConfig.Changed := True;

    ValueConverterSelectorList.Lock;
    try
      DebuggerOptions.FpDbgConverterConfig.AssignEnabledTo(ValueConverterSelectorList);
    finally
      ValueConverterSelectorList.Unlock;
    end;
  end;
end;

class function TIdeDbgFpValConvOptionsFrame.SupportedOptionsClass: TAbstractIDEOptionsClass;
begin
  Result := TDebuggerOptions;
end;

end.

