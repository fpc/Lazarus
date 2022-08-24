unit IdeDebugger_ValConv_Options;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, IDEOptEditorIntf, IDEOptionsIntf,
  FpDebugValueConvertors, IdeDebuggerStringConstants,
  IdeDbgValueConverterSettingsFrame, IdeDebuggerOpts,
  IdeDebuggerFpDbgValueConv;

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
  RegisterIDEOptionsEditor(GroupDebugger, TIdeDbgValConvOptionsFrame, DbgOptionsFpDbgOpts);
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
  Result := dlgFpConvOptFpDebugOptions;
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
  FValConvList.Assign(DebuggerOptions.FpDbgConverterConfig);
  FValConvList.Changed := False;
  DbgValConvFrame1.ValConvList := FValConvList;
end;

procedure TIdeDbgValConvOptionsFrame.WriteSettings(
  AOptions: TAbstractIDEOptions);
begin
  DbgValConvFrame1.SaveCurrent;
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

class function TIdeDbgValConvOptionsFrame.SupportedOptionsClass: TAbstractIDEOptionsClass;
begin
  Result := TDebuggerOptions;
end;

end.

