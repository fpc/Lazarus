unit IdeDebugger_ValFormatter_Options;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Forms, Controls, IDEOptEditorIntf, IDEOptionsIntf,
  DbgIntfDebuggerBase, IdeDebuggerStringConstants, IdeDebuggerOpts,
  IdeDbgValueFormatterSettingsFrame, IdeDebuggerValueFormatter;

type

  { TIdeDbgValFormatOptionsFrame }

  TIdeDbgValFormatOptionsFrame = class(TAbstractIDEOptionsEditor)
    IdeDbgVarFormatterFrame1: TIdeDbgVarFormatterFrame;
  private
    FValFormatList: TIdeDbgValueFormatterSelectorList;
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
  RegisterIDEOptionsEditor(GroupDebugger, TIdeDbgValFormatOptionsFrame, DbgOptionsValFormatter);
end;

{$R *.lfm}

{ TIdeDbgValFormatOptionsFrame }

destructor TIdeDbgValFormatOptionsFrame.Destroy;
begin
  inherited Destroy;
  FValFormatList.Free;
end;

function TIdeDbgValFormatOptionsFrame.GetTitle: String;
begin
  Result := dlgVarFormatterDebugOptions;
end;

procedure TIdeDbgValFormatOptionsFrame.Setup(
  ADialog: TAbstractOptionsEditorDialog);
begin
  IdeDbgVarFormatterFrame1.Setup;
end;

procedure TIdeDbgValFormatOptionsFrame.ReadSettings(
  AOptions: TAbstractIDEOptions);
begin
  if FValFormatList = nil then
    FValFormatList := TIdeDbgValueFormatterSelectorList.Create;
  FValFormatList.Assign(DebuggerOptions.ValueFormatterConfig);
  FValFormatList.Changed := False;
  IdeDbgVarFormatterFrame1.ValFormatterList := FValFormatList;
end;

procedure TIdeDbgValFormatOptionsFrame.WriteSettings(
  AOptions: TAbstractIDEOptions);
begin
  IdeDbgVarFormatterFrame1.SaveCurrent;
  if FValFormatList.Changed then begin
    DebuggerOptions.ValueFormatterConfig.Assign(FValFormatList);
    DebuggerOptions.ValueFormatterConfig.Changed := True;
  end;
end;

class function TIdeDbgValFormatOptionsFrame.SupportedOptionsClass: TAbstractIDEOptionsClass;
begin
  Result := TDebuggerOptions;
end;

end.

