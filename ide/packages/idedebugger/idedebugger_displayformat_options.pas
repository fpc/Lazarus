unit IdeDebugger_DisplayFormat_Options;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, IDEOptEditorIntf, IDEOptionsIntf, DbgIntfDebuggerBase,
  DisplayFormatConfigFrame, DisplayFormatDefaultsConfigFrame, IdeDebuggerDisplayFormats,
  IdeDebuggerStringConstants, IdeDebuggerOpts;

type

  { TIdeDbgDisplayFormatOptionsFrame }

  TIdeDbgDisplayFormatOptionsFrame = class(TAbstractIDEOptionsEditor)
    DisplayFormatDefaultsConfigFrame1: TDisplayFormatDefaultsConfigFrame;
  private
    FDisplayFormatConfig: TDisplayFormatConfig;
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
  RegisterIDEOptionsEditor(GroupDebugger, TIdeDbgDisplayFormatOptionsFrame, DbgOptionsDispFormat);
end;

{$R *.lfm}

{ TIdeDbgDisplayFormatOptionsFrame }

destructor TIdeDbgDisplayFormatOptionsFrame.Destroy;
begin
  inherited Destroy;
  FDisplayFormatConfig.Free;
end;

function TIdeDbgDisplayFormatOptionsFrame.GetTitle: String;
begin
  Result := dlgDisplayFormatDebugOptions;
end;

procedure TIdeDbgDisplayFormatOptionsFrame.Setup(ADialog: TAbstractOptionsEditorDialog);
begin
  DisplayFormatDefaultsConfigFrame1.ShowOverrideChecks := False;
  DisplayFormatDefaultsConfigFrame1.Setup;
end;

procedure TIdeDbgDisplayFormatOptionsFrame.ReadSettings(AOptions: TAbstractIDEOptions);
begin
  if FDisplayFormatConfig = nil then
    FDisplayFormatConfig := TDisplayFormatConfig.Create;
  FDisplayFormatConfig.Assign(DebuggerOptions.DisplayFormatConfigs);
  DisplayFormatDefaultsConfigFrame1.DisplayFormatConfig := FDisplayFormatConfig;
end;

procedure TIdeDbgDisplayFormatOptionsFrame.WriteSettings(AOptions: TAbstractIDEOptions);
begin
  DisplayFormatDefaultsConfigFrame1.SaveConfig;
  DebuggerOptions.DisplayFormatConfigs.Changed := False;
  DebuggerOptions.DisplayFormatConfigs.Assign(FDisplayFormatConfig); // assign will trigger changed, if anything changed

  // trigger chnage notification
  DebuggerOptions.DisplayFormatConfigs.Changed := DebuggerOptions.DisplayFormatConfigs.Changed;
end;

class function TIdeDbgDisplayFormatOptionsFrame.SupportedOptionsClass: TAbstractIDEOptionsClass;
begin
  Result := TDebuggerOptions;
end;

end.

