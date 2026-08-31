{
  debugterminalsettingsframe.pas
  ------------------------------
  The plug-in's own settings frame, rendered by the IDE's console window
  options page. The page knows nothing about what is on it -- it asks the
  registry entry for a frame class and hands the frame the plug-in to read
  from and write back to.

  SPDX-License-Identifier: MIT
}
unit IdeDebugTerminalSettingsFrameExamle;

{$mode objfpc}{$H+}
{$INTERFACES CORBA}

interface

uses
  Classes, SysUtils, Forms, StdCtrls, Graphics, Dialogs,   { TColorButton }
  // IdeIntf
  IdeDebuggerConsolePlugInIntf, IdeDebuggerPlugInIntf,
  // local
  IdeDebugTerminalOptionsExample, IdeDebugTerminalPluginExample;

type

  { TDebugTerminalSettingsFrame }

  TDebugTerminalSettingsFrame = class(TFrame, ILazDbgIdePlugInSettingsFrameIntf)
    cbBackspace: TComboBox;
    cbLineEnding: TComboBox;
    chkLocalEcho: TCheckBox;
    clrBackground: TColorButton;
    clrForeground: TColorButton;
    lblBackground: TLabel;
    lblBackspace: TLabel;
    lblForeground: TLabel;
    lblLineEnding: TLabel;
  public
    procedure ReadFrom(APlugIn: ILazDbgIdePlugInConfiguration);
    function  WriteTo(APlugIn: ILazDbgIdePlugInConfiguration): Boolean;
  end;

implementation

{$R *.lfm}

function ConfigOf(APlugIn: ILazDbgIdePlugInConfiguration): TDebugTerminalConfig;
var
  o: TObject;
begin
  Result := nil;
  if APlugIn = nil then
    exit;
  o := APlugIn.GetConfigObject;
  if o is TDebugTerminalConfig then
    Result := TDebugTerminalConfig(o);
end;

procedure TDebugTerminalSettingsFrame.ReadFrom(APlugIn: ILazDbgIdePlugInConfiguration);
var
  c: TDebugTerminalConfig;
begin
  c := ConfigOf(APlugIn);
  if c = nil then
    exit;
  cbLineEnding.ItemIndex := ord(c.LineEnding);
  cbBackspace.ItemIndex  := ord(c.Backspace);
  chkLocalEcho.Checked   := c.LocalEcho;
  clrBackground.ButtonColor := c.BackgroundColor;
  clrForeground.ButtonColor := c.ForegroundColor;
end;

function TDebugTerminalSettingsFrame.WriteTo(APlugIn: ILazDbgIdePlugInConfiguration): Boolean;
var
  c: TDebugTerminalConfig;
begin
  Result := False;
  c := ConfigOf(APlugIn);
  if c = nil then
    exit;
  if cbLineEnding.ItemIndex >= 0 then
    c.LineEnding := TDebugTerminalLineEnding(cbLineEnding.ItemIndex);
  if cbBackspace.ItemIndex >= 0 then
    c.Backspace := TDebugTerminalBackspaceKey(cbBackspace.ItemIndex);
  c.LocalEcho       := chkLocalEcho.Checked;
  c.BackgroundColor := clrBackground.ButtonColor;
  c.ForegroundColor := clrForeground.ButtonColor;
  Result := True;
end;

end.
