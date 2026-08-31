{
  debugterminalplugin.pas
  -----------------------
  Registers the SynEdit terminal window as a debug console window plug-in.

  This is an ordinary third-party package: it depends on IdeIntf, LCL and
  SynEdit, and on nothing private to the IDE. It exists to be the second entry
  in the chooser -- the case the selection design is for, two console windows
  installed at once, both wanting the debuggee's stream.

  SPDX-License-Identifier: MIT
}
unit IdeDebugTerminalPluginExample;

{$mode objfpc}{$H+}
{$INTERFACES CORBA}

interface

uses
  Classes, SysUtils, Graphics,
  // IdeIntf
  IdeDebuggerConsolePlugInIntf, IdeDebuggerPlugInIntf,
  // LazDebuggerIntf
  LazDebuggerIntfBaseTypes,
  // local
  IdeDebugTerminalOptionsExample, IdeDebugTerminalPanelExample;

const
  DebugTerminalPlugInId = 'LazDebugTerminal/TDebugTerminalPlugIn';

type

  { TDebugTerminalConfig -- published settings, rendered and stored by the IDE }

  TDebugTerminalConfig = class(TPersistent)
  private
    FLineEnding:      TDebugTerminalLineEnding;
    FLocalEcho:       Boolean;
    FBackspace:       TDebugTerminalBackspaceKey;
    FBackgroundColor: TColor;
    FForegroundColor: TColor;
  public
    constructor Create;
    procedure Assign(Source: TPersistent); override;
  published
    property LineEnding: TDebugTerminalLineEnding read FLineEnding write FLineEnding
      default {$IFDEF windows}TDebugTerminalLineEnding.CRLF{$ELSE}TDebugTerminalLineEnding.CR{$ENDIF};
    property LocalEcho: Boolean read FLocalEcho write FLocalEcho default False;
    property Backspace: TDebugTerminalBackspaceKey read FBackspace write FBackspace
      default TDebugTerminalBackspaceKey.BS;
    property BackgroundColor: TColor read FBackgroundColor write FBackgroundColor
      default clBlack;
    property ForegroundColor: TColor read FForegroundColor write FForegroundColor
      default clWhite;
  end;

  { TDebugTerminalPlugInRegistryEntry }

  TDebugTerminalPlugInRegistryEntry = class(TLazDbgIdeConsoleWindowPlugInRegistryEntry)
  public
    class function CreateIdeConsoleWindowPlugIn: ILazDbgIdeConsoleWindowPlugIn; override;
    class function GetDisplayName: String; override;
    class function GetPlugInId: String; override;
  end;

  { TDebugTerminalPlugIn }

  TDebugTerminalPlugIn = class(
    specialize TGenLazDbgIdePlugIn<
      specialize TGenLazDbgIdePlugInConfiguration<TObject>,
      TDebugTerminalPlugInRegistryEntry>,
    ILazDbgIdeConsoleWindowPlugIn, ILazDbgIdePlugInConfiguration)
  private
    FHook:   ILazDbgIdeTargetIoHook;
    FConfig: TDebugTerminalConfig;
    FAutoShowOnInput: Boolean;
    procedure HandleFormSend(Sender: TObject; const AText: string);
    procedure ApplyConfig;
  protected
    procedure DoFree;
    function GetSettingsFrameClass: TFrameClass;
  public
    constructor Create;
    destructor Destroy; override;

    // ILazDbgIdePlugInConfiguration
    function  GetConfigObject: TObject; reintroduce;
    function  CreateCopy: ILazDbgIdePlugInConfiguration; reintroduce;
    procedure AssignOptions(ASource: ILazDbgIdePlugInConfiguration); reintroduce;
    procedure ILazDbgIdePlugInConfiguration.FreeCopy = DoFree;

    // ILazDbgIdeConsoleWindowPlugIn
    procedure ILazDbgIdeConsoleWindowPlugIn.Free = DoFree;
    procedure HandleUserSelectedAsActive;
    procedure HandleUserDeselectedFromActive;
    procedure ProcessAddedToPlugInHook(AHook: ILazDbgIdeTargetIoHook);
    procedure ProcessRemovedFromPlugInHook;
    procedure HandleUserShow;
    procedure StartNewDebugSession;
    procedure AddOutputFromTargetConsole(AChannel: TLzDbgTargetIoChannel; AText: String);
    procedure BringToFront;
    procedure SetAutoShowState(AShowOnInput: Boolean);
  end;

procedure Register;

implementation

uses
  IdeDebugTerminalSettingsFrameExamle;

{ TDebugTerminalConfig }

constructor TDebugTerminalConfig.Create;
begin
  inherited Create;
  (* Default to what the debug-console case needs on each platform: Windows
     stdin is a raw pipe with no CR->LF translation, so CR alone double-Enters;
     Unix routes through a PTY whose ICRNL turns CR into LF. A serial or
     bare-metal target can still pick CR. *)
  {$IFDEF windows}
  FLineEnding      := TDebugTerminalLineEnding.CRLF;
  {$ELSE}
  FLineEnding      := TDebugTerminalLineEnding.CR;
  {$ENDIF}
  FLocalEcho       := False;
  FBackspace       := TDebugTerminalBackspaceKey.BS;
  FBackgroundColor := clBlack;
  FForegroundColor := clWhite;
end;

procedure TDebugTerminalConfig.Assign(Source: TPersistent);
begin
  if Source is TDebugTerminalConfig then begin
    FLineEnding      := TDebugTerminalConfig(Source).FLineEnding;
    FLocalEcho       := TDebugTerminalConfig(Source).FLocalEcho;
    FBackspace       := TDebugTerminalConfig(Source).FBackspace;
    FBackgroundColor := TDebugTerminalConfig(Source).FBackgroundColor;
    FForegroundColor := TDebugTerminalConfig(Source).FForegroundColor;
  end
  else
    inherited Assign(Source);
end;

{ TDebugTerminalPlugIn }

constructor TDebugTerminalPlugIn.Create;
begin
  inherited Create;
  FConfig := TDebugTerminalConfig.Create;
end;

destructor TDebugTerminalPlugIn.Destroy;
begin
  FConfig.Free;
  inherited Destroy;
end;

function TDebugTerminalPlugIn.GetConfigObject: TObject;
begin
  Result := FConfig;
end;

function TDebugTerminalPlugIn.CreateCopy: ILazDbgIdePlugInConfiguration;
var
  r: TDebugTerminalPlugIn;
begin
  r := TDebugTerminalPlugIn.Create;
  r.FConfig.Assign(FConfig);
  Result := r;
end;

procedure TDebugTerminalPlugIn.AssignOptions(ASource: ILazDbgIdePlugInConfiguration);
var
  o: TObject;
begin
  o := ASource.GetConfigObject;
  if not (o is TDebugTerminalConfig) then
    exit;
  FConfig.Assign(TDebugTerminalConfig(o));
end;

procedure TDebugTerminalPlugIn.DoFree;
begin
  Destroy;
end;

procedure TDebugTerminalPlugIn.HandleFormSend(Sender: TObject; const AText: string);
begin
  if FHook <> nil then
    FHook.SendInputToTargetConsole(Self, AText);
end;

{ Copy the settings onto the live window, if there is one. }
procedure TDebugTerminalPlugIn.ApplyConfig;
var
  F: TDebugTerminalForm;
  Opts: TDebugTerminalDisplayOptions;
begin
  F := DebugTerminalForm;
  if F = nil then
    exit;
  Opts.LineEnding      := FConfig.LineEnding;
  Opts.LocalEcho       := FConfig.LocalEcho;
  Opts.Backspace       := FConfig.Backspace;
  Opts.BackgroundColor := LongInt(FConfig.BackgroundColor);
  Opts.ForegroundColor := LongInt(FConfig.ForegroundColor);
  F.Options := Opts;
end;

function TDebugTerminalPlugIn.GetSettingsFrameClass: TFrameClass;
begin
  Result := TDebugTerminalSettingsFrame;
end;

procedure TDebugTerminalPlugIn.HandleUserSelectedAsActive;
begin
  SetDebugTerminalActive(True);
end;

procedure TDebugTerminalPlugIn.HandleUserDeselectedFromActive;
begin
  (* The window is left where it is and marked instead. Taking it away would
     mean destroying it -- closing is not enough, because Screen.CustomForms is
     scanned before the window creators -- and destroying loses both its stored
     coordinates and its dock site, neither of which a plug-in can hand back
     without knowing which dock master is installed.

     It also stays readable, which some will want: the previous run's output
     beside the current one. *)
  SetDebugTerminalActive(False);
end;

procedure TDebugTerminalPlugIn.ProcessAddedToPlugInHook(AHook: ILazDbgIdeTargetIoHook);
var
  F: TDebugTerminalForm;
begin
  FHook := AHook;

  (* Deliberately does not build the window. This runs at IDE startup, before
     the layout is restored, and the layout is what decides whether the window
     should be on screen at all -- forcing one open here would override a
     desktop that had it closed. AddOutput and the menu entry create it when
     something actually needs it. *)
  F := DebugTerminalForm;
  if F <> nil then begin
    F.OnSendData := @HandleFormSend;
    ApplyConfig;
  end;
  SetDebugTerminalActive(True);
end;

procedure TDebugTerminalPlugIn.ProcessRemovedFromPlugInHook;
var
  F: TDebugTerminalForm;
begin
  FHook := nil;
  F := DebugTerminalForm;
  if F <> nil then
    F.OnSendData := nil;   { an idle window discards keystrokes }

  (* The window is not destroyed here. This also runs at IDE shutdown, and the
     window has to still exist when the IDE stores its position, or the layout
     is saved as "not visible" with stale coordinates. Destroying it belongs to
     the reconcile at session start, where being deselected is what is actually
     being acted on. *)
end;

procedure TDebugTerminalPlugIn.HandleUserShow;
begin
  ShowDebugTerminalForm;
end;

procedure TDebugTerminalPlugIn.StartNewDebugSession;
var
  F: TDebugTerminalForm;
begin
  (* Called by the IDE at session init, so this is where the live window picks
     up any settings edited since the last run. Deliberately not propagated
     while a session is running: colours and line endings changing under a
     program that is mid-output is more surprising than useful, and the config
     object edited in the options dialog is not the one in use until OK.

     Not done per chunk of output either -- that path has to stay cheap. *)
  F := DebugTerminalForm;
  if F <> nil then begin
    ApplyConfig;
    F.ClearDisplay;
  end;
end;

procedure TDebugTerminalPlugIn.AddOutputFromTargetConsole(AChannel: TLzDbgTargetIoChannel;
  AText: String);
var
  F: TDebugTerminalForm;
begin
  (* AChannel is dtcUnknown for every backend today. When one starts reporting
     stderr separately this is where it would colour differently; until then
     there is nothing to branch on. *)
  F := EnsureDebugTerminalForm;
  if F = nil then
    exit;
  if F.OnSendData = nil then begin
    F.OnSendData := @HandleFormSend;
    ApplyConfig;
  end;
  F.AppendText(AText);
  if FAutoShowOnInput and (FHook <> nil) then begin
    ShowDebugTerminalForm;
    FHook.NotifyDidAutoShow(Self);
  end;
end;

procedure TDebugTerminalPlugIn.BringToFront;
begin
  ShowDebugTerminalForm;
end;

procedure TDebugTerminalPlugIn.SetAutoShowState(AShowOnInput: Boolean);
begin
  FAutoShowOnInput := AShowOnInput;
end;

{ TDebugTerminalPlugInRegistryEntry }

class function TDebugTerminalPlugInRegistryEntry.CreateIdeConsoleWindowPlugIn: ILazDbgIdeConsoleWindowPlugIn;
begin
  Result := TDebugTerminalPlugIn.Create;
end;

class function TDebugTerminalPlugInRegistryEntry.GetDisplayName: String;
begin
  Result := 'Example Debug Terminal with SynEdit';
end;

class function TDebugTerminalPlugInRegistryEntry.GetPlugInId: String;
begin
  Result := DebugTerminalPlugInId;
end;

procedure Register;
begin
//
end;

initialization
  ConsoleWindowPlugInRegistry.RegisterPlugIn(TDebugTerminalPlugInRegistryEntry);

end.
