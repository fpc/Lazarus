{
 ***************************************************************************
 *                                                                         *
 *   This source is free software; you can redistribute it and/or modify   *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 ***************************************************************************

  The IDE's own debug console window, as a registered plug-in.

  This adds no behaviour. It exists so that the built-in window is not a
  special case: it goes through the same contract as any package-supplied
  alternative, and it is what the selection falls back to. If the built-in were
  left hard-wired while alternatives went through the interface, every switch
  would have two code paths and only one of them would be exercised.

  The window itself stays where it is, owned and shown by the debug manager.
  This plug-in only forwards.
}
unit IdeDebuggerBuiltInConsolePlugIn;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  // IdeIntf
  IdeDebuggerConsolePlugInIntf, IdeDebuggerPlugInIntf,
  // LazDebuggerIntf
  LazDebuggerIntfBaseTypes,
  // IdeDebugger
  BaseDebugManager, IdeDebuggerStringConstants, IdeDebuggerOpts;

type

  { TLazDbgIdeBuiltInConsolePlugInRegistryEntry }

  TLazDbgIdeBuiltInConsolePlugInRegistryEntry = class(TLazDbgIdeConsoleWindowPlugInRegistryEntry)
  public
    class function CreateIdeConsoleWindowPlugIn: ILazDbgIdeConsoleWindowPlugIn; override;
    class function GetDisplayName: String; override;
    class function GetPlugInId: String; override;
  end;

  { TLazDbgIdeBuiltInConsolePlugIn }

  TLazDbgIdeBuiltInConsolePlugIn = class(
    specialize TGenLazDbgIdePlugIn<
      specialize TGenLazDbgIdePlugInConfiguration<TObject>,
      TLazDbgIdeBuiltInConsolePlugInRegistryEntry>,
    ILazDbgIdeConsoleWindowPlugIn, ILazDbgIdePlugInConfiguration)
  private
    FHook: ILazDbgIdeTargetIoHook;
  protected
    procedure DoFree;
    function GetSettingsFrameClass: TFrameClass; virtual;
  public
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

implementation

{ TLazDbgIdeBuiltInConsolePlugIn }

function TLazDbgIdeBuiltInConsolePlugIn.GetConfigObject: TObject;
begin
  (* No settings of its own yet. What the built-in window can be told to do --
     when to auto-open -- is still an environment option, read by the IDE
     rather than by this plug-in. *)
  Result := nil;
end;

function TLazDbgIdeBuiltInConsolePlugIn.CreateCopy: ILazDbgIdePlugInConfiguration;
begin
  Result := TLazDbgIdeBuiltInConsolePlugIn.Create;
end;

procedure TLazDbgIdeBuiltInConsolePlugIn.AssignOptions(ASource: ILazDbgIdePlugInConfiguration);
//var
//  o: TObject;
begin
  //o := ASource.GetConfigObject;
  //if not (o is TDebugTerminalConfig) then
  //  exit;
  //FConfig.Assign(TDebugTerminalConfig(o));
end;

procedure TLazDbgIdeBuiltInConsolePlugIn.DoFree;
begin
  Destroy;
end;

function TLazDbgIdeBuiltInConsolePlugIn.GetSettingsFrameClass: TFrameClass;
begin
  Result := nil;
end;

procedure TLazDbgIdeBuiltInConsolePlugIn.HandleUserSelectedAsActive;
begin
  //
end;

procedure TLazDbgIdeBuiltInConsolePlugIn.HandleUserDeselectedFromActive;
begin
  //
end;

procedure TLazDbgIdeBuiltInConsolePlugIn.ProcessAddedToPlugInHook(
  AHook: ILazDbgIdeTargetIoHook);
begin
  FHook := AHook;
end;

procedure TLazDbgIdeBuiltInConsolePlugIn.ProcessRemovedFromPlugInHook;
begin
  FHook := nil;
end;

procedure TLazDbgIdeBuiltInConsolePlugIn.HandleUserShow;
begin
  DebugBoss.ConsoleWindowShow(True);
end;

procedure TLazDbgIdeBuiltInConsolePlugIn.StartNewDebugSession;
begin
  DebugBoss.ConsoleWindowClear;
end;

procedure TLazDbgIdeBuiltInConsolePlugIn.AddOutputFromTargetConsole(
  AChannel: TLzDbgTargetIoChannel; AText: String);
begin
  (* The built-in window shows one stream. Until a backend reports the channel
     it is always dtcUnknown, and even then this window has nothing to do with
     the distinction -- it is kept in the signature so that a display which
     does care needs no interface change. *)
  DebugBoss.ConsoleWindowAddOutput(AText);
end;

procedure TLazDbgIdeBuiltInConsolePlugIn.BringToFront;
begin
  DebugBoss.ConsoleWindowShow(True);
end;

procedure TLazDbgIdeBuiltInConsolePlugIn.SetAutoShowState(AShowOnInput: Boolean);
begin
  (* Ignored on purpose. For the built-in the IDE still applies the auto-open
     environment option itself, exactly as before this interface existed. A
     plug-in that owns its own window is the case this call is for. *)
end;

{ TLazDbgIdeBuiltInConsolePlugInRegistryEntry }

class function TLazDbgIdeBuiltInConsolePlugInRegistryEntry.CreateIdeConsoleWindowPlugIn: ILazDbgIdeConsoleWindowPlugIn;
begin
  Result := TLazDbgIdeBuiltInConsolePlugIn.Create;
end;

class function TLazDbgIdeBuiltInConsolePlugInRegistryEntry.GetDisplayName: String;
begin
  Result := lisDebugConsoleBuiltInName;
end;

class function TLazDbgIdeBuiltInConsolePlugInRegistryEntry.GetPlugInId: String;
begin
  Result := BuiltInConsolePlugInId;
end;

initialization
  ConsoleWindowPlugInRegistry.RegisterPlugIn(TLazDbgIdeBuiltInConsolePlugInRegistryEntry);

end.
