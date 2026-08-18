{
  Options page for choosing which registered plug-in shows the debuggee's
  captured console output.

  The chooser is written once, here, rather than by each package that registers
  a console window. A per-package "active" checkbox would leave the user with
  nowhere to see which one is actually in use, and every package carrying its
  own options page does not scale.
}
unit IdeDebugger_ConsoleWindow_Options;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Forms, Controls, StdCtrls, ExtCtrls,
  // IdeIntf
  IDEOptEditorIntf, IDEOptionsIntf, IdeDebuggerConsolePlugInIntf,
  // IdeDebugger
  IdeDebuggerStringConstants, IdeDebuggerOpts;

type

  { TIdeDbgConsoleWindowOptionsFrame }

  TIdeDbgConsoleWindowOptionsFrame = class(TAbstractIDEOptionsEditor)
    cbPlugIn: TComboBox;
    lblDescription: TLabel;
    lblPlugIn: TLabel;
    pnlSettings: TPanel;
    pnlTop: TPanel;
    procedure cbPlugInChange(Sender: TObject);
  private
    FPlugIns: TIdeDbgConsoleWindowPlugInList;   // the working copy
    FFrame: TFrame;
    FFramePlugIn: ILazDbgIdePlugIn;
    procedure UpdateDescription;
    function  SelectedPlugInId: String;
    procedure ShowSettingsFrame;
    procedure SaveSettingsFrame;
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

{$R *.lfm}

type
  { The LCL has no TFrameClass; GetSettingsFrameClass returns a plain TClass so
    that the interface unit needs no LCL dependency. }
  TSettingsFrameClass = class of TFrame;

procedure Register;
begin
  RegisterIDEOptionsEditor(GroupDebugger, TIdeDbgConsoleWindowOptionsFrame,
    DbgOptionsConsoleWindow);
end;

{ TIdeDbgConsoleWindowOptionsFrame }

destructor TIdeDbgConsoleWindowOptionsFrame.Destroy;
begin
  inherited Destroy;
  FPlugIns.Free;
end;

function TIdeDbgConsoleWindowOptionsFrame.GetTitle: String;
begin
  Result := dlgDebugConsoleWindowOptions;
end;

(* Build the selected plug-in's own settings frame, if it has one. The frame
   class comes from the plug-in's package and is rendered here without this
   page knowing anything about what it contains. *)
procedure TIdeDbgConsoleWindowOptionsFrame.ShowSettingsFrame;
var
  Entry: TLazDbgIdeConsoleWindowPlugInRegistryEntryClass;
  FrameClass: TClass;
  Intf: ILazDbgIdePlugInSettingsFrameIntf;
  Id: String;
begin
  SaveSettingsFrame;
  FreeAndNil(FFrame);
  FFramePlugIn := nil;

  Id := SelectedPlugInId;
  if Id = '' then
    exit;
  Entry := ConsoleWindowPlugIns.FindByPlugInId(Id);
  if Entry = nil then
    exit;
  FrameClass := Entry.GetSettingsFrameClass;
  if (FrameClass = nil) or (not FrameClass.InheritsFrom(TFrame)) then
    exit;

  FFramePlugIn := FPlugIns.PlugInById(Id);
  if FFramePlugIn = nil then
    exit;

  (* Into its own panel, not straight onto this frame: alClient on the page
     itself covers the chooser rather than sitting under it. *)
  FFrame := TSettingsFrameClass(FrameClass).Create(Self);
  FFrame.Parent := pnlSettings;
  FFrame.Align := alClient;
  if FFrame.GetInterface(ILazDbgIdePlugInSettingsFrameIntf, Intf) then
    Intf.ReadFrom(FFramePlugIn);
end;

procedure TIdeDbgConsoleWindowOptionsFrame.SaveSettingsFrame;
var
  Intf: ILazDbgIdePlugInSettingsFrameIntf;
begin
  if (FFrame = nil) or (FFramePlugIn = nil) then
    exit;
  if FFrame.GetInterface(ILazDbgIdePlugInSettingsFrameIntf, Intf) then
    if Intf.WriteTo(FFramePlugIn) then
      FPlugIns.Changed := True;
end;

procedure TIdeDbgConsoleWindowOptionsFrame.Setup(
  ADialog: TAbstractOptionsEditorDialog);
var
  i: Integer;
begin
  lblPlugIn.Caption := dlgDebugConsoleWindowSelect;
  cbPlugIn.Clear;
  for i := 0 to ConsoleWindowPlugIns.Count - 1 do
    cbPlugIn.Items.Add(ConsoleWindowPlugIns[i].GetDisplayName);
end;

procedure TIdeDbgConsoleWindowOptionsFrame.ReadSettings(
  AOptions: TAbstractIDEOptions);
var
  Entry: TLazDbgIdeConsoleWindowPlugInRegistryEntryClass;
  i: Integer;
begin
  i := -1;
  Entry := ConsoleWindowPlugIns.FindByPlugInId(DebuggerOptions.ConsoleWindowPlugInId);
  if Entry <> nil then
    i := ConsoleWindowPlugIns.IndexOf(Entry)
  else
  if ConsoleWindowPlugIns.Count > 0 then
    (* Either nothing was ever chosen, or the chosen plug-in's package is not
       installed in this IDE. Both show the fallback rather than an empty box;
       the stored id is left alone until the user picks something, so moving a
       config between installations does not silently discard it. *)
    i := 0;
  cbPlugIn.ItemIndex := i;

  (* Edit a copy. Cancel then costs nothing, which is the whole reason the
     plug-in interface carries CreateCopy. *)
  if FPlugIns = nil then
    FPlugIns := TIdeDbgConsoleWindowPlugInList.Create;
  FPlugIns.Assign(DebuggerOptions.ConsoleWindowPlugIns);
  FPlugIns.Changed := False;

  UpdateDescription;
  ShowSettingsFrame;
end;

procedure TIdeDbgConsoleWindowOptionsFrame.WriteSettings(
  AOptions: TAbstractIDEOptions);
var
  s: String;
begin
  SaveSettingsFrame;
  s := SelectedPlugInId;
  if s <> '' then
    DebuggerOptions.ConsoleWindowPlugInId := s;
  if FPlugIns.Changed then begin
    DebuggerOptions.ConsoleWindowPlugIns.Assign(FPlugIns);
    DebuggerOptions.ConsoleWindowPlugIns.Changed := True;
  end;
end;

function TIdeDbgConsoleWindowOptionsFrame.SelectedPlugInId: String;
begin
  Result := '';
  if (cbPlugIn.ItemIndex >= 0) and (cbPlugIn.ItemIndex < ConsoleWindowPlugIns.Count) then
    Result := ConsoleWindowPlugIns[cbPlugIn.ItemIndex].GetPlugInId;
end;

procedure TIdeDbgConsoleWindowOptionsFrame.UpdateDescription;
var
  s: String;
begin
  s := SelectedPlugInId;
  if s = '' then
    lblDescription.Caption := dlgDebugConsoleWindowNone
  else
    lblDescription.Caption := Format(dlgDebugConsoleWindowChangeTakesEffect, [s]);
end;

procedure TIdeDbgConsoleWindowOptionsFrame.cbPlugInChange(Sender: TObject);
begin
  UpdateDescription;
  ShowSettingsFrame;
end;

class function TIdeDbgConsoleWindowOptionsFrame.SupportedOptionsClass: TAbstractIDEOptionsClass;
begin
  Result := TDebuggerOptions;
end;

end.
