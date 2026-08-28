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
  // LazControls
  DividerBevel,
  // IdeIntf
  IDEOptEditorIntf, IDEOptionsIntf, IdeDebuggerConsolePlugInIntf, IdeDebuggerPlugInIntf,
  // IdeDebugger
  IdeDebuggerStringConstants, IdeDebuggerOpts;

type

  { TIdeDbgConsoleWindowOptionsFrame }

  TIdeDbgConsoleWindowOptionsFrame = class(TAbstractIDEOptionsEditor)
    cbPlugIn: TComboBox;
    divEditPlugIn: TDividerBevel;
    divSelectPlugIn: TDividerBevel;
    lblDescription: TLabel;
    lblEditing: TLabel;
    pnlSettings: TPanel;
    procedure cbPlugInChange(Sender: TObject);
  private
    FPlugIns: TIdeDbgConsoleWindowPlugInList;   // the working copy
    FFrame: TFrame;
    FFramePlugIn: ILazDbgIdeConsoleWindowPlugIn;
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
  Cfg: ILazDbgIdePlugInConfiguration;
  FrameClass: TClass;
  Intf: ILazDbgIdePlugInSettingsFrameIntf;
begin
  SaveSettingsFrame;
  FreeAndNil(FFrame);
  FFramePlugIn := nil;

  if cbPlugIn.ItemIndex < 0 then
    exit;
  FFramePlugIn := FPlugIns.PlugIns[cbPlugIn.ItemIndex];
  if FFramePlugIn = nil then
    exit;

  Cfg := FFramePlugIn.GetConfiguration;
  if Cfg = nil then
    exit;

  FrameClass := Cfg.GetSettingsFrameClass;
  if (FrameClass = nil) or (not FrameClass.InheritsFrom(TFrame)) then
    exit;

  (* Into its own panel, not straight onto this frame: alClient on the page
     itself covers the chooser rather than sitting under it. *)
  FFrame := TSettingsFrameClass(FrameClass).Create(Self);
  FFrame.Parent := pnlSettings;
  FFrame.Align := alClient;
  if FFrame.GetInterface(ILazDbgIdePlugInSettingsFrameIntf, Intf) then
    Intf.ReadFrom(Cfg);
end;

procedure TIdeDbgConsoleWindowOptionsFrame.SaveSettingsFrame;
var
  Cfg: ILazDbgIdePlugInConfiguration;
  Intf: ILazDbgIdePlugInSettingsFrameIntf;
begin
  if (FFrame = nil) or (FFramePlugIn = nil) then
    exit;
  Cfg := FFramePlugIn.GetConfiguration;
  if Cfg = nil then
    exit;
  if FFrame.GetInterface(ILazDbgIdePlugInSettingsFrameIntf, Intf) then
    if Intf.WriteTo(Cfg) then
      FPlugIns.Changed := True;
end;

procedure TIdeDbgConsoleWindowOptionsFrame.Setup(
  ADialog: TAbstractOptionsEditorDialog);
begin
  divSelectPlugIn.Caption := dlgDebugConsoleWindowSelectDiv;
  divEditPlugIn.Caption := dlgDebugConsoleWindowEditDiv;
  (* Where the per-project override lives is a hint rather than a label: it
     answers a question the user only asks once, and the page already carries
     as much standing text as it can afford. *)
  cbPlugIn.Hint := dlgDebugConsoleWindowRunParamsHint;
  cbPlugIn.ShowHint := True;
end;

procedure TIdeDbgConsoleWindowOptionsFrame.ReadSettings(
  AOptions: TAbstractIDEOptions);
var
  i, c: Integer;
begin
  (* Edit a copy. Cancel then costs nothing, which is the whole reason the
     plug-in interface carries CreateCopy. *)
  if FPlugIns = nil then
    FPlugIns := TIdeDbgConsoleWindowPlugInList.Create;
  FPlugIns.Assign(DebuggerOptions.ConsoleWindowPlugIns);
  FPlugIns.Changed := False;

  cbPlugIn.Clear;
  c := 0;
  for i := 0 to FPlugIns.Count - 1 do begin
    cbPlugIn.Items.Add(FPlugIns.PlugIns[i].GetDisplayName);
    if SameText(FPlugIns.Ids[i], DebuggerOptions.ConsoleWindowPlugInId) then
      c := i;
  end;
  cbPlugIn.ItemIndex := c;

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
  if FPlugIns = nil then
    exit;
  if (cbPlugIn.ItemIndex >= 0) and (cbPlugIn.ItemIndex < FPlugIns.Count) then
    Result := FPlugIns.Ids[cbPlugIn.ItemIndex];
end;

procedure TIdeDbgConsoleWindowOptionsFrame.UpdateDescription;
var
  i: Integer;
begin
  i := cbPlugIn.ItemIndex;
  if (i < 0) or (i >= ConsoleWindowPlugIns.Count) then begin
    lblDescription.Caption := dlgDebugConsoleWindowNone;
    lblEditing.Caption := '';
    divEditPlugIn.Visible := False;
    lblEditing.Visible := False;
    exit;
  end;

  lblDescription.Caption := dlgDebugConsoleWindowChangeTakesEffect;
  (* The display name, not the id. The id is "package/class" and says nothing
     to the user; this names the same plug-in the drop-down is showing, so the
     settings below are attributable without reading the combo again. *)
  lblEditing.Caption := Format(dlgDebugConsoleWindowEditing,
    [ConsoleWindowPlugIns[i].GetDisplayName]);
  divEditPlugIn.Visible := True;
  lblEditing.Visible := True;
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
