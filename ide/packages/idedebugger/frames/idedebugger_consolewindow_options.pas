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
  SysUtils, Forms, Controls, StdCtrls,
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
    procedure cbPlugInChange(Sender: TObject);
  private
    procedure UpdateDescription;
    function  SelectedPlugInId: String;
  public
    function GetTitle: String; override;
    procedure Setup({%H-}ADialog: TAbstractOptionsEditorDialog); override;
    procedure ReadSettings({%H-}AOptions: TAbstractIDEOptions); override;
    procedure WriteSettings({%H-}AOptions: TAbstractIDEOptions); override;
    class function SupportedOptionsClass: TAbstractIDEOptionsClass; override;
  end;

procedure Register;

implementation

{$R *.lfm}

procedure Register;
begin
  RegisterIDEOptionsEditor(GroupDebugger, TIdeDbgConsoleWindowOptionsFrame,
    DbgOptionsConsoleWindow);
end;

{ TIdeDbgConsoleWindowOptionsFrame }

function TIdeDbgConsoleWindowOptionsFrame.GetTitle: String;
begin
  Result := dlgDebugConsoleWindowOptions;
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
  UpdateDescription;
end;

procedure TIdeDbgConsoleWindowOptionsFrame.WriteSettings(
  AOptions: TAbstractIDEOptions);
var
  s: String;
begin
  s := SelectedPlugInId;
  if s <> '' then
    DebuggerOptions.ConsoleWindowPlugInId := s;
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
end;

class function TIdeDbgConsoleWindowOptionsFrame.SupportedOptionsClass: TAbstractIDEOptionsClass;
begin
  Result := TDebuggerOptions;
end;

end.
