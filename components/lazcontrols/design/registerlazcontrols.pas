unit RegisterLazControls;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ExtendedTabControls, ComponentEditors, ObjInspStrConsts, PropEdits,
  ComCtrls;

type

  { TExtendedTabControlComponentEditor }

  TExtendedTabControlComponentEditor = class(TOldTabControlComponentEditor)
  public
    procedure ExecuteVerb(Index: Integer); override;
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;
    function ToolBar: TToolBar; virtual;
  end;


procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('LazControls',[TExtendedTabControl]);
  RegisterNoIcon([TExtendedTabToolbar, TExtendedTabToolButton]);
  RegisterComponentEditor(TExtendedTabControl, TExtendedTabControlComponentEditor);
end;

{ TExtendedTabControlComponentEditor }

procedure TExtendedTabControlComponentEditor.ExecuteVerb(Index: Integer);
var
  NewStyle: TToolButtonStyle;
  Hook: TPropertyEditorHook;
  NewToolButton: TToolButton;
  NewName: string;
  CurToolBar: TToolBar;
  SiblingButton: TToolButton;
  c: Integer;
begin
  c := inherited GetVerbCount;
  if Index < c then begin
    inherited ExecuteVerb(Index);
    exit;
  end;

  Index := Index - c;
  Hook:=nil;
  if not GetHook(Hook) then exit;
  case Index of
    0: NewStyle := tbsButton;
    1: NewStyle := tbsCheck;
    2: NewStyle := tbsSeparator;
    3: NewStyle := tbsDivider;
  else
    exit;
  end;
  CurToolBar := ToolBar;
  NewToolButton := TExtendedTabToolButton.Create(CurToolBar.Owner);
  NewName := GetDesigner.CreateUniqueComponentName(NewToolButton.ClassName);
  NewToolButton.Caption := NewName;
  NewToolButton.Name := NewName;
  NewToolButton.Style := NewStyle;
  if NewStyle = tbsDivider then
    NewToolButton.Width := 3;
  // position the button next to the last button
  if CurToolBar.ButtonCount > 0 then
  begin
    SiblingButton := CurToolBar.Buttons[CurToolBar.ButtonCount - 1];
    NewToolButton.SetBounds(SiblingButton.Left + SiblingButton.Width,
      SiblingButton.Top, NewToolButton.Width, NewToolButton.Height);
  end;
  NewToolButton.Parent := CurToolBar;
  Hook.PersistentAdded(NewToolButton, True);
  Modified;
end;

function TExtendedTabControlComponentEditor.GetVerb(Index: Integer): string;
var
  c: Integer;
begin
  c := inherited GetVerbCount;
  if Index < c then
    Result := inherited GetVerb(Index)
  else
    case Index - c of
      0: Result := tbceNewButton;
      1: Result := tbceNewCheckbutton;
      2: Result := tbceNewSeparator;
      3: Result := tbceNewDivider;
    else
      Result := '';
    end;
end;

function TExtendedTabControlComponentEditor.GetVerbCount: Integer;
begin
  Result := inherited GetVerbCount + 4;
end;

function TExtendedTabControlComponentEditor.ToolBar: TToolBar;
begin
  Result := nil;
  if TabControl = nil then
    exit;
  Result := TExtendedTabControlNoteBookStrings(TCustomExtendedTabControl(TabControl).Tabs).ToolBar;
end;

end.

