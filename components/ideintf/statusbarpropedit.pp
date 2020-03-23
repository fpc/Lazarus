{
 *****************************************************************************
  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************

 Property editor for TStatusBar objects

}
unit StatusBarPropEdit;

{$MODE OBJFPC}{$H+}

interface

uses
  Classes,
  // LCL
  ComCtrls,
  // IdeIntf
  PropEdits, ComponentEditors, ObjInspStrConsts;

type
  { TStatusBarComponentEditor }

  TStatusBarComponentEditor = class(TComponentEditor)
  public
    procedure ExecuteVerb(Index: Integer); override;
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;
  end;
 
implementation


{ TStatusBarComponentEditor }

procedure TStatusBarComponentEditor.ExecuteVerb(Index: Integer);
var
  Hook: TPropertyEditorHook;
  AStatusBar: TStatusBar;
begin
  if Index = 0 then
  begin
    GetHook(Hook);
    AStatusBar := GetComponent as TStatusBar;
    EditCollection(AStatusBar, AStatusBar.Panels, 'Panels');
    if Assigned(Hook) then Hook.Modified(Self);
  end;
end;

function TStatusBarComponentEditor.GetVerb(Index: Integer): string;
begin
  Result := '';
  if Index = 0 then Result := sccsSBEditPanels;
end;

function TStatusBarComponentEditor.GetVerbCount: Integer;
begin
  Result := 1;
end;

initialization
  //Register a component editor for TStatusBar
  RegisterComponentEditor(TStatusBar, TStatusBarComponentEditor);
  
end.
