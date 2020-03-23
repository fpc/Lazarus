{
 *****************************************************************************
  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************

 Property editor for THeaderControl objects

}
unit HeaderControlPropEdit;

{$MODE OBJFPC}{$H+}

interface

uses
  Classes,
  // LCL
  ComCtrls,
  // IdeIntf
  PropEdits, ComponentEditors, ObjInspStrConsts;

type
  { THeaderControlComponentEditor }

  THeaderControlComponentEditor = class(TComponentEditor)
  public
    procedure ExecuteVerb(Index: Integer); override;
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;
  end;
 
implementation


{ THeaderControlComponentEditor }

procedure THeaderControlComponentEditor.ExecuteVerb(Index: Integer);
var
  Hook: TPropertyEditorHook;
  AHeaderControl: THeaderControl;
begin
  if Index = 0 then
  begin
    GetHook(Hook);
    AHeaderControl := GetComponent as THeaderControl;
    EditCollection(AHeaderControl, AHeaderControl.Sections, 'Sections');
    if Assigned(Hook) then Hook.Modified(Self);
  end;
end;

function THeaderControlComponentEditor.GetVerb(Index: Integer): string;
begin
  Result := '';
  if Index = 0 then Result := sccsHCEditSections;
end;

function THeaderControlComponentEditor.GetVerbCount: Integer;
begin
  Result := 1;
end;

initialization
  //Register a component editor for THeaderControl
  RegisterComponentEditor(THeaderControl, THeaderControlComponentEditor);
  
end.
