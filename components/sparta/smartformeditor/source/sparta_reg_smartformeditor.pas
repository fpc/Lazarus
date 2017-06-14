{
 *****************************************************************************
  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************

  Author: Maciej Izak

  DaThoX 2004-2015
  FreeSparta.com
}

unit sparta_reg_SmartFormEditor;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, SpartaAPI, sparta_EDTU_Main, Controls, FormEditingIntf, sparta_FakeFormBG;

type

  { TStarterDesignTimeUtilsManager }

  TStarterDesignTimeUtilsManager = class(TSTADesignTimeUtilsManager)
  public
    function CreateMainDTU(AParent, AAddons: TWinControl): ISTAMainDesignTimeUtil; override;
  end;

procedure Register;

implementation

var
  Manager: TStarterDesignTimeUtilsManager = nil;

procedure Register;
begin
  FormEditingHook.StandardDesignerBaseClasses[DesignerBaseClassId_TForm] := TFakeFormBG;
  Manager := TStarterDesignTimeUtilsManager.Create;
  DTUManager := Manager;
end;

{ TStarterDesignTimeUtilsManager }

function TStarterDesignTimeUtilsManager.CreateMainDTU(AParent,
  AAddons: TWinControl): ISTAMainDesignTimeUtil;
var
  LMain: TedtuMain;
begin
  LMain := TedtuMain.Create(AParent);
  with LMain do
  begin
    Parent := AParent;
    Align := alTop;
    AParent.Height := 22;
    Height := 22;
    pAddons := AAddons;
  end;
  Result := LMain;
end;

finalization
  Manager.Free;
end.

