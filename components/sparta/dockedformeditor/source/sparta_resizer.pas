{
 *****************************************************************************
  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************

  Author: Maciej Izak

  DaThoX 2004-2015
  FreeSparta.com
}

unit sparta_Resizer;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, ExtCtrls, Forms, StdCtrls,
  LCLType, LazIDEIntf, Buttons, SpartaAPI, Dialogs, FormEditingIntf,
  sparta_InterfacesMDI, sparta_BasicResizeFrame, sparta_BasicResizer;

type

  { TResizer }

  TResizer = class(TBasicResizer, IResizer)
  protected
    FMainDTU: ISTAMainDesignTimeUtil;
    FEDTU: TList;

  class var
    FStarter, FProfessional: TNotifyEvent;

    procedure SetDesignedForm(const AValue: IDesignedForm); override;
  public
    property MainDTU: ISTAMainDesignTimeUtil read FMainDTU;

    constructor Create(AParent: TWinControl; AResizerFrameClass: TResizerFrameClass); override;
    destructor Destroy; override;
  end;

implementation

{ TResizer }

procedure TResizer.SetDesignedForm(const AValue: IDesignedForm);
var
  LLookupRoot: TComponent;
begin
  inherited SetDesignedForm(AValue);

  if AValue <> nil then
  begin
    // in this place DesignedForm should be initialized by current editor (+ "sizer")
    // TODO some interfaces for utils (Design Time Utils - DTU) ?
    LLookupRoot := LookupRoot(DesignedForm.Form);

    if FMainDTU <> nil then
      FMainDTU.Root := LLookupRoot;
  end
  else
  begin
    if FMainDTU <> nil then
      FMainDTU.Root := nil;
  end;
end;

constructor TResizer.Create(AParent: TWinControl;
  AResizerFrameClass: TResizerFrameClass);
begin
  inherited Create(AParent, AResizerFrameClass);

  FEDTU := TList.Create;

  if DTUManager <> nil then
  begin
    FMainDTU := DTUManager.CreateMainDTU(pMainDTU, pAddons);
  end;

  if Assigned(FStarter) then
    FStarter(Self);
end;

destructor TResizer.Destroy;
begin
  FEDTU.Free;
  Pointer(FMainDTU) := nil; // released by owner

  inherited Destroy;
end;

end.

