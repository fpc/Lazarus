{  $Id$  }
{
 /***************************************************************************
                            lazaruspackageintf.pas
                            ----------------------


 ***************************************************************************/

 *****************************************************************************
 *  See the file COPYING.modifiedLGPL.txt, included in this distribution,
 *  for details about the license.
 *****************************************************************************

  Author: Mattias Gaertner

  Abstract:
    API for packages and registration of units and packages.
}
unit LazarusPackageIntf;

{$mode objfpc}{$H+}
{$Interfaces CORBA}

interface

uses
  {$IFDEF FPC_DOTTEDUNITS}
  System.Classes, System.SysUtils, System.FGL;
  {$ELSE}
  Classes, SysUtils, fgl;
  {$ENDIF}

type
  TRegisterProc = procedure;

  TRegisterUnitProc = procedure(const TheUnitName: string;
                                RegisterProc: TRegisterProc) of object;

type
  TRegisteredPackage = record
    Name: string;
    RegisterProc: TRegisterProc;
  end;
  PRegisteredPackage = ^TRegisteredPackage;

  (* ISetupDlgFrame - Used in initial setup dlg
     Registration must be done from an initialization section.
     The dialog is shown before the "Register" procedures of package are called
  *)

  ISetupDlgFrame = interface;

  // Ordered by severity
  TInitSetupDlgFrameState = ( // icon to show in SetupDlg
    issOk, issInfo, issHint, issNote, issWarning, issError, issFatal
  );
  TInitSetupDlgFrameAction = ( // actions needed
    isaReady,
    isaRestartNeeded
    //isaRebuildNeeded
  );
  ISetupDlgProvider = interface ['{DF17C90A-FED6-4D04-8BFA-B8C2DE84876F}']
    procedure FrameStateChanged(AnSender: ISetupDlgFrame; AState: TInitSetupDlgFrameState; AnAction: TInitSetupDlgFrameAction);
    procedure SetGroupCaption(AGroupId, AName: String);
  end;

  ISetupDlgFrame = interface ['{2516A45E-32D5-4826-AA7E-45163437E7E9}']
    (* RequireSetup: The package wants the initial setup dialog shown, it requires user feedback *)
    function  RequireSetup: boolean;
    (* AddToDialog: Package should perform all necessary GUI setup
       During creation, he package will receive 3 calls in the following order:
       - AddToDialog: To setup GUI
       - Init:        To do any required work, except GUI
       - UpdateState: To indicate its initial state. It may also Update it's own GUI
    *)
    procedure AddToDialog(AnOwner, AParent: TComponent;  // AParent: TWinControl that will hold the frame
                          ADialog: ISetupDlgProvider
                         );
    (* Init: Any heavy initialization work. No GUI work. (This might be  run in a thread) *)
    procedure Init;                             // Called once
    (* Done: Teardown, Owner/Parent get freed. THe frame will be freed if it is owned. *)
    procedure Done;                             // Called once // Owner/Parent get freed
    function  Caption: String;
    function  SortOrder: Integer;               // 3rd Party start at 1000 upwards
    function  UniqueId: String;
    function  GroupId: String;
    (* PageSelected: Either the user selected the page, or it was show automatically *)
    procedure PageSelected(AnUserActivated: Boolean);
    (* UpdateState: The Package should call all methods on ISetupDlgProvider needed to indicate its state.
       It must do so before it returns.
       Any state (if more states are added in future) that is not indicated shall be assumed default or undefined.
       States must be indicated, even if they have not changed since the last time.
     *)
    procedure UpdateState;
    (* ApplySelection: Called before the IDE is started *)
    procedure ApplySelection;
    function  Internal: TObject;                // for usage by the package
  end;


  TSetupDlgFrameList = specialize TFPGList<ISetupDlgFrame>;

var
  RegisteredPackages: TFPList; // list of PRegisteredPackage
  RegisterUnitProc: TRegisterUnitProc;
  TheSetupDlgFrameList: TSetupDlgFrameList;

procedure RegisterUnit(const TheUnitName: string; RegisterProc: TRegisterProc);
procedure RegisterPackage(const ThePackageName: string;
                          RegisterProc: TRegisterProc);
procedure ClearRegisteredPackages;

function SetupDlgFrameList: TSetupDlgFrameList;

implementation

procedure RegisterUnit(const TheUnitName: string; RegisterProc: TRegisterProc);
begin
  RegisterUnitProc(TheUnitName,RegisterProc);
end;

procedure RegisterPackage(const ThePackageName: string;
  RegisterProc: TRegisterProc);
var
  NewRegisteredPackage: PRegisteredPackage;
begin
  if RegisteredPackages=nil then RegisteredPackages:=TFPList.Create;
  New(NewRegisteredPackage);
  NewRegisteredPackage^.Name:=ThePackageName;
  NewRegisteredPackage^.RegisterProc:=RegisterProc;
  RegisteredPackages.Add(NewRegisteredPackage);
end;

procedure ClearRegisteredPackages;
var
  RegisteredPackage: PRegisteredPackage;
  i: Integer;
begin
  if RegisteredPackages<>nil then begin
    for i:=0 to RegisteredPackages.Count-1 do begin
      RegisteredPackage:=PRegisteredPackage(RegisteredPackages[i]);
      Dispose(RegisteredPackage);
    end;
    RegisteredPackages.Free;
    RegisteredPackages:=nil;
  end;
end;

function SetupDlgFrameList: TSetupDlgFrameList;
begin
  if TheSetupDlgFrameList = nil then
    TheSetupDlgFrameList := TSetupDlgFrameList.Create;
  Result := TheSetupDlgFrameList;
end;

procedure InternalInit;
begin
  RegisterUnitProc:=nil;
  RegisteredPackages:=nil;
end;

procedure InternalFinal;
begin
  ClearRegisteredPackages;
end;

initialization
  InternalInit;

finalization
  InternalFinal;
  FreeAndNil(TheSetupDlgFrameList);

end.

