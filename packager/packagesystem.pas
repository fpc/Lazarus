{  $Id$  }
{
 /***************************************************************************
                            packagesystem.pas
                            -----------------


 ***************************************************************************/

 ***************************************************************************
 *                                                                         *
 *   This source is free software; you can redistribute it and/or modify   *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 *   This code is distributed in the hope that it will be useful, but      *
 *   WITHOUT ANY WARRANTY; without even the implied warranty of            *
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU     *
 *   General Public License for more details.                              *
 *                                                                         *
 *   A copy of the GNU General Public License is available on the World    *
 *   Wide Web at <http://www.gnu.org/copyleft/gpl.html>. You can also      *
 *   obtain it by writing to the Free Software Foundation,                 *
 *   Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.        *
 *                                                                         *
 ***************************************************************************

  Author: Mattias Gaertner

  Abstract:
    The package registration.
}
unit PackageSystem;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, AVL_Tree, FileCtrl, Forms, Controls, Dialogs,
  LazarusIDEStrConsts, IDEProcs, PackageLinks, PackageDefs, LazarusPackageIntf;
  
type
  TLazPackageGraph = class
  private
    FAbortRegistration: boolean;
    FErrorMsg: string;
    FRegistrationFile: TPkgFile;
    FRegistrationPackage: TLazPackage;
    FRegistrationUnitName: string;
    FTree: TAVLTree; // sorted tree of TLazPackage
    FItems: TList;   // unsorted list of TLazPackage
    function GetPackages(Index: integer): TLazPackage;
    procedure SetAbortRegistration(const AValue: boolean);
    procedure SetErrorMsg(const AValue: string);
    procedure SetRegistrationPackage(const AValue: TLazPackage);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    function Count: integer;
    function FindLeftMostByName(const PkgName: string): TAVLTreeNode;
    function FindNextSameName(ANode: TAVLTreeNode): TAVLTreeNode;
    function PackageNameExists(const PkgName: string;
      IgnorePackage: TLazPackage): boolean;
    function CreateUniquePkgName(const Prefix: string;
      IgnorePackage: TLazPackage): string;
    function NewPackage(const Prefix: string): TLazPackage;
    procedure ConsistencyCheck;
    procedure RegisterUnit(const TheUnitName: string;
      RegisterProc: TRegisterProc);
    procedure RegistrationError(const Msg: string);
    procedure RegisterPackage(APackage: TLazPackage);
  public
    property Packages[Index: integer]: TLazPackage read GetPackages;
    property RegistrationPackage: TLazPackage read FRegistrationPackage
                                              write SetRegistrationPackage;
    property RegistrationUnitName: string read FRegistrationUnitName;
    property RegistrationFile: TPkgFile read FRegistrationFile;
    property ErrorMsg: string read FErrorMsg write SetErrorMsg;
    property AbortRegistration: boolean read FAbortRegistration
                                        write SetAbortRegistration;
  end;
  
var
  PackageGraph: TLazPackageGraph;

implementation

{ TLazPackageGraph }

function TLazPackageGraph.GetPackages(Index: integer): TLazPackage;
begin
  Result:=TLazPackage(FItems[Index]);
end;

procedure TLazPackageGraph.SetAbortRegistration(const AValue: boolean);
begin
  if FAbortRegistration=AValue then exit;
  FAbortRegistration:=AValue;
end;

procedure TLazPackageGraph.SetErrorMsg(const AValue: string);
begin
  if FErrorMsg=AValue then exit;
  FErrorMsg:=AValue;
end;

procedure TLazPackageGraph.SetRegistrationPackage(const AValue: TLazPackage);
begin
  if FRegistrationPackage=AValue then exit;
  FRegistrationPackage:=AValue;
  AbortRegistration:=false;
  LazarusPackageIntf.RegisterUnit:=@RegisterUnit;
end;

constructor TLazPackageGraph.Create;
begin
  FTree:=TAVLTree.Create(@CompareLazPackage);
  FItems:=TList.Create;
end;

destructor TLazPackageGraph.Destroy;
begin
  if LazarusPackageIntf.RegisterUnit=@RegisterUnit then
    LazarusPackageIntf.RegisterUnit:=nil;
  Clear;
  FItems.Free;
  FTree.Free;
  inherited Destroy;
end;

procedure TLazPackageGraph.Clear;
begin
  FTree.FreeAndClear;
  FItems.Clear;
end;

function TLazPackageGraph.Count: integer;
begin
  Result:=FItems.Count;
end;

function TLazPackageGraph.FindLeftMostByName(const PkgName: string
  ): TAVLTreeNode;
var
  PriorNode: TAVLTreeNode;
begin
  Result:=nil;
  if PkgName='' then exit;
  Result:=FTree.FindKey(PChar(PkgName),@CompareNameWithPackage);
  while Result<>nil do begin
    PriorNode:=FTree.FindPrecessor(Result);
    if (PriorNode=nil)
    or (AnsiCompareText(PkgName,TLazPackage(PriorNode.Data).Name)<>0) then
      break;
    Result:=PriorNode;
  end;
end;

function TLazPackageGraph.FindNextSameName(ANode: TAVLTreeNode): TAVLTreeNode;
var
  NextNode: TAVLTreeNode;
begin
  Result:=nil;
  if ANode=nil then exit;
  NextNode:=FTree.FindSuccessor(ANode);
  if (NextNode=nil)
  or (AnsiCompareText(TLazPackage(ANode.Data).Name,
                      TLazPackage(NextNode.Data).Name)<>0)
  then exit;
  Result:=NextNode;
end;

function TLazPackageGraph.PackageNameExists(const PkgName: string;
  IgnorePackage: TLazPackage): boolean;
var
  ANode: TAVLTreeNode;
begin
  Result:=false;
  if PkgName<>'' then begin
    ANode:=FindLeftMostByName(PkgName);
    if (ANode<>nil) and (IgnorePackage=TLazPackage(ANode.Data)) then
      ANode:=FindNextSameName(ANode);
    Result:=ANode<>nil;
  end;
end;

function TLazPackageGraph.CreateUniquePkgName(const Prefix: string;
  IgnorePackage: TLazPackage): string;
var
  i: Integer;
begin
  // try Prefix alone
  if not PackageNameExists(Prefix,IgnorePackage) then begin
    Result:=Prefix;
  end else begin
    // try Prefix + number
    i:=1;
    while PackageNameExists(Prefix+IntToStr(i),IgnorePackage) do inc(i);
    Result:=Prefix+IntToStr(i);
  end;
end;

function TLazPackageGraph.NewPackage(const Prefix: string): TLazPackage;
begin
  Result:=TLazPackage.Create;
  Result.Name:=CreateUniquePkgName('NewPackage',nil);
  FItems.Add(Result);
  FTree.Add(Result);
end;

procedure TLazPackageGraph.ConsistencyCheck;
begin
  CheckList(FItems,true,true,true);
end;

procedure TLazPackageGraph.RegisterUnit(const TheUnitName: string;
  RegisterProc: TRegisterProc);
begin
  if AbortRegistration then exit;

  ErrorMsg:='';
  FRegistrationFile:=nil;
  FRegistrationUnitName:='';

  // check package
  if FRegistrationPackage=nil then begin
    RegistrationError('');
    exit;
  end;
  try
    // check unitname
    FRegistrationUnitName:=TheUnitName;
    if not IsValidIdent(FRegistrationUnitName) then begin
      RegistrationError('Invalid Unitname: '+FRegistrationUnitName);
      exit;
    end;
    FRegistrationFile:=FRegistrationPackage.FindUnit(FRegistrationUnitName);
    if FRegistrationFile=nil then begin
      RegistrationError('Unit not found: '+FRegistrationUnitName);
      exit;
    end;
    // check registration procedure
    if RegisterProc=nil then begin
      RegistrationError('Register procedure is nil');
      exit;
    end;
    try
      // call the registration procedure
      RegisterProc();
    except
      on E: Exception do begin
        RegistrationError(E.Message);
      end;
    end;
    // clean up
  finally
    FRegistrationUnitName:='';
    FRegistrationFile:=nil;
  end;
end;

procedure TLazPackageGraph.RegistrationError(const Msg: string);
var
  DlgResult: Integer;
begin
  // create nice and useful error message

  // current registration package
  if FRegistrationPackage=nil then begin
    ErrorMsg:='RegisterUnit was called, but no package is registering.';
  end else begin
    ErrorMsg:='Package: "'+FRegistrationPackage.NameAndVersion+'"';
    // current unitname
    if FRegistrationUnitName<>'' then
      ErrorMsg:=ErrorMsg+#13+'Unit Name: "'+FRegistrationUnitName+'"';
    // current file
    if FRegistrationFile<>nil then
      ErrorMsg:=ErrorMsg+#13+'File Name: "'+FRegistrationFile.Filename+'"';
  end;
  // append message
  if Msg<>'' then
    ErrorMsg:=ErrorMsg+#13#13+Msg;
  // tell user
  DlgResult:=MessageDlg('Registration Error',
                        ErrorMsg,mtError,[mbIgnore,mbAbort],0);
  if DlgResult=mrAbort then
    AbortRegistration:=true;
end;

procedure TLazPackageGraph.RegisterPackage(APackage: TLazPackage);
begin
  RegistrationPackage:=APackage;
  // ToDo
end;

initialization
  PackageGraph:=nil;

end.

