{ $Id$}
{
 *****************************************************************************
 *                              wslclclasses.pp                              * 
 *                              ---------------                              * 
 *                                                                           *
 *                                                                           *
 *****************************************************************************

 *****************************************************************************
 *                                                                           *
 *  This file is part of the Lazarus Component Library (LCL)                 *
 *                                                                           *
 *  See the file COPYING.LCL, included in this distribution,                 *
 *  for details about the copyright.                                         *
 *                                                                           *
 *  This program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
 *                                                                           *
 *****************************************************************************
}
unit WSLCLClasses;

{$mode objfpc}{$H+}

interface

uses
  Classes, LCLType, InterfaceBase;

type

  { TWSLCLComponent } 
  
  TWSLCLComponent = class(TObject) 
  private
  protected
  public
    class function CreateHandle(const AComponent: TComponent; 
                                const AParams: TCreateParams): THandle; virtual;
  end;

  TWSLCLComponentClass = class of TWSLCLComponent;


function FindWSComponentClass(const AComponent: TComponentClass): TWSLCLComponentClass;
procedure RegisterWSComponent(const AComponent: TComponentClass;
                              const AWSComponent: TWSLCLComponentClass);

implementation

uses
  SysUtils, LCLClasses;

{ TWSLCLComponent }    

function TWSLCLComponent.CreateHandle(const AComponent: TComponent; 
  const AParams: TCreateParams): THandle;
begin
  // For now default to the old creation routines
  Result := InterfaceObject.CreateComponent(AComponent);
end;

////////////////////////////////////////////////////
// Registration code
////////////////////////////////////////////////////
type
  PClassNode = ^TClassNode;
  TClassNode = record
    LCLClass: TComponentClass;
    WSClass: TWSLCLComponentClass;
    VClass: Pointer;
    Parent: PClassNode;
    Child: PClassNode;
    Sibling: PClassNode;
  end;

var
  MComponentIndex: TStringList;
  MWSRegisterIndex: TStringList;

function FindWSComponentClass(
  const AComponent: TComponentClass): TWSLCLComponentClass;
var
  idx: Integer;
  cls: TClass;
  Node: PClassNode;
begin
  Result := nil;
  cls := AComponent;
  while cls <> nil do
  begin
    idx := MWSRegisterIndex.IndexOf(cls.ClassName);
    if idx <> -1 
    then begin
      Node := PClassNode(MWSRegisterIndex.Objects[idx]);
      Result := Node^.WSClass;
      Exit;
    end;
    cls := cls.ClassParent;
  end;
end;

procedure RegisterWSComponent(const AComponent: TComponentClass;
  const AWSComponent: TWSLCLComponentClass);
  
  function FindNode(const AClass: TClass): PClassNode;
  var
    idx: Integer;
    Name: String;
  begin  
    if (AClass = nil)
    or not (AClass.InheritsFrom(TLCLComponent))
    then begin
      Result := nil;
      Exit;
    end;
    
    Name := AClass.ClassName;
    idx := MComponentIndex.IndexOf(Name);
    if idx = -1 
    then begin
      New(Result);
      Result^.LCLClass := TComponentClass(AClass);
      Result^.WSClass := nil;
      Result^.VClass := nil;
      Result^.Child := nil;
      Result^.Parent := FindNode(AClass.ClassParent);
      if Result^.Parent = nil
      then begin
        Result^.Sibling := nil;
      end
      else begin
        Result^.Sibling := Result^.Parent^.Child;
        Result^.Parent^.Child := Result;
      end;
      MComponentIndex.AddObject(Name, TObject(Result));
    end
    else begin
      Result := PClassNode(MComponentIndex.Objects[idx]);
    end;
  end;
  
var
  Node: PClassNode;
begin          
  Node := FindNode(AComponent);
  if Node = nil then Exit;
  
  if Node^.WSClass = nil
  then MWSRegisterIndex.AddObject(AComponent.ClassName, TObject(Node));
  
  Node^.WSClass := AWSComponent;
end;


initialization
  MComponentIndex := TStringList.Create;
  MComponentIndex.Sorted := True;
  MComponentIndex.Duplicates := dupError;

  MWSRegisterIndex := TStringList.Create;
  MWSRegisterIndex.Sorted := True;
  MWSRegisterIndex.Duplicates := dupError;

finalization
  FreeAndNil(MComponentIndex);
  FreeAndNil(MWSRegisterIndex);
  
end.
