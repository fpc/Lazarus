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
  SysUtils;

var
  MWSComponentList: TStringList;

function FindWSComponentClass(
  const AComponent: TComponentClass): TWSLCLComponentClass;
var
  idx: Integer;
  cls: TClass;
begin
  Result := nil;
  cls := AComponent;
  while cls <> nil do
  begin
    idx := MWSComponentList.IndexOf(cls.ClassName);
    if idx <> -1 
    then begin
      Result := TWSLCLComponentClass(MWSComponentList.Objects[idx]);
      Exit;
    end;
    cls := cls.ClassParent;
  end;
end;

procedure RegisterWSComponent(const AComponent: TComponentClass;
  const AWSComponent: TWSLCLComponentClass);
var
  idx: Integer;
  Name: String;
begin          
  Name := AComponent.ClassName;
  idx := MWSComponentList.IndexOf(Name);
  if idx = -1 
  then MWSComponentList.AddObject(Name, TObject(Pointer(AWSComponent)))
  else MWSComponentList.Objects[idx] := TObject(Pointer(AWSComponent));
end;


{ TWSLCLComponent }    

function TWSLCLComponent.CreateHandle(const AComponent: TComponent; 
  const AParams: TCreateParams): THandle;
begin
  // For now default to the old creation routines
  Result := InterfaceObject.IntfCreateHandle(AComponent, AParams);
end;
  
initialization
  MWSComponentList := TStringList.Create;
  MWSComponentList.Sorted := True;
  MWSComponentList.Duplicates := dupError;

finalization
  FreeAndNil(MWSComponentList);
  
end.
