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
unit wslclclasses;

{$mode objfpc}{H+}

interface

uses
  classes;

type

  { TWSLCLComponent } 
  
  TWSLCLComponentClass = class of TWSLCLComponent;
  TWSLCLComponent = class(TObject)
  private
  protected
  public
  end;
  
function FindWSComponentClass(const AComponent: TComponentClass): TWSLCLComponentClass;
procedure RegisterWSComponent(const AComponent: TComponentClass; const AWSComponent: TWSLCLComponentClass);

implementation

uses
  SysUtils;

var
  MWSComponentList: TStringList;

function FindWSComponentClass(const AComponent: TComponentClass): TWSLCLComponentClass;
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

procedure RegisterWSComponent(const AComponent: TComponentClass; const AWSComponent: TWSLCLComponentClass);
var
  idx: Integer;
  Name: String;
begin          
  Name := AComponent.ClassName;
  idx := MWSComponentList.IndexOf(Name);
  if idx = -1 
  then MWSComponentList.AddObject(Name, TObject(AWSComponent))
  else MWSComponentList.Objects[idx] := TObject(AWSComponent);
end;
  
  
initialization
  MWSComponentList := TStringList.Create;
  MWSComponentList.Sorted := True;
  MWSComponentList.Duplicates := dupError;

finalization
  FreeAndNil(MWSComponentList);
  
end.
