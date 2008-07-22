{
 *****************************************************************************
 *                                                                           *
 *  This file is part of the Lazarus Component Library (LCL)                 *
 *                                                                           *
 *  See the file COPYING.modifiedLGPL.txt, included in this distribution,        *
 *  for details about the copyright.                                         *
 *                                                                           *
 *  This program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
 *                                                                           *
 *****************************************************************************
 
  Author: Mattias Gaertner

  Abtract:
   Registers a new designer base class (like TForm or TDataModule) in the IDE.
}
unit CustomComponentClass;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LCLProc, LResources, Forms, FormEditingIntf;
  
type

  { TMyComponentClass }

  TMyComponentClass = class(TComponent)
  private
    FDemoProperty: integer;
  protected
    procedure GetChildren(Proc: TGetChildProc; Root: TComponent); override;
  public
    constructor Create(TheOwner: TComponent); override;
  published
    property DemoProperty: integer read FDemoProperty write FDemoProperty;
  end;
  
procedure Register;


implementation


procedure Register;
begin
  FormEditingHook.RegisterDesignerBaseClass(TMyComponentClass);
end;

{ TMyComponentClass }

procedure TMyComponentClass.GetChildren(Proc: TGetChildProc; Root: TComponent);
// this method is called by TWriter to retrieve the child components to write
var
  I: Integer;
  OwnedComponent: TComponent;
begin
  DebugLn(['TMyComponentClass.GetChildren ComponentCount=',ComponentCount]);
  inherited GetChildren(Proc, Root);
  if Root = Self then begin
    for I := 0 to ComponentCount - 1 do
    begin
      OwnedComponent := Components[I];
      if not OwnedComponent.HasParent then Proc(OwnedComponent);
    end;
  end;
end;

constructor TMyComponentClass.Create(TheOwner: TComponent);
// init the component with an IDE resource
begin
  DebugLn(['TMyComponentClass.Create ',DbgSName(TheOwner)]);
  GlobalNameSpace.BeginWrite;
  try
    inherited Create(TheOwner);
    if (ClassType <> TMyComponentClass) and not (csDesigning in ComponentState)
    then begin
      if not InitResourceComponent(Self, TDataModule) then begin
        raise EResNotFound.Create('Resource missing for class '+ClassName);
      end;
    end;
  finally
    GlobalNameSpace.EndWrite;
  end;
end;

end.

