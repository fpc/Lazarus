{ $Id$}
{
 *****************************************************************************
 *                               lclclasses.pp                               * 
 *                               -------------                               * 
 *                                                                           *
 *                                                                           *
 *****************************************************************************

 *****************************************************************************
 *                                                                           *
 *  This file is part of the Lazarus Component Library (LCL)                 *
 *                                                                           *
 *  See the file COPYING.modifiedLGPL, included in this distribution,        *
 *  for details about the copyright.                                         *
 *                                                                           *
 *  This program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
 *                                                                           *
 *****************************************************************************

  Defines the base class for all LCL TComponents including controls.
}
unit LCLClasses;

{$mode objfpc}{$H+}

interface

uses
  Classes, LCLProc, WSLCLClasses;

type

  { TLCLComponent }

  TLCLComponent = class(TComponent)
  private
    FWidgetSetClass: TWSLCLComponentClass;
  protected
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure BeforeDestruction; override; // fixes missing call to Destroying in FPC
    class function NewInstance: TObject; override;
    procedure RemoveAllHandlersOfObject(AnObject: TObject); virtual;
    property WidgetSetClass: TWSLCLComponentClass read FWidgetSetClass;
  end;

implementation                    

constructor TLCLComponent.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  {$IFDEF DebugLCLComponents}
  //DebugLn('TLCLComponent.Create ',DbgSName(Self));
  DebugLCLComponents.MarkCreated(Self,DbgSName(Self));
  {$ENDIF}
end;

destructor TLCLComponent.Destroy;
begin
  {$IFDEF DebugLCLComponents}
  //DebugLn('TLCLComponent.Destroy ',DbgSName(Self));
  DebugLCLComponents.MarkDestroyed(Self);
  {$ENDIF}
  inherited Destroy;
end;

procedure TLCLComponent.BeforeDestruction;
begin
  inherited;
  Destroying;
end;

class function TLCLComponent.NewInstance: TObject;
begin
  Result := inherited NewInstance; 
  TLCLComponent(Result).FWidgetSetClass := FindWSComponentClass(Self);
  if TLCLComponent(Result).FWidgetSetClass = nil
  then TLCLComponent(Result).FWidgetSetClass := TWSLCLComponent; 
end;

procedure TLCLComponent.RemoveAllHandlersOfObject(AnObject: TObject);
begin

end;

end.