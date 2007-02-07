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
  Classes, WSLCLClasses, LCLType, LCLProc;

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
  
  { TLCLHandleComponent }         
  // A base class for all components having a handle

  TLCLHandleComponent = class(TLCLComponent)
  private
    FHandle: TLCLIntfHandle;
    FCreating: Boolean; // Set if we are creating the handle
    function  GetHandle: TLCLIntfHandle;
    procedure HandleNeeded;
  protected
    procedure CreateHandle; 
    procedure CreateParams(var AParams: TCreateParams); virtual;
    procedure DestroyHandle; 
    procedure HandleCreated; virtual;    // gets called after the Handle is created
    procedure HandleDestroying; virtual; // gets called before the Handle is destroyed
  protected
    property Handle: TLCLIntfHandle read GetHandle;
  public             
    function HandleAllocated: Boolean;
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

{ TLCLHandleComponent }

function TLCLHandleComponent.GetHandle: TLCLIntfHandle;
begin
  if FHandle = 0 then HandleNeeded;
  Result := FHandle;
end;

procedure TLCLHandleComponent.HandleNeeded;
begin
  if FHandle <> 0 then Exit;
  if FCreating
  then begin
    // raise some error ?
    DebugLn('TLCLHandleComponent: Circulair handle creation');
    Exit;
  end;

  FCreating := True;
  try
    CreateHandle;
    if FHandle = 0
    then begin
      // raise some error ?
      DebugLn('TLCLHandleComponent: Handle creation failed');
      Exit;
    end;
  finally
    FCreating := False;
  end;
  HandleCreated;
end;

procedure TLCLHandleComponent.CreateHandle;
var
  Params: TCreateParams;
begin
  CreateParams(Params);
  // TODO: some WScall here
end;

procedure TLCLHandleComponent.CreateParams(var AParams: TCreateParams);
begin
end;

procedure TLCLHandleComponent.DestroyHandle;
begin
  HandleDestroying;
  // TODO: some WScall here
  FHandle := 0;
end;

procedure TLCLHandleComponent.HandleCreated;
begin
end;

procedure TLCLHandleComponent.HandleDestroying;
begin
end;

function TLCLHandleComponent.HandleAllocated: Boolean;
begin
  Result := FHandle <> 0;
end;

end.

