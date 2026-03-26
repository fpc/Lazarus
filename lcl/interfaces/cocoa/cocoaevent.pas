unit CocoaEvent;

{$mode objfpc}{$H+}
{$modeswitch objectivec2}
{$include cocoadefines.inc}

interface

uses
  Classes, SysUtils,
  MacOSAll, CocoaAll,
  Cocoa_Extra;

type

  { TCocoaEventTap }

  TCocoaEventTap = class
  protected
    _port: CFMachPortRef;
  public
    procedure createTap; virtual; abstract;
    function hasTap: Boolean; virtual;
    procedure enableTap; virtual;
    procedure disableTap; virtual;
    function authorized: Boolean; virtual;
    function promptAuthorization: Boolean; virtual;
  end;

  TCocoaEventTapClass = class of TCocoaEventTap;

  { TCocoaEventTapUtil }

  TCocoaEventTapUtil = class
  public
    class procedure createTap( const tapClass: TCocoaEventTapClass );
    class procedure enableTap;
    class procedure disableTap;
    class function promptAuthorization: Boolean;
  end;

implementation

var
  eventTap: TCocoaEventTap;

{ TCocoaEventTap }

function TCocoaEventTap.hasTap: Boolean;
begin
  Result:= Assigned( _port );
end;

procedure TCocoaEventTap.enableTap;
begin
  if self.hasTap then
    CGEventTapEnable( self._Port, 1 );
end;

procedure TCocoaEventTap.disableTap;
begin
  if self.hasTap then
    CGEventTapEnable( self._Port, 0 );
end;

function TCocoaEventTap.authorized: Boolean;
begin
  Result:= AXIsProcessTrusted();
end;

function TCocoaEventTap.promptAuthorization: Boolean;
var
  options: NSDictionary;
begin
  options:= NSDictionary.dictionaryWithObject_forKey(
    NSNumber.numberWithBool(True),
    NSString(kAXTrustedCheckOptionPrompt) );
  Result:= AXIsProcessTrustedWithOptions( CFDictionaryRef(options) );
end;

{ TCocoaEventTapUtil }

class procedure TCocoaEventTapUtil.createTap( const tapClass: TCocoaEventTapClass );
begin
  if NOT Assigned(eventTap) then
    eventTap:= tapClass.Create;
  eventTap.createTap;
end;

class function TCocoaEventTapUtil.promptAuthorization: Boolean;
begin
  Result:= False;
  if NOT Assigned(eventTap) then
    Exit;
  Result:= eventTap.promptAuthorization;
end;

class procedure TCocoaEventTapUtil.enableTap;
begin
  if NOT Assigned(eventTap) then
    Exit;
  if NOT eventTap.authorized then
    Exit;
  if NOT eventTap.hasTap then
    eventTap.createTap;
  eventTap.enableTap;
end;

class procedure TCocoaEventTapUtil.disableTap;
begin
  if NOT Assigned(eventTap) then
    Exit;
  eventTap.disableTap;
end;

end.
