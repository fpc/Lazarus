unit CocoaEvent;

{$mode objfpc}{$H+}
{$modeswitch objectivec2}
{$include cocoadefines.inc}

{ Currently, only one tap is supported at a time }

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
    procedure releaseTap; virtual;
    function hasTap: Boolean; virtual;
    procedure enableTap; virtual;
    procedure disableTap; virtual;
    function authorized: Boolean; virtual;
    function promptAuthorization: Boolean; virtual;
  public
    destructor Destroy; override;
  end;

  TCocoaEventTapClass = class of TCocoaEventTap;

  { TCocoaEventTapUtil }

  TCocoaEventTapUtil = class
  public
    class procedure installTap( const tapClass: TCocoaEventTapClass );
    class procedure uninstallTap;
    class procedure enableTap;
    class procedure disableTap;
    class function promptAuthorization: Boolean;
  end;

implementation

var
  eventTap: TCocoaEventTap;

{ TCocoaEventTap }

procedure TCocoaEventTap.releaseTap;
begin
  if self.hasTap then begin
    CFRelease( _port );
    _port:= nil;
  end;
end;

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

destructor TCocoaEventTap.Destroy;
begin
  self.releaseTap;
end;

{ TCocoaEventTapUtil }

class procedure TCocoaEventTapUtil.installTap( const tapClass: TCocoaEventTapClass );
begin
  if NOT Assigned(eventTap) then
    eventTap:= tapClass.Create;
  eventTap.createTap;
end;

class procedure TCocoaEventTapUtil.uninstallTap;
begin
  if NOT Assigned(eventTap) then
    Exit;
  FreeAndNil( eventTap );
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
