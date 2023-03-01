{***************************************************************************
 *                                                                         *
 * This unit is distributed under the LGPL version 2                       *
 *                                                                         *
 * Additionally this unit can be used under any newer version (3 or up)    *
 * of the LGPL                                                             *
 *                                                                         *
 * Users are also granted the same "linknig exception" as defined          *
 * for the LCL.                                                            *
 * See the LCL license for details                                         *
 *                                                                         *
 *                                                                         *
 ***************************************************************************
 @author(Martin Friebe)
}
unit LazDebuggerTemplate;

{$mode objfpc}{$H+}
{$INTERFACES CORBA} // no ref counting needed

interface

uses
  Classes, SysUtils, fgl, LazDebuggerIntf;

type


  { TDbgDataRequestTemplateBase }

  generic TDbgDataRequestTemplateBase<_BASE: TObject; _SENDER_INTF: TDbgDataRequestIntf> = class(_BASE)
  private type
    TDbgDataRequestEventList = specialize TFPGList<TDbgDataRequestEvent>;
  private
    FEventLists: array [TDbgDataRequestEventType] of TDbgDataRequestEventList;
  protected
    procedure AddNotification(AnEventType: TDbgDataRequestEventType; AnEvent: TDbgDataRequestEvent);
    procedure RemoveNotification(AnEventType: TDbgDataRequestEventType; AnEvent: TDbgDataRequestEvent);
    procedure CallNotifications(AnEventType: TDbgDataRequestEventType; AnEventData: TDbgDataRequestEventData);

    procedure BeginUpdate; virtual;
    procedure EndUpdate; virtual;

    procedure DoDestroy; // FPC can not compile "destructor Destroy; override;"
  end;

  { TDbgDataRequestTemplate }

  generic TDbgDataRequestTemplate<_BASE: TObject; _SENDER_INTF: TDbgDataRequestIntf>
    = class(specialize TDbgDataRequestTemplateBase<_BASE, _SENDER_INTF>, TDbgDataRequestIntf)
  private type
    TNotifyEventList = specialize TFPGList<TNotifyEvent>;
  private
    FFreeNotifyList: TNotifyEventList;
  protected
    procedure AddFreeNotification(ANotification: TNotifyEvent);
    procedure RemoveFreeNotification(ANotification: TNotifyEvent);
    procedure CallFreeNotifications;

    procedure DoDestroy; // FPC can not compile "destructor Destroy; override;"
  end;

  { TInternalDbgMonitorBase }

  generic TInternalDbgMonitorBase<
    _BASE: TObject;
    _MONITOR_INTF: TInternalDbgMonitorIntfType;
    _SUPPLIER_INTF//: TInternalDbgSupplierIntfType
    >
    = class(_BASE)
  strict private
    FSupplier: _SUPPLIER_INTF;
  private
    procedure SetSupplier(ASupplier: _SUPPLIER_INTF);
    procedure RemoveSupplier(ASupplier: _SUPPLIER_INTF);
  protected
    procedure DoNewSupplier; virtual;

    procedure DoDestroy; // FPC can not compile "destructor Destroy; override;"
  public
    property Supplier: _SUPPLIER_INTF read FSupplier write SetSupplier;
  end;

  { TInternalDbgSupplierBase }

  generic TInternalDbgSupplierBase<
    _BASE: TObject;
    _SUPPLIER_INTF: TInternalDbgSupplierIntfType;
    _MONITOR_INTF //: TInternalDbgMonitorIntfType
    >
    = class(_BASE)
  strict private
    FMonitor: _MONITOR_INTF;
  private
    procedure SetMonitor(AMonitor: _MONITOR_INTF);
  protected
    procedure DoNewMonitor; virtual;

    procedure DoDestroy; // FPC can not compile "destructor Destroy; override;"

    property Monitor: _MONITOR_INTF read FMonitor;
  end;


type

  { TWatchesMonitorClassTemplate }

  generic TWatchesMonitorClassTemplate<_BASE: TObject> = class(
    specialize TInternalDbgMonitorBase<_BASE, TWatchesMonitorIntf, TWatchesSupplierIntf>,
    TWatchesMonitorIntf
  )
  protected
    procedure InvalidateWatchValues; virtual;
    procedure DoStateChange(const AOldState, ANewState: TDBGState); virtual; // deprecated;
  end;

  { TWatchesSupplierClassTemplate }

  generic TWatchesSupplierClassTemplate<_BASE: TObject> = class(
    specialize TInternalDbgSupplierBase<_BASE, TWatchesSupplierIntf, TWatchesMonitorIntf>,
    TWatchesSupplierIntf
  )
  protected
  public
    procedure RequestData(AWatchValue: TWatchValueIntf); virtual;
    procedure TriggerInvalidateWatchValues; virtual;
  end;

implementation

{ TDbgDataRequestTemplateBase }

procedure TDbgDataRequestTemplateBase.AddNotification(
  AnEventType: TDbgDataRequestEventType; AnEvent: TDbgDataRequestEvent);
begin
  if FEventLists[AnEventType] = nil then
    FEventLists[AnEventType] := TDbgDataRequestEventList.Create;

  FEventLists[AnEventType].Add(AnEvent);
end;

procedure TDbgDataRequestTemplateBase.RemoveNotification(
  AnEventType: TDbgDataRequestEventType; AnEvent: TDbgDataRequestEvent);
begin
  if FEventLists[AnEventType] = nil then
    exit;

  FEventLists[AnEventType].Remove(AnEvent);
end;

procedure TDbgDataRequestTemplateBase.CallNotifications(
  AnEventType: TDbgDataRequestEventType; AnEventData: TDbgDataRequestEventData);
var
  i: integer;
begin
  if FEventLists[AnEventType] = nil then
    exit;

  for i := FEventLists[AnEventType].Count - 1 downto 0 do
    FEventLists[AnEventType][i](Self as _SENDER_INTF, AnEventData)
end;

procedure TDbgDataRequestTemplateBase.BeginUpdate;
begin
  //
end;

procedure TDbgDataRequestTemplateBase.EndUpdate;
begin
  //
end;

procedure TDbgDataRequestTemplateBase.DoDestroy;
var
  i: TDbgDataRequestEventType;
begin
  for i := low(TDbgDataRequestEventType) to high(TDbgDataRequestEventType) do
    FEventLists[i].Free;
end;

{ TDbgDataRequestTemplate }

procedure TDbgDataRequestTemplate.AddFreeNotification(
  ANotification: TNotifyEvent);
begin
  if FFreeNotifyList = nil then
    FFreeNotifyList := TNotifyEventList.Create;

  FFreeNotifyList.Add(ANotification);
end;

procedure TDbgDataRequestTemplate.RemoveFreeNotification(
  ANotification: TNotifyEvent);
begin
  if FFreeNotifyList = nil then
    exit;

  FFreeNotifyList.Remove(ANotification);
end;

procedure TDbgDataRequestTemplate.CallFreeNotifications;
var
  i: integer;
begin
  if FFreeNotifyList = nil then
    exit;

  for i := FFreeNotifyList.Count - 1 downto 0 do
    FFreeNotifyList[i](nil)
end;

procedure TDbgDataRequestTemplate.DoDestroy;
begin
  FFreeNotifyList.Free;
  inherited DoDestroy;
end;

{ TInternalDbgMonitorBase }

procedure TInternalDbgMonitorBase.SetSupplier(ASupplier: _SUPPLIER_INTF);
begin
  if FSupplier = ASupplier then exit;
  assert((FSupplier=nil) or (ASupplier=nil), 'TInternalDbgMonitorBase.SetSupplier: (FSupplier=nil) or (ASupplier=nil)');

  if FSupplier <> nil then FSupplier.SetMonitor(nil);
  FSupplier := ASupplier;
  if FSupplier <> nil then FSupplier.SetMonitor(Self as _MONITOR_INTF);

  DoNewSupplier;
end;

procedure TInternalDbgMonitorBase.RemoveSupplier(ASupplier: _SUPPLIER_INTF);
begin
  if Supplier = ASupplier then
    Supplier := nil;
end;

procedure TInternalDbgMonitorBase.DoNewSupplier;
begin
  //
end;

procedure TInternalDbgMonitorBase.DoDestroy;
begin
  Supplier := nil;
end;

{ TInternalDbgSupplierBase }

procedure TInternalDbgSupplierBase.SetMonitor(AMonitor: _MONITOR_INTF);
begin
  if FMonitor = AMonitor then exit;
  assert((FMonitor=nil) or (AMonitor=nil), 'TInternalDbgSupplierBase.SetMonitor: (FMonitor=nil) or (AMonitor=nil)');
  FMonitor := AMonitor;

  DoNewMonitor;
end;

procedure TInternalDbgSupplierBase.DoNewMonitor;
begin
  //
end;

procedure TInternalDbgSupplierBase.DoDestroy;
begin
  if FMonitor <> nil then
    FMonitor.RemoveSupplier(Self as _SUPPLIER_INTF);
end;

{ TWatchesMonitorClassTemplate }

procedure TWatchesMonitorClassTemplate.InvalidateWatchValues;
begin
  //
end;

procedure TWatchesMonitorClassTemplate.DoStateChange(const AOldState,
  ANewState: TDBGState);
begin
  //
end;

{ TWatchesSupplierClassTemplate }

procedure TWatchesSupplierClassTemplate.RequestData(AWatchValue: TWatchValueIntf);
begin
  AWatchValue.Validity := ddsError;
end;

procedure TWatchesSupplierClassTemplate.TriggerInvalidateWatchValues;
begin
  if (Self <> nil) and (Monitor <> nil) then
    Monitor.InvalidateWatchValues;
end;


end.

