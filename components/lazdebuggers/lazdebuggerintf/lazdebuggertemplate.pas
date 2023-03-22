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

  generic TDbgDataRequestTemplateBase<_BASE: TObject; _SENDER_INTF: IDbgDataRequestIntf> = class(_BASE)
  private type
    TDbgDataRequestEventList = specialize TFPGList<TDbgDataRequestEvent>;
  strict private
    FEventLists: array [TDbgDataRequestEventType] of TDbgDataRequestEventList;
    FUpdateCount: Integer;
    function GetIsUpdating: boolean; inline;
  protected
    procedure AddNotification(AnEventType: TDbgDataRequestEventType; AnEvent: TDbgDataRequestEvent);
    procedure RemoveNotification(AnEventType: TDbgDataRequestEventType; AnEvent: TDbgDataRequestEvent);
    procedure CallNotifications(AnEventType: TDbgDataRequestEventType; AnEventData: TDbgDataRequestEventData);

    procedure BeginUpdate;
    procedure EndUpdate;
    procedure DoBeginUpdating; virtual;
    procedure DoEndUpdating; virtual;
    property UpdateCount: Integer read FUpdateCount;
    property IsUpdating: boolean read GetIsUpdating;

    procedure DoDestroy; // FPC can not compile "destructor Destroy; override;"
  end;

  { TDbgDataRequestTemplate }

  generic TDbgDataRequestTemplate<_BASE: TObject; _SENDER_INTF: IDbgDataRequestIntf>
    = class(specialize TDbgDataRequestTemplateBase<_BASE, _SENDER_INTF>, IDbgDataRequestIntf)
  private type
    TNotifyEventList = specialize TFPGList<TNotifyEvent>;
  strict private
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
    _MONITOR_INTF: IInternalDbgMonitorIntfType;
    _SUPPLIER_INTF//: IInternalDbgSupplierIntfType
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
    _SUPPLIER_INTF: IInternalDbgSupplierIntfType;
    _MONITOR_INTF //: IInternalDbgMonitorIntfType
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
    specialize TInternalDbgMonitorBase<_BASE, IDbgWatchesMonitorIntf, IDbgWatchesSupplierIntf>,
    IDbgWatchesMonitorIntf
  )
  protected
    procedure InvalidateWatchValues; virtual;
    procedure DoStateChange(const AOldState, ANewState: TDBGState); virtual; // deprecated;
  end;

  { TWatchesSupplierClassTemplate }

  generic TWatchesSupplierClassTemplate<_BASE: TObject> = class(
    specialize TInternalDbgSupplierBase<_BASE, IDbgWatchesSupplierIntf, IDbgWatchesMonitorIntf>,
    IDbgWatchesSupplierIntf
  )
  protected
  public
    procedure RequestData(AWatchValue: IDbgWatchValueIntf); virtual;
    procedure TriggerInvalidateWatchValues; virtual;
  end;

  { TLocalsMonitorClassTemplate }

  generic TLocalsMonitorClassTemplate<_BASE: TObject> = class(
    specialize TInternalDbgMonitorBase<_BASE, IDbgLocalsMonitorIntf, IDbgLocalsSupplierIntf>,
    IDbgLocalsMonitorIntf
  )
  protected
    procedure InvalidateLocalValues; virtual;
    procedure DoStateChange(const AOldState, ANewState: TDBGState); virtual; // deprecated;
  end;

  { TLocalsSupplierClassTemplate }

  generic TLocalsSupplierClassTemplate<_BASE: TObject> = class(
    specialize TInternalDbgSupplierBase<_BASE, IDbgLocalsSupplierIntf, IDbgLocalsMonitorIntf>,
    IDbgLocalsSupplierIntf
  )
  protected
  public
    procedure RequestData(ALocalsList: IDbgLocalsListIntf); virtual;
    procedure TriggerInvalidateLocalsValues; virtual;
  end;

implementation

{ TDbgDataRequestTemplateBase }

function TDbgDataRequestTemplateBase.GetIsUpdating: boolean;
begin
  Result := FUpdateCount > 0;
end;

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
  inc(FUpdateCount);
  if FUpdateCount = 1 then
    DoBeginUpdating;
end;

procedure TDbgDataRequestTemplateBase.EndUpdate;
begin
  dec(FUpdateCount);
  if FUpdateCount = 0 then
    DoEndUpdating;
end;

procedure TDbgDataRequestTemplateBase.DoBeginUpdating;
begin
  //
end;

procedure TDbgDataRequestTemplateBase.DoEndUpdating;
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

procedure TWatchesSupplierClassTemplate.RequestData(AWatchValue: IDbgWatchValueIntf);
begin
  AWatchValue.Validity := ddsError;
end;

procedure TWatchesSupplierClassTemplate.TriggerInvalidateWatchValues;
begin
  if (Self <> nil) and (Monitor <> nil) then
    Monitor.InvalidateWatchValues;
end;

{ TLocalsMonitorClassTemplate }

procedure TLocalsMonitorClassTemplate.InvalidateLocalValues;
begin
  //
end;

procedure TLocalsMonitorClassTemplate.DoStateChange(const AOldState,
  ANewState: TDBGState);
begin
  //
end;

{ TLocalsSupplierClassTemplate }

procedure TLocalsSupplierClassTemplate.RequestData(ALocalsList: IDbgLocalsListIntf
  );
begin
  ALocalsList.Validity := ddsError;
end;

procedure TLocalsSupplierClassTemplate.TriggerInvalidateLocalsValues;
begin
  if (Self <> nil) and (Monitor <> nil) then
    Monitor.InvalidateLocalValues;
end;


end.

