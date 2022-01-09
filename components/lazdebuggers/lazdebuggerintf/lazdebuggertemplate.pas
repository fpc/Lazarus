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

  generic TInternalDbgMonitorBase<
    _BASE: TObject;
    _SUPPLIER_INTF: TInternalDbgSupplierIntfType
    >
    = class(_BASE)
  private
    FSupplier: _SUPPLIER_INTF;
  protected
    property Supplier: _SUPPLIER_INTF read FSupplier;
  public
    procedure SetSupplier(AValue: _SUPPLIER_INTF); virtual;
  end;


  generic TInternalDbgSupplierBase<
    _BASE: TObject;
    _MONITOR_INTF: TInternalDbgMonitorIntfType
    >
    = class(_BASE)
  private
    FMonitor: _MONITOR_INTF;
  protected
    property Monitor: _MONITOR_INTF read FMonitor;
  public
    procedure SetMonitor(AMonitor: _MONITOR_INTF);
  end;


type

  { TWatchesMonitorBase }

  generic TWatchesMonitorBase<_BASE: TObject> = class(specialize TInternalDbgMonitorBase<_BASE, TWatchesSupplierIntf>, TWatchesMonitorIntf)
    procedure InvalidateWatchValues;
  end;

  { TWatchesSupplierBase }

  generic TWatchesSupplierBase<_BASE: TObject> = class(specialize TInternalDbgSupplierBase<_BASE, TWatchesMonitorIntf>, TWatchesSupplierIntf)
    procedure RequestData(AWatchValue: TWatchValueIntf);
    procedure TriggerInvalidateWatchValues;
  end;

implementation

procedure TInternalDbgMonitorBase.SetSupplier(AValue: _SUPPLIER_INTF);
begin
  FSupplier := AValue;
end;

procedure TInternalDbgSupplierBase.SetMonitor(AMonitor: _MONITOR_INTF);
begin
  FMonitor := AMonitor;
end;

{ TWatchesMonitorBase }

procedure TWatchesMonitorBase.InvalidateWatchValues;
begin

end;

{ TWatchesSupplierBase }

procedure TWatchesSupplierBase.RequestData(AWatchValue: TWatchValueIntf);
begin

end;

procedure TWatchesSupplierBase.TriggerInvalidateWatchValues;
begin

end;


end.

