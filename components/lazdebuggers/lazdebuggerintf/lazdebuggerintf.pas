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
unit LazDebuggerIntf;

{$mode objfpc}{$H+}
{$INTERFACES CORBA} // no ref counting needed

interface

uses
  Classes, SysUtils;

type

  {$REGION ***** Internal types ***** }

  TInternalDbgMonitorIntfType  = interface end;
  TInternalDbgSupplierIntfType = interface end;

  generic TInternalDbgMonitorIntf<_SUPPLIER_INTF> = interface(TInternalDbgMonitorIntfType)
    procedure SetSupplier(AValue: _SUPPLIER_INTF);
  end;

  generic TInternalDbgSupplierIntf<_MONITOR_INTF> = interface(TInternalDbgSupplierIntfType)
    procedure SetMonitor(AMonitor: _MONITOR_INTF);
  end;

  {$ENDREGION}

type

  TDebuggerDataState = (ddsUnknown,                    //
                        ddsRequested, ddsEvaluating,   //
                        ddsValid,                      // Got a valid value
                        ddsInvalid,                    // Does not have a value
                        ddsError                       // Error, but got some Value to display (e.g. error msg)
                       );

  TDbgDataRequestIntf = interface
    procedure AddFreeNotification(ANotification: TNotifyEvent);
    procedure RemoveFreeNotification(ANotification: TNotifyEvent);
  end;



{%region **********************************************************************
 ******************************************************************************
 **                                                                          **
 **   W A T C H T E S                                                        **
 **                                                                          **
 ******************************************************************************
 ******************************************************************************}

  TWatchDisplayFormat =
    (wdfDefault,
     wdfStructure,
     wdfChar, wdfString,
     wdfDecimal, wdfUnsigned, wdfFloat, wdfHex,
     wdfPointer,
     wdfMemDump, wdfBinary
    );

  TWatcheEvaluateFlag =
    (defNoTypeInfo,        // No Typeinfo object will be returned // for structures that means a printed value will be returned
     defSimpleTypeInfo,    // Returns: Kind (skSimple, skClass, ..); TypeName (but does make no attempt to avoid an alias)
     defFullTypeInfo,      // Get all typeinfo, resolve all anchestors
     defClassAutoCast,     // Find real class of instance, and use, instead of declared class of variable
     defAllowFunctionCall//,
//     defRawMemory,         // returns Array of bytes for hex dump
//     defNoValue            // Skip the value, if returning raw mem
    );
  TWatcheEvaluateFlags = set of TWatcheEvaluateFlag;

  TDBGTypeBase = class(TObject)
  end;

  { TWatchValueIntf }

  TWatchValueIntf = interface(TDbgDataRequestIntf)
    function GetDisplayFormat: TWatchDisplayFormat;
    function GetEvaluateFlags: TWatcheEvaluateFlags;
    function GetExpression: String;
    function GetRepeatCount: Integer;
    function GetStackFrame: Integer;
    function GetThreadId: Integer;
    function GetValidity: TDebuggerDataState;
    procedure SetTypeInfo(AValue: TDBGTypeBase);
    function GetValue: String; // FpGdbmiDebugger
    procedure SetValidity(AValue: TDebuggerDataState);
    procedure SetValue(AValue: String);

    property DisplayFormat: TWatchDisplayFormat read GetDisplayFormat;
    property EvaluateFlags: TWatcheEvaluateFlags read GetEvaluateFlags;
    property RepeatCount: Integer read GetRepeatCount;
    property ThreadId: Integer read GetThreadId;
    property StackFrame: Integer read GetStackFrame;
    property Expression: String read GetExpression;

    property Validity: TDebuggerDataState read GetValidity write SetValidity;
    property Value: String read GetValue write SetValue;
    property TypeInfo: TDBGTypeBase {read GetTypeInfo} write SetTypeInfo;
  end;



  TWatchesSupplierIntf = interface;

  TWatchesMonitorIntf = interface(specialize TInternalDbgMonitorIntf<TWatchesSupplierIntf>)
    procedure InvalidateWatchValues;
  end;

  TWatchesSupplierIntf = interface(specialize TInternalDbgSupplierIntf<TWatchesMonitorIntf>)
    procedure RequestData(AWatchValue: TWatchValueIntf);
    procedure TriggerInvalidateWatchValues;
  end;

{%endregion   ^^^^^  Watches  ^^^^^   }



implementation


end.

