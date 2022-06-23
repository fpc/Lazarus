unit FpDebugDebuggerBase;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FPDbgController, FpdMemoryTools, FpDbgClasses, FpDbgUtil,
  DbgIntfDebuggerBase, FpDebugDebuggerUtils;

type

  TFpDebugDebuggerBase = class(TDebuggerIntf)
  protected
    FDbgController: TDbgController;
    FMemManager: TFpDbgMemManager;
    FMemReader: TDbgMemReader;
    FMemConverter: TFpDbgMemConvertorLittleEndian;
    FLockList: TFpDbgLockList;
    FWorkQueue: TFpThreadPriorityWorkerQueue;
  public
    property DbgController: TDbgController read FDbgController;
    property MemManager:    TFpDbgMemManager read FMemManager;
    property MemReader:     TDbgMemReader read FMemReader;
    property MemConverter:  TFpDbgMemConvertorLittleEndian read FMemConverter;
    property LockList:      TFpDbgLockList read FLockList;
    property WorkQueue:     TFpThreadPriorityWorkerQueue read FWorkQueue;
  end;


implementation

end.

