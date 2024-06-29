{
    This file is part of the Free Component Library (FCL)
    Copyright (c) 1999-2000 by the Free Pascal development team

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
{$IFNDEF FPC_DOTTEDUNITS}
unit process331;
{$ENDIF FPC_DOTTEDUNITS}
{$define windows}
{$mode objfpc}
{$h+}
{$modeswitch typehelpers}

interface

{$IFDEF FPC_DOTTEDUNITS}
Uses System.Classes,
     System.Pipes,
     System.SysUtils,
     System.Math;
{$ELSE FPC_DOTTEDUNITS}
Uses Classes,
     pipes331,
     SysUtils,
     Math;
{$ENDIF FPC_DOTTEDUNITS}

Type
  TProcessOption = (poRunSuspended,poWaitOnExit,
                    poUsePipes,poStderrToOutPut,
                    poNoConsole,poNewConsole,
                    poDefaultErrorMode,poNewProcessGroup,
                    poDebugProcess,poDebugOnlyThisProcess,poDetached,
                    poPassInput,poRunIdle);
  TProcessOptionHelper = type helper for TProcessOption
    function ToString : String;
  end;

  TShowWindowOptions = (swoNone,swoHIDE,swoMaximize,swoMinimize,swoRestore,swoShow,
                        swoShowDefault,swoShowMaximized,swoShowMinimized,
                        swoshowMinNOActive,swoShowNA,swoShowNoActivate,swoShowNormal);
  TShowWindowOptionsHelper = type helper for TShowWindowOptions
    function ToString : String;
  end;

  TStartupOption = (suoUseShowWindow,suoUseSize,suoUsePosition,
                    suoUseCountChars,suoUseFillAttribute);
  TStartupOptionHelper = type helper for TStartupOption
    function ToString : String;
  end;

  // only win32/64 and wince uses this. wince doesn't have the constants in the headers for the latter two.
  // unix defines them (as nice levels?), but doesn't use them.
  TProcessPriority = (ppHigh,ppIdle,ppNormal,ppRealTime{$ifndef wince},ppBelowNormal,ppAboveNormal{$endif});
  TProcessPriorityhelper = type helper for TProcessPriority
    function ToString : String;
  end;

  TProcessOptions = set of TProcessOption;
  TStartupOptions = set of TStartupOption;

  TRunCommandEventCode = (RunCommandIdle,RunCommandReadOutputString,RunCommandReadOutputStream,RunCommandFinished,RunCommandException);
  TRunCommandEventCodeHelper = type helper for TRunCommandEventCode
    function ToString : string;
  end;

  TRunCommandEventCodeSet = set of TRunCommandEventCode;
  TOnRunCommandEvent = procedure(Sender,Context : TObject;Status:TRunCommandEventCode;const Message:string) of object;
  EProcess = Class(Exception);

  {$ifdef UNIX}
  TProcessForkEvent = procedure(Sender : TObject) of object;
  {$endif UNIX}

  TIOType = (iotDefault, iotPipe, iotFile, iotHandle, iotProcess, iotNull);
  TIOTypeHelper = type helper for TIOType
    function ToString : string;
  end;

  TProcessHandleType = (phtInput,phtOutput,phtError);
  TProcessHandleTypeHelper = type helper for TProcessHandleType
    function ToString : string;
  end;

  TGetHandleEvent = procedure(Sender : TObject; var aHandle : THandle; var CloseOnExecute : Boolean) of object;
  TAfterAllocateHandleEvent = procedure(Sender : TObject; aHandle : THandle; var CloseOnExecute : Boolean) of object;

  { TIODescriptor }
  TProcess = Class;

{$macro on}
{define processunicodestring}

{$i processbody.inc}

end.
