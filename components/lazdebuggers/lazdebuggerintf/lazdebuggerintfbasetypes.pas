unit LazDebuggerIntfBaseTypes;

{$mode objfpc}{$H+}

interface

type

  (* TDBGPtr
     datatype pointing to data on the target
  *)
  TDBGPtr = type QWord;
  PDBGPtr = ^TDBGPtr;
  TDBGPtrArray = Array of TDBGPtr;

  { Debugger states
    --------------------------------------------------------------------------
    dsNone:
      The debug object is created, but no instance of an external debugger
      exists.
      Initial state, leave with Init, enter with Done

    dsIdle:
      The external debugger is started, but no filename (or no other params
      required to start) were given.

    dsStop:
      (Optional) The execution of the target is stopped
      The external debugger is loaded and ready to (re)start the execution
      of the target.
      Breakpoints, watches etc can be defined

    dsPause:
      The debugger has paused the target. Target variables can be examined

    dsInternalPause:
      Pause, not visible to user.
      For examble auto continue breakpoint: Allow collection of Snapshot data

    dsInit:
      (Optional, Internal) The debugger is about to run

    dsRun:
      The target is running.

    dsError:
      Something unforseen has happened. A shutdown of the debugger is in
      most cases needed.

    -dsDestroying
      The debugger is about to be destroyed.
      Should normally happen immediate on calling Release.
      But the debugger may be in nested calls, and has to exit them first.
    --------------------------------------------------------------------------
  }
  TDBGState = (
    dsNone,
    dsIdle,
    dsStop,
    dsPause,
    dsInternalPause,
    dsInit,
    dsRun,
    dsError,
    dsDestroying
    );

  TDbgThreadState = (dtsUnknown, dtsRunning, dtsPaused, dtsSuspended);

  (* Which of the debuggee's standard streams a piece of captured output came
     from.

     dtcUnknown is not an error case. It means the backend did not tell us:
     either the streams were captured merged (which is the only form that
     preserves the order the program wrote in), or the backend does not
     distinguish them at all. A display may colour dtcStdErr differently; it
     cannot do anything useful with the distinction between "merged" and "not
     supported", so there is deliberately only the one value for both. *)
  TLzDbgTargetIoChannel = (dtcUnknown, dtcStdOut, dtcStdErr);

implementation

end.

