unit FpDbgCommon;

{$mode objfpc}{$H+}

interface

uses Classes;

type
// Target information, could be different from host debugger
  TMachineType = (mtNone, mtSPARC, mt386, mt68K, mtPPC, mtPPC64, mtARM, mtARM64,
                  mtOLD_ALPHA, mtIA_64, mtX86_64, mtAVR8, mtALPHA);
  TBitness = (bNone, b32, b64);
  TByteOrder = (boNone, boLSB, boMSB);
  TOperatingSystem = (osNone, osBSD, osDarwin, osEmbedded, osLinux, osUnix, osMac, osWindows);

  TTargetDescriptor = record
    machineType: TMachineType;
    bitness: TBitness;
    byteOrder: TByteOrder;
    OS: TOperatingSystem;
  end;

// This function returns the host descriptor
// Use when target information not yet loaded - assumes that debug target is the same as host
function hostDescriptor: TTargetDescriptor;

{$IFDEF FPDEBUG_THREAD_CHECK}
procedure AssertFpDebugThreadId(const AName: String);
procedure AssertFpDebugThreadIdNotMain(const AName: String);
procedure SetCurrentFpDebugThreadIdForAssert(AnId: TThreadID);
property CurrentFpDebugThreadIdForAssert: TThreadID write SetCurrentFpDebugThreadIdForAssert;
{$ENDIF}

implementation

function hostDescriptor: TTargetDescriptor;
begin
  with Result do
  begin
    // TODO: Expand list when debugger support updated for other targets
    machineType := {$if defined(CPU386) or defined(CPUI386)} mt386
                   {$elseif defined(CPUX86_64) or defined(CPUAMD64) or defined(CPUX64)} mtX86_64
                   {$elseif defined(CPUAARCH64)} mtARM64
                   {$elseif defined(CPUARM)} mtARM
                   {$elseif defined(CPUPOWERPC)} mtPPC
                   {$endif};
    bitness     := {$if defined(CPU64)} b64 {$elseif defined(CPU32)} b32 {$else} bNone {$endif};

    byteorder   := {$ifdef ENDIAN_LITTLE} boLSB {$else} boMSB {$endif};

    OS          := {$if defined(DARWIN)} osDarwin
                   {$elseif defined(EMBEDDED)} osEmbedded
                   {$elseif defined(LINUX)} osLinux
                   {$elseif defined(BSD)} osBSD
                   {$elseif defined(UNIX)} osUnix
                   {$elseif defined(MSWINDOWS)} osWindows {$endif};
  end;
end;

{$IFDEF FPDEBUG_THREAD_CHECK}
var
  FCurrentFpDebugThreadIdForAssert: TThreadID;
  FCurrentFpDebugThreadIdValidForAssert: Boolean;

procedure AssertFpDebugThreadId(const AName: String);
begin
  if FCurrentFpDebugThreadIdValidForAssert then
    assert(GetCurrentThreadId = FCurrentFpDebugThreadIdForAssert, AName);
end;

procedure AssertFpDebugThreadIdNotMain(const AName: String);
begin
  AssertFpDebugThreadId(AName);
  assert(GetCurrentThreadId<>MainThreadID, AName + ' runnig outside main thread');
end;

procedure SetCurrentFpDebugThreadIdForAssert(AnId: TThreadID);
begin
  FCurrentFpDebugThreadIdForAssert := AnId;
  FCurrentFpDebugThreadIdValidForAssert := True;
end;

{$ENDIF}

end.

