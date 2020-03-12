unit FpDbgCommon;

{$mode objfpc}{$H+}

interface

type
// Target information, could be different from host debugger
  TMachineType = (mtNone, mtSPARC, mt386, mt68K, mtPPC, mtPPC64, mtARM,
                  mtOLD_ALPHA, mtIA_64, mtX86_64, mtAVR8, mtALPHA);
  TBitness = (bNone, b32, b64);
  TByteOrder = (boNone, boLSB, boMSB);
  TOperatingSystem = (osNone, osBSD, osDarwin, osEmbedded, osLinux, osMac, osWindows);

  TTargetDescriptor = record
    machineType: TMachineType;
    bitness: TBitness;
    byteOrder: TByteOrder;
    OS: TOperatingSystem;
  end;

// This function returns the host descriptor
// Use when target information not yet loaded - assumes that debug target is the same as host
function hostDescriptor: TTargetDescriptor;

implementation

function hostDescriptor: TTargetDescriptor;
begin
  with Result do
  begin
    // TODO: Expand list when debugger support updated for other targets
    machineType := {$if defined(CPU386) or defined(CPUI386)} mt386
                   {$elseif defined(CPUX86_64) or defined(CPUAMD64) or defined(CPUX64)} mtX86_64
                   {$elseif defined(CPUARM)} mtARM
                   {$elseif defined(CPUPOWERPC)} mtPPC
                   {$endif};
    bitness     := {$if defined(CPU64)} b64 {$elseif defined(CPU32)} b32 {$else} bNone {$endif};

    byteorder   := {$ifdef ENDIAN_LITTLE} boLSB {$else} boMSB {$endif};

    OS          := {$if defined(DARWIN)} osDarwin
                   {$elseif defined(LINUX)} osLinux
                   {$elseif defined(MSWINDOWS)} osWindows {$endif};
  end;
end;

end.

