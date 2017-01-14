{
 *****************************************************************************
 *                             MUIglobal.pas                                 *
 *                              --------------                               *
 *     Global functions for easier implementation of different Systems       *
 *                                                                           *
 *****************************************************************************

 *****************************************************************************
  This file is part of the Lazarus Component Library (LCL)

  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************
}
unit MUIglobal;

{$mode objfpc}{$H+}
{$if defined(AROS) and defined(VER3_0)}
  {$define FPC4AROS_VER3_FIXES}
{$endif}
interface

uses
  Classes, SysUtils, exec, amigados, intuition, agraphics, timer,
{$if defined(CPU68) or defined(CPUPOWERPC)}
  {$if defined(AMIGA68k) or defined(MorphOS)}
  amigalib,
  {$endif}
{$endif}
  utility, mui, tagsparamshelper;

{$ifdef MorphOS}
// Missing in the fpc units
const
  RPTAG_PenMode    = $80000080;
  RPTAG_FgColor    = $80000081;
  RPTAG_BgColor    = $80000082;
{$endif}
{$ifdef AmigaOS4}
// Colorsetting tags are different to AROS/MorphOS
const
  RPTAG_FGCOLOR = RPTAG_APENCOLOR;
  RPTAG_BGCOLOR = RPTAG_BPENCOLOR;
  RPTAG_PENMODE = TAG_IGNORE;
{$endif}
{$if defined(Amiga68k) and (FPC_FULLVERSION<30101)}
const
  IECODE_MBUTTON   = $6A;
  IECODE_UP_PREFIX = $80;
  MIDDLEUP         = IECODE_MBUTTON + IECODE_UP_PREFIX;
  MIDDLEDOWN       = IECODE_MBUTTON;
{$endif}


type
  THookFunc = function(Hook: PHook; Obj: PObject_; Msg: Pointer): LongInt;

function GetLCLTime: Int64;

procedure ConnectHookFunction(MUIField: PtrUInt; TriggerValue: PtrUInt; Obj: PObject_; Data: TObject; Hook: PHook; HookFunc: THookFunc);
procedure SetHook(var Hook: THook; Func: THookFunc; Data: Pointer);
{$ifndef AROS}
function CallHook(h: PHook; obj: APTR; params: array of NativeUInt): LongWord;
{$endif}
function CreateRastPortA: PRastPort; inline;
function CloneRastPortA(Rp: PRastPort): PRastPort; inline;
procedure FreeRastPortA(Rp: PRastPort); inline;

{$ifdef FPC4AROS_VER3_FIXES}
function DoMethod(Obj: PObject_; const Args: array of PtrUInt): IPTR;
function GetAttr(AttrID: LongWord; Object_: PObject_; var Storage: IPTR): LongWord; overload syscall IntuitionBase 109;
{$endif}
{$ifdef MorphOS}
function DoMethodA(obj : pObject_; msg1 : Pointer): longword; overload;
{$endif}

{$ifdef Amiga68k}
var
  IntuitionBase: PIntuitionBase;
{$endif}
{$ifdef Amiga}
function DoMethodA(obj : pObject_; msg : APTR): ulong;
function DoMethod(obj: Pointer; params: array of DWord): LongWord; overload;
function DoMethod(obj: LongWord; params: array of DWord): LongWord; overload;
{$endif}

implementation

// *****************************************************
// Use local GetMsCount with fixed timer.device, faster
// because it's polled very often (CheckTimer)
// But its not threadsafe!
// can be removed if a threadvar version is implemented in RTL
var
  Tr: PTimeRequest = nil;

procedure NewList (list: pList);
begin
  with list^ do
  begin
    lh_Head := PNode(@lh_Tail);
    lh_Tail := nil;
    lh_TailPred := PNode(@lh_Head)
  end;
end;

function CreateExtIO(Port: PMsgPort; Size: LongInt): PIORequest;
begin
  Result := nil;
  if Port <> nil then
  begin
    {$if FPC_FULLVERSION<30101}
    Result := Exec.AllocMem(Size, MEMF_CLEAR);
    {$else}
    Result := ExecAllocMem(Size, MEMF_CLEAR);
    {$endif}
    if Result <> nil then
    begin
      Result^.io_Message.mn_Node.ln_Type := 7;
      Result^.io_Message.mn_Length := Size;
      Result^.io_Message.mn_ReplyPort := Port;
    end;
  end;
end;

procedure DeleteExtIO (IoReq: PIORequest);
begin
  if IoReq <> nil then
  begin
    IoReq^.io_Message.mn_Node.ln_Type := $FF;
    IoReq^.io_Message.mn_ReplyPort := PMsgPort(-1);
    IoReq^.io_Device := PDevice(-1);
    ExecFreeMem(IoReq, IoReq^.io_Message.mn_Length);
  end
end;

function Createport(Name: PChar; Pri: LongInt): PMsgPort;
var
  sigbit: ShortInt;
begin
  Result := nil;
  SigBit := AllocSignal(-1);
  if SigBit = -1 then
   Exit;
  {$if FPC_FULLVERSION<30101}
  Result := Exec.AllocMem(SizeOf(TMsgPort), MEMF_CLEAR);
  {$else}
  Result := ExecAllocMem(SizeOf(TMsgPort), MEMF_CLEAR);
  {$endif}
  if Result = nil then
  begin
    FreeSignal(SigBit);
    Exit;
  end;
  with Result^ do
  begin
    if Assigned(Name) then
      mp_Node.ln_Name := Name
    else
      mp_Node.ln_Name := nil;
    mp_Node.ln_Pri := Pri;
    mp_Node.ln_Type := 4;
    mp_Flags := 0;
    mp_SigBit := SigBit;
    mp_SigTask := FindTask(nil);
  end;
  if Assigned(Name) then
    AddPort(Result)
  else
    NewList(Addr(Result^.mp_MsgList));
end;

procedure DeletePort(Port: PMsgPort);
begin
  if Port <> nil then
  begin
    if Port^.mp_Node.ln_Name <> nil then
      RemPort(Port);
    port^.mp_Node.ln_Type := $FF;
    port^.mp_MsgList.lh_Head := PNode(-1);
    FreeSignal(Port^.mp_SigBit);
    ExecFreeMem(Port, SizeOf(TMsgPort));
  end;
end;

function Create_Timer(TheUnit: LongInt): PTimeRequest;
var
  TimerPort: PMsgPort;
begin
  Result := nil;
  TimerPort := CreatePort(nil, 0);
  if TimerPort = nil then
    Exit;
  Result := PTimeRequest(CreateExtIO(TimerPort, SizeOf(TTimeRequest)));
  if Result = Nil then
  begin
    DeletePort(TimerPort);
    Exit;
  end;
  if OpenDevice(TIMERNAME, TheUnit, PIORequest(Result), 0) <> 0 then
  begin
    DeleteExtIO(pIORequest(Result));
    DeletePort(TimerPort);
    Result := nil;
  end;
end;

Procedure Delete_Timer(WhichTimer: PTimeRequest);
var
  WhichPort: PMsgPort;
begin
  WhichPort := WhichTimer^.tr_Node.io_Message.mn_ReplyPort;
  if assigned(WhichTimer) then
  begin
    CloseDevice(PIORequest(WhichTimer));
    DeleteExtIO(PIORequest(WhichTimer));
  end;
  if Assigned(WhichPort) then
    DeletePort(WhichPort);
end;

function get_sys_time(tv: PTimeVal): LongInt;
begin
  Result := -1;
  if not Assigned(Tr) then
    Tr := Create_Timer(UNIT_MICROHZ);
  // non zero return says error
  if tr = nil then
    Exit;
  tr^.tr_node.io_Command := TR_GETSYSTIME;
  DoIO(PIORequest(tr));
  // structure assignment
  tv^ := tr^.tr_time;
  Result := 0;
end;

function GetLCLTime: Int64;
var
  TV: TTimeVal;
begin
  Get_Sys_Time(@TV);
  Result := Int64(TV.TV_Secs) * 1000 + TV.TV_Micro div 1000;
end;
// End of LCLs own GetMsCount
//**************************************

{$ifdef MorphOS}
function DoMethodA(obj : pObject_; msg1 : Pointer): longword;
begin
  Result := Amigalib.DoMethodA(LongWord(Obj), Msg1);
end;
{$endif}

{$undef SetHook}

{$ifdef CPU68}
{$define SetHook}
procedure SetHook(var Hook: THook; Func: THookFunc; Data: Pointer);
begin
  {$if defined(VER3_0)}
  Hook.h_Entry := @HookEntry; { is defined in AmigaLib unit now }
  {$else}
  Hook.h_Entry := @HookEntryPas; { is defined in AmigaLib unit now }
  {$endif}
  Hook.h_SubEntry := Func;
  Hook.h_Data := Data;
end;
{$endif}

{$if defined(CPU86) or defined(CPUARM) or defined(CPU64)}
{$define SetHook}
procedure HookEntry(h: PHook; obj: PObject_; Msg: Pointer); cdecl;
var
  Proc: THookFunc;
begin
  Proc := THookFunc(h^.h_SubEntry);
  Proc(h, obj, msg);
end;

procedure SetHook(var Hook: THook; Func: THookFunc; Data: Pointer);
begin
  Hook.h_Entry := IPTR(@HookEntry);
  Hook.h_SubEntry := IPTR(Func);
  Hook.h_Data := Data;
end;
{$endif}

{$ifdef CPUPOWERPC}
{$ifdef MorphOS}
{$define SetHook}
procedure SetHook(var Hook: THook; Func: THookFunc; Data: Pointer);
{ This is MorphOS magic. Basically, CallHookPkt is designed to enter 68k code
  (remember, MorphOS is 68k AmigaOS binary compatible!) so this TRAP just
  redirects that call back to native PPC code. HookEntry is defined in
  AmigaLib unit }
const
  HOOKENTRY_TRAP: TEmulLibEntry = ( Trap: TRAP_LIB; Extension: 0; Func: @HookEntry );
begin
  Hook.h_Entry := @HOOKENTRY_TRAP;
  Hook.h_SubEntry := Func;
  Hook.h_Data := Data;
end;
{$endif}
{$ifdef AMIGAOS4}
{$define SetHook}
procedure SetHook(var Hook: THook; Func: THookFunc; Data: Pointer);
begin
  Hook.h_Entry := Func;
  Hook.h_SubEntry := Func;
  Hook.h_Data := Data;
end;
{$endif}
{$endif}

{$ifndef SetHook}
{$FATAL "SetHook not implemented for this platform"}
{$endif}

procedure ConnectHookFunction(MUIField: PtrUInt; TriggerValue: PtrUInt; Obj: PObject_; Data: TObject; Hook: PHook; HookFunc: THookFunc);
var
  Para: TAParamList;
begin
  SetHook(Hook^, HookFunc, Data);

  Para.SetParams([
    MUIM_Notify, MUIField, TriggerValue, MUIV_Notify_Self,
    2,
    MUIM_CallHook, NativeUInt(Hook),
    0]);
  //
  DoMethodA(Obj, Para);
end;

function CallHook(h: PHook; obj: APTR; params: array of NativeUInt): LongWord;
begin
  Result := CallHookPkt(h, obj, @Params[0]);
end;

function CreateRastPortA: PRastPort;
begin
  {$if (not defined(AROS)) or defined(CPU64)}
  Result := System.AllocMem(SizeOf(TRastPort));
  InitRastPort(Result);
  {$else}
  Result := CreateRastPort;
  {$endif}
end;

function CloneRastPortA(Rp: PRastPort): PRastPort;
begin
  {$if (not defined(AROS)) or defined(CPU64)}
  Result := System.AllocMem(SizeOf(TRastPort));
  Move(Rp^, Result^, SizeOf(TRastPort));
  {$else}
  Result := CloneRastPort(Rp);
  {$endif}
end;

procedure FreeRastPortA(Rp: PRastPort);
begin
  {$if (not defined(AROS)) or defined(CPU64)}
  FreeMem(Rp);
  {$else}
  FreeRastPort(Rp);
  {$endif}
end;

{$ifdef FPC4AROS_VER3_FIXES}
function DoMethod(Obj: PObject_; const Args: array of PtrUInt): IPTR; inline;
begin
  DoMethod := 0;
  if obj = nil then
    Exit;
  DoMethod := CALLHOOKPKT_(PHook(OCLASS(Obj)), Obj, @Args);
end;
{$endif}

{$ifdef Amiga}

function DoMethodA(obj : pObject_; msg : APTR): ulong;
begin
  if assigned(obj) then
  begin
    DoMethodA := CallHookPkt(@THook(OCLASS(obj)^.cl_Dispatcher), obj, msg);
  end
  else
    DoMethodA := 0;
end;

function DoMethod(obj: Pointer; params: array of DWord): LongWord;
begin
  Result := DoMethodA(obj, @params);
end;

function DoMethod(obj: LongWord; params: array of DWord): LongWord;
begin
  Result := DoMethodA(Pointer(obj), @params);
end;
{$endif}

initialization
  if not Assigned(Tr) then
    Tr := create_timer(UNIT_MICROHZ);
{$ifdef Amiga68k}
  IntuitionBase := _IntuitionBase;
{$endif}

finalization
  if Assigned(Tr) then
    Delete_timer(tr);
end.
