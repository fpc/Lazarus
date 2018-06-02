procedure DebuglnStack(const s: string = '');

procedure DbgOut(const s: string = ''); inline; overload;
procedure DbgOut(Args: array of const); {inline;} overload;
procedure DbgOut(const S: String; Args: array of const); {inline;} overload;// similar to Format(s,Args)
procedure DbgOut(const s1, s2: string; const s3: string = '';
                 const s4: string = ''; const s5: string = ''; const s6: string = '';
                 const s7: string = ''; const s8: string = ''; const s9: string = '';
                 const s10: string = ''; const s11: string = ''; const s12: string = '';
                 const s13: string = ''; const s14: string = ''; const s15: string = '';
                 const s16: string = ''; const s17: string = ''; const s18: string = ''); inline; overload;

procedure DebugLn(const s: string = ''); inline; overload;
procedure DebugLn(Args: array of const); {inline;} overload;
procedure DebugLn(const S: String; Args: array of const); {inline;} overload;// similar to Format(s,Args)
procedure DebugLn(const s1, s2: string; const s3: string = '';
                  const s4: string = ''; const s5: string = ''; const s6: string = '';
                  const s7: string = ''; const s8: string = ''; const s9: string = '';
                  const s10: string = ''; const s11: string = ''; const s12: string = '';
                  const s13: string = ''; const s14: string = ''; const s15: string = '';
                  const s16: string = ''; const s17: string = ''; const s18: string = ''); inline; overload;

procedure DebugLnEnter(const s: string = ''); inline; overload;
procedure DebugLnEnter(Args: array of const); {inline;} overload;
procedure DebugLnEnter(s: string; Args: array of const); {inline;} overload;
procedure DebugLnEnter(const s1, s2: string; const s3: string = '';
                       const s4: string = ''; const s5: string = ''; const s6: string = '';
                       const s7: string = ''; const s8: string = ''; const s9: string = '';
                       const s10: string = ''; const s11: string = ''; const s12: string = '';
                       const s13: string = ''; const s14: string = ''; const s15: string = '';
                       const s16: string = ''; const s17: string = ''; const s18: string = ''); inline; overload;

procedure DebugLnExit(const s: string = ''); inline; overload;
procedure DebugLnExit(Args: array of const); {inline;} overload;
procedure DebugLnExit(s: string; Args: array of const); {inline;} overload;
procedure DebugLnExit (const s1, s2: string; const s3: string = '';
                       const s4: string = ''; const s5: string = ''; const s6: string = '';
                       const s7: string = ''; const s8: string = ''; const s9: string = '';
                       const s10: string = ''; const s11: string = ''; const s12: string = '';
                       const s13: string = ''; const s14: string = ''; const s15: string = '';
                       const s16: string = ''; const s17: string = ''; const s18: string = ''); inline; overload;


procedure DebuglnStack(LogGroup: PLazLoggerLogGroup; const s: string = '');

procedure DbgOut(LogGroup: PLazLoggerLogGroup; const s: string = ''); overload;
procedure DbgOut(LogGroup: PLazLoggerLogGroup; Args: array of const); overload;
procedure DbgOut(LogGroup: PLazLoggerLogGroup; const S: String; Args: array of const); overload;// similar to Format(s,Args)
procedure DbgOut(LogGroup: PLazLoggerLogGroup; const s1, s2: string; const s3: string = '';
                 const s4: string = ''; const s5: string = ''; const s6: string = '';
                 const s7: string = ''; const s8: string = ''; const s9: string = '';
                 const s10: string = ''; const s11: string = ''; const s12: string = '';
                 const s13: string = ''; const s14: string = ''; const s15: string = '';
                 const s16: string = ''; const s17: string = ''; const s18: string = ''); overload;

procedure DebugLn(LogGroup: PLazLoggerLogGroup; const s: string = ''); overload;
procedure DebugLn(LogGroup: PLazLoggerLogGroup; Args: array of const); overload;
procedure DebugLn(LogGroup: PLazLoggerLogGroup; const S: String; Args: array of const); overload;// similar to Format(s,Args)
procedure DebugLn(LogGroup: PLazLoggerLogGroup; const s1, s2: string; const s3: string = '';
                  const s4: string = ''; const s5: string = ''; const s6: string = '';
                  const s7: string = ''; const s8: string = ''; const s9: string = '';
                  const s10: string = ''; const s11: string = ''; const s12: string = '';
                  const s13: string = ''; const s14: string = ''; const s15: string = '';
                  const s16: string = ''; const s17: string = ''; const s18: string = ''); overload;

procedure DebugLnEnter(LogGroup: PLazLoggerLogGroup; const s: string = ''); overload;
procedure DebugLnEnter(LogGroup: PLazLoggerLogGroup; Args: array of const); overload;
procedure DebugLnEnter(LogGroup: PLazLoggerLogGroup; s: string; Args: array of const); overload;
procedure DebugLnEnter(LogGroup: PLazLoggerLogGroup; const s1, s2: string; const s3: string = '';
                       const s4: string = ''; const s5: string = ''; const s6: string = '';
                       const s7: string = ''; const s8: string = ''; const s9: string = '';
                       const s10: string = ''; const s11: string = ''; const s12: string = '';
                       const s13: string = ''; const s14: string = ''; const s15: string = '';
                       const s16: string = ''; const s17: string = ''; const s18: string = ''); overload;

procedure DebugLnExit(LogGroup: PLazLoggerLogGroup; const s: string = ''); overload;
procedure DebugLnExit(LogGroup: PLazLoggerLogGroup; Args: array of const); overload;
procedure DebugLnExit(LogGroup: PLazLoggerLogGroup; s: string; Args: array of const); overload;
procedure DebugLnExit(LogGroup: PLazLoggerLogGroup; const s1, s2: string; const s3: string = '';
                      const s4: string = ''; const s5: string = ''; const s6: string = '';
                      const s7: string = ''; const s8: string = ''; const s9: string = '';
                      const s10: string = ''; const s11: string = ''; const s12: string = '';
                      const s13: string = ''; const s14: string = ''; const s15: string = '';
                      const s16: string = ''; const s17: string = ''; const s18: string = ''); overload;


function DbgS(const c: cardinal): string; overload;
function DbgS(const i: longint): string; overload;
function DbgS(const i: int64): string; overload;
function DbgS(const q: qword): string; overload;
function DbgS(const r: TRect): string; overload;
function DbgS(const p: TPoint): string; overload;
function DbgS(const p: pointer): string; overload;
function DbgS(const e: extended; MaxDecimals: integer = 999): string; overload;
function DbgS(const b: boolean): string; overload;
function DbgS(const m: TMethod): string; overload;
function DbgS(const ASize: TSize): string; overload;
function DbgS(const s: TComponentState): string; overload;
function DbgS(const i1,i2,i3,i4: integer): string; overload;
function DbgS(const Shift: TShiftStateEnum): string; overload;
function DbgS(const Shift: TShiftState): string; overload;

function DbgSJoin(const s1, s2: string): string;
function DbgSName(const p: TObject): string; overload;
function DbgSName(const p: TClass): string; overload;
function dbgObjMem(AnObject: TObject): string; overload;
function dbghex(i: Int64): string; overload;

function dbgMemRange(P: Pointer; Count: integer; Width: integer = 0): string; overload;
function dbgMemStream(MemStream: TCustomMemoryStream; Count: integer): string; overload;

procedure DumpExceptionBackTrace;
