unit TestParser;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testutils, testregistry,
  SimpleIDE, TextTools, leakinfo, LazLogger;

type

  TExpLine = record
    FileName  : string;   // should be empty if file is unknown
    LineNum   : Integer;  // -1 is line is unknown
    Column    : Integer;  // 0 is unknown
    Addr      : Int64;    // -1 if address is unknown
    RawLineData: string;
  end;

  { TTestLeakParser }

  TTestLeakParser = class(TTestCase)
  private
    FLineEnd: String;
    procedure CheckTraceLine(AName: String; ExpLine: TExpLine; ATraceLine: TStackLine);
    procedure CheckTrace(AName: String; ExpLines: Array of TExpLine; ATrace: TStackLines);

    function  TextLeak1(WithHeader: Boolean = True): String;
    procedure CheckLeak1(TrcList: TStackTraceList);
    function  TextGdb1: String;
    procedure CheckGdb1(TrcList: TStackTraceList; ReplHash: String = '#');
    function  TextGdb2: String;
    procedure CheckGdb2(TrcList: TStackTraceList; ReplHash: String = '#');
    function  TextLldb1: String;
    function  TextValgrind: String;
  published
    procedure TestLeakText1;
    procedure TestLeakText1NoHeader;
    procedure TestLeakNoTraceHeader;
    procedure TestLeakLeadSpaces;
    procedure TestGdb1;
    procedure TestGdb1Mantis;
    procedure TestGdb2;
    procedure TestGdb2Mantis;
    procedure TestLldb;
    procedure TestValgrind;
  end;

implementation

function el(FileName: string; LineNum: Integer = -2;
  Addr: Int64 = -2;
  Column: Integer = -2;
  RawLineData: string = #1
): TExpLine;
begin
  Result.FileName    := FileName;
  Result.LineNum     := LineNum;
  Result.Column      := Column;
  Result.Addr        := Addr;
  Result.RawLineData := RawLineData;
end;

function el(FileName: string; LineNum: Integer;
  Addr: Int64;
  RawLineData: string
): TExpLine;
begin
  Result := el(FileName, LineNum, Addr, -2, RawLineData);
end;

function el(Addr: Int64;
  RawLineData: string = #1
): TExpLine;
begin
  Result := el(#1, -2, Addr, -2, RawLineData);
end;

procedure TTestLeakParser.CheckTraceLine(AName: String; ExpLine: TExpLine;
  ATraceLine: TStackLine);
begin
  DebugLn('Comparing (%s)  FN: %s=%s  Ln: %d=%d  CN: %d=%d  @: %d=%d  // %s',
    [AName, ExpLine.FileName, ATraceLine.FileName, ExpLine.LineNum, ATraceLine.LineNum, ExpLine.Column, ATraceLine.Column, ExpLine.Addr, ATraceLine.Addr, ATraceLine.RawLineData]);
  if ExpLine.FileName   <> #1 then AssertEquals(AName + ' FileName',    ExpLine.FileName,    ATraceLine.FileName);
  if ExpLine.LineNum    <> -2 then AssertEquals(AName + ' LineNum',     ExpLine.LineNum,     ATraceLine.LineNum);
  if ExpLine.Column     <> -2 then AssertEquals(AName + ' Column',      ExpLine.Column,      ATraceLine.Column);
// TODO: Valgrind
//  if ExpLine.Addr       <> -2 then AssertEquals(AName + ' Addr',        ExpLine.Addr,        ATraceLine.Addr);
  if ExpLine.RawLineData<> #1 then AssertEquals(AName + ' RawLineData', ExpLine.RawLineData, ATraceLine.RawLineData);
end;

procedure TTestLeakParser.CheckTrace(AName: String;
  ExpLines: array of TExpLine; ATrace: TStackLines);
var
  i: Integer;
  t: String;
begin
  AssertEquals(AName+' Cnt', Length(ExpLines), ATrace.Count);
  t := '';
  for i := 0 to Length(ExpLines) - 1 do begin
    try
      CheckTraceLine(AName+ ' Line:'+IntToStr(i), ExpLines[i], ATrace.Lines[i]);
    Except
      on E: Exception do
        t := t + E.Message + LineEnding;
    end;
  end;
  DebugLn(t<>'', ['********** ERR: ', t]);
  AssertEquals('',t);
end;

function TTestLeakParser.TextLeak1(WithHeader: Boolean): String;
begin
  result := '';
  if WithHeader then
    result := result +
'C:\Users\Jeff\Documents\NVExitMon\ExternalMonitor.exe -DB path name' + FLineEnd +
'Heap dump by heaptrc unit' + FLineEnd +
'56107 memory blocks allocated : 6527758/6720728' + FLineEnd +
'56065 memory blocks freed     : 6504089/6697040' + FLineEnd +
'42 unfreed memory blocks : 23669' + FLineEnd +
'True heap size : 1212416' + FLineEnd +
'True free heap : 1185264' + FLineEnd +
'Should be : 1186040' + FLineEnd;
    result := result +
'Call trace for block $083909D8 size 260' + FLineEnd +
'  $0043F1C9 line 227 of dbcommonunit.pas' + FLineEnd +
'  $00675FB3 line 598 of colorunit.pas' + FLineEnd +
'  $0043DA37 line 2775 of externalmonmain.pas' + FLineEnd +
'  $0042A208' + FLineEnd +
'  $00429811' + FLineEnd +
'  $0040DF58' + FLineEnd +
'  $0050A3F1' + FLineEnd +
'  $0042B07D' + FLineEnd +
'Call trace for block $083D0570 size 656' + FLineEnd +
'  $006740DE line 285 of colorunit.pas' + FLineEnd +
'  $0066F1F7 line 209 of colorunit.pas' + FLineEnd +
'  $0043DA09 line 2772 of externalmonmain.pas' + FLineEnd +
'  $0042A208' + FLineEnd +
'  $00429811' + FLineEnd +
'  $0040DF58' + FLineEnd +
'  $0050A3F1' + FLineEnd +
'  $0042B07D' + FLineEnd +
'Call trace for block $063E91C0 size 456' + FLineEnd +
'  $0043DA09 line 2772 of externalmonmain.pas' + FLineEnd +
'  $0042A208' + FLineEnd +
'  $00429811' + FLineEnd +
'  $0040DF58' + FLineEnd +
'Call trace for block $063E91C2 size 456' + FLineEnd +
'  $0042A208' + FLineEnd +
'  $00429811' + FLineEnd +
'  $0040DF58' + FLineEnd +
'Call trace for block $083E8CF0 size 30' + FLineEnd +
'  $0045AD8F' + FLineEnd +
'  $004432D3 line 66 of monitorunit.pas' + FLineEnd +
'  $0043D944 line 2745 of externalmonmain.pas' + FLineEnd +
'  $0042A208' + FLineEnd +
'  $00429811' + FLineEnd +
'  $0040DF58' + FLineEnd +
'  $0050A3F1' + FLineEnd +
'  $0042B07D' + FLineEnd +
'Call trace for block $083A8C40 size 656' + FLineEnd +
'  $006740DE line 285 of colorunit.pas' + FLineEnd +
'  $0066F1F7 line 209 of colorunit.pas' + FLineEnd +
'  $0066A7F1 line 249 of spectrometerunit.pas' + FLineEnd +
'  $0066A66A line 216 of spectrometerunit.pas' + FLineEnd +
'  $0043725A line 641 of externalmonmain.pas' + FLineEnd +
'  $0043792F line 785 of externalmonmain.pas' + FLineEnd +
'  $0043D93F line 2743 of externalmonmain.pas' + FLineEnd +
'Call trace for block $019D3738 size 656' + FLineEnd +
'  $0066F347 line 228 of colorunit.pas' + FLineEnd +
'  $0066F1F7 line 209 of colorunit.pas' + FLineEnd +
'  $0042E79F' + FLineEnd +
'  $004348B2' + FLineEnd +
'  $00402E76 line 42 of ExternalMonitor.lpr' + FLineEnd +
'Call trace for block $063E8B90 size 456' + FLineEnd +
'  $004467CC line 107 of colorsetupunit.pas' + FLineEnd +
'  $00429F8D' + FLineEnd +
'  $00428557' + FLineEnd +
'  $0042E79F' + FLineEnd +
'  $004348B2' + FLineEnd +
'  $00402E76 line 42 of ExternalMonitor.lpr' + FLineEnd +
'  $0058EA88' + FLineEnd +
'  $005A0F8F' + FLineEnd
end;

procedure TTestLeakParser.CheckLeak1(TrcList: TStackTraceList);
begin
  CheckTrace('T0', [
    el('dbcommonunit.pas',     227, $0043F1C9, '  $0043F1C9 line 227 of dbcommonunit.pas'),
    el('colorunit.pas',        598, $00675FB3, '  $00675FB3 line 598 of colorunit.pas'),
    el('externalmonmain.pas', 2775, $0043DA37, '  $0043DA37 line 2775 of externalmonmain.pas'),
    el('', -1, $0042A208, '  $0042A208'),
    el('', -1, $00429811, '  $00429811'),
    el('', -1, $0040DF58, '  $0040DF58'),
    el('', -1, $0050A3F1, '  $0050A3F1'),
    el('', -1, $0042B07D, '  $0042B07D')
    ], TrcList[0]);

  CheckTrace('T1', [
    el('colorunit.pas', 285, $006740DE, '  $006740DE line 285 of colorunit.pas'),
    el('colorunit.pas', 209, $0066F1F7, '  $0066F1F7 line 209 of colorunit.pas'),
    el('externalmonmain.pas', 2772, $0043DA09, '  $0043DA09 line 2772 of externalmonmain.pas'),
    el('', -1, $0042A208, '  $0042A208'),
    el('', -1, $00429811, '  $00429811'),
    el('', -1, $0040DF58, '  $0040DF58'),
    el('', -1, $0050A3F1, '  $0050A3F1'),
    el('', -1, $0042B07D, '  $0042B07D')
    ], TrcList[1]);

  CheckTrace('T2', [
    el('externalmonmain.pas', 2772, $0043DA09, '  $0043DA09 line 2772 of externalmonmain.pas'),
    el('', -1, $0042A208, '  $0042A208'),
    el('', -1, $00429811, '  $00429811'),
    el('', -1, $0040DF58, '  $0040DF58')
    ], TrcList[2]);

  CheckTrace('T3', [
    el('', -1, $0042A208, '  $0042A208'),
    el('', -1, $00429811, '  $00429811'),
    el('', -1, $0040DF58, '  $0040DF58')
    ], TrcList[3]);

  CheckTrace('T4', [
    el('', -1, $0045AD8F, '  $0045AD8F'),
    el('monitorunit.pas', 66, $004432D3, '  $004432D3 line 66 of monitorunit.pas'),
    el('externalmonmain.pas', 2745, $0043D944, '  $0043D944 line 2745 of externalmonmain.pas'),
    el('', -1, $0042A208, '  $0042A208'),
    el('', -1, $00429811, '  $00429811'),
    el('', -1, $0040DF58, '  $0040DF58'),
    el('', -1, $0050A3F1, '  $0050A3F1'),
    el('', -1, $0042B07D, '  $0042B07D')
    ], TrcList[4]);

  CheckTrace('T5', [
    el('colorunit.pas', 285, $006740DE, '  $006740DE line 285 of colorunit.pas'),
    el('colorunit.pas', 209, $0066F1F7, '  $0066F1F7 line 209 of colorunit.pas'),
    el('spectrometerunit.pas', 249, $0066A7F1, '  $0066A7F1 line 249 of spectrometerunit.pas'),
    el('spectrometerunit.pas', 216, $0066A66A, '  $0066A66A line 216 of spectrometerunit.pas'),
    el('externalmonmain.pas', 641, $0043725A, '  $0043725A line 641 of externalmonmain.pas'),
    el('externalmonmain.pas', 785, $0043792F, '  $0043792F line 785 of externalmonmain.pas'),
    el('externalmonmain.pas', 2743, $0043D93F, '  $0043D93F line 2743 of externalmonmain.pas')
    ], TrcList[5]);

  CheckTrace('T6', [
    el('colorunit.pas', 228, $0066F347, '  $0066F347 line 228 of colorunit.pas'),
    el('colorunit.pas', 209, $0066F1F7, '  $0066F1F7 line 209 of colorunit.pas'),
    el('', -1, $0042E79F, '  $0042E79F'),
    el('', -1, $004348B2, '  $004348B2'),
    el('ExternalMonitor.lpr', 42, $00402E76, '  $00402E76 line 42 of ExternalMonitor.lpr')
    ], TrcList[6]);

  CheckTrace('T7', [
    el('colorsetupunit.pas', 107, $004467CC, '  $004467CC line 107 of colorsetupunit.pas'),
    el('', -1, $00429F8D, '  $00429F8D'),
    el('', -1, $00428557, '  $00428557'),
    el('', -1, $0042E79F, '  $0042E79F'),
    el('', -1, $004348B2, '  $004348B2'),
    el('ExternalMonitor.lpr', 42, $00402E76, '  $00402E76 line 42 of ExternalMonitor.lpr'),
    el('', -1, $0058EA88, '  $0058EA88'),
    el('', -1, $005A0F8F, '  $005A0F8F')
    ], TrcList[7]);
end;

function TTestLeakParser.TextGdb1: String;
begin
  Result :=
'#0  GETCARETX (this=0x229cb9e0) at synedit.pp:4501' + FLineEnd +
'#1  0x00783cd2 in CARETXPIX (this=0x229cb9e0) at synedit.pp:2642' + FLineEnd +
'#2  0x0078b15e in UPDATECARET (this=0x229cb9e0, IGNOREPAINTLOCK=false)' + FLineEnd +
'    at synedit.pp:4839' + FLineEnd +
'#3  0x0078346d in DODECPAINTLOCK (this=0x229cb9e0, SENDER=0x229cb9e0)' + FLineEnd +
'    at synedit.pp:2491' + FLineEnd +
'#4  0x0045e23d in CALLNOTIFYEVENTS (this=0x221ebf58, SENDER=0x229cb9e0)' + FLineEnd +
'    at lazmethodlist.pas:315' + FLineEnd +
'#5  0x00b9009d in SENDNOTIFICATION (this=0x1b820060,' + FLineEnd +
'    AREASON=SENRDECPAINTLOCK, ASENDER=0x229cb9e0) at synedittextbuffer.pp:1742' + FLineEnd +
'#6  0x00b8e2c2 in SETUPDATESTATE (this=0x1b820060, UPDATING=false,' + FLineEnd +
'    SENDER=0x229cb9e0) at synedittextbuffer.pp:1445' + FLineEnd +
'#7  0x00b87e69 in ENDUPDATE (this=0x1b820060, SENDER=0x229cb9e0)' + FLineEnd +
'    at lazsynedittext.pas:978' + FLineEnd +
'#8  0x00782e27 in DECPAINTLOCK (this=0x229cb9e0) at synedit.pp:2387' + FLineEnd +
'#9  0x12597e58 in ?? ()' + FLineEnd +                                          // unknown
'#10 0x76cb0419 in KERNEL32!BaseThreadInitThunk ()' + FLineEnd +                // kernel
'   from C:\WINDOWS\System32\kernel32.dll' + FLineEnd +
'#11  0x77a9662d in ntdll!RtlGetAppContainerNamedObjectPath ()' + FLineEnd +
'   from C:\WINDOWS\SYSTEM32\ntdll.dll' + FLineEnd +
'#12 0x0079458f in ENDUPDATE (this=0x229cb9e0) at synedit.pp:7359' + FLineEnd +
'#13 0x00942274 in SETPAGEINDEX (this=0x26ba1cc0, AVALUE=0)' + FLineEnd +
'    at sourceeditor.pp:7102' + FLineEnd +
'#14 0x00943458 in NEWSE (this=0x26ba1cc0, PAGENUM=0, NEWPAGENUM=-1,' + FLineEnd +
'    ASHAREDEDITOR=0x0, ATABCAPTION=0x221f105c ''unit1'') at sourceeditor.pp:7403' + FLineEnd +
'#15 0x009462ce in NEWFILE (this=0x26ba1cc0, NEWSHORTNAME=0x221f3edc ''unit1'',' + FLineEnd +
'    ASOURCE=0x252b2f50, FOCUSIT=true, ASHAREEDITOR=0x0)' + FLineEnd +
'    at sourceeditor.pp:8208' + FLineEnd +
'#16 0x00ab05ce in NEWFILE (NEWFILEDESCRIPTOR=0x25210920,' + FLineEnd +
'    NEWFILENAME=0x2240e874 ''unit1.pas'', NEWSOURCE=0x0, NEWFLAGS=...,' + FLineEnd +
'    NEWOWNER=0x0) at sourcefilemanager.pas:2154' + FLineEnd +
'#17 0x004ade8d in DONEWFILE (this=0x126405d0, NEWFILEDESCRIPTOR=0x25210920,' + FLineEnd +
'    NEWFILENAME=0x2240e874 ''unit1.pas'', NEWSOURCE=0x0, NEWFLAGS=...,' + FLineEnd +
'    NEWOWNER=0x0) at main.pp:5854' + FLineEnd +
'#18 0x007ea7d7 in DONEWEDITORFILE (this=0x126405d0,' + FLineEnd +
'    NEWFILEDESCRIPTOR=0x25210920, NEWFILENAME=0x2240e874 ''unit1.pas'',' + FLineEnd +
'    NEWSOURCE=0x0, NEWFLAGS=...) at lazideintf.pas:759' + FLineEnd +
'#19 0x008466e1 in CREATESTARTFILES (this=0x22344618, APROJECT=0x26308f30)' + FLineEnd +
'    at projectdescriptors.pas:171' + FLineEnd +
'#20 0x00ab5bb6 in INITNEWPROJECT (PROJECTDESC=0x22344618)' + FLineEnd +
'    at sourcefilemanager.pas:3635' + FLineEnd +
'#21 0x004b00c4 in DONEWPROJECT (this=0x126405d0, PROJECTDESC=0x22344618)' + FLineEnd +
'    at main.pp:6502' + FLineEnd +
'#22 0x0049c522 in SETUPSTARTPROJECT (this=0x126405d0) at main.pp:2391' + FLineEnd +
'#23 0x004376b1 in SYSUTILS_$$_FORMAT$ANSISTRING$array_of_const$$ANSISTRING ()' + FLineEnd + // $$ format
'#24 0x0040395a in main () at lazarus.pp:150' + FLineEnd
  ;
end;

procedure TTestLeakParser.CheckGdb1(TrcList: TStackTraceList; ReplHash: String);
begin
  CheckTrace('T0', [
    el('synedit.pp'            , 4501,  0,        ReplHash+'0  GETCARETX (this=0x229cb9e0) at synedit.pp:4501'),
    el('synedit.pp'            , 2642, $00783cd2, ReplHash+'1  0x00783cd2 in CARETXPIX (this=0x229cb9e0) at synedit.pp:2642'),
    el('synedit.pp'            , 4839, $0078b15e, ReplHash+'2  0x0078b15e in UPDATECARET (this=0x229cb9e0, IGNOREPAINTLOCK=false)'+'    at synedit.pp:4839'),
el(#1),
    el('synedit.pp'            , 2491, $0078346d, ReplHash+'3  0x0078346d in DODECPAINTLOCK (this=0x229cb9e0, SENDER=0x229cb9e0)'+'    at synedit.pp:2491'),
el(#1),
    el('lazmethodlist.pas'     ,  315, $0045e23d, ReplHash+'4  0x0045e23d in CALLNOTIFYEVENTS (this=0x221ebf58, SENDER=0x229cb9e0)'+'    at lazmethodlist.pas:315'),
el(#1),
    el('synedittextbuffer.pp'  , 1742, $00b9009d, ReplHash+'5  0x00b9009d in SENDNOTIFICATION (this=0x1b820060,'+'    AREASON=SENRDECPAINTLOCK, ASENDER=0x229cb9e0) at synedittextbuffer.pp:1742'),
el(#1),
    el('synedittextbuffer.pp'  , 1445, $00b8e2c2, ReplHash+'6  0x00b8e2c2 in SETUPDATESTATE (this=0x1b820060, UPDATING=false,'+'    SENDER=0x229cb9e0) at synedittextbuffer.pp:1445'),
el(#1),
    el('lazsynedittext.pas'    ,  978, $00b87e69, ReplHash+'7  0x00b87e69 in ENDUPDATE (this=0x1b820060, SENDER=0x229cb9e0)'+'    at lazsynedittext.pas:978'),
el(#1),
    el('synedit.pp'            , 2387, $00782e27, ReplHash+'8  0x00782e27 in DECPAINTLOCK (this=0x229cb9e0) at synedit.pp:2387'),
    el(''                      ,   -1, $12597e58, ReplHash+'9  0x12597e58 in ?? ()'),
    el(''                      ,   -1, $76cb0419, ReplHash+'10 0x76cb0419 in KERNEL32!BaseThreadInitThunk ()'+'   from C:\WINDOWS\System32\kernel32.dll'),
el(#1),
    el(''                      ,   -1, $77a9662d, ReplHash+'11  0x77a9662d in ntdll!RtlGetAppContainerNamedObjectPath ()'+'   from C:\WINDOWS\SYSTEM32\ntdll.dll'),
el(#1),
    el('synedit.pp'            , 7359, $0079458f, ReplHash+'12 0x0079458f in ENDUPDATE (this=0x229cb9e0) at synedit.pp:7359'),
    el('sourceeditor.pp'       , 7102, $00942274, ReplHash+'13 0x00942274 in SETPAGEINDEX (this=0x26ba1cc0, AVALUE=0)'+'    at sourceeditor.pp:7102'),
el(#1),
    el('sourceeditor.pp'       , 7403, $00943458, ReplHash+'14 0x00943458 in NEWSE (this=0x26ba1cc0, PAGENUM=0, NEWPAGENUM=-1,'+'    ASHAREDEDITOR=0x0, ATABCAPTION=0x221f105c ''unit1'') at sourceeditor.pp:7403'),
el(#1),
//    el('sourceeditor.pp'       , 8208, $009462ce, ReplHash+'15 0x009462ce in NEWFILE (this=0x26ba1cc0, NEWSHORTNAME=0x221f3edc ''unit1'','+'    ASOURCE=0x252b2f50, FOCUSIT=true, ASHAREEDITOR=0x0)'+'    at sourceeditor.pp:8208'),
el(#1),
el(#1),
el(#1),
//    el('sourcefilemanager.pas' , 2154, $00ab05ce, ReplHash+'16 0x00ab05ce in NEWFILE (NEWFILEDESCRIPTOR=0x25210920,'+'    NEWFILENAME=0x2240e874 ''unit1.pas'', NEWSOURCE=0x0, NEWFLAGS=...,'+'    NEWOWNER=0x0) at sourcefilemanager.pas:2154'),
el(#1),
el(#1),
el(#1),
//    el('main.pp'               , 5854, $004ade8d, ReplHash+'17 0x004ade8d in DONEWFILE (this=0x126405d0, NEWFILEDESCRIPTOR=0x25210920,'+'    NEWFILENAME=0x2240e874 ''unit1.pas'', NEWSOURCE=0x0, NEWFLAGS=...,'+'    NEWOWNER=0x0) at main.pp:5854'),
el(#1),
el(#1),
el(#1),
//    el('lazideintf.pas'        ,  759, $007ea7d7, ReplHash+'18 0x007ea7d7 in DONEWEDITORFILE (this=0x126405d0,'+'    NEWFILEDESCRIPTOR=0x25210920, NEWFILENAME=0x2240e874 ''unit1.pas'','+'    NEWSOURCE=0x0, NEWFLAGS=...) at lazideintf.pas:759'),
el(#1),
el(#1),
el(#1),
    el('projectdescriptors.pas',  171, $008466e1, ReplHash+'19 0x008466e1 in CREATESTARTFILES (this=0x22344618, APROJECT=0x26308f30)'+'    at projectdescriptors.pas:171'),
el(#1),
    el('sourcefilemanager.pas' , 3635, $00ab5bb6, ReplHash+'20 0x00ab5bb6 in INITNEWPROJECT (PROJECTDESC=0x22344618)'+'    at sourcefilemanager.pas:3635'),
el(#1),
    el('main.pp'               , 6502, $004b00c4, ReplHash+'21 0x004b00c4 in DONEWPROJECT (this=0x126405d0, PROJECTDESC=0x22344618)'+'    at main.pp:6502'),
el(#1),
    el('main.pp'               , 2391, $0049c522, ReplHash+'22 0x0049c522 in SETUPSTARTPROJECT (this=0x126405d0) at main.pp:2391'),
    el(''                      ,   -1, $004376b1, ReplHash+'23 0x004376b1 in SYSUTILS_$$_FORMAT$ANSISTRING$array_of_const$$ANSISTRING ()'),
    el('lazarus.pp'            ,  150, $0040395a, ReplHash+'24 0x0040395a in main () at lazarus.pp:150')
    ], TrcList[0]);
end;

function TTestLeakParser.TextGdb2: String;
begin
  Result :=
'#0 fpc_raiseexception(0x1598bd8, 0x1598bc0, 0xd7d) at ..\inc\except.inc:163' + FLineEnd +
'#1 ASSERTERRORHANDLER(''TSynCustomFoldHighlighter''..., ''synhighlighterpas.pp'', 3453, 0x1426ec68) at ..\objpas\sysutils\sysutils.inc:488' + FLineEnd +
'#2 fpc_assert(''TSynCustomFoldHighlighter''..., ''synhighlighterpas.pp'', 3453, 0x1426ec68) at ..\inc\system.inc:1565' + FLineEnd +
'#3 FOLDBLOCKENDLEVEL(0x24fa86d0, -1, {FOLDGROUP = 1, FLAGS = [SFBINCLUDEDISABLED]}) at synhighlighterpas.pp:3453' + FLineEnd +
'#4 FOLDBLOCKENDLEVEL(0x24fa86d0, -1, 1, [SFBINCLUDEDISABLED]) at synedithighlighterfoldbase.pas:1843' + FLineEnd +
'#5 INITCOUNT(0x234abcf0) at synedithighlighterfoldbase.pas:1475' + FLineEnd +
'#6 COUNT(0x234abcf0) at synedithighlighterfoldbase.pas:1674' + FLineEnd +
'#7 SRCSYNCARETCHANGED(0x1f303360, 0x0) at sourcesyneditor.pas:1469' + FLineEnd +
'#8 DOONSTATUSCHANGE(0x1f303360, [SCLINESINWINDOW]) at sourcesyneditor.pas:1555' + FLineEnd +
'#9 DODECPAINTLOCK(0x1f303360, 0x1f303360) at synedit.pp:2501' + FLineEnd +
'#10 CALLNOTIFYEVENTS(0x236ff020, 0x1f303360) at lazmethodlist.pas:315' + FLineEnd +
'#11 SENDNOTIFICATION(0x14365540, SENRDECPAINTLOCK, 0x1f303360) at synedittextbuffer.pp:1564' + FLineEnd +
'#12 SETUPDATESTATE(0x14365540, false, 0x1f303360) at synedittextbuffer.pp:1267' + FLineEnd +
'#13 ENDUPDATE(0x14365540, 0x1f303360) at lazsynedittext.pas:978' + FLineEnd +
'#14 DECPAINTLOCK(0x1f303360) at synedit.pp:2387' + FLineEnd +
'#15 SETHIGHLIGHTER(0x1f303360, 0x24ce2a20) at synedit.pp:6417' + FLineEnd +
'#16 SETHIGHLIGHTER(0x1f303360, 0x24ce2a20) at sourcesyneditor.pas:1713' + FLineEnd +
'#17 SETSYNTAXHIGHLIGHTERTYPE(0x22b8c598, LSHFREEPASCAL) at sourceeditor.pp:4890' + FLineEnd +
'#18 RENAMEUNIT(0x18d45bf0, 0x2345cb24 ''P:\programs\lazarus''..., 0x238c1b6c ''OPopupForm'', 0x0, 0x0) at sourcefilemanager.pas:5460' + FLineEnd +
'#19 SHOWSAVEFILEASDIALOG(0x236fd32c ''unit2.pas'', 0x18d45bf0, 0x0, 0x0, false) at sourcefilemanager.pas:4721' + FLineEnd +
'#20 SAVEEDITORFILE(0x22b8c598, [SFSAVEAS, SFCHECKAMBIGUOUSFILES]) at sourcefilemanager.pas:2487' + FLineEnd +
'#21 DOSAVEEDITORFILE(0x1433e058, 0x22b8c598, [SFCHECKAMBIGUOUSFILES]) at main.pp:5862' + FLineEnd +
'#22 MNUSAVECLICKED(0x1433e058, 0x1433e058) at main.pp:3341' + FLineEnd +
'#23 PROCESSIDECOMMAND(0x1433e058, 0x24fa1810, 1208, true) at main.pp:3538' + FLineEnd +
'#24 PROCESSPARENTCOMMAND(0x24fa1810, 0x22b8c598, 1208, '''', 0x0, true) at sourceeditor.pp:8833' + FLineEnd +
'#25 PROCESSUSERCOMMAND(0x22b8c598, 0x1f303360, 1208, '''', 0x0) at sourceeditor.pp:4203' + FLineEnd +
'#26 DOONPROCESSCOMMAND(0x1f303360, 1208, '''', 0x0) at synedit.pp:7261' + FLineEnd +
'#27 COMMANDPROCESSOR(0x1f303360, 1208, '''', 0x0, []) at synedit.pp:6607' + FLineEnd +
'#28 KEYDOWN(0x1f303360, 0, [SSCTRL]) at synedit.pp:3006' + FLineEnd +
'#29 KEYDOWNBEFOREINTERFACE(0x1f303360, 0, [SSCTRL]) at include\wincontrol.inc:5685' + FLineEnd +
'#30 DOKEYDOWNBEFOREINTERFACE(0x1f303360, {MSG = 48384, CHARCODE = 0, UNUSED = 0, KEYDATA = 2031617, RESULT = 0}, false) at include\wincontrol.inc:5816' + FLineEnd +
'#31 CNKEYDOWN(0x1f303360, {MSG = 48384, CHARCODE = 0, UNUSED = 0, KEYDATA = 2031617, RESULT = 0}) at include\wincontrol.inc:7205' + FLineEnd +
'#32 DISPATCH(0x1f303360, 0) at ..\inc\objpas.inc:684' + FLineEnd +
'#33 WNDPROC(0x1f303360, {MSG = 48384, WPARAM = 0, LPARAM = 2031617, RESULT = 0, WPARAMLO = 0, WPARAMHI = 0, WPARAMFILLER = {}, LPARAMLO = 1, LPARAMHI = 31, LPARAMFILLER = {}, RESULTLO = 0, RESULTHI = 0, RESULTFILLER = {}}) at include\control.inc:2242' + FLineEnd +
'#34 WNDPROC(0x1f303360, {MSG = 48384, WPARAM = 0, LPARAM = 2031617, RESULT = 0, WPARAMLO = 0, WPARAMHI = 0, WPARAMFILLER = {}, LPARAMLO = 1, LPARAMHI = 31, LPARAMFILLER = {}, RESULTLO = 0, RESULTHI = 0, RESULTFILLER = {}}) at include\wincontrol.inc:5412' + FLineEnd +
'#35 WNDPROC(0x1f303360, {MSG = 48384, WPARAM = 0, LPARAM = 2031617, RESULT = 0, WPARAMLO = 0, WPARAMHI = 0, WPARAMFILLER = {}, LPARAMLO = 1, LPARAMHI = 31, LPARAMFILLER = {}, RESULTLO = 0, RESULTHI = 0, RESULTFILLER = {}}) at synedit.pp:6188' + FLineEnd +
'#36 DELIVERMESSAGE(0x1f303360, 0) at lclmessageglue.pas:112' + FLineEnd +
'#37 DOWINDOWPROC({WINDOW = 591444, MSG = 256, WPARAM = 83, LPARAM = 2031617, LMESSAGE = {MSG = 0, WPARAM = 0, LPARAM = 0, RESULT = 0, WPARAMLO = 0, WPARAMHI = 0, WPARAMFILLER = {}, LPARAMLO = 0, LPARAMHI = 0, LPARAMFILLER = {}, RESULTLO = 0, RESULTHI = 0, RESULTFILLER = {}}, PLMSG = 0x1426fa6c, LWINCONTROL = 0x1f303360, WINPROCESS = false, NOTIFYUSERINPUT = true, WINDOWINFO = 0x2354d7a8, BACKUPBUFFER = {DC = 0, BITMAP = 0, BITMAPWIDTH = 0, BITMAPHEIGHT = 0}, WINDOWWIDTH = 0, WINDOWHEIGHT = 0, PAINTMSG = {MSG = 0, DC = 0, PAINTSTRUCT = 0x0, RESULT = 0}, RTLLAYOUT = false, ORGCHARCODE = 0, LMSCROLL = {MSG = 0, SCROLLCODE = 0, SMALLPOS = 0, SCROLLBAR = 0, RESULT = 0, POS = 0}, LMKEY = {MSG = 48384, CHARCODE = 0, UNUSED = 0, KEYDATA = 2031617, RESULT = 0}, LMCHAR = {MSG = 0, CHARCODE = 0, UNUSED = 0, KEYDATA = 0, RESULT = 0}, LMMOUSE = {MSG = 0, KEYS = 0, XPOS = 0, YPOS = 0, POS = {X = 0, Y = 0}, DUMMY = 0, RESULT = 0}, LMCONTEXTMENU = {MSG = 0, HWND = 0, XPOS = 0, YPOS = 0, POS = {X = 0, Y = 0}, DUMMY = 0, RESULT = 0}, LMMOUSEMOVE = {MSG = 0, KEYS = 0, XPOS = 0, YPOS = 0, POS = {X = 0, Y = 0}, DUMMY = 0, RESULT = 0}, LMMOUSEEVENT = {MSG = 0, BUTTON = 0, WHEELDELTA = 0, X = 0, Y = 0, RESULT = 0, USERDATA = 0x0, STATE = []}, LMMOVE = {MSG = 0, MOVETYPE = 0, XPOS = 0, YPOS = 0, POS = {X = 0, Y = 0}, DUMMY = 0, RESULT = 0}, LMNOTIFY = {MSG = 0, IDCTRL = 0, NMHDR = 0x0, RESULT = 0}, DRAWLISTITEMSTRUCT = {ITEMID = 0, AREA = {LEFT = 0, TOP = 0, RIGHT = 0, BOTTOM = 0, TOPLEFT = {X = 0, Y = 0}, BOTTOMRIGHT = {X = 0, Y = 0}, VECTOR = {0, 0, 0, 0}}, DC = 0, ITEMSTATE = []}, NMHDR = 0x1f0001}) at win32\win32callback.inc:2516' + FLineEnd +
'#38 WINDOWPROC(591444, 256, 83, 2031617) at win32\win32callback.inc:2677' + FLineEnd +
'#39 USER32!AddClipboardFormatListener at :0' + FLineEnd +
'#40 USER32!CallWindowProcW at :0' + FLineEnd +
'#41 USER32!DispatchMessageW at :0' + FLineEnd +
'#42 USER32!DispatchMessageW at :0' + FLineEnd +
'#43 APPPROCESSMESSAGES(0x1433de58) at win32\win32object.inc:407' + FLineEnd +
'#44 HANDLEMESSAGE(0x142d6298) at include\application.inc:1282' + FLineEnd +
'#45 RUNLOOP(0x142d6298) at include\application.inc:1419' + FLineEnd +
'#46 APPRUN(0x1433de58, {Proc = {procedure (POINTER)} 0x1426fed4, Self = 0x142d6298}) at include\interfacebase.inc:54' + FLineEnd +
'#47 RUN(0x142d6298) at include\application.inc:1407' + FLineEnd +
'#48 main at lazarus.pp:152' + FLineEnd
  ;
end;

procedure TTestLeakParser.CheckGdb2(TrcList: TStackTraceList; ReplHash: String);
begin
  CheckTrace('T0', [
    el('..\inc\except.inc'              ,  163, -2, ReplHash+'0 fpc_raiseexception(0x1598bd8, 0x1598bc0, 0xd7d) at ..\inc\except.inc:163'),
    el('..\objpas\sysutils\sysutils.inc',  488, -2, ReplHash+'1 ASSERTERRORHANDLER(''TSynCustomFoldHighlighter''..., ''synhighlighterpas.pp'', 3453, 0x1426ec68) at ..\objpas\sysutils\sysutils.inc:488'),
    el('..\inc\system.inc'              , 1565, -2, ReplHash+'2 fpc_assert(''TSynCustomFoldHighlighter''..., ''synhighlighterpas.pp'', 3453, 0x1426ec68) at ..\inc\system.inc:1565'),
    el('synhighlighterpas.pp'           , 3453, -2, ReplHash+'3 FOLDBLOCKENDLEVEL(0x24fa86d0, -1, {FOLDGROUP = 1, FLAGS = [SFBINCLUDEDISABLED]}) at synhighlighterpas.pp:3453'),
    el('synedithighlighterfoldbase.pas' , 1843, -2, ReplHash+'4 FOLDBLOCKENDLEVEL(0x24fa86d0, -1, 1, [SFBINCLUDEDISABLED]) at synedithighlighterfoldbase.pas:1843'),
    el('synedithighlighterfoldbase.pas' , 1475, -2, ReplHash+'5 INITCOUNT(0x234abcf0) at synedithighlighterfoldbase.pas:1475'),
    el('synedithighlighterfoldbase.pas' , 1674, -2, ReplHash+'6 COUNT(0x234abcf0) at synedithighlighterfoldbase.pas:1674'),
    el('sourcesyneditor.pas'            , 1469, -2, ReplHash+'7 SRCSYNCARETCHANGED(0x1f303360, 0x0) at sourcesyneditor.pas:1469'),
    el('sourcesyneditor.pas'            , 1555, -2, ReplHash+'8 DOONSTATUSCHANGE(0x1f303360, [SCLINESINWINDOW]) at sourcesyneditor.pas:1555'),
    el('synedit.pp'                     , 2501, -2, ReplHash+'9 DODECPAINTLOCK(0x1f303360, 0x1f303360) at synedit.pp:2501'),
    el('lazmethodlist.pas'              ,  315, -2, ReplHash+'10 CALLNOTIFYEVENTS(0x236ff020, 0x1f303360) at lazmethodlist.pas:315'),
    el('synedittextbuffer.pp'           , 1564, -2, ReplHash+'11 SENDNOTIFICATION(0x14365540, SENRDECPAINTLOCK, 0x1f303360) at synedittextbuffer.pp:1564'),
    el('synedittextbuffer.pp'           , 1267, -2, ReplHash+'12 SETUPDATESTATE(0x14365540, false, 0x1f303360) at synedittextbuffer.pp:1267'),
    el('lazsynedittext.pas'             ,  978, -2, ReplHash+'13 ENDUPDATE(0x14365540, 0x1f303360) at lazsynedittext.pas:978'),
    el('synedit.pp'                     , 2387, -2, ReplHash+'14 DECPAINTLOCK(0x1f303360) at synedit.pp:2387'),
    el('synedit.pp'                     , 6417, -2, ReplHash+'15 SETHIGHLIGHTER(0x1f303360, 0x24ce2a20) at synedit.pp:6417'),
    el('sourcesyneditor.pas'            , 1713, -2, ReplHash+'16 SETHIGHLIGHTER(0x1f303360, 0x24ce2a20) at sourcesyneditor.pas:1713'),
    el('sourceeditor.pp'                , 4890, -2, ReplHash+'17 SETSYNTAXHIGHLIGHTERTYPE(0x22b8c598, LSHFREEPASCAL) at sourceeditor.pp:4890'),
    el('sourcefilemanager.pas'          , 5460, -2, ReplHash+'18 RENAMEUNIT(0x18d45bf0, 0x2345cb24 ''P:\programs\lazarus''..., 0x238c1b6c ''OPopupForm'', 0x0, 0x0) at sourcefilemanager.pas:5460'),
    el('sourcefilemanager.pas'          , 4721, -2, ReplHash+'19 SHOWSAVEFILEASDIALOG(0x236fd32c ''unit2.pas'', 0x18d45bf0, 0x0, 0x0, false) at sourcefilemanager.pas:4721'),
    el('sourcefilemanager.pas'          , 2487, -2, ReplHash+'20 SAVEEDITORFILE(0x22b8c598, [SFSAVEAS, SFCHECKAMBIGUOUSFILES]) at sourcefilemanager.pas:2487'),
    el('main.pp'                        , 5862, -2, ReplHash+'21 DOSAVEEDITORFILE(0x1433e058, 0x22b8c598, [SFCHECKAMBIGUOUSFILES]) at main.pp:5862'),
    el('main.pp'                        , 3341, -2, ReplHash+'22 MNUSAVECLICKED(0x1433e058, 0x1433e058) at main.pp:3341'),
    el('main.pp'                        , 3538, -2, ReplHash+'23 PROCESSIDECOMMAND(0x1433e058, 0x24fa1810, 1208, true) at main.pp:3538'),
    el('sourceeditor.pp'                , 8833, -2, ReplHash+'24 PROCESSPARENTCOMMAND(0x24fa1810, 0x22b8c598, 1208, '''', 0x0, true) at sourceeditor.pp:8833'),
    el('sourceeditor.pp'                , 4203, -2, ReplHash+'25 PROCESSUSERCOMMAND(0x22b8c598, 0x1f303360, 1208, '''', 0x0) at sourceeditor.pp:4203'),
    el('synedit.pp'                     , 7261, -2, ReplHash+'26 DOONPROCESSCOMMAND(0x1f303360, 1208, '''', 0x0) at synedit.pp:7261'),
    el('synedit.pp'                     , 6607, -2, ReplHash+'27 COMMANDPROCESSOR(0x1f303360, 1208, '''', 0x0, []) at synedit.pp:6607'),
    el('synedit.pp'                     , 3006, -2, ReplHash+'28 KEYDOWN(0x1f303360, 0, [SSCTRL]) at synedit.pp:3006'),
    el('include\wincontrol.inc'         , 5685, -2, ReplHash+'29 KEYDOWNBEFOREINTERFACE(0x1f303360, 0, [SSCTRL]) at include\wincontrol.inc:5685'),
    el('include\wincontrol.inc'         , 5816, -2, ReplHash+'30 DOKEYDOWNBEFOREINTERFACE(0x1f303360, {MSG = 48384, CHARCODE = 0, UNUSED = 0, KEYDATA = 2031617, RESULT = 0}, false) at include\wincontrol.inc:5816'),
    el('include\wincontrol.inc'         , 7205, -2, ReplHash+'31 CNKEYDOWN(0x1f303360, {MSG = 48384, CHARCODE = 0, UNUSED = 0, KEYDATA = 2031617, RESULT = 0}) at include\wincontrol.inc:7205'),
    el('..\inc\objpas.inc'              ,  684, -2, ReplHash+'32 DISPATCH(0x1f303360, 0) at ..\inc\objpas.inc:684'),
    el('include\control.inc'            , 2242, -2, ReplHash+'33 WNDPROC(0x1f303360, {MSG = 48384, WPARAM = 0, LPARAM = 2031617, RESULT = 0, WPARAMLO = 0, WPARAMHI = 0, WPARAMFILLER = {}, LPARAMLO = 1, LPARAMHI = 31, LPARAMFILLER = {}, RESULTLO = 0, RESULTHI = 0, RESULTFILLER = {}}) at include\control.inc:2242'),
    el('include\wincontrol.inc'         , 5412, -2, ReplHash+'34 WNDPROC(0x1f303360, {MSG = 48384, WPARAM = 0, LPARAM = 2031617, RESULT = 0, WPARAMLO = 0, WPARAMHI = 0, WPARAMFILLER = {}, LPARAMLO = 1, LPARAMHI = 31, LPARAMFILLER = {}, RESULTLO = 0, RESULTHI = 0, RESULTFILLER = {}}) at include\wincontrol.inc:5412'),
    el('synedit.pp'                     , 6188, -2, ReplHash+'35 WNDPROC(0x1f303360, {MSG = 48384, WPARAM = 0, LPARAM = 2031617, RESULT = 0, WPARAMLO = 0, WPARAMHI = 0, WPARAMFILLER = {}, LPARAMLO = 1, LPARAMHI = 31, LPARAMFILLER = {}, RESULTLO = 0, RESULTHI = 0, RESULTFILLER = {}}) at synedit.pp:6188'),
    el('lclmessageglue.pas'             ,  112, -2, ReplHash+'36 DELIVERMESSAGE(0x1f303360, 0) at lclmessageglue.pas:112'),
    el('win32\win32callback.inc'        , 2516, -2, ReplHash+'37 DOWINDOWPROC({WINDOW = 591444, MSG = 256, WPARAM = 83, LPARAM = 2031617, LMESSAGE = {MSG = 0, WPARAM = 0, LPARAM = 0, RESULT = 0, WPARAMLO = 0, WPARAMHI = 0, WPARAMFILLER = {}, LPARAMLO = 0, LPARAMHI = 0, LPARAMFILLER = {}, RESULTLO = 0, RESULTHI = 0, RESULTFILLER = {}}, PLMSG = 0x1426fa6c, LWINCONTROL = 0x1f303360, WINPROCESS = false, NOTIFYUSERINPUT = true, WINDOWINFO = 0x2354d7a8, BACKUPBUFFER = {DC = 0, BITMAP = 0, BITMAPWIDTH = 0, BITMAPHEIGHT = 0}, WINDOWWIDTH = 0, WINDOWHEIGHT = 0, PAINTMSG = {MSG = 0, DC = 0, PAINTSTRUCT = 0x0, RESULT = 0}, RTLLAYOUT = false, ORGCHARCODE = 0, LMSCROLL = {MSG = 0, SCROLLCODE = 0, SMALLPOS = 0, SCROLLBAR = 0, RESULT = 0, POS = 0}, LMKEY = {MSG = 48384, CHARCODE = 0, UNUSED = 0, KEYDATA = 2031617, RESULT = 0}, LMCHAR = {MSG = 0, CHARCODE = 0, UNUSED = 0, KEYDATA = 0, RESULT = 0}, LMMOUSE = {MSG = 0, KEYS = 0, XPOS = 0, YPOS = 0, POS = {X = 0, Y = 0}, DUMMY = 0, RESULT = 0}, LMCONTEXTMENU = {MSG = 0, HWND = 0, XPOS = 0, YPOS = 0, POS = {X = 0, Y = 0}, DUMMY = 0, RESULT = 0}, LMMOUSEMOVE = {MSG = 0, KEYS = 0, XPOS = 0, YPOS = 0, POS = {X = 0, Y = 0}, DUMMY = 0, RESULT = 0}, LMMOUSEEVENT = {MSG = 0, BUTTON = 0, WHEELDELTA = 0, X = 0, Y = 0, RESULT = 0, USERDATA = 0x0, STATE = []}, LMMOVE = {MSG = 0, MOVETYPE = 0, XPOS = 0, YPOS = 0, POS = {X = 0, Y = 0}, DUMMY = 0, RESULT = 0}, LMNOTIFY = {MSG = 0, IDCTRL = 0, NMHDR = 0x0, RESULT = 0}, DRAWLISTITEMSTRUCT = {ITEMID = 0, AREA = {LEFT = 0, TOP = 0, RIGHT = 0, BOTTOM = 0, TOPLEFT = {X = 0, Y = 0}, BOTTOMRIGHT = {X = 0, Y = 0}, VECTOR = {0, 0, 0, 0}}, DC = 0, ITEMSTATE = []}, NMHDR = 0x1f0001}) at win32\win32callback.inc:2516'),
    el('win32\win32callback.inc'        , 2677, -2, ReplHash+'38 WINDOWPROC(591444, 256, 83, 2031617) at win32\win32callback.inc:2677'),
    el(''                               ,    0, -2, ReplHash+'39 USER32!AddClipboardFormatListener at :0'),
    el(''                               ,    0, -2, ReplHash+'40 USER32!CallWindowProcW at :0'),
    el(''                               ,    0, -2, ReplHash+'41 USER32!DispatchMessageW at :0'),
    el(''                               ,    0, -2, ReplHash+'42 USER32!DispatchMessageW at :0'),
    el('win32\win32object.inc'          ,  407, -2, ReplHash+'43 APPPROCESSMESSAGES(0x1433de58) at win32\win32object.inc:407'),
    el('include\application.inc'        , 1282, -2, ReplHash+'44 HANDLEMESSAGE(0x142d6298) at include\application.inc:1282'),
    el('include\application.inc'        , 1419, -2, ReplHash+'45 RUNLOOP(0x142d6298) at include\application.inc:1419'),
    el('include\interfacebase.inc'      ,   54, -2, ReplHash+'46 APPRUN(0x1433de58, {Proc = {procedure (POINTER)} 0x1426fed4, Self = 0x142d6298}) at include\interfacebase.inc:54'),
    el('include\application.inc'        , 1407, -2, ReplHash+'47 RUN(0x142d6298) at include\application.inc:1407'),
    el('lazarus.pp'                     ,  152, -2, ReplHash+'48 main at lazarus.pp:152')
    ], TrcList[0]);
end;

function TTestLeakParser.TextLldb1: String;
begin
  Result :=
'  * frame #0: 0x0041ab6f project1.exe`DOCREATE(this=0x04a91060) at customform.inc:939' + FLineEnd +
'    frame #1: 0x048fa158' + FLineEnd +
'    frame #2: 0x00402a42 project1.exe`main at project1.lpr:19' + FLineEnd
  ;
end;

function TTestLeakParser.TextValgrind: String;
begin
  Result :=
'==2119== Memcheck, a memory error detector' + FLineEnd +
'==2119== Copyright (C) 2002-2017, and GNU GPL''d, by Julian Seward et al.' + FLineEnd +
'==2119== Using Valgrind-3.13.0 and LibVEX; rerun with -h for copyright info' + FLineEnd +
'==2119== Command: ../projects/dummy2/project1' + FLineEnd +
'==2119== Parent PID: 8694' + FLineEnd +
'==2119==' + FLineEnd +
'==2119==' + FLineEnd +
'==2119==' + FLineEnd +
'==2119== Invalid read of size 4' + FLineEnd +
'==2119==    at 0x153590B: FPDMEMORYTOOLS$_$TFPDBGMEMCACHEMANAGERSIMPLE_$__$$_HASMEMORY$TDBGPTR$LONGWORD$$BOOLEAN (fpdmemorytools.pas:709)' + FLineEnd +
'==2119==    by 0x1535A9A: FPDMEMORYTOOLS$_$TFPDBGMEMCACHEMANAGERSIMPLE_$__$$_ADDCACHE$TDBGPTR$LONGWORD$$TFPDBGMEMCACHEBASE (fpdmemorytools.pas:737)' + FLineEnd +
'==2119==    by 0x163E00B: FPLLDBDEBUGGER$_$TFPLLDBDBGMEMCACHEMANAGERSIMPLE_$__$$_READMEMORY$TDBGPTR$LONGWORD$POINTER$$BOOLEAN (fplldbdebugger.pas:568)' + FLineEnd +
'==2119==    by 0x1536B05: FPDMEMORYTOOLS$_$TFPDBGMEMMANAGER_$__$$_READMEMORY$crc1EE57BA2 (fpdmemorytools.pas:894)' + FLineEnd +
'==2119==    by 0x14FC5E5: FPDBGDWARF$_$TFPDWARFVALUEPOINTER_$__$$_GETASSTRING$$ANSISTRING (fpdbgdwarf.pas:1944)' + FLineEnd +
'==2119==    by 0x152F7FE: FPPASCALBUILDER$_$TFPPASCALPRETTYPRINTER_$_INTERNALPRINTVALUE$crc05F9855F_$$_DOPOINTER$BOOLEAN (fppascalbuilder.pas:508)' + FLineEnd +
'==2119==    by 0x152C603: FPPASCALBUILDER$_$TFPPASCALPRETTYPRINTER_$__$$_INTERNALPRINTVALUE$crc05F9855F (fppascalbuilder.pas:886)' + FLineEnd +
'==2119==    by 0x152DB9C: FPPASCALBUILDER$_$TFPPASCALPRETTYPRINTER_$_INTERNALPRINTVALUE$crc05F9855F_$$_DOSTRUCTURE (fppascalbuilder.pas:743)' + FLineEnd +
'==2119==    by 0x152C693: FPPASCALBUILDER$_$TFPPASCALPRETTYPRINTER_$__$$_INTERNALPRINTVALUE$crc05F9855F (fppascalbuilder.pas:902)' + FLineEnd +
'==2119==    by 0x152FD04: FPPASCALBUILDER$_$TFPPASCALPRETTYPRINTER_$__$$_PRINTVALUE$crc19949757 (fppascalbuilder.pas:932)' + FLineEnd +
'==2119==    by 0x1642298: FPLLDBDEBUGGER$_$TFPLLDBDEBUGGER_$__$$_EVALUATEEXPRESSION$crc5FF8C3B9 (fplldbdebugger.pas:1298)' + FLineEnd +
'==2119==    by 0x1640D46: FPLLDBDEBUGGER$_$TFPLLDBDEBUGGER_$__$$_REQUESTCOMMAND$TDBGCOMMAND$array_of_const$TMETHOD$$BOOLEAN (fplldbdebugger.pas:1006)' + FLineEnd +
'==2119==  Address 0x24986000 is 144 bytes inside a block of size 160 in arena "client"' + FLineEnd +
'==2119==' + FLineEnd +
'==2119== Invalid read of size 8' + FLineEnd +
'==2119==    at 0x1535916: FPDMEMORYTOOLS$_$TFPDBGMEMCACHEMANAGERSIMPLE_$__$$_HASMEMORY$TDBGPTR$LONGWORD$$BOOLEAN (fpdmemorytools.pas:708)' + FLineEnd +
'==2119==    by 0x1535A9A: FPDMEMORYTOOLS$_$TFPDBGMEMCACHEMANAGERSIMPLE_$__$$_ADDCACHE$TDBGPTR$LONGWORD$$TFPDBGMEMCACHEBASE (fpdmemorytools.pas:737)' + FLineEnd +
'==2119==    by 0x163E00B: FPLLDBDEBUGGER$_$TFPLLDBDBGMEMCACHEMANAGERSIMPLE_$__$$_READMEMORY$TDBGPTR$LONGWORD$POINTER$$BOOLEAN (fplldbdebugger.pas:568)' + FLineEnd +
'==2119==    by 0x1536B05: FPDMEMORYTOOLS$_$TFPDBGMEMMANAGER_$__$$_READMEMORY$crc1EE57BA2 (fpdmemorytools.pas:894)' + FLineEnd +
'==2119==    by 0x14FC5E5: FPDBGDWARF$_$TFPDWARFVALUEPOINTER_$__$$_GETASSTRING$$ANSISTRING (fpdbgdwarf.pas:1944)' + FLineEnd +
'==2119==    by 0x152F7FE: FPPASCALBUILDER$_$TFPPASCALPRETTYPRINTER_$_INTERNALPRINTVALUE$crc05F9855F_$$_DOPOINTER$BOOLEAN (fppascalbuilder.pas:508)' + FLineEnd +
'==2119==    by 0x152C603: FPPASCALBUILDER$_$TFPPASCALPRETTYPRINTER_$__$$_INTERNALPRINTVALUE$crc05F9855F (fppascalbuilder.pas:886)' + FLineEnd +
'==2119==    by 0x152DB9C: FPPASCALBUILDER$_$TFPPASCALPRETTYPRINTER_$_INTERNALPRINTVALUE$crc05F9855F_$$_DOSTRUCTURE (fppascalbuilder.pas:743)' + FLineEnd +
'==2119==    by 0x152C693: FPPASCALBUILDER$_$TFPPASCALPRETTYPRINTER_$__$$_INTERNALPRINTVALUE$crc05F9855F (fppascalbuilder.pas:902)' + FLineEnd +
'==2119==    by 0x152FD04: FPPASCALBUILDER$_$TFPPASCALPRETTYPRINTER_$__$$_PRINTVALUE$crc19949757 (fppascalbuilder.pas:932)' + FLineEnd +
'==2119==    by 0x1642298: FPLLDBDEBUGGER$_$TFPLLDBDEBUGGER_$__$$_EVALUATEEXPRESSION$crc5FF8C3B9 (fplldbdebugger.pas:1298)' + FLineEnd +
'==2119==    by 0x1640D46: FPLLDBDEBUGGER$_$TFPLLDBDEBUGGER_$__$$_REQUESTCOMMAND$TDBGCOMMAND$array_of_const$TMETHOD$$BOOLEAN (fplldbdebugger.pas:1006)' + FLineEnd +
'==2119==  Address 0x24985ff8 is 136 bytes inside a block of size 160 in arena "client"' + FLineEnd +
'==2119==' + FLineEnd +
'==2119== Invalid write of size 1' + FLineEnd +
'==2119==    at 0x163D4E4: FPLLDBDEBUGGER$_$TFPLLDBDBGMEMREADER_$__$$_READMEMORY$TDBGPTR$LONGWORD$POINTER$$BOOLEAN (fplldbdebugger.pas:453)' + FLineEnd +
'==2119==    by 0x15354EE: FPDMEMORYTOOLS$_$TFPDBGMEMCACHESIMPLE_$__$$_READMEMORY$TDBGPTR$LONGWORD$POINTER$$BOOLEAN (fpdmemorytools.pas:651)' + FLineEnd +
'==2119==    by 0x1535A24: FPDMEMORYTOOLS$_$TFPDBGMEMCACHEMANAGERSIMPLE_$__$$_READMEMORY$TDBGPTR$LONGWORD$POINTER$$BOOLEAN (fpdmemorytools.pas:726)' + FLineEnd +
'==2119==    by 0x163E02F: FPLLDBDEBUGGER$_$TFPLLDBDBGMEMCACHEMANAGERSIMPLE_$__$$_READMEMORY$TDBGPTR$LONGWORD$POINTER$$BOOLEAN (fplldbdebugger.pas:569)' + FLineEnd +
'==2119==    by 0x1535E6F: FPDMEMORYTOOLS$_$TFPDBGMEMMANAGER_$__$$_READMEMORY$crcAA80CA53 (fpdmemorytools.pas:785)' + FLineEnd +
'==2119==    by 0x153744A: FPDMEMORYTOOLS$_$TFPDBGMEMMANAGER_$__$$_READADDRESS$crc594D75F5 (fpdmemorytools.pas:967)' + FLineEnd +
'==2119==    by 0x14FF00E: FPDBGDWARF$_$TFPDWARFVALUESTRUCT_$__$$_GETDATAADDRESS$$TFPDBGMEMLOCATION (fpdbgdwarf.pas:2336)' + FLineEnd +
'==2119==    by 0x152D29B: FPPASCALBUILDER$_$TFPPASCALPRETTYPRINTER_$_INTERNALPRINTVALUE$crc05F9855F_$$_DOSTRUCTURE (fppascalbuilder.pas:684)' + FLineEnd +
'==2119==    by 0x152C67F: FPPASCALBUILDER$_$TFPPASCALPRETTYPRINTER_$__$$_INTERNALPRINTVALUE$crc05F9855F (fppascalbuilder.pas:900)' + FLineEnd +
'==2119==    by 0x152DB9C: FPPASCALBUILDER$_$TFPPASCALPRETTYPRINTER_$_INTERNALPRINTVALUE$crc05F9855F_$$_DOSTRUCTURE (fppascalbuilder.pas:743)' + FLineEnd +
'==2119==    by 0x152C693: FPPASCALBUILDER$_$TFPPASCALPRETTYPRINTER_$__$$_INTERNALPRINTVALUE$crc05F9855F (fppascalbuilder.pas:902)' + FLineEnd +
'==2119==    by 0x152FD04: FPPASCALBUILDER$_$TFPPASCALPRETTYPRINTER_$__$$_PRINTVALUE$crc19949757 (fppascalbuilder.pas:932)' + FLineEnd +
'==2119==  Address 0x231c4de8 is 24 bytes inside a block of size 32 free''d' + FLineEnd +
'==2119==    at 0x4C2FDAC: free (vg_replace_malloc.c:530)' + FLineEnd +
'==2119==    by 0x44F924: CMEM_$$_CFREEMEM$POINTER$$QWORD (cmem.pp:75)' + FLineEnd +
'==2119==    by 0x43C0A9: SYSTEM_$$_FREEMEM$POINTER$$QWORD (heap.inc:316)' + FLineEnd +
'==2119==    by 0x4344D2: fpc_dynarray_clear (dynarr.inc:77)' + FLineEnd +
'==2119==    by 0x439CDE: fpc_finalize (rtti.inc:203)' + FLineEnd +
'==2119==    by 0x439ADD: SYSTEM_$$_RECORDRTTI$POINTER$POINTER$TRTTIPROC (rtti.inc:112)' + FLineEnd +
'==2119==    by 0x436076: SYSTEM$_$TOBJECT_$__$$_CLEANUPINSTANCE (objpas.inc:673)' + FLineEnd +
'==2119==    by 0x435988: SYSTEM$_$TOBJECT_$__$$_FREEINSTANCE (objpas.inc:364)' + FLineEnd +
'==2119==    by 0x435757: SYSTEM$_$TOBJECT_$__$$_DESTROY (objpas.inc:281)' + FLineEnd +
'==2119==    by 0x43578E: SYSTEM$_$TOBJECT_$__$$_FREE (objpas.inc:288)' + FLineEnd +
'==2119==    by 0x1535B34: FPDMEMORYTOOLS$_$TFPDBGMEMCACHEMANAGERSIMPLE_$__$$_REMOVECACHE$TFPDBGMEMCACHEBASE (fpdmemorytools.pas:751)' + FLineEnd +
'==2119==    by 0x163E116: FPLLDBDEBUGGER$_$TFPLLDBDBGMEMCACHEMANAGERSIMPLE_$__$$_CLEAR (fplldbdebugger.pas:577)' + FLineEnd +
'==2119==  Block was alloc''d at' + FLineEnd +
'==2119==    at 0x4C2EBAB: malloc (vg_replace_malloc.c:299)' + FLineEnd +
'==2119==    by 0x44F8D9: CMEM_$$_CGETMEM$QWORD$$POINTER (cmem.pp:62)' + FLineEnd +
'==2119==    by 0x43BF7D: SYSTEM_$$_GETMEM$POINTER$QWORD (heap.inc:276)' + FLineEnd +
'==2119==    by 0x434622: fpc_dynarray_setlength (dynarr.inc:153)' + FLineEnd +
'==2119==    by 0x1535485: FPDMEMORYTOOLS$_$TFPDBGMEMCACHESIMPLE_$__$$_READMEMORY$TDBGPTR$LONGWORD$POINTER$$BOOLEAN (fpdmemorytools.pas:650)' + FLineEnd +
'==2119==    by 0x1535A24: FPDMEMORYTOOLS$_$TFPDBGMEMCACHEMANAGERSIMPLE_$__$$_READMEMORY$TDBGPTR$LONGWORD$POINTER$$BOOLEAN (fpdmemorytools.pas:726)' + FLineEnd +
'==2119==    by 0x163E02F: FPLLDBDEBUGGER$_$TFPLLDBDBGMEMCACHEMANAGERSIMPLE_$__$$_READMEMORY$TDBGPTR$LONGWORD$POINTER$$BOOLEAN (fplldbdebugger.pas:569)' + FLineEnd +
'==2119==    by 0x1535E6F: FPDMEMORYTOOLS$_$TFPDBGMEMMANAGER_$__$$_READMEMORY$crcAA80CA53 (fpdmemorytools.pas:785)' + FLineEnd +
'==2119==    by 0x153744A: FPDMEMORYTOOLS$_$TFPDBGMEMMANAGER_$__$$_READADDRESS$crc594D75F5 (fpdmemorytools.pas:967)' + FLineEnd +
'==2119==    by 0x14FF00E: FPDBGDWARF$_$TFPDWARFVALUESTRUCT_$__$$_GETDATAADDRESS$$TFPDBGMEMLOCATION (fpdbgdwarf.pas:2336)' + FLineEnd +
'==2119==    by 0x152D29B: FPPASCALBUILDER$_$TFPPASCALPRETTYPRINTER_$_INTERNALPRINTVALUE$crc05F9855F_$$_DOSTRUCTURE (fppascalbuilder.pas:684)' + FLineEnd +
'==2119==    by 0x152C67F: FPPASCALBUILDER$_$TFPPASCALPRETTYPRINTER_$__$$_INTERNALPRINTVALUE$crc05F9855F (fppascalbuilder.pas:900)' + FLineEnd +
'==2119==' + FLineEnd
  ;
end;

procedure TTestLeakParser.TestLeakText1;
var
  Trc: TLeakInfo;
  TrcData: TLeakStatus;
  TrcList: TStackTraceList;
begin
  FLineEnd := LineEnding;
  Trc := AllocHeapTraceInfoFromText(TextLeak1);
  TrcList := TStackTraceList.Create();
  Trc.GetLeakInfo(TrcData, TrcList);

  AssertEquals('Has 8 traces', 8, TrcList.Count);
  CheckLeak1(TrcList);

  TrcList.Free;
end;

procedure TTestLeakParser.TestLeakText1NoHeader;
var
  Trc: TLeakInfo;
  TrcData: TLeakStatus;
  TrcList: TStackTraceList;
begin
  FLineEnd := LineEnding;
  Trc := AllocHeapTraceInfoFromText(TextLeak1(False));
  TrcList := TStackTraceList.Create();
  Trc.GetLeakInfo(TrcData, TrcList);

  AssertEquals('Has 8 traces', 8, TrcList.Count);
  CheckLeak1(TrcList);

  TrcList.Free;
end;

procedure TTestLeakParser.TestLeakNoTraceHeader;
var
  Trc: TLeakInfo;
  TrcData: TLeakStatus;
  TrcList: TStackTraceList;
begin
  FLineEnd := LineEnding;
  Trc := AllocHeapTraceInfoFromText(
'  $0043F1C9 line 227 of dbcommonunit.pas' + FLineEnd +
'  $00675FB3 line 598 of colorunit.pas' + FLineEnd +
'  $0043DA37 line 2775 of externalmonmain.pas' + FLineEnd +
'  $0042A208' + FLineEnd +
'  $00429811' + FLineEnd +
'  $0040DF58' + FLineEnd +
'  $0050A3F1' + FLineEnd +
'  $0042B07D' + FLineEnd
  );
  TrcList := TStackTraceList.Create();
  Trc.GetLeakInfo(TrcData, TrcList);

  AssertEquals('Has 1 trace', 1, TrcList.Count);
  CheckTrace('T0', [
    el('dbcommonunit.pas',     227, $0043F1C9, '  $0043F1C9 line 227 of dbcommonunit.pas'),
    el('colorunit.pas',        598, $00675FB3, '  $00675FB3 line 598 of colorunit.pas'),
    el('externalmonmain.pas', 2775, $0043DA37, '  $0043DA37 line 2775 of externalmonmain.pas'),
    el('', -1, $0042A208, '  $0042A208'),
    el('', -1, $00429811, '  $00429811'),
    el('', -1, $0040DF58, '  $0040DF58'),
    el('', -1, $0050A3F1, '  $0050A3F1'),
    el('', -1, $0042B07D, '  $0042B07D')
    ], TrcList[0]);

  TrcList.Free;
end;

procedure TTestLeakParser.TestLeakLeadSpaces;
var
  Trc: TLeakInfo;
  TrcData: TLeakStatus;
  TrcList: TStackTraceList;
begin
  FLineEnd := LineEnding;
  Trc := AllocHeapTraceInfoFromText(
'Call trace for block $083909D8 size 260' + FLineEnd +
' $0043F1C9 line 227 of dbcommonunit.pas' + FLineEnd +
' $00675FB3 line 598 of colorunit.pas' + FLineEnd +
' $00429811' + FLineEnd +
'Call trace for block $083D0570 size 656' + FLineEnd +
'$006740DE line 285 of colorunit.pas' + FLineEnd +
'$0066F1F7 line 209 of colorunit.pas' + FLineEnd +
'$0043DA09 line 2772 of externalmonmain.pas' + FLineEnd +
'$0042B07D' + FLineEnd
  );
  TrcList := TStackTraceList.Create();
  Trc.GetLeakInfo(TrcData, TrcList);

  AssertEquals('Has 2 trace', 2, TrcList.Count);
  CheckTrace('T0', [
    el('dbcommonunit.pas',     227, $0043F1C9, ' $0043F1C9 line 227 of dbcommonunit.pas'),
    el('colorunit.pas',        598, $00675FB3, ' $00675FB3 line 598 of colorunit.pas'),
    el('', -1, $00429811, ' $00429811')
    ], TrcList[0]);

  CheckTrace('T1', [
    el('colorunit.pas', 285, $006740DE, '$006740DE line 285 of colorunit.pas'),
    el('colorunit.pas', 209, $0066F1F7, '$0066F1F7 line 209 of colorunit.pas'),
    el('externalmonmain.pas', 2772, $0043DA09, '$0043DA09 line 2772 of externalmonmain.pas'),
    el('', -1, $0042B07D, '$0042B07D')
    ], TrcList[1]);

  TrcList.Free;
end;

procedure TTestLeakParser.TestGdb1;
var
  Trc: TLeakInfo;
  TrcData: TLeakStatus;
  TrcList: TStackTraceList;
begin
  FLineEnd := LineEnding;
  Trc := AllocHeapTraceInfoFromText(TextGdb1);
  TrcList := TStackTraceList.Create();
  Trc.GetLeakInfo(TrcData, TrcList);

  AssertEquals('Has 1 traces', 1, TrcList.Count);

  CheckGdb1(TrcList);

  TrcList.Free;
end;

procedure TTestLeakParser.TestGdb1Mantis;
var
  Trc: TLeakInfo;
  TrcData: TLeakStatus;
  TrcList: TStackTraceList;
begin
  FLineEnd := LineEnding;
  Trc := AllocHeapTraceInfoFromText(StringReplace(TextGdb1, '#', '0000', [rfReplaceAll]));
  TrcList := TStackTraceList.Create();
  Trc.GetLeakInfo(TrcData, TrcList);

  AssertEquals('Has 1 traces', 1, TrcList.Count);

  CheckGdb1(TrcList, '0000');

  TrcList.Free;
end;

procedure TTestLeakParser.TestGdb2;
var
  Trc: TLeakInfo;
  TrcData: TLeakStatus;
  TrcList: TStackTraceList;
begin
  FLineEnd := LineEnding;
  Trc := AllocHeapTraceInfoFromText(TextGdb2);
  TrcList := TStackTraceList.Create();
  Trc.GetLeakInfo(TrcData, TrcList);

  AssertEquals('Has 1 traces', 1, TrcList.Count);

  CheckGdb2(TrcList);

  TrcList.Free;
end;

procedure TTestLeakParser.TestGdb2Mantis;
var
  Trc: TLeakInfo;
  TrcData: TLeakStatus;
  TrcList: TStackTraceList;
begin
  FLineEnd := LineEnding;
  Trc := AllocHeapTraceInfoFromText(StringReplace(TextGdb2, '#', '0000', [rfReplaceAll]));
  TrcList := TStackTraceList.Create();
  Trc.GetLeakInfo(TrcData, TrcList);

  AssertEquals('Has 1 traces', 1, TrcList.Count);

  CheckGdb2(TrcList, '0000');

  TrcList.Free;
end;

procedure TTestLeakParser.TestLldb;
var
  Trc: TLeakInfo;
  TrcData: TLeakStatus;
  TrcList: TStackTraceList;
begin
  FLineEnd := LineEnding;
  Trc := AllocHeapTraceInfoFromText(TextLldb1);
  TrcList := TStackTraceList.Create();
  Trc.GetLeakInfo(TrcData, TrcList);

  AssertEquals('Has 1 traces', 1, TrcList.Count);

  CheckTrace('T0', [
    el('customform.inc', 939, $0041ab6f, '  * frame #0: 0x0041ab6f project1.exe`DOCREATE(this=0x04a91060) at customform.inc:939'),
    el(''              ,   0, -2 {$048fa158}, '    frame #1: 0x048fa158'),
    el('project1.lpr'  ,  19, $00402a42, '    frame #2: 0x00402a42 project1.exe`main at project1.lpr:19')
    ], TrcList[0]);

  TrcList.Free;
end;

procedure TTestLeakParser.TestValgrind;
var
  Trc: TLeakInfo;
  TrcData: TLeakStatus;
  TrcList: TStackTraceList;
begin
  FLineEnd := LineEnding;
  Trc := AllocHeapTraceInfoFromText(TextValgrind);
  TrcList := TStackTraceList.Create();
  Trc.GetLeakInfo(TrcData, TrcList);

  AssertEquals('Has 3 traces', 3, TrcList.Count);

  CheckTrace('T0', [
el('fpdmemorytools.pas' ,  709, $153590B, '==2119==    at 0x153590B: FPDMEMORYTOOLS$_$TFPDBGMEMCACHEMANAGERSIMPLE_$__$$_HASMEMORY$TDBGPTR$LONGWORD$$BOOLEAN (fpdmemorytools.pas:709)'),
el('fpdmemorytools.pas' ,  737, $1535A9A, '==2119==    by 0x1535A9A: FPDMEMORYTOOLS$_$TFPDBGMEMCACHEMANAGERSIMPLE_$__$$_ADDCACHE$TDBGPTR$LONGWORD$$TFPDBGMEMCACHEBASE (fpdmemorytools.pas:737)'),
el('fplldbdebugger.pas' ,  568, $163E00B, '==2119==    by 0x163E00B: FPLLDBDEBUGGER$_$TFPLLDBDBGMEMCACHEMANAGERSIMPLE_$__$$_READMEMORY$TDBGPTR$LONGWORD$POINTER$$BOOLEAN (fplldbdebugger.pas:568)'),
el('fpdmemorytools.pas' ,  894, $1536B05, '==2119==    by 0x1536B05: FPDMEMORYTOOLS$_$TFPDBGMEMMANAGER_$__$$_READMEMORY$crc1EE57BA2 (fpdmemorytools.pas:894)'),
el('fpdbgdwarf.pas'     , 1944, $14FC5E5, '==2119==    by 0x14FC5E5: FPDBGDWARF$_$TFPDWARFVALUEPOINTER_$__$$_GETASSTRING$$ANSISTRING (fpdbgdwarf.pas:1944)'),
el('fppascalbuilder.pas',  508, $152F7FE, '==2119==    by 0x152F7FE: FPPASCALBUILDER$_$TFPPASCALPRETTYPRINTER_$_INTERNALPRINTVALUE$crc05F9855F_$$_DOPOINTER$BOOLEAN (fppascalbuilder.pas:508)'),
el('fppascalbuilder.pas',  886, $152C603, '==2119==    by 0x152C603: FPPASCALBUILDER$_$TFPPASCALPRETTYPRINTER_$__$$_INTERNALPRINTVALUE$crc05F9855F (fppascalbuilder.pas:886)'),
el('fppascalbuilder.pas',  743, $152DB9C, '==2119==    by 0x152DB9C: FPPASCALBUILDER$_$TFPPASCALPRETTYPRINTER_$_INTERNALPRINTVALUE$crc05F9855F_$$_DOSTRUCTURE (fppascalbuilder.pas:743)'),
el('fppascalbuilder.pas',  902, $152C693, '==2119==    by 0x152C693: FPPASCALBUILDER$_$TFPPASCALPRETTYPRINTER_$__$$_INTERNALPRINTVALUE$crc05F9855F (fppascalbuilder.pas:902)'),
el('fppascalbuilder.pas',  932, $152FD04, '==2119==    by 0x152FD04: FPPASCALBUILDER$_$TFPPASCALPRETTYPRINTER_$__$$_PRINTVALUE$crc19949757 (fppascalbuilder.pas:932)'),
el('fplldbdebugger.pas' , 1298, $1642298, '==2119==    by 0x1642298: FPLLDBDEBUGGER$_$TFPLLDBDEBUGGER_$__$$_EVALUATEEXPRESSION$crc5FF8C3B9 (fplldbdebugger.pas:1298)'),
el('fplldbdebugger.pas' , 1006, $1640D46, '==2119==    by 0x1640D46: FPLLDBDEBUGGER$_$TFPLLDBDEBUGGER_$__$$_REQUESTCOMMAND$TDBGCOMMAND$array_of_const$TMETHOD$$BOOLEAN (fplldbdebugger.pas:1006)'),
el(''                   ,    0, 0, '==2119==  Address 0x24986000 is 144 bytes inside a block of size 160 in arena "client"')
,el('',0,0,'==2119==')
    ], TrcList[0]);

  CheckTrace('T1', [
el('fpdmemorytools.pas' ,  708, $1535916, '==2119==    at 0x1535916: FPDMEMORYTOOLS$_$TFPDBGMEMCACHEMANAGERSIMPLE_$__$$_HASMEMORY$TDBGPTR$LONGWORD$$BOOLEAN (fpdmemorytools.pas:708)'),
el('fpdmemorytools.pas' ,  737, $1535A9A, '==2119==    by 0x1535A9A: FPDMEMORYTOOLS$_$TFPDBGMEMCACHEMANAGERSIMPLE_$__$$_ADDCACHE$TDBGPTR$LONGWORD$$TFPDBGMEMCACHEBASE (fpdmemorytools.pas:737)'),
el('fplldbdebugger.pas' ,  568, $163E00B, '==2119==    by 0x163E00B: FPLLDBDEBUGGER$_$TFPLLDBDBGMEMCACHEMANAGERSIMPLE_$__$$_READMEMORY$TDBGPTR$LONGWORD$POINTER$$BOOLEAN (fplldbdebugger.pas:568)'),
el('fpdmemorytools.pas' ,  894, $1536B05, '==2119==    by 0x1536B05: FPDMEMORYTOOLS$_$TFPDBGMEMMANAGER_$__$$_READMEMORY$crc1EE57BA2 (fpdmemorytools.pas:894)'),
el('fpdbgdwarf.pas'     , 1944, $14FC5E5, '==2119==    by 0x14FC5E5: FPDBGDWARF$_$TFPDWARFVALUEPOINTER_$__$$_GETASSTRING$$ANSISTRING (fpdbgdwarf.pas:1944)'),
el('fppascalbuilder.pas',  508, $152F7FE, '==2119==    by 0x152F7FE: FPPASCALBUILDER$_$TFPPASCALPRETTYPRINTER_$_INTERNALPRINTVALUE$crc05F9855F_$$_DOPOINTER$BOOLEAN (fppascalbuilder.pas:508)'),
el('fppascalbuilder.pas',  886, $152C603, '==2119==    by 0x152C603: FPPASCALBUILDER$_$TFPPASCALPRETTYPRINTER_$__$$_INTERNALPRINTVALUE$crc05F9855F (fppascalbuilder.pas:886)'),
el('fppascalbuilder.pas',  743, $152DB9C, '==2119==    by 0x152DB9C: FPPASCALBUILDER$_$TFPPASCALPRETTYPRINTER_$_INTERNALPRINTVALUE$crc05F9855F_$$_DOSTRUCTURE (fppascalbuilder.pas:743)'),
el('fppascalbuilder.pas',  902, $152C693, '==2119==    by 0x152C693: FPPASCALBUILDER$_$TFPPASCALPRETTYPRINTER_$__$$_INTERNALPRINTVALUE$crc05F9855F (fppascalbuilder.pas:902)'),
el('fppascalbuilder.pas',  932, $152FD04, '==2119==    by 0x152FD04: FPPASCALBUILDER$_$TFPPASCALPRETTYPRINTER_$__$$_PRINTVALUE$crc19949757 (fppascalbuilder.pas:932)'),
el('fplldbdebugger.pas' , 1298, $1642298, '==2119==    by 0x1642298: FPLLDBDEBUGGER$_$TFPLLDBDEBUGGER_$__$$_EVALUATEEXPRESSION$crc5FF8C3B9 (fplldbdebugger.pas:1298)'),
el('fplldbdebugger.pas' , 1006, $1640D46, '==2119==    by 0x1640D46: FPLLDBDEBUGGER$_$TFPLLDBDEBUGGER_$__$$_REQUESTCOMMAND$TDBGCOMMAND$array_of_const$TMETHOD$$BOOLEAN (fplldbdebugger.pas:1006)'),
el(''                   ,    0, 0, '==2119==  Address 0x24985ff8 is 136 bytes inside a block of size 160 in arena "client"')
,el('',0,0,'==2119==')
    ], TrcList[1]);

  CheckTrace('T2', [
el('fplldbdebugger.pas' ,  453, $163D4E4, '==2119==    at 0x163D4E4: FPLLDBDEBUGGER$_$TFPLLDBDBGMEMREADER_$__$$_READMEMORY$TDBGPTR$LONGWORD$POINTER$$BOOLEAN (fplldbdebugger.pas:453)'),
el('fpdmemorytools.pas' ,  651, $15354EE, '==2119==    by 0x15354EE: FPDMEMORYTOOLS$_$TFPDBGMEMCACHESIMPLE_$__$$_READMEMORY$TDBGPTR$LONGWORD$POINTER$$BOOLEAN (fpdmemorytools.pas:651)'),
el('fpdmemorytools.pas' ,  726, $1535A24, '==2119==    by 0x1535A24: FPDMEMORYTOOLS$_$TFPDBGMEMCACHEMANAGERSIMPLE_$__$$_READMEMORY$TDBGPTR$LONGWORD$POINTER$$BOOLEAN (fpdmemorytools.pas:726)'),
el('fplldbdebugger.pas' ,  569, $163E02F, '==2119==    by 0x163E02F: FPLLDBDEBUGGER$_$TFPLLDBDBGMEMCACHEMANAGERSIMPLE_$__$$_READMEMORY$TDBGPTR$LONGWORD$POINTER$$BOOLEAN (fplldbdebugger.pas:569)'),
el('fpdmemorytools.pas' ,  785, $1535E6F, '==2119==    by 0x1535E6F: FPDMEMORYTOOLS$_$TFPDBGMEMMANAGER_$__$$_READMEMORY$crcAA80CA53 (fpdmemorytools.pas:785)'),
el('fpdmemorytools.pas' ,  967, $153744A, '==2119==    by 0x153744A: FPDMEMORYTOOLS$_$TFPDBGMEMMANAGER_$__$$_READADDRESS$crc594D75F5 (fpdmemorytools.pas:967)'),
el('fpdbgdwarf.pas'     , 2336, $14FF00E, '==2119==    by 0x14FF00E: FPDBGDWARF$_$TFPDWARFVALUESTRUCT_$__$$_GETDATAADDRESS$$TFPDBGMEMLOCATION (fpdbgdwarf.pas:2336)'),
el('fppascalbuilder.pas',  684, $152D29B, '==2119==    by 0x152D29B: FPPASCALBUILDER$_$TFPPASCALPRETTYPRINTER_$_INTERNALPRINTVALUE$crc05F9855F_$$_DOSTRUCTURE (fppascalbuilder.pas:684)'),
el('fppascalbuilder.pas',  900, $152C67F, '==2119==    by 0x152C67F: FPPASCALBUILDER$_$TFPPASCALPRETTYPRINTER_$__$$_INTERNALPRINTVALUE$crc05F9855F (fppascalbuilder.pas:900)'),
el('fppascalbuilder.pas',  743, $152DB9C, '==2119==    by 0x152DB9C: FPPASCALBUILDER$_$TFPPASCALPRETTYPRINTER_$_INTERNALPRINTVALUE$crc05F9855F_$$_DOSTRUCTURE (fppascalbuilder.pas:743)'),
el('fppascalbuilder.pas',  902, $152C693, '==2119==    by 0x152C693: FPPASCALBUILDER$_$TFPPASCALPRETTYPRINTER_$__$$_INTERNALPRINTVALUE$crc05F9855F (fppascalbuilder.pas:902)'),
el('fppascalbuilder.pas',  932, $152FD04, '==2119==    by 0x152FD04: FPPASCALBUILDER$_$TFPPASCALPRETTYPRINTER_$__$$_PRINTVALUE$crc19949757 (fppascalbuilder.pas:932)'),
el(''                   ,    0, 0, '==2119==  Address 0x231c4de8 is 24 bytes inside a block of size 32 free''d'),
el('vg_replace_malloc.c',  530, $4C2FDAC, '==2119==    at 0x4C2FDAC: free (vg_replace_malloc.c:530)'),
el('cmem.pp'            ,   75, $44F924, '==2119==    by 0x44F924: CMEM_$$_CFREEMEM$POINTER$$QWORD (cmem.pp:75)'),
el('heap.inc'           ,  316, $43C0A9, '==2119==    by 0x43C0A9: SYSTEM_$$_FREEMEM$POINTER$$QWORD (heap.inc:316)'),
el('dynarr.inc'         ,   77, $4344D2, '==2119==    by 0x4344D2: fpc_dynarray_clear (dynarr.inc:77)'),
el('rtti.inc'           ,  203, $439CDE, '==2119==    by 0x439CDE: fpc_finalize (rtti.inc:203)'),
el('rtti.inc'           ,  112, $439ADD, '==2119==    by 0x439ADD: SYSTEM_$$_RECORDRTTI$POINTER$POINTER$TRTTIPROC (rtti.inc:112)'),
el('objpas.inc'         ,  673, $436076, '==2119==    by 0x436076: SYSTEM$_$TOBJECT_$__$$_CLEANUPINSTANCE (objpas.inc:673)'),
el('objpas.inc'         ,  364, $435988, '==2119==    by 0x435988: SYSTEM$_$TOBJECT_$__$$_FREEINSTANCE (objpas.inc:364)'),
el('objpas.inc'         ,  281, $435757, '==2119==    by 0x435757: SYSTEM$_$TOBJECT_$__$$_DESTROY (objpas.inc:281)'),
el('objpas.inc'         ,  288, $43578E, '==2119==    by 0x43578E: SYSTEM$_$TOBJECT_$__$$_FREE (objpas.inc:288)'),
el('fpdmemorytools.pas' ,  751, $1535B34, '==2119==    by 0x1535B34: FPDMEMORYTOOLS$_$TFPDBGMEMCACHEMANAGERSIMPLE_$__$$_REMOVECACHE$TFPDBGMEMCACHEBASE (fpdmemorytools.pas:751)'),
el('fplldbdebugger.pas' ,  577, $163E116, '==2119==    by 0x163E116: FPLLDBDEBUGGER$_$TFPLLDBDBGMEMCACHEMANAGERSIMPLE_$__$$_CLEAR (fplldbdebugger.pas:577)'),
el(''                   ,    0, 0, '==2119==  Block was alloc''d at'),
el('vg_replace_malloc.c',  299, $4C2EBAB, '==2119==    at 0x4C2EBAB: malloc (vg_replace_malloc.c:299)'),
el('cmem.pp'            ,   62, $44F8D9, '==2119==    by 0x44F8D9: CMEM_$$_CGETMEM$QWORD$$POINTER (cmem.pp:62)'),
el('heap.inc'           ,  276, $43BF7D, '==2119==    by 0x43BF7D: SYSTEM_$$_GETMEM$POINTER$QWORD (heap.inc:276)'),
el('dynarr.inc'         ,  153, $434622, '==2119==    by 0x434622: fpc_dynarray_setlength (dynarr.inc:153)'),
el('fpdmemorytools.pas' ,  650, $1535485, '==2119==    by 0x1535485: FPDMEMORYTOOLS$_$TFPDBGMEMCACHESIMPLE_$__$$_READMEMORY$TDBGPTR$LONGWORD$POINTER$$BOOLEAN (fpdmemorytools.pas:650)'),
el('fpdmemorytools.pas' ,  726, $1535A24, '==2119==    by 0x1535A24: FPDMEMORYTOOLS$_$TFPDBGMEMCACHEMANAGERSIMPLE_$__$$_READMEMORY$TDBGPTR$LONGWORD$POINTER$$BOOLEAN (fpdmemorytools.pas:726)'),
el('fplldbdebugger.pas' ,  569, $163E02F, '==2119==    by 0x163E02F: FPLLDBDEBUGGER$_$TFPLLDBDBGMEMCACHEMANAGERSIMPLE_$__$$_READMEMORY$TDBGPTR$LONGWORD$POINTER$$BOOLEAN (fplldbdebugger.pas:569)'),
el('fpdmemorytools.pas' ,  785, $1535E6F, '==2119==    by 0x1535E6F: FPDMEMORYTOOLS$_$TFPDBGMEMMANAGER_$__$$_READMEMORY$crcAA80CA53 (fpdmemorytools.pas:785)'),
el('fpdmemorytools.pas' ,  967, $153744A, '==2119==    by 0x153744A: FPDMEMORYTOOLS$_$TFPDBGMEMMANAGER_$__$$_READADDRESS$crc594D75F5 (fpdmemorytools.pas:967)'),
el('fpdbgdwarf.pas'     , 2336, $14FF00E, '==2119==    by 0x14FF00E: FPDBGDWARF$_$TFPDWARFVALUESTRUCT_$__$$_GETDATAADDRESS$$TFPDBGMEMLOCATION (fpdbgdwarf.pas:2336)'),
el('fppascalbuilder.pas',  684, $152D29B, '==2119==    by 0x152D29B: FPPASCALBUILDER$_$TFPPASCALPRETTYPRINTER_$_INTERNALPRINTVALUE$crc05F9855F_$$_DOSTRUCTURE (fppascalbuilder.pas:684)'),
el('fppascalbuilder.pas',  900, $152C67F, '==2119==    by 0x152C67F: FPPASCALBUILDER$_$TFPPASCALPRETTYPRINTER_$__$$_INTERNALPRINTVALUE$crc05F9855F (fppascalbuilder.pas:900)')
,el('',0,0,'==2119==')
    ], TrcList[2]);

  TrcList.Free;
end;



initialization

  RegisterTest(TTestLeakParser);
end.

