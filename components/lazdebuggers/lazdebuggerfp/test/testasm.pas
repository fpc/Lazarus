unit TestAsm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FpPascalBuilder, FpDbgDisasX86, FpDbgClasses, FpDbgLoader,
  FpDbgUtil, {$ifdef FORCE_LAZLOGGER_DUMMY} LazLoggerDummy {$else} LazLoggerBase {$endif}, TestOutputLogger, TestDbgTestSuites, fpcunit,
  testutils, testregistry, RegExpr;

type

  { TTestAssembler }

  TTestAssembler = class(TTestCase)
  published
    procedure TestDisAsm;
  end;

implementation

type
  TDummyProcess = class(TDbgProcess)
  public
    property NewMode: TFPDMode write SetMode;
  end;

procedure TTestAssembler.TestDisAsm;
var
  DisAss: TX86AsmDecoder;
  Process: TDummyProcess;


  procedure TestDis(AName: String; AData: String; AExp: String);
  var
    p : pointer  ;
    s, codeBytes, asmInstr : string;
    r: TRegExpr;
  begin
    s := AData + #90#90#90#90#90#90#90#90#90#90#90#90#90#90#90#90;
    p := @s[1];
    DisAss.Disassemble(p, codeBytes, asmInstr);



debugln([AName, ' // ',
codeBytes, '  ', asmInstr,
'  ', p-@s[1] ,' == ',Length(AData)
]);

    AssertEquals(AName+' Cnt bytes', Length(AData), p-@s[1]);

    r := TRegExpr.Create('(\$)0+([0-9a-fA-F])');

    s := Trim(LowerCase(asmInstr));
    s := StringReplace(s, '  ', ' ', [rfReplaceAll]);  // space
    s := StringReplace(s, ',  ', ',', [rfReplaceAll]); // space
    s := r.Replace(s, '$1$2', True);

    AExp := StringReplace(AExp, '  ', ' ', [rfReplaceAll]);  // space
    AExp := StringReplace(AExp, ',  ', ',', [rfReplaceAll]); // space
    AExp := r.Replace(AExp, '$1$2', True);
    AExp := LowerCase(AExp);
    r.Free;
    AssertEquals(AName+' asm ', AExp, s);


  end;

begin
  Process := TDummyProcess.Create('', nil, nil, nil);
  Process.NewMode := dm64;
  DisAss := TX86AsmDecoder.Create(Process);
  try

  TestDis('callq  0x7ffbf0250860',       #$e8#$99#$da#$04#$00        ,  'call +$0004DA99');
  TestDis('callq  *0x6c1ec(%rip)',       #$ff#$15#$ec#$c1#$06#$00    ,  'call qword ptr [rip+$0006C1EC]');
  TestDis('rex.W callq *0x724f2(%rip)',  #$48#$ff#$15#$f2#$24#$07#$00,  'call qword ptr [rip+$000724F2]');
  TestDis('callq  0x100001f70',          #$e8#$7a#$48#$dc#$ff        ,  'call -$0023B786');
  TestDis('callq  *0x100(%rbx)',         #$ff#$93#$00#$01#$00#$00    ,  'call qword ptr [rbx+$00000100]');
  TestDis('callq  *(%rax)',              #$ff#$10                    ,  'call qword ptr [rax]');

  TestDis('add al,$05',                  #$04#$05,                             'add al,$05');
  TestDis('add ah,$05',                  #$80#$c4#$05,                         'add ah,$05');
  TestDis('add ax,$05',                  #$66#$05#$05#$00,                     'add ax,$05');
  TestDis('add eax,$05',                 #$83#$c0#$05,                         'add eax,$05');
  TestDis('add rax,$05',                 #$48#$83#$c0#$05,                     'add rax,$05');
  TestDis('add bl,$05',                  #$80#$c3#$05,                         'add bl,$05');
  TestDis('add bh,$05',                  #$80#$c7#$05,                         'add bh,$05');
  TestDis('add bx,$05',                  #$66#$81#$c3#$05#$00,                 'add bx,$05');
  TestDis('add ebx,$05',                 #$83#$c3#$05,                         'add ebx,$05');
  TestDis('add rbx,$05',                 #$48#$83#$c3#$05,                     'add rbx,$05');
  TestDis('add al,al',                   #$00#$c0,                             'add al,al');
  TestDis('add ah,al',                   #$00#$c4,                             'add ah,al');
  TestDis('add ah,ah',                   #$00#$e4,                             'add ah,ah');
  TestDis('add ax,ax',                   #$66#$01#$c0,                         'add ax,ax');
  TestDis('add eax,eax',                 #$01#$c0,                             'add eax,eax');
  TestDis('add rax,rax',                 #$48#$01#$c0,                         'add rax,rax');
  TestDis('add al,bl',                   #$00#$d8,                             'add al,bl');
  TestDis('add ah,bl',                   #$00#$dc,                             'add ah,bl');
  TestDis('add ah,bh',                   #$00#$fc,                             'add ah,bh');
  TestDis('add ax,bx',                   #$66#$01#$d8,                         'add ax,bx');
  TestDis('add eax,ebx',                 #$01#$d8,                             'add eax,ebx');
  TestDis('add rax,rbx',                 #$48#$01#$d8,                         'add rax,rbx');
  TestDis('add bl,cl',                   #$00#$cb,                             'add bl,cl');
  TestDis('add bl,ch',                   #$00#$eb,                             'add bl,ch');
  TestDis('add bh,ch',                   #$00#$ef,                             'add bh,ch');
  TestDis('add bx,cx',                   #$66#$01#$cb,                         'add bx,cx');
  TestDis('add ebx,ecx',                 #$01#$cb,                             'add ebx,ecx');
  TestDis('add rbx,rcx',                 #$48#$01#$cb,                         'add rbx,rcx');
  TestDis('add al,[rdx]',                #$02#$02,                             'add al,[rdx]');
  TestDis('add ah,[rdx]',                #$02#$22,                             'add ah,[rdx]');
  TestDis('add ah,[rdx]',                #$02#$22,                             'add ah,[rdx]');
  TestDis('add ax,[rdx]',                #$66#$03#$02,                         'add ax,[rdx]');
  TestDis('add eax,[rdx]',               #$03#$02,                             'add eax,[rdx]');
  TestDis('add rax,[rdx]',               #$48#$03#$02,                         'add rax,[rdx]');
  TestDis('add al,[edx]',                #$67#$02#$02,                         'add al,[edx]');
  TestDis('add ah,[edx]',                #$67#$02#$22,                         'add ah,[edx]');
  TestDis('add ah,[edx]',                #$67#$02#$22,                         'add ah,[edx]');
  TestDis('add ax,[edx]',                #$67#$66#$03#$02,                     'add ax,[edx]');
  TestDis('add eax,[edx]',               #$67#$03#$02,                         'add eax,[edx]');
  TestDis('add rax,[edx]',               #$67#$48#$03#$02,                     'add rax,[edx]');
  TestDis('add al,[$ab1234]',            #$02#$04#$25#$34#$12#$ab#$00,         'add al,[$ab1234]');
  TestDis('add ah,[$ab1234]',            #$02#$24#$25#$34#$12#$ab#$00,         'add ah,[$ab1234]');
  TestDis('add ax,[$ab1234]',            #$66#$03#$04#$25#$34#$12#$ab#$00,     'add ax,[$ab1234]');
  TestDis('add eax,[$ab1234]',           #$03#$04#$25#$34#$12#$ab#$00,         'add eax,[$ab1234]');
  TestDis('add rax,[$ab1234]',           #$48#$03#$04#$25#$34#$12#$ab#$00,     'add rax,[$ab1234]');
  TestDis('add al,[rdx+$12345678]',      #$02#$82#$78#$56#$34#$12,             'add al,[rdx+$12345678]');
  TestDis('add ah,[rdx+$12345678]',      #$02#$a2#$78#$56#$34#$12,             'add ah,[rdx+$12345678]');
  TestDis('add ah,[rdx+$12345678]',      #$02#$a2#$78#$56#$34#$12,             'add ah,[rdx+$12345678]');
  TestDis('add ax,[rdx+$12345678]',      #$66#$03#$82#$78#$56#$34#$12,         'add ax,[rdx+$12345678]');
  TestDis('add eax,[rdx+$12345678]',     #$03#$82#$78#$56#$34#$12,             'add eax,[rdx+$12345678]');
  TestDis('add rax,[rdx+$12345678]',     #$48#$03#$82#$78#$56#$34#$12,         'add rax,[rdx+$12345678]');
  TestDis('add al,[edx+$12345678]',      #$67#$02#$82#$78#$56#$34#$12,         'add al,[edx+$12345678]');
  TestDis('add ah,[edx+$12345678]',      #$67#$02#$a2#$78#$56#$34#$12,         'add ah,[edx+$12345678]');
  TestDis('add ah,[edx+$12345678]',      #$67#$02#$a2#$78#$56#$34#$12,         'add ah,[edx+$12345678]');
  TestDis('add ax,[edx+$12345678]',      #$67#$66#$03#$82#$78#$56#$34#$12,     'add ax,[edx+$12345678]');
  TestDis('add eax,[edx+$12345678]',     #$67#$03#$82#$78#$56#$34#$12,         'add eax,[edx+$12345678]');
  TestDis('add rax,[edx+$12345678]',     #$67#$48#$03#$82#$78#$56#$34#$12,     'add rax,[edx+$12345678]');
  TestDis('add al,[rdx+rdx]',            #$02#$04#$12,                         'add al,[rdx+rdx]');
  TestDis('add ah,[rdx+rdx]',            #$02#$24#$12,                         'add ah,[rdx+rdx]');
  TestDis('add ah,[rdx+rdx]',            #$02#$24#$12,                         'add ah,[rdx+rdx]');
  TestDis('add ax,[rdx+rdx]',            #$66#$03#$04#$12,                     'add ax,[rdx+rdx]');
  TestDis('add eax,[rdx+rdx]',           #$03#$04#$12,                         'add eax,[rdx+rdx]');
  TestDis('add rax,[rdx+rdx]',           #$48#$03#$04#$12,                     'add rax,[rdx+rdx]');
  TestDis('add al,[edx+edx]',            #$67#$02#$04#$12,                     'add al,[edx+edx]');
  TestDis('add ah,[edx+edx]',            #$67#$02#$24#$12,                     'add ah,[edx+edx]');
  TestDis('add ah,[edx+edx]',            #$67#$02#$24#$12,                     'add ah,[edx+edx]');
  TestDis('add ax,[edx+edx]',            #$67#$66#$03#$04#$12,                 'add ax,[edx+edx]');
  TestDis('add eax,[edx+edx]',           #$67#$03#$04#$12,                     'add eax,[edx+edx]');
  TestDis('add rax,[edx+edx]',           #$67#$48#$03#$04#$12,                 'add rax,[edx+edx]');
  TestDis('add al,[rsi*4+rdx]',          #$02#$04#$b2,                         'add al,[rsi*4+rdx]');
  TestDis('add ah,[rsi*4+rdx]',          #$02#$24#$b2,                         'add ah,[rsi*4+rdx]');
  TestDis('add ah,[rsi*4+rdx]',          #$02#$24#$b2,                         'add ah,[rsi*4+rdx]');
  TestDis('add ax,[rsi*4+rdx]',          #$66#$03#$04#$b2,                     'add ax,[rsi*4+rdx]');
  TestDis('add eax,[rsi*4+rdx]',         #$03#$04#$b2,                         'add eax,[rsi*4+rdx]');
  TestDis('add rax,[rsi*4+rdx]',         #$48#$03#$04#$b2,                     'add rax,[rsi*4+rdx]');
  TestDis('add al,[esi*4+edx]',          #$67#$02#$04#$b2,                     'add al,[esi*4+edx]');
  TestDis('add ah,[esi*4+edx]',          #$67#$02#$24#$b2,                     'add ah,[esi*4+edx]');
  TestDis('add ah,[esi*4+edx]',          #$67#$02#$24#$b2,                     'add ah,[esi*4+edx]');
  TestDis('add ax,[esi*4+edx]',          #$67#$66#$03#$04#$b2,                 'add ax,[esi*4+edx]');
  TestDis('add eax,[esi*4+edx]',         #$67#$03#$04#$b2,                     'add eax,[esi*4+edx]');
  TestDis('add rax,[esi*4+edx]',         #$67#$48#$03#$04#$b2,                 'add rax,[esi*4+edx]');
  TestDis('add al,[esi*4+edx+$123456]',  #$67#$02#$84#$b2#$56#$34#$12#$00,     'add al,[esi*4+edx+$123456]');
  TestDis('add ah,[esi*4+edx+$123456]',  #$67#$02#$a4#$b2#$56#$34#$12#$00,     'add ah,[esi*4+edx+$123456]');
  TestDis('add ah,[esi*4+edx+$123456]',  #$67#$02#$a4#$b2#$56#$34#$12#$00,     'add ah,[esi*4+edx+$123456]');
  TestDis('add ax,[esi*4+edx+$123456]',  #$67#$66#$03#$84#$b2#$56#$34#$12#$00, 'add ax,[esi*4+edx+$123456]');
  TestDis('add eax,[esi*4+edx+$123456]', #$67#$03#$84#$b2#$56#$34#$12#$00,     'add eax,[esi*4+edx+$123456]');
  TestDis('add rax,[esi*4+edx+$123456]', #$67#$48#$03#$84#$b2#$56#$34#$12#$00, 'add rax,[esi*4+edx+$123456]');

  TestDis('add [rdx],al',                #$00#$02,                             'add [rdx],al');
  TestDis('add [rdx],ah',                #$00#$22,                             'add [rdx],ah');
  TestDis('add [rdx],ah',                #$00#$22,                             'add [rdx],ah');
  TestDis('add [rdx],ax',                #$66#$01#$02,                         'add [rdx],ax');
  TestDis('add [rdx],eax',               #$01#$02,                             'add [rdx],eax');
  TestDis('add [rdx],rax',               #$48#$01#$02,                         'add [rdx],rax');
  TestDis('add [edx],al',                #$67#$00#$02,                         'add [edx],al');
  TestDis('add [edx],ah',                #$67#$00#$22,                         'add [edx],ah');
  TestDis('add [edx],ah',                #$67#$00#$22,                         'add [edx],ah');
  TestDis('add [edx],ax',                #$67#$66#$01#$02,                     'add [edx],ax');
  TestDis('add [edx],eax',               #$67#$01#$02,                         'add [edx],eax');
  TestDis('add [edx],rax',               #$67#$48#$01#$02,                     'add [edx],rax');
  TestDis('add [$ab1234],al',            #$00#$04#$25#$34#$12#$ab#$00,         'add [$ab1234],al');
  TestDis('add [$ab1234],ah',            #$00#$24#$25#$34#$12#$ab#$00,         'add [$ab1234],ah');
  TestDis('add [$ab1234],ax',            #$66#$01#$04#$25#$34#$12#$ab#$00,     'add [$ab1234],ax');
  TestDis('add [$ab1234],eax',           #$01#$04#$25#$34#$12#$ab#$00,         'add [$ab1234],eax');
  TestDis('add [$ab1234],rax',           #$48#$01#$04#$25#$34#$12#$ab#$00,     'add [$ab1234],rax');
  TestDis('add [rdx+$12345678],al',      #$00#$82#$78#$56#$34#$12,             'add [rdx+$12345678],al');
  TestDis('add [rdx+$12345678],ah',      #$00#$a2#$78#$56#$34#$12,             'add [rdx+$12345678],ah');
  TestDis('add [rdx+$12345678],ah',      #$00#$a2#$78#$56#$34#$12,             'add [rdx+$12345678],ah');
  TestDis('add [rdx+$12345678],ax',      #$66#$01#$82#$78#$56#$34#$12,         'add [rdx+$12345678],ax');
  TestDis('add [rdx+$12345678],eax',     #$01#$82#$78#$56#$34#$12,             'add [rdx+$12345678],eax');
  TestDis('add [rdx+$12345678],rax',     #$48#$01#$82#$78#$56#$34#$12,         'add [rdx+$12345678],rax');
  TestDis('add [edx+$12345678],al',      #$67#$00#$82#$78#$56#$34#$12,         'add [edx+$12345678],al');
  TestDis('add [edx+$12345678],ah',      #$67#$00#$a2#$78#$56#$34#$12,         'add [edx+$12345678],ah');
  TestDis('add [edx+$12345678],ah',      #$67#$00#$a2#$78#$56#$34#$12,         'add [edx+$12345678],ah');
  TestDis('add [edx+$12345678],ax',      #$67#$66#$01#$82#$78#$56#$34#$12,     'add [edx+$12345678],ax');
  TestDis('add [edx+$12345678],eax',     #$67#$01#$82#$78#$56#$34#$12,         'add [edx+$12345678],eax');
  TestDis('add [edx+$12345678],rax',     #$67#$48#$01#$82#$78#$56#$34#$12,     'add [edx+$12345678],rax');
  TestDis('add [rdx+rdx],al',            #$00#$04#$12,                         'add [rdx+rdx],al');
  TestDis('add [rdx+rdx],ah',            #$00#$24#$12,                         'add [rdx+rdx],ah');
  TestDis('add [rdx+rdx],ah',            #$00#$24#$12,                         'add [rdx+rdx],ah');
  TestDis('add [rdx+rdx],ax',            #$66#$01#$04#$12,                     'add [rdx+rdx],ax');
  TestDis('add [rdx+rdx],eax',           #$01#$04#$12,                         'add [rdx+rdx],eax');
  TestDis('add [rdx+rdx],rax',           #$48#$01#$04#$12,                     'add [rdx+rdx],rax');
  TestDis('add [edx+edx],al',            #$67#$00#$04#$12,                     'add [edx+edx],al');
  TestDis('add [edx+edx],ah',            #$67#$00#$24#$12,                     'add [edx+edx],ah');
  TestDis('add [edx+edx],ah',            #$67#$00#$24#$12,                     'add [edx+edx],ah');
  TestDis('add [edx+edx],ax',            #$67#$66#$01#$04#$12,                 'add [edx+edx],ax');
  TestDis('add [edx+edx],eax',           #$67#$01#$04#$12,                     'add [edx+edx],eax');
  TestDis('add [edx+edx],rax',           #$67#$48#$01#$04#$12,                 'add [edx+edx],rax');
  TestDis('add [rsi*4+rdx],al',          #$00#$04#$b2,                         'add [rsi*4+rdx],al');
  TestDis('add [rsi*4+rdx],ah',          #$00#$24#$b2,                         'add [rsi*4+rdx],ah');
  TestDis('add [rsi*4+rdx],ah',          #$00#$24#$b2,                         'add [rsi*4+rdx],ah');
  TestDis('add [rsi*4+rdx],ax',          #$66#$01#$04#$b2,                     'add [rsi*4+rdx],ax');
  TestDis('add [rsi*4+rdx],eax',         #$01#$04#$b2,                         'add [rsi*4+rdx],eax');
  TestDis('add [rsi*4+rdx],rax',         #$48#$01#$04#$b2,                     'add [rsi*4+rdx],rax');
  TestDis('add [esi*4+edx],al',          #$67#$00#$04#$b2,                     'add [esi*4+edx],al');
  TestDis('add [esi*4+edx],ah',          #$67#$00#$24#$b2,                     'add [esi*4+edx],ah');
  TestDis('add [esi*4+edx],ah',          #$67#$00#$24#$b2,                     'add [esi*4+edx],ah');
  TestDis('add [esi*4+edx],ax',          #$67#$66#$01#$04#$b2,                 'add [esi*4+edx],ax');
  TestDis('add [esi*4+edx],eax',         #$67#$01#$04#$b2,                     'add [esi*4+edx],eax');
  TestDis('add [esi*4+edx],rax',         #$67#$48#$01#$04#$b2,                 'add [esi*4+edx],rax');
  TestDis('add [rsi*4+rdx+$123456],al',  #$00#$84#$b2#$56#$34#$12#$00,         'add [rsi*4+rdx+$123456],al');
  TestDis('add [rsi*4+rdx+$123456],ah',  #$00#$a4#$b2#$56#$34#$12#$00,         'add [rsi*4+rdx+$123456],ah');
  TestDis('add [rsi*4+rdx+$123456],ax',  #$66#$01#$84#$b2#$56#$34#$12#$00,     'add [rsi*4+rdx+$123456],ax');
  TestDis('add [rsi*4+rdx+$123456],eax', #$01#$84#$b2#$56#$34#$12#$00,         'add [rsi*4+rdx+$123456],eax');
  TestDis('add [rsi*4+rdx+$123456],rax', #$48#$01#$84#$b2#$56#$34#$12#$00,     'add [rsi*4+rdx+$123456],rax');
  TestDis('add [esi*4+edx+$123456],al',  #$67#$00#$84#$b2#$56#$34#$12#$00,     'add [esi*4+edx+$123456],al');
  TestDis('add [esi*4+edx+$123456],ah',  #$67#$00#$a4#$b2#$56#$34#$12#$00,     'add [esi*4+edx+$123456],ah');
  TestDis('add [esi*4+edx+$123456],ax',  #$67#$66#$01#$84#$b2#$56#$34#$12#$00, 'add [esi*4+edx+$123456],ax');
  TestDis('add [esi*4+edx+$123456],eax', #$67#$01#$84#$b2#$56#$34#$12#$00,     'add [esi*4+edx+$123456],eax');
  TestDis('add [esi*4+edx+$123456],rax', #$67#$48#$01#$84#$b2#$56#$34#$12#$00, 'add [esi*4+edx+$123456],rax');

  TestDis('xadd [rdi],ax',                #$66#$0f#$c1#$07,                    'xadd [rdi],ax');
  TestDis('lock xadd [rdi],ax',           #$f0#$66#$0f#$c1#$07,                'lock xadd [rdi],ax');

  TestDis('mov [$0000001a],al ',   #$a2#$1a#$00#$00#$00#$00#$00#$00#$00,         'mov [$0000001a],al');
  TestDis('mov [$0000001a],ax ',   #$66#$a3#$1a#$00#$00#$00#$00#$00#$00#$00,     'mov [$0000001a],ax');
  TestDis('mov [$0000001a],eax',   #$a3#$1a#$00#$00#$00#$00#$00#$00#$00,         'mov [$0000001a],eax');
  TestDis('mov [$0000001a],rax',   #$48#$a3#$1a#$00#$00#$00#$00#$00#$00#$00,     'mov [$0000001a],rax');

  TestDis('vmovmskpd ecx,xmm1',     #$C5#$F9#$50#$C9,             'vmovmskpd ecx,xmm1');
  TestDis('vmovmskpd ecx,ymm1',     #$C5#$FD#$50#$C9,             'vmovmskpd ecx,ymm1');
  TestDis('vmovmskps ecx,xmm1',     #$C5#$F8#$50#$C9,             'vmovmskps ecx,xmm1');
  TestDis('vmovmskps ecx,ymm1',     #$C5#$FC#$50#$C9,             'vmovmskps ecx,ymm1');
  TestDis('vroundsd xmm1,xmm2,xmm3,$07', #$C4#$E3#$69#$0B#$CB#$07,     'vroundsd xmm1,xmm2,xmm3,$07');
  TestDis('vroundss xmm1,xmm2,xmm3,$07', #$C4#$E3#$69#$0A#$CB#$07,     'vroundss xmm1,xmm2,xmm3,$07');


  TestDis('vmovss xmm1,dword ptr [rsi]', #$C5#$FA#$10#$0E,                 'vmovss xmm1,dword ptr [rsi]');
  TestDis('vmovss [rsi],xmm1',           #$C5#$FA#$11#$0E,                 'vmovss [rsi],xmm1');
  TestDis('vmovsd xmm1,qword ptr [rsi]', #$C5#$FB#$10#$0E,                 'vmovsd xmm1,qword ptr [rsi]');
  TestDis('vmovsd [rsi],xmm1',           #$C5#$FB#$11#$0E,                 'vmovsd [rsi],xmm1');
  TestDis('vmovss xmm1,xmm2,xmm3',       #$C5#$EA#$10#$CB,                 'vmovss xmm1,xmm2,xmm3');
  TestDis('vmovss xmm1,xmm2,xmm3',       #$C5#$EA#$10#$CB,                 'vmovss xmm1,xmm2,xmm3');
  TestDis('vmovsd xmm1,xmm2,xmm3',       #$C5#$EB#$10#$CB,                 'vmovsd xmm1,xmm2,xmm3');
  TestDis('vmovsd xmm1,xmm2,xmm3',       #$C5#$EB#$10#$CB,                 'vmovsd xmm1,xmm2,xmm3');




  TestDis('movd mm1,edi',     #$0F#$6E#$CF,             'movd mm1,edi');
  TestDis('movq mm1,rdi',     #$48#$0F#$6E#$CF,         'movq mm1,rdi');
  TestDis('movd edi,mm1',     #$0F#$7E#$CF,             'movd edi,mm1');
  TestDis('movq rdi,mm1',     #$48#$0F#$7E#$CF,         'movq rdi,mm1');
  TestDis('movd xmm1,edi',    #$66#$0F#$6E#$CF,         'movd xmm1,edi');
  TestDis('movq xmm1,rdi',    #$66#$48#$0F#$6E#$CF,     'movq xmm1,rdi');
  TestDis('movd edi,xmm1',    #$66#$0F#$7E#$CF,         'movd edi,xmm1');
  TestDis('movq rdi,xmm1',    #$66#$48#$0F#$7E#$CF,     'movq rdi,xmm1');
  TestDis('vmovd [rax],xmm0', #$C4#$E1#$79#$7E#$00,     'vmovd [rax],xmm0');
  TestDis('vmovq [rax],xmm0', #$C4#$E1#$F9#$7E#$00,     'vmovq [rax],xmm0');

  TestDis('test rax,$FFFFFEFE', #$48#$A9#$FE#$FE#$FF#$FF,       'test rax,$FFFFFEFE');
  TestDis('test eax,$FFFFFEFE', #$A9#$FE#$FE#$FF#$FF,           'test eax,$FFFFFEFE');
  TestDis('vzeroupper',         #$C5#$F8#$77,                   'vzeroupper');
  TestDis('vzeroall',           #$C5#$FC#$77,                   'vzeroall');


  TestDis('pslldq xmm0,$07',           #$66#$0F#$73#$F8#$07,               'pslldq xmm0,$07');
  TestDis('pslldq xmm1,$09',           #$66#$0F#$73#$F9#$09,               'pslldq xmm1,$09');
  TestDis('pslldq xmm2,$81',           #$66#$0F#$73#$FA#$81,               'pslldq xmm2,$81');
  TestDis('vpslldq xmm1,xmm2,$07',     #$C5#$F1#$73#$FA#$07,               'vpslldq xmm1,xmm2,$07');
  TestDis('vpslldq xmm0,xmm3,$0B',     #$C5#$F9#$73#$FB#$0B,               'vpslldq xmm0,xmm3,$0B');
  TestDis('vpslldq ymm1,ymm2,$07',     #$C5#$F5#$73#$FA#$07,               'vpslldq ymm1,ymm2,$07');
  TestDis('vpslldq ymm2,ymm0,$01',     #$C5#$ED#$73#$F8#$01,               'vpslldq ymm2,ymm0,$01');
  TestDis('psrldq xmm0,$07',           #$66#$0F#$73#$D8#$07,               'psrldq xmm0,$07');
  TestDis('psrldq xmm1,$09',           #$66#$0F#$73#$D9#$09,               'psrldq xmm1,$09');
  TestDis('psrldq xmm2,$81',           #$66#$0F#$73#$DA#$81,               'psrldq xmm2,$81');
  TestDis('vpsrldq xmm1,xmm2,$07',     #$C5#$F1#$73#$DA#$07,               'vpsrldq xmm1,xmm2,$07');
  TestDis('vpsrldq xmm0,xmm3,$0B',     #$C5#$F9#$73#$DB#$0B,               'vpsrldq xmm0,xmm3,$0B');
  TestDis('vpsrldq ymm1,ymm2,$07',     #$C5#$F5#$73#$DA#$07,               'vpsrldq ymm1,ymm2,$07');
  TestDis('vpsrldq ymm2,ymm0,$01',     #$C5#$ED#$73#$D8#$01,               'vpsrldq ymm2,ymm0,$01');
  TestDis('psllw mm1,[rsi]',           #$0F#$F1#$0E,                     'psllw mm1,[rsi]');
  TestDis('psllw xmm0,[rsi]',          #$66#$0F#$F1#$06,                 'psllw xmm0,[rsi]');
  TestDis('psllw xmm1,xmm2',           #$66#$0F#$F1#$CA,                 'psllw xmm1,xmm2');
  TestDis('psllw xmm2,xmm0',           #$66#$0F#$F1#$D0,                 'psllw xmm2,xmm0');
  TestDis('psllw mm1,$07',             #$0F#$71#$F1#$07,                 'psllw mm1,$07');
  TestDis('psllw xmm1,$07',            #$66#$0F#$71#$F1#$07,             'psllw xmm1,$07');
  TestDis('pslld mm1,[rsi]',           #$0F#$F2#$0E,                     'pslld mm1,[rsi]');
  TestDis('pslld xmm0,[rsi]',          #$66#$0F#$F2#$06,                 'pslld xmm0,[rsi]');
  TestDis('pslld xmm1,xmm2',           #$66#$0F#$F2#$CA,                 'pslld xmm1,xmm2');
  TestDis('pslld xmm2,xmm0',           #$66#$0F#$F2#$D0,                 'pslld xmm2,xmm0');
  TestDis('pslld mm1,$07',             #$0F#$72#$F1#$07,                 'pslld mm1,$07');
  TestDis('pslld xmm1,$07',            #$66#$0F#$72#$F1#$07,             'pslld xmm1,$07');
  TestDis('psllq mm1,[rsi]',           #$0F#$F3#$0E,                     'psllq mm1,[rsi]');
  TestDis('psllq xmm0,[rsi]',          #$66#$0F#$F3#$06,                 'psllq xmm0,[rsi]');
  TestDis('psllq xmm1,xmm2',           #$66#$0F#$F3#$CA,                 'psllq xmm1,xmm2');
  TestDis('psllq xmm2,xmm0',           #$66#$0F#$F3#$D0,                 'psllq xmm2,xmm0');
  TestDis('psllq mm1,$07',             #$0F#$73#$F1#$07,                 'psllq mm1,$07');
  TestDis('psllq xmm1,$07',            #$66#$0F#$73#$F1#$07,             'psllq xmm1,$07');
  TestDis('psrlw mm1,[rsi]',           #$0F#$D1#$0E,                     'psrlw mm1,[rsi]');
  TestDis('psrlw xmm0,[rsi]',          #$66#$0F#$D1#$06,                 'psrlw xmm0,[rsi]');
  TestDis('psrlw xmm1,xmm2',           #$66#$0F#$D1#$CA,                 'psrlw xmm1,xmm2');
  TestDis('psrlw xmm2,xmm0',           #$66#$0F#$D1#$D0,                 'psrlw xmm2,xmm0');
  TestDis('psrlw mm1,$07',             #$0F#$71#$D1#$07,                 'psrlw mm1,$07');
  TestDis('psrlw xmm1,$07',            #$66#$0F#$71#$D1#$07,             'psrlw xmm1,$07');
  TestDis('psrld mm1,[rsi]',           #$0F#$D2#$0E,                     'psrld mm1,[rsi]');
  TestDis('psrld xmm0,[rsi]',          #$66#$0F#$D2#$06,                 'psrld xmm0,[rsi]');
  TestDis('psrld xmm1,xmm2',           #$66#$0F#$D2#$CA,                 'psrld xmm1,xmm2');
  TestDis('psrld xmm2,xmm0',           #$66#$0F#$D2#$D0,                 'psrld xmm2,xmm0');
  TestDis('psrld mm1,$07',             #$0F#$72#$D1#$07,                 'psrld mm1,$07');
  TestDis('psrld xmm1,$07',            #$66#$0F#$72#$D1#$07,             'psrld xmm1,$07');
  TestDis('psrlq mm1,[rsi]',           #$0F#$D3#$0E,                     'psrlq mm1,[rsi]');
  TestDis('psrlq xmm0,[rsi]',          #$66#$0F#$D3#$06,                 'psrlq xmm0,[rsi]');
  TestDis('psrlq xmm1,xmm2',           #$66#$0F#$D3#$CA,                 'psrlq xmm1,xmm2');
  TestDis('psrlq xmm2,xmm0',           #$66#$0F#$D3#$D0,                 'psrlq xmm2,xmm0');
  TestDis('psrlq mm1,$07',             #$0F#$73#$D1#$07,                 'psrlq mm1,$07');
  TestDis('psrlq xmm1,$07',            #$66#$0F#$73#$D1#$07,             'psrlq xmm1,$07');
  TestDis('extrq xmm0,$03,$04',        #$66#$0F#$78#$C0#$03#$04,         'extrq xmm0,$03,$04');
  TestDis('extrq xmm1,$03,$04',        #$66#$0F#$78#$C1#$03#$04,         'extrq xmm1,$03,$04');
  TestDis('extrq xmm1,xmm2',           #$66#$0F#$79#$CA,                 'extrq xmm1,xmm2');
  TestDis('insertq xmm1,xmm1,$03,$04', #$F2#$0F#$78#$C9#$03#$04,         'insertq xmm1,xmm1,$03,$04');
  TestDis('insertq xmm1,xmm1',         #$F2#$0F#$79#$C9,                 'insertq xmm1,xmm1');
  TestDis('insertq xmm1,xmm2,$03,$04', #$F2#$0F#$78#$CA#$03#$04,         'insertq xmm1,xmm2,$03,$04');
  TestDis('insertq xmm1,xmm2',         #$F2#$0F#$79#$CA,                 'insertq xmm1,xmm2');
  TestDis('movntsd [rsi],xmm2',        #$F2#$0F#$2B#$16,                 'movntsd [rsi],xmm2');
  TestDis('movntsd [rsi],xmm1',        #$F2#$0F#$2B#$0E,                 'movntsd [rsi],xmm1');
  TestDis('movntss [rsi],xmm2',        #$F3#$0F#$2B#$16,                 'movntss [rsi],xmm2');
  TestDis('movntss [rsi],xmm1',        #$F3#$0F#$2B#$0E,                 'movntss [rsi],xmm1');
  TestDis('pause',                     #$F3#$90,                         'pause');




  Process.NewMode := dm32;

  TestDis('call   0x77cf4d10',           #$e8#$67#$70#$fc#$ff        ,  'call -$00038F99');
  TestDis('call   *0x7718202c',          #$ff#$15#$2c#$20#$18#$77    ,  'call dword ptr [$7718202C]');
  TestDis('call   *%esi',                #$ff#$d6                    ,  'call esi');
  TestDis('call   *0x80(%ebx)',          #$ff#$93#$80#$00#$00#$00    ,  'call dword ptr [ebx+$00000080]');
  TestDis('call   0x4301a0',             #$e8#$c2#$de#$ff#$ff        ,  'call -$0000213E');
  TestDis('call   *(%edx)',              #$ff#$12                    ,  'call dword ptr [edx]');
  TestDis('call   *0x7c(%ebx)',          #$ff#$53#$7c                ,  'call dword ptr [ebx+$7C]');

  TestDis('add al,$05',                  #$04#$05,                             'add al,$05');
  TestDis('add ah,$05',                  #$80#$c4#$05,                         'add ah,$05');
  TestDis('add ax,$05',                  #$66#$05#$05#$00,                     'add ax,$05');
  TestDis('add eax,$05',                 #$83#$c0#$05,                         'add eax,$05');
  TestDis('add bl,$05',                  #$80#$c3#$05,                         'add bl,$05');
  TestDis('add bh,$05',                  #$80#$c7#$05,                         'add bh,$05');
  TestDis('add bx,$05',                  #$66#$81#$c3#$05#$00,                 'add bx,$05');
  TestDis('add ebx,$05',                 #$83#$c3#$05,                         'add ebx,$05');
  TestDis('add al,al',                   #$00#$c0,                             'add al,al');
  TestDis('add ah,al',                   #$00#$c4,                             'add ah,al');
  TestDis('add ah,ah',                   #$00#$e4,                             'add ah,ah');
  TestDis('add ax,ax',                   #$66#$01#$c0,                         'add ax,ax');
  TestDis('add eax,eax',                 #$01#$c0,                             'add eax,eax');
  TestDis('add al,bl',                   #$00#$d8,                             'add al,bl');
  TestDis('add ah,bl',                   #$00#$dc,                             'add ah,bl');
  TestDis('add ah,bh',                   #$00#$fc,                             'add ah,bh');
  TestDis('add ax,bx',                   #$66#$01#$d8,                         'add ax,bx');
  TestDis('add eax,ebx',                 #$01#$d8,                             'add eax,ebx');
  TestDis('add bl,cl',                   #$00#$cb,                             'add bl,cl');
  TestDis('add bl,ch',                   #$00#$eb,                             'add bl,ch');
  TestDis('add bh,ch',                   #$00#$ef,                             'add bh,ch');
  TestDis('add bx,cx',                   #$66#$01#$cb,                         'add bx,cx');
  TestDis('add ebx,ecx',                 #$01#$cb,                             'add ebx,ecx');
  TestDis('add al,[edx]',                #$02#$02,                             'add al,[edx]');
  TestDis('add ah,[edx]',                #$02#$22,                             'add ah,[edx]');
  TestDis('add ah,[edx]',                #$02#$22,                             'add ah,[edx]');
  TestDis('add ax,[edx]',                #$66#$03#$02,                         'add ax,[edx]');
  TestDis('add eax,[edx]',               #$03#$02,                             'add eax,[edx]');
  TestDis('add al,[$ab1234]',            #$02#$05#$34#$12#$ab#$00,             'add al,[$ab1234]');
  TestDis('add ah,[$ab1234]',            #$02#$25#$34#$12#$ab#$00,             'add ah,[$ab1234]');
  TestDis('add ax,[$ab1234]',            #$66#$03#$05#$34#$12#$ab#$00,         'add ax,[$ab1234]');
  TestDis('add eax,[$ab1234]',           #$03#$05#$34#$12#$ab#$00,             'add eax,[$ab1234]');
  TestDis('add al,[edx+$12345678]',      #$02#$82#$78#$56#$34#$12,             'add al,[edx+$12345678]');
  TestDis('add ah,[edx+$12345678]',      #$02#$a2#$78#$56#$34#$12,             'add ah,[edx+$12345678]');
  TestDis('add ah,[edx+$12345678]',      #$02#$a2#$78#$56#$34#$12,             'add ah,[edx+$12345678]');
  TestDis('add ax,[edx+$12345678]',      #$66#$03#$82#$78#$56#$34#$12,         'add ax,[edx+$12345678]');
  TestDis('add eax,[edx+$12345678]',     #$03#$82#$78#$56#$34#$12,             'add eax,[edx+$12345678]');
  TestDis('add al,[edx+edx]',            #$02#$04#$12,                         'add al,[edx+edx]');
  TestDis('add ah,[edx+edx]',            #$02#$24#$12,                         'add ah,[edx+edx]');
  TestDis('add ah,[edx+edx]',            #$02#$24#$12,                         'add ah,[edx+edx]');
  TestDis('add ax,[edx+edx]',            #$66#$03#$04#$12,                     'add ax,[edx+edx]');
  TestDis('add eax,[edx+edx]',           #$03#$04#$12,                         'add eax,[edx+edx]');
  TestDis('add al,[esi*4+edx]',          #$02#$04#$b2,                         'add al,[esi*4+edx]');
  TestDis('add ah,[esi*4+edx]',          #$02#$24#$b2,                         'add ah,[esi*4+edx]');
  TestDis('add ah,[esi*4+edx]',          #$02#$24#$b2,                         'add ah,[esi*4+edx]');
  TestDis('add ax,[esi*4+edx]',          #$66#$03#$04#$b2,                     'add ax,[esi*4+edx]');
  TestDis('add eax,[esi*4+edx]',         #$03#$04#$b2,                         'add eax,[esi*4+edx]');
  TestDis('add al,[esi*4+edx+$123456]',  #$02#$84#$b2#$56#$34#$12#$00,         'add al,[esi*4+edx+$123456]');
  TestDis('add ah,[esi*4+edx+$123456]',  #$02#$a4#$b2#$56#$34#$12#$00,         'add ah,[esi*4+edx+$123456]');
  TestDis('add ah,[esi*4+edx+$123456]',  #$02#$a4#$b2#$56#$34#$12#$00,         'add ah,[esi*4+edx+$123456]');
  TestDis('add ax,[esi*4+edx+$123456]',  #$66#$03#$84#$b2#$56#$34#$12#$00,     'add ax,[esi*4+edx+$123456]');
  TestDis('add eax,[esi*4+edx+$123456]', #$03#$84#$b2#$56#$34#$12#$00,         'add eax,[esi*4+edx+$123456]');
  TestDis('add [edx],al',                #$00#$02,                             'add [edx],al');
  TestDis('add [edx],ah',                #$00#$22,                             'add [edx],ah');
  TestDis('add [edx],ah',                #$00#$22,                             'add [edx],ah');
  TestDis('add [edx],ax',                #$66#$01#$02,                         'add [edx],ax');
  TestDis('add [edx],eax',               #$01#$02,                             'add [edx],eax');
  TestDis('add [$ab1234],al',            #$00#$05#$34#$12#$ab#$00,             'add [$ab1234],al');
  TestDis('add [$ab1234],ah',            #$00#$25#$34#$12#$ab#$00,             'add [$ab1234],ah');
  TestDis('add [$ab1234],ax',            #$66#$01#$05#$34#$12#$ab#$00,         'add [$ab1234],ax');
  TestDis('add [$ab1234],eax',           #$01#$05#$34#$12#$ab#$00,             'add [$ab1234],eax');
  TestDis('add [edx+$12345678],al',      #$00#$82#$78#$56#$34#$12,             'add [edx+$12345678],al');
  TestDis('add [edx+$12345678],ah',      #$00#$a2#$78#$56#$34#$12,             'add [edx+$12345678],ah');
  TestDis('add [edx+$12345678],ah',      #$00#$a2#$78#$56#$34#$12,             'add [edx+$12345678],ah');
  TestDis('add [edx+$12345678],ax',      #$66#$01#$82#$78#$56#$34#$12,         'add [edx+$12345678],ax');
  TestDis('add [edx+$12345678],eax',     #$01#$82#$78#$56#$34#$12,             'add [edx+$12345678],eax');
  TestDis('add [edx+edx],al',            #$00#$04#$12,                         'add [edx+edx],al');
  TestDis('add [edx+edx],ah',            #$00#$24#$12,                         'add [edx+edx],ah');
  TestDis('add [edx+edx],ah',            #$00#$24#$12,                         'add [edx+edx],ah');
  TestDis('add [edx+edx],ax',            #$66#$01#$04#$12,                     'add [edx+edx],ax');
  TestDis('add [edx+edx],eax',           #$01#$04#$12,                         'add [edx+edx],eax');
  TestDis('add [esi*4+edx],al',          #$00#$04#$b2,                         'add [esi*4+edx],al');
  TestDis('add [esi*4+edx],ah',          #$00#$24#$b2,                         'add [esi*4+edx],ah');
  TestDis('add [esi*4+edx],ah',          #$00#$24#$b2,                         'add [esi*4+edx],ah');
  TestDis('add [esi*4+edx],ax',          #$66#$01#$04#$b2,                     'add [esi*4+edx],ax');
  TestDis('add [esi*4+edx],eax',         #$01#$04#$b2,                         'add [esi*4+edx],eax');
  TestDis('add [esi*4+edx+$123456],al',  #$00#$84#$b2#$56#$34#$12#$00,         'add [esi*4+edx+$123456],al');
  TestDis('add [esi*4+edx+$123456],ah',  #$00#$a4#$b2#$56#$34#$12#$00,         'add [esi*4+edx+$123456],ah');
  TestDis('add [esi*4+edx+$123456],ah',  #$00#$a4#$b2#$56#$34#$12#$00,         'add [esi*4+edx+$123456],ah');
  TestDis('add [esi*4+edx+$123456],ax',  #$66#$01#$84#$b2#$56#$34#$12#$00,     'add [esi*4+edx+$123456],ax');
  TestDis('add [esi*4+edx+$123456],eax', #$01#$84#$b2#$56#$34#$12#$00,         'add [esi*4+edx+$123456],eax');

  TestDis('xadd [edi],ax',                #$66#$0f#$c1#$07,                    'xadd [edi],ax');
  TestDis('lock xadd [edi],ax',           #$f0#$66#$0f#$c1#$07,                'lock xadd [edi],ax');

  TestDis('mov [$0000001a],al ',   #$a2#$1a#$00#$00#$00,         'mov [$0000001a],al');
  TestDis('mov [$0000001a],ax ',   #$66#$a3#$1a#$00#$00#$00,     'mov [$0000001a],ax');
  TestDis('mov [$0000001a],eax',   #$a3#$1a#$00#$00#$00,         'mov [$0000001a],eax');

  TestDis('vmovmskpd ecx,xmm1',     #$C5#$F9#$50#$C9,             'vmovmskpd ecx,xmm1');
  TestDis('vmovmskpd ecx,ymm1',     #$C5#$FD#$50#$C9,             'vmovmskpd ecx,ymm1');
  TestDis('vmovmskps ecx,xmm1',     #$C5#$F8#$50#$C9,             'vmovmskps ecx,xmm1');
  TestDis('vmovmskps ecx,ymm1',     #$C5#$FC#$50#$C9,             'vmovmskps ecx,ymm1');
  TestDis('vroundsd xmm1,xmm2,xmm3,$07', #$C4#$E3#$69#$0B#$CB#$07,     'vroundsd xmm1,xmm2,xmm3,$07');
  TestDis('vroundss xmm1,xmm2,xmm3,$07', #$C4#$E3#$69#$0A#$CB#$07,     'vroundss xmm1,xmm2,xmm3,$07');

  TestDis('vmovss xmm1,xmm2,xmm3',       #$C5#$EA#$10#$CB,                 'vmovss xmm1,xmm2,xmm3');
  TestDis('vmovss xmm1,xmm2,xmm3',       #$C5#$EA#$10#$CB,                 'vmovss xmm1,xmm2,xmm3');
  TestDis('vmovsd xmm1,xmm2,xmm3',       #$C5#$EB#$10#$CB,                 'vmovsd xmm1,xmm2,xmm3');
  TestDis('vmovsd xmm1,xmm2,xmm3',       #$C5#$EB#$10#$CB,                 'vmovsd xmm1,xmm2,xmm3');

  TestDis('movd mm1,edi',     #$0F#$6E#$CF,             'movd mm1,edi');
  TestDis('movd edi,mm1',     #$0F#$7E#$CF,             'movd edi,mm1');
  TestDis('movd xmm1,edi',    #$66#$0F#$6E#$CF,         'movd xmm1,edi');
  TestDis('movd edi,xmm1',    #$66#$0F#$7E#$CF,         'movd edi,xmm1');
  TestDis('vmovd [eax],xmm0', #$C4#$E1#$79#$7E#$00,     'vmovd [eax],xmm0');

  TestDis('test eax,$FFFFFEFE', #$A9#$FE#$FE#$FF#$FF,           'test eax,$FFFFFEFE');
  TestDis('vzeroupper',         #$C5#$F8#$77,                   'vzeroupper');
  TestDis('vzeroall',           #$C5#$FC#$77,                   'vzeroall');

  TestDis('pslldq xmm0,$07',           #$66#$0F#$73#$F8#$07,               'pslldq xmm0,$07');
  TestDis('pslldq xmm1,$09',           #$66#$0F#$73#$F9#$09,               'pslldq xmm1,$09');
  TestDis('pslldq xmm2,$81',           #$66#$0F#$73#$FA#$81,               'pslldq xmm2,$81');
  TestDis('vpslldq xmm1,xmm2,$07',     #$C5#$F1#$73#$FA#$07,               'vpslldq xmm1,xmm2,$07');
  TestDis('vpslldq xmm0,xmm3,$0B',     #$C5#$F9#$73#$FB#$0B,               'vpslldq xmm0,xmm3,$0B');
  TestDis('vpslldq ymm1,ymm2,$07',     #$C5#$F5#$73#$FA#$07,               'vpslldq ymm1,ymm2,$07');
  TestDis('vpslldq ymm2,ymm0,$01',     #$C5#$ED#$73#$F8#$01,               'vpslldq ymm2,ymm0,$01');
  TestDis('psrldq xmm0,$07',           #$66#$0F#$73#$D8#$07,               'psrldq xmm0,$07');
  TestDis('psrldq xmm1,$09',           #$66#$0F#$73#$D9#$09,               'psrldq xmm1,$09');
  TestDis('psrldq xmm2,$81',           #$66#$0F#$73#$DA#$81,               'psrldq xmm2,$81');
  TestDis('vpsrldq xmm1,xmm2,$07',     #$C5#$F1#$73#$DA#$07,               'vpsrldq xmm1,xmm2,$07');
  TestDis('vpsrldq xmm0,xmm3,$0B',     #$C5#$F9#$73#$DB#$0B,               'vpsrldq xmm0,xmm3,$0B');
  TestDis('vpsrldq ymm1,ymm2,$07',     #$C5#$F5#$73#$DA#$07,               'vpsrldq ymm1,ymm2,$07');
  TestDis('vpsrldq ymm2,ymm0,$01',     #$C5#$ED#$73#$D8#$01,               'vpsrldq ymm2,ymm0,$01');
  TestDis('psllw mm1,[esi]',           #$0F#$F1#$0E,                   'psllw mm1,[esi]');
  TestDis('psllw xmm0,[esi]',          #$66#$0F#$F1#$06,                 'psllw xmm0,[esi]');
  TestDis('psllw xmm1,xmm2',           #$66#$0F#$F1#$CA,                 'psllw xmm1,xmm2');
  TestDis('psllw xmm2,xmm0',           #$66#$0F#$F1#$D0,                 'psllw xmm2,xmm0');
  TestDis('psllw mm1,$07',             #$0F#$71#$F1#$07,                 'psllw mm1,$07');
  TestDis('psllw xmm1,$07',            #$66#$0F#$71#$F1#$07,               'psllw xmm1,$07');
  TestDis('pslld mm1,[esi]',           #$0F#$F2#$0E,                   'pslld mm1,[esi]');
  TestDis('pslld xmm0,[esi]',          #$66#$0F#$F2#$06,                 'pslld xmm0,[esi]');
  TestDis('pslld xmm1,xmm2',           #$66#$0F#$F2#$CA,                 'pslld xmm1,xmm2');
  TestDis('pslld xmm2,xmm0',           #$66#$0F#$F2#$D0,                 'pslld xmm2,xmm0');
  TestDis('pslld mm1,$07',             #$0F#$72#$F1#$07,                 'pslld mm1,$07');
  TestDis('pslld xmm1,$07',            #$66#$0F#$72#$F1#$07,               'pslld xmm1,$07');
  TestDis('psllq mm1,[esi]',           #$0F#$F3#$0E,                   'psllq mm1,[esi]');
  TestDis('psllq xmm0,[esi]',          #$66#$0F#$F3#$06,                 'psllq xmm0,[esi]');
  TestDis('psllq xmm1,xmm2',           #$66#$0F#$F3#$CA,                 'psllq xmm1,xmm2');
  TestDis('psllq xmm2,xmm0',           #$66#$0F#$F3#$D0,                 'psllq xmm2,xmm0');
  TestDis('psllq mm1,$07',             #$0F#$73#$F1#$07,                 'psllq mm1,$07');
  TestDis('psllq xmm1,$07',            #$66#$0F#$73#$F1#$07,               'psllq xmm1,$07');
  TestDis('psrlw mm1,[esi]',           #$0F#$D1#$0E,                   'psrlw mm1,[esi]');
  TestDis('psrlw xmm0,[esi]',          #$66#$0F#$D1#$06,                 'psrlw xmm0,[esi]');
  TestDis('psrlw xmm1,xmm2',           #$66#$0F#$D1#$CA,                 'psrlw xmm1,xmm2');
  TestDis('psrlw xmm2,xmm0',           #$66#$0F#$D1#$D0,                 'psrlw xmm2,xmm0');
  TestDis('psrlw mm1,$07',             #$0F#$71#$D1#$07,                 'psrlw mm1,$07');
  TestDis('psrlw xmm1,$07',            #$66#$0F#$71#$D1#$07,               'psrlw xmm1,$07');
  TestDis('psrld mm1,[esi]',           #$0F#$D2#$0E,                   'psrld mm1,[esi]');
  TestDis('psrld xmm0,[esi]',          #$66#$0F#$D2#$06,                 'psrld xmm0,[esi]');
  TestDis('psrld xmm1,xmm2',           #$66#$0F#$D2#$CA,                 'psrld xmm1,xmm2');
  TestDis('psrld xmm2,xmm0',           #$66#$0F#$D2#$D0,                 'psrld xmm2,xmm0');
  TestDis('psrld mm1,$07',             #$0F#$72#$D1#$07,                 'psrld mm1,$07');
  TestDis('psrld xmm1,$07',            #$66#$0F#$72#$D1#$07,               'psrld xmm1,$07');
  TestDis('psrlq mm1,[esi]',           #$0F#$D3#$0E,                   'psrlq mm1,[esi]');
  TestDis('psrlq xmm0,[esi]',          #$66#$0F#$D3#$06,                 'psrlq xmm0,[esi]');
  TestDis('psrlq xmm1,xmm2',           #$66#$0F#$D3#$CA,                 'psrlq xmm1,xmm2');
  TestDis('psrlq xmm2,xmm0',           #$66#$0F#$D3#$D0,                 'psrlq xmm2,xmm0');
  TestDis('psrlq mm1,$07',             #$0F#$73#$D1#$07,                 'psrlq mm1,$07');
  TestDis('psrlq xmm1,$07',            #$66#$0F#$73#$D1#$07,               'psrlq xmm1,$07');
  TestDis('extrq xmm0,$03,$04',        #$66#$0F#$78#$C0#$03#$04,             'extrq xmm0,$03,$04');
  TestDis('extrq xmm1,$03,$04',        #$66#$0F#$78#$C1#$03#$04,             'extrq xmm1,$03,$04');
  TestDis('extrq xmm1,xmm2',           #$66#$0F#$79#$CA,                 'extrq xmm1,xmm2');
  TestDis('insertq xmm1,xmm1,$03,$04', #$F2#$0F#$78#$C9#$03#$04,             'insertq xmm1,xmm1,$03,$04');
  TestDis('insertq xmm1,xmm1',         #$F2#$0F#$79#$C9,                 'insertq xmm1,xmm1');
  TestDis('insertq xmm1,xmm2,$03,$04', #$F2#$0F#$78#$CA#$03#$04,             'insertq xmm1,xmm2,$03,$04');
  TestDis('insertq xmm1,xmm2',         #$F2#$0F#$79#$CA,                 'insertq xmm1,xmm2');
  TestDis('movntsd [esi],xmm2',        #$F2#$0F#$2B#$16,                 'movntsd [esi],xmm2');
  TestDis('movntsd [esi],xmm1',        #$F2#$0F#$2B#$0E,                 'movntsd [esi],xmm1');
  TestDis('movntss [esi],xmm2',        #$F3#$0F#$2B#$16,                 'movntss [esi],xmm2');
  TestDis('movntss [esi],xmm1',        #$F3#$0F#$2B#$0E,                 'movntss [esi],xmm1');
  TestDis('pause',                     #$F3#$90,                     'pause');


  Process.NewMode := dm64;

  TestDis('push rax', #$50, 'push rax'); // push rax
  TestDis('push rcx', #$51, 'push rcx'); // push rcx
  TestDis('push rdx', #$52, 'push rdx'); // push rdx
  TestDis('pop rax',  #$58, 'pop rax');  // pop rax
  TestDis('pop rcx',  #$59, 'pop rcx');  // pop rcx
  TestDis('pop rdx',  #$5A, 'pop rdx');  // pop rdx

  TestDis('push r8', #$41#$50, 'push r8'); // pop r8
  TestDis('push r8', #$41#$51, 'push r9'); // pop r8
  TestDis('pop r9',  #$41#$58, 'pop r8');  // pop r9
  TestDis('pop r9',  #$41#$59, 'pop r9');  // pop r9

  TestDis('push cx',  #$66#$51,    'push cx');
  TestDis('push r8w', #$66#$41#$50, 'push r8w');


  TestDis('nopw', #$66#$2e#$0f#$1f#$84#$00#$00#$00#$00#$00, 'nop word ptr (cs:)[rax+rax+$0]'); // nopw   %cs:0x0(%rax,%rax,1)
  TestDis('nopl', #$0f#$1f#$00, 'nop dword ptr [rax]'); // nopl   (%rax)

  Process.NewMode := dm32;

  TestDis('32: push eax', #$50, 'push eax'); // push eax
  TestDis('32: push ecx', #$51, 'push ecx'); // push ecx
  TestDis('32: push edx', #$52, 'push edx'); // push edx
  TestDis('32: pop eax',  #$58, 'pop eax');  // pop eax
  TestDis('32: pop ecx',  #$59, 'pop ecx');  // pop ecx
  TestDis('32: pop edx',  #$5A, 'pop edx');  // pop edx

  TestDis('32: push ax',  #$66#$50,    'push ax');
  TestDis('32: push cx',  #$66#$51,    'push cx');

  TestDis('32: inc eax', #$40, 'inc eax');
  TestDis('32: inc ecx', #$41, 'inc ecx');

  TestDis('32: nopw', #$66#$2e#$0f#$1f#$84#$00#$00#$00#$00#$00, 'nop word ptr cs:[eax+eax+$0]'); // nopw   %cs:0x0(%rax,%rax,1)
  TestDis('32: nopl', #$0f#$1f#$00, 'nop dword ptr [eax]'); // nopl   (%rax)


  finally
  DisAss.Free;
  Process.Free;
  end;
end;

(*gdb 64
0000000100001720 662e0f1f840000000000     nopw   %cs:0x0(%rax,%rax,1)

000000010000172E 0f1f00                   nopl   (%rax)


32 bit
004016DA 662e0f1f840000000000     nopw   %cs:0x0(%eax,%eax,1)

004016E8 0f1f00                   nopl   (%eax)
*)

initialization

  RegisterTest(TTestAssembler);
end.

