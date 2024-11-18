unit TestAsm;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, FpDbgDisasX86, FpDbgClasses, FpDbgUtil,
  FpDbgCpuX86, LazLoggerBase, fpcunit, testregistry, RegExpr;

type

  { TTestAssembler }

  TTestAssembler = class(TTestCase)
  published
    procedure TestDisAsm;
  end;

implementation

type

  { TDummyProcess }

  TDummyProcess = class(TDbgProcess)
  public
    function CreateBreakPointTargetHandler: TFpBreakPointTargetHandler; override;
    property NewMode: TFPDMode write SetMode;
  end;

{ TDummyProcess }

function TDummyProcess.CreateBreakPointTargetHandler: TFpBreakPointTargetHandler;
begin
  Result := TBreakPointx86Handler.Create(Self);
end;

procedure TTestAssembler.TestDisAsm;
var
  DisAss: TX86AsmDecoder;
  Process: TDummyProcess;
  IgnoreSizeWordPtr: Boolean;


  procedure TestDis(AName: String; AData: String; AExp: String);
  var
    p : pointer  ;
    s, codeBytes, asmInstr : string;
    r: TRegExpr;
  begin
    s := AData + #90#90#90#90#90#90#90#90#90#90#90#90#90#90#90#90;
    p := @s[1];
    DisAss.Disassemble(p, codeBytes, asmInstr);

    //debugln([AName, ' // ', codeBytes, '  ', asmInstr, '  ', p-@s[1] ,' == ',Length(AData)]);

    AssertEquals(AName+' Cnt bytes', Length(AData), p-@s[1]);

    s := Trim(LowerCase(asmInstr));
    AExp := LowerCase(AExp);

    if IgnoreSizeWordPtr and
       ( (pos('word ptr', AExp) < 1) or (pos('word ptr', s) < 1) ) // don't ignore, if both have it
    then begin
      s := StringReplace(s, 'dword ptr',   ' ', [rfReplaceAll]);
      s := StringReplace(s, 'qword ptr',   ' ', [rfReplaceAll]);
      s := StringReplace(s, 'xmmword ptr', ' ', [rfReplaceAll]);
      s := StringReplace(s, 'ymmword ptr', ' ', [rfReplaceAll]);
      s := StringReplace(s, 'word ptr',    ' ', [rfReplaceAll]);
      s := StringReplace(s, 'byte ptr',    ' ', [rfReplaceAll]);
      AExp := StringReplace(AExp, 'dword ptr',   ' ', [rfReplaceAll]);
      AExp := StringReplace(AExp, 'qword ptr',   ' ', [rfReplaceAll]);
      AExp := StringReplace(AExp, 'xmmword ptr', ' ', [rfReplaceAll]);
      AExp := StringReplace(AExp, 'ymmword ptr', ' ', [rfReplaceAll]);
      AExp := StringReplace(AExp, 'word ptr',    ' ', [rfReplaceAll]);
      AExp := StringReplace(AExp, 'byte ptr',    ' ', [rfReplaceAll]);
    end;

    r := TRegExpr.Create('(\$)0+([0-9a-fA-F])');

    s := StringReplace(s, '  ', ' ', [rfReplaceAll]);  // space
    s := StringReplace(s, ',  ', ',', [rfReplaceAll]); // space
    s := StringReplace(s, ', ', ',', [rfReplaceAll]); // space
    s := r.Replace(s, '$1$2', True);

    AExp := StringReplace(AExp, '  ', ' ', [rfReplaceAll]);  // space
    AExp := StringReplace(AExp, ',  ', ',', [rfReplaceAll]); // space
    AExp := StringReplace(AExp, ', ', ',', [rfReplaceAll]); // space
    AExp := r.Replace(AExp, '$1$2', True);
    r.Free;
    AssertEquals(AName+' asm ', AExp, s);


  end;

begin
  IgnoreSizeWordPtr := False;
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
  TestDis('mov    dh,ch',               #$88#$ee,                     'mov dh,ch');
  TestDis('mov    ah,bh',               #$88#$fc,                     'mov ah,bh');
  TestDis('mov    al,bl',               #$88#$d8,                     'mov al,bl');

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

  TestDis('pmovzxbq xmm3,xmm9',                  #$66#$41#$0f#$38#$32#$d9,             'pmovzxbq xmm3,xmm9');
  TestDis('pmovzxbq xmm3,WORD PTR [rdi+$3]',     #$66#$0f#$38#$32#$5f#$03,             'pmovzxbq xmm3,WORD PTR [rdi+$3]');
  TestDis('pmovsxbq xmm3,xmm9',                  #$66#$41#$0f#$38#$22#$d9,             'pmovsxbq xmm3,xmm9');
  TestDis('pmovsxbq xmm3,WORD PTR [rdi+$3]',     #$66#$0f#$38#$22#$5f#$03,             'pmovsxbq xmm3,WORD PTR [rdi+$3]');
  TestDis('vpmovzxbq xmm3,xmm9',                 #$c4#$c2#$79#$32#$d9,                 'vpmovzxbq xmm3,xmm9');
  TestDis('vpmovzxbq xmm3,WORD PTR [rdi+$3]',    #$c4#$e2#$79#$32#$5f#$03,             'vpmovzxbq xmm3,WORD PTR [rdi+$3]');
  TestDis('vpmovsxbq xmm3,xmm9',                 #$c4#$c2#$79#$22#$d9,                 'vpmovsxbq xmm3,xmm9');
  TestDis('vpmovsxbq xmm3,WORD PTR [rdi+$3]',    #$c4#$e2#$79#$22#$5f#$03,             'vpmovsxbq xmm3,WORD PTR [rdi+$3]');
  TestDis('vpmovzxbq ymm3,xmm9',                 #$c4#$c2#$7d#$32#$d9,                 'vpmovzxbq ymm3,xmm9');
  TestDis('vpmovzxbq ymm3,DWORD PTR [rdi+$3]',   #$c4#$e2#$7d#$32#$5f#$03,             'vpmovzxbq ymm3,DWORD PTR [rdi+$3]');
  TestDis('vpmovsxbq ymm3,xmm9',                 #$c4#$c2#$7d#$22#$d9,                 'vpmovsxbq ymm3,xmm9');
  TestDis('vpmovsxbq ymm3,DWORD PTR [rdi+$3]',   #$c4#$e2#$7d#$22#$5f#$03,             'vpmovsxbq ymm3,DWORD PTR [rdi+$3]');
  TestDis('pmovzxbw xmm1,xmm7',                  #$66#$0f#$38#$30#$cf,                 'pmovzxbw xmm1,xmm7');
  TestDis('pmovzxbw xmm1,QWORD PTR [rdi+$3]',    #$66#$0f#$38#$30#$4f#$03,             'pmovzxbw xmm1,QWORD PTR [rdi+$3]');
  TestDis('pmovzxbd xmm2,xmm8',                  #$66#$41#$0f#$38#$31#$d0,             'pmovzxbd xmm2,xmm8');
  TestDis('pmovzxbd xmm2,DWORD PTR [rdi+$3]',    #$66#$0f#$38#$31#$57#$03,             'pmovzxbd xmm2,DWORD PTR [rdi+$3]');
  TestDis('pmovzxwd xmm4,xmm10',                 #$66#$41#$0f#$38#$33#$e2,             'pmovzxwd xmm4,xmm10');
  TestDis('pmovzxwd xmm4,QWORD PTR [rdi+$3]',    #$66#$0f#$38#$33#$67#$03,             'pmovzxwd xmm4,QWORD PTR [rdi+$3]');
  TestDis('pmovzxwq xmm5,xmm11',                 #$66#$41#$0f#$38#$34#$eb,             'pmovzxwq xmm5,xmm11');
  TestDis('pmovzxwq xmm5,DWORD PTR [rdi+$3]',    #$66#$0f#$38#$34#$6f#$03,             'pmovzxwq xmm5,DWORD PTR [rdi+$3]');
  TestDis('pmovzxdq xmm6,xmm12',                 #$66#$41#$0f#$38#$35#$f4,             'pmovzxdq xmm6,xmm12');
  TestDis('pmovzxdq xmm6,QWORD PTR [rdi+$3]',    #$66#$0f#$38#$35#$77#$03,             'pmovzxdq xmm6,QWORD PTR [rdi+$3]');
  TestDis('pmovsxbw xmm1,xmm7',                  #$66#$0f#$38#$20#$cf,                 'pmovsxbw xmm1,xmm7');
  TestDis('pmovsxbw xmm1,QWORD PTR [rdi+$3]',    #$66#$0f#$38#$20#$4f#$03,             'pmovsxbw xmm1,QWORD PTR [rdi+$3]');
  TestDis('pmovsxbd xmm2,xmm8',                  #$66#$41#$0f#$38#$21#$d0,             'pmovsxbd xmm2,xmm8');
  TestDis('pmovsxbd xmm2,DWORD PTR [rdi+$3]',    #$66#$0f#$38#$21#$57#$03,             'pmovsxbd xmm2,DWORD PTR [rdi+$3]');
  TestDis('pmovsxwd xmm4,xmm10',                 #$66#$41#$0f#$38#$23#$e2,             'pmovsxwd xmm4,xmm10');
  TestDis('pmovsxwd xmm4,QWORD PTR [rdi+$3]',    #$66#$0f#$38#$23#$67#$03,             'pmovsxwd xmm4,QWORD PTR [rdi+$3]');
  TestDis('pmovsxwq xmm5,xmm11',                 #$66#$41#$0f#$38#$24#$eb,             'pmovsxwq xmm5,xmm11');
  TestDis('pmovsxwq xmm5,DWORD PTR [rdi+$3]',    #$66#$0f#$38#$24#$6f#$03,             'pmovsxwq xmm5,DWORD PTR [rdi+$3]');
  TestDis('pmovsxdq xmm6,xmm12',                 #$66#$41#$0f#$38#$25#$f4,             'pmovsxdq xmm6,xmm12');
  TestDis('pmovsxdq xmm6,QWORD PTR [rdi+$3]',    #$66#$0f#$38#$25#$77#$03,             'pmovsxdq xmm6,QWORD PTR [rdi+$3]');
  TestDis('pmovzxbw xmm1,xmm7',                  #$66#$0f#$38#$30#$cf,                 'pmovzxbw xmm1,xmm7');
  TestDis('pmovzxbw xmm1,QWORD PTR [rdi+$3]',    #$66#$0f#$38#$30#$4f#$03,             'pmovzxbw xmm1,QWORD PTR [rdi+$3]');
  TestDis('pmovzxbd xmm2,xmm8',                  #$66#$41#$0f#$38#$31#$d0,             'pmovzxbd xmm2,xmm8');
  TestDis('pmovzxbd xmm2,DWORD PTR [rdi+$3]',    #$66#$0f#$38#$31#$57#$03,             'pmovzxbd xmm2,DWORD PTR [rdi+$3]');
  TestDis('pmovzxwd xmm4,xmm10',                 #$66#$41#$0f#$38#$33#$e2,             'pmovzxwd xmm4,xmm10');
  TestDis('pmovzxwd xmm4,QWORD PTR [rdi+$3]',    #$66#$0f#$38#$33#$67#$03,             'pmovzxwd xmm4,QWORD PTR [rdi+$3]');
  TestDis('pmovzxwq xmm5,xmm11',                 #$66#$41#$0f#$38#$34#$eb,             'pmovzxwq xmm5,xmm11');
  TestDis('pmovzxwq xmm5,DWORD PTR [rdi+$3]',    #$66#$0f#$38#$34#$6f#$03,             'pmovzxwq xmm5,DWORD PTR [rdi+$3]');
  TestDis('pmovzxdq xmm6,xmm12',                 #$66#$41#$0f#$38#$35#$f4,             'pmovzxdq xmm6,xmm12');
  TestDis('pmovzxdq xmm6,QWORD PTR [rdi+$3]',    #$66#$0f#$38#$35#$77#$03,             'pmovzxdq xmm6,QWORD PTR [rdi+$3]');
  TestDis('vpmovsxbw xmm1,xmm7',                 #$c4#$e2#$79#$20#$cf,                 'vpmovsxbw xmm1,xmm7');
  TestDis('vpmovsxbw xmm1,QWORD PTR [rdi+$3]',   #$c4#$e2#$79#$20#$4f#$03,             'vpmovsxbw xmm1,QWORD PTR [rdi+$3]');
  TestDis('vpmovsxbd xmm2,xmm8',                 #$c4#$c2#$79#$21#$d0,                 'vpmovsxbd xmm2,xmm8');
  TestDis('vpmovsxbd xmm2,DWORD PTR [rdi+$3]',   #$c4#$e2#$79#$21#$57#$03,             'vpmovsxbd xmm2,DWORD PTR [rdi+$3]');
  TestDis('vpmovsxwd xmm4,xmm10',                #$c4#$c2#$79#$23#$e2,                 'vpmovsxwd xmm4,xmm10');
  TestDis('vpmovsxwd xmm4,QWORD PTR [rdi+$3]',   #$c4#$e2#$79#$23#$67#$03,             'vpmovsxwd xmm4,QWORD PTR [rdi+$3]');
  TestDis('vpmovsxwq xmm5,xmm11',                #$c4#$c2#$79#$24#$eb,                 'vpmovsxwq xmm5,xmm11');
  TestDis('vpmovsxwq xmm5,DWORD PTR [rdi+$3]',   #$c4#$e2#$79#$24#$6f#$03,             'vpmovsxwq xmm5,DWORD PTR [rdi+$3]');
  TestDis('vpmovsxdq xmm6,xmm12',                #$c4#$c2#$79#$25#$f4,                 'vpmovsxdq xmm6,xmm12');
  TestDis('vpmovsxdq xmm6,QWORD PTR [rdi+$3]',   #$c4#$e2#$79#$25#$77#$03,             'vpmovsxdq xmm6,QWORD PTR [rdi+$3]');
  TestDis('vpmovsxbw ymm1,xmm7',                 #$c4#$e2#$7d#$20#$cf,                 'vpmovsxbw ymm1,xmm7');
  TestDis('vpmovsxbw ymm1,XMMWORD PTR [rdi+$3]', #$c4#$e2#$7d#$20#$4f#$03,             'vpmovsxbw ymm1,XMMWORD PTR [rdi+$3]');
  TestDis('vpmovsxbd ymm2,xmm8',                 #$c4#$c2#$7d#$21#$d0,                 'vpmovsxbd ymm2,xmm8');
  TestDis('vpmovsxbd ymm2,QWORD PTR [rdi+$3]',   #$c4#$e2#$7d#$21#$57#$03,             'vpmovsxbd ymm2,QWORD PTR [rdi+$3]');
  TestDis('vpmovsxwd ymm4,xmm10',                #$c4#$c2#$7d#$23#$e2,                 'vpmovsxwd ymm4,xmm10');
  TestDis('vpmovsxwd ymm4,XMMWORD PTR [rdi+$3]', #$c4#$e2#$7d#$23#$67#$03,             'vpmovsxwd ymm4,XMMWORD PTR [rdi+$3]');
  TestDis('vpmovsxwq ymm5,xmm11',                #$c4#$c2#$7d#$24#$eb,                 'vpmovsxwq ymm5,xmm11');
  TestDis('vpmovsxwq ymm5,QWORD PTR [rdi+$3]',   #$c4#$e2#$7d#$24#$6f#$03,             'vpmovsxwq ymm5,QWORD PTR [rdi+$3]');
  TestDis('vpmovsxdq ymm6,xmm12',                #$c4#$c2#$7d#$25#$f4,                 'vpmovsxdq ymm6,xmm12');
  TestDis('vpmovsxdq ymm6,XMMWORD PTR [rdi+$3]', #$c4#$e2#$7d#$25#$77#$03,             'vpmovsxdq ymm6,XMMWORD PTR [rdi+$3]');


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



  TestDis('dpps   xmm1,xmm2,$07',       #$66#$0f#$3a#$40#$ca#$07,             'dpps xmm1,xmm2,$07');
  TestDis('dpps   xmm2,xmm3,$05',       #$66#$0f#$3a#$40#$d3#$05,             'dpps xmm2,xmm3,$05');
  TestDis('vdpps  xmm1,xmm2,xmm3,$07',  #$c4#$e3#$69#$40#$cb#$07,             'vdpps xmm1,xmm2,xmm3,$07');
  TestDis('vdpps  xmm3,xmm1,xmm3,$05',  #$c4#$e3#$71#$40#$db#$05,             'vdpps xmm3,xmm1,xmm3,$05');
  TestDis('vdpps  ymm1,ymm2,ymm3,$07',  #$c4#$e3#$6d#$40#$cb#$07,             'vdpps ymm1,ymm2,ymm3,$07');
  TestDis('vdpps  ymm3,ymm1,ymm3,$05',  #$c4#$e3#$75#$40#$db#$05,             'vdpps ymm3,ymm1,ymm3,$05');
  TestDis('dpps   xmm1,[rdi],$07',      #$66#$0f#$3a#$40#$0f#$07,             'dpps xmm1,[rdi],$07');
  TestDis('dpps   xmm2,[rdi],$05',      #$66#$0f#$3a#$40#$17#$05,             'dpps xmm2,[rdi],$05');
  TestDis('dpps   xmm3,[rsi],$05',      #$66#$0f#$3a#$40#$1e#$05,             'dpps xmm3,[rsi],$05');
  TestDis('vdpps  xmm1,xmm2,[rdi],$07', #$c4#$e3#$69#$40#$0f#$07,             'vdpps xmm1,xmm2,[rdi],$07');
  TestDis('vdpps  xmm3,xmm4,[rdi],$05', #$c4#$e3#$59#$40#$1f#$05,             'vdpps xmm3,xmm4,[rdi],$05');
  TestDis('vdpps  xmm3,xmm1,[rsi],$07', #$c4#$e3#$71#$40#$1e#$07,             'vdpps xmm3,xmm1,[rsi],$07');
  TestDis('vdpps  ymm1,ymm2,[rdi],$07', #$c4#$e3#$6d#$40#$0f#$07,             'vdpps ymm1,ymm2,[rdi],$07');
  TestDis('vdpps  ymm3,ymm4,[rdi],$07', #$c4#$e3#$5d#$40#$1f#$07,             'vdpps ymm3,ymm4,[rdi],$07');
  TestDis('vdpps  ymm3,ymm1,[rsi],$07', #$c4#$e3#$75#$40#$1e#$07,             'vdpps ymm3,ymm1,[rsi],$07');
  TestDis('dppd   xmm1,xmm2,$07',       #$66#$0f#$3a#$41#$ca#$07,             'dppd xmm1,xmm2,$07');
  TestDis('dppd   xmm2,xmm3,$05',       #$66#$0f#$3a#$41#$d3#$05,             'dppd xmm2,xmm3,$05');
  TestDis('vdppd  xmm1,xmm2,xmm3,$07',  #$c4#$e3#$69#$41#$cb#$07,             'vdppd xmm1,xmm2,xmm3,$07');
  TestDis('vdppd  xmm3,xmm1,xmm3,$05',  #$c4#$e3#$71#$41#$db#$05,             'vdppd xmm3,xmm1,xmm3,$05');
  TestDis('dppd   xmm1,[rdi],$07',      #$66#$0f#$3a#$41#$0f#$07,             'dppd xmm1,[rdi],$07');
  TestDis('dppd   xmm2,[rdi],$05',      #$66#$0f#$3a#$41#$17#$05,             'dppd xmm2,[rdi],$05');
  TestDis('dppd   xmm3,[rsi],$05',      #$66#$0f#$3a#$41#$1e#$05,             'dppd xmm3,[rsi],$05');
  TestDis('vdppd  xmm1,xmm2,[rdi],$07', #$c4#$e3#$69#$41#$0f#$07,             'vdppd xmm1,xmm2,[rdi],$07');
  TestDis('vdppd  xmm3,xmm4,[rdi],$05', #$c4#$e3#$59#$41#$1f#$05,             'vdppd xmm3,xmm4,[rdi],$05');
  TestDis('vdppd  xmm3,xmm1,[rsi],$07', #$c4#$e3#$71#$41#$1e#$07,             'vdppd xmm3,xmm1,[rsi],$07');


  TestDis('popcnt rcx,rdx',             #$f3#$48#$0f#$b8#$ca,          'popcnt rcx,rdx');
  TestDis('popcnt ecx,edx',             #$f3#$0f#$b8#$ca,              'popcnt ecx,edx');
  TestDis('popcnt cx,dx',               #$f3#$66#$0f#$b8#$ca,          'popcnt cx,dx');
  TestDis('popcnt rcx,[rdx]',           #$f3#$48#$0f#$b8#$0a,          'popcnt rcx,[rdx]');
  TestDis('popcnt ecx,[rdx]',           #$f3#$0f#$b8#$0a,              'popcnt ecx,[rdx]');
  TestDis('popcnt cx,[rdx]',            #$f3#$66#$0f#$b8#$0a,          'popcnt cx,[rdx]');
  TestDis('bsf    rcx,rdx',             #$48#$0f#$bc#$ca,              'bsf rcx,rdx');
  TestDis('bsf    ecx,edx',             #$0f#$bc#$ca,                  'bsf ecx,edx');
  TestDis('bsf    cx,dx',               #$66#$0f#$bc#$ca,              'bsf cx,dx');
  TestDis('bsf    rcx,[rdx]',           #$48#$0f#$bc#$0a,              'bsf rcx,[rdx]');
  TestDis('bsf    ecx,[rdx]',           #$0f#$bc#$0a,                  'bsf ecx,[rdx]');
  TestDis('bsf    cx,[rdx]',            #$66#$0f#$bc#$0a,              'bsf cx,[rdx]');
  TestDis('bsr    rcx,rdx',             #$48#$0f#$bd#$ca,              'bsr rcx,rdx');
  TestDis('bsr    ecx,edx',             #$0f#$bd#$ca,                  'bsr ecx,edx');
  TestDis('bsr    cx,dx',               #$66#$0f#$bd#$ca,              'bsr cx,dx');
  TestDis('bsr    rcx,[rdx]',           #$48#$0f#$bd#$0a,              'bsr rcx,[rdx]');
  TestDis('bsr    ecx,[rdx]',           #$0f#$bd#$0a,                  'bsr ecx,[rdx]');
  TestDis('bsr    cx,[rdx]',            #$66#$0f#$bd#$0a,              'bsr cx,[rdx]');
  TestDis('tzcnt  rcx,rdx',             #$f3#$48#$0f#$bc#$ca,          'tzcnt rcx,rdx');
  TestDis('tzcnt  ecx,edx',             #$f3#$0f#$bc#$ca,              'tzcnt ecx,edx');
  TestDis('tzcnt  cx,dx',               #$66#$f3#$0f#$bc#$ca,          'tzcnt cx,dx');
  TestDis('tzcnt  rcx,[rdx]',           #$f3#$48#$0f#$bc#$0a,          'tzcnt rcx,[rdx]');
  TestDis('tzcnt  ecx,[rdx]',           #$f3#$0f#$bc#$0a,              'tzcnt ecx,[rdx]');
  TestDis('tzcnt  cx,[rdx]',            #$66#$f3#$0f#$bc#$0a,          'tzcnt cx,[rdx]');
  TestDis('lzcnt  rcx,rdx',             #$f3#$48#$0f#$bd#$ca,          'lzcnt rcx,rdx');
  TestDis('lzcnt  ecx,edx',             #$f3#$0f#$bd#$ca,              'lzcnt ecx,edx');
  TestDis('lzcnt  cx,dx',               #$66#$f3#$0f#$bd#$ca,          'lzcnt cx,dx');
  TestDis('lzcnt  rcx,[rdx]',           #$f3#$48#$0f#$bd#$0a,          'lzcnt rcx,[rdx]');
  TestDis('lzcnt  ecx,[rdx]',           #$f3#$0f#$bd#$0a,              'lzcnt ecx,[rdx]');
  TestDis('lzcnt  cx,[rdx]',            #$66#$f3#$0f#$bd#$0a,          'lzcnt cx,[rdx]');

  TestDis('pextrw edx,mm1,$2',                   #$0f#$c5#$d1#$02,                         'pextrw edx,mm1,$2');
  TestDis('pextrw edx,xmm2,$2',                  #$66#$0f#$c5#$d2#$02,                     'pextrw edx,xmm2,$2');
  TestDis('pextrw edx,xmm2,$2',                  #$66#$0f#$3a#$15#$d2#$02,                 'pextrw edx,xmm2,$2');
  TestDis('pextrw WORD PTR [rsi-$F],xmm3,$2',    #$66#$0f#$3a#$15#$5e#$f1#$02,             'pextrw WORD PTR [rsi-$F],xmm3,$2');
  TestDis('vpextrw edx,xmm1,$2',                 #$c5#$f9#$c5#$d1#$02,                     'vpextrw edx,xmm1,$2');
  TestDis('vpextrw WORD PTR [rsi-$F],xmm2,$2',   #$c4#$e3#$79#$15#$56#$f1#$02,             'vpextrw WORD PTR [rsi-$F],xmm2,$2');
  TestDis('pextrb edx,xmm1,$2',                  #$66#$0f#$3a#$14#$ca#$02,                 'pextrb edx,xmm1,$2');
  TestDis('pextrd edx,xmm2,$2',                  #$66#$0f#$3a#$16#$d2#$02,                 'pextrd edx,xmm2,$2');
  TestDis('pextrq rdx,xmm3,$2',                  #$66#$48#$0f#$3a#$16#$da#$02,             'pextrq rdx,xmm3,$2');
  TestDis('vpextrb edx,xmm1,$2',                 #$c4#$e3#$79#$14#$ca#$02,                 'vpextrb edx,xmm1,$2');
  TestDis('vpextrd edx,xmm2,$2',                 #$c4#$e3#$79#$16#$d2#$02,                 'vpextrd edx,xmm2,$2');
  TestDis('vpextrq rdx,xmm3,$2',                 #$c4#$e3#$f9#$16#$da#$02,                 'vpextrq rdx,xmm3,$2');
  TestDis('pextrb BYTE PTR [rsi-$F],xmm1,$2',    #$66#$0f#$3a#$14#$4e#$f1#$02,             'pextrb BYTE PTR [rsi-$F],xmm1,$2');
  TestDis('pextrd DWORD PTR [rsi-$F],xmm2,$2',   #$66#$0f#$3a#$16#$56#$f1#$02,             'pextrd DWORD PTR [rsi-$F],xmm2,$2');
  TestDis('pextrq QWORD PTR [rsi-$F],xmm3,$2',   #$66#$48#$0f#$3a#$16#$5e#$f1#$02,         'pextrq QWORD PTR [rsi-$F],xmm3,$2');
  TestDis('vpextrb BYTE PTR [rsi-$F],xmm1,$2',   #$c4#$e3#$79#$14#$4e#$f1#$02,             'vpextrb BYTE PTR [rsi-$F],xmm1,$2');
  TestDis('vpextrd DWORD PTR [rsi-$F],xmm2,$2',  #$c4#$e3#$79#$16#$56#$f1#$02,             'vpextrd DWORD PTR [rsi-$F],xmm2,$2');
  TestDis('vpextrq QWORD PTR [rsi-$F],xmm3,$2',  #$c4#$e3#$f9#$16#$5e#$f1#$02,             'vpextrq QWORD PTR [rsi-$F],xmm3,$2');

  IgnoreSizeWordPtr := True;

  TestDis('aesdeclast xmm2,XMMWORD PTR [rdi]',         #$66#$0f#$38#$df#$17,                     'aesdeclast xmm2,XMMWORD PTR [rdi]');
  TestDis('aesdeclast xmm2,XMMWORD PTR [rsi]',         #$66#$0f#$38#$df#$16,                     'aesdeclast xmm2,XMMWORD PTR [rsi]');
  TestDis('aesdeclast xmm2,XMMWORD PTR [rbx]',         #$66#$0f#$38#$df#$13,                     'aesdeclast xmm2,XMMWORD PTR [rbx]');
  TestDis('aesdeclast xmm2,XMMWORD PTR [rax+rdi]',     #$66#$0f#$38#$df#$14#$07,                 'aesdeclast xmm2,XMMWORD PTR [rax+rdi]');
  TestDis('aesdeclast xmm2,XMMWORD PTR [rax+rsi]',     #$66#$0f#$38#$df#$14#$06,                 'aesdeclast xmm2,XMMWORD PTR [rax+rsi]');
  TestDis('aesdeclast xmm2,xmm3',                      #$66#$0f#$38#$df#$d3,                     'aesdeclast xmm2,xmm3');
  TestDis('aesdeclast xmm5,XMMWORD PTR [rdi]',         #$66#$0f#$38#$df#$2f,                     'aesdeclast xmm5,XMMWORD PTR [rdi]');
  TestDis('aesdeclast xmm9,XMMWORD PTR [rdi]',         #$66#$44#$0f#$38#$df#$0f,                 'aesdeclast xmm9,XMMWORD PTR [rdi]');
  TestDis('aesdeclast xmm5,XMMWORD PTR [rsi]',         #$66#$0f#$38#$df#$2e,                     'aesdeclast xmm5,XMMWORD PTR [rsi]');
  TestDis('aesdeclast xmm5,XMMWORD PTR [rbx]',         #$66#$0f#$38#$df#$2b,                     'aesdeclast xmm5,XMMWORD PTR [rbx]');
  TestDis('aesdeclast xmm5,xmm3',                      #$66#$0f#$38#$df#$eb,                     'aesdeclast xmm5,xmm3');
  TestDis('aesdec xmm2,XMMWORD PTR [rdi]',             #$66#$0f#$38#$de#$17,                     'aesdec xmm2,XMMWORD PTR [rdi]');
  TestDis('aesdec xmm2,xmm3',                          #$66#$0f#$38#$de#$d3,                     'aesdec xmm2,xmm3');
  TestDis('aesenclast xmm2,XMMWORD PTR [rdi]',         #$66#$0f#$38#$dd#$17,                     'aesenclast xmm2,XMMWORD PTR [rdi]');
  TestDis('aesenclast xmm2,xmm3',                      #$66#$0f#$38#$dd#$d3,                     'aesenclast xmm2,xmm3');
  TestDis('aesenc xmm2,XMMWORD PTR [rdi]',             #$66#$0f#$38#$dc#$17,                     'aesenc xmm2,XMMWORD PTR [rdi]');
  TestDis('aesenc xmm2,xmm3',                          #$66#$0f#$38#$dc#$d3,                     'aesenc xmm2,xmm3');
  TestDis('pclmulqdq xmm4,XMMWORD PTR [rsi+$f],$3a',   #$66#$0f#$3a#$44#$66#$0f#$3a,             'pclmulqdq xmm4,XMMWORD PTR [rsi+$f],$3a');
  TestDis('pclmulqdq xmm6,XMMWORD PTR [rbx+$e],$32',   #$66#$0f#$3a#$44#$73#$0e#$32,             'pclmulqdq xmm6,XMMWORD PTR [rbx+$e],$32');
  TestDis('insertps xmm2,DWORD PTR [rdi],$1',          #$66#$0f#$3a#$21#$17#$01,                 'insertps xmm2,DWORD PTR [rdi],$1');
  TestDis('insertps xmm7,DWORD PTR [rdi],$5',          #$66#$0f#$3a#$21#$3f#$05,                 'insertps xmm7,DWORD PTR [rdi],$5');
  TestDis('insertps xmm2,DWORD PTR [rsi],$4',          #$66#$0f#$3a#$21#$16#$04,                 'insertps xmm2,DWORD PTR [rsi],$4');
  TestDis('insertps xmm2,xmm3,$1',                     #$66#$0f#$3a#$21#$d3#$01,                 'insertps xmm2,xmm3,$1');
  TestDis('insertps xmm5,xmm7,$2',                     #$66#$0f#$3a#$21#$ef#$02,                 'insertps xmm5,xmm7,$2');
  TestDis('pinsrq xmm6,rdi,$1',                        #$66#$48#$0f#$3a#$22#$f7#$01,             'pinsrq xmm6,rdi,$1');
  TestDis('pinsrq xmm2,QWORD PTR [rdi],$1',            #$66#$48#$0f#$3a#$22#$17#$01,             'pinsrq xmm2,QWORD PTR [rdi],$1');
  TestDis('pinsrq xmm2,QWORD PTR [rdi],$3',            #$66#$48#$0f#$3a#$22#$17#$03,             'pinsrq xmm2,QWORD PTR [rdi],$3');
  TestDis('pinsrb xmm5,edi,$1',                        #$66#$0f#$3a#$20#$ef#$01,                 'pinsrb xmm5,edi,$1');
  TestDis('pinsrb xmm2,BYTE PTR [rdi],$1',             #$66#$0f#$3a#$20#$17#$01,                 'pinsrb xmm2,BYTE PTR [rdi],$1');
  TestDis('pinsrb xmm2,BYTE PTR [rdi],$3',             #$66#$0f#$3a#$20#$17#$03,                 'pinsrb xmm2,BYTE PTR [rdi],$3');
  TestDis('pinsrd xmm4,DWORD PTR [rdx],$22',           #$66#$0f#$3a#$22#$22#$22,                 'pinsrd xmm4,DWORD PTR [rdx],$22');
  TestDis('pinsrd xmm2,DWORD PTR [rdi],$1',            #$66#$0f#$3a#$22#$17#$01,                 'pinsrd xmm2,DWORD PTR [rdi],$1');
  TestDis('pinsrd xmm5,DWORD PTR [rdi],$1',            #$66#$0f#$3a#$22#$2f#$01,                 'pinsrd xmm5,DWORD PTR [rdi],$1');
  TestDis('pinsrd xmm5,DWORD PTR [rdi],$2',            #$66#$0f#$3a#$22#$2f#$02,                 'pinsrd xmm5,DWORD PTR [rdi],$2');
  TestDis('pinsrd xmm5,DWORD PTR [rsi],$2',            #$66#$0f#$3a#$22#$2e#$02,                 'pinsrd xmm5,DWORD PTR [rsi],$2');
  TestDis('pinsrw mm2,WORD PTR [rdi],$2',              #$0f#$c4#$17#$02,                         'pinsrw mm2,WORD PTR [rdi],$2');
  TestDis('pinsrw xmm2,WORD PTR [rdi],$2',             #$66#$0f#$c4#$17#$02,                     'pinsrw xmm2,WORD PTR [rdi],$2');
  TestDis('pinsrw xmm2,WORD PTR [rsi],$2',             #$66#$0f#$c4#$16#$02,                     'pinsrw xmm2,WORD PTR [rsi],$2');
  TestDis('pinsrw xmm2,WORD PTR [rcx],$2',             #$66#$0f#$c4#$11#$02,                     'pinsrw xmm2,WORD PTR [rcx],$2');
  TestDis('pinsrw xmm5,WORD PTR [rdi],$2',             #$66#$0f#$c4#$2f#$02,                     'pinsrw xmm5,WORD PTR [rdi],$2');
  TestDis('vpinsrq xmm3,xmm2,rdi,$1',                  #$c4#$e3#$e9#$22#$df#$01,                 'vpinsrq xmm3,xmm2,rdi,$1');
  TestDis('vpinsrq xmm3,xmm9,rsi,$3',                  #$c4#$e3#$b1#$22#$de#$03,                 'vpinsrq xmm3,xmm9,rsi,$3');
  TestDis('vpinsrq xmm6,xmm2,QWORD PTR [rdi],$1',      #$c4#$e3#$e9#$22#$37#$01,                 'vpinsrq xmm6,xmm2,QWORD PTR [rdi],$1');
  TestDis('vpinsrq xmm3,xmm7,QWORD PTR [rsi],$2',      #$c4#$e3#$c1#$22#$1e#$02,                 'vpinsrq xmm3,xmm7,QWORD PTR [rsi],$2');
  TestDis('vpinsrb xmm3,xmm2,BYTE PTR [rdi],$1',       #$c4#$e3#$69#$20#$1f#$01,                 'vpinsrb xmm3,xmm2,BYTE PTR [rdi],$1');
  TestDis('vpinsrb xmm9,xmm1,BYTE PTR [rax],$1',       #$c4#$63#$71#$20#$08#$01,                 'vpinsrb xmm9,xmm1,BYTE PTR [rax],$1');
  TestDis('vpinsrd xmm3,xmm2,DWORD PTR [rdi],$1',      #$c4#$e3#$69#$22#$1f#$01,                 'vpinsrd xmm3,xmm2,DWORD PTR [rdi],$1');
  TestDis('vpinsrd xmm1,xmm9,DWORD PTR [rsi],$2',      #$c4#$e3#$31#$22#$0e#$02,                 'vpinsrd xmm1,xmm9,DWORD PTR [rsi],$2');
  TestDis('vpinsrw xmm3,xmm9,WORD PTR [rdi],$2',       #$c5#$b1#$c4#$1f#$02,                     'vpinsrw xmm3,xmm9,WORD PTR [rdi],$2');
  TestDis('vpinsrw xmm1,xmm2,WORD PTR [rsi],$2',       #$c5#$e9#$c4#$0e#$02,                     'vpinsrw xmm1,xmm2,WORD PTR [rsi],$2');
  TestDis('vinsertps xmm1,xmm2,DWORD PTR [rdi],$1',    #$c4#$e3#$69#$21#$0f#$01,                 'vinsertps xmm1,xmm2,DWORD PTR [rdi],$1');
  TestDis('vinsertps xmm1,xmm2,xmm3,$1',               #$c4#$e3#$69#$21#$cb#$01,                 'vinsertps xmm1,xmm2,xmm3,$1');
  TestDis('vpclmulhqlqdq xmm1,xmm2,XMMWORD PTR [rdi]', #$c4#$e3#$69#$44#$0f#$01,                 'vpclmulqdq xmm1,xmm2,XMMWORD PTR [rdi],$1');
  TestDis('vpclmulhqlqdq xmm7,xmm2,XMMWORD PTR [rsi]', #$c4#$e3#$69#$44#$3e#$01,                 'vpclmulqdq xmm7,xmm2,XMMWORD PTR [rsi],$1');
  TestDis('vpclmullqhqdq xmm7,xmm2,XMMWORD PTR [rsi]', #$c4#$e3#$69#$44#$3e#$02,                 'vpclmulqdq xmm7,xmm2,XMMWORD PTR [rsi],$2');
  TestDis('vaesenc xmm1,xmm2,XMMWORD PTR [rdi]',       #$c4#$e2#$69#$dc#$0f,                     'vaesenc xmm1,xmm2,XMMWORD PTR [rdi]');
  TestDis('vaesenc xmm3,xmm7,XMMWORD PTR [rsi]',       #$c4#$e2#$41#$dc#$1e,                     'vaesenc xmm3,xmm7,XMMWORD PTR [rsi]');
  TestDis('vaesenclast xmm1,xmm2,XMMWORD PTR [rdi]',   #$c4#$e2#$69#$dd#$0f,                     'vaesenclast xmm1,xmm2,XMMWORD PTR [rdi]');
  TestDis('vaesenclast xmm1,xmm2,XMMWORD PTR [rsi]',   #$c4#$e2#$69#$dd#$0e,                     'vaesenclast xmm1,xmm2,XMMWORD PTR [rsi]');
  TestDis('vaesdec xmm1,xmm2,XMMWORD PTR [rdi]',       #$c4#$e2#$69#$de#$0f,                     'vaesdec xmm1,xmm2,XMMWORD PTR [rdi]');
  TestDis('vaesdeclast xmm1,xmm2,XMMWORD PTR [rdi]',   #$c4#$e2#$69#$df#$0f,                     'vaesdeclast xmm1,xmm2,XMMWORD PTR [rdi]');
  TestDis('aesdeclast xmm2,XMMWORD PTR [edi]',         #$67#$66#$0f#$38#$df#$17,                 'aesdeclast xmm2,XMMWORD PTR [edi]');
  TestDis('aesdeclast xmm2,XMMWORD PTR [esi]',         #$67#$66#$0f#$38#$df#$16,                 'aesdeclast xmm2,XMMWORD PTR [esi]');
  TestDis('aesdeclast xmm2,XMMWORD PTR [ebx]',         #$67#$66#$0f#$38#$df#$13,                 'aesdeclast xmm2,XMMWORD PTR [ebx]');
  TestDis('aesdeclast xmm2,XMMWORD PTR [eax+edi]',     #$67#$66#$0f#$38#$df#$14#$07,             'aesdeclast xmm2,XMMWORD PTR [eax+edi]');
  TestDis('aesdeclast xmm2,XMMWORD PTR [eax+esi]',     #$67#$66#$0f#$38#$df#$14#$06,             'aesdeclast xmm2,XMMWORD PTR [eax+esi]');
  TestDis('aesdeclast xmm5,XMMWORD PTR [edi]',         #$67#$66#$0f#$38#$df#$2f,                 'aesdeclast xmm5,XMMWORD PTR [edi]');
  TestDis('aesdeclast xmm9,XMMWORD PTR [edi]',         #$67#$66#$44#$0f#$38#$df#$0f,             'aesdeclast xmm9,XMMWORD PTR [edi]');
  TestDis('aesdeclast xmm5,XMMWORD PTR [esi]',         #$67#$66#$0f#$38#$df#$2e,                 'aesdeclast xmm5,XMMWORD PTR [esi]');
  TestDis('aesdeclast xmm5,XMMWORD PTR [ebx]',         #$67#$66#$0f#$38#$df#$2b,                 'aesdeclast xmm5,XMMWORD PTR [ebx]');
  TestDis('aesdec xmm2,XMMWORD PTR [edi]',             #$67#$66#$0f#$38#$de#$17,                 'aesdec xmm2,XMMWORD PTR [edi]');
  TestDis('aesenclast xmm2,XMMWORD PTR [edi]',         #$67#$66#$0f#$38#$dd#$17,                 'aesenclast xmm2,XMMWORD PTR [edi]');
  TestDis('aesenc xmm2,XMMWORD PTR [edi]',             #$67#$66#$0f#$38#$dc#$17,                 'aesenc xmm2,XMMWORD PTR [edi]');
  TestDis('insertps xmm2,DWORD PTR [edi],$1',          #$67#$66#$0f#$3a#$21#$17#$01,             'insertps xmm2,DWORD PTR [edi],$1');
  TestDis('insertps xmm7,DWORD PTR [edi],$5',          #$67#$66#$0f#$3a#$21#$3f#$05,             'insertps xmm7,DWORD PTR [edi],$5');
  TestDis('insertps xmm2,DWORD PTR [esi],$4',          #$67#$66#$0f#$3a#$21#$16#$04,             'insertps xmm2,DWORD PTR [esi],$4');
  TestDis('pinsrq xmm2,QWORD PTR [edi],$1',            #$67#$66#$48#$0f#$3a#$22#$17#$01,         'pinsrq xmm2,QWORD PTR [edi],$1');
  TestDis('pinsrq xmm2,QWORD PTR [edi],$3',            #$67#$66#$48#$0f#$3a#$22#$17#$03,         'pinsrq xmm2,QWORD PTR [edi],$3');
  TestDis('pinsrb xmm5,edi,$1',                        #$66#$0f#$3a#$20#$ef#$01,                 'pinsrb xmm5,edi,$1');
  TestDis('pinsrb xmm2,BYTE PTR [edi],$1',             #$67#$66#$0f#$3a#$20#$17#$01,             'pinsrb xmm2,BYTE PTR [edi],$1');
  TestDis('pinsrb xmm2,BYTE PTR [edi],$3',             #$67#$66#$0f#$3a#$20#$17#$03,             'pinsrb xmm2,BYTE PTR [edi],$3');
  TestDis('pinsrd xmm2,DWORD PTR [edi],$1',            #$67#$66#$0f#$3a#$22#$17#$01,             'pinsrd xmm2,DWORD PTR [edi],$1');
  TestDis('pinsrd xmm5,DWORD PTR [edi],$1',            #$67#$66#$0f#$3a#$22#$2f#$01,             'pinsrd xmm5,DWORD PTR [edi],$1');
  TestDis('pinsrd xmm5,DWORD PTR [edi],$2',            #$67#$66#$0f#$3a#$22#$2f#$02,             'pinsrd xmm5,DWORD PTR [edi],$2');
  TestDis('pinsrd xmm5,DWORD PTR [esi],$2',            #$67#$66#$0f#$3a#$22#$2e#$02,             'pinsrd xmm5,DWORD PTR [esi],$2');
  TestDis('pinsrw mm2,WORD PTR [edi],$2',              #$67#$0f#$c4#$17#$02,                     'pinsrw mm2,WORD PTR [edi],$2');
  TestDis('pinsrw xmm2,WORD PTR [edi],$2',             #$67#$66#$0f#$c4#$17#$02,                 'pinsrw xmm2,WORD PTR [edi],$2');
  TestDis('pinsrw xmm2,WORD PTR [esi],$2',             #$67#$66#$0f#$c4#$16#$02,                 'pinsrw xmm2,WORD PTR [esi],$2');
  TestDis('pinsrw xmm2,WORD PTR [ecx],$2',             #$67#$66#$0f#$c4#$11#$02,                 'pinsrw xmm2,WORD PTR [ecx],$2');
  TestDis('pinsrw xmm5,WORD PTR [edi],$2',             #$67#$66#$0f#$c4#$2f#$02,                 'pinsrw xmm5,WORD PTR [edi],$2');
  TestDis('vpinsrq xmm6,xmm2,QWORD PTR [edi],$1',      #$67#$c4#$e3#$e9#$22#$37#$01,             'vpinsrq xmm6,xmm2,QWORD PTR [edi],$1');
  TestDis('vpinsrq xmm3,xmm7,QWORD PTR [esi],$2',      #$67#$c4#$e3#$c1#$22#$1e#$02,             'vpinsrq xmm3,xmm7,QWORD PTR [esi],$2');
  TestDis('vpinsrb xmm3,xmm2,BYTE PTR [edi],$1',       #$67#$c4#$e3#$69#$20#$1f#$01,             'vpinsrb xmm3,xmm2,BYTE PTR [edi],$1');
  TestDis('vpinsrb xmm9,xmm1,BYTE PTR [eax],$1',       #$67#$c4#$63#$71#$20#$08#$01,             'vpinsrb xmm9,xmm1,BYTE PTR [eax],$1');
  TestDis('vpinsrd xmm3,xmm2,DWORD PTR [edi],$1',      #$67#$c4#$e3#$69#$22#$1f#$01,             'vpinsrd xmm3,xmm2,DWORD PTR [edi],$1');
  TestDis('vpinsrd xmm1,xmm9,DWORD PTR [esi],$2',      #$67#$c4#$e3#$31#$22#$0e#$02,             'vpinsrd xmm1,xmm9,DWORD PTR [esi],$2');
  TestDis('vpinsrw xmm3,xmm9,WORD PTR [edi],$2',       #$67#$c5#$b1#$c4#$1f#$02,                 'vpinsrw xmm3,xmm9,WORD PTR [edi],$2');
  TestDis('vpinsrw xmm1,xmm2,WORD PTR [esi],$2',       #$67#$c5#$e9#$c4#$0e#$02,                 'vpinsrw xmm1,xmm2,WORD PTR [esi],$2');
  TestDis('vinsertps xmm1,xmm2,DWORD PTR [edi],$1',    #$67#$c4#$e3#$69#$21#$0f#$01,             'vinsertps xmm1,xmm2,DWORD PTR [edi],$1');
  TestDis('vpclmulhqlqdq xmm1,xmm2,XMMWORD PTR [edi]', #$67#$c4#$e3#$69#$44#$0f#$01,             'vpclmulqdq xmm1,xmm2,XMMWORD PTR [edi],$1');
  TestDis('vpclmulhqlqdq xmm7,xmm2,XMMWORD PTR [esi]', #$67#$c4#$e3#$69#$44#$3e#$01,             'vpclmulqdq xmm7,xmm2,XMMWORD PTR [esi],$1');
  TestDis('vpclmullqhqdq xmm7,xmm2,XMMWORD PTR [esi]', #$67#$c4#$e3#$69#$44#$3e#$02,             'vpclmulqdq xmm7,xmm2,XMMWORD PTR [esi],$2');
  TestDis('vaesenc xmm1,xmm2,XMMWORD PTR [edi]',       #$67#$c4#$e2#$69#$dc#$0f,                 'vaesenc xmm1,xmm2,XMMWORD PTR [edi]');
  TestDis('vaesenc xmm3,xmm7,XMMWORD PTR [esi]',       #$67#$c4#$e2#$41#$dc#$1e,                 'vaesenc xmm3,xmm7,XMMWORD PTR [esi]');
  TestDis('vaesenclast xmm1,xmm2,XMMWORD PTR [edi]',   #$67#$c4#$e2#$69#$dd#$0f,                 'vaesenclast xmm1,xmm2,XMMWORD PTR [edi]');
  TestDis('vaesenclast xmm1,xmm2,XMMWORD PTR [esi]',   #$67#$c4#$e2#$69#$dd#$0e,                 'vaesenclast xmm1,xmm2,XMMWORD PTR [esi]');
  TestDis('vaesdec xmm1,xmm2,XMMWORD PTR [edi]',       #$67#$c4#$e2#$69#$de#$0f,                 'vaesdec xmm1,xmm2,XMMWORD PTR [edi]');
  TestDis('vaesdeclast xmm1,xmm2,XMMWORD PTR [edi]',   #$67#$c4#$e2#$69#$df#$0f,                 'vaesdeclast xmm1,xmm2,XMMWORD PTR [edi]');

  TestDis('vcvtph2ps xmm1,xmm2',                            #$c4#$e2#$79#$13#$ca,                                               'vcvtph2ps xmm1,xmm2');
  TestDis('vcvtph2ps xmm9,xmm2',                            #$c4#$62#$79#$13#$ca,                                               'vcvtph2ps xmm9,xmm2');
  TestDis('vcvtph2ps xmm1,xmm1',                            #$c4#$e2#$79#$13#$c9,                                               'vcvtph2ps xmm1,xmm1');
  TestDis('vcvtph2ps xmm1,xmm10',                           #$c4#$c2#$79#$13#$ca,                                               'vcvtph2ps xmm1,xmm10');
  TestDis('vcvtph2ps xmm1,QWORD PTR [rdi]',                 #$c4#$e2#$79#$13#$0f,                                               'vcvtph2ps xmm1,QWORD PTR [rdi]');
  TestDis('vcvtph2ps xmm9,QWORD PTR [rdi]',                 #$c4#$62#$79#$13#$0f,                                               'vcvtph2ps xmm9,QWORD PTR [rdi]');
  TestDis('vcvtph2ps xmm1,QWORD PTR [rdi+$3]',              #$c4#$e2#$79#$13#$4f#$03,                                           'vcvtph2ps xmm1,QWORD PTR [rdi+$3]');
  TestDis('vcvtph2ps xmm9,QWORD PTR [rdi+$3]',              #$c4#$62#$79#$13#$4f#$03,                                           'vcvtph2ps xmm9,QWORD PTR [rdi+$3]');
  TestDis('vcvtph2ps xmm1,QWORD PTR [rax+rdi]',           #$c4#$e2#$79#$13#$0c#$07,                                             'vcvtph2ps xmm1,QWORD PTR [rax+rdi]');
  TestDis('vcvtph2ps xmm9,QWORD PTR [rax+rdi]',           #$c4#$62#$79#$13#$0c#$07,                                             'vcvtph2ps xmm9,QWORD PTR [rax+rdi]');
  TestDis('vcvtph2ps xmm1,QWORD PTR [eax+edi]',           #$67#$c4#$e2#$79#$13#$0c#$07,                                         'vcvtph2ps xmm1,QWORD PTR [eax+edi]');
  TestDis('vcvtph2ps xmm9,QWORD PTR [eax+edi]',           #$67#$c4#$62#$79#$13#$0c#$07,                                         'vcvtph2ps xmm9,QWORD PTR [eax+edi]');
  TestDis('vcvtph2ps xmm1,QWORD PTR [rax+rdi+$3]',        #$c4#$e2#$79#$13#$4c#$07#$03,                                         'vcvtph2ps xmm1,QWORD PTR [rax+rdi+$3]');
  TestDis('vcvtph2ps xmm9,QWORD PTR [rax+rdi+$3]',        #$c4#$62#$79#$13#$4c#$07#$03,                                         'vcvtph2ps xmm9,QWORD PTR [rax+rdi+$3]');
  TestDis('vcvtph2ps xmm1,QWORD PTR [eax+edi+$3]',        #$67#$c4#$e2#$79#$13#$4c#$07#$03,                                     'vcvtph2ps xmm1,QWORD PTR [eax+edi+$3]');
  TestDis('vcvtph2ps xmm9,QWORD PTR [eax+edi+$3]',        #$67#$c4#$62#$79#$13#$4c#$07#$03,                                     'vcvtph2ps xmm9,QWORD PTR [eax+edi+$3]');
  TestDis('vcvtph2ps xmm1,QWORD PTR [rax*2+rdi]',           #$c4#$e2#$79#$13#$0c#$47,                                           'vcvtph2ps xmm1,QWORD PTR [rax*2+rdi]');
  TestDis('vcvtph2ps xmm9,QWORD PTR [rax*2+rdi]',           #$c4#$62#$79#$13#$0c#$47,                                           'vcvtph2ps xmm9,QWORD PTR [rax*2+rdi]');
  TestDis('vcvtph2ps xmm1,QWORD PTR [eax*2+edi]',           #$67#$c4#$e2#$79#$13#$0c#$47,                                       'vcvtph2ps xmm1,QWORD PTR [eax*2+edi]');
  TestDis('vcvtph2ps xmm9,QWORD PTR [eax*2+edi]',           #$67#$c4#$62#$79#$13#$0c#$47,                                       'vcvtph2ps xmm9,QWORD PTR [eax*2+edi]');
  TestDis('vcvtph2ps xmm1,QWORD PTR [rax*2+rdi+$3]',        #$c4#$e2#$79#$13#$4c#$47#$03,                                       'vcvtph2ps xmm1,QWORD PTR [rax*2+rdi+$3]');
  TestDis('vcvtph2ps xmm9,QWORD PTR [rax*2+rdi+$3]',        #$c4#$62#$79#$13#$4c#$47#$03,                                       'vcvtph2ps xmm9,QWORD PTR [rax*2+rdi+$3]');
  TestDis('vcvtph2ps xmm1,QWORD PTR [eax*2+edi+$3]',        #$67#$c4#$e2#$79#$13#$4c#$47#$03,                                   'vcvtph2ps xmm1,QWORD PTR [eax*2+edi+$3]');
  TestDis('vcvtph2ps xmm9,QWORD PTR [eax*2+edi+$3]',        #$67#$c4#$62#$79#$13#$4c#$47#$03,                                   'vcvtph2ps xmm9,QWORD PTR [eax*2+edi+$3]');
  TestDis('vcvtph2ps ymm1,xmm2',                            #$c4#$e2#$7d#$13#$ca,                                               'vcvtph2ps ymm1,xmm2');
  TestDis('vcvtph2ps ymm9,xmm2',                            #$c4#$62#$7d#$13#$ca,                                               'vcvtph2ps ymm9,xmm2');
  TestDis('vcvtph2ps ymm1,xmm1',                            #$c4#$e2#$7d#$13#$c9,                                               'vcvtph2ps ymm1,xmm1');
  TestDis('vcvtph2ps ymm1,xmm10',                           #$c4#$c2#$7d#$13#$ca,                                               'vcvtph2ps ymm1,xmm10');
  TestDis('vcvtph2ps ymm1,XMMWORD PTR [rdi]',               #$c4#$e2#$7d#$13#$0f,                                               'vcvtph2ps ymm1,XMMWORD PTR [rdi]');
  TestDis('vcvtph2ps ymm9,XMMWORD PTR [rdi]',               #$c4#$62#$7d#$13#$0f,                                               'vcvtph2ps ymm9,XMMWORD PTR [rdi]');
  TestDis('vcvtph2ps ymm1,XMMWORD PTR [rdi+$3]',            #$c4#$e2#$7d#$13#$4f#$03,                                           'vcvtph2ps ymm1,XMMWORD PTR [rdi+$3]');
  TestDis('vcvtph2ps ymm9,XMMWORD PTR [rdi+$3]',            #$c4#$62#$7d#$13#$4f#$03,                                           'vcvtph2ps ymm9,XMMWORD PTR [rdi+$3]');
  TestDis('vcvtph2ps ymm1,XMMWORD PTR [rax+rdi]',         #$c4#$e2#$7d#$13#$0c#$07,                                             'vcvtph2ps ymm1,XMMWORD PTR [rax+rdi]');
  TestDis('vcvtph2ps ymm9,XMMWORD PTR [rax+rdi]',         #$c4#$62#$7d#$13#$0c#$07,                                             'vcvtph2ps ymm9,XMMWORD PTR [rax+rdi]');
  TestDis('vcvtph2ps ymm1,XMMWORD PTR [eax+edi]',         #$67#$c4#$e2#$7d#$13#$0c#$07,                                         'vcvtph2ps ymm1,XMMWORD PTR [eax+edi]');
  TestDis('vcvtph2ps ymm9,XMMWORD PTR [eax+edi]',         #$67#$c4#$62#$7d#$13#$0c#$07,                                         'vcvtph2ps ymm9,XMMWORD PTR [eax+edi]');
  TestDis('vcvtph2ps ymm1,XMMWORD PTR [rax+rdi+$3]',      #$c4#$e2#$7d#$13#$4c#$07#$03,                                         'vcvtph2ps ymm1,XMMWORD PTR [rax+rdi+$3]');
  TestDis('vcvtph2ps ymm9,XMMWORD PTR [rax+rdi+$3]',      #$c4#$62#$7d#$13#$4c#$07#$03,                                         'vcvtph2ps ymm9,XMMWORD PTR [rax+rdi+$3]');
  TestDis('vcvtph2ps ymm1,XMMWORD PTR [eax+edi+$3]',      #$67#$c4#$e2#$7d#$13#$4c#$07#$03,                                     'vcvtph2ps ymm1,XMMWORD PTR [eax+edi+$3]');
  TestDis('vcvtph2ps ymm9,XMMWORD PTR [eax+edi+$3]',      #$67#$c4#$62#$7d#$13#$4c#$07#$03,                                     'vcvtph2ps ymm9,XMMWORD PTR [eax+edi+$3]');
  TestDis('vcvtph2ps ymm1,XMMWORD PTR [rax*2+rdi]',         #$c4#$e2#$7d#$13#$0c#$47,                                           'vcvtph2ps ymm1,XMMWORD PTR [rax*2+rdi]');
  TestDis('vcvtph2ps ymm9,XMMWORD PTR [rax*2+rdi]',         #$c4#$62#$7d#$13#$0c#$47,                                           'vcvtph2ps ymm9,XMMWORD PTR [rax*2+rdi]');
  TestDis('vcvtph2ps ymm1,XMMWORD PTR [eax*2+edi]',         #$67#$c4#$e2#$7d#$13#$0c#$47,                                       'vcvtph2ps ymm1,XMMWORD PTR [eax*2+edi]');
  TestDis('vcvtph2ps ymm9,XMMWORD PTR [eax*2+edi]',         #$67#$c4#$62#$7d#$13#$0c#$47,                                       'vcvtph2ps ymm9,XMMWORD PTR [eax*2+edi]');
  TestDis('vcvtph2ps ymm1,XMMWORD PTR [rax*2+rdi+$3]',      #$c4#$e2#$7d#$13#$4c#$47#$03,                                       'vcvtph2ps ymm1,XMMWORD PTR [rax*2+rdi+$3]');
  TestDis('vcvtph2ps ymm9,XMMWORD PTR [rax*2+rdi+$3]',      #$c4#$62#$7d#$13#$4c#$47#$03,                                       'vcvtph2ps ymm9,XMMWORD PTR [rax*2+rdi+$3]');
  TestDis('vcvtph2ps ymm1,XMMWORD PTR [eax*2+edi+$3]',      #$67#$c4#$e2#$7d#$13#$4c#$47#$03,                                   'vcvtph2ps ymm1,XMMWORD PTR [eax*2+edi+$3]');
  TestDis('vcvtph2ps ymm9,XMMWORD PTR [eax*2+edi+$3]',      #$67#$c4#$62#$7d#$13#$4c#$47#$03,                                   'vcvtph2ps ymm9,XMMWORD PTR [eax*2+edi+$3]');
  TestDis('vcvtph2ps zmm1,ymm2',                            #$62#$f2#$7d#$48#$13#$ca,                                           'vcvtph2ps zmm1,ymm2');
  TestDis('vcvtph2ps zmm9,ymm2',                            #$62#$72#$7d#$48#$13#$ca,                                           'vcvtph2ps zmm9,ymm2');
  TestDis('vcvtph2ps zmm1,ymm1',                            #$62#$f2#$7d#$48#$13#$c9,                                           'vcvtph2ps zmm1,ymm1');
  TestDis('vcvtph2ps zmm1,ymm10',                           #$62#$d2#$7d#$48#$13#$ca,                                           'vcvtph2ps zmm1,ymm10');
  TestDis('vcvtph2ps zmm1,YMMWORD PTR [rdi]',               #$62#$f2#$7d#$48#$13#$0f,                                           'vcvtph2ps zmm1,YMMWORD PTR [rdi]');
  TestDis('vcvtph2ps zmm9,YMMWORD PTR [rdi]',               #$62#$72#$7d#$48#$13#$0f,                                           'vcvtph2ps zmm9,YMMWORD PTR [rdi]');
  TestDis('vcvtph2ps zmm1,YMMWORD PTR [rdi+$3]',            #$62#$f2#$7d#$48#$13#$8f#$03#$00#$00#$00,                           'vcvtph2ps zmm1,YMMWORD PTR [rdi+$3]');
  TestDis('vcvtph2ps zmm9,YMMWORD PTR [rdi+$3]',            #$62#$72#$7d#$48#$13#$8f#$03#$00#$00#$00,                           'vcvtph2ps zmm9,YMMWORD PTR [rdi+$3]');
  TestDis('vcvtph2ps zmm1,YMMWORD PTR [rax+rdi]',         #$62#$f2#$7d#$48#$13#$0c#$07,                                         'vcvtph2ps zmm1,YMMWORD PTR [rax+rdi]');
  TestDis('vcvtph2ps zmm9,YMMWORD PTR [rax+rdi]',         #$62#$72#$7d#$48#$13#$0c#$07,                                         'vcvtph2ps zmm9,YMMWORD PTR [rax+rdi]');
  TestDis('vcvtph2ps zmm1,YMMWORD PTR [eax+edi]',         #$67#$62#$f2#$7d#$48#$13#$0c#$07,                                     'vcvtph2ps zmm1,YMMWORD PTR [eax+edi]');
  TestDis('vcvtph2ps zmm9,YMMWORD PTR [eax+edi]',         #$67#$62#$72#$7d#$48#$13#$0c#$07,                                     'vcvtph2ps zmm9,YMMWORD PTR [eax+edi]');
  TestDis('vcvtph2ps zmm1,YMMWORD PTR [rax+rdi+$3]',      #$62#$f2#$7d#$48#$13#$8c#$07#$03#$00#$00#$00,                         'vcvtph2ps zmm1,YMMWORD PTR [rax+rdi+$3]');
  TestDis('vcvtph2ps zmm9,YMMWORD PTR [rax+rdi+$3]',      #$62#$72#$7d#$48#$13#$8c#$07#$03#$00#$00#$00,                         'vcvtph2ps zmm9,YMMWORD PTR [rax+rdi+$3]');
  TestDis('vcvtph2ps zmm1,YMMWORD PTR [eax+edi+$3]',      #$67#$62#$f2#$7d#$48#$13#$8c#$07#$03#$00#$00#$00, 'vcvtph2ps zmm1,YMMWORD PTR [eax+edi+$3]');
  TestDis('vcvtph2ps zmm9,YMMWORD PTR [eax+edi+$3]',      #$67#$62#$72#$7d#$48#$13#$8c#$07#$03#$00#$00#$00, 'vcvtph2ps zmm9,YMMWORD PTR [eax+edi+$3]');
  TestDis('vcvtph2ps zmm1,YMMWORD PTR [rax*2+rdi]',         #$62#$f2#$7d#$48#$13#$0c#$47,                                       'vcvtph2ps zmm1,YMMWORD PTR [rax*2+rdi]');
  TestDis('vcvtph2ps zmm9,YMMWORD PTR [rax*2+rdi]',         #$62#$72#$7d#$48#$13#$0c#$47,                                       'vcvtph2ps zmm9,YMMWORD PTR [rax*2+rdi]');
  TestDis('vcvtph2ps zmm1,YMMWORD PTR [eax*2+edi]',         #$67#$62#$f2#$7d#$48#$13#$0c#$47,                                   'vcvtph2ps zmm1,YMMWORD PTR [eax*2+edi]');
  TestDis('vcvtph2ps zmm9,YMMWORD PTR [eax*2+edi]',         #$67#$62#$72#$7d#$48#$13#$0c#$47,                                   'vcvtph2ps zmm9,YMMWORD PTR [eax*2+edi]');
  TestDis('vcvtph2ps zmm1,YMMWORD PTR [rax*2+rdi+$3]',      #$62#$f2#$7d#$48#$13#$8c#$47#$03#$00#$00#$00,                       'vcvtph2ps zmm1,YMMWORD PTR [rax*2+rdi+$3]');
  TestDis('vcvtph2ps zmm9,YMMWORD PTR [rax*2+rdi+$3]',      #$62#$72#$7d#$48#$13#$8c#$47#$03#$00#$00#$00,                       'vcvtph2ps zmm9,YMMWORD PTR [rax*2+rdi+$3]');
  TestDis('vcvtph2ps zmm1,YMMWORD PTR [eax*2+edi+$3]',      #$67#$62#$f2#$7d#$48#$13#$8c#$47#$03#$00#$00#$00, 'vcvtph2ps zmm1,YMMWORD PTR [eax*2+edi+$3]');
  TestDis('vcvtph2ps zmm9,YMMWORD PTR [eax*2+edi+$3]',      #$67#$62#$72#$7d#$48#$13#$8c#$47#$03#$00#$00#$00, 'vcvtph2ps zmm9,YMMWORD PTR [eax*2+edi+$3]');
  TestDis('vcvtps2ph xmm2,xmm1,$35',                        #$c4#$e3#$79#$1d#$ca#$35,                                           'vcvtps2ph xmm2,xmm1,$35');
  TestDis('vcvtps2ph xmm2,xmm13,$35',                       #$c4#$63#$79#$1d#$ea#$35,                                           'vcvtps2ph xmm2,xmm13,$35');
  TestDis('vcvtps2ph xmm12,xmm1,$35',                       #$c4#$c3#$79#$1d#$cc#$35,                                           'vcvtps2ph xmm12,xmm1,$35');
  TestDis('vcvtps2ph xmm12,xmm13,$35',                      #$c4#$43#$79#$1d#$ec#$35,                                           'vcvtps2ph xmm12,xmm13,$35');
  TestDis('vcvtps2ph xmm2,ymm1,$35',                        #$c4#$e3#$7d#$1d#$ca#$35,                                           'vcvtps2ph xmm2,ymm1,$35');
  TestDis('vcvtps2ph xmm2,ymm13,$35',                       #$c4#$63#$7d#$1d#$ea#$35,                                           'vcvtps2ph xmm2,ymm13,$35');
  TestDis('vcvtps2ph xmm12,ymm1,$35',                       #$c4#$c3#$7d#$1d#$cc#$35,                                           'vcvtps2ph xmm12,ymm1,$35');
  TestDis('vcvtps2ph xmm12,ymm13,$35',                      #$c4#$43#$7d#$1d#$ec#$35,                                           'vcvtps2ph xmm12,ymm13,$35');
  TestDis('vcvtps2ph ymm2,zmm1,$35',                        #$62#$f3#$7d#$48#$1d#$ca#$35,                                       'vcvtps2ph ymm2,zmm1,$35');
  TestDis('vcvtps2ph ymm2,zmm13,$35',                       #$62#$73#$7d#$48#$1d#$ea#$35,                                       'vcvtps2ph ymm2,zmm13,$35');
  TestDis('vcvtps2ph ymm12,zmm1,$35',                       #$62#$d3#$7d#$48#$1d#$cc#$35,                                       'vcvtps2ph ymm12,zmm1,$35');
  TestDis('vcvtps2ph ymm12,zmm13,$35',                      #$62#$53#$7d#$48#$1d#$ec#$35,                                       'vcvtps2ph ymm12,zmm13,$35');
  TestDis('vcvtps2ph QWORD PTR [rdi],xmm3,$35',             #$c4#$e3#$79#$1d#$1f#$35,                                           'vcvtps2ph QWORD PTR [rdi],xmm3,$35');
  TestDis('vcvtps2ph QWORD PTR [rdi],xmm11,$35',            #$c4#$63#$79#$1d#$1f#$35,                                           'vcvtps2ph QWORD PTR [rdi],xmm11,$35');
  TestDis('vcvtps2ph XMMWORD PTR [rdi],ymm3,$35',           #$c4#$e3#$7d#$1d#$1f#$35,                                           'vcvtps2ph XMMWORD PTR [rdi],ymm3,$35');
  TestDis('vcvtps2ph XMMWORD PTR [rdi],ymm11,$35',          #$c4#$63#$7d#$1d#$1f#$35,                                           'vcvtps2ph XMMWORD PTR [rdi],ymm11,$35');
  TestDis('vcvtps2ph YMMWORD PTR [rdi],zmm3,$35',           #$62#$f3#$7d#$48#$1d#$1f#$35,                                       'vcvtps2ph YMMWORD PTR [rdi],zmm3,$35');
  TestDis('vcvtps2ph YMMWORD PTR [rdi],zmm11,$35',          #$62#$73#$7d#$48#$1d#$1f#$35,                                       'vcvtps2ph YMMWORD PTR [rdi],zmm11,$35');
  TestDis('vcvtps2ph QWORD PTR [rdi+$3],xmm3,$35',          #$c4#$e3#$79#$1d#$5f#$03#$35,                                       'vcvtps2ph QWORD PTR [rdi+$3],xmm3,$35');
  TestDis('vcvtps2ph QWORD PTR [rdi+$3],xmm11,$35',         #$c4#$63#$79#$1d#$5f#$03#$35,                                       'vcvtps2ph QWORD PTR [rdi+$3],xmm11,$35');
  TestDis('vcvtps2ph XMMWORD PTR [rdi+$3],ymm3,$35',        #$c4#$e3#$7d#$1d#$5f#$03#$35,                                       'vcvtps2ph XMMWORD PTR [rdi+$3],ymm3,$35');
  TestDis('vcvtps2ph XMMWORD PTR [rdi+$3],ymm11,$35',       #$c4#$63#$7d#$1d#$5f#$03#$35,                                       'vcvtps2ph XMMWORD PTR [rdi+$3],ymm11,$35');
  TestDis('vcvtps2ph YMMWORD PTR [rdi+$3],zmm3,$35',        #$62#$f3#$7d#$48#$1d#$9f#$03#$00#$00#$00#$35,                       'vcvtps2ph YMMWORD PTR [rdi+$3],zmm3,$35');
  TestDis('vcvtps2ph YMMWORD PTR [rdi+$3],zmm11,$35',       #$62#$73#$7d#$48#$1d#$9f#$03#$00#$00#$00#$35,                       'vcvtps2ph YMMWORD PTR [rdi+$3],zmm11,$35');
  TestDis('vcvtps2ph QWORD PTR [rax+rdi],xmm3,$35',       #$c4#$e3#$79#$1d#$1c#$07#$35,                                         'vcvtps2ph QWORD PTR [rax+rdi],xmm3,$35');
  TestDis('vcvtps2ph QWORD PTR [rax+rdi],xmm11,$35',      #$c4#$63#$79#$1d#$1c#$07#$35,                                         'vcvtps2ph QWORD PTR [rax+rdi],xmm11,$35');
  TestDis('vcvtps2ph XMMWORD PTR [rax+rdi],ymm3,$35',     #$c4#$e3#$7d#$1d#$1c#$07#$35,                                         'vcvtps2ph XMMWORD PTR [rax+rdi],ymm3,$35');
  TestDis('vcvtps2ph XMMWORD PTR [rax+rdi],ymm11,$35',    #$c4#$63#$7d#$1d#$1c#$07#$35,                                         'vcvtps2ph XMMWORD PTR [rax+rdi],ymm11,$35');
  TestDis('vcvtps2ph YMMWORD PTR [rax+rdi],zmm3,$35',     #$62#$f3#$7d#$48#$1d#$1c#$07#$35,                                     'vcvtps2ph YMMWORD PTR [rax+rdi],zmm3,$35');
  TestDis('vcvtps2ph YMMWORD PTR [rax+rdi],zmm11,$35',    #$62#$73#$7d#$48#$1d#$1c#$07#$35,                                     'vcvtps2ph YMMWORD PTR [rax+rdi],zmm11,$35');
  TestDis('vcvtps2ph QWORD PTR [rax+rdi+$3],xmm3,$35',    #$c4#$e3#$79#$1d#$5c#$07#$03#$35,                                     'vcvtps2ph QWORD PTR [rax+rdi+$3],xmm3,$35');
  TestDis('vcvtps2ph QWORD PTR [rax+rdi+$3],xmm11,$35',   #$c4#$63#$79#$1d#$5c#$07#$03#$35,                                     'vcvtps2ph QWORD PTR [rax+rdi+$3],xmm11,$35');
  TestDis('vcvtps2ph XMMWORD PTR [rax+rdi+$3],ymm3,$35',  #$c4#$e3#$7d#$1d#$5c#$07#$03#$35,                                     'vcvtps2ph XMMWORD PTR [rax+rdi+$3],ymm3,$35');
  TestDis('vcvtps2ph XMMWORD PTR [rax+rdi+$3],ymm11,$35', #$c4#$63#$7d#$1d#$5c#$07#$03#$35,                                     'vcvtps2ph XMMWORD PTR [rax+rdi+$3],ymm11,$35');
  TestDis('vcvtps2ph YMMWORD PTR [rax+rdi+$3],zmm3,$35',  #$62#$f3#$7d#$48#$1d#$9c#$07#$03#$00#$00#$00#$35, 'vcvtps2ph YMMWORD PTR [rax+rdi+$3],zmm3,$35');
  TestDis('vcvtps2ph YMMWORD PTR [rax+rdi+$3],zmm11,$35', #$62#$73#$7d#$48#$1d#$9c#$07#$03#$00#$00#$00#$35, 'vcvtps2ph YMMWORD PTR [rax+rdi+$3],zmm11,$35');
  TestDis('vcvtps2ph QWORD PTR [rax*2+rdi],xmm3,$35',       #$c4#$e3#$79#$1d#$1c#$47#$35,                                       'vcvtps2ph QWORD PTR [rax*2+rdi],xmm3,$35');
  TestDis('vcvtps2ph QWORD PTR [rax*2+rdi],xmm11,$35',      #$c4#$63#$79#$1d#$1c#$47#$35,                                       'vcvtps2ph QWORD PTR [rax*2+rdi],xmm11,$35');
  TestDis('vcvtps2ph XMMWORD PTR [rax*2+rdi],ymm3,$35',     #$c4#$e3#$7d#$1d#$1c#$47#$35,                                       'vcvtps2ph XMMWORD PTR [rax*2+rdi],ymm3,$35');
  TestDis('vcvtps2ph XMMWORD PTR [rax*2+rdi],ymm11,$35',    #$c4#$63#$7d#$1d#$1c#$47#$35,                                       'vcvtps2ph XMMWORD PTR [rax*2+rdi],ymm11,$35');
  TestDis('vcvtps2ph YMMWORD PTR [rax*2+rdi],zmm3,$35',     #$62#$f3#$7d#$48#$1d#$1c#$47#$35,                                   'vcvtps2ph YMMWORD PTR [rax*2+rdi],zmm3,$35');
  TestDis('vcvtps2ph YMMWORD PTR [rax*2+rdi],zmm11,$35',    #$62#$73#$7d#$48#$1d#$1c#$47#$35,                                   'vcvtps2ph YMMWORD PTR [rax*2+rdi],zmm11,$35');
  TestDis('vcvtps2ph QWORD PTR [rax*2+rdi+$3],xmm3,$35',    #$c4#$e3#$79#$1d#$5c#$47#$03#$35,                                   'vcvtps2ph QWORD PTR [rax*2+rdi+$3],xmm3,$35');
  TestDis('vcvtps2ph QWORD PTR [rax*2+rdi+$3],xmm11,$35',   #$c4#$63#$79#$1d#$5c#$47#$03#$35,                                   'vcvtps2ph QWORD PTR [rax*2+rdi+$3],xmm11,$35');
  TestDis('vcvtps2ph XMMWORD PTR [rax*2+rdi+$3],ymm3,$35',  #$c4#$e3#$7d#$1d#$5c#$47#$03#$35,                                   'vcvtps2ph XMMWORD PTR [rax*2+rdi+$3],ymm3,$35');
  TestDis('vcvtps2ph XMMWORD PTR [rax*2+rdi+$3],ymm11,$35', #$c4#$63#$7d#$1d#$5c#$47#$03#$35,                                   'vcvtps2ph XMMWORD PTR [rax*2+rdi+$3],ymm11,$35');
  TestDis('vcvtps2ph YMMWORD PTR [rax*2+rdi+$3],zmm3,$35',  #$62#$f3#$7d#$48#$1d#$9c#$47#$03#$00#$00#$00#$35, 'vcvtps2ph YMMWORD PTR [rax*2+rdi+$3],zmm3,$35');
  TestDis('vcvtps2ph YMMWORD PTR [rax*2+rdi+$3],zmm11,$35', #$62#$73#$7d#$48#$1d#$9c#$47#$03#$00#$00#$00#$35, 'vcvtps2ph YMMWORD PTR [rax*2+rdi+$3],zmm11,$35');
  TestDis('vcvtps2ph QWORD PTR [eax+edi],xmm3,$35',       #$67#$c4#$e3#$79#$1d#$1c#$07#$35,                                     'vcvtps2ph QWORD PTR [eax+edi],xmm3,$35');
  TestDis('vcvtps2ph QWORD PTR [eax+edi],xmm11,$35',      #$67#$c4#$63#$79#$1d#$1c#$07#$35,                                     'vcvtps2ph QWORD PTR [eax+edi],xmm11,$35');
  TestDis('vcvtps2ph XMMWORD PTR [eax+edi],ymm3,$35',     #$67#$c4#$e3#$7d#$1d#$1c#$07#$35,                                     'vcvtps2ph XMMWORD PTR [eax+edi],ymm3,$35');
  TestDis('vcvtps2ph XMMWORD PTR [eax+edi],ymm11,$35',    #$67#$c4#$63#$7d#$1d#$1c#$07#$35,                                     'vcvtps2ph XMMWORD PTR [eax+edi],ymm11,$35');
  TestDis('vcvtps2ph YMMWORD PTR [eax+edi],zmm3,$35',     #$67#$62#$f3#$7d#$48#$1d#$1c#$07#$35,                                 'vcvtps2ph YMMWORD PTR [eax+edi],zmm3,$35');
  TestDis('vcvtps2ph YMMWORD PTR [eax+edi],zmm11,$35',    #$67#$62#$73#$7d#$48#$1d#$1c#$07#$35,                                 'vcvtps2ph YMMWORD PTR [eax+edi],zmm11,$35');
  TestDis('vcvtps2ph QWORD PTR [eax+edi+$3],xmm3,$35',    #$67#$c4#$e3#$79#$1d#$5c#$07#$03#$35,                                 'vcvtps2ph QWORD PTR [eax+edi+$3],xmm3,$35');
  TestDis('vcvtps2ph QWORD PTR [eax+edi+$3],xmm11,$35',   #$67#$c4#$63#$79#$1d#$5c#$07#$03#$35,                                 'vcvtps2ph QWORD PTR [eax+edi+$3],xmm11,$35');
  TestDis('vcvtps2ph XMMWORD PTR [eax+edi+$3],ymm3,$35',  #$67#$c4#$e3#$7d#$1d#$5c#$07#$03#$35,                                 'vcvtps2ph XMMWORD PTR [eax+edi+$3],ymm3,$35');
  TestDis('vcvtps2ph XMMWORD PTR [eax+edi+$3],ymm11,$35', #$67#$c4#$63#$7d#$1d#$5c#$07#$03#$35,                                 'vcvtps2ph XMMWORD PTR [eax+edi+$3],ymm11,$35');
  TestDis('vcvtps2ph YMMWORD PTR [eax+edi+$3],zmm3,$35',  #$67#$62#$f3#$7d#$48#$1d#$9c#$07#$03#$00#$00#$00#$35, 'vcvtps2ph YMMWORD PTR [eax+edi+$3],zmm3,$35');
  TestDis('vcvtps2ph YMMWORD PTR [eax+edi+$3],zmm11,$35', #$67#$62#$73#$7d#$48#$1d#$9c#$07#$03#$00#$00#$00#$35, 'vcvtps2ph YMMWORD PTR [eax+edi+$3],zmm11,$35');
  IgnoreSizeWordPtr := False;


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
  TestDis('mov    dh,ch',               #$88#$ee,                     'mov dh,ch');
  TestDis('mov    ah,bh',               #$88#$fc,                     'mov ah,bh');
  TestDis('mov    al,bl',               #$88#$d8,                     'mov al,bl');

  TestDis('vmovmskpd ecx,xmm1',     #$C5#$F9#$50#$C9,             'vmovmskpd ecx,xmm1');
  TestDis('vmovmskpd ecx,ymm1',     #$C5#$FD#$50#$C9,             'vmovmskpd ecx,ymm1');
  TestDis('vmovmskps ecx,xmm1',     #$C5#$F8#$50#$C9,             'vmovmskps ecx,xmm1');
  TestDis('vmovmskps ecx,ymm1',     #$C5#$FC#$50#$C9,             'vmovmskps ecx,ymm1');
  TestDis('vroundsd xmm1,xmm2,xmm3,$07', #$C4#$E3#$69#$0B#$CB#$07,     'vroundsd xmm1,xmm2,xmm3,$07');
  TestDis('vroundss xmm1,xmm2,xmm3,$07', #$C4#$E3#$69#$0A#$CB#$07,     'vroundss xmm1,xmm2,xmm3,$07');

  TestDis('pmovzxbq xmm3,xmm7',                  #$66#$0f#$38#$32#$df,                 'pmovzxbq xmm3,xmm7');
  TestDis('pmovzxbq xmm3,WORD PTR [edi+$3]',     #$66#$0f#$38#$32#$5f#$03,             'pmovzxbq xmm3,WORD PTR [edi+$3]');
  TestDis('pmovsxbq xmm3,xmm7',                  #$66#$0f#$38#$22#$df,                 'pmovsxbq xmm3,xmm7');
  TestDis('pmovsxbq xmm3,WORD PTR [edi+$3]',     #$66#$0f#$38#$22#$5f#$03,             'pmovsxbq xmm3,WORD PTR [edi+$3]');
  TestDis('vpmovzxbq xmm3,xmm7',                 #$c4#$e2#$79#$32#$df,                 'vpmovzxbq xmm3,xmm7');
  TestDis('vpmovzxbq xmm3,WORD PTR [edi+$3]',    #$c4#$e2#$79#$32#$5f#$03,             'vpmovzxbq xmm3,WORD PTR [edi+$3]');
  TestDis('vpmovsxbq xmm3,xmm7',                 #$c4#$e2#$79#$22#$df,                 'vpmovsxbq xmm3,xmm7');
  TestDis('vpmovsxbq xmm3,WORD PTR [edi+$3]',    #$c4#$e2#$79#$22#$5f#$03,             'vpmovsxbq xmm3,WORD PTR [edi+$3]');
  TestDis('vpmovzxbq ymm3,xmm7',                 #$c4#$e2#$7d#$32#$df,                 'vpmovzxbq ymm3,xmm7');
  TestDis('vpmovzxbq ymm3,DWORD PTR [edi+$3]',   #$c4#$e2#$7d#$32#$5f#$03,             'vpmovzxbq ymm3,DWORD PTR [edi+$3]');
  TestDis('vpmovsxbq ymm3,xmm7',                 #$c4#$e2#$7d#$22#$df,                 'vpmovsxbq ymm3,xmm7');
  TestDis('vpmovsxbq ymm3,DWORD PTR [edi+$3]',   #$c4#$e2#$7d#$22#$5f#$03,             'vpmovsxbq ymm3,DWORD PTR [edi+$3]');
  TestDis('pmovzxbw xmm1,xmm7',                  #$66#$0f#$38#$30#$cf,                 'pmovzxbw xmm1,xmm7');
  TestDis('pmovzxbw xmm1,QWORD PTR [edi+$3]',    #$66#$0f#$38#$30#$4f#$03,             'pmovzxbw xmm1,QWORD PTR [edi+$3]');
  TestDis('pmovzxbd xmm2,xmm6',                  #$66#$0f#$38#$31#$d6,                 'pmovzxbd xmm2,xmm6');
  TestDis('pmovzxbd xmm2,DWORD PTR [edi+$3]',    #$66#$0f#$38#$31#$57#$03,             'pmovzxbd xmm2,DWORD PTR [edi+$3]');
  TestDis('pmovzxwd xmm4,xmm5',                  #$66#$0f#$38#$33#$e5,                 'pmovzxwd xmm4,xmm5');
  TestDis('pmovzxwd xmm4,QWORD PTR [edi+$3]',    #$66#$0f#$38#$33#$67#$03,             'pmovzxwd xmm4,QWORD PTR [edi+$3]');
  TestDis('pmovzxwq xmm5,xmm6',                  #$66#$0f#$38#$34#$ee,                 'pmovzxwq xmm5,xmm6');
  TestDis('pmovzxwq xmm5,DWORD PTR [edi+$3]',    #$66#$0f#$38#$34#$6f#$03,             'pmovzxwq xmm5,DWORD PTR [edi+$3]');
  TestDis('pmovzxdq xmm6,xmm1',                  #$66#$0f#$38#$35#$f1,                 'pmovzxdq xmm6,xmm1');
  TestDis('pmovzxdq xmm6,QWORD PTR [edi+$3]',    #$66#$0f#$38#$35#$77#$03,             'pmovzxdq xmm6,QWORD PTR [edi+$3]');
  TestDis('pmovsxbw xmm1,xmm7',                  #$66#$0f#$38#$20#$cf,                 'pmovsxbw xmm1,xmm7');
  TestDis('pmovsxbw xmm1,QWORD PTR [edi+$3]',    #$66#$0f#$38#$20#$4f#$03,             'pmovsxbw xmm1,QWORD PTR [edi+$3]');
  TestDis('pmovsxbd xmm2,xmm6',                  #$66#$0f#$38#$21#$d6,                 'pmovsxbd xmm2,xmm6');
  TestDis('pmovsxbd xmm2,DWORD PTR [edi+$3]',    #$66#$0f#$38#$21#$57#$03,             'pmovsxbd xmm2,DWORD PTR [edi+$3]');
  TestDis('pmovsxwd xmm4,xmm5',                  #$66#$0f#$38#$23#$e5,                 'pmovsxwd xmm4,xmm5');
  TestDis('pmovsxwd xmm4,QWORD PTR [edi+$3]',    #$66#$0f#$38#$23#$67#$03,             'pmovsxwd xmm4,QWORD PTR [edi+$3]');
  TestDis('pmovsxwq xmm5,xmm6',                  #$66#$0f#$38#$24#$ee,                 'pmovsxwq xmm5,xmm6');
  TestDis('pmovsxwq xmm5,DWORD PTR [edi+$3]',    #$66#$0f#$38#$24#$6f#$03,             'pmovsxwq xmm5,DWORD PTR [edi+$3]');
  TestDis('pmovsxdq xmm6,xmm1',                  #$66#$0f#$38#$25#$f1,                 'pmovsxdq xmm6,xmm1');
  TestDis('pmovsxdq xmm6,QWORD PTR [edi+$3]',    #$66#$0f#$38#$25#$77#$03,             'pmovsxdq xmm6,QWORD PTR [edi+$3]');
  TestDis('pmovzxbw xmm1,xmm7',                  #$66#$0f#$38#$30#$cf,                 'pmovzxbw xmm1,xmm7');
  TestDis('pmovzxbw xmm1,QWORD PTR [edi+$3]',    #$66#$0f#$38#$30#$4f#$03,             'pmovzxbw xmm1,QWORD PTR [edi+$3]');
  TestDis('pmovzxbd xmm2,xmm6',                  #$66#$0f#$38#$31#$d6,                 'pmovzxbd xmm2,xmm6');
  TestDis('pmovzxbd xmm2,DWORD PTR [edi+$3]',    #$66#$0f#$38#$31#$57#$03,             'pmovzxbd xmm2,DWORD PTR [edi+$3]');
  TestDis('pmovzxwd xmm4,xmm5',                  #$66#$0f#$38#$33#$e5,                 'pmovzxwd xmm4,xmm5');
  TestDis('pmovzxwd xmm4,QWORD PTR [edi+$3]',    #$66#$0f#$38#$33#$67#$03,             'pmovzxwd xmm4,QWORD PTR [edi+$3]');
  TestDis('pmovzxwq xmm5,xmm6',                  #$66#$0f#$38#$34#$ee,                 'pmovzxwq xmm5,xmm6');
  TestDis('pmovzxwq xmm5,DWORD PTR [edi+$3]',    #$66#$0f#$38#$34#$6f#$03,             'pmovzxwq xmm5,DWORD PTR [edi+$3]');
  TestDis('pmovzxdq xmm6,xmm1',                  #$66#$0f#$38#$35#$f1,                 'pmovzxdq xmm6,xmm1');
  TestDis('pmovzxdq xmm6,QWORD PTR [edi+$3]',    #$66#$0f#$38#$35#$77#$03,             'pmovzxdq xmm6,QWORD PTR [edi+$3]');
  TestDis('vpmovsxbw xmm1,xmm7',                 #$c4#$e2#$79#$20#$cf,                 'vpmovsxbw xmm1,xmm7');
  TestDis('vpmovsxbw xmm1,QWORD PTR [edi+$3]',   #$c4#$e2#$79#$20#$4f#$03,             'vpmovsxbw xmm1,QWORD PTR [edi+$3]');
  TestDis('vpmovsxbd xmm2,xmm6',                 #$c4#$e2#$79#$21#$d6,                 'vpmovsxbd xmm2,xmm6');
  TestDis('vpmovsxbd xmm2,DWORD PTR [edi+$3]',   #$c4#$e2#$79#$21#$57#$03,             'vpmovsxbd xmm2,DWORD PTR [edi+$3]');
  TestDis('vpmovsxwd xmm4,xmm5',                 #$c4#$e2#$79#$23#$e5,                 'vpmovsxwd xmm4,xmm5');
  TestDis('vpmovsxwd xmm4,QWORD PTR [edi+$3]',   #$c4#$e2#$79#$23#$67#$03,             'vpmovsxwd xmm4,QWORD PTR [edi+$3]');
  TestDis('vpmovsxwq xmm5,xmm6',                 #$c4#$e2#$79#$24#$ee,                 'vpmovsxwq xmm5,xmm6');
  TestDis('vpmovsxwq xmm5,DWORD PTR [edi+$3]',   #$c4#$e2#$79#$24#$6f#$03,             'vpmovsxwq xmm5,DWORD PTR [edi+$3]');
  TestDis('vpmovsxdq xmm6,xmm1',                 #$c4#$e2#$79#$25#$f1,                 'vpmovsxdq xmm6,xmm1');
  TestDis('vpmovsxdq xmm6,QWORD PTR [edi+$3]',   #$c4#$e2#$79#$25#$77#$03,             'vpmovsxdq xmm6,QWORD PTR [edi+$3]');
  TestDis('vpmovsxbw ymm1,xmm7',                 #$c4#$e2#$7d#$20#$cf,                 'vpmovsxbw ymm1,xmm7');
  TestDis('vpmovsxbw ymm1,XMMWORD PTR [edi+$3]', #$c4#$e2#$7d#$20#$4f#$03,             'vpmovsxbw ymm1,XMMWORD PTR [edi+$3]');
  TestDis('vpmovsxbd ymm2,xmm6',                 #$c4#$e2#$7d#$21#$d6,                 'vpmovsxbd ymm2,xmm6');
  TestDis('vpmovsxbd ymm2,QWORD PTR [edi+$3]',   #$c4#$e2#$7d#$21#$57#$03,             'vpmovsxbd ymm2,QWORD PTR [edi+$3]');
  TestDis('vpmovsxwd ymm4,xmm5',                 #$c4#$e2#$7d#$23#$e5,                 'vpmovsxwd ymm4,xmm5');
  TestDis('vpmovsxwd ymm4,XMMWORD PTR [edi+$3]', #$c4#$e2#$7d#$23#$67#$03,             'vpmovsxwd ymm4,XMMWORD PTR [edi+$3]');
  TestDis('vpmovsxwq ymm5,xmm6',                 #$c4#$e2#$7d#$24#$ee,                 'vpmovsxwq ymm5,xmm6');
  TestDis('vpmovsxwq ymm5,QWORD PTR [edi+$3]',   #$c4#$e2#$7d#$24#$6f#$03,             'vpmovsxwq ymm5,QWORD PTR [edi+$3]');
  TestDis('vpmovsxdq ymm6,xmm1',                 #$c4#$e2#$7d#$25#$f1,                 'vpmovsxdq ymm6,xmm1');
  TestDis('vpmovsxdq ymm6,XMMWORD PTR [edi+$3]', #$c4#$e2#$7d#$25#$77#$03,             'vpmovsxdq ymm6,XMMWORD PTR [edi+$3]');

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


  TestDis('dpps   xmm1,xmm2,$07',       #$66#$0f#$3a#$40#$ca#$07,             'dpps xmm1,xmm2,$07');
  TestDis('dpps   xmm2,xmm3,$05',       #$66#$0f#$3a#$40#$d3#$05,             'dpps xmm2,xmm3,$05');
  TestDis('vdpps  xmm1,xmm2,xmm3,$07',  #$c4#$e3#$69#$40#$cb#$07,             'vdpps xmm1,xmm2,xmm3,$07');
  TestDis('vdpps  xmm3,xmm1,xmm3,$05',  #$c4#$e3#$71#$40#$db#$05,             'vdpps xmm3,xmm1,xmm3,$05');
  TestDis('vdpps  ymm1,ymm2,ymm3,$07',  #$c4#$e3#$6d#$40#$cb#$07,             'vdpps ymm1,ymm2,ymm3,$07');
  TestDis('vdpps  ymm3,ymm1,ymm3,$05',  #$c4#$e3#$75#$40#$db#$05,             'vdpps ymm3,ymm1,ymm3,$05');
  TestDis('dpps   xmm1,[edi],$07',      #$66#$0f#$3a#$40#$0f#$07,             'dpps xmm1,[edi],$07');
  TestDis('dpps   xmm2,[edi],$05',      #$66#$0f#$3a#$40#$17#$05,             'dpps xmm2,[edi],$05');
  TestDis('dpps   xmm3,[esi],$05',      #$66#$0f#$3a#$40#$1e#$05,             'dpps xmm3,[esi],$05');
  TestDis('vdpps  xmm1,xmm2,[edi],$07', #$c4#$e3#$69#$40#$0f#$07,             'vdpps xmm1,xmm2,[edi],$07');
  TestDis('vdpps  xmm3,xmm4,[edi],$05', #$c4#$e3#$59#$40#$1f#$05,             'vdpps xmm3,xmm4,[edi],$05');
  TestDis('vdpps  xmm3,xmm1,[esi],$07', #$c4#$e3#$71#$40#$1e#$07,             'vdpps xmm3,xmm1,[esi],$07');
  TestDis('vdpps  ymm1,ymm2,[edi],$07', #$c4#$e3#$6d#$40#$0f#$07,             'vdpps ymm1,ymm2,[edi],$07');
  TestDis('vdpps  ymm3,ymm4,[edi],$07', #$c4#$e3#$5d#$40#$1f#$07,             'vdpps ymm3,ymm4,[edi],$07');
  TestDis('vdpps  ymm3,ymm1,[esi],$07', #$c4#$e3#$75#$40#$1e#$07,             'vdpps ymm3,ymm1,[esi],$07');
  TestDis('dppd   xmm1,xmm2,$07',       #$66#$0f#$3a#$41#$ca#$07,             'dppd xmm1,xmm2,$07');
  TestDis('dppd   xmm2,xmm3,$05',       #$66#$0f#$3a#$41#$d3#$05,             'dppd xmm2,xmm3,$05');
  TestDis('vdppd  xmm1,xmm2,xmm3,$07',  #$c4#$e3#$69#$41#$cb#$07,             'vdppd xmm1,xmm2,xmm3,$07');
  TestDis('vdppd  xmm3,xmm1,xmm3,$05',  #$c4#$e3#$71#$41#$db#$05,             'vdppd xmm3,xmm1,xmm3,$05');
  TestDis('dppd   xmm1,[edi],$07',      #$66#$0f#$3a#$41#$0f#$07,             'dppd xmm1,[edi],$07');
  TestDis('dppd   xmm2,[edi],$05',      #$66#$0f#$3a#$41#$17#$05,             'dppd xmm2,[edi],$05');
  TestDis('dppd   xmm3,[esi],$05',      #$66#$0f#$3a#$41#$1e#$05,             'dppd xmm3,[esi],$05');
  TestDis('vdppd  xmm1,xmm2,[edi],$07', #$c4#$e3#$69#$41#$0f#$07,             'vdppd xmm1,xmm2,[edi],$07');
  TestDis('vdppd  xmm3,xmm4,[edi],$05', #$c4#$e3#$59#$41#$1f#$05,             'vdppd xmm3,xmm4,[edi],$05');
  TestDis('vdppd  xmm3,xmm1,[esi],$07', #$c4#$e3#$71#$41#$1e#$07,             'vdppd xmm3,xmm1,[esi],$07');


  TestDis('popcnt ecx,edx',             #$f3#$0f#$b8#$ca,             'popcnt ecx,edx');
  TestDis('popcnt cx,dx',               #$f3#$66#$0f#$b8#$ca,         'popcnt cx,dx');
  TestDis('popcnt ecx,[edx]',           #$f3#$0f#$b8#$0a,             'popcnt ecx,[edx]');
  TestDis('popcnt cx,[edx]',            #$f3#$66#$0f#$b8#$0a,         'popcnt cx,[edx]');
  TestDis('bsf    ecx,edx',             #$0f#$bc#$ca,                 'bsf ecx,edx');
  TestDis('bsf    cx,dx',               #$66#$0f#$bc#$ca,             'bsf cx,dx');
  TestDis('bsf    ecx,[edx]',           #$0f#$bc#$0a,                 'bsf ecx,[edx]');
  TestDis('bsf    cx,[edx]',            #$66#$0f#$bc#$0a,             'bsf cx,[edx]');
  TestDis('bsr    ecx,edx',             #$0f#$bd#$ca,                 'bsr ecx,edx');
  TestDis('bsr    cx,dx',               #$66#$0f#$bd#$ca,             'bsr cx,dx');
  TestDis('bsr    ecx,[edx]',           #$0f#$bd#$0a,                 'bsr ecx,[edx]');
  TestDis('bsr    cx,[edx]',            #$66#$0f#$bd#$0a,             'bsr cx,[edx]');
  TestDis('tzcnt  ecx,edx',             #$f3#$0f#$bc#$ca,             'tzcnt ecx,edx');
  TestDis('tzcnt  cx,dx',               #$66#$f3#$0f#$bc#$ca,         'tzcnt cx,dx');
  TestDis('tzcnt  ecx,[edx]',           #$f3#$0f#$bc#$0a,             'tzcnt ecx,[edx]');
  TestDis('tzcnt  cx,[edx]',            #$66#$f3#$0f#$bc#$0a,         'tzcnt cx,[edx]');
  TestDis('lzcnt  ecx,edx',             #$f3#$0f#$bd#$ca,             'lzcnt ecx,edx');
  TestDis('lzcnt  cx,dx',               #$66#$f3#$0f#$bd#$ca,         'lzcnt cx,dx');
  TestDis('lzcnt  ecx,[edx]',           #$f3#$0f#$bd#$0a,             'lzcnt ecx,[edx]');
  TestDis('lzcnt  cx,[edx]',            #$66#$f3#$0f#$bd#$0a,         'lzcnt cx,[edx]');

  TestDis('pextrw edx,mm1,$2',                  #$0f#$c5#$d1#$02,                       'pextrw edx,mm1,$2');
  TestDis('pextrw edx,xmm2,$2',                 #$66#$0f#$c5#$d2#$02,                   'pextrw edx,xmm2,$2');
  TestDis('pextrw edx,xmm2,$2',                 #$66#$0f#$3a#$15#$d2#$02,               'pextrw edx,xmm2,$2');
  TestDis('pextrw WORD PTR [esi-$F],xmm3,$2',   #$66#$0f#$3a#$15#$5e#$f1#$02,           'pextrw WORD PTR [esi-$F],xmm3,$2');
  TestDis('vpextrw edx,xmm1,$2',                #$c5#$f9#$c5#$d1#$02,                   'vpextrw edx,xmm1,$2');
  TestDis('vpextrw WORD PTR [esi-$F],xmm2,$2',  #$c4#$e3#$79#$15#$56#$f1#$02,           'vpextrw WORD PTR [esi-$F],xmm2,$2');
  TestDis('pextrb edx,xmm1,$2',                 #$66#$0f#$3a#$14#$ca#$02,               'pextrb edx,xmm1,$2');
  TestDis('pextrd edx,xmm2,$2',                 #$66#$0f#$3a#$16#$d2#$02,               'pextrd edx,xmm2,$2');
  TestDis('vpextrb edx,xmm1,$2',                #$c4#$e3#$79#$14#$ca#$02,               'vpextrb edx,xmm1,$2');
  TestDis('vpextrd edx,xmm2,$2',                #$c4#$e3#$79#$16#$d2#$02,               'vpextrd edx,xmm2,$2');
  TestDis('pextrb BYTE PTR [esi-$F],xmm1,$2',   #$66#$0f#$3a#$14#$4e#$f1#$02,           'pextrb BYTE PTR [esi-$F],xmm1,$2');
  TestDis('pextrd DWORD PTR [esi-$F],xmm2,$2',  #$66#$0f#$3a#$16#$56#$f1#$02,           'pextrd DWORD PTR [esi-$F],xmm2,$2');
  TestDis('vpextrb BYTE PTR [esi-$F],xmm1,$2',  #$c4#$e3#$79#$14#$4e#$f1#$02,           'vpextrb BYTE PTR [esi-$F],xmm1,$2');
  TestDis('vpextrd DWORD PTR [esi-$F],xmm2,$2', #$c4#$e3#$79#$16#$56#$f1#$02,           'vpextrd DWORD PTR [esi-$F],xmm2,$2');

  IgnoreSizeWordPtr := True;

  TestDis('aesdeclast xmm2,XMMWORD PTR [edi]',         #$66#$0f#$38#$df#$17,                   'aesdeclast xmm2,XMMWORD PTR [edi]');
  TestDis('aesdeclast xmm2,XMMWORD PTR [esi]',         #$66#$0f#$38#$df#$16,                   'aesdeclast xmm2,XMMWORD PTR [esi]');
  TestDis('aesdeclast xmm2,XMMWORD PTR [ebx]',         #$66#$0f#$38#$df#$13,                   'aesdeclast xmm2,XMMWORD PTR [ebx]');
  TestDis('aesdeclast xmm2,XMMWORD PTR [eax+edi]',     #$66#$0f#$38#$df#$14#$07,               'aesdeclast xmm2,XMMWORD PTR [eax+edi]');
  TestDis('aesdeclast xmm2,XMMWORD PTR [eax+esi]',     #$66#$0f#$38#$df#$14#$06,               'aesdeclast xmm2,XMMWORD PTR [eax+esi]');
  TestDis('aesdeclast xmm2,xmm3',                      #$66#$0f#$38#$df#$d3,                   'aesdeclast xmm2,xmm3');
  TestDis('aesdeclast xmm5,XMMWORD PTR [edi]',         #$66#$0f#$38#$df#$2f,                   'aesdeclast xmm5,XMMWORD PTR [edi]');
  TestDis('aesdeclast xmm5,XMMWORD PTR [esi]',         #$66#$0f#$38#$df#$2e,                   'aesdeclast xmm5,XMMWORD PTR [esi]');
  TestDis('aesdeclast xmm5,XMMWORD PTR [ebx]',         #$66#$0f#$38#$df#$2b,                   'aesdeclast xmm5,XMMWORD PTR [ebx]');
  TestDis('aesdeclast xmm5,xmm3',                      #$66#$0f#$38#$df#$eb,                   'aesdeclast xmm5,xmm3');
  TestDis('aesdec xmm2,XMMWORD PTR [edi]',             #$66#$0f#$38#$de#$17,                   'aesdec xmm2,XMMWORD PTR [edi]');
  TestDis('aesdec xmm2,xmm3',                          #$66#$0f#$38#$de#$d3,                   'aesdec xmm2,xmm3');
  TestDis('aesenclast xmm2,XMMWORD PTR [edi]',         #$66#$0f#$38#$dd#$17,                   'aesenclast xmm2,XMMWORD PTR [edi]');
  TestDis('aesenclast xmm2,xmm3',                      #$66#$0f#$38#$dd#$d3,                   'aesenclast xmm2,xmm3');
  TestDis('aesenc xmm2,XMMWORD PTR [edi]',             #$66#$0f#$38#$dc#$17,                   'aesenc xmm2,XMMWORD PTR [edi]');
  TestDis('aesenc xmm2,xmm3',                          #$66#$0f#$38#$dc#$d3,                   'aesenc xmm2,xmm3');
  TestDis('pclmulqdq xmm4,XMMWORD PTR [esi+$f],$3a',   #$66#$0f#$3a#$44#$66#$0f#$3a,           'pclmulqdq xmm4,XMMWORD PTR [esi+$f],$3a');
  TestDis('pclmulqdq xmm6,XMMWORD PTR [ebx+$e],$32',   #$66#$0f#$3a#$44#$73#$0e#$32,           'pclmulqdq xmm6,XMMWORD PTR [ebx+$e],$32');
  TestDis('insertps xmm2,DWORD PTR [edi],$1',          #$66#$0f#$3a#$21#$17#$01,               'insertps xmm2,DWORD PTR [edi],$1');
  TestDis('insertps xmm7,DWORD PTR [edi],$5',          #$66#$0f#$3a#$21#$3f#$05,               'insertps xmm7,DWORD PTR [edi],$5');
  TestDis('insertps xmm2,DWORD PTR [esi],$4',          #$66#$0f#$3a#$21#$16#$04,               'insertps xmm2,DWORD PTR [esi],$4');
  TestDis('insertps xmm2,xmm3,$1',                     #$66#$0f#$3a#$21#$d3#$01,               'insertps xmm2,xmm3,$1');
  TestDis('insertps xmm5,xmm7,$2',                     #$66#$0f#$3a#$21#$ef#$02,               'insertps xmm5,xmm7,$2');
  TestDis('pinsrb xmm5,edi,$1',                        #$66#$0f#$3a#$20#$ef#$01,               'pinsrb xmm5,edi,$1');
  TestDis('pinsrb xmm2,BYTE PTR [edi],$1',             #$66#$0f#$3a#$20#$17#$01,               'pinsrb xmm2,BYTE PTR [edi],$1');
  TestDis('pinsrb xmm2,BYTE PTR [edi],$3',             #$66#$0f#$3a#$20#$17#$03,               'pinsrb xmm2,BYTE PTR [edi],$3');
  TestDis('pinsrd xmm4,DWORD PTR [esi+$f],$3a',        #$66#$0f#$3a#$22#$66#$0f#$3a,           'pinsrd xmm4,DWORD PTR [esi+$f],$3a');
  TestDis('pinsrd xmm2,DWORD PTR [edi],$1',            #$66#$0f#$3a#$22#$17#$01,               'pinsrd xmm2,DWORD PTR [edi],$1');
  TestDis('pinsrd xmm5,DWORD PTR [edi],$1',            #$66#$0f#$3a#$22#$2f#$01,               'pinsrd xmm5,DWORD PTR [edi],$1');
  TestDis('pinsrd xmm5,DWORD PTR [edi],$2',            #$66#$0f#$3a#$22#$2f#$02,               'pinsrd xmm5,DWORD PTR [edi],$2');
  TestDis('pinsrd xmm5,DWORD PTR [esi],$2',            #$66#$0f#$3a#$22#$2e#$02,               'pinsrd xmm5,DWORD PTR [esi],$2');
  TestDis('pinsrw mm2,WORD PTR [edi],$2',              #$0f#$c4#$17#$02,                       'pinsrw mm2,WORD PTR [edi],$2');
  TestDis('pinsrw xmm2,WORD PTR [edi],$2',             #$66#$0f#$c4#$17#$02,                   'pinsrw xmm2,WORD PTR [edi],$2');
  TestDis('pinsrw xmm2,WORD PTR [esi],$2',             #$66#$0f#$c4#$16#$02,                   'pinsrw xmm2,WORD PTR [esi],$2');
  TestDis('pinsrw xmm2,WORD PTR [ecx],$2',             #$66#$0f#$c4#$11#$02,                   'pinsrw xmm2,WORD PTR [ecx],$2');
  TestDis('pinsrw xmm5,WORD PTR [edi],$2',             #$66#$0f#$c4#$2f#$02,                   'pinsrw xmm5,WORD PTR [edi],$2');
  TestDis('vpinsrd xmm6,xmm2,DWORD PTR [edi],$1',      #$c4#$e3#$e9#$22#$37#$01,               'vpinsrq xmm6,xmm2,DWORD PTR [edi],$1');
  TestDis('vpinsrd xmm3,xmm7,DWORD PTR [esi],$2',      #$c4#$e3#$c1#$22#$1e#$02,               'vpinsrq xmm3,xmm7,DWORD PTR [esi],$2');
  TestDis('vpinsrb xmm3,xmm2,BYTE PTR [edi],$1',       #$c4#$e3#$69#$20#$1f#$01,               'vpinsrb xmm3,xmm2,BYTE PTR [edi],$1');
  TestDis('vpinsrb xmm7,xmm1,BYTE PTR [eax],$1',       #$c4#$e3#$71#$20#$38#$01,               'vpinsrb xmm7,xmm1,BYTE PTR [eax],$1');
  TestDis('vpinsrd xmm3,xmm2,DWORD PTR [edi],$1',      #$c4#$e3#$69#$22#$1f#$01,               'vpinsrd xmm3,xmm2,DWORD PTR [edi],$1');
  TestDis('vpinsrd xmm1,xmm7,DWORD PTR [esi],$2',      #$c4#$e3#$41#$22#$0e#$02,               'vpinsrd xmm1,xmm7,DWORD PTR [esi],$2');
  TestDis('vpinsrw xmm3,xmm7,WORD PTR [edi],$2',       #$c5#$c1#$c4#$1f#$02,                   'vpinsrw xmm3,xmm7,WORD PTR [edi],$2');
  TestDis('vpinsrw xmm1,xmm2,WORD PTR [esi],$2',       #$c5#$e9#$c4#$0e#$02,                   'vpinsrw xmm1,xmm2,WORD PTR [esi],$2');
  TestDis('vinsertps xmm1,xmm2,DWORD PTR [edi],$1',    #$c4#$e3#$69#$21#$0f#$01,               'vinsertps xmm1,xmm2,DWORD PTR [edi],$1');
  TestDis('vinsertps xmm1,xmm2,xmm3,$1',               #$c4#$e3#$69#$21#$cb#$01,               'vinsertps xmm1,xmm2,xmm3,$1');
  TestDis('vpclmulhqlqdq xmm1,xmm2,XMMWORD PTR [edi]', #$c4#$e3#$69#$44#$0f#$01,               'vpclmulqdq xmm1,xmm2,XMMWORD PTR [edi],$1');
  TestDis('vpclmulhqlqdq xmm7,xmm2,XMMWORD PTR [esi]', #$c4#$e3#$69#$44#$3e#$01,               'vpclmulqdq xmm7,xmm2,XMMWORD PTR [esi],$1');
  TestDis('vpclmullqhqdq xmm7,xmm2,XMMWORD PTR [esi]', #$c4#$e3#$69#$44#$3e#$02,               'vpclmulqdq xmm7,xmm2,XMMWORD PTR [esi],$2');
  TestDis('vaesenc xmm1,xmm2,XMMWORD PTR [edi]',       #$c4#$e2#$69#$dc#$0f,                   'vaesenc xmm1,xmm2,XMMWORD PTR [edi]');
  TestDis('vaesenc xmm3,xmm7,XMMWORD PTR [esi]',       #$c4#$e2#$41#$dc#$1e,                   'vaesenc xmm3,xmm7,XMMWORD PTR [esi]');
  TestDis('vaesenclast xmm1,xmm2,XMMWORD PTR [edi]',   #$c4#$e2#$69#$dd#$0f,                   'vaesenclast xmm1,xmm2,XMMWORD PTR [edi]');
  TestDis('vaesenclast xmm1,xmm2,XMMWORD PTR [esi]',   #$c4#$e2#$69#$dd#$0e,                   'vaesenclast xmm1,xmm2,XMMWORD PTR [esi]');
  TestDis('vaesdec xmm1,xmm2,XMMWORD PTR [edi]',       #$c4#$e2#$69#$de#$0f,                   'vaesdec xmm1,xmm2,XMMWORD PTR [edi]');
  TestDis('vaesdeclast xmm1,xmm2,XMMWORD PTR [edi]',   #$c4#$e2#$69#$df#$0f,                   'vaesdeclast xmm1,xmm2,XMMWORD PTR [edi]');
  TestDis('pinsrw mm2,WORD PTR [edi],$2',              #$0f#$c4#$17#$02,                       'pinsrw mm2,WORD PTR [edi],$2');
  TestDis('pinsrw mm1,WORD PTR [edi],$c4',             #$0f#$c4#$0f#$c4,                       'pinsrw mm1,WORD PTR [edi],$c4');


  TestDis('vcvtph2ps xmm1,xmm2',                           #$c4#$e2#$79#$13#$ca,                           'vcvtph2ps xmm1,xmm2');
  TestDis('vcvtph2ps xmm1,xmm1',                           #$c4#$e2#$79#$13#$c9,                           'vcvtph2ps xmm1,xmm1');
  TestDis('vcvtph2ps xmm1,QWORD PTR [edi]',                #$c4#$e2#$79#$13#$0f,                           'vcvtph2ps xmm1,QWORD PTR [edi]');
  TestDis('vcvtph2ps xmm1,QWORD PTR [edi+$3]',             #$c4#$e2#$79#$13#$4f#$03,                       'vcvtph2ps xmm1,QWORD PTR [edi+$3]');
  TestDis('vcvtph2ps xmm1,QWORD PTR [eax+edi]',          #$c4#$e2#$79#$13#$0c#$07,                         'vcvtph2ps xmm1,QWORD PTR [eax+edi]');
  TestDis('vcvtph2ps xmm1,QWORD PTR [eax+edi+$3]',       #$c4#$e2#$79#$13#$4c#$07#$03,                     'vcvtph2ps xmm1,QWORD PTR [eax+edi+$3]');
  TestDis('vcvtph2ps xmm1,QWORD PTR [eax*2+edi]',          #$c4#$e2#$79#$13#$0c#$47,                       'vcvtph2ps xmm1,QWORD PTR [eax*2+edi]');
  TestDis('vcvtph2ps xmm1,QWORD PTR [eax*2+edi+$3]',       #$c4#$e2#$79#$13#$4c#$47#$03,                   'vcvtph2ps xmm1,QWORD PTR [eax*2+edi+$3]');
  TestDis('vcvtph2ps ymm1,xmm2',                           #$c4#$e2#$7d#$13#$ca,                           'vcvtph2ps ymm1,xmm2');
  TestDis('vcvtph2ps ymm1,xmm1',                           #$c4#$e2#$7d#$13#$c9,                           'vcvtph2ps ymm1,xmm1');
  TestDis('vcvtph2ps ymm1,XMMWORD PTR [edi]',              #$c4#$e2#$7d#$13#$0f,                           'vcvtph2ps ymm1,XMMWORD PTR [edi]');
  TestDis('vcvtph2ps ymm1,XMMWORD PTR [edi+$3]',           #$c4#$e2#$7d#$13#$4f#$03,                       'vcvtph2ps ymm1,XMMWORD PTR [edi+$3]');
  TestDis('vcvtph2ps ymm1,XMMWORD PTR [eax+edi]',        #$c4#$e2#$7d#$13#$0c#$07,                         'vcvtph2ps ymm1,XMMWORD PTR [eax+edi]');
  TestDis('vcvtph2ps ymm1,XMMWORD PTR [eax+edi+$3]',     #$c4#$e2#$7d#$13#$4c#$07#$03,                     'vcvtph2ps ymm1,XMMWORD PTR [eax+edi+$3]');
  TestDis('vcvtph2ps ymm1,XMMWORD PTR [eax*2+edi]',        #$c4#$e2#$7d#$13#$0c#$47,                       'vcvtph2ps ymm1,XMMWORD PTR [eax*2+edi]');
  TestDis('vcvtph2ps ymm1,XMMWORD PTR [eax*2+edi+$3]',     #$c4#$e2#$7d#$13#$4c#$47#$03,                   'vcvtph2ps ymm1,XMMWORD PTR [eax*2+edi+$3]');
  TestDis('vcvtph2ps zmm1,ymm2',                           #$62#$f2#$7d#$48#$13#$ca,                       'vcvtph2ps zmm1,ymm2');
  TestDis('vcvtph2ps zmm1,ymm1',                           #$62#$f2#$7d#$48#$13#$c9,                       'vcvtph2ps zmm1,ymm1');
  TestDis('vcvtph2ps zmm1,YMMWORD PTR [edi]',              #$62#$f2#$7d#$48#$13#$0f,                       'vcvtph2ps zmm1,YMMWORD PTR [edi]');
  TestDis('vcvtph2ps zmm1,YMMWORD PTR [edi+$3]',           #$62#$f2#$7d#$48#$13#$8f#$03#$00#$00#$00,       'vcvtph2ps zmm1,YMMWORD PTR [edi+$3]');
  TestDis('vcvtph2ps zmm1,YMMWORD PTR [eax+edi]',        #$62#$f2#$7d#$48#$13#$0c#$07,                     'vcvtph2ps zmm1,YMMWORD PTR [eax+edi]');
  TestDis('vcvtph2ps zmm1,YMMWORD PTR [eax+edi+$3]',     #$62#$f2#$7d#$48#$13#$8c#$07#$03#$00#$00#$00,     'vcvtph2ps zmm1,YMMWORD PTR [eax+edi+$3]');
  TestDis('vcvtph2ps zmm1,YMMWORD PTR [eax*2+edi]',        #$62#$f2#$7d#$48#$13#$0c#$47,                   'vcvtph2ps zmm1,YMMWORD PTR [eax*2+edi]');
  TestDis('vcvtph2ps zmm1,YMMWORD PTR [eax*2+edi+$3]',     #$62#$f2#$7d#$48#$13#$8c#$47#$03#$00#$00#$00,   'vcvtph2ps zmm1,YMMWORD PTR [eax*2+edi+$3]');
  TestDis('vcvtps2ph xmm2,xmm1,$35',                       #$c4#$e3#$79#$1d#$ca#$35,                       'vcvtps2ph xmm2,xmm1,$35');
  TestDis('vcvtps2ph xmm2,ymm1,$35',                       #$c4#$e3#$7d#$1d#$ca#$35,                       'vcvtps2ph xmm2,ymm1,$35');
  TestDis('vcvtps2ph ymm2,zmm1,$35',                       #$62#$f3#$7d#$48#$1d#$ca#$35,                   'vcvtps2ph ymm2,zmm1,$35');
  TestDis('vcvtps2ph QWORD PTR [edi],xmm3,$35',            #$c4#$e3#$79#$1d#$1f#$35,                       'vcvtps2ph QWORD PTR [edi],xmm3,$35');
  TestDis('vcvtps2ph XMMWORD PTR [edi],ymm3,$35',          #$c4#$e3#$7d#$1d#$1f#$35,                       'vcvtps2ph XMMWORD PTR [edi],ymm3,$35');
  TestDis('vcvtps2ph YMMWORD PTR [edi],zmm3,$35',          #$62#$f3#$7d#$48#$1d#$1f#$35,                   'vcvtps2ph YMMWORD PTR [edi],zmm3,$35');
  TestDis('vcvtps2ph QWORD PTR [edi+$3],xmm3,$35',         #$c4#$e3#$79#$1d#$5f#$03#$35,                   'vcvtps2ph QWORD PTR [edi+$3],xmm3,$35');
  TestDis('vcvtps2ph XMMWORD PTR [edi+$3],ymm3,$35',       #$c4#$e3#$7d#$1d#$5f#$03#$35,                   'vcvtps2ph XMMWORD PTR [edi+$3],ymm3,$35');
  TestDis('vcvtps2ph YMMWORD PTR [edi+$3],zmm3,$35',       #$62#$f3#$7d#$48#$1d#$9f#$03#$00#$00#$00#$35,   'vcvtps2ph YMMWORD PTR [edi+$3],zmm3,$35');
  TestDis('vcvtps2ph QWORD PTR [eax+edi],xmm3,$35',      #$c4#$e3#$79#$1d#$1c#$07#$35,                     'vcvtps2ph QWORD PTR [eax+edi],xmm3,$35');
  TestDis('vcvtps2ph XMMWORD PTR [eax+edi],ymm3,$35',    #$c4#$e3#$7d#$1d#$1c#$07#$35,                     'vcvtps2ph XMMWORD PTR [eax+edi],ymm3,$35');
  TestDis('vcvtps2ph YMMWORD PTR [eax+edi],zmm3,$35',    #$62#$f3#$7d#$48#$1d#$1c#$07#$35,                 'vcvtps2ph YMMWORD PTR [eax+edi],zmm3,$35');
  TestDis('vcvtps2ph QWORD PTR [eax+edi+$3],xmm3,$35',   #$c4#$e3#$79#$1d#$5c#$07#$03#$35,                 'vcvtps2ph QWORD PTR [eax+edi+$3],xmm3,$35');
  TestDis('vcvtps2ph XMMWORD PTR [eax+edi+$3],ymm3,$35', #$c4#$e3#$7d#$1d#$5c#$07#$03#$35,                 'vcvtps2ph XMMWORD PTR [eax+edi+$3],ymm3,$35');
  TestDis('vcvtps2ph YMMWORD PTR [eax+edi+$3],zmm3,$35', #$62#$f3#$7d#$48#$1d#$9c#$07#$03#$00#$00#$00#$35, 'vcvtps2ph YMMWORD PTR [eax+edi+$3],zmm3,$35');
  TestDis('vcvtps2ph QWORD PTR [eax*2+edi],xmm3,$35',      #$c4#$e3#$79#$1d#$1c#$47#$35,                   'vcvtps2ph QWORD PTR [eax*2+edi],xmm3,$35');
  TestDis('vcvtps2ph XMMWORD PTR [eax*2+edi],ymm3,$35',    #$c4#$e3#$7d#$1d#$1c#$47#$35,                   'vcvtps2ph XMMWORD PTR [eax*2+edi],ymm3,$35');
  TestDis('vcvtps2ph YMMWORD PTR [eax*2+edi],zmm3,$35',    #$62#$f3#$7d#$48#$1d#$1c#$47#$35,               'vcvtps2ph YMMWORD PTR [eax*2+edi],zmm3,$35');
  TestDis('vcvtps2ph QWORD PTR [eax*2+edi+$3],xmm3,$35',   #$c4#$e3#$79#$1d#$5c#$47#$03#$35,               'vcvtps2ph QWORD PTR [eax*2+edi+$3],xmm3,$35');
  TestDis('vcvtps2ph XMMWORD PTR [eax*2+edi+$3],ymm3,$35', #$c4#$e3#$7d#$1d#$5c#$47#$03#$35,               'vcvtps2ph XMMWORD PTR [eax*2+edi+$3],ymm3,$35');
  TestDis('vcvtps2ph YMMWORD PTR [eax*2+edi+$3],zmm3,$35', #$62#$f3#$7d#$48#$1d#$9c#$47#$03#$00#$00#$00#$35, 'vcvtps2ph YMMWORD PTR [eax*2+edi+$3],zmm3,$35');
  IgnoreSizeWordPtr := False;


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

