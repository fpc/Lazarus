{ Command line utility to create closed source Lazarus packages.
  Run with -h to see help

  Copyright (C) 2013 Mattias Gaertner mattias@freepascal.org

  This library is free software; you can redistribute it and/or modify it
  under the terms of the GNU Library General Public License as published by
  the Free Software Foundation; either version 2 of the License, or (at your
  option) any later version with the following modification:

  As a special exception, the copyright holders of this library give you
  permission to link this library with independent modules to produce an
  executable, regardless of the license terms of these independent modules,and
  to copy and distribute the resulting executable under terms of your choice,
  provided that you also meet, for each linked independent module, the terms
  and conditions of the license of that module. An independent module is a
  module which is not derived from or based on this library. If you modify
  this library, you may extend this exception to your version of the library,
  but you are not obligated to do so. If you do not wish to do so, delete this
  exception statement from your version.

  This program is distributed in the hope that it will be useful, but WITHOUT
  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
  FITNESS FOR A PARTICULAR PURPOSE. See the GNU Library General Public License
  for more details.

  You should have received a copy of the GNU Library General Public License
  along with this library; if not, write to the Free Software Foundation,
  Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
}
program sourcecloser;

{$mode objfpc}{$H+}

uses
  Classes, SysUtils, CustApp,
  // LazUtils
  AvgLvlTree, LazLogger, LazFileUtils, Laz2_XMLCfg, LazUTF8,
  // CodeTools
  FileProcs, BasicCodeTools, CodeToolManager, CodeCache, SourceChanger,
  CodeTree, DefineTemplates, LinkScanner;

type

  { TTarget }

  TTarget = class
  public
    Names, Values: array of string;
    procedure Add(Name, Value: string);
    function AsString: string;
  end;

  { TSourceCloser }

  TSourceCloser = class(TCustomApplication)
  private
    FClosedSrcError: TStringList;
    FCompilerOptions: string;
    FDefines: TStringToStringTree;
    FDisableCompile: boolean;
    FIncludePath: string;
    FLPKFilenames: TStrings;
    FRemovePrivateSections: boolean;
    FSyntaxMode: TCompilerMode;
    FUndefines: TStringToStringTree;
    FUnitFilenames: TStrings;
    FVerbosity: integer;
    fDefinesApplied: boolean;
    procedure DeleteNode(Tool: TCodeTool; Node: TCodeTreeNode;
      const StartPos, EndPos: Integer;
      Changer: TSourceChangeCache; AddEmptyLine: boolean);
  protected
    procedure DoRun; override;
    procedure ApplyDefines;
    procedure ConvertLPK(LPKFilename: string);
    procedure ConvertUnit(UnitFilename: string);
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure WriteHelp; virtual;
    property Verbosity: integer read FVerbosity write FVerbosity;
    property SyntaxMode: TCompilerMode read FSyntaxMode write FSyntaxMode;
    property Defines: TStringToStringTree read FDefines;
    property Undefines: TStringToStringTree read FUndefines;
    property IncludePath: string read FIncludePath write FIncludePath;
    property LPKFilenames: TStrings read FLPKFilenames;
    property UnitFilenames: TStrings read FUnitFilenames;
    property CompilerOptions: string read FCompilerOptions write FCompilerOptions;
    property RemovePrivateSections: boolean read FRemovePrivateSections write FRemovePrivateSections;
    property ClosedSrcError: TStringList read FClosedSrcError write FClosedSrcError;
    property DisableCompile: boolean read FDisableCompile write FDisableCompile;
  end;

function IndexOfFilename(List: TStrings; Filename: string): integer;
begin
  Result:=List.Count-1;
  while (Result>=0) and (CompareFilenames(List[Result],Filename)<>0) do
    dec(Result);
end;

{ TTarget }

procedure TTarget.Add(Name, Value: string);
var
  Cnt: Integer;
begin
  Cnt:=length(Names);
  SetLength(Names,Cnt+1);
  Names[Cnt]:=Name;
  SetLength(Values,Cnt+1);
  Values[Cnt]:=Value;
end;

function TTarget.AsString: string;
var
  i: Integer;
begin
  Result:='';
  for i:=0 to length(Names)-1 do begin
    if Result<>'' then Result+=',';
    Result+=Names[i]+'='+Values[i];
  end;
end;

{ TSourceCloser }

procedure TSourceCloser.DeleteNode(Tool: TCodeTool; Node: TCodeTreeNode;
  const StartPos, EndPos: Integer; Changer: TSourceChangeCache;
  AddEmptyLine: boolean);

  procedure E(Msg: string);
  begin
    writeln('ERROR: '+Msg);
    Halt(1);
  end;

var
  EndCodePos: TCodePosition;
  StartCodePos: TCodePosition;
  s: String;
begin
  //debugln(['TSourceCloser.DeleteNode ',Node.DescAsString,' "',dbgstr(Tool.Src,StartPos,EndPos-StartPos),'"']);
  if not Tool.CleanPosToCodePos(StartPos, StartCodePos) then
    E('unable to delete '+Node.DescAsString+' of "'+Tool.MainFilename+'" (invalid startpos '
      +Tool.CleanPosToStr(StartPos, true)+')');
  if not Tool.CleanPosToCodePos(EndPos, EndCodePos) then
    E('unable to delete '+Node.DescAsString+' of "'+Tool.MainFilename+'" (invalid endpos '+
      Tool.CleanPosToStr(EndPos, true)+')');
  if StartCodePos.Code<>EndCodePos.Code then
    E('unable to delete '+Node.DescAsString+' of "'+Tool.MainFilename+'" from '+
      Tool.CleanPosToStr(StartPos, true)+' to '+Tool.CleanPosToStr(EndPos, true)
      );
  s:='';
  if AddEmptyLine then
    s:=LineEnding;
  if not Changer.ReplaceEx(gtNone, gtNone, 0, 0, StartCodePos.Code,
    StartCodePos.P, EndCodePos.P, s) then
    E('unable to delete '+Node.DescAsString+' of "'+Tool.MainFilename+'"');
end;

procedure TSourceCloser.DoRun;
const
  ShortOpts = 'hvqc:pM:d:u:i:ke:';
  LongOpts = 'help verbose quiet compileroptions: disablecompile mode: define: undefine: includepath: keepprivate error:';
var
  ErrMsgIsDefault: Boolean;

  procedure E(Msg: string; WithHelp: boolean = false);
  begin
    DebugLn(['ERROR: ',Msg]);
    if WithHelp then
      WriteHelp;
    Terminate;
    Halt(1);
  end;

  procedure ParseValueParam(ShortOpt: char; Value: string);
  var
    m: TCompilerMode;
  begin
    case ShortOpt of
    'c':
      begin
        FCompilerOptions+=Value;
      end;
    'e':
      begin
        if ErrMsgIsDefault then begin
          ErrMsgIsDefault:=false;
          FClosedSrcError.Clear;
        end;
        Value:=UTF8Trim(Value,[]);
        if Value='' then exit;
        FClosedSrcError.Add(Value);
      end;
    'i':
      begin
        Value:=UTF8Trim(Value,[]);
        if Value='' then exit;
        if IncludePath<>'' then
          Value:=';'+Value;
        fIncludePath+=Value;
      end;
    'M':
      begin
      for m in TCompilerMode do
        if SameText(CompilerModeNames[m],Value) then begin
          SyntaxMode:=m;
          exit;
        end;
      E('invalid syntaxmode "'+Value+'"');
      end;
    'd':
      begin
        if not IsValidIdent(Value) then
          E('invalid define:'+Value);
        Defines[Value]:='';
      end;
    'u':
      begin
        if not IsValidIdent(Value) then
          E('invalid define:'+Value);
        Defines[Value]:='';
      end;
    else
      E('invalid option "'+ShortOpt+'"');
    end;
  end;

  procedure AddFile(aFilename: string);
  var
    Ext: String;
  begin
    debugln(['AddFile ',aFilename]);
    Ext:=lowercase(ExtractFileExt(aFilename));
    if Ext='.lpk' then begin
      if IndexOfFilename(LPKFilenames,aFilename)>=0 then
        E('duplicate lpk:'+aFilename); // duplicate lpk, compilation order is unclear => error
      LPKFilenames.Add(aFilename);
    end
    else if FilenameIsPascalUnit(aFilename) then begin
      if IndexOfFilename(UnitFilenames,aFilename)>=0 then
        exit; // duplicate unit is ok => ignore
      UnitFilenames.Add(aFilename);
    end else
      E('only lpk and units are supported, invalid file:'+aFilename);
  end;

  procedure AddFiles(Pattern: string);
  var
    Info: TSearchRec;
  begin
    if FindFirstUTF8(Pattern,faAnyFile,Info)=0 then begin
      repeat
        if (Info.Name='.') or (Info.Name='..') then continue;
        if (faDirectory and Info.Attr)>0 then continue;
        AddFile(ExtractFilePath(Pattern)+Info.Name);
      until FindNextUTF8(Info)<>0;
    end;
    FindCloseUTF8(Info);
  end;

var
  ErrorMsg: String;
  i: Integer;
  Param: String;
  S2SItem: PStringToStringItem;
  Filename: String;
  Option: string;
  p: SizeInt;
begin
  // quick check parameters
  ErrorMsg:=CheckOptions(ShortOpts,LongOpts);
  if ErrorMsg<>'' then
    E(ErrorMsg);

  // parse parameters
  if HasOption('h','help') then begin
    WriteHelp;
    Terminate;
    Exit;
  end;

  ErrMsgIsDefault:=true;
  i:=1;
  while i<=System.ParamCount do begin
    Param:=ParamStrUTF8(i);
    inc(i);
    if Param='' then continue;
    if (Param='-q') or (Param='--quiet') then
      dec(fVerbosity)
    else if (Param='-v') or (Param='--verbose') then
      inc(fVerbosity)
    else if (Param='-k') or (Param='--keepprivate') then
      RemovePrivateSections:=false
    else if (Param='-p') or (Param='--disablecompile') then
      DisableCompile:=true
    else if Param[1]<>'-' then begin
      Filename:=TrimAndExpandFilename(Param);
      if (Pos('*',ExtractFileName(Filename))>0) or (Pos('?',ExtractFileName(Filename))>0)
      then begin
        AddFiles(Filename);
      end else begin
        if not FileExistsUTF8(Filename) then
          E('file not found: '+Param);
        if DirPathExists(Filename) then
          E('file is a directory: '+Param);
        AddFile(Filename);
      end;
    end else if (length(Param)=2) and (Pos(Param[2]+':',ShortOpts)>0) then begin
      // e.g. -t <target>
      Option:=Param[2];
      if i>System.ParamCount then
        E('missing value for option: '+Param);
      Param:=ParamStrUTF8(i);
      inc(i);
      ParseValueParam(Option[1],Param);
    end else if (copy(Param,1,2)='--') then begin
      p:=Pos('=',Param);
      if p<1 then
        E('invalid long option syntax: '+Param);
      Option:=copy(Param,3,p-3);
      delete(Param,1,p);
      if Option='compileroptions' then Option:='c'
      else if Option='mode' then Option:='M'
      else if Option='define' then Option:='d'
      else if Option='undefine' then Option:='u'
      else if Option='includepath' then Option:='i'
      else if Option='error' then Option:='e'
      else
        E('invalid long option');
      ParseValueParam(Option[1],Param);
    end else
      E('invalid option: '+Param);
  end;

  if Verbosity>0 then begin
    debugln(['Options:']);
    debugln(['Verbosity=',Verbosity]);
    debugln(['IncludePath=',IncludePath]);
    for S2SItem in Defines do
      debugln('Define:',S2SItem^.Name);
    for S2SItem in Undefines do
      debugln('Undefine:',S2SItem^.Name);
    for i:=0 to LPKFilenames.Count-1 do
      debugln(['LPK[',i+1,']:',LPKFilenames[i]]);
    for i:=0 to UnitFilenames.Count-1 do
      debugln(['Unit[',i+1,']:',UnitFilenames[i]]);
  end;
  if (LPKFilenames.Count=0) and (UnitFilenames.Count=0) then
    E('you must pass at least one lpk or pas file',true);

  for i:=0 to LPKFilenames.Count-1 do
    ConvertLPK(LPKFilenames[i]);
  for i:=0 to UnitFilenames.Count-1 do
    ConvertUnit(UnitFilenames[i]);

  // stop program loop
  Terminate;
end;

procedure TSourceCloser.ApplyDefines;

  procedure AddDefine(const MacroName, Value: string);
  var
    DefTemplate: TDefineTemplate;
  begin
    DefTemplate:=TDefineTemplate.Create('Define '+MacroName,
      'Define '+MacroName,
      MacroName,Value,da_DefineRecurse);
    CodeToolBoss.DefineTree.Add(DefTemplate);
  end;

  procedure AddUndefine(const MacroName: string);
  var
    DefTemplate: TDefineTemplate;
  begin
    DefTemplate:=TDefineTemplate.Create('Undefine '+MacroName,
      'Undefine '+MacroName,
      MacroName,'',da_UndefineRecurse);
    CodeToolBoss.DefineTree.Add(DefTemplate);
  end;

  procedure AddDefineUndefine(const AName: string; Define: boolean);
  begin
    if Define then
      AddDefine(AName,'')
    else
      AddUndefine(AName);
  end;

var
  IncPathTemplate: TDefineTemplate;
  S2SItem: PStringToStringItem;
  m: Integer;
begin
  if fDefinesApplied then exit;
  fDefinesApplied:=true;

  CodeToolBoss.SimpleInit('codetools.cache');

  if IncludePath<>'' then begin
    IncPathTemplate:=TDefineTemplate.Create('IncPath',
      'extending include search path',
      IncludePathMacroName,  // variable name: #IncPath
      '$('+IncludePathMacroName+');'+IncludePath
      ,da_DefineRecurse
      );
    CodeToolBoss.DefineTree.Add(IncPathTemplate);
  end;

  for m:=low(FPCSyntaxModes) to high(FPCSyntaxModes) do
    AddDefineUndefine('FPC_'+FPCSyntaxModes[m],
      SameText(CompilerModeNames[SyntaxMode],FPCSyntaxModes[m]));

  for S2SItem in Defines do
    AddDefine(S2SItem^.Name,S2SItem^.Value);
  for S2SItem in Undefines do
    AddUndefine(S2SItem^.Name);
end;

procedure TSourceCloser.ConvertLPK(LPKFilename: string);
// set lpk to compile only manually
// add -Ur to compiler options
const
  OtherPath='Package/CompilerOptions/Other/';
  CustomOptionsPath=OtherPath+'CustomOptions/Value';
var
  xml: TXMLConfig;
  CustomOptions: String;
  NewOptions: String;
begin
  debugln(['Converting lpk: ',LPKFilename]);
  xml:=TXMLConfig.Create(LPKFilename);
  try
    // set lpk to compile only manually
    xml.SetValue('Package/AutoUpdate/Value','Manually');

    // add -Ur to compiler options
    CustomOptions:=xml.GetValue(CustomOptionsPath,'');
    NewOptions:=CustomOptions;
    if Pos('-Ur',NewOptions)<1 then begin
      if NewOptions<>'' then NewOptions+=' ';
      NewOptions+='-Ur';
    end;
    if FCompilerOptions<>'' then begin
      if NewOptions<>'' then NewOptions+=' ';
      NewOptions+=FCompilerOptions;
    end;
    xml.SetValue(CustomOptionsPath,NewOptions);

    // disable compile commands
    if DisableCompile then begin
      xml.SetValue(OtherPath+'CompilerPath/Value','');
      xml.SetDeleteValue(OtherPath+'ExecuteBefore/Command/Value','','');
      xml.SetDeleteValue(OtherPath+'ExecuteAfter/Command/Value','','');
      xml.SetDeleteValue(OtherPath+'CreateMakefileOnBuild/Value','','');
    end;

    // write
    xml.Flush;
  finally
    xml.Free;
  end;
end;

procedure TSourceCloser.ConvertUnit(UnitFilename: string);

  procedure E(Msg: string);
  begin
    writeln('ERROR: '+Msg);
    Halt(1);
  end;

var
  Code: TCodeBuffer;
  Changer: TSourceChangeCache;
  Tool: TCodeTool;
  Node: TCodeTreeNode;
  StartPos: Integer;
  EndPos: Integer;
  CodeList: TFPList;
  i: Integer;
  FromPos: Integer;
  ToPos: Integer;
  AddEmptyLine: Boolean;
  s: String;
begin
  debugln(['Converting unit: ',UnitFilename]);
  ApplyDefines;

  // load file
  Code:=CodeToolBoss.LoadFile(UnitFilename,true,false);
  if Code=nil then
    E('unable to read "'+UnitFilename+'"');
  // parse whole unit
  if (not CodeToolBoss.Explore(Code,Tool,false)) or (CodeToolBoss.ErrorMessage<>'') then
    E('parse error');
  if Tool.GetSourceType<>ctnUnit then
    E('not a unit, skipping "'+Code.Filename+'"');

  // init SourceChangeCache
  Changer:=CodeToolBoss.SourceChangeCache;
  Changer.MainScanner:=Tool.Scanner;

  // add errors, so that user cannot accidentally compile the unit
  s:='';
  for i:=0 to FClosedSrcError.Count-1 do begin
    s:=s+'{$Error '+FClosedSrcError[i]+'}'+LineEnding;
  end;
  if s<>'' then
    Changer.ReplaceEx(gtNone,gtNone,0,0,Code,1,1,s);

  if RemovePrivateSections then begin
    // delete private sections in the interface
    Node:=Tool.FindInterfaceNode;
    while Node<>nil do begin
      if Node.Desc=ctnClassPrivate then begin
        FromPos:=Tool.FindLineEndOrCodeInFrontOfPosition(Node.StartPos);
        ToPos:=Tool.FindLineEndOrCodeInFrontOfPosition(Node.EndPos);
        DeleteNode(Tool,Node,FromPos,ToPos,Changer,false);
        Node:=Node.NextSkipChilds;
      end else
        Node:=Node.Next;
      if Node=nil then break;
      if Node.Desc in [ctnImplementation,ctnInitialization,ctnFinalization] then
        break;
    end;
  end;

  // delete implementation, initialization and finalization section
  Node:=Tool.Tree.Root;
  while (Node<>nil) do begin
    if Node.Desc=ctnImplementation then begin
      // delete implementation section excluding the 'implementation' keyword
      StartPos:=Node.StartPos+length('implementation');
      if Node.NextBrother.Desc=ctnEndPoint then
        EndPos:=Node.EndPos
      else
        EndPos:=Node.NextBrother.StartPos;
      DeleteNode(Tool, Node, StartPos, EndPos, Changer, true);
    end else if Node.Desc in [ctnInitialization,ctnFinalization] then begin
      // delete the content of the finalization and initialization section
      Tool.MoveCursorToNodeStart(Node);
      Tool.ReadNextAtom; // read 'initialization'
      StartPos:=Tool.CurPos.EndPos;
      EndPos:=Node.NextBrother.StartPos;
      AddEmptyLine:=true;
      if Trim(copy(Tool.Src,StartPos,EndPos-StartPos))='' then begin
        // empty initialization => delete keyword as well
        StartPos:=Node.StartPos;
        AddEmptyLine:=false;
      end else begin
        // initialization section is not empty
        // => keep the keyword, because it is needed by tools like 'Unused units'
      end;
      DeleteNode(Tool, Node, StartPos, EndPos, Changer, AddEmptyLine);
    end;
    Node:=Node.NextBrother;
  end;

  // apply changes and write changes to disk
  CodeList:=TFPList.Create;
  try
    for i:=0 to Changer.BuffersToModifyCount-1 do
      CodeList.Add(Changer.BuffersToModify[i]);
    if not Changer.Apply then
      E('unable to modify "'+UnitFilename+'"');
    for i:=0 to CodeList.Count-1 do begin
      Code:=TCodeBuffer(CodeList[i]);
      if not Code.Save then
        E('unable to write "'+Code.Filename+'"');
    end;
  finally
    CodeList.Free;
  end;
end;

constructor TSourceCloser.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  StopOnException:=True;
  FSyntaxMode:=cmOBJFPC;
  fDefines:=TStringToStringTree.Create(false);
  FUndefines:=TStringToStringTree.Create(false);
  FLPKFilenames:=TStringList.Create;
  FUnitFilenames:=TStringList.Create;
  FRemovePrivateSections:=true;
  FClosedSrcError:=TStringList.Create;
  FClosedSrcError.Add('This is a closed source unit. You cannot compile it, it was already compiled.');
  FClosedSrcError.Add('Probably the IDE has cleaned up and you have to unpack the zip again.');
end;

destructor TSourceCloser.Destroy;
begin
  FreeAndNil(FClosedSrcError);
  FreeAndNil(FLPKFilenames);
  FreeAndNil(FUnitFilenames);
  FreeAndNil(FDefines);
  FreeAndNil(FUndefines);
  inherited Destroy;
end;

procedure TSourceCloser.WriteHelp;
var
  i: Integer;
begin
  writeln('Usage:');
  writeln('  ',ExeName,' -h');
  writeln('  ',ExeName,' [options] [unit1.pas unit2.pp ...] [pkg1.lpk pk2.lpk ..]');
  writeln;
  writeln('Description:');
  writeln('  This tool helps creating closed source Lazarus packages.');
  writeln;
  writeln('  If the file names contain * or ? it will be used as mask.');
  writeln('  If you pass a lpk file, this tool will set "compile manually"');
  writeln('  and appends "-Ur" to the compiler options.');
  writeln('  You can pass multiple lpk files and they will be edited in this order.');
  writeln('  If you pass a .pas or .pp file it will be treated as a pascal unit and');
  writeln('  will remove the implementation, initialization, finalization sections.');
  writeln;
  writeln('Options:');
  writeln('  -h, --help    : This help messages.');
  writeln('  -v, --verbose : be more verbose.');
  writeln('  -q, --quiet   : be more quiet.');
  writeln('Package/lpk options:');
  writeln('  --compileroptions=<compiler options>');
  writeln('          Add custom compiler options to lpk.');
  writeln('  -p, --disablecompile');
  writeln('          Remove all compile commands from lpk.');
  writeln('Unit options:');
  writeln('  -M <SyntaxMode>, --mode=<SyntaxMode> : delphi|delphiunicode|objfpc|fpc|macpas, default: '+CompilerModeNames[SyntaxMode]);
  writeln('  -d <MacroName>, --define=<MacroName> :');
  writeln('          Define Free Pascal macro. Can be passed multiple times.');
  writeln('  -u <MacroName>, --undefine=<MacroName> :');
  writeln('          Undefine Free Pascal macro. Can be passed multiple times.');
  writeln('  -i <path>, --includepath=<path> :');
  writeln('         Append <path> to include search path. Can be passed multiple times.');
  writeln('  -k, --keepprivate');
  writeln('         Keep private sections in interface.');
  writeln('  -e <errormessage>, --error=<error message>');
  writeln('         Change the message of the error directive added to the units.');
  writeln('         You can add this multiple times to add multiple directives.');
  writeln('         The default error message is:');
  for i:=0 to FClosedSrcError.Count-1 do
    writeln('   ',FClosedSrcError[i]);
  writeln;
  writeln('Environment variables:');
  writeln('  PP            path to compiler,');
  writeln('                e.g. C:\lazarus\fpc\2.6.2\bin\i386-win32\fpc.exe');
  writeln('                The compiler is queried for the current defines for the');
  writeln('                target platform.');
  writeln('  FPCTARGET     target os, e.g. win32');
  writeln('  FPCTARGETCPU  target cpu, e.g. i386');
end;

var
  Application: TSourceCloser;
begin
  Application:=TSourceCloser.Create(nil);
  Application.Title:='SourceCloser';
  Application.Run;
  Application.Free;
end.

