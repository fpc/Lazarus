{
gir2pascal.lpr
Copyright (C) 2011  Andrew Haines andrewd207@aol.com

This program is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public License
as published by the Free Software Foundation; either version 2
of the License, or (at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.
}
program gir2pascal;

{$mode objfpc}{$H+}
{ $DEFINE CreatePascalClasses}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, SysUtils,CommandLineOptions, DOM, XMLRead, girNameSpaces, girFiles,
  process,
  girpascalwriter, girErrors, girCTypesMapping, girTokens, girObjects,
  girPascalClassWriter, girpascalwritertypes{$IFDEF UNIX}, baseunix, termio{$ENDIF};


type

  { TGirConsoleConverter }

  TGirConsoleConverter = class
  private
    FCmdOptions: TCommandLineOptions;
    FWriteCount: Integer;
    FPaths: TStringList;
    FOutPutDirectory : String;
    FFileToConvert: String;
    FUnitPrefix: String;
    FOverWriteFiles: Boolean;
    FOptions: TgirOptions;
    FEnumImpl: TgirEnumImpl;
    procedure AddDefaultPaths;
    procedure AddPaths(APaths: String);
    procedure VerifyOptions;
    procedure Convert;

    // options
    function CheckOptions: String;

    //callbacks
    function NeedGirFile(AGirFile: TObject; NamespaceName: String) : TXMLDocument;
    // AName is the whole name unit.pas or file.c
    procedure WriteFile(Sender: TObject;  AName: String; AStream: TStringStream);
    procedure Terminate;
  protected
    function LoadFile(const FileName: String): TXMLDocument;
    procedure DoRun; //override;
  public
    constructor Create;
    destructor Destroy; override;
    procedure WriteHelp; virtual;
    procedure Run;
  end;


{ TGirConsoleConverter }

procedure TGirConsoleConverter.AddDefaultPaths;
const
{$ifdef Linux}
{$ifdef CpuArmHF}
  TargetArchLibC = '-gnueabihf';
{$endif CpuArmHF}
{$ifdef CpuArmEL}
  TargetArchLibC = '-gnueabi';
{$endif CpuArmEL}
{$ifndef CpuArm}
  TargetArchLibC = '-gnu';
{$endif CpuArm}
  TargetOs = 'linux'; {FpcTargetOs returns Linux instead of linux}
{$else Linux}
  TargetArchLibC = '';
  TargetOs = {$Include %FpcTargetOs%};
{$endif Linux}
  TargetCpu = {$Include %FpcTargetCpu%};
  TargetSystem = TargetOs + TargetArchLibC;
  TargetMultiarch = TargetCpu + '-' + TargetSystem;

var
  DefaultSystemPaths: String;

begin
  DefaultSystemPaths := '/usr/lib/' + TargetMultiarch + '/girepository-1.0' +
     FPaths.Delimiter + '/usr/share/girepository-1.0' +
     FPaths.Delimiter + '/usr/share/gir-1.0';
  AddPaths(DefaultSystemPaths);
end;

procedure TGirConsoleConverter.AddPaths(APaths: String);
var
  Strs: TStringList;
  Str: String;
  Path: String;
begin
  Strs := TStringList.Create;
  Strs.Delimiter := FPaths.Delimiter;
  Strs.StrictDelimiter:=True;
  Strs.DelimitedText:=APaths;

  // so we can add the delimiter
  for Str in Strs do begin
    Path := IncludeTrailingPathDelimiter(Str);
    if FPaths.IndexOf(Path) < 0 then begin
      FPaths.Add(Path);
    end;
  end;

  Strs.Free;
end;

procedure TGirConsoleConverter.VerifyOptions;
begin
  if not ForceDirectories(FOutPutDirectory) then
  begin
    WriteLn(Format('Could not create output directory "%s"!', [FOutPutDirectory]));
    Terminate;
  end;
  if FFileToConvert = '' then
  begin
    WriteLn('No input file specified! See -h for options.');
    Terminate;
    Halt;
  end;
  if FCmdOptions.HasOption('objects') and FCmdOptions.HasOption('classes') then
  begin
    WriteLn('Cannot use options ''--objects'' and ''--classes'' together!.');
    Terminate;
    Halt;
  end;
end;

function TGirConsoleConverter.NeedGirFile(AGirFile: TObject; NamespaceName: String): TXMLDocument;
const
  ext: Array of String = ('.gir', '.typelib');
var
  Path: String;
  fn: String;
  n: Integer;
begin
  WriteLn('Looking for gir file: ', NamespaceName);
  Result := nil;
  for Path in FPaths do
  begin
    WriteLn('Looking in path: ', Path);
    for n := Low(ext) to High(ext) do begin
      fn := Path + NamespaceName + ext[n];
      WriteLn('Searching for: ', fn);
      if FileExists(fn) then
      begin
        Exit(LoadFile(fn));
      end;
    end;
  end;
  if Result = nil then
    WriteLn('Fatal: Unable to find gir file: ',NamespaceName);
end;

procedure TGirConsoleConverter.WriteFile(Sender: TObject; AName: String; AStream: TStringStream);
var
  SStream: TFileStream;
  OutFileName: String;
begin
  Inc(FWriteCount);
  OutFileName:=FOutPutDirectory+LowerCase(AName);
  if not FileExists(OutFileName)
  or (FileExists(OutFileName) and FOverWriteFiles) then
  begin
    WriteLn(Format('Writing: %s', [OutFileName]));
    AStream.Position:=0;
    ForceDirectories(FOutPutDirectory);
    SStream := TFileStream.Create(OutFileName, fmCreate or fmOpenReadWrite);
    SStream.CopyFrom(AStream,AStream.Size);
    SStream.Free;
    AStream.Free;
  end
  else
  begin
    WriteLn(Format('File %s already exists! Stopping.', [OutFileName]));
    Terminate;
    Halt;
  end;
end;

procedure TGirConsoleConverter.Terminate;
begin
  Halt(1);
end;

procedure TGirConsoleConverter.Convert;
var
  Doc: TXMLDocument;
  girFile: TgirFile;
  Writer: TgirPascalWriter;
  StartTime, EndTime:TDateTime;
begin
  StartTime := Now;
  Doc := LoadFile(FFileToConvert);

  girFile := TgirFile.Create(Self, FCmdOptions);
  girFile.OnNeedGirFile:=@NeedGirFile;
  girFile.ParseXMLDocument(Doc);
  Doc.Free;

  Writer := TgirPascalWriter.Create(girFile.NameSpaces, FOptions, FUnitPrefix);
  Writer.OnUnitWriteEvent:= @WriteFile;
  Writer.GenerateUnits;

  Writer.Free;
  EndTime := Now;

  EndTime := EndTime-StartTime;
  WriteLn(Format('Converted %d file(s) in %f seconds',[FWriteCount, DateTimeToTimeStamp(EndTime).Time / 1000]));
end;

function TGirConsoleConverter.CheckOptions: String;
begin
  Result := '';
  //FCmdOptions.SetOptions(ShortOpts, LongOpts);
  with FCmdOptions do
  begin
    AddOption(['h', 'help'], False ,'Show this help message.');
    AddOption(['i', 'input'], True ,'.gir filename to convert.');
    AddOption(['o', 'output-directory'], True ,'Directory to write the resulting .pas files to. If not specified then the current working directory is used.');
    AddOption(['D', 'dynamic'], False , 'Use unit dynlibs and link at runtime');
    {$IFDEF CreatePascalClasses}
    AddOption(['s', 'seperate-units'], False ,'Creates seperate units for each gir file: (xConsts, xTypes, xFunctions, [xClasses, xObjects].');

    AddOption(['C', 'classes'], False ,'Create Pascal classes that envelope/wrap the GObjects. Also forces ''-s''');
    AddOption(['O', 'objects'], False ,'OPTION NOT IMPLEMENTED YET. See Note below. '+
                               'Creates a seperate unit for pascal Objects (not classes). Forces ''-s'' '+
                               'Note: If -C or -O are not used then pascal Objects and consts '+
                               'are in a single unit.');
    {$ENDIF CreatePascalClasses}
    AddOption(['N', 'no-wrappers'], False ,'Do not create wrappers for objects.');
    AddOption(['w', 'overwrite-files'], False ,'If the output .pas file(s) already exists then overwrite them.');
    AddOption(['n', 'no-default'], False ,'/usr/share/gir-1.0 is not added as a search location for needed .gir files.');
    AddOption(['p', 'paths'], True ,'List of paths seperated by ":" to search for needed .gir files.');
    AddOption(['d', 'deprecated'], False, 'Include fields and methods marked as deprecated.');
    AddOption(['t', 'test'], False ,'Creates a test program per unit to verify struct sizes.');
    AddOption(['P', 'unit-prefix'], True, 'Set a prefix to be added to each unitname.');
    AddOption(['M', 'max-version'], True, 'Do not include symbols introduced after <max-version>. Can be used multiple times. i.e "-M gtk-3.12 -M glib-2.23"');
    AddOption(['k', 'keep-deprecated-version'], True, 'Include deprecated symbols that are >= to $version. Uses the same format as --max-version. Has no effect if --deprecated is defined');
    AddOption(['e', 'declare-enums-as'], True, 'Declare C enums as either IntConst, TypedIntConst, IntAliasConst, Enum or Set');
  end;
  FCmdOptions.ReadOptions;
  if FCmdOptions.OptionsMalformed then
    REsult := 'Error reading arguments';
end;

procedure TGirConsoleConverter.DoRun;
var
  EnumImpl: TgirEnumImpl;
  ErrorCode: Word;
begin
  // quick check parameters
  CheckOptions;//('hnp:o:i:wtDCsO',['help','no-default','paths','output-directory', 'input', 'overwrite-files', 'test', 'dynamic', 'classes', 'seperate-units', 'objects']);

  // parse parameters
  if FCmdOptions.OptionsMalformed then
  begin
    WriteLn('See -h for options.');
    Terminate;
    Halt;

  end;

  if FCmdOptions.HasOption('help') then begin
    WriteHelp;
    Terminate;
    Exit;
  end;

  if not FCmdOptions.HasOption('input') then
  begin
    WriteLn('No input file specified! See -h for options.');
    Terminate;
    Halt;
  end;

  FFileToConvert:=FCmdOptions.OptionValue('input');
    AddPaths(ExtractFilePath(FFileToConvert));

  if FCmdOptions.HasOption('paths') then
    AddPaths(FCmdOptions.OptionValue('paths'));

  if not FCmdOptions.HasOption('no-default') then
    AddDefaultPaths;

  if FCmdOptions.HasOption('output-directory') then
    FOutPutDirectory:=IncludeTrailingPathDelimiter(FCmdOptions.OptionValue('output-directory'))
  else
    FOutPutDirectory:=IncludeTrailingPathDelimiter(GetCurrentDir);

  if FCmdOptions.HasOption('unit-prefix') then
    FUnitPrefix := FCmdOptions.OptionValue('unit-prefix');

  if FCmdOptions.HasOption('overwrite-files') then
    FOverWriteFiles:=True;

  if FCmdOptions.HasOption('test') then
    Include(FOptions, goWantTest);

  if FCmdOptions.HasOption('dynamic') then
    Include(FOptions, goLinkDynamic);

  if FCmdOptions.HasOption('deprecated') then
    Include(FOptions, goIncludeDeprecated);

  if FCmdOptions.HasOption('classes') then
  begin
    Include(FOptions, goClasses);
    Include(FOptions, goSeperateConsts);
  end;

  if FCmdOptions.HasOption('no-wrappers') then
    Include(FOptions, goNoWrappers);

  if FCmdOptions.HasOption('objects') then
  begin
    Include(FOptions, goObjects);
    Include(FOptions, goSeperateConsts);
  end;

  if FCmdOptions.HasOption('seperate-units') then
    Include(FOptions, goSeperateConsts);

  if FCmdOptions.HasOption('unit-prefix') then
    FUnitPrefix:=FCmdOptions.OptionValue('unit-prefix')
  else
    FUnitPrefix:='';

  if FCmdOptions.HasOption('declare-enums-as') then begin
    Val('goEnumAs' + FCmdOptions.OptionValue('declare-enums-as'), EnumImpl, ErrorCode);
    if ErrorCode > 0 then begin
      WriteLn('Invalid enum declaration type: "', FCmdOptions.OptionValue('declare-enums-as'), '"');
      Terminate;
    end;
    Include(FOptions, EnumImpl);
  end;

  VerifyOptions;

  // does all the heavy lifting
  Convert;

  // stop program loop
  Terminate;
end;

function TGirConsoleConverter.LoadFile(const FileName: String): TXMLDocument;
var
  p: TProcess;
  S: String = '';
  n: Integer;
begin
  if FileName.EndsWith('.gir') then begin
    ReadXMLFile(Result, FileName);
  end else begin
    p := TProcess.Create(nil);
    p.Executable := 'g-ir-generate';
    p.Parameters.add(FileName);
    p.Options := [poUsePipes];
    p.Execute;
    try
      ReadXMLFile(Result, p.Output);
    except
        WriteLn('############################################################');
        n := p.Stderr.NumBytesAvailable;
        SetLength(S, n + 1);
        p.Stderr.Read(S[1], n);
        S[n] := #0;
        WriteLn('ExitCode = ', p.ExitCode);
        WriteLn(S);
        WriteLn('############################################################');
        Result := nil;
    end;
    p.Free;
  end;
end;

constructor TGirConsoleConverter.Create;
begin
  //inherited Create(TheOwner);
  FCmdOptions := TCommandLineOptions.Create;
  FPaths := TStringList.Create;
  FPaths.Delimiter := PathSep;
end;

destructor TGirConsoleConverter.Destroy;
begin
  FPaths.Free;
  FCmdOptions.Free;
  inherited Destroy;
end;

procedure TGirConsoleConverter.WriteHelp;
var
  {$IFDEF UNIX}
  w: winsize;
  {$ENDIF}
  ConsoleWidth: Integer;
begin
  ConsoleWidth:=80;
  {$IFDEF UNIX}
  fpioctl(0, TIOCGWINSZ, @w);
  ConsoleWidth:=w.ws_col;
  {$ENDIF}
  Writeln('Usage: ',ExtractFileName(ParamStr(0)),' [options] -i filename');
  with FCmdOptions.PrintHelp(ConsoleWidth) do
  begin
    WriteLn(Text);
    Free;
  end;
{
  Writeln('');
  writeln('    Usage: ',ExtractFileName(ParamStr(0)),' [options] -i filename');
  Writeln('');
  Writeln('');
  Writeln('      -i --input=            .gir filename to convert.');
  Writeln('      -o --output-directory=  Directory to write the resulting .pas files to. If not');
  Writeln('                                specified then the current working directory is used.');
  WriteLn('      -D --dynamic            Use unit dynlibs and link at runtime');
  WriteLn('      -s --seperate-units     Creates seperate units for each gir file:');
  WriteLn('                                (xConsts, xTypes, xFunctions, [xClasses, xObjects].');
  WriteLn('      -C --classes            Create Pascal classes that envelope/wrap the GObjects.');
  WriteLn('                                Also forces ''-s''');
  WriteLn('      -O --objects            OPTION NOT IMPLEMENTED YET. See Note below');
  WriteLn('                                Creates a seperate unit for pascal Objects (not classes). Forces ''-s''');
  WriteLn('                                Note: If -C or -O are not used then pascal Objects and consts');
  WriteLn('                                are in a single unit.');
  Writeln('      -w --overwrite-files    If the output .pas file(s) already exists then overwrite them.');
  Writeln('      -n --no-default         /usr/share/gir-1.0 is not added as a search location for ');
  Writeln('                                needed .gir files.');
  Writeln('      -p --paths=             List of paths seperated by ":" to search for needed .gir files.');
  Writeln('      -t --test               Creates a test program and a test c file per unit to verify struct sizes.');
  Writeln('');
}
end;
procedure TGirConsoleConverter.Run;
begin
  DoRun;
end;

var
  Application: TGirConsoleConverter;

{$R *.res}

begin
  Application:=TGirConsoleConverter.Create;
  Application.Run;
  Application.Free;
end.

