program update_lcl_docs;

{ Runs FPC's fpdoc document generator to generate LCL documentation,
  e.g. in CHM format }

{$mode objfpc}{$H+}
{$IFDEF MSWINDOWS}
{$APPTYPE console}
{$ENDIF}

uses
  Classes, Sysutils, GetOpts, LazFileUtils, FileUtil, UTF8Process, LazUtilities,
  LazStringUtils, Process;

var
  DefaultFPDocExe: string = 'fpdoc';
  DefaultCSSFile: string = 'fpdoc.css';
  WarningsCount: Integer;
  Verbosity: integer;
  ShowCmd: Boolean;
  EnvParams: String;
  DefaultXCTDir: String;
  DefaultFPDocParams: string = '';
  DefaultOutFormat: string = 'html';
  DefaultFooterFilename: string = 'locallclfooter.xml'; // ToDo

type
  TFPDocRunStep = (
    frsCreated,
    frsVarsInitialized,
    frsFilesGathered,
    frsOutDirCreated,
    frsFPDocExecuted,
    frsCopiedToXCTDir,
    frsComplete
    );
  TFPDocRunOption = (
    foCopyToXCTDir  // copy the created chm and xct file to the xct directory
    );
  TFPDocRunOptions = set of TFPDocRunOption;
const
  DefaultFPDocRunOptions = [foCopyToXCTDir];

type

  { TFPDocRun }

  TFPDocRun = class
  private
    FCSSFile: String;
    FFooterFilename: String;
    FFPDocExe: String;
    FIncludePath: string;
    FInputFile: string;
    FOptions: TFPDocRunOptions;
    FOutDir: string;
    FOutFormat: String;
    FPackageName: string;
    FPasSrcDir: string;
    FStep: TFPDocRunStep;
    FUsedPkgs: TStringList;
    FXCTDir: string;
    FXCTFile: string;
    FXMLSrcDir: string;
    procedure SetCSSFile(AValue: String);
    procedure SetFooterFilename(AValue: String);
    procedure SetIncludePath(AValue: string);
    procedure SetInputFile(AValue: string);
    procedure SetOutDir(AValue: string);
    procedure SetPasSrcDir(AValue: string);
    procedure SetXCTDir(AValue: string);
    procedure SetXMLSrcDir(AValue: string);
  public
    Params: TStringList;
    ParseParams: string;
    constructor Create(aPackageName: string);
    destructor Destroy; override;
    procedure InitVars;
    procedure AddFilesToList(Dir: String; List: TStrings);
    procedure FindSourceFiles;
    procedure CreateOuputDir;
    procedure RunFPDoc;
    procedure CopyToXCTDir;
    procedure Execute;
    property Options: TFPDocRunOptions read FOptions write FOptions default DefaultFPDocRunOptions;
    property CSSFile: String read FCSSFile write SetCSSFile;
    property FooterFilename: String read FFooterFilename write SetFooterFilename;
    property FPDocExe: String read FFPDocExe write FFPDocExe;
    property IncludePath: string read FIncludePath write SetIncludePath;// semicolon separated search path
    property InputFile: string read FInputFile write SetInputFile; // relative to OutDir, automatically created
    property OutDir: string read FOutDir write SetOutDir;
    property OutFormat: String read FOutFormat write FOutFormat;
    property PackageName: string read FPackageName;
    property PasSrcDir: string read FPasSrcDir write SetPasSrcDir;
    property Step: TFPDocRunStep read FStep;
    property UsedPkgs: TStringList read FUsedPkgs; // e.g. 'rtl','fcl', 'lazutils'
    property XCTDir: string read FXCTDir write SetXCTDir;
    property XMLSrcDir: string read FXMLSrcDir write SetXMLSrcDir;
    property XCTFile: string read FXCTFile;
  end;

procedure GetEnvDef(var S: String; DefaultValue: String; EnvName: String);
begin
  S := GetEnvironmentVariable(EnvName);
  if S = '' then
    S := DefaultValue;
end;

function FileInEnvPATH(FileName: String): Boolean;
var
  FullFilename: String;
begin
  FullFilename:=FindDefaultExecutablePath(Filename);
  Result:=(FullFilename<>'') and not DirectoryExistsUTF8(FullFilename);
end;

procedure PrintHelp;
begin
  WriteLn('Usage for '+ ExtractFileName(ParamStr(0)), ':');
  WriteLn;
  Writeln('    --css-file <value> (CHM format only) CSS file to be used by fpdoc');
  Writeln('                       for the layout of the help pages. Default is "',DefaultCSSFile,'"');
  WriteLn('    --fpdoc <value>    The full path to fpdoc to use. Default is "',DefaultFPDocExe,'"');
  WriteLn('    --fpcdocs <value>  The directory that contains the required .xct files.');
  WriteLn('                       Use this to make help that contains links to rtl and fcl');
  WriteLn('    --footer <value>   Filename of a file to use a footer used in the generated pages.');
  WriteLn('                       Default is "'+DefaultFooterFilename+'"');
  WriteLn('    --help             Show this message');
  WriteLn('    --arg <value>      Passes value to fpdoc as an arg. Use this option as');
  WriteLn('                       many times as needed.');
  WriteLn('    --outfmt html|chm  Use value as the format fpdoc will use. Default is "'+DefaultOutFormat+'"');
  WriteLn('    --showcmd          Print the command that would be run instead if running it.');
  WriteLn('    --warnings         Show warnings while working.');
  WriteLn('    --verbose          be more verbose');
  WriteLn;
  WriteLn('The following are environment variables that will override the above params if set:');
  WriteLn('     FPDOCFORMAT, FPDOCPARAMS, FPDOC, FPDOCFOOTER, FPCDOCS, RTLLINKPREFIX, FCLLINKPREFIX, <Pkg>LINKPREFIX, ...');
  WriteLn;
  Halt(0);
end;

procedure ReadOptions;
var
  c: char;
  Options: array of TOption;
  OptIndex: Longint;
begin
  ShowCmd := False;
  WarningsCount:=-1;
  SetLength(Options, 10);

  Options[0].Name:='help';
  Options[1].Name:='arg';
  Options[1].Has_arg:=1;
  Options[2].Name:='fpdoc';
  Options[2].Has_arg:=1;
  Options[3].Name:='outfmt';
  Options[3].Has_arg:=1;
  Options[4].Name:='showcmd';
  Options[5].Name:='fpcdocs';
  Options[5].Has_arg:=1;
  Options[6].Name:='footer';
  Options[6].Has_arg:=1;
  Options[7].Name:='warnings';
  Options[8].Name:='css-file';
  Options[8].Has_arg:=1;
  Options[9].Name:='verbose';
  OptIndex:=0;
  repeat
    c := GetLongOpts('help arg: fpdoc: outfmt: showcmd fpcdocs: footer: warnings css-file verbose', @Options[0], OptIndex);
    case c of
      #0:
         begin
           //WriteLn(Options[OptIndex-1].Name, ' = ', OptArg);
           case OptIndex-1 of
             0:  PrintHelp;
             1:  DefaultFPDocParams := DefaultFPDocParams + ' ' + OptArg;
             2:  DefaultFPDocExe := OptArg;
             3:  DefaultOutFormat := OptArg;
             4:  ShowCmd := True;
             5:  DefaultXCTDir := OptArg;
             6:  DefaultFooterFilename := OptArg;
             7:  WarningsCount:=0;
             8:  DefaultCssFile := OptArg;
             9:  inc(Verbosity);
           else
             WriteLn('Unknown Value: ', OptIndex);
           end;
         end;
      '?': PrintHelp;
      EndOfOptions: Break;
    else
      WriteLn('Unknown option -',c,' ',OptArg);
      PrintHelp;
    end;
  until c = EndOfOptions;

  GetEnvDef(DefaultOutFormat, DefaultOutFormat, 'FPDOCFORMAT');
  GetEnvDef(EnvParams, '', 'FPDOCPARAMS');
  GetEnvDef(DefaultFPDocExe, DefaultFPDocExe, 'FPDOC');
  GetEnvDef(DefaultFooterFilename, DefaultFooterFilename, 'FPDOCFOOTER');
  GetEnvDef(DefaultXCTDir, DefaultXCTDir, 'FPCDOCS');

  if DefaultOutFormat = '' then
  begin
    writeln('Error: Param outfmt wrong');
    PrintHelp;
  end;
end;

{ TFPDocRun }

procedure TFPDocRun.SetInputFile(AValue: string);
begin
  AValue:=TrimFilename(AValue);
  if FInputFile=AValue then Exit;
  FInputFile:=AValue;
end;

procedure TFPDocRun.SetOutDir(AValue: string);
begin
  AValue:=TrimAndExpandFilename(AValue);
  if FOutDir=AValue then Exit;
  FOutDir:=AValue;
end;

procedure TFPDocRun.SetIncludePath(AValue: string);
begin
  if FIncludePath=AValue then Exit;
  FIncludePath:=AValue;
end;

procedure TFPDocRun.SetCSSFile(AValue: String);
begin
  AValue:=TrimAndExpandFilename(AValue);
  if FCSSFile=AValue then Exit;
  FCSSFile:=AValue;
end;

procedure TFPDocRun.SetFooterFilename(AValue: String);
begin
  AValue:=TrimAndExpandFilename(AValue);
  if FFooterFilename=AValue then Exit;
  FFooterFilename:=AValue;
end;

procedure TFPDocRun.SetPasSrcDir(AValue: string);
begin
  AValue:=TrimAndExpandFilename(AValue);
  if FPasSrcDir=AValue then Exit;
  FPasSrcDir:=AValue;
end;

procedure TFPDocRun.SetXCTDir(AValue: string);
begin
  AValue:=TrimAndExpandFilename(AValue);
  if FXCTDir=AValue then Exit;
  FXCTDir:=AValue;
end;

procedure TFPDocRun.SetXMLSrcDir(AValue: string);
begin
  AValue:=TrimAndExpandFilename(AValue);
  if FXMLSrcDir=AValue then Exit;
  FXMLSrcDir:=AValue;
end;

constructor TFPDocRun.Create(aPackageName: string);
begin
  FPackageName:=aPackageName;
  FOptions:=DefaultFPDocRunOptions;
  fUsedPkgs:=TStringList.Create;
  InputFile := 'inputfile.txt';
  OutDir:=PackageName;
  FPDocExe:=TrimFilename(DefaultFPDocExe);
  CSSFile:=DefaultCSSFile;
  Params:=TStringList.Create;
  SplitCmdLineParams(DefaultFPDocParams,Params);
  OutFormat:=DefaultOutFormat;
  FooterFilename:=DefaultFooterFilename;
  XCTDir:=DefaultXCTDir;

  FStep:=frsCreated;
end;

destructor TFPDocRun.Destroy;
begin
  FreeAndNil(fUsedPkgs);
  inherited Destroy;
end;

procedure TFPDocRun.InitVars;
var
  Pkg, Prefix, IncludeDir, Param: String;
  p: Integer;
begin
  if ord(Step)>=ord(frsVarsInitialized) then
    raise Exception.Create('TFPDocRun.InitVars not again');

  // add IncludePath to ParseParams
  p:=1;
  while p<=length(IncludePath) do begin
    IncludeDir:=GetNextDelimitedItem(IncludePath,';',p);
    if IncludeDir='' then continue;
    IncludeDir:=TrimAndExpandFilename(ChompPathDelim(IncludeDir));
    ParseParams+=' -Fi'+CreateRelativePath(IncludeDir,OutDir);
  end;

  FXCTFile:=AppendPathDelim(OutDir)+PackageName+'.xct';

  Params.Add('--content='+CreateRelativePath(XCTFile,OutDir));
  Params.Add('--package='+PackageName);
  Params.Add('--descr='+CreateRelativePath(AppendPathDelim(XMLSrcDir)+PackageName+'.xml',OutDir));
  Params.Add('--format='+OutFormat);
  if FilenameIsAbsolute(InputFile) then
    Params.Add('--input=@'+CreateRelativePath(InputFile,OutDir))
  else
    Params.Add('--input=@'+InputFile);

  if XCTDir <> '' then
  begin
    for Pkg in UsedPkgs do
    begin
      Prefix:='';
      if OutFormat = 'html' then
        Prefix:='../'+Lowercase(Pkg)+'/'
      else if OutFormat = 'chm' then
        Prefix:='ms-its:'+LowerCase(Pkg)+'.chm::/'
      else
        Prefix:='';
      GetEnvDef(Prefix, Prefix, UpperCase(Pkg)+'LINKPREFIX');

      Param:='--import='+CreateRelativePath(AppendPathDelim(XCTDir)+LowerCase(Pkg)+'.xct',OutDir);
      if Prefix<>'' then
        Param+=','+Prefix;
      Params.Add(Param);
    end;
  end;
  
  if OutFormat='chm' then
  begin
    Params.Add('--output='+ ChangeFileExt(PackageName, '.chm'));
    Params.Add('--auto-toc');
    Params.Add('--auto-index');
    Params.Add('--make-searchable');
    if CSSFile<>'' then
      Params.Add('--css-file='+ExtractFileName(CSSFile)); // the css file is copied to the OutDir
  end;

  if (FooterFilename<>'') and FileExistsUTF8(FooterFilename) then
    Params.Add('--footer='+FooterFilename);

  if EnvParams<>'' then
    SplitCmdLineParams(EnvParams,Params);

  if Verbosity>0 then
  begin
    writeln('Verbose Params: ------------------');
    writeln('FPDocExe=',FPDocExe);
    writeln('OutFormat=',OutFormat);
    writeln('CSSFile=',CSSFile);
    writeln('FooterFilename=',FooterFilename);
    writeln('InputFile=',InputFile);
    writeln('OutDir=',OutDir);
    writeln('ParseParams=');
    writeln(ParseParams);
    writeln('FPDocParams=');
    writeln(Params.Text);
    writeln('----------------------------------');
  end;

  FStep:=frsVarsInitialized;
end;

procedure TFPDocRun.AddFilesToList(Dir: String; List: TStrings);
var
  FRec: TSearchRec;
begin
  Dir:=AppendPathDelim(TrimFilename(Dir));
  if FindFirstUTF8(Dir+AllFilesMask, faAnyFile, FRec)=0 then
    repeat
      //WriteLn('Checking file ' +FRec.Name);
      if (FRec.Name='') or (FRec.Name='.') or (FRec.Name='..') then continue;
      if (FRec.Name='fpmake.pp') then continue;
      if ((FRec.Attr and faDirectory) <> 0) then
      begin
        AddFilesToList(Dir+FRec.Name, List);
        //WriteLn('Checking Subfolder ',Dir+ FRec.Name);
      end
      else if FilenameIsPascalUnit(FRec.Name) then
      begin
        List.Add(Dir+FRec.Name);
      end;
    until FindNextUTF8(FRec)<>0;
  FindCloseUTF8(FRec);
end;

procedure TFPDocRun.FindSourceFiles;
var
  FileList: TStringList;
  InputList: TStringList;
  I: Integer;
  XMLFile, Filename: String;
begin
  if ord(Step)>=ord(frsFilesGathered) then
    raise Exception.Create('TFPDocRun.FindSourceFiles not again');
  if ord(Step)<ord(frsVarsInitialized) then
    InitVars;

  if Verbosity>0 then
    writeln('PasSrcDir="',PasSrcDir,'"');
  FileList := TStringList.Create;
  InputList := TStringList.Create;
  AddFilesToList(PasSrcDir, FileList);

  FileList.Sort;
  for I := 0 to FileList.Count-1 do
  begin
    XMLFile := AppendPathDelim(XMLSrcDir)+ExtractFileNameOnly(FileList[I])+'.xml';
    if FileExistsUTF8(XMLFile) then
    begin
      InputList.Add(CreateRelativePath(FileList[I],OutDir) + ParseParams);
      Params.Add('--descr='+CreateRelativePath(XMLFile,OutDir));
    end
    else
    begin
      if WarningsCount >= 0 then
        WriteLn('Warning! No corresponding xml file for unit ' + FileList[I])
      else
        Dec(WarningsCount);
    end;
  end;
  FileList.Free;

  Filename:=InputFile;
  if not FilenameIsAbsolute(Filename) then
    Filename:=TrimFilename(AppendPathDelim(OutDir)+Filename);
  InputList.SaveToFile(Filename);
  InputList.Free;

  FStep:=frsFilesGathered;
end;

procedure TFPDocRun.CreateOuputDir;
var
  TargetCSSFile: String;
begin
  if ord(Step)>=ord(frsOutDirCreated) then
    raise Exception.Create('TFPDocRun.CreateOuputDir not again');

  if Not DirectoryExistsUTF8(OutDir) then
  begin
    writeln('Creating directory "',OutDir,'"');
    if not CreateDirUTF8(OutDir) then
      raise Exception.Create('unable to create directory "'+OutDir+'"');
  end;

  if ord(Step)<ord(frsFilesGathered) then
    FindSourceFiles;

  if (OutFormat='chm') and (CSSFile<>'') then
  begin
    TargetCSSFile:=AppendPathDelim(OutDir)+ExtractFileName(CSSFile);
    if CompareFilenames(TargetCSSFile,CSSFile)<>0 then
    begin
      if not CopyFile(CSSFile,TargetCSSFile) then
        raise Exception.Create('unable to copy css file: CSSfile="'+CSSFile+'" to "'+TargetCSSFile+'"');
    end;
  end;

  FStep:=frsOutDirCreated;
end;

procedure TFPDocRun.RunFPDoc;
var
  Process: TProcess;
  CmdLine: String;
begin
  if ord(Step)>=ord(frsFPDocExecuted) then
    raise Exception.Create('TFPDocRun.Run not again');
  if ord(Step)<ord(frsOutDirCreated) then
    CreateOuputDir;

  if ShowCmd then
  begin
    Writeln('WorkDirectory:',OutDir);
    WriteLn('Exe:',FPDocExe);
    WriteLn(Params.Text);
    exit;
  end;
  {$IFDEF MSWINDOWS}FPDocExe := ChangeFileExt(FPDocExe,'.exe');{$ENDIF}
  if not FileInEnvPATH(FPDocExe) then
  begin
    WriteLn('Error: fpdoc ('+FPDocExe+') cannot be found. Please add its location to the PATH ',
            'or set it with --fpdoc path',PathDelim,'to',PathDelim,'fpdoc'{$IFDEF MSWINDOWS},'.exe'{$ENDIF});
    Halt(1);
  end;
  Process := TProcessUTF8.Create(nil);
  try
    Process.Options := Process.Options + [poWaitOnExit];
    Process.CurrentDirectory := OutDir;
    Process.Executable:=FPDocExe;
    Process.Parameters.Assign(Params);
    CmdLine:=Process.Executable+' '+MergeCmdLineParams(Params);
    if Verbosity>0 then
      writeln('CmdLine: ',CmdLine);
    try
      Process.Execute;
      if Process.ExitCode<>0 then
        raise Exception.Create('fpdoc failed with code '+IntToStr(Process.ExitCode));
    except
      if WarningsCount >= 0 then
      begin
        WriteLn('Error running fpdoc, command line: '+CmdLine)
      end
      else
        Dec(WarningsCount);
    end;
    if WarningsCount < -1 then
      WriteLn(abs(WarningsCount+1), ' Warnings hidden. Use --warnings to see them all.');
    if not FileExistsUTF8(XCTFile) then
      raise Exception.Create('File not found: '+XCTFile);
  finally
    Process.Free;
  end;

  FStep:=frsFPDocExecuted;
end;

procedure TFPDocRun.CopyToXCTDir;
var
  TargetXCTFile, SrcCHMFile, TargetCHMFile: String;
begin
  if ord(Step)>=ord(frsCopiedToXCTDir) then
    raise Exception.Create('TFPDocRun.CopyToXCTDir not again');
  if ord(Step)<ord(frsFPDocExecuted) then
    RunFPDoc;

  if (foCopyToXCTDir in Options)
  and (CompareFilenames(ChompPathDelim(OutDir),ChompPathDelim(XCTDir))<>0) then
  begin
    TargetXCTFile:=AppendPathDelim(XCTDir)+ExtractFileName(XCTFile);
    if ShowCmd then
      writeln('cp ',XCTFile,' ',TargetXCTFile)
    else if not CopyFile(XCTFile,TargetXCTFile) then
      raise Exception.Create('unable to copy xct file: "'+XCTFile+'" to "'+TargetXCTFile+'"');
    writeln('Created ',TargetXCTFile);
    if OutFormat='chm' then
    begin
      SrcCHMFile:=AppendPathDelim(OutDir)+PackageName+'.chm';
      TargetCHMFile:=AppendPathDelim(XCTDir)+PackageName+'.chm';
      if ShowCmd then
        writeln('cp ',SrcCHMFile,' ',TargetCHMFile)
      else if not CopyFile(SrcCHMFile,TargetCHMFile) then
        raise Exception.Create('unable to copy chm file: "'+SrcCHMFile+'" to "'+TargetCHMFile+'"');
      writeln('Created ',TargetCHMFile);
    end;
  end;

  FStep:=frsCopiedToXCTDir;
end;

procedure TFPDocRun.Execute;
begin
  writeln('===================================================================');
  if ord(Step)>=ord(frsComplete) then
    raise Exception.Create('TFPDocRun.Execute not again');
  if ord(Step)<ord(frsCopiedToXCTDir) then
    CopyToXCTDir;

  FStep:=frsComplete;
end;

var
  Run: TFPDocRun;
begin
  ReadOptions;

  Run:=TFPDocRun.Create('lazutils');
  Run.UsedPkgs.Add('rtl');
  Run.UsedPkgs.Add('fcl');
  Run.XMLSrcDir := '..'+PathDelim+'xml'+PathDelim+'lazutils';
  Run.PasSrcDir := '..'+PathDelim+'..'+PathDelim+'components'+PathDelim+'lazutils';
  Run.Execute;
  Run.Free;

  Run:=TFPDocRun.Create('lcl');
  Run.UsedPkgs.Add('rtl');
  Run.UsedPkgs.Add('fcl');
  Run.UsedPkgs.Add('lazutils');
  Run.XMLSrcDir := '..'+PathDelim+'xml'+PathDelim+'lcl'+PathDelim;
  Run.PasSrcDir := '..'+PathDelim+'..'+PathDelim+'lcl'+PathDelim;
  Run.IncludePath := Run.PasSrcDir+PathDelim+'include';
  Run.Execute;
  Run.Free;

  if ShowCmd then
    writeln('Not executing, simulation ended. Stop');
end.

