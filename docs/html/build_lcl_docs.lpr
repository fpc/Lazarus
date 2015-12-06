program update_lcl_docs;

{ Runs FPC's fpdoc document generator to generate LCL documentation,
  e.g. in CHM format }

{$mode objfpc}{$H+}
{$IFDEF MSWINDOWS}
{$APPTYPE console}
{$ENDIF}

uses
  Classes, Sysutils, GetOpts, LazFileUtils, FileUtil, UTF8Process, Process;

var
  DefaultFPDocExe: string = 'fpdoc';
  DefaultCSSFile: string = 'fpdoc.css';
  WarningsCount: Integer;
  Verbosity: integer;
  ShowCmd: Boolean;
  EnvParams: String;
  XCTDir: String;
  DefaultFPDocParams: string = '';
  DefaultOutFormat: string = 'html';
  DefaultFooterFilename: string;
type

  { TFPDocRun }

  TFPDocRun = class
  private
    FFPDocParams: string;
    FInputFileList: string;
    FPackageName: string;
    FPasSrcDir: string;
    FXMLSrcDir: string;
    procedure SetFPDocParams(AValue: string);
    procedure SetInputFileList(AValue: string);
    procedure SetPasSrcDir(AValue: string);
    procedure SetXMLSrcDir(AValue: string);
  public
    FPDocExe: String;
    CSSFile: String;
    Params: String;
    FooterFilename: String;
    OutFormat: String;
    UsedPkgs: TStringList; // e.g. 'rtl','fcl', 'lazutils'
    constructor Create(aPackageName: string);
    destructor Destroy; override;
    procedure InitVars;
    procedure AddFilesToList(Dir: String; List: TStrings);
    procedure MakeFileList;
    procedure CreateOuputDir;
    procedure Run;
    property PackageName: string read FPackageName;
    property XMLSrcDir: string read FXMLSrcDir write SetXMLSrcDir;
    property PasSrcDir: string read FPasSrcDir write SetPasSrcDir;
    property Input_FileList_Filename: string read FInputFileList write SetInputFileList;
    property FPDocParams: string read FFPDocParams write SetFPDocParams;
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
             5:  XCTDir := OptArg;
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
  GetEnvDef(DefaultFooterFilename, '', 'FPDOCFOOTER');
  GetEnvDef(XCTDir, XCTDir, 'FPCDOCS');

  XCTDir:=TrimAndExpandDirectory(XCTDir);

  if DefaultOutFormat = '' then
  begin
    writeln('Error: Param outfmt wrong');
    PrintHelp;
  end;
end;

{ TFPDocRun }

procedure TFPDocRun.InitVars;
var
  Pkg, Prefix: String;
begin
  FPDocExe:=TrimFilename(DefaultFPDocExe);
  CSSFile:=TrimFilename(DefaultCSSFile);
  XMLSrcDir := '..'+PathDelim+'..'+PathDelim+'xml'+PathDelim+PackageName+PathDelim;
  PasSrcDir := '..'+PathDelim+'..'+PathDelim+PackageName+PathDelim;
  Input_FileList_Filename := 'inputfile.txt';
  FPDocParams := ' --content='+PackageName+'.xct'
                + ' --package='+PackageName
                + ' --descr='+XMLSrcDir+PackageName+'.xml'
                + ' --input=@'+Input_FileList_Filename+' ';
  Params:=DefaultFPDocParams;
  OutFormat:=DefaultOutFormat;
  FooterFilename:=TrimFilename(DefaultFooterFilename);


  Params+=' --format='+OutFormat+' ';

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

      Params+=' --import='+TrimFilename(XCTDir+PathDelim+LowerCase(Pkg)+'.xct');
      if Prefix<>'' then
        Params+=','+Prefix;
    end;
  end;
  
  if OutFormat='chm' then
  begin
    if CSSFile='' then CSSFile:='..'+PathDelim+'fpdoc.css'; //css file is chm only
    Params+=' --output='+ ChangeFileExt(PackageName, '.chm')
              +' --auto-toc --auto-index --make-searchable'
              +' --css-file='+CSSFile+' ';
  end;
end;

procedure TFPDocRun.AddFilesToList(Dir: String; List: TStrings);
var
  FRec: TSearchRec;
  SubDirs: String; // we do not want the PasSrcDir in this string but the subfolders only
begin
  Dir:=AppendPathDelim(Dir);
  if FindFirstUTF8(Dir+AllFilesMask, faAnyFile, FRec)=0 then
    repeat
      //WriteLn('Checking file ' +FRec.Name);
      if (FRec.Name='') or (FRec.Name='.') or (FRec.Name='..') then continue;
      if ((FRec.Attr and faDirectory) <> 0) then
      begin
        AddFilesToList(Dir+FRec.Name, List);
        //WriteLn('Checking Subfolder ',Dir+ FRec.Name);
      end
      else if FilenameIsPascalUnit(FRec.Name) then
      begin
        SubDirs := AppendPathDelim(Copy(Dir, Length(PasSrcDir)+1, Length(Dir)));
        if Length(SubDirs) = 1 then
          SubDirs:='';
        List.Add(SubDirs+FRec.Name);
      end;
    until FindNextUTF8(FRec)<>0;
  FindCloseUTF8(FRec);
end;

procedure TFPDocRun.MakeFileList;
var
  FileList: TStringList;
  InputList: TStringList;
  I: Integer;
  XMLFile: String;
begin
  if Verbosity>0 then
    writeln('PasSrcDir="',PasSrcDir,'"');
  FileList := TStringList.Create;
  InputList := TStringList.Create;
  AddFilesToList(PasSrcDir, FileList);

  FileList.Sort;
  for I := 0 to FileList.Count-1 do
  begin
    XMLFile := XMLSrcDir+ChangeFileExt(FileList[I],'.xml');
    if FileExistsUTF8(PackageName+PathDelim+XMLFile) and (filelist[i]<>'fpmake.pp') then
    begin
      InputList.Add('..'+PathDelim+PasSrcDir+FileList[I] + ' -Fi..'+PathDelim+PasSrcDir+'include');
      Params:=Params+' --descr='+XMLSrcDir+ChangeFileExt(FileList[I],'.xml');
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
  InputList.SaveToFile(PackageName+PathDelim+Input_FileList_Filename);
  InputList.Free;
end;

procedure TFPDocRun.CreateOuputDir;
var
  OutDir: String;
begin
  OutDir:=PackageName;
  if Not DirectoryExistsUTF8(OutDir) then
  begin
    writeln('Creating directory "',OutDir,'"');
    if not CreateDirUTF8(OutDir) then
      raise Exception.Create('unable to create directory "'+OutDir+'"');
  end;
end;

procedure TFPDocRun.Run;
var
  Process: TProcess;
  CmdLine: String;
  WorkDir: String;
begin
  CmdLine := FPDocExe + FPDocParams + Params + EnvParams;
  WorkDir := GetCurrentDirUTF8+PathDelim+PackageName;
  if ShowCmd then
  begin
    Writeln('WorkDirectory:',WorkDir);
    WriteLn(CmdLine);
    Exit;
  end;
  {$IFDEF MSWINDOWS}fpdoc := ChangeFileExt(fpdoc,'.exe');{$ENDIF}
  if not FileInEnvPATH(FPDocExe) then
  begin
    WriteLn('Error: fpdoc ('+FPDocExe+') cannot be found. Please add its location to the PATH ',
            'or set it with --fpdoc path',PathDelim,'to',PathDelim,'fpdoc'{$IFDEF MSWINDOWS},'.exe'{$ENDIF});
    Halt(1);
  end;
  Process := TProcessUTF8.Create(nil);
  try
    Process.Options := Process.Options + [poWaitOnExit];
    Process.CurrentDirectory := WorkDir;
    Process.CommandLine := CmdLine;
    if Verbosity>0 then
      writeln('Command="',Process.CommandLine,'"');
    try
      Process.Execute;
    except
      if WarningsCount >= 0 then
        WriteLn('Error running fpdoc, command line: '+CmdLine)
      else
        Dec(WarningsCount);
    end;
    if WarningsCount < -1 then
      WriteLn(abs(WarningsCount+1), ' Warnings hidden. Use --warnings to see them all.');
  finally
    Process.Free;
  end;
end;

procedure TFPDocRun.SetFPDocParams(AValue: string);
begin
  if FFPDocParams=AValue then Exit;
  FFPDocParams:=AValue;
end;

procedure TFPDocRun.SetInputFileList(AValue: string);
begin
  if FInputFileList=AValue then Exit;
  FInputFileList:=AValue;
end;

procedure TFPDocRun.SetPasSrcDir(AValue: string);
begin
  if FPasSrcDir=AValue then Exit;
  FPasSrcDir:=AValue;
end;

procedure TFPDocRun.SetXMLSrcDir(AValue: string);
begin
  if FXMLSrcDir=AValue then Exit;
  FXMLSrcDir:=AValue;
end;

constructor TFPDocRun.Create(aPackageName: string);
begin
  UsedPkgs:=TStringList.Create;
  FPackageName:=aPackageName;
end;

destructor TFPDocRun.Destroy;
begin
  FreeAndNil(UsedPkgs);
  inherited Destroy;
end;

var
  Run: TFPDocRun;
begin
  ReadOptions;

  Run:=TFPDocRun.Create('lcl');
  Run.UsedPkgs.Add('rtl');
  Run.UsedPkgs.Add('fcl');

  Run.InitVars;
  Run.MakeFileList;
  Run.CreateOuputDir;
  Run.Run;

  Run.Free;
end.

