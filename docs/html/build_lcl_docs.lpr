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
  fpdoc: String = 'fpdoc';
  ArgParams: String;
  CSSFile: String;
  EnvParams: String;
  fpdocfooter: String;
  FPCDocsPath: String;
  OutFormat: String;
  ShowCmd: Boolean;
  RTLPrefix: String;
  FCLPrefix: String;
  WarningsCount: Integer;

const
  PackageName   = 'lcl';
  XMLSrcDir     = '..'+PathDelim+'..'+PathDelim+'xml'+PathDelim+'lcl'+PathDelim;
  PasSrcDir     = '..'+PathDelim+'..'+PathDelim+'lcl'+PathDelim;
  InputFileList = 'inputfile.txt';
  FPDocParams   = ' --content='+PackageName+'.xct'
                + ' --package='+PackageName
                + ' --descr='+XMLSrcDir+PackageName+'.xml'
                + ' --input=@'+InputFileList+' ';
  
procedure GetEnvDef(var S: String; DefaultValue: String; EnvName: String);
begin
  S := GetEnvironmentVariable(EnvName);
  if S = '' then
    S := DefaultValue;
end;

procedure PrintHelp;
begin
  WriteLn('Usage for '+ ExtractFileName(ParamStr(0)), ':');
  WriteLn;
  Writeln('    --css-file <value> (CHM format only) CSS file to be used by fpdoc');
  Writeln('                       for the layout of the help pages.');
  WriteLn('    --fpdoc <value>    The full path to fpdoc to use. Default is "fpdoc"');
  WriteLn('    --fpcdocs <value>  The directory that contains the fcl and rtl .xct files.');
  WriteLn('                       Use this to make help that contains links to the rtl and fcl');
  WriteLn('    --footer <value>   Filename of a file to use a footer used in the generated pages.');
  WriteLn('    --help             Show this message');
  WriteLn('    --arg <value>      Passes value to fpdoc as an arg. Use this option as');
  WriteLn('                       many times as needed.');
  WriteLn('    --outfmt html|chm  Use value as the format fpdoc will use. Default is "html"');
  WriteLn('    --showcmd          Print the command that would be run instead if running it.');
  WriteLn('    --warnings         Show warnings while working.');
  WriteLn;
  WriteLn('The following are environment variables that will override the above params if set:');
  WriteLn('     FPDOCFORMAT, FPDOCPARAMS, FPDOC, FPDOCFOOTER, FPCDOCS, RTLLINKPREFIX, FCLLINKPREFIX');
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
  OptIndex:=0;
  repeat
    c := GetLongOpts('help:arg:fpdoc:outfmt:showcmd:fpcdocs:footer:warnings:css-file', @Options[0], OptIndex);
    case c of
      #0:
         begin
           //WriteLn(Options[OptIndex-1].Name, ' = ', OptArg);
           case OptIndex-1 of
             0:  PrintHelp;
             1:  ArgParams := ArgParams + ' ' + OptArg;
             2:  fpdoc := OptArg;
             3:  OutFormat := OptArg;
             4:  ShowCmd := True;
             5:  FPCDocsPath := OptArg;
             6:  fpdocfooter := OptArg;
             7:  WarningsCount:=0;
             8:  CssFile := OptArg;
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
end;
  
procedure InitVars;
begin
  // see if any are set or set them to a default value
  GetEnvDef(OutFormat,   OutFormat,  'FPDOCFORMAT');
  GetEnvDef(EnvParams,   '',         'FPDOCPARAMS');

  GetEnvDef(fpdoc,       fpdoc,    'FPDOC');

  GetEnvDef(fpdocfooter, '',         'FPDOCFOOTER');
  fpdocfooter:=TrimFilename(fpdocfooter);

  GetEnvDef(FPCDocsPath, FPCDocsPath, 'FPCDOCS');
  FPCDocsPath:=TrimAndExpandDirectory(FPCDocsPath);
  
  if OutFormat = '' then
    OutFormat := 'html';

  if FPCDocsPath <> '' then
  begin
    if OutFormat = 'html' then
    begin
      GetEnvDef(RTLPrefix, '../rtl/', 'RTLLINKPREFIX');
      GetEnvDef(FCLPrefix, '../fcl/', 'FCLLINKPREFIX');
    end
    else if OutFormat = 'chm' then
    begin
      GetEnvDef(RTLPrefix, 'ms-its:rtl.chm::/', 'RTLLINKPREFIX');
      GetEnvDef(FCLPrefix, 'ms-its:fcl.chm::/', 'FCLLINKPREFIX');
    end
    else
    begin
      GetEnvDef(RTLPrefix, '', 'RTLLINKPREFIX');
      GetEnvDef(FCLPrefix, '', 'FCLLINKPREFIX');
    end;
    
    if (RTLPrefix<>'') and (RTLPrefix[1]<>',') then
      RTLPrefix := ','+RTLPrefix;
    if (FCLPrefix<>'') and (FCLPrefix[1]<>',') then
      FCLPrefix := ','+FCLPrefix;
    ArgParams:=ArgParams+ ' --import='+TrimFilename(FPCDocsPath+PathDelim+'rtl.xct')+RTLPrefix
                        + ' --import='+TrimFilename(FPCDocsPath+PathDelim+'fcl.xct')+FCLPrefix;
  end;
  
  if OutFormat='chm' then
  begin
    if CSSFile='' then CSSFile:='..'+PathDelim+'fpdoc.css'; //css file is chm only
    ArgParams:=ArgParams+' --output='+ ChangeFileExt(PackageName, '.chm')
                          +' --auto-toc --auto-index --make-searchable'
                          +' --css-file='+CSSFile+' ';
  end;
  
  ArgParams:=ArgParams+' --format='+OutFormat+' ';
end;

procedure AddFilesToList(Dir: String; Ext: String; List: TStrings);
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
        AddFilesToList(Dir+FRec.Name, Ext, List);
        //WriteLn('Checking Subfolder ',Dir+ FRec.Name);
      end
      else if Lowercase(ExtractFileExt(FRec.Name)) = Ext then
      begin
        SubDirs := AppendPathDelim(Copy(Dir, Length(PasSrcDir)+1, Length(Dir)));
        if Length(SubDirs) = 1 then
          SubDirs:='';
        List.Add(SubDirs+FRec.Name);
      end;
    until FindNextUTF8(FRec)<>0;
  FindCloseUTF8(FRec);
end;

function FileInPath(FileName: String): Boolean;
var
  FullFilename: String;
begin
  FullFilename:=FindDefaultExecutablePath(Filename);
  Result:=(FullFilename<>'') and not DirectoryExistsUTF8(FullFilename);
end;

procedure MakeFileList;
var
  FileList: TStringList;
  InputList: TStringList;
  I: Integer;
  XMLFile: String;
begin
  FileList := TStringList.Create;
  InputList := TStringList.Create;
  AddFilesToList(PasSrcDir, '.pas', FileList);
  AddFilesToList(PasSrcDir, '.pp',  FileList);
  
  FileList.Sort;
  for I := 0 to FileList.Count-1 do
  begin
    XMLFile := XMLSrcDir+ChangeFileExt(FileList[I],'.xml');
    if FileExistsUTF8(PackageName+PathDelim+XMLFile) then
    begin
      InputList.Add('..'+PathDelim+PasSrcDir+FileList[I] + ' -Fi..'+PathDelim+PasSrcDir+'include');
      ArgParams:=ArgParams+' --descr='+XMLSrcDir+ChangeFileExt(FileList[I],'.xml');
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
  InputList.SaveToFile(PackageName+PathDelim+InputFileList);
  InputList.Free;
end;

procedure Run;
var
  Process: TProcess;
  CmdLine: String;
  WorkDir: String;
begin
  CmdLine := fpdoc + FPDocParams + ArgParams + EnvParams;
  WorkDir := GetCurrentDirUTF8+PathDelim+PackageName;
  if ShowCmd then
  begin
    Writeln('WorkDirectory:',WorkDir);
    WriteLn(CmdLine);
    Exit;
  end;
  {$IFDEF MSWINDOWS}fpdoc := ChangeFileExt(fpdoc,'.exe');{$ENDIF}
  if not FileInPath(fpdoc) then
  begin
    WriteLn('Error: fpdoc cannot be found. Please add the directory it is in to the PATH ',
            'or set it with --fpdoc path',PathDelim,'to',PathDelim,'fpdoc'{$IFDEF MSWINDOWS},'.exe'{$ENDIF});
    Halt(1);
  end;
  Process := TProcessUTF8.Create(nil);
  try
    Process.Options := Process.Options + [poWaitOnExit];
    Process.CurrentDirectory := WorkDir;
    Process.CommandLine := CmdLine;
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

begin
  ReadOptions;
  if Not DirectoryExistsUTF8(PackageName) then
    mkdir(PackageName);
  InitVars;
  MakeFileList;
  Run;
end.

