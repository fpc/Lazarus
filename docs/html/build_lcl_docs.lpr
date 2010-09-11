program update_lcl_docs;

{$mode objfpc}{$H+}
{$IFDEF MSWINDOWS}
{$APPTYPE console}
{$ENDIF}

uses
  Classes, Sysutils, GetOpts, Process;
  
var
  fpdoc: String;
  ArgParams: String;
  EnvParams: String;
  fpdocfooter: String;
  FPCDocsPath: String;
  OutFormat: String;
  ShowCmd: Boolean;
  RTLPrefix: String;
  FCLPrefix: String;

const
  PackageName   = 'lcl';
  XMLSrcDir     = '..'+PathDelim+'..'+PathDelim+'xml'+PathDelim+'lcl'+PathDelim;
  PasSrcDir     = '..'+PathDelim+'..'+PathDelim+'lcl'+PathDelim;
  InputFileList = 'inputfile.txt';
  FPDocParams   = ' --content='+PackageName+'.xct'
                + ' --package='+PackageName
                + ' --descr='+XMLSrcDir+PackageName+'.xml'
                + ' --input=@'+InputFileList+' ';
  
procedure SetString(var S: String; DefaultValue: String; EnvName: String);
begin
  S := GetEnvironmentVariable(EnvName);
  if S = '' then
    S := DefaultValue;
end;

procedure PrintHelp;
begin
  WriteLn('Usage for '+ ExtractFileName(ParamStr(0)), ':');
  WriteLn;
  WriteLn('    --fpdoc <value>    The full path to fpdoc to use. Default is "fpdoc"');
  WriteLn('    --fpcdocs <value>  The directory that contains the fcl and rtl .xct files.');
  WriteLn('                       Use this to make help that contains links to the rtl and fcl');
  WriteLn('    --footer <value>   Filename of a file to use a footer used in the generated pages.');
  WriteLn('    --help             Show this message');
  WriteLn('    --arg <value>      Passes value to fpdoc as an arg. Use this option as');
  WriteLn('                       many times as needed.');
  WriteLn('    --outfmt html|chm  Use value as the format fpdoc will use. Default is "html"');
  WriteLn('    --showcmd          Print the command that would be run instead if running it.');
  WriteLn;
  WriteLn('The following are Environment variables that will override the above params if set:');
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
  repeat
    c := GetLongOpts('help:arg:fpdoc:outfmt:showcmd:fpcdocs:footer', @Options[0], OptIndex);
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
           else
             WriteLn('Unknown Value: ', OptIndex);
           end;
         end;
      '?': PrintHelp;
      EndOfOptions: Break;
    else
      WriteLn('UNknown option ', c, ' ', ord(c), ' ',OptArg);
    end;
  until c = EndOfOptions;
end;
  
procedure InitVars;
begin
  // see if any are set or set then to a default value
  SetString(OutFormat,   OutFormat,  'FPDOCFORMAT');
  SetString(EnvParams,   '',         'FPDOCPARAMS');
  if fpdoc <> '' then
    SetString(fpdoc,         fpdoc,    'FPDOC')
  else
    SetString(fpdoc,       'fpdoc',    'FPDOC');
  SetString(fpdocfooter, '',         'FPDOCFOOTER');
  SetString(FPCDocsPath, FPCDocsPath, 'FPCDOCS');
  
  if OutFormat = '' then
    OutFormat := 'html';

  if FPCDocsPath <> '' then
  begin
    if OutFormat = 'html' then
    begin
      SetString(RTLPrefix, '../rtl/', 'RTLLINKPREFIX');
      SetString(FCLPrefix, '../fcl/', 'FCLLINKPREFIX');
    end
    else if OutFormat = 'chm' then
    begin
      SetString(RTLPrefix, 'ms-its:rtl.chm::/', 'RTLLINKPREFIX');
      SetString(FCLPrefix, 'ms-its:fcl.chm::/', 'FCLLINKPREFIX');
    end
    else
    begin
      SetString(RTLPrefix, '', 'RTLLINKPREFIX');
      SetString(FCLPrefix, '', 'FCLLINKPREFIX');
    end;
    
    if (RTLPrefix<>'') and (RTLPrefix[1]<>',') then
      RTLPrefix := ','+RTLPrefix;
    if (FCLPrefix<>'') and (FCLPrefix[1]<>',') then
      FCLPrefix := ','+FCLPrefix;
    ArgParams:=ArgParams+ '--import='+FPCDocsPath+PathDelim+'rtl.xct'+RTLPrefix
                        +' --import='+FPCDocsPath+PathDelim+'fcl.xct'+FCLPrefix;
  end;
  
  if OutFormat='chm' then
  begin
    ArgParams:=ArgParams+' --output='+ ChangeFileExt(PackageName, '.chm')
                          +' --auto-toc --auto-index --make-searchable'
                          +' --css-file=..'+PathDelim+'fpdoc.css ';
  end;
  
  ArgParams:=ArgParams+' --format='+OutFormat+' ';
end;

procedure AddFilesToList(Dir: String; Ext: String; List: TStrings);
var
  FRec: TSearchRec;
  Res: Longint;
  SubDirs: String; // we do not want the PasSrcDir in this string but the subfolders only
begin
  Res := FindFirst(Dir+'*', faAnyFile, FRec);
  while Res = 0 do begin
    //WriteLn('Checking file ' +FRec.Name);
    if ((FRec.Attr and faDirectory) <> 0) and (FRec.Name[1] <> '.')then
    begin
      AddFilesToList(IncludeTrailingPathDelimiter(Dir)+FRec.Name, Ext, List);
      //WriteLn('Checking Subfolder ',Dir+ FRec.Name);
    end
    else if Lowercase(ExtractFileExt(FRec.Name)) = Ext then
    begin
      SubDirs := IncludeTrailingPathDelimiter(Copy(Dir, Length(PasSrcDir)+1, Length(Dir)));
      List.Add(SubDirs+FRec.Name);

    end;
    Res := FindNext(FRec);
  end;
  FindClose(FRec);
end;

function FileInPath(FileName: String): Boolean;
var
  FRec: TSearchRec;
  Paths: TStringList;
  I: Integer;
begin
  Result := FileExists(FileName);
  if Result then
    Exit;
  Paths := TStringList.Create;
  Paths.Delimiter:=PathSeparator;
  Paths.DelimitedText := GetEnvironmentVariable('PATH');
  for I := 0 to Paths.Count-1 do
  begin
    if FindFirst(IncludeTrailingPathDelimiter(Paths[I])+FileName,
          faAnyFile and not faDirectory, FRec) = 0 then
      Result := True;
    FindClose(FRec);
    if Result then break;
  end;
  Paths.Free;
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
    if FileExists(PackageName+PathDelim+XMLFile) then
    begin
      InputList.Add('..'+PathDelim+PasSrcDir+FileList[I] + ' -Fi..'+PathDelim+PasSrcDir+'include');
      ArgParams:=ArgParams+' --descr='+XMLSrcDir+ChangeFileExt(FileList[I],'.xml');
    end
    else
      WriteLn('Warning! No corresponding xml file for unit ' + FileList[I]);
  end;
  FileList.Free;
  InputList.SaveToFile(PackageName+PathDelim+InputFileList);
  InputList.Free;
end;

procedure Run;
var
  Process: TProcess;
  CmdLine: String;
begin
  CmdLine := fpdoc + FPDocParams + ArgParams + EnvParams;
  if ShowCmd then
  begin
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
  Process := TProcess.Create(nil);
  Process.Options := Process.Options + [poWaitOnExit];
  Process.CurrentDirectory := GetCurrentDir+PathDelim+PackageName;
  Process.CommandLine := CmdLine;
  Process.Execute;
  Process.Free;
end;

begin
  ReadOptions;
  if Not DirectoryExists(PackageName) then
    mkdir(PackageName);
  InitVars;
  MakeFileList;
  Run;
end.

