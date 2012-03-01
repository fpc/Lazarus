{ Converter for wiki pages to fpdoc, html, xhtml

  Copyright (C) 2012  Mattias Gaertner  mattias@freepascal.org

  This source is free software; you can redistribute it and/or modify it under
  the terms of the GNU General Public License as published by the Free
  Software Foundation; either version 2 of the License, or (at your option)
  any later version.

  This code is distributed in the hope that it will be useful, but WITHOUT ANY
  WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
  FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
  details.

  A copy of the GNU General Public License is available on the World Wide Web
  at <http://www.gnu.org/copyleft/gpl.html>. You can also obtain it by writing
  to the Free Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
  MA 02111-1307, USA.
}
program wikiconvert;

{$mode objfpc}{$H+}

uses
  Classes, SysUtils, CustApp, WikiParser, WikiFormat,
  Wiki2FPDocConvert, Wiki2XHTMLConvert, Wiki2HTMLConvert, Wiki2CHMConvert,
  LazFileUtils, FileUtil, LazLogger, KeywordFuncLists;

type

  { TWiki2FPDocApplication }

  TWiki2FPDocApplication = class(TCustomApplication)
  private
    FCHMConverter: TWiki2CHMConverter;
    FConverter: TWiki2FormatConverter;
    FFPDocConverter: TWiki2FPDocConverter;
    FHTMLConverter: TWiki2HTMLConverter;
    FLanguageTags: TKeyWordFunctionList;
    FXHTMLConverter: TWiki2XHTMLConverter;
    procedure Test;
  protected
    procedure DoRun; override;
    procedure SearchFileFound(FileIterator: TFileIterator);
    procedure AddFiles(aFileMask: string);
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure WriteHelp; virtual;
    property FPDocConverter: TWiki2FPDocConverter read FFPDocConverter;
    property XHTMLConverter: TWiki2XHTMLConverter read FXHTMLConverter;
    property HTMLConverter: TWiki2HTMLConverter read FHTMLConverter;
    property CHMConverter: TWiki2CHMConverter read FCHMConverter;
    property Converter: TWiki2FormatConverter read FConverter;
    property LanguageTags: TKeyWordFunctionList read FLanguageTags;
  end;

{ TMyApplication }

procedure TWiki2FPDocApplication.DoRun;
var
  i: Integer;
  Param: String;
  ParamName: String;
  p: SizeInt;
  ParamValue: String;

  procedure E(ErrorMsg: string; ShowHelp: boolean = false);
  begin
    writeln('ERROR: ',ErrorMsg);
    if ShowHelp then WriteHelp;
    Terminate;
  end;

begin
  //Test;

  // parse parameters

  // first parse --format= to give nicer help
  if HasOption('format') then begin
    Param:=GetOptionValue('format');
    if Param='fpdoc' then
      fConverter:=FPDocConverter
    else if Param='xhtml' then
      fConverter:=XHTMLConverter
    else if Param='html' then
      fConverter:=HTMLConverter
    else if Param='chm' then
      fConverter:=CHMConverter
    else begin
      writeln('Error: invalid format: '+Param);
      writeln('use option -h to see all formats');
      Terminate; exit;
    end;
  end;

  if HasOption('h','help') then begin
    WriteHelp;
    Terminate; exit;
  end;

  if not HasOption('format') then begin
    writeln('Error: missing option --format=');
    writeln('use option -h to see all options');
    Terminate; exit;
  end;

  // wiki files
  for i:=1 to ParamCount do begin
    Param:=ParamStrUTF8(i);
    if Param='' then continue;
    if (length(Param)>2) and (Param[1]='-') and (Param[2]='-') then begin
      ParamName:=copy(Param,3,length(Param));
      p:=Pos('=',ParamName);
      if p<1 then begin
        E('invalid parameter "'+Param+'"');
        exit;
      end;
      ParamValue:=copy(ParamName,p+1,length(ParamName));
      ParamName:=copy(ParamName,1,p-1);
      // general parameters
      if ParamName='format' then begin
        // already handled above
        continue;
      end else if ParamName='outputdir' then begin
        Param:=TrimAndExpandDirectory(ParamValue);
        if not DirPathExists(Param) then begin
          E('outputdir not found: '+Param);
          exit;
        end;
        Converter.OutputDir:=Param;
        continue;
      end else if ParamName='imagesdir' then begin
        Param:=TrimAndExpandDirectory(ParamValue);
        if not DirPathExists(Param) then begin
          E('imagesdir not found: '+Param);
          exit;
        end;
        Converter.ImagesDir:=Param;
        continue;
      end else if ParamName='title' then begin
        Converter.Title:=Param;
        continue;
      end else if ParamName='nowarnurl' then begin
        Converter.NoWarnBaseURLs[ParamName]:='1';
        continue;
      end;
      if Converter is TWiki2FPDocConverter then begin
        // FPDoc parameters
        if ParamName='root' then begin
          TWiki2FPDocConverter(Converter).RootName:=ParamValue;
          continue;
        end
        else if ParamName='package' then begin
          TWiki2FPDocConverter(Converter).PackageName:=ParamValue;
          continue;
        end;
      end;
      if Converter is TWiki2XHTMLConverter then begin
        // HTML parameters
        if ParamName='css' then begin
          TWiki2XHTMLConverter(Converter).CSSFilename:=ParamValue;
          continue;
        end
      end;
      if Converter is TWiki2CHMConverter then begin
        // CHM parameters
        if ParamName='chm' then begin
          ParamValue:=TrimAndExpandFilename(ParamValue);
          if (ParamValue='') or (not DirPathExists(ExtractFilePath(ParamValue)))
          then begin
            E('directory of chm file does not exists: '+ParamValue);
            exit;
          end;
          TWiki2CHMConverter(Converter).CHMFile:=ParamValue;
          continue;
        end
      end;
    end else if Param[1]<>'-' then begin
      AddFiles(Param);
      if Terminated then exit;
    end else begin
      E('unknown parameter "'+Param+'"');
      exit;
    end;
  end;

  if Converter.Count=0 then begin
    writeln('Error: give at last one wiki file');
    WriteHelp;
    Terminate; exit;
  end;

  Converter.OutputDir:=TrimAndExpandDirectory(Converter.OutputDir);
  Converter.ImagesDir:=TrimAndExpandDirectory(Converter.ImagesDir);

  Converter.Convert;

  // stop program loop
  Terminate;
end;

procedure TWiki2FPDocApplication.SearchFileFound(FileIterator: TFileIterator);
var
  Filename: String;
begin
  Filename:=TrimAndExpandFilename(FileIterator.FileName);
  Converter.AddWikiPage(Filename);
end;

procedure TWiki2FPDocApplication.AddFiles(aFileMask: string);
var
  Filename: String;
  Search: TFileSearcher;
begin
  if (Pos('*',aFileMask)>0) or (Pos('?',aFileMask)>0) then begin
    // file mask
    Search:=TFileSearcher.Create;
    try
      Search.OnFileFound:=@SearchFileFound;
      Search.Search(ExtractFilePath(aFileMask),ExtractFilename(aFileMask),false);
    finally
      Search.Free;
    end;
  end else begin
    // single file
    Filename:=TrimAndExpandFilename(aFileMask);
    if not FileExistsUTF8(Filename) then begin
      writeln('Error: wiki file not found: ',Filename);
      Terminate;  exit;
    end;
    if DirPathExists(Filename) then begin
      writeln('Error: wiki file is a directory: ',Filename,'. Note: You can use file masks.');
      Terminate;  exit;
    end;
    Converter.AddWikiPage(Filename);
  end;
end;

procedure TWiki2FPDocApplication.Test;
  procedure t(Page: string);
  var
    Filename: String;
    NewPage: String;
  begin
    Filename:=WikiPageToFilename(Page,false,true);
    NewPage:=WikiFilenameToPage(Filename);
    writeln('t Page="',Page,'" NewPage="',NewPage,'" File="',Filename,'" Fits=',Page=NewPage);
  end;
begin
  t('ACS_demo.jpg');
  Halt;
end;

constructor TWiki2FPDocApplication.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  StopOnException:=True;
  FLanguageTags:=WikiCreateCommonLanguageList(true);
  FFPDocConverter:=TWiki2FPDocConverter.Create;
  FPDocConverter.LanguageTags:=LanguageTags;
  FXHTMLConverter:=TWiki2XHTMLConverter.Create;
  XHTMLConverter.LanguageTags:=LanguageTags;
  FHTMLConverter:=TWiki2HTMLConverter.Create;
  HTMLConverter.LanguageTags:=LanguageTags;
  FCHMConverter:=TWiki2CHMConverter.Create;
  CHMConverter.LanguageTags:=LanguageTags;
  FConverter:=FHTMLConverter;
end;

destructor TWiki2FPDocApplication.Destroy;
begin
  FreeAndNil(FCHMConverter);
  FreeAndNil(FHTMLConverter);
  FreeAndNil(FXHTMLConverter);
  FreeAndNil(FFPDocConverter);
  FreeAndNil(FLanguageTags);
  inherited Destroy;
end;

procedure TWiki2FPDocApplication.WriteHelp;
begin
  writeln('Usage: ',ExeName,' -h');
  writeln;
  writeln('  -h or --help : this help');
  writeln('  --format=[fpdoc|html|xhtml|chm]');
  writeln('  --outputdir=<output directory> : directory for all files. default: ',Converter.OutputDir);
  writeln('  --imagesdir=<images directory> : directory of image files. default: ',Converter.ImagesDir);
  writeln('  --title=<string> : the title of the wiki. default: "',Converter.Title,'"');
  writeln('  --nowarnurl=<string> : do not warn for URLs starting with this. Can be given multiple times.');
  writeln('  <inputfile> : wiki page in xml format, can be given multiple times');
  writeln('     Duplicates are ignored.');
  writeln('     You can use globbing, like "xml/*.html". You must quote such parameters on console/shell.');
  writeln;
  writeln('Options for --format=fpdoc :');
  writeln('  --root=<fpdoc xml root node> : default: ',FPDocConverter.RootName);
  writeln('  --package=<fpdoc package name> : default: ',FPDocConverter.PackageName);
  writeln;
  writeln('Options for --format=xhtml,html,chm :');
  writeln('  --css=<path of stylesheet> : default: ',XHTMLConverter.CSSFilename);
  writeln;
  writeln('Options for --format=chm :');
  writeln('   Note: the default page is the first page');
  writeln('  --chm=<output chm file> : default: ',CHMConverter.CHMFile);
  writeln;
  writeln('Examples:');
  writeln('  ',ParamStrUTF8(0),' --format=fpdoc --outputdir=fpdoc wikipage1.xml wikipage2.xml');
end;

var
  Application: TWiki2FPDocApplication;
begin
  Application:=TWiki2FPDocApplication.Create(nil);
  Application.Title:='WikiConvert';
  Application.Run;
  Application.Free;
end.

