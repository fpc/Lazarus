program aarreupdatelist;

{$mode objfpc}{$H+}

uses
  {$IFDEF Unix}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, SysUtils, LazFileUtils, LazLogger, Laz2_XMLCfg, CustApp, contnrs,
  AarrePkgList;

type

  { TAarreUpdateList }

  TAarreUpdateList = class(TCustomApplication)
  private
    FDirectory: string;
    FList: TAarrePkgList;
    FOutputFile: string;
    FQuiet: boolean;
    FVerbose: boolean;
    procedure SetDirectory(AValue: string);
    procedure SetOutputFile(AValue: string);
  protected
    procedure DoRun; override;
    procedure ScanDirectory(Dir: string);
    procedure AddLPK(LPKFilename: string);
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure WriteHelp; virtual;

    // options
    property Verbose: boolean read FVerbose write FVerbose;
    property Quiet: boolean read FQuiet write FQuiet;
    property Directory: string read FDirectory write SetDirectory;
    property OutputFile: string read FOutputFile write SetOutputFile;

    property List: TAarrePkgList read FList;
  end;

{ TAarreUpdateList }

procedure TAarreUpdateList.SetDirectory(AValue: string);
begin
  if FDirectory=AValue then Exit;
  FDirectory:=AValue;
end;

procedure TAarreUpdateList.SetOutputFile(AValue: string);
begin
  if FOutputFile=AValue then Exit;
  FOutputFile:=AValue;
end;

procedure TAarreUpdateList.DoRun;

  procedure E(ErrMsg: string);
  begin
    writeln('ERROR: ',ErrMsg);
    Halt;
  end;

var
  ErrorMsg: String;
begin
  // quick check parameters
  ErrorMsg:=CheckOptions('hvqd:o:','help verbose quiet directory: output:');
  if ErrorMsg<>'' then
    E(ErrorMsg);

  // parse parameters
  if HasOption('h','help') or not HasOption('d','directory') then begin
    WriteHelp;
    Exit;
  end;

  Quiet:=HasOption('q','quiet');
  Verbose:=HasOption('v','verbose');
  if HasOption('o','output') then
    OutputFile:=GetOptionValue('o','output');
  if (OutputFile='') then
    E('output file is empty');
  OutputFile:=TrimAndExpandFilename(OutputFile);
  if not DirectoryExistsUTF8(ExtractFilePath(OutputFile)) then
    E('missing output directory: '+ExtractFilePath(OutputFile));
  if DirectoryExistsUTF8(OutputFile) then
    E('output file is directory: '+OutputFile);
  Directory:=GetOptionValue('d','directory');
  if Directory='' then
    E('directory is empty');
  Directory:=TrimAndExpandFilename(Directory);
  if not DirectoryExistsUTF8(Directory) then
    E('directory not found: '+Directory);

  if not Quiet then begin
    debugln(['scanning directory "',Directory,'" ...']);
  end;

  ScanDirectory(Directory);
  WriteLn(List.AsString);

  // stop program loop
  Terminate;
end;

procedure TAarreUpdateList.ScanDirectory(Dir: string);
var
  fileinfo: TSearchRec;
  Filename: TFilename;
begin
  Dir:=AppendPathDelim(Dir);
  if FindFirstUTF8(Dir+AllFilesMask,faAnyFile,fileinfo)=0 then begin
    repeat
      Filename:=fileinfo.Name;
      if (Filename='') or (Filename='.') or (Filename='..')
      or (Filename='.svn') or (Filename='CVS') or (Filename='.git') then
        continue;
      if faDirectory and fileinfo.Attr>0 then begin
        ScanDirectory(Dir+Filename);
      end else begin
        if ExtractFileExt(Filename)='.lpk' then
          AddLPK(Dir+Filename);
      end;
    until FindNextUTF8(fileinfo)<>0;
  end;
  FindCloseUTF8(fileinfo);
end;

procedure TAarreUpdateList.AddLPK(LPKFilename: string);
var
  Pkg: TAarrePkgListItem;
  ok: Boolean;
begin
  DebugLn(['TAarreUpdateList.AddLPK ',LPKFilename]);

  Pkg:=TAarrePkgListItem.Create;
  ok:=false;
  try
    Pkg.LoadLPK(LPKFilename);
    ok:=true;
  except
    on E: Exception do begin
      debugln(['ERROR: while reading "'+LPKFilename+'": '+E.Message]);
    end;
  end;
  if ok then begin
    List.Add(Pkg);
  end else
    Pkg.Free;
end;

constructor TAarreUpdateList.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  StopOnException:=True;
  Verbose:=false;
  Quiet:=false;
  Directory:='.';
  OutputFile:='packages.gz';
  FList:=TAarrePkgList.Create;
end;

destructor TAarreUpdateList.Destroy;
begin
  FreeAndNil(FList);
  inherited Destroy;
end;

procedure TAarreUpdateList.WriteHelp;
begin
  writeln('Usage: ',ExeName,' -h');
  writeln;
  writeln('-h : write this help and exit');
  writeln('-v : be more verbose');
  writeln('-q : be quiet');
  writeln('-d <repository>  : The directory to scan.');
  writeln('-o <pkglistfile.gz> : The file to create. Default: ',OutputFile);
  Terminate;
end;

var
  Application: TAarreUpdateList;
begin
  Application:=TAarreUpdateList.Create(nil);
  Application.Title:='Aarre Update List';
  Application.Run;
  Application.Free;
end.

