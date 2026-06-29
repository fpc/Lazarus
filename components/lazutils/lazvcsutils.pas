unit LazVCSUtils;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LazFileUtils, Laz2_DOM, Laz_XMLRead, process, StrUtils;

type
  TVCSSCoutLogEvent = procedure(Msg: string) of object;

  { TVCSScout }

  TVCSScout = class(TComponent)
  private
    FMainBranch: string;
    FOnShow: TVCSSCoutLogEvent;
    FRevisionStr: string;
    FSourceDirectory: string;
  public
    function RunCommand(ACmd: String; AParams: array of string; ABytesNeeded: Integer = -1): Boolean; virtual;
    function RunCommand(ACmd: String; AParams: array of string; out ARes: String;
      ABytesNeeded: Integer = -1): Boolean; virtual;
    function CmdInPath(Exe, Param: string): boolean; virtual;
    function SvnInPath: Boolean; virtual;
    function GitInPath: Boolean; virtual;
    function HgInPath: Boolean; virtual;
    function FindRevision: boolean;
    function IsThisGitUpstreamBranch: boolean;
    function GetRevisionFromGitVersion(ACommitIsh: String = '') : boolean;
    function GetGitCommitInRemoteBranch(ARemotePattern: String = '*'): string;
    function GitRevisionFromRemoteSVNGitVersion: boolean;
    function GitDescribeCommit(KeepRevStr: Boolean): boolean;
    procedure Show(const Msg: string); virtual;
    property SourceDirectory: string read FSourceDirectory write FSourceDirectory;
    property MainBranch: string read FMainBranch write FMainBranch;
    property RevisionStr: string read FRevisionStr write FRevisionStr;
    property OnShow: TVCSSCoutLogEvent read FOnShow write FOnShow;
  end;


implementation

function TVCSScout.RunCommand(ACmd: String;
  AParams: array of string; ABytesNeeded: Integer): Boolean;
var
  ADummy: String;
begin
  Result := RunCommand(ACmd, AParams, ADummy, ABytesNeeded);
end;

function TVCSScout.RunCommand(ACmd: String; AParams: array of string; out ARes: String;
  ABytesNeeded: Integer): Boolean;
const
  cBufSize = 500;
var
  p: TProcess;
  Buffer: string;
  n: LongInt;
  t: QWord;
  i: Integer;
begin
  Result := True;
  p := TProcess.Create(nil);
  try
    try
      with p do begin
        Executable := ACmd;
        for i := 0 to high(AParams) do
          Parameters.Add(AParams[i]);
        Options := [poUsePipes];
        ShowWindow := swoHIDE;

        Execute;
        { now process the output }
        SetLength(Buffer{%H-}, cBufSize);
        ARes := '';
        t := GetTickCount64;
        while (ABytesNeeded < 0) or (Length(ARes) < ABytesNeeded) do begin
          if Output.NumBytesAvailable > 0 then begin
            n := OutPut.Read(Buffer[1], cBufSize);
            ARes := ARes + Copy(Buffer, 1, n);
          end
          else begin
            if not p.Running then
              break;
            if GetTickCount64 - t > 30 * 1000 then begin
              Result := False;
              exit
            end;
            if Stderr.NumBytesAvailable > 0 then begin
              n := Stderr.Read(Buffer[1], cBufSize);
              Show(Copy(Buffer, 1, n));
            end;
            sleep(10);
          end;
        end;
      end;
    except
      Result := False;
    end;
  finally
    try
      p.Terminate(0);
    finally
      p.Free;
    end;
  end;
end;

function TVCSScout.CmdInPath(Exe, Param: string): boolean;
begin
  if FindDefaultExecutablePath(Exe+GetExeExt)='' then
    exit(false);
  try
    Result := RunCommand(Exe, [Param], 0);
  except
    Result:=false;
  end;
end;

function TVCSScout.SvnInPath: Boolean;
begin
  Result := CmdInPath('svn', '--version');
end;

function TVCSScout.GitInPath: Boolean;
begin
  Result := CmdInPath('git','--version');
end;

function TVCSScout.HgInPath: Boolean;
begin
  Result := CmdInPath('hg','--version');
end;

function TVCSScout.FindRevision: boolean;
var
  GitDir, SVNDir: string;

  function GetRevisionFromHgVersion : boolean;
  var
    HgVersionProcess: TProcess;
    Buffer: string;
    n: LongInt;
    ScrapeResult: string;
  begin
    Result:=false;
    HgVersionProcess := TProcess.Create(nil);
    try
      with HgVersionProcess do begin
        // Get global revision ID (no need to worry about branches)
        CurrentDirectory:=SourceDirectory;
        Executable:='hg';
        Parameters.Add('parents');
        Parameters.Add('--template={svnrev}:{node|short}');
        Options := [poUsePipes, poWaitOnExit];
        try
          Execute;
          SetLength(Buffer{%H-}, 80);
          n:=OutPut.Read(Buffer[1], 80);

          Result:=true;
          // Just blindly copy results; check for errors below.
          ScrapeResult := Trim(Copy(Buffer, 1, n));
          System.Delete(ScrapeResult, 1, Pos(#13, ScrapeResult));
          System.Delete(ScrapeResult, 1, Pos(#10, ScrapeResult));
          System.Delete(ScrapeResult, Pos(' ', ScrapeResult), Length(ScrapeResult));
          if ScrapeResult[1]='"' then //linux returns "
            ScrapeResult:=copy(ScrapeResult,2,length(ScrapeResult)-2);
          if ScrapeResult[1]=':' then //no svn version found.
            //Indicate we're dealing with Mercurial to avoid confusing the user:
            ScrapeResult:='hg'+ScrapeResult
          else
            ScrapeResult:=copy(ScrapeResult,1,pos(':',ScrapeResult)-1);
        except
        // ignore error, default result is false
        end;
        // Check for errors returned by command (e.g. repository not found)
        if ExitStatus<>0 then
        begin
          show('GetRevisionFromHgRevision: non-zero exit status: no hg repo?');
          result:=false;
        end;
        if result then RevisionStr:=ScrapeResult;
      end;
    finally
      HgVersionProcess.Free;
    end;
    if Result then
    begin
      Show('Success retrieving revision with hg/mercurial.');
    end
    else
    begin
      Show('Failed retrieving revision with hg/mercurial.');
      if n>0 then
      begin
        Show('');
        Show('hg parents error output:');
        Show(Copy(Buffer, 1, n));
      end;
    end;
    Show('');
  end;

  function GetRevisionFromSvnVersion : boolean;
  var
    SvnVersionProcess: TProcess;
    Buffer: string;
    n: LongInt;
  begin
    Result:=false;
    SvnVersionProcess := TProcess.Create(nil);
    try
      with SvnVersionProcess do begin
        Executable:='svnversion';
        Parameters.Add('-n');
        Parameters.Add(SourceDirectory);
        Options := [poUsePipes, poWaitOnExit];
        try
          Execute;
          SetLength(Buffer{%H-}, 80);
          n:=OutPut.Read(Buffer[1], 80);
          RevisionStr := Copy(Buffer, 1, n);

          // If cannot determine svn version it will return localized message
          // "Unversioned directory" with no error result but svn revisions
          // always start with a number.
          Result:=(n > 0) and (RevisionStr[1] in ['0'..'9']);

          SetLength(Buffer, 1024);
          n:=Stderr.Read(Buffer[1], 1024);
        except
        // ignore error, default result is false
        end;
      end;
    finally
      SvnVersionProcess.Free;
    end;
    if Result then
    begin
      Show('Success retrieving revision with svnversion.');
    end
    else
    begin
      Show('Failed retrieving revision with svnversion.');
      if n>0 then
      begin
        Show('');
        Show('svnversion error output:');
        Show(Copy(Buffer, 1, n));
      end;
    end;
    Show('');
  end;

  function GetRevisionFromEntriesTxt: boolean;
  var
    EntriesFileName: string;
    Line: string;
    Lines: TStringList;
    i: Integer;
  begin
    Result:=false;
    EntriesFileName:=AppendPathDelim(SourceDirectory)+'.svn'+PathDelim+'entries';
    if FileExistsUTF8(EntriesFileName) then begin
      try
        Lines:=TStringList.Create;
        try
          Lines.LoadFromFile(EntriesFileName);
          // skip three lines
          i:=0;
          Line:=Lines[i];
          if line<>'<?xml version="1.0" encoding="utf-8"?>' then begin
            inc(i,3);
            RevisionStr:=Lines[i];
            Result := RevisionStr <> '';
          end;
        finally
          Lines.Free;
        end;
      except
        // ignore error, default result is false
      end;
    end;
    if Result then
      Show('Success retrieving revision with entries file: '+EntriesFileName)
    else
      Show('Failure retrieving revision with entries file: '+EntriesFileName);
    Show('');
  end;

  function GetRevisionFromEntriesXml: boolean;
  var
    EntriesFileName: string;
    EntriesDoc: TXMLDocument;
    EntryNode: TDomNode;
  begin
    Result := False;
    EntriesFileName:=AppendPathDelim(SourceDirectory)+'.svn'+PathDelim+'entries';
    if FileExistsUTF8(EntriesFileName) then
    begin
       try
         EntriesDoc := nil;
         try
           ReadXMLFile(EntriesDoc, EntriesFileName);
           EntryNode := EntriesDoc.FirstChild.FirstChild;
           while not Result and Assigned(EntryNode) do
           begin
             if EntryNode.Attributes.GetNamedItem('name').NodeValue='' then
             begin
               RevisionStr:=String(EntryNode.Attributes.GetNamedItem('revision').NodeValue);
               Result := True;
             end;
             EntryNode := EntryNode.NextSibling;
           end;
         finally
           EntriesDoc.Free;
         end;
       except
         // ignore error, default result is false
       end;
    end;
    if Result then
      Show('Success retrieving revision with entries XML file: '+EntriesFileName)
    else
      Show('Failure retrieving revision with entries XML file: '+EntriesFileName);
    Show('');
  end;

begin
  Show('Going to retrieve revision for source directory: '+SourceDirectory);
  Result:=false;

  if not Result then
  begin
    // try git
    GitDir:= AppendPathDelim(SourceDirectory)+'.git';
    if (DirectoryExistsUTF8(GitDir) or FileExistsUTF8(GitDir)) and GitInPath then
    begin
      Result:=GitDescribeCommit(Result);
      if not Result then
        Result := GetRevisionFromGitVersion;
      if not Result then
        Result := GitRevisionFromRemoteSVNGitVersion;
    end;
  end;

  // Try Subversion/svn
  if not Result then
  begin
    SVNDir:= AppendPathDelim(SourceDirectory)+'.svn';
    if DirectoryExistsUTF8(SVNDir) and SvnInPath then
    begin
      // Use or's short circuiting to make sure only the last successful function writes to RevisionStr
      Result := GetRevisionFromSvnVersion or GetRevisionFromEntriesTxt or
                GetRevisionFromEntriesXml;
    end;
  end;

  // Try Mercurial/hg
  if not Result then
  begin
    if HgInPath then
      Result := GetRevisionFromHgVersion;
  end;
end;

{
  Determine what branch we are in by looking at the 'git branch' output.
  Sample output:
    $ git branch
      custom-patches
      docs
      dubydebugger
      externtools
      filebrowser
    * filefilters
      fixes
      graeme
      upstream
      work

}
function TVCSScout.IsThisGitUpstreamBranch: boolean;
const
  cBufSize = 2048;
  MainBranchNames: array[0..1] of string = ('upstream', 'master');
var
  p: TProcess;
  Buffer: string;
  s: string;
  i, j: integer;
  n: LongInt;
  sl: TStringList;
begin
  Result := false;
  p := TProcess.Create(nil);
  sl := TStringList.Create;
  try
    p.Executable:='git';
    p.Parameters.Add('branch');
    p.Options := [poUsePipes, poWaitOnExit];
    p.Execute;
    // Now lets process the output
    SetLength(Buffer{%H-}, cBufSize);
    s := '';
    repeat
      n := p.Output.Read(Buffer[1], cBufSize);
      s := s + Copy(Buffer, 1, n);
    until n < cBufSize;
    sl.Text := s;
    // Search for the active branch marker '*' symbol.
    // Guess the main branch name. Support 'master' and 'upstream'.
    MainBranch := '';
    for i := 0 to sl.Count-1 do begin
      for j := Low(MainBranchNames) to High(MainBranchNames) do begin
        if Pos(MainBranchNames[j], sl[i]) > 0 then begin
          MainBranch := MainBranchNames[j];
          if sl[i][1] = '*' then begin
            Result := True;
            exit;
          end;
        end;
      end;
    end;
  finally
    sl.Free;
    p.Free;
  end;
end;

function TVCSScout.GetRevisionFromGitVersion(ACommitIsh: String
  ): boolean;
var
  s: String;
begin
  if ACommitIsh <> '' then
    Result := RunCommand('git', ['log', '-1', '--pretty=format:"%b"', ACommitIsh], s)
  else
    Result := RunCommand('git', ['log', '-1', '--pretty=format:"%b"'], s);
  if not Result then
    exit;

  // git-svn-id: http://svn.freepascal.org/svn/lazarus/trunk@60656 4005530d-fff6-0310-9dd1-cebe43e6787f
  Result := Pos('git-svn-id: ', s) > 0;
  if Result then begin
    //Read version is OK
    System.Delete(s, 1, Pos('git-svn-id: ', s));
    System.Delete(s, 1, Pos('@', s));
    if PosSet([' ', #10,#13], s) > 0 then
      System.Delete(s, PosSet([' ', #10,#13], s), Length(s));
    RevisionStr := s;
  end;
  if Result then
  begin
    Show('Success retrieving revision with git log.');
  end
  else
  begin
    Show('Failed retrieving revision with git log.');
  end;
end;

{ Determine the closest parent commit that is in a remote branch
}
function TVCSScout.GetGitCommitInRemoteBranch(
  ARemotePattern: String): string;
begin
  if not  RunCommand('git',
    ['log', 'HEAD', '--not', '--remotes="'+ARemotePattern+'"', '--reverse', '--pretty=format:"%H"'],
    Result, 40) // sha1 = 40 bytes
  then begin
    Result := '';
    exit;
  end;

  if Result <> '' then
    Result := Result + '~1'; // parent
end;

{ Get the branch point, and exact the SVN revision from that commit log }
function TVCSScout.GitRevisionFromRemoteSVNGitVersion: boolean;
var
  sha1: string;
begin
  Result := False;
  sha1 := GetGitCommitInRemoteBranch; // try any remote branch
  if sha1 <> '' then begin
    Result := GetRevisionFromGitVersion(sha1);
    if not Result then begin
      // git svn  uses /remote/svn/<branch> as remote
      sha1 := GetGitCommitInRemoteBranch('svn/*');
      Result := GetRevisionFromGitVersion(sha1);
    end;
  end;
  if Result then
    RevisionStr := RevisionStr + '(B)'; // local commits "Based on"
end;

function TVCSScout.GitDescribeCommit(KeepRevStr: Boolean
  ): boolean;
var
  s: string;
begin
  Result := RunCommand('git', ['describe', '--always', '--first-parent'], s);
  if not Result then
    exit;

  if PosSet([#10,#13], s) > 0 then
    System.Delete(s, PosSet([#10,#13], s), Length(s));
  s := Trim(s);
  Result := s <> '';
  if Result then
  begin
    Result := True;
    if KeepRevStr and (RevisionStr <> '') then
      s := s + ' / ' + RevisionStr;
    RevisionStr := s;
  end;
end;

procedure TVCSScout.Show(const Msg: string);
begin
  if Assigned(OnShow) then
    OnShow(Msg);
end;

end.

