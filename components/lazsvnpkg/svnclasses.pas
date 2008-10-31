{ Copyright (C) 2008 Darius Blaszijk

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

unit SVNClasses;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ComCtrls, FileUtil, XMLCfg, LCLProc, Dialogs, Controls,
  XMLRead, DOM, Process;

resourcestring
  rsAction = 'Action';
  rsAdded = 'Added';
  rsAuthor = 'Author';
  rsCommit = 'Commit';
  rsCommitRevision = 'Commit revision';
  rsConflict = 'Conflict';
  rsCopyFromPath = 'Copy from path';
  rsDate = 'Date';
  rsDelete = 'Delete';
  rsDeleted = 'Deleted';
  rsDiffActiveFile = 'Diff active file';
  rsEdit = 'Edit';
  rsExtension = 'Extension';
  rsFileStatus = 'File status';
  rsIndexOutOfBoundsD = 'Index out of bounds (%d)';
  rsLazarusSVNCommit = 'LazarusSVN Commit';
  rsLazarusSVNDiff = '%s - LazarusSVN Diff...';
  rsLazarusSVNLog = '%s - LazarusSVN Log...';
  rsLazarusSVNUpdate = '%s - LazarusSVN Update...';
  rsMerged = 'Merged';
  rsMessage = 'Message';
  rsPath = 'Path';
  rsProjectFilename = 'Project filename';
  rsProjectIsActive = 'Project is active';
  rsProjectIsNotActiveInSVNSettingsPleaseActivateFirst = 'Project is not '
    +'active in SVN settings, please activate first.';
  rsProjectName = 'Project name';
  rsPropertyStatus = 'Property status';
  rsRepositoryPath = 'Repository path';
  rsRevision = 'Revision';
  rsSave = 'Save';
  rsSettings = 'Settings';
  rsShowDiff = 'Show diff';
  rsShowLog = 'Show log';
  rsSourceFileDoesNotBelongToTheProjectPleaseAddFirst = 'Source file does not '
    +'belong to the project. Please add first.';
  rsSVNTools = 'SVN tools';
  rsUpdate = 'Update';
  rsUpdated = 'Updated';

const
   READ_BYTES = 2048;

type
  TSortDirection  = (sdAscending, sdDescending);
  //TColumnSortInfo = record
    //Index: integer;
    //SortOrder: TSortOrder;
  //end;

  { TSVNSettings }

  TSVNSettings = class(TObject)
  private
    { private declarations }
    XML: TXMLConfig;
    function GetActive(Index: integer): boolean;
    function GetPath(Index: integer): string;
    function GetProjectCount: integer;
    function GetRepository(Index: integer): string;
    procedure SetActive(Index: integer; const AValue: boolean);
    procedure SetPath(Index: integer; const AValue: string);
    procedure SetProjectCount(const AValue: integer);
    procedure SetRepository(Index: integer; const AValue: string);
  public
    { public declarations }
    constructor Create(const AFileName: string);
    destructor Destroy; override;

    function RepositoryByPath(APath: string; var ARepo: string): boolean;
    procedure AddProject(APath, ARepo: string; AActive: boolean = False);
    procedure UpdateProject(APath, ARepo: string; AActive: boolean);
    procedure DeleteProjectByIndex(Index: integer);

    property ProjectCount: integer read GetProjectCount write SetProjectCount;
    property Path[Index: integer]: string read GetPath write SetPath;
    property Repository[Index: integer]: string read GetRepository write SetRepository;
    property Active[Index: integer]: boolean read GetActive write SetActive;
  end;

  TStatusItemName = (siChecked, siPath, siExtension, siPropStatus, siItemStatus,
                     siRevision, siCommitRevision, siAuthor, siDate);

  PSVNStatusItem = ^TSVNStatusItem;
  TSVNStatusItem = record
    Checked: boolean;
    Path: string;
    Extension: string;
    PropStatus: string;
    ItemStatus: string;
    Revision: integer;
    CommitRevision: integer;
    Author: string;
    Date: TDate;
  end;

  { TSVNStatus }

  TSVNStatus = class(TObject)
  private
    FRepository: string;
    FSortDirection: TSortDirection;
    FSortItem: TStatusItemName;
  public
    List: TFPList;

    constructor Create(const ARepoPath: string);
    destructor Destroy; override;

    property Repository: string read FRepository write FRepository;
    procedure Sort(ASortItem: TStatusItemName; ADirection: TSortDirection);
    procedure ReverseSort(ASortItem: TStatusItemName);
    property SortDirection: TSortDirection read FSortDirection write FSortDirection;
    property SortItem: TStatusItemName read FSortItem write FSortItem;
  end;

var
  SVNSettings: TSVNSettings;

procedure SetColumn(ListView: TListView; ColNo, DefaultWidth: integer; AName: string; AutoSize: boolean = true);
function SVNExecutable: string;
function ReplaceLineEndings(const s, NewLineEnds: string): string;
function ISO8601ToDateTime(DateTime: string): TDateTime;

implementation

uses
  SVNAddProjectForm;

procedure SetColumn(ListView: TListView; ColNo, DefaultWidth: integer; AName: string; AutoSize: boolean = true);
begin
  ListView.Column[ColNo].Caption:=AName;
  ListView.Column[ColNo].AutoSize:=AutoSize;
  ListView.Column[ColNo].Width:=DefaultWidth;
end;

function SVNExecutable: string;
begin
  //encapsulate with " because of the incompatibility on windows
  //when svn in in "Program Files" directory
  Result := '"' + FindDefaultExecutablePath('svn') + '"';
end;

function ReplaceLineEndings(const s, NewLineEnds: string): string;
var
  p: Integer;
  StartPos: LongInt;
begin
  Result:=s;
  p:=1;
  while (p<=length(Result)) do begin
    if Result[p] in [#10,#13] then begin
      StartPos:=p;
      if (p<length(Result))
      and (Result[p+1] in [#10,#13]) and (Result[p]<>Result[p+1]) then
        inc(p);
      Result:=copy(Result,1,StartPos-1)+NewLineEnds+copy(Result,p+1,length(Result));
      inc(p,length(NewLineEnds));
    end else begin
      inc(p);
    end;
  end;
end;

function ISO8601ToDateTime(DateTime: string): TDateTime;
var
  y, m, d, h, n, s: word;
begin
  y := StrToInt(Copy(DateTime, 1, 4));
  m := StrToInt(Copy(DateTime, 6, 2));
  d := StrToInt(Copy(DateTime, 9, 2));
  h := StrToInt(Copy(DateTime, 12, 2));
  n := StrToInt(Copy(DateTime, 15, 2));
  s := StrToInt(Copy(DateTime, 18, 2));

  Result := EncodeDate(y,m,d) + EncodeTime(h,n,s,0);
end;

{ TSVNSettings }

function TSVNSettings.GetActive(Index: integer): boolean;
begin
  Result := XML.GetValue('projects/item' + IntToStr(Index) + '/active', False);
end;

function TSVNSettings.GetPath(Index: integer): string;
begin
  Result := XML.GetValue('projects/item' + IntToStr(Index) + '/path', '');
end;

function TSVNSettings.GetProjectCount: integer;
begin
  Result := XML.GetValue('projects/count', 0);
end;

function TSVNSettings.GetRepository(Index: integer): string;
begin
  Result := XML.GetValue('projects/item' + IntToStr(Index) + '/repository', '');
end;

procedure TSVNSettings.SetActive(Index: integer; const AValue: boolean);
begin
  XML.SetValue('projects/item' + IntToStr(Index) + '/active', AValue);
end;

procedure TSVNSettings.SetPath(Index: integer; const AValue: string);
begin
  XML.SetValue('projects/item' + IntToStr(Index) + '/path', AValue);
end;

procedure TSVNSettings.SetProjectCount(const AValue: integer);
begin
  XML.SetValue('projects/count', AValue);
end;

procedure TSVNSettings.SetRepository(Index: integer; const AValue: string);
begin
  XML.SetValue('projects/item' + IntToStr(Index) + '/repository', AValue);
end;

constructor TSVNSettings.Create(const AFileName: string);
begin
  XML := TXMLConfig.Create(nil);
  XML.Filename:=AFileName;
end;

destructor TSVNSettings.Destroy;
begin
  XML.Flush;
  XML.Free;

  inherited Destroy;
end;

procedure TSVNSettings.AddProject(APath, ARepo: string; AActive: boolean);
var
  count: integer;
begin
  count := ProjectCount;
  Inc(Count);

  ProjectCount := count;
  Path[Count - 1] := APath;
  Active[Count - 1] := AActive;
  Repository[Count - 1] := ARepo;
end;

procedure TSVNSettings.UpdateProject(APath, ARepo: string; AActive: boolean);
var
  count: integer;
  i: integer;
begin
  debugln('TSVNSettings.UpdateProject searching for project');
  count := ProjectCount;

  for i := 0 to Count - 1 do
    if Path[i] = APath then
    begin
      Active[i] := AActive;
      Repository[i] := ARepo;
      exit;
    end;

  //project not found, so add it as new
  debugln('TSVNSettings.UpdateProject project not found adding a new one');
  AddProject(APath, ARepo, AActive);
end;

procedure TSVNSettings.DeleteProjectByIndex(Index: integer);
var
  i: integer;
  count: integer;
begin
  count := ProjectCount;
  ProjectCount := count - 1;
  for i := Index to count - 1 do
  begin
    Active[i] := Active[i + 1];
    Path[i] := Path[i + 1];
    Repository[i] := Repository[i + 1];
  end;

  XML.DeletePath('projects/item' + IntToStr(count - 1) + '/active');
  XML.DeletePath('projects/item' + IntToStr(count - 1) + '/path');
  XML.DeletePath('projects/item' + IntToStr(count - 1) + '/repository');
end;

function TSVNSettings.RepositoryByPath(APath: string; var ARepo: string
  ): boolean;
var
  count: integer;
  i: integer;
begin
  debugln('TSVNSettingsFrm.GetRepository APath=' + APath);

  count := ProjectCount;

  for i := 0 to Count - 1 do
    if Path[i] = APath then
    begin
      Result := Active[i];
      ARepo := Repository[i];
      exit;
    end;

  if QuestionDlg('Project not found',
                 'Current project not in project list. Would you like to add it?',
                 mtWarning,
                 [mrYes, mrNo],
                 0) = mrYes then
  begin
    ShowSVNAddProjectFrm(APath, ARepo, True);
    Result := True;
  end;
end;

function SortSelectedAscending(Item1, Item2: Pointer): Integer;
begin
   if PSVNStatusItem(Item1)^.Checked > PSVNStatusItem(Item2)^.Checked then
     Result := 1
   else
     if PSVNStatusItem(Item1)^.Checked = PSVNStatusItem(Item2)^.Checked then
       Result := 0
     else
       Result := -1;
end;

function SortSelectedDescending(Item1, Item2: Pointer): Integer;
begin
  Result := -SortSelectedAscending(Item1, Item2);
end;

function SortPathAscending(Item1, Item2: Pointer): Integer;
begin
   Result := CompareText(PSVNStatusItem(Item1)^.Path, PSVNStatusItem(Item2)^.Path);
end;

function SortPathDescending(Item1, Item2: Pointer): Integer;
begin
  Result := -SortPathAscending(Item1, Item2);
end;

function SortExtensionAscending(Item1, Item2: Pointer): Integer;
begin
   Result := CompareText(PSVNStatusItem(Item1)^.Extension, PSVNStatusItem(Item2)^.Extension);
end;

function SortExtensionDescending(Item1, Item2: Pointer): Integer;
begin
  Result := -SortExtensionAscending(Item1, Item2);
end;

function SortItemStatusAscending(Item1, Item2: Pointer): Integer;
begin
   Result := CompareText(PSVNStatusItem(Item1)^.ItemStatus, PSVNStatusItem(Item2)^.ItemStatus);
end;

function SortItemStatusDescending(Item1, Item2: Pointer): Integer;
begin
  Result := -SortItemStatusAscending(Item1, Item2);
end;

function SortPropStatusAscending(Item1, Item2: Pointer): Integer;
begin
   Result := CompareText(PSVNStatusItem(Item1)^.PropStatus, PSVNStatusItem(Item2)^.PropStatus);
end;

function SortPropStatusDescending(Item1, Item2: Pointer): Integer;
begin
  Result := -SortPropStatusAscending(Item1, Item2);
end;

function SortPropertyAuthorAscending(Item1, Item2: Pointer): Integer;
begin
   Result := CompareText(PSVNStatusItem(Item1)^.Author, PSVNStatusItem(Item2)^.Author);
end;

function SortPropertyAuthorDescending(Item1, Item2: Pointer): Integer;
begin
  Result := -SortPropertyAuthorAscending(Item1, Item2);
end;

function SortPropertyRevisionAscending(Item1, Item2: Pointer): Integer;
begin
   if PSVNStatusItem(Item1)^.Revision > PSVNStatusItem(Item2)^.Revision then
     Result := 1
   else
     if PSVNStatusItem(Item1)^.Revision = PSVNStatusItem(Item2)^.Revision then
       Result := 0
     else
       Result := -1;
end;

function SortPropertyRevisionDescending(Item1, Item2: Pointer): Integer;
begin
  Result := -SortPropertyRevisionAscending(Item1, Item2);
end;

function SortPropertyCommitRevisionAscending(Item1, Item2: Pointer): Integer;
begin
   if PSVNStatusItem(Item1)^.CommitRevision > PSVNStatusItem(Item2)^.CommitRevision then
     Result := 1
   else
     if PSVNStatusItem(Item1)^.CommitRevision = PSVNStatusItem(Item2)^.CommitRevision then
       Result := 0
     else
       Result := -1;
end;

function SortPropertyCommitRevisionDescending(Item1, Item2: Pointer): Integer;
begin
  Result := -SortPropertyCommitRevisionAscending(Item1, Item2);
end;

function SortPropertyDateAscending(Item1, Item2: Pointer): Integer;
begin
   if PSVNStatusItem(Item1)^.Date > PSVNStatusItem(Item2)^.Date then
     Result := 1
   else
     if PSVNStatusItem(Item1)^.Date = PSVNStatusItem(Item2)^.Date then
       Result := 0
     else
       Result := -1;
end;

function SortPropertyDateDescending(Item1, Item2: Pointer): Integer;
begin
  Result := -SortPropertyDateAscending(Item1, Item2);
end;

{ TSVNStatus }

constructor TSVNStatus.Create(const ARepoPath: string);
var
  Doc: TXMLDocument;
  Node: TDOMNode;
  SubNode: TDOMNode;
  ListItem: PSVNStatusItem;
  AProcess: TProcess;
  i: integer;
  M: TMemoryStream;
  n: LongInt;
  BytesRead: LongInt;
  F: LongInt;
  Path: string;
  NodeName: string;
  NodeValue: string;
begin
  List := TFPList.Create;
  Repository := ARepoPath;

  M := TMemoryStream.Create;
  BytesRead := 0;

  AProcess := TProcess.Create(nil);
  AProcess.CommandLine := SVNExecutable + ' stat --verbose --xml ' + Repository  + ' --non-interactive';
  debugln('TSVNStatus.Create CommandLine ' + AProcess.CommandLine);
  AProcess.Options := AProcess.Options + [poUsePipes, poStdErrToOutput];
  AProcess.ShowWindow := swoHIDE;
  AProcess.Execute;

  while AProcess.Running do
  begin
    // make sure we have room
    M.SetSize(BytesRead + READ_BYTES);

    // try reading it
    n := AProcess.Output.Read((M.Memory + BytesRead)^, READ_BYTES);
    if n > 0 then
      Inc(BytesRead, n)
    else
      // no data, wait 100 ms
      Sleep(100);
  end;
  // read last part
  repeat
    // make sure we have room
    M.SetSize(BytesRead + READ_BYTES);
    // try reading it
    n := AProcess.Output.Read((M.Memory + BytesRead)^, READ_BYTES);
    if n > 0 then
      Inc(BytesRead, n);
  until n <= 0;
  M.SetSize(BytesRead);

  ReadXMLFile(Doc, M);

  AProcess.Free;
  M.Free;

  //now process the XML file
  Node := Doc.DocumentElement.FirstChild.FirstChild;
  repeat
    SubNode := Node;

    New(ListItem);

    Path := SubNode.Attributes.Item[0].NodeValue;

    F:=FileGetAttr(Path);
    If F<>-1 then
      If (F and faDirectory)=0 then
      begin
        //path
        ListItem^.Path := Path;

        //Extension
        ListItem^.Extension:=ExtractFileExt(Path);

        //get the wc-status attributes
        ListItem^.ItemStatus:='';
        ListItem^.Checked:=False;
        ListItem^.PropStatus:='';
        for i := 0 to SubNode.ChildNodes.Item[0].Attributes.Length -1 do
        begin
          NodeName := SubNode.ChildNodes.Item[0].Attributes.Item[i].NodeName;
          NodeValue := SubNode.ChildNodes.Item[0].Attributes.Item[i].NodeValue;

          if NodeName = 'item' then
          begin
            //ItemStatus
            ListItem^.ItemStatus := NodeValue;

            //Checked
            ListItem^.Checked:=(NodeValue<>'unversioned') and (NodeValue<>'normal');
          end;

          if NodeName = 'props' then
            //PropStatus
            ListItem^.PropStatus := NodeValue;

          if NodeName = 'revision' then
            //Revision
            ListItem^.Revision := StrToInt(NodeValue);
        end;

        //get the commit attributes
        if Assigned(SubNode.ChildNodes.Item[0].ChildNodes.Item[0]) then
        begin
          //CommitRevision
          ListItem^.CommitRevision:=StrToInt(SubNode.ChildNodes.Item[0].ChildNodes.Item[0].Attributes.Item[0].NodeValue);

          if Assigned(SubNode.ChildNodes.Item[0].ChildNodes.Item[0].ChildNodes.Item[0]) then
          begin
            //Author
            ListItem^.Author:=SubNode.ChildNodes.Item[0].ChildNodes.Item[0].ChildNodes.Item[0].FirstChild.NodeValue;
            //Date
            ListItem^.Date:=ISO8601ToDateTime(SubNode.ChildNodes.Item[0].ChildNodes.Item[0].ChildNodes.Item[0].NextSibling.FirstChild.NodeValue);
          end;
        end;

        List.Add(ListItem);
      end;

    Node := Node.NextSibling;
  until not Assigned(Node);
end;

destructor TSVNStatus.Destroy;
begin
  List.Free;

  inherited Destroy;
end;

procedure TSVNStatus.Sort(ASortItem: TStatusItemName; ADirection: TSortDirection);
begin
  SortDirection := ADirection;
  SortItem := ASortItem;

  if ADirection = sdDescending then
    case ASortItem of
      siChecked:        List.Sort(@SortSelectedAscending);
      siPath:           List.Sort(@SortPathAscending);
      siExtension:      List.Sort(@SortExtensionAscending);
      siItemStatus:     List.Sort(@SortItemStatusAscending);
      siPropStatus:     List.Sort(@SortPropStatusAscending);
      siAuthor:         List.Sort(@SortPropertyAuthorAscending);
      siRevision:       List.Sort(@SortPropertyRevisionAscending);
      siCommitRevision: List.Sort(@SortPropertyCommitRevisionAscending);
      siDate:           List.Sort(@SortPropertyDateAscending);
    end
  else
    case ASortItem of
      siChecked:        List.Sort(@SortSelectedDescending);
      siPath:           List.Sort(@SortPathDescending);
      siExtension:      List.Sort(@SortExtensionDescending);
      siItemStatus:     List.Sort(@SortItemStatusDescending);
      siPropStatus:     List.Sort(@SortPropStatusDescending);
      siAuthor:         List.Sort(@SortPropertyAuthorDescending);
      siRevision:       List.Sort(@SortPropertyRevisionDescending);
      siCommitRevision: List.Sort(@SortPropertyCommitRevisionDescending);
      siDate:           List.Sort(@SortPropertyDateDescending);
    end;
end;

procedure TSVNStatus.ReverseSort(ASortItem: TStatusItemName);
begin
  if SortItem = ASortItem then
  begin
     if SortDirection = sdDescending then
       Sort(ASortItem, sdAscending)
     else
       Sort(ASortItem, sdDescending)
  end
  else
    Sort(ASortItem, sdAscending);
end;

initialization
  //GetAppConfigDir(False) + 'lazsvnsettings.xml';
  SVNSettings := TSVNSettings.Create('lazsvnsettings.xml');

finalization
  SVNSettings.Free;

end.

