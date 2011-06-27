unit ChmProg;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  Dialogs, FileUtil,
  LazHelpIntf, HelpIntfs,
  IDEHelpIntf, MacroIntf;

const
  sFPCCompilerDirectives = 'FreePascal Compiler directives';

type

  { TFPCDirectivesHelpDatabase }

  TFPCDirectivesHelpDatabase = class(THelpDatabase)
  private
    FDocsDir: string;
    FDirectiveNodes: TFPList;
    function SearchForDirective(ADirective: string;
      var ListOfNodes: THelpNodeQueryList): Boolean;
    procedure ClearDirectiveNodes;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    function GetNodesForDirective(const HelpDirective: string;
      var ListOfNodes: THelpNodeQueryList;
      var ErrMsg: string): TShowHelpResult; override;
    function ShowHelp(Query: THelpQuery; {%H-}BaseNode, NewNode: THelpNode;
                      {%H-}QueryItem: THelpQueryItem;
                      var ErrMsg: string): TShowHelpResult; override;
    property DocsDir: string read FDocsDir write FDocsDir;
  end;

procedure RegisterFPCDirectivesHelpDatabase;

var
  FPCDirectivesHelpDatabase: TFPCDirectivesHelpDatabase = nil;

implementation

uses chmreader, chmFiftiMain;

procedure RegisterFPCDirectivesHelpDatabase;
begin
  if not Assigned(FPCDirectivesHelpDatabase) then
    FPCDirectivesHelpDatabase := TFPCDirectivesHelpDatabase(HelpDatabases.CreateHelpDatabase(sFPCCompilerDirectives,
                                               TFPCDirectivesHelpDatabase, true));
end;

{ TFPCDirectivesHelpDatabase }

function TFPCDirectivesHelpDatabase.SearchForDirective(ADirective: string;
  var ListOfNodes: THelpNodeQueryList): Boolean;
var
  chm: TChmFileList;
  fchm: TChmReader;
  DocTitle, URL: string;
  ms: TMemoryStream;
  SearchReader: TChmSearchReader;
  TitleResults: TChmWLCTopicArray;
  i, k: Integer;
  DirectiveNode: THelpNode;
begin
  ADirective := UpperCase(ADirective);
  Result := False;
  if FDocsDir = '' then
  begin
    FDocsDir := '$(LazarusDir)';
    IDEMacros.SubstituteMacros(FDocsDir);
    FDocsDir := AppendPathDelim(FDocsDir) + 'docs' + PathDelim + 'html';
  end;
  FDocsDir := AppendPathDelim(FDocsDir);

  if FileExistsUTF8(FDocsDir + 'prog.chm') then
  begin
    chm := TChmFileList.Create(Utf8ToSys(FDocsDir + 'prog.chm'));
    try
      if chm.Count = 0 then Exit;
      fchm := chm.Chm[0];

      if fchm.SearchReader = nil then
      begin
        ms := fchm.GetObject('/$FIftiMain');
        if ms = nil then Exit;
        SearchReader := TChmSearchReader.Create(ms, True); //frees the stream when done
        fchm.SearchReader := SearchReader;
      end
      else
        SearchReader := fchm.SearchReader;
      SearchReader.LookupWord(Copy(ADirective, 2, MaxInt), TitleResults);
      for k := 0 to High(TitleResults) do
      begin
        URL := fchm.LookupTopicByID(TitleResults[k].TopicIndex, DocTitle);
        i := Pos(ADirective, DocTitle);
        if (i = 0) or (Length(DocTitle) >= i + Length(ADirective))
          and (upCase(DocTitle[i + Length(ADirective)]) in ['A'..'Z','0'..'9']) then Continue;
        if (Length(URL) > 0) and (URL[1] = '/') then
          Delete(URL, 1, 1);
        if URL = '' then Continue;
        DirectiveNode := THelpNode.CreateURL(Self, ADirective, 'prog.chm://' + URL);
        DirectiveNode.Title := 'FPC directives: ' + DocTitle;
        CreateNodeQueryListAndAdd(DirectiveNode, nil, ListOfNodes, True);
        FDirectiveNodes.Add(DirectiveNode);
        Result := True;
      end;

      fchm.Free;
    finally
      chm.Free;
    end;
  end;
end;

procedure TFPCDirectivesHelpDatabase.ClearDirectiveNodes;
var i: Integer;
begin
  for i := 0 to FDirectiveNodes.Count - 1 do
    TObject(FDirectiveNodes[i]).Free;
  FDirectiveNodes.Clear;
end;

constructor TFPCDirectivesHelpDatabase.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  FDirectiveNodes := TFPList.Create;
end;

destructor TFPCDirectivesHelpDatabase.Destroy;
begin
  ClearDirectiveNodes;
  FDirectiveNodes.Free;
  inherited Destroy;
end;

function TFPCDirectivesHelpDatabase.GetNodesForDirective(
  const HelpDirective: string; var ListOfNodes: THelpNodeQueryList;
  var ErrMsg: string): TShowHelpResult;
var
  Directive: String;
begin
  Result := shrHelpNotFound;
  if (csDesigning in ComponentState) then Exit;
  if (FPCDirectiveHelpPrefix<>'')
  and (LeftStr(HelpDirective, Length(FPCDirectiveHelpPrefix)) = FPCDirectiveHelpPrefix) then
  begin
    if not FileExistsUTF8(FDocsDir + 'prog.chm') then
    begin
      Result := shrDatabaseNotFound;
      ErrMsg := Format('prog.chm not found. Please put prog.chm help file in '+ LineEnding
                         + '%s' +  LineEnding
                         +'or set the path to it with "HelpFilesPath" in '
                         +' Environment Options -> Help -> Help Options ->' + LineEnding
                         +'under Viewers - CHM Help Viewer', [FDocsDir]);
      Exit;
    end;
    // HelpDirective starts with DirectivePrefix
    Directive := Copy(HelpDirective, Length(FPCDirectiveHelpPrefix) + 1, Length(HelpDirective));
    ClearDirectiveNodes;
    if SearchForDirective(Directive, ListOfNodes) then
      Result := shrSuccess;
  end;
end;

function TFPCDirectivesHelpDatabase.ShowHelp(Query: THelpQuery; BaseNode,
  NewNode: THelpNode; QueryItem: THelpQueryItem; var ErrMsg: string
  ): TShowHelpResult;
var
  Viewer: THelpViewer;
begin
  Result:=shrHelpNotFound;
  if not (Query is THelpQueryDirective) then exit;
  Result := FindViewer('text/html', ErrMsg, Viewer);
  if Result <> shrSuccess then Exit;
  Result := Viewer.ShowNode(NewNode, ErrMsg);
end;

end.

