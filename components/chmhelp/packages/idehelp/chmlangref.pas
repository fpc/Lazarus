unit ChmLangRef;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  Dialogs, FileUtil,
  LazHelpIntf, HelpIntfs,
  IDEHelpIntf, MacroIntf;

const
  sFPCLangRef = 'FPC Language Reference';

type

  { TLangRefHelpDatabase }

  TLangRefHelpDatabase = class(THelpDatabase)
  private
    FKeywordNodes: TList;
    FKeyWordsList: TStringList;
    procedure ClearKeywordNodes;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure LoadKeywordList(Path: string);
    function GetNodesForKeyword(const HelpKeyword: string;
                        var ListOfNodes: THelpNodeQueryList; var {%H-}ErrMsg: string
                        ): TShowHelpResult; override;
    function ShowHelp(Query: THelpQuery; {%H-}BaseNode, NewNode: THelpNode;
                      {%H-}QueryItem: THelpQueryItem;
                      var ErrMsg: string): TShowHelpResult; override;
  end;

procedure RegisterLangRefHelpDatabase;

var
  LangRefHelpDatabase: TLangRefHelpDatabase = nil;

implementation

procedure RegisterLangRefHelpDatabase;
begin
  if not Assigned(LangRefHelpDatabase) then
    LangRefHelpDatabase := TLangRefHelpDatabase(HelpDatabases.CreateHelpDatabase(sFPCLangRef,
                                               TLangRefHelpDatabase, true));
end;

{ TLangRefHelpDatabase }

procedure TLangRefHelpDatabase.ClearKeywordNodes;
var i: Integer;
begin
  for i := 0 to FKeywordNodes.Count - 1 do
    TObject(FKeywordNodes[i]).Free;
  FKeywordNodes.Clear;
end;

constructor TLangRefHelpDatabase.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  FKeywordNodes := TList.Create;
  FKeyWordsList := TStringList.Create;
  FKeyWordsList.CaseSensitive := False;
end;

destructor TLangRefHelpDatabase.Destroy;
begin
  ClearKeywordNodes;
  FKeywordNodes.Free;
  FKeyWordsList.Free;
  inherited Destroy;
end;

procedure TLangRefHelpDatabase.LoadKeywordList(Path: string);
begin
  if Path = '' then
  begin
    Path := '$(LazarusDir)';
    IDEMacros.SubstituteMacros(Path);
    Path := AppendPathDelim(Path) + 'docs' + PathDelim + 'html';
  end;
  Path := AppendPathDelim(Path);

  if FileExistsUTF8(Path + 'ref.kwd') then
  begin
    FKeyWordsList.LoadFromFile(Utf8ToSys(Path + 'ref.kwd'));
  end else FKeyWordsList.Clear;
end;

function TLangRefHelpDatabase.GetNodesForKeyword(const HelpKeyword: string;
  var ListOfNodes: THelpNodeQueryList; var ErrMsg: string): TShowHelpResult;
var
  KeyWord: String;
  i, n: Integer;
  KeywordNode: THelpNode;
begin
  Result := shrHelpNotFound;
  if (csDesigning in ComponentState) then Exit;
  if (FPCKeyWordHelpPrefix<>'')
  and (LeftStr(HelpKeyword,length(FPCKeyWordHelpPrefix))=FPCKeyWordHelpPrefix) then
  begin
    // HelpKeyword starts with KeywordPrefix
    KeyWord := Copy(HelpKeyword, Length(FPCKeyWordHelpPrefix) + 1, Length(HelpKeyword));
    ClearKeywordNodes;
    n := 0;
    for i := 0 to FKeyWordsList.Count - 1 do
      if SameText(FKeyWordsList.Names[i], KeyWord) then
      begin
        Inc(n);
        KeywordNode := THelpNode.CreateURL(Self,KeyWord,'ref.chm://ref/' + FKeyWordsList.ValueFromIndex[i]);
        KeywordNode.Title := Format('Pascal keyword "%s"', [KeyWord]);
        if n > 1 then
          KeywordNode.Title := KeywordNode.Title + ' (' + IntToStr(n) + ')';
        FKeywordNodes.Add(KeywordNode);
        CreateNodeQueryListAndAdd(KeywordNode,nil,ListOfNodes,true);
        Result := shrSuccess;
      end;
    if Result <> shrSuccess then Exit;
    { for => +forin, in => +forin }
    if SameText(KeyWord, 'for') or SameText(KeyWord, 'in') then
    begin
      i := FKeyWordsList.IndexOfName('forin');
      if i < 0 then Exit;
      KeywordNode := THelpNode.CreateURL(Self,KeyWord,'ref.chm://ref/' + FKeyWordsList.ValueFromIndex[i]);
      KeywordNode.Title := 'Pascal keyword "for..in"';
      FKeywordNodes.Add(KeywordNode);
      CreateNodeQueryListAndAdd(KeywordNode, nil, ListOfNodes, True);
    end;
  end;
end;

function TLangRefHelpDatabase.ShowHelp(Query: THelpQuery; BaseNode,
  NewNode: THelpNode; QueryItem: THelpQueryItem; var ErrMsg: string
  ): TShowHelpResult;
var
  Viewer: THelpViewer;
begin
  Result:=shrHelpNotFound;
  if not (Query is THelpQueryKeyword) then exit;
  Result := FindViewer('text/html', ErrMsg, Viewer);
  if Result <> shrSuccess then Exit;
  Result := Viewer.ShowNode(NewNode, ErrMsg);
end;

end.

