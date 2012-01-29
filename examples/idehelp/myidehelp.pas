unit myidehelp;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, LazHelpHTML,
  LazHelpIntf, HelpIntfs, IDEHelpIntf, IDEDialogs;

type

  { TMyFPCKeywordHelpDatabase
    Help for FPC keywords. }

  TMyFPCKeywordHelpDatabase = class(THTMLHelpDatabase)
  private
    FKeywordPrefixNode: THelpNode;
  public
    KeywordToText: TStrings; // every line has the format: Keyword=Text
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    function GetNodesForKeyword(const HelpKeyword: string;
                        var ListOfNodes: THelpNodeQueryList; var {%H-}ErrMsg: string
                        ): TShowHelpResult; override;
    function ShowHelp(Query: THelpQuery; {%H-}BaseNode, {%H-}NewNode: THelpNode;
                      {%H-}QueryItem: THelpQueryItem;
                      var {%H-}ErrMsg: string): TShowHelpResult; override;
  end;


  TMyHelpSetupDialog = class(TForm)
  private
  public
  end;

var
  MyHelpSetupDialog: TMyHelpSetupDialog;

procedure Register;

implementation

procedure Register;
begin
  HelpDatabases.CreateHelpDatabase('MyFPCKeyWordHelpDB',TMyFPCKeywordHelpDatabase,true);
end;

{ TMyFPCKeywordHelpDatabase }

constructor TMyFPCKeywordHelpDatabase.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  KeywordToText:=TStringList.Create;
  KeywordToText.Add('procedure=Named code block');
end;

destructor TMyFPCKeywordHelpDatabase.Destroy;
begin
  FreeAndNil(KeywordToText);
  inherited Destroy;
end;

function TMyFPCKeywordHelpDatabase.GetNodesForKeyword(
  const HelpKeyword: string; var ListOfNodes: THelpNodeQueryList;
  var ErrMsg: string): TShowHelpResult;
var
  KeyWord: String;
  i: Integer;
begin
  Result:=shrHelpNotFound;
  if (csDesigning in ComponentState) then exit;
  if (FPCKeyWordHelpPrefix<>'')
  and (LeftStr(HelpKeyword,length(FPCKeyWordHelpPrefix))=FPCKeyWordHelpPrefix) then begin
    // HelpKeyword starts with KeywordPrefix
    KeyWord:=copy(HelpKeyword,length(FPCKeyWordHelpPrefix)+1,length(HelpKeyword));
    i:=KeywordToText.IndexOfName(lowercase(KeyWord));
    if i>=0 then begin
      // this help database knows this keyword
      // => add a node, so that if there are several possibilities the IDE can
      //    show the user a dialog to choose
      if FKeywordPrefixNode=nil then
        FKeywordPrefixNode:=THelpNode.CreateURL(Self,'','');
      FKeywordPrefixNode.Title:='Pascal keyword '+KeyWord;
      CreateNodeQueryListAndAdd(FKeywordPrefixNode,nil,ListOfNodes,true);
      Result:=shrSuccess;
    end;
  end;
end;

function TMyFPCKeywordHelpDatabase.ShowHelp(Query: THelpQuery; BaseNode,
  NewNode: THelpNode; QueryItem: THelpQueryItem; var ErrMsg: string
  ): TShowHelpResult;
var
  KeywordQuery: THelpQueryKeyword;
  KeyWord: String;
  Txt: String;
begin
  Result:=shrHelpNotFound;
  if not (Query is THelpQueryKeyword) then exit;
  KeywordQuery:=THelpQueryKeyword(Query);
  KeyWord:=copy(KeywordQuery.Keyword,length(FPCKeyWordHelpPrefix)+1,length(KeywordQuery.Keyword));
  Txt:=KeywordToText.Values[lowercase(KeyWord)];
  IDEMessageDialog('My fpc keyword key',
    'The keyword "'+KeyWord+'":'#13#13
    +Txt,mtInformation,[mbOk]);
  Result:=shrSuccess;
end;

{$R *.lfm}

end.

