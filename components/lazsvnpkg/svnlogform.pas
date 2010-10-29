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

unit SVNLogForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Dialogs, Controls, FileUtil,
  ComCtrls, StdCtrls, ButtonPanel, ExtCtrls, Process, Spin, XMLRead, DOM,
  Menus, LCLProc, LazIDEIntf;

type
  TActionItem = record
    Action: string;
    Path: string;
    CopyPath: string;
    CopyRev: string;
  end;

  { TSVNLogItem }

  TSVNLogItem = class(TObject)
    FAuthor: string;
    FCount: integer;
    FDate: TDateTime;
    FMsg: string;
    FRevision: integer;
    FAction: array of TActionItem;
  private
    function GetAction(Index: Integer): TActionItem;
  public
    constructor Create;
    destructor Destroy; override;
    function GetActionPointer(Index: Integer): Pointer;
    procedure AddAction(AActionItem: TActionItem);
    property Action[Index: Integer]: TActionItem read GetAction;
    property Count: integer read FCount write FCount;
    property Revision: integer read FRevision write FRevision;
    property Author: string read FAuthor write FAuthor;
    property Date: TDateTime read FDate write FDate;
    property Msg: string read FMsg write FMsg;
  end;

  { TSVNLogFrm }

  TSVNLogFrm = class(TForm)
    ImageList: TImageList;
    mnuOpenCurent: TMenuItem;
    mnuOpenPrevRevision: TMenuItem;
    mnuOpenRevision: TMenuItem;
    mnuShowDiff: TMenuItem;
    SVNActionsPopupMenu: TPopupMenu;
    RefreshButton: TButton;
    ButtonPanel: TButtonPanel;
    Label1: TLabel;
    LogListView: TListView;
    SVNActionsListView: TListView;
    SVNLogMsgMemo: TMemo;
    SVNLogLimit: TSpinEdit;
    Splitter1: TSplitter;
    Splitter2: TSplitter;
    procedure mnuOpenCurentClick(Sender: TObject);
    procedure mnuOpenPrevRevisionClick(Sender: TObject);
    procedure mnuOpenRevisionClick(Sender: TObject);
    procedure mnuShowDiffClick(Sender: TObject);
    procedure RefreshButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure LogListViewSelectItem(Sender: TObject; Item: TListItem;
      Selected: Boolean);
    procedure SVNActionsPopupMenuPopup(Sender: TObject);
  private
    FRepositoryPath: string;
    { private declarations }
    LogList: TFPList;
    procedure UpdateLogListView;
    procedure ChangeCursor(ACursor: TCursor);
  public
    { public declarations }
    procedure Execute(Data: PtrInt);

    property RepositoryPath: string read FRepositoryPath write FRepositoryPath;
  end;

procedure ShowSVNLogFrm(ARepoPath: string);

implementation

{$R *.lfm}

uses
  SVNDiffForm, SVNClasses;

procedure ShowSVNLogFrm(ARepoPath: string);
var
  SVNLogFrm: TSVNLogFrm;
begin
  SVNLogFrm := TSVNLogFrm.Create(nil);

  SVNLogFrm.RepositoryPath:=ARepoPath;
  SVNLogFrm.ShowModal;

  SVNLogFrm.Free;
end;

{ TSVNLogItem }

function TSVNLogItem.GetAction(Index: Integer): TActionItem;
begin
  if (Index < 0) or (Index >= Count) then
    raise Exception.CreateFmt(rsIndexOutOfBoundsD, [Index]);

  Result := FAction[Index];
end;

function  TSVNLogItem.GetActionPointer(Index: Integer): Pointer;
begin
  if (Index < 0) or (Index >= Count) then
    raise Exception.CreateFmt(rsIndexOutOfBoundsD, [Index]);
    Result := @FAction[Index];
end;

constructor TSVNLogItem.Create;
begin
  //initialize author to unknown, beacuse in anonymous repositories this really happens
  Author := rsNoAuthor;
end;

destructor TSVNLogItem.Destroy;
begin
  inherited Destroy;
end;

procedure TSVNLogItem.AddAction(AActionItem: TActionItem);
begin
  Inc(FCount);
  SetLength(FAction, Count);

  FAction[Count - 1] := AActionItem;
end;

function FindSVNLogItemByRevision(List: TFPList; RevNo: integer): TSVNLogItem;
  function SearchLinear(List: TFPList; RevNo: integer): TSVNLogItem;
  var
    i: integer;
  begin
    Result := nil;
    for i := 0 to List.Count - 1 do
      if TSVNLogItem(List.Items[i]).Revision = RevNo then
      begin
        Result := TSVNLogItem(List.Items[i]);
        exit;
      end;
  end;
var
  tmpRev: integer;
  index: integer;
begin
  Result := nil;

  tmpRev := TSVNLogItem(List.Items[0]).Revision;

  //calculate most probable index
  index := tmpRev - RevNo;

  if (index < 0) or (index >= List.Count) then
    //invalid index, so just do a linear search
    Result := SearchLinear(List, RevNo)
  else
  begin
    if TSVNLogItem(List.Items[index]).Revision = RevNo then
      //found!
      Result := TSVNLogItem(List.Items[index])
    else
      //revision not found on expected location, search linear
      Result := SearchLinear(List, RevNo);
  end;
end;

{ TSVNLogFrm }

procedure TSVNLogFrm.FormShow(Sender: TObject);
begin
  ChangeCursor(crHourGlass);
  Caption := Format(rsLazarusSVNLog, [RepositoryPath]);
  Application.QueueAsyncCall(@Execute, 0);
end;

procedure TSVNLogFrm.LogListViewSelectItem(Sender: TObject; Item: TListItem;
  Selected: Boolean);
var
  RevNo: integer;
  SVNLogItem: TSVNLogItem;
  i: integer;
begin
  RevNo := StrToInt(Item.Caption);

  SVNLogItem := FindSVNLogItemByRevision(LogList, RevNo);

  SVNActionsListView.Visible := False; // BeginUpdate won't help when autosize is true
  SVNActionsListView.Clear;

  if Assigned(SVNLogItem) then
  begin
    SVNLogMsgMemo.Lines.Text:=SVNLogItem.Msg;

    for i := 0 to SVNLogItem.Count - 1 do
      with SVNActionsListView.Items.Add do
      begin
        Caption := SVNLogItem.Action[i].Action;
        SubItems.Add(CreateRelativePath(SVNLogItem.Action[i].Path, RepositoryPath));
        SubItems.Add(CreateRelativePath(SVNLogItem.Action[i].CopyPath, RepositoryPath));
        SubItems.Add(SVNLogItem.Action[i].CopyRev);
        Data := SVNLogItem.GetActionPointer(i);
      end;
  end
  else
    SVNLogMsgMemo.Clear;
  SVNActionsListView.Visible := True;
end;

procedure TSVNLogFrm.SVNActionsPopupMenuPopup(Sender: TObject);
var
  P: TPoint;
  LI: TListItem;
begin
  // make sure the row under the mouse is selected
  P :=  SVNActionsListView.ScreenToControl(Mouse.CursorPos);
  LI := SVNActionsListView.GetItemAt(P.X, P.Y);
  if LI <> nil then
    SVNActionsListView.Selected := LI;
end;

procedure TSVNLogFrm.UpdateLogListView;
var
  i: integer;
  LogItem : TSVNLogItem;
begin
  LogListView.Clear;

  for i := 0 to LogList.Count - 1 do
    with LogListView.Items.Add do
    begin
      LogItem := TSVNLogItem(LogList.Items[i]);

      //revision
      Caption := IntToStr(LogItem.Revision);

      //author
      SubItems.Add(LogItem.Author);

      //date
      SubItems.Add(DateTimeToStr(LogItem.Date));

      //message
      SubItems.Add(LogItem.Msg);
    end;
end;

procedure TSVNLogFrm.ChangeCursor(ACursor: TCursor);
begin
  LogListView.Cursor:=ACursor;
  SVNLogMsgMemo.Cursor:=ACursor;
  SVNActionsListView.Cursor:=ACursor;
  Application.ProcessMessages;
end;

procedure TSVNLogFrm.RefreshButtonClick(Sender: TObject);
begin
  ChangeCursor(crHourGlass);
  Execute(0);
end;

procedure TSVNLogFrm.mnuShowDiffClick(Sender: TObject);
var
  Path: string;
  i: integer;
  Revision: integer;
begin
  if Assigned(SVNActionsListView.Selected) and Assigned(LogListView.Selected) then
  begin
    Revision := StrToInt(LogListView.Selected.Caption);
    Path := TActionItem(SVNActionsListView.Selected.Data^).Path;
    DebugLn('TSVNLogFrm.mnuShowDiffClick Path=' , Path);
    if TActionItem(SVNActionsListView.Selected.Data^).Action = 'M' then
      ShowSVNDiffFrm(Format('-r %d:%d', [Revision - 1, Revision]), Path)
    else
      ShowMessage(rsOnlyModifiedItemsCanBeDiffed);
  end;
end;

procedure TSVNLogFrm.mnuOpenRevisionClick(Sender: TObject);
begin

end;

procedure TSVNLogFrm.mnuOpenPrevRevisionClick(Sender: TObject);
begin

end;

procedure TSVNLogFrm.mnuOpenCurentClick(Sender: TObject);
var
  Path: String;
begin
  if Assigned(SVNActionsListView.Selected) and Assigned(LogListView.Selected) then
  begin
    Path := TActionItem(SVNActionsListView.Selected.Data^).Path;
    if FileExists(Path) then
      LazarusIDE.DoOpenEditorFile(Path, -1, -1, [ofOnlyIfExists])
    else
      ShowMessage(rsFileNotInWorkingCopyAnymore);
  end;
end;

procedure TSVNLogFrm.FormCreate(Sender: TObject);
begin
  LogList := TFPList.Create;

  SetColumn(LogListView, 0, 75, rsRevision);
  SetColumn(LogListView, 1, 75, rsAuthor);
  SetColumn(LogListView, 2, 150, rsDate);
  SetColumn(LogListView, 3, 200, rsMessage);

  SetColumn(SVNActionsListView, 0, 50, rsAction);
  SetColumn(SVNActionsListView, 1, 200, rsPath);
  SetColumn(SVNActionsListView, 2, 150, rsCopyFromPath);
  SetColumn(SVNActionsListView, 3, 75, rsRevision);

  ImageList.AddLazarusResource('menu_svn_diff');

  mnuShowDiff.Caption := rsShowDiff;
  mnuOpenCurent.Caption := rsOpenFileInEditor;
  mnuOpenRevision.Caption := rsOpenThisRevisionInEditor;
  mnuOpenPrevRevision.Caption := rsOpenPreviousRevisionInEditor;

  Label1.Caption:=rsShowDiffCountRev;
end;

procedure TSVNLogFrm.FormDestroy(Sender: TObject);
var
  i: Integer;
begin
  for i := 0 to LogList.Count - 1 do
    TSVNLogItem(LogList[i]).Free;
  LogList.Free;
end;

procedure TSVNLogFrm.Execute(Data: PtrInt);
var
  ActionItem: TActionItem;
  ActionNode: TDOMNode;
  Doc: TXMLDocument;
  InfoUrl: String;
  InfoRoot: String;
  i: integer;
  LogItem: TSVNLogItem;
  Node: TDOMNode;
  NodeName: string;
  SubNode: TDOMNode;
  t: string;
  tmpNode: TDOMNode;

  procedure AddItem(Node: TDomNode);
  begin
    with LogListView.Items.Add do
    begin
      Caption := Node.NodeName;
    end;
  end;

  function AbsPath(APath: String): String;
  var
    Prefix: String;
    PrefixLength: Integer;
    APathBak: String;
  begin
    // svn will always output a path that is relative
    // to the repository root on the server
    // it starts with '/trunk/' or '/branches/foo/'
    // we have already done 'svn info' and this gave
    // us 'root' and 'url' so we can now use this to
    // cut off the prefix and then make
    // it into an absolute path on our harddrive

    PrefixLength := Length(InfoUrl) - Length(InfoRoot);
    Prefix := InfoUrl;
    Delete(Prefix, 1, Length(InfoRoot));

    if Pos(Prefix, APath) = 1 then begin
      APathBak := APath;
      // first make the path relative to our working copy
      // by cutting of the prefix (that only exists on the server)
      Delete(APath, 1, PrefixLength + 1);

      // now make it an absolute path with our local repository
      // base path on our harddrive
      Result := CreateAbsolutePath(APath, RepositoryPath);

      // never ever return an absolute local path for a
      // file that does not exist on our harddrive
      if not FileExists(Result) then
        Result := InfoRoot + APathBak;
    end else begin
      // if it does not have our prefix then it is from
      // another directory or branch on the server.
      Result := InfoRoot + APath;
    end;
  end;

begin
  // first get 'svn info' because we need the paths 'root' and 'url'
  // for some path manipulation to gnerate the absolute paths
  // of the files on our hard drive
  Doc := ExecuteSvnReturnXml('info --xml "' + RepositoryPath + '"');
  try
    Node := Doc.DocumentElement.FirstChild.FindNode('url');
    InfoUrl := Node.TextContent;
    Node := Doc.DocumentElement.FirstChild.FindNode('repository').FindNode('root');
    InfoRoot := Node.TextContent;
  except
    Doc.Free;
    UpdateLogListView;
    ChangeCursor(crDefault);
    exit();
  end;
  Doc.Free;

  Doc := ExecuteSvnReturnXml('log --xml --verbose --limit ' + IntToStr(SVNLogLimit.Value) + ' "' + RepositoryPath  + '" --non-interactive');
  LogList.Clear;
  Node := Doc.DocumentElement.FirstChild;
  if Assigned(Node) then
  begin
    repeat
      SubNode := Node;

      LogItem := TSVNLogItem.Create;

      //revision
      LogItem.Revision := StrToInt(SubNode.Attributes.Item[0].NodeValue);

      //action
      tmpNode := SubNode.FirstChild;
      while Assigned(tmpNode) do
      begin
        NodeName := tmpNode.NodeName;

        //Author
        if NodeName = 'author' then
          LogItem.Author := tmpNode.FirstChild.NodeValue;

        //Date
        if NodeName = 'date' then
          LogItem.Date := ISO8601ToDateTime(tmpNode.FirstChild.NodeValue);

        //message
        if NodeName = 'msg' then
          if Assigned(tmpNode.FirstChild) then
            LogItem.Msg:=ReplaceLineEndings(tmpNode.FirstChild.NodeValue, LineEnding);

        ActionNode := tmpNode.FirstChild;
        if Assigned(ActionNode) and Assigned(ActionNode.Attributes) then
        repeat
          ActionItem.CopyRev := '';
          ActionItem.CopyPath := '';

          //attributes
          for i := 0 to ActionNode.Attributes.Length-1 do
          begin
            t := ActionNode.Attributes.Item[i].NodeName;

            if t = 'action' then
              ActionItem.Action := ActionNode.Attributes.Item[i].NodeValue
            else
              if t = 'copyfrom-rev' then
                ActionItem.CopyRev := ActionNode.Attributes.Item[i].NodeValue
              else
                if t = 'copyfrom-path' then
                  ActionItem.CopyPath := AbsPath(
                    ActionNode.Attributes.Item[i].NodeValue);
          end;

          //paths
          ActionItem.Path:=AbsPath(ActionNode.FirstChild.NodeValue);

          LogItem.AddAction(ActionItem);
          ActionNode := ActionNode.NextSibling;
        until not Assigned(ActionNode);
        tmpNode := tmpNode.NextSibling;

      end;

      LogList.Add(LogItem);

      Node := Node.NextSibling;
    until not Assigned(Node);
  end;
  Doc.Free;
  UpdateLogListView;
  ChangeCursor(crDefault);
end;

end.

