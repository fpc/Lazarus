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
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  ComCtrls, StdCtrls, ButtonPanel, ExtCtrls, Process, Spin, XMLRead, DOM,
  Menus, LCLProc;

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

    function GetAction(Index: Integer): TActionItem;
  private
  public
    constructor Create;
    destructor Destroy; override;

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
    procedure mnuShowDiffClick(Sender: TObject);
    procedure RefreshButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure LogListViewSelectItem(Sender: TObject; Item: TListItem;
      Selected: Boolean);
  private
    FRepositoryPath: string;
    { private declarations }
    LogList: TFPList;
    procedure UpdateLogListView;
    procedure ChangeCursor(ACursor: TCursor);
  public
    { public declarations }
    procedure Execute(Data: PtrInt);

    property RepositoryPath: string read FRepositoryPath write FrepositoryPath;
  end;

procedure ShowSVNLogFrm(ARepoPath: string);

implementation

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

  SVNActionsListView.Clear;

  if Assigned(SVNLogItem) then
  begin
    SVNLogMsgMemo.Lines.Text:=SVNLogItem.Msg;

    for i := 0 to SVNLogItem.Count - 1 do
      with SVNActionsListView.Items.Add do
      begin
        Caption := SVNLogItem.Action[i].Action;
        SubItems.Add(SVNLogItem.Action[i].Path);
        SubItems.Add(SVNLogItem.Action[i].CopyPath);
        SubItems.Add(SVNLogItem.Action[i].CopyRev);
      end;
  end
  else
    SVNLogMsgMemo.Clear;
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
  path: string;
  i: integer;
  revision: integer;
begin
  {$note implement opening file in source editor}
  if Assigned(SVNActionsListView.Selected) and Assigned(LogListView.Selected) then
  begin
    debugln('TSVNLogFrm.mnuShowDiffClick Path=' ,SVNActionsListView.Selected.SubItems[0]);
    revision := StrToInt(LogListView.Selected.Caption);
    path := SVNActionsListView.Selected.SubItems[0];
    Delete(path, 1, 1);
    i := pos('/', path);
    ShowSVNDiffFrm(Format('-r %d:%d', [revision - 1, revision]),
                   RepositoryPath + Copy(path, i, length(path) - i + 1));
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
  AProcess: TProcess;
  BytesRead: LongInt;
  Doc: TXMLDocument;
  i: integer;
  LogItem: TSVNLogItem;
  M: TMemoryStream;
  n: LongInt;
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
begin
  debugln('TSVNLogFrm.Execute RepositoryPath=' ,RepositoryPath);

  AProcess := TProcess.Create(nil);
  AProcess.CommandLine := SVNExecutable + ' log --xml --verbose --limit ' + IntToStr(SVNLogLimit.Value) + ' "' + RepositoryPath  + '" --non-interactive';
  debugln('TSVNLogFrm.Execute CommandLine ' + AProcess.CommandLine);
  AProcess.Options := AProcess.Options + [poUsePipes, poStdErrToOutput];
  AProcess.ShowWindow := swoHIDE;
  AProcess.Execute;

  M := TMemoryStream.Create;
  BytesRead := 0;

  while AProcess.Running do
  begin
    // make sure we have room
    M.SetSize(BytesRead + READ_BYTES);

    // try reading it
    n := AProcess.Output.Read((M.Memory + BytesRead)^, READ_BYTES);
    if n > 0
    then begin
      Inc(BytesRead, n);
    end
    else begin
      // no data, wait 100 ms
      Sleep(100);
    end;
  end;

  // read last part
  repeat
    // make sure we have room
    M.SetSize(BytesRead + READ_BYTES);

    // try reading it
    n := AProcess.Output.Read((M.Memory + BytesRead)^, READ_BYTES);
    if n > 0
    then begin
      Inc(BytesRead, n);
    end;
  until n <= 0;
  M.SetSize(BytesRead);

  ReadXMLFile(Doc, M);

  M.Free;
  AProcess.Free;

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
      ActionItem.CopyRev := '';
      ActionItem.CopyPath := '';
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
                  ActionItem.CopyPath := ActionNode.Attributes.Item[i].NodeValue;
          end;

          //paths
          ActionItem.Path:=ActionNode.FirstChild.NodeValue;

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

initialization
  {$I svnlogform.lrs}

end.

