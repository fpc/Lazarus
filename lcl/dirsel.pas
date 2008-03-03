{
 /***************************************************************************
                               DirSel.pas
                               ----------
                            Component Library


 ***************************************************************************/

 *****************************************************************************
 *                                                                           *
 *  This file is part of the Lazarus Component Library (LCL)                 *
 *                                                                           *
 *  See the file COPYING.modifiedLGPL, included in this distribution,        *
 *  for details about the copyright.                                         *
 *                                                                           *
 *  This program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
 *                                                                           *
 *****************************************************************************
}
unit DirSel;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, Buttons,
  StdCtrls, ComCtrls, ExtCtrls;

type

  { TDirSelDlg }

  TDirSelDlg = class(TForm)
    btnOK: TButton;
    btnCancel: TButton;
    lblDirectory: TLabel;
    Panel1: TPanel;
    DirectoryPanel: TPanel;
    TV: TTreeview;
    procedure FormShow(Sender: TObject);
    procedure TVExpanded(Sender: TObject; Node: TTreeNode);
  private
    FRootDir: string;
    FDir: string;
    FShowHidden: Boolean;
    //TheImageList: TImageList;
    procedure AddDirectories(Node: TTreeNode; Dir: string);
    function GetAbsolutePath(Node: TTreeNode): string;
    procedure SetDir(const Value: string);
    procedure SetRootDir(const Value: string);
    procedure SetupCaptions;
  public
    function SelectedDir: string;
    property Directory: string read FDir write SetDir;
    property RootDirectory: string read FRootDir write SetRootDir;
    property ShowHidden: Boolean read FShowHidden write FShowHidden;
  end; 

var
  DirSelDlg: TDirSelDlg;

  
implementation

uses
  FileUtil, LCLStrConsts;


{function HasSubDirs returns True if the directory passed has subdirectories}
function HasSubDirs(const Dir: string; AShowHidden: boolean): Boolean;
var
  FileInfo: TSearchRec;
  FCurrentDir: string;
begin
  //Assume No
  Result := False;
  if Dir <> '' then
  begin
    FCurrentDir := AppendPathDelim(Dir);
    FCurrentDir := FCurrentDir + GetAllFilesMask;
//    debugln('FCurrentDir=' + FCurrentDir);
    try
      if SysUtils.FindFirst(FCurrentDir, faAnyFile, FileInfo)=0 then
      begin
        repeat
          if FileInfo.Name = '' then
            Continue;

          // check if special file
          if ((FileInfo.Name='.') or (FileInfo.Name='..')) or
            // unix dot directories (aka hidden directories)
            ((FileInfo.Name[1] in ['.']) and AShowHidden) or
            // check Hidden attribute
            (((faHidden and FileInfo.Attr)>0) and AShowHidden) then
            Continue;

          Result := ((faDirectory and FileInfo.Attr)>0);

          //We found at least one non special dir, that's all we need.
          if Result then
            break;
        until SysUtils.FindNext(FileInfo)<>0;
      end;//if
    finally
      SysUtils.FindClose(FileInfo);
    end;//Try-Finally
  end;//if
end;//HasSubDirs


{procedure AddDirectories Adds Subdirectories to a passed node if they exist}
procedure TDirSelDlg.AddDirectories(Node: TTreeNode; Dir: string);
var
  FileInfo: TSearchRec;
  NewNode: TTreeNode;
  i: integer;
  FCurrentDir: string;
  //used to sort the directories.
  SortList: TStringList;
begin
  if Dir <> '' then
  begin
    FCurrentDir:= Dir;
    AppendPathDelim(FCurrentDir);
    i:= length(FCurrentDir);
    FCurrentDir:= Dir + GetAllFilesMask;
    try
      if SysUtils.FindFirst(FCurrentDir, faAnyFile,FileInfo)=0 then
      begin
        Try
          SortList:= TStringList.Create;
          SortList.Sorted:= True;
          repeat
            // check if special file
            if (FileInfo.Name='.') or (FileInfo.Name='..') or (FileInfo.Name='') then
              Continue;
            // if hidden files or directories must be filtered, we test for
            // dot files, considered hidden under unix type OS's.
            if not FShowHidden then
              if (FileInfo.Name[1] in ['.']) then
                Continue;

            // if this is a directory then add it to the tree.
            if ((faDirectory and FileInfo.Attr)>0) then
            begin
              //if this is a hidden file and we have not been requested to show
              //hidden files then do not add it to the list.
              if ((faHidden and FileInfo.Attr)>0) and not FShowHidden then
                continue;

              SortList.Add(FileInfo.Name);
            end;//if
          until SysUtils.FindNext(FileInfo)<>0;
          for i:= 0 to SortList.Count - 1 do
          begin
            NewNode:= TV.Items.AddChild(Node,SortList[i]);
            //if subdirectories then indicate so.
            NewNode.HasChildren := HasSubDirs(AppendPathDelim(Dir) + NewNode.Text, FShowHidden);
          end;//for
        finally
          SortList.free;
        end;//Try-Finally
      end;//if
    finally
      SysUtils.FindClose(FileInfo);
    end;//Try-Finally
  end;//if
  if Node.Level = 0 then Node.Text := Dir;
end;//AddDirectories

{procedure SetRootNode Clear the TreeView and Add the root with it's
 subdirectories}
procedure TDirSelDlg.SetRootDir(const Value: string);
var
  RootNode: TTreeNode;
begin
  //Clear the list
  TV.Items.Clear;
  FRootDir:= Value;
  //Remove the path delimiter unless this is root.
  if FRootDir = '' then FRootDir := PathDelim;
  if (FRootDir <> PathDelim) and (FRootDir[length(FRootDir)] = PathDelim) then
    FRootDir:= copy(FRootDir,1,length(FRootDir) - 1);
  //Create the root node and add it to the Tree View.
  RootNode:= TV.Items.Add(nil,FRootDir);
  //Add the Subdirectories to Root.
  AddDirectories(RootNode,FRootDir);
  //Set the root node as the selected node.
  TV.Selected:= RootNode;
end;//SetRootDir

procedure TDirSelDlg.SetupCaptions;
begin
  Caption := rsfdSelectDirectory;
  btnOK.Caption := rsMbOK;
  btnCancel.Caption := rsMbCancel;
  lblDirectory.Caption := rsDirectory;
end;

{Returns the absolute path to a node.}
function TDirSelDlg.GetAbsolutePath(Node: TTreeNode): string;
begin
  Result:= '';
  While Node<>nil do
  begin
    if Node.Text = PathDelim then
      Result:= Node.Text + Result
    else
      Result:= Node.Text + PathDelim + Result;
    Node:= Node.Parent;
  end;//while
end;//GetAbsolutePath

procedure TDirSelDlg.FormShow(Sender: TObject);
begin
  SetupCaptions;
  if TV.Selected <> nil then
    TV.Selected.Expand(false);
end;//FormShow

procedure TDirSelDlg.TVExpanded(Sender: TObject; Node: TTreeNode);
begin
  if Node.Count = 0 then
    AddDirectories(Node, GetAbsolutePath(Node));
end;//TVExpanded

procedure TDirSelDlg.SetDir(const Value: string);
var
  StartDir: string;
  Node: TTreeNode;
  i,p: integer;
  SubDir: PChar;
begin

  FDir:= Value;
  StartDir:= Value;
  if TV.Items.Count = 0 then exit;
  p:= AnsiPos(FRootDir, StartDir);
  if p = 1 then
    Delete(StartDir,P,Length(FRootDir));
  for i:= 1 to Length(StartDir) do
    if (StartDir[i] = PathDelim) then StartDir[i] := #0;
  SubDir:= PChar(StartDir);
  if SubDir[0] = #0 then
    SubDir:= @SubDir[1];
  Node:= TV.Items.GetFirstNode;
  While SubDir[0] <> #0 do
  begin
    Node:= Node.GetFirstChild;
    while (Node <> nil) and (AnsiCompareStr(Node.Text, SubDir) <> 0) do
      Node:= Node.GetNextSibling;
    if Node = nil then break
    else
    begin
      Node.Expand(False);
    end;//else
    SubDir:= SubDir + StrLen(SubDir) + 1
  end;//While
  TV.Selected.MakeVisible;
end;//SetDir

function TDirSelDlg.SelectedDir: string;
begin
  Result:= '';
  if TV.Selected <> nil then
    Result:= GetAbsolutePath(TV.Selected);
end;//SelectedDir

initialization
  {$I dirsel.lrs}

end.
