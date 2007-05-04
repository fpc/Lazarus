{  $Id$  }
{
 ***************************************************************************
 *                                                                         *
 *   This source is free software; you can redistribute it and/or modify   *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 *   This code is distributed in the hope that it will be useful, but      *
 *   WITHOUT ANY WARRANTY; without even the implied warranty of            *
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU     *
 *   General Public License for more details.                              *
 *                                                                         *
 *   A copy of the GNU General Public License is available on the World    *
 *   Wide Web at <http://www.gnu.org/copyleft/gpl.html>. You can also      *
 *   obtain it by writing to the Free Software Foundation,                 *
 *   Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.        *
 *                                                                         *
 ***************************************************************************

  Author: Mattias Gaertner

  Name:
       updatepofiles - updates po files.

  Synopsis:
       updatepofiles filename1.po [filename2.po ... filenameN.po]

  Description:
       updatepofiles deletes doubles in the po file and merges new strings into
       all translated po files (filename1.po.xx)

}
program UpdatePoFiles;

{$mode objfpc}{$H+}

uses
  Classes, SysUtils, FileUtil, AvL_Tree;
  
type
  TMsgItem = record
    Comment: string;
    ID: string;
    Str: string;
  end;
  PMsgItem = ^TMsgItem;
  
function CompareMsgItems(Data1, Data2: pointer): integer;
var
  MsgItem1: PMsgItem;
  MsgItem2: PMsgItem;
begin
  MsgItem1:=PMsgItem(Data1);
  MsgItem2:=PMsgItem(Data2);
  Result:=CompareStr(MsgItem1^.ID,MsgItem2^.ID);
end;

procedure DisposeMsgTree(var Tree: TAVLTree);
var
  Node: TAVLTreeNode;
  MsgItem: PMsgItem;
begin
  Node:=Tree.FindLowest;
  while Node<>nil do begin
    MsgItem:=PMsgItem(Node.Data);
    Dispose(MsgItem);
    Node:=Tree.FindSuccessor(Node);
  end;
  Tree.Free;
  Tree:=nil;
end;

type
  TPoFile = class
  public
    Tree: TAVLTree;
    Header: TStringList;
    constructor Create;
    destructor Destroy; override;
  end;

{ TPoFile }

constructor TPoFile.Create;
begin
  Tree:=TAVLTree.Create(@CompareMsgItems);
  Header:=TStringList.Create;
end;

destructor TPoFile.Destroy;
begin
  DisposeMsgTree(Tree);
  Header.Free;
  inherited Destroy;
end;

//==============================================================================
var
  Files: TStringList;
  Prefix: string;

procedure IncPrefix;
begin
  Prefix:=Prefix+'  ';
end;

procedure DecPrefix;
begin
  Prefix:=LeftStr(Prefix,length(Prefix)-2);
end;

function ParamsValid: boolean;
var
  i: Integer;
  Filename: String;
  Ext: String;
  Name: string;
begin
  Result:=false;
  if ParamCount<1 then exit;
  for i:=1 to ParamCount do begin
    Filename:=ParamStr(1);
    if not FileExists(Filename) then begin
      writeln('ERROR: file not found: ',FileName);
      exit;
    end;
    Ext:=ExtractFileExt(Filename);
    if (Ext<>'.po') then begin
      writeln('ERROR: invalid extension: ',Filename);
      exit;
    end;
    Name:=ExtractFileName(Filename);
    Name:=LeftStr(Name,length(Name)-length(Ext));
    if Pos('.',Name)>0 then begin
      writeln('ERROR: invalid unitname: ',Name);
      exit;
    end;
    if Files=nil then Files:=TStringList.Create;
    Files.Add(Filename);
  end;
  Result:=true;
end;

function ReadMessageItem(SrcFile: TStringList; var Line: integer): PMsgItem;
var
  s: string;
begin
  New(Result);
  while Line<SrcFile.Count do begin
    s:=SrcFile[Line];
    if (s<>'') and (s[1]='#') then begin
      Result^.Comment:=Result^.Comment+copy(s,2,length(s));
    end
    else if (LeftStr(s,7)='msgid "') then begin
      // read ID
      Result^.ID:=copy(s,8,length(s)-8);
      inc(Line);
      while Line<SrcFile.Count do begin
        s:=SrcFile[Line];
        if (s<>'') and (s[1]='"') then begin
          Result^.ID:=Result^.ID+copy(s,2,length(s)-2);
          inc(Line);
        end else
          break;
      end;
      // read Str
      if Line<SrcFile.Count then begin
        s:=SrcFile[Line];
        if LeftStr(s,8)='msgstr "' then begin
          Result^.Str:=copy(s,9,length(s)-9);
          inc(Line);
          while Line<SrcFile.Count do begin
            s:=SrcFile[Line];
            if (s<>'') and (s[1]='"') then begin
              Result^.Str:=Result^.Str+copy(s,2,length(s)-2);
              inc(Line);
            end else
              break;
          end;
        end;
      end;
      exit;
    end;
    inc(Line);
  end;
end;

procedure WriteMessageItem(MsgItem: PMsgItem; DestFile: TStringList);

  procedure WriteItem(const Prefix: string; Str: string);
  var
    s: String;
    p: Integer;
    PrefixWritten: Boolean;
  begin
    s:=Prefix+' "';
    PrefixWritten:=false;
    p:=1;
    while (p<=length(Str)) do begin
      if Str[p]='\' then begin
        inc(p,2);
        if (p<=length(Str)+1) and (Str[p-1]='n') then begin
          // a new line \n
          // -> break line
          if not PrefixWritten then begin
            // end last line and add it
            s:=s+'"';
            DestFile.Add(s);
            PrefixWritten:=true;
            s:='"';
          end;
          // add line
          s:=s+copy(Str,1,p-1)+'"';
          DestFile.Add(s);
          Str:=copy(Str,p,length(Str));
          p:=1;
          // start new line
          s:='"';
        end;
      end else
        inc(p);
    end;
    if (Str<>'') or (not PrefixWritten) then begin
      s:=s+Str+'"';
      DestFile.Add(s);
    end;
  end;

begin
  if MsgItem^.Comment<>'' then
    DestFile.Add('#'+MsgItem^.Comment);
  WriteItem('msgid',MsgItem^.ID);
  WriteItem('msgstr',MsgItem^.Str);
  DestFile.Add('');
end;

function ReadPoFile(const Filename: string): TPoFile;
var
  SrcFile: TStringList;
  MsgItem: PMsgItem;
  Line: Integer;
begin
  Result:=TPoFile.Create;

  // read source .po file
  //writeln(Prefix,'Loading ',Filename,' ...');
  SrcFile:=TStringList.Create;
  SrcFile.LoadFromFile(Filename);
  
  Line:=0;
  while Line<SrcFile.Count do begin
    if (SrcFile[Line]='') then begin
      // empty line
      inc(Line);
    end
    else begin
      // message
      MsgItem:=ReadMessageItem(SrcFile,Line);
      // ignore doubles
      if (Result.Tree.FindKey(MsgItem,@CompareMsgItems)<>nil) then begin
        Dispose(MsgItem);
        continue;
      end;
      // add message
      Result.Tree.Add(MsgItem);
    end;
  end;

  SrcFile.Free;
end;

procedure WritePoFile(PoFile: TPoFile; const Filename: string);
var
  DestFile: TStringList;
  Node: TAVLTreeNode;
  MsgItem: PMsgItem;
  Save: Boolean;
  OldDestFile: TStringList;
begin
  //writeln(Prefix,'Saving ',Filename,' ...');
  DestFile:=TStringList.Create;
  if (PoFile.Header.Count>0) then begin
    DestFile.Add('msgid ""');
    DestFile.Add('msgstr ""');
    DestFile.AddStrings(PoFile.Header);
    DestFile.Add('');
  end;
  Node:=PoFile.Tree.FindLowest;
  while Node<>nil do begin
    MsgItem:=PMsgItem(Node.Data);
    WriteMessageItem(MsgItem,DestFile);
    Node:=PoFile.Tree.FindSuccessor(Node);
  end;
  Save:=true;
  if FileExists(Filename) then begin
    OldDestFile:=TStringList.Create;
    OldDestFile.LoadFromFile(Filename);
    if OldDestFile.Text=DestFile.Text then Save:=false;
    OldDestFile.Free;
  end;
  if Save then
    DestFile.SaveToFile(Filename);
  DestFile.Free;
end;

function FindAllTranslatedPoFiles(const Filename: string): TStringList;
var
  Path: String;
  Name: String;
  NameOnly: String;
  Ext: String;
  FileInfo: TSearchRec;
  CurExt: String;
begin
  Result:=TStringList.Create;
  Path:=ExtractFilePath(Filename);
  Name:=ExtractFilename(Filename);
  Ext:=ExtractFileExt(Filename);
  NameOnly:=LeftStr(Name,length(Name)-length(Ext));
  if SysUtils.FindFirst(Path+GetAllFilesMask,faAnyFile,FileInfo)=0 then begin
    repeat
      if (FileInfo.Name='.') or (FileInfo.Name='..') or (FileInfo.Name='')
      or (CompareFilenames(FileInfo.Name,Name)=0) then continue;
      CurExt:=ExtractFileExt(FileInfo.Name);
      if (CompareFilenames(CurExt,'.po')<>0)
      or (CompareFilenames(LeftStr(FileInfo.Name,length(NameOnly)),NameOnly)<>0)
      then
        continue;
      Result.Add(Path+FileInfo.Name);
    until SysUtils.FindNext(FileInfo)<>0;
  end;
  SysUtils.FindClose(FileInfo);
end;

procedure MergePoTrees(SrcTree, DestTree: TAVLTree);
var
  SrcNode, DestNode: TAVLTreeNode;
  SrcMsgItem, DestMsgItem: PMsgItem;
  OldNode: TAVLTreeNode;
begin
  // add all message items from SrcTree into DestTree
  SrcNode:=SrcTree.FindLowest;
  while SrcNode<>nil do begin
    SrcMsgItem:=PMsgItem(SrcNode.Data);
    DestNode:=DestTree.FindKey(SrcMsgItem,@CompareMsgItems);
    if DestNode<>nil then begin
      // ID already exists -> update comment
      DestMsgItem:=PMsgItem(DestNode.Data);
      DestMsgItem^.Comment:=SrcMsgItem^.Comment;
    end else begin
      // new ID -> add new message item to DestTree
      New(DestMsgItem);
      DestMsgItem^.Comment:=SrcMsgItem^.Comment;
      DestMsgItem^.ID:=SrcMsgItem^.ID;
      DestMsgItem^.Str:=SrcMsgItem^.Str;
      DestTree.Add(DestMsgItem);
    end;
    SrcNode:=SrcTree.FindSuccessor(SrcNode);
  end;
  // remove all old messages in DestTree
  DestNode:=DestTree.FindLowest;
  while DestNode<>nil do begin
    DestMsgItem:=PMsgItem(DestNode.Data);
    OldNode:=DestNode;
    DestNode:=DestTree.FindSuccessor(DestNode);
    if (DestMsgItem^.ID<>'')
    and (SrcTree.FindKey(DestMsgItem,@CompareMsgItems)=nil) then begin
      // unused message -> delete it
      writeln('Deleting unused message "',DestMsgItem^.ID,'"');
      Dispose(DestMsgItem);
      DestTree.Delete(OldNode);
    end;
  end;
end;

procedure UpdatePoFile(const Filename: string);
var
  SrcFile, DestFile: TPoFile;
  DestFiles: TStringList;
  i: Integer;
begin
  writeln('Loading ',Filename,' ...');
  SrcFile:=ReadPoFile(Filename);
  DestFiles:=FindAllTranslatedPoFiles(Filename);
  IncPrefix;
  for i:=0 to DestFiles.Count-1 do begin
    writeln(Prefix,'Updating ',DestFiles[i]);
    IncPrefix;
    DestFile:=ReadPoFile(DestFiles[i]);
    MergePoTrees(SrcFile.Tree,DestFile.Tree);
    WritePoFile(DestFile,DestFiles[i]);
    DestFile.Free;
    DecPrefix;
  end;
  DecPrefix;
  DestFiles.Free;
  SrcFile.Free;
end;

procedure UpdateAllPoFiles;
var
  i: Integer;
begin
  for i:=0 to Files.Count-1 do begin
    UpdatePoFile(Files[i]);
  end;
end;

begin
  Prefix:='';
  Files:=nil;
  if not ParamsValid then begin
    writeln('Usage: ',ExtractFileName(ParamStr(0))
       ,' filename1.po [filename2.po ... filenameN.po]');
    exit;
  end else begin
    UpdateAllPoFiles;
  end;
  Files.Free;
end.

