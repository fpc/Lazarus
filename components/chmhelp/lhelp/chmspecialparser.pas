{ Copyright (C) <2005> <Andrew Haines> chmspecialparser.pas

  This library is free software; you can redistribute it and/or modify it
  under the terms of the GNU Library General Public License as published by
  the Free Software Foundation; either version 2 of the License, or (at your
  option) any later version.

  This program is distributed in the hope that it will be useful, but WITHOUT
  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
  FITNESS FOR A PARTICULAR PURPOSE. See the GNU Library General Public License
  for more details.

  You should have received a copy of the GNU Library General Public License
  along with this library; if not, write to the Free Software Foundation,
  Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
}
{
  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the copyright.
}
unit ChmSpecialParser;

{$mode objfpc}{$H+}

interface

uses
  Classes, Forms, SysUtils, Controls, ComCtrls, chmsitemap;
  
type

  TContentTreeNode = class(TTreeNode)
  private
    fUrl: String;
  public
    property Url:String read fUrl write fUrl;
  end;
  
  TContentNode = record
    Name: String;
    Url: String;
    LineCount: Integer;
  end;
  
  TIndexItem = class(TListITem)
  private
    fUrl: String;
  public
    property Url:String read fUrl write fUrl;
  end;
  
  
  { TContentsFiller }

  TContentsFiller = class(TObject)
  private
    fTreeView: TTreeView;
    fSitemap: TChmSiteMap;
    fChm: TObject;

    fBranchCount: DWord;
    fStop: PBoolean;
    procedure CustomCreateContentTreeItem(Sender: TCustomTreeView; var ATreeNode: TTreenode);
    procedure AddItem(AItem: TChmSiteMapItem; AParentNode: TTreeNode);
  public
    constructor Create(ATreeView: TTreeView; AStream: TStream; StopBoolean: PBoolean; AChm: TObject);
    procedure DoFill(ParentNode: TTreeNode);
    

  end;
  
  { TIndexFiller }

  TIndexFiller = class(TObject)
  private
    fListView: TListView;
    fStream: TStream;
    fChm: Tobject;
    fText: TStringList;
    function GetLIEnd(StartLine: Integer): Integer;
    function GetNextLI(StartLine: Integer): Integer;
    function AddLIObjects(StartLine: Integer; SubItem: Boolean): Integer;
    function LineHasLI(ALine: Integer): Boolean;
    function LineStartsUL(ALine: Integer): Boolean;
    function LineEndsUL(ALine: Integer): Boolean;
    
  public
    constructor Create(AListView: TListView; AStream: TStream; AChm: TObject);
    procedure DoFill;

  end;

implementation

function FixURL(URL: String):String;
var
  X: LongInt;
begin
  X := Pos('%20', Url);
  while X > 0 do begin
    Delete(Url, X, 3);
    Insert(' ', Url, X);
    X := Pos('%20', Url);
  end;
  Result := StringReplace(Url, '\', '/', [rfReplaceAll]);
end;

{ TContentsFiller }

procedure TContentsFiller.CustomCreateContentTreeItem(Sender: TCustomTreeView;
  var ATreeNode: TTreenode);
begin
  ATreeNode := TContentTreeNode.Create(TTreeView(Sender).Items);
end;

procedure TContentsFiller.AddItem(AItem: TChmSiteMapItem; AParentNode: TTreeNode);
var
  NewNode: TContentTreeNode;
  X: Integer;
begin
  if fStop^ then Exit;
  NewNode := TContentTreeNode(fTreeView.Items.AddChild(AParentNode, AItem.Text));
  NewNode.Url:='/'+AItem.Local;
  NewNode.Data:=fChm;
  NewNode.ImageIndex := 3;
  NewNode.SelectedIndex := 3;
  if (AParentNode.ImageIndex < 0) or (AParentNode.ImageIndex > 2) then
  begin
    AParentNode.ImageIndex := 1;
    AParentNode.SelectedIndex := 1;
  end;
  Inc(fBranchCount);

  if fBranchCount mod 400 = 0 then
    Application.ProcessMessages;

  for X := 0 to AItem.Children.Count-1 do
    AddItem(AItem.Children.Item[X], NewNode);
end;

constructor TContentsFiller.Create(ATreeView: TTreeView; AStream: TStream; StopBoolean: PBoolean; AChm: TObject);
begin
  inherited Create;
  fTreeView := ATreeView;
  fSitemap := TChmSiteMap.Create(stTOC);
  fSitemap.LoadFromStream(AStream);
  fStop := StopBoolean;
  fChm := AChm;
end;

procedure TContentsFiller.DoFill(ParentNode: TTreeNode);
var
 OrigEvent: TTVCustomCreateNodeEvent;
 X: Integer;
begin
  OrigEvent := fTreeView.OnCustomCreateItem;
  fTreeView.OnCustomCreateItem := @CustomCreateContentTreeItem;
  

  fTreeView.BeginUpdate;

  for X := 0 to fSitemap.Items.Count-1 do
    AddItem(fSitemap.Items.Item[X], ParentNode);





  fTreeView.OnCustomCreateItem := OrigEvent;

  fTreeView.EndUpdate;
end;

{ TIndexFiller }

function TIndexFiller.GetLIEnd(StartLine: Integer): Integer;
begin
//  for X := StartLine to
  Result := -1;
end;

function TIndexFiller.GetNextLI(StartLine: Integer): Integer;
begin
  Result := -1;
end;

function TIndexFiller.AddLIObjects(StartLine: Integer; SubItem: Boolean): Integer;
var
  NeedsUrl: Boolean = True;
  NeedsName: Boolean = True;
  ItemNAme: String;
  ItemUrl: String;
  Line: String;
  fPos: Integer;
  fLength: Integer;
  Item: TIndexItem;
  X: LongInt;
begin
  for X:= StartLine to fText.Count-1 do begin
    Line := fText.Strings[X];
    if NeedsName then begin
      fPos := Pos('<param name="name" value="', LowerCase(Line));
      if fPos > 0 then begin
        fLength := Length('<param name="name" value="');
        ItemName := Copy(Line, fPos+fLength, Length(Line)-(fLength+fPos));
        ItemName := Copy(ItemNAme, 1, Pos('"', ItemName)-1);
        if SubItem then
          ItemName := '  ' + ItemName;
        NeedsName := False;
        NeedsUrl := True;
      end;
    end
    else if NeedsUrl then begin
      fPos := Pos('<param name="local" value="', LowerCase(Line));
      if fPos > 0 then begin
        fLength := Length('<param name="Local" value="');
        ItemUrl := Copy(Line, fPos+fLength, Length(Line)-(fLength+fPos));
        ItemUrl := FixUrl('/'+Copy(ItemUrl, 1, Pos('"', ItemUrl)-1));
        NeedsName := False;
        NeedsUrl := False;
        Item := TIndexItem.Create(fListView.Items);
        fListView.Items.AddItem(Item);
        Item.Caption := ItemName;
        Item.Url := ItemUrl;
        Item.Data := fChm;
        ItemName := '';
        ItemUrl := '';
      end;
    end;
    if Pos('</OBJECT>', UpperCase(Line)) > 0 then begin
      Result := X-StartLine;
      Break;
    end;
  end;

end;

function TIndexFiller.LineHasLI(ALine: Integer): Boolean;
begin
  Result := Pos('<LI>', UpperCase(fText.Strings[ALine])) > 0;
end;

function TIndexFiller.LineStartsUL(ALine: Integer): Boolean;
begin
  Result := Pos('<UL>', UpperCase(fText.Strings[ALine])) > 0;
end;

function TIndexFiller.LineEndsUL(ALine: Integer): Boolean;
begin
  Result := Pos('</UL>', UpperCase(fText.Strings[ALine])) > 0;
end;

constructor TIndexFiller.Create(AListView: TListView; AStream: TStream; AChm: TObject);
begin
 inherited Create;
 fListView := AListView;
 fStream := AStream;
 fChm := AChm;
end;

procedure TIndexFiller.DoFill;
var
  X: Integer;
  IsSubItem: Boolean;
  HasInitialUL: Boolean;
begin
  fStream.Position := 0;
  fText := TStringList.Create;
  fText.LoadFromStream(fStream);
  fListView.BeginUpdate;
  fListView.Items.Clear;
  X := -1;
  HasInitialUL := False;
  IsSubItem    := False;
  while X < fText.Count-1 do begin
    Inc(X);
    if LineStartsUL(X) then begin
      IsSubItem := HasInitialUL and True;
      HasInitialUL := True;
    end;
    if LineEndsUL(X) then
      IsSubItem := False;
    if LineHasLI(X) then Inc(X, AddLIObjects(X, IsSubItem));
  end;
  fText.Free;
  fListView.EndUpdate;
end;

end.

