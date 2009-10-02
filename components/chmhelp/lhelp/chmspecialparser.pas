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
    destructor Destroy; override;
    procedure DoFill(ParentNode: TTreeNode);
    

  end;
  
  { TIndexFiller }

  TIndexFiller = class(TObject)
  private
    fListView: TListView;
    fSitemap: TChmSiteMap;
    fChm: Tobject;
    procedure AddItem(Item: TChmSiteMapItem; ASubItem: Boolean);
    
  public
    constructor Create(AListView: TListView; AStream: TStream; AChm: TObject);
    destructor Destroy; override;
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
  NewNode.Url:=FixURL('/'+AItem.Local);
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

destructor TContentsFiller.Destroy;
begin
  fSitemap.Free;
  inherited Destroy;
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

procedure TIndexFiller.AddItem(Item: TChmSiteMapItem; ASubItem: Boolean);
var
  NewItem: TIndexItem;
  X: Integer;
begin
  NewItem := TIndexItem.Create(fListView.Items);
  if ASubItem then
    NewItem.Caption := '  ' + Item.Text
  else
    NewItem.Caption := Item.Text;
  NewItem.Url       := FixURL('/' + Item.Local);
  NewItem.Data      := fChm;
  fListView.Items.AddItem(NewItem);
  if Item.Children.Count > 0 then
    for X := 0 to Item.Children.Count-1 do
      AddItem(Item.Children.Item[X], True);

end;

constructor TIndexFiller.Create(AListView: TListView; AStream: TStream; AChm: TObject);
begin
 inherited Create;
 fListView := AListView;
 fSitemap := TChmSiteMap.Create(stIndex);
 fSitemap.LoadFromStream(AStream);
 fChm := AChm;
end;

destructor TIndexFiller.Destroy;
begin
  fSitemap.Free;
  inherited Destroy;
end;

procedure TIndexFiller.DoFill;
var
  X: Integer;
begin
  fListView.BeginUpdate;
  fListView.Items.Clear;

  for X := 0 to fSitemap.Items.Count-1 do
    AddItem(fSitemap.Items.Item[X], False);

  fListView.EndUpdate;
end;

end.

