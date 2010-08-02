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
    procedure AddItem(AItem: TChmSiteMapItem; AParentNode: TTreeNode);
  public
    constructor Create(ATreeView: TTreeView; ASitemap: TChmSiteMap; StopBoolean: PBoolean; AChm: TObject);
    procedure DoFill(ParentNode: TTreeNode);
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

procedure TContentsFiller.AddItem(AItem: TChmSiteMapItem; AParentNode: TTreeNode);
var
  NewNode: TContentTreeNode;
  X: Integer;
begin
  if fStop^ then Exit;
  NewNode := TContentTreeNode(fTreeView.Items.AddChild(AParentNode, AItem.Text));
  NewNode.Url:=FixURL('/'+AItem.Local);
  NewNode.Data:=fChm;
  if fTreeView.Images <> nil then
  begin
    NewNode.ImageIndex := 3;
    NewNode.SelectedIndex := 3;

    if (AParentNode.ImageIndex < 0) or (AParentNode.ImageIndex > 2) then
    begin
      AParentNode.ImageIndex := 1;
      AParentNode.SelectedIndex := 1;
    end;
  end;
  Inc(fBranchCount);

  if fBranchCount mod 200 = 0 then
    Application.ProcessMessages;

  for X := 0 to AItem.Children.Count-1 do
    AddItem(AItem.Children.Item[X], NewNode);
end;

constructor TContentsFiller.Create(ATreeView: TTreeView; ASitemap: TChmSiteMap; StopBoolean: PBoolean; AChm: TObject);
begin
  inherited Create;
  fTreeView := ATreeView;
  fSitemap := ASitemap;
  fStop := StopBoolean;
  fChm := AChm;
end;

procedure TContentsFiller.DoFill(ParentNode: TTreeNode);
var
 OrigEvent: TTVCustomCreateNodeEvent;
 X: Integer;
begin
  fTreeView.BeginUpdate;

  for X := 0 to fSitemap.Items.Count-1 do
    AddItem(fSitemap.Items.Item[X], ParentNode);

  fTreeView.EndUpdate;
end;


end.

