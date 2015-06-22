{
 *****************************************************************************
  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************

  Copyright (C) <2005> <Andrew Haines> chmspecialparser.pas
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
    fLastNode: TTreeNode;
    procedure AddItem(AItem: TChmSiteMapItem; AParentNode: TTreeNode);
  public
    constructor Create(ATreeView: TTreeView; ASitemap: TChmSiteMap; StopBoolean: PBoolean; AChm: TObject);
    procedure DoFill(ParentNode: TTreeNode);
  end;
  
implementation

uses
  LConvEncoding, LazUTF8, HTMLDefs;

function ToUTF8(AText: ansistring): String;
var
  encoding: String;
begin
  encoding := GuessEncoding(AText);
  if (encoding <> EncodingUTF8) then
    Result := ConvertEncoding(AText, encoding, EncodingUTF8)
  else
    Result := AText;
end;

function FixEscapedHTML(AText: string): string;
var
  i: Integer;
  ampstr: string;
  ws: widestring;
  entity: widechar;
begin
  Result := '';
  i := 1;
  while i <= Length(AText) do begin
    if AText[i]='&' then begin
      ampStr := '';
      inc(i);
      while AText[i] <> ';' do begin
        ampStr := ampStr + AText[i];
        inc(i);
      end;
      ws := UTF8Encode(ampStr);
      if ResolveHTMLEntityReference(ws, entity) then
        Result := Result + UnicodeToUTF8(cardinal(entity))
      else
        Result := Result + '?';
    end else
      Result := Result + AText[i];
    inc(i);
  end;
end;


{ TForm1 }

// Replace %20 with space, \ with /
function FixURL(URL: String):String;
var
  X: LongInt;
begin
  X := Pos('%20', Url);
  while X > 0 do
  begin
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
  txt: string;
begin
  if fStop^ then Exit;
  txt := AItem.KeyWord;
  // Fallback:
  if txt = '' then txt := AItem.Text;
  txt := FixEscapedHTML(ToUTF8(Trim(txt)));
  if not Assigned(fLastNode) or (fLastNode.Text <> txt) then
  begin
    // Add new child node
    fLastNode := AParentNode;
    NewNode := TContentTreeNode(fTreeView.Items.AddChild(AParentNode, txt));
    NewNode.Url := FixURL('/'+AItem.Local);
    NewNode.Data := fChm;
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
  end
  else
    NewNode := TContentTreeNode(fLastNode);

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
  X: Integer;
begin
  fTreeView.BeginUpdate;

  for X := 0 to fSitemap.Items.Count-1 do
    AddItem(fSitemap.Items.Item[X], ParentNode);

  fTreeView.EndUpdate;
end;


end.

