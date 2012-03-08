{ Search options offline wiki

  Copyright (C) 2012  Mattias Gaertner  mattias@freepascal.org

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
unit WikiSearchOptions;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, AvgLvlTree, LazLogger, WikiHelpManager,
  WikiFormat, WikiStrConsts, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, ComCtrls;

type

  { TWikiSearchOptsWnd }

  TWikiSearchOptsWnd = class(TForm)
    ImageList1: TImageList;
    LanguagesGroupBox: TGroupBox;
    LanguagesSplitter: TSplitter;
    LanguagesTreeView: TTreeView;
    procedure FormCreate(Sender: TObject);
  private
    FLanguages: string;
    procedure SetLanguages(AValue: string);
    function LangNodeTextToCode(NodeText: string): string;
    function LangToNodeText(LangID: string; Count: integer = -1): string;
  public
    property Languages: string read FLanguages write SetLanguages;
    procedure UpdateAvailableLanguages;
    procedure UpdateEnabledLanguages;
    function LangCodeEnabled(const ID: string): boolean;
  end;

var
  WikiSearchOptsWnd: TWikiSearchOptsWnd = nil;

implementation

{$R *.lfm}

{ TWikiSearchOptsWnd }

procedure TWikiSearchOptsWnd.FormCreate(Sender: TObject);
begin
  Caption:=wrsWikiSearchOptions;
  LanguagesGroupBox.Caption:=wrsLanguages;
end;

procedure TWikiSearchOptsWnd.SetLanguages(AValue: string);
begin
  if FLanguages=AValue then Exit;
  FLanguages:=AValue;
end;

function TWikiSearchOptsWnd.LangNodeTextToCode(NodeText: string): string;
var
  p: SizeInt;
begin
  p:=Pos(' (',NodeText);
  if p>0 then Delete(NodeText,p,length(NodeText));
  Result:=WikiHelp.LangCaptionToCode(NodeText)
end;

function TWikiSearchOptsWnd.LangToNodeText(LangID: string; Count: integer
  ): string;
begin
  Result:=WikiHelp.LangCodeToCaption(LangID);
  if Count<0 then
    Result+=' (?)'
  else
    Result+=' ('+IntToStr(Count)+')';
end;

procedure TWikiSearchOptsWnd.UpdateAvailableLanguages;
var
  Langs: TStringList;
  i: Integer;
  TVNode: TTreeNode;
  LangToCount: TStringToPointerTree;
  Lang: String;
  S2PItem: PStringToPointerItem;
begin
  // collect all languages and count them
  Langs:=TStringList.Create;
  if WikiHelp.LoadComplete then begin
    LangToCount:=TStringToPointerTree.Create(true);
    try
      for i:=0 to WikiHelp.Converter.Count-1 do begin
        Lang:=GetWikiPageLanguage(WikiHelp.Converter[i].WikiDocumentName);
        LangToCount[Lang]:=LangToCount[Lang]+1;
      end;
      for S2PItem in LangToCount do
        if S2PItem^.Name<>'' then
          Langs.Add(LangToNodeText(S2PItem^.Name,{%H-}PtrUInt(S2PItem^.Value)));
      Langs.Sort;
      Langs.Insert(0,LangToNodeText('',{%H-}PtrUInt(LangToCount[''])));
    finally
      LangToCount.Free;
    end;
  end else begin
    Langs.Add(LangToNodeText(''));
  end;

  LanguagesTreeView.BeginUpdate;
  try
    for i:=0 to Langs.Count-1 do begin
      if i<LanguagesTreeView.Items.TopLvlCount then begin
        TVNode:=LanguagesTreeView.Items.TopLvlItems[i];
        TVNode.Text:=Langs[i];
      end else begin
        TVNode:=LanguagesTreeView.Items.Add(nil,Langs[i]);
      end;
    end;
    while LanguagesTreeView.Items.TopLvlCount>Langs.Count do
      LanguagesTreeView.Items.TopLvlItems[LanguagesTreeView.Items.TopLvlCount-1].Delete;
  finally
    LanguagesTreeView.EndUpdate;
    Langs.Free;
  end;
  UpdateEnabledLanguages;
end;

procedure TWikiSearchOptsWnd.UpdateEnabledLanguages;
var
  i: Integer;
  TVNode: TTreeNode;
  LangEnabled: Boolean;
begin
  for i:=0 to LanguagesTreeView.Items.TopLvlCount-1 do begin
    TVNode:=LanguagesTreeView.Items.TopLvlItems[i];
    LangEnabled:=LangCodeEnabled(LangNodeTextToCode(TVNode.Text));
    if LangEnabled then
      TVNode.StateIndex:=1
    else
      TVNode.StateIndex:=0;
  end;
end;

function TWikiSearchOptsWnd.LangCodeEnabled(const ID: string): boolean;
begin
  Result:=WikiLangInLanguageFilter(ID,Languages);
end;

end.

