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
  Classes, SysUtils, FileUtil, LazLogger, BasicCodeTools,
  CodeToolsStructs, WikiHelpManager, WikiStrConsts, WikiFormat, Forms, Controls,
  Graphics, Dialogs, ExtCtrls, StdCtrls, ComCtrls, Grids;

type

  { TWikiSearchOptsWnd }

  TWikiSearchOptsWnd = class(TForm)
    ImageList1: TImageList;
    LanguagesGroupBox: TGroupBox;
    LanguagesSplitter: TSplitter;
    LanguagesTreeView: TTreeView;
    ScoresGroupBox: TGroupBox;
    ScoresStringGrid: TStringGrid;
    procedure FormClose(Sender: TObject; var {%H-}CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure LanguagesTreeViewMouseDown(Sender: TObject; {%H-}Button: TMouseButton;
      {%H-}Shift: TShiftState; X, Y: Integer);
    procedure ScoresStringGridEditingDone(Sender: TObject);
  private
    FLanguages: TStringToStringTree;
    FOnOptionsChanged: TNotifyEvent;
    FScoring: TWHScoring;
    function GetLangCodeEnabled(const ID: string): boolean;
    function GetLanguages: string;
    function LangNodeTextToCode(NodeText: string): string;
    function LangToNodeText(LangID: string; Count: integer = -1): string;
    procedure SetLangCodeEnabled(const ID: string; AValue: boolean);
    procedure SetLanguages(AValue: string);
    procedure FillScoresGrid;
    function Score2String(s: single): string;
    procedure DoOptionsChanged;
  public
    property Languages: string read GetLanguages write SetLanguages;
    property LangCodeEnabled[const ID: string]: boolean read GetLangCodeEnabled
                                                       write SetLangCodeEnabled;
    procedure UpdateAvailableLanguages;
    procedure UpdateEnabledLanguages;
    property OnOptionsChanged: TNotifyEvent read FOnOptionsChanged write FOnOptionsChanged;
    property Scoring: TWHScoring read FScoring;
  end;

var
  WikiSearchOptsWnd: TWikiSearchOptsWnd = nil;

implementation

{$R *.lfm}

{ TWikiSearchOptsWnd }

procedure TWikiSearchOptsWnd.FormCreate(Sender: TObject);
begin
  FScoring:=TWHScoring.Create;
  FScoring.Assign(WikiHelp.DefaultScoring);

  Caption:=wrsWikiSearchOptions;
  LanguagesGroupBox.Caption:=wrsLanguages;
  FLanguages:=TStringToStringTree.Create(false);
  FLanguages['']:='1';

  ScoresGroupBox.Caption:=wrsScores;
  with ScoresStringGrid do begin
    RowCount:=9;
    Cells[0, 1]:=wrsPageTitleWholeWord;
    Cells[0, 2]:=wrsPageTitlePart;
    Cells[0, 3]:=wrsHeaderWholeWord;
    Cells[0, 4]:=wrsHeaderPart;
    Cells[0, 5]:=wrsTextWholeWord;
    Cells[0, 6]:=wrsTextPart;
    Cells[0, 7]:=wrsLinkWholeWord;
    Cells[0, 8]:=wrsLinkPart;
  end;
  FillScoresGrid;
end;

procedure TWikiSearchOptsWnd.FormClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  FreeAndNil(FLanguages);
  FreeAndNil(FScoring);
end;

procedure TWikiSearchOptsWnd.LanguagesTreeViewMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  TVNode: TTreeNode;
  LangID: String;
begin
  TVNode:=LanguagesTreeView.GetNodeAt(X,Y);
  if TVNode=nil then exit;
  if x>=TVNode.DisplayStateIconLeft then begin
    LangID:=LangNodeTextToCode(TVNode.Text);
    LangCodeEnabled[LangID]:=not LangCodeEnabled[LangID];
  end;
end;

procedure TWikiSearchOptsWnd.ScoresStringGridEditingDone(Sender: TObject);
var
  Category: TWHFitsCategory;
  FitsString: TWHFitsStringFlag;
  OldScore: Single;
  NewScore: Extended;
  Row: Integer;
  Col: Integer;
begin
  Row:=ScoresStringGrid.Row;
  Col:=ScoresStringGrid.Col;
  if Col=1 then begin
    case Row of
    1: begin Category:=whfcPageTitle; FitsString:=whfsWholeWord; end;
    2: begin Category:=whfcPageTitle; FitsString:=whfsPart; end;
    3: begin Category:=whfcHeader; FitsString:=whfsWholeWord; end;
    4: begin Category:=whfcHeader; FitsString:=whfsPart; end;
    5: begin Category:=whfcText; FitsString:=whfsWholeWord; end;
    6: begin Category:=whfcText; FitsString:=whfsPart; end;
    7: begin Category:=whfcLink; FitsString:=whfsWholeWord; end;
    8: begin Category:=whfcLink; FitsString:=whfsPart; end;
    else exit;
    end;
    OldScore:=Scoring.Phrases[Category,FitsString];
    NewScore:=StrToFloatDef(ScoresStringGrid.Cells[Col,Row],OldScore);
    if (NewScore<-10000) then NewScore:=-10000;
    if (NewScore>10000) then NewScore:=10000;
    ScoresStringGrid.Cells[Col,Row]:=Score2String(NewScore);
    if OldScore<>NewScore then begin
      Scoring.Phrases[Category,FitsString]:=NewScore;
      DoOptionsChanged;
    end;
  end;
end;

procedure TWikiSearchOptsWnd.SetLanguages(AValue: string);
var
  p: PChar;
  StartPos: PChar;
  i: Integer;
  Lang: String;
begin
  if AValue=GetLanguages then Exit;
  FLanguages.Clear;
  if AValue='' then begin
    // empty = only english,original
    FLanguages['']:='1';
  end else begin
    p:=PChar(AValue);
    while p^<>#0 do begin
      StartPos:=p;
      while not (p^ in [#0,',']) do inc(p);
      if p>StartPos then begin
        if StartPos^='-' then begin
          // not original language
        end else if StartPos^='*' then begin
          // fit any
          for i:=0 to LanguagesTreeView.Items.TopLvlCount-1 do
            FLanguages[LangNodeTextToCode(LanguagesTreeView.Items.TopLvlItems[i].Text)]:='1';
        end else  begin
          // a specific language
          Lang:=SubString(StartPos,p-StartPos);
          if WikiLangCodeToCaption(Lang)<>Lang then
            FLanguages[Lang]:='1';
        end;
      end;
      while p^=',' do inc(p);
    end;
  end;
  UpdateEnabledLanguages;
end;

procedure TWikiSearchOptsWnd.FillScoresGrid;
begin
  with ScoresStringGrid do begin
    Cells[1,1]:=Score2String(Scoring.Phrases[whfcPageTitle,whfsWholeWord]);
    Cells[1,2]:=Score2String(Scoring.Phrases[whfcPageTitle,whfsPart]);
    Cells[1,3]:=Score2String(Scoring.Phrases[whfcHeader,whfsWholeWord]);
    Cells[1,4]:=Score2String(Scoring.Phrases[whfcHeader,whfsPart]);
    Cells[1,5]:=Score2String(Scoring.Phrases[whfcText,whfsWholeWord]);
    Cells[1,6]:=Score2String(Scoring.Phrases[whfcText,whfsPart]);
    Cells[1,7]:=Score2String(Scoring.Phrases[whfcLink,whfsWholeWord]);
    Cells[1,8]:=Score2String(Scoring.Phrases[whfcLink,whfsPart]);
  end;
end;

function TWikiSearchOptsWnd.Score2String(s: single): string;
begin
  Result:=FloatToStrF(s,ffGeneral,5,2);
end;

procedure TWikiSearchOptsWnd.DoOptionsChanged;
begin
  if Assigned(OnOptionsChanged) then
    OnOptionsChanged(Self);
end;

procedure TWikiSearchOptsWnd.SetLangCodeEnabled(const ID: string;
  AValue: boolean);
begin
  if AValue=GetLangCodeEnabled(ID) then exit;
  if AValue then
    FLanguages[ID]:='1'
  else
    FLanguages.Remove(ID);
  UpdateEnabledLanguages;
  DoOptionsChanged;
end;

function TWikiSearchOptsWnd.LangNodeTextToCode(NodeText: string): string;
var
  p: SizeInt;
begin
  p:=Pos(' (',NodeText);
  if p>0 then Delete(NodeText,p,length(NodeText));
  Result:=WikiLangCaptionToCode(NodeText);
end;

function TWikiSearchOptsWnd.LangToNodeText(LangID: string; Count: integer
  ): string;
begin
  Result:=WikiLangCodeToCaption(LangID);
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
  S2PItem: PStringToPointerTreeItem;
begin
  // collect all languages and count them
  Langs:=TStringList.Create;
  if WikiHelp.LoadComplete then begin
    LangToCount:=TStringToPointerTree.Create(true);
    try
      for i:=0 to WikiHelp.Converter.Count-1 do begin
        Lang:=WikiHelp.Converter[i].WikiLanguage;
        LangToCount[Lang]:=LangToCount[Lang]+1;
      end;
      for S2PItem in LangToCount do
        if S2PItem^.Name<>'' then
          Langs.Add(LangToNodeText(S2PItem^.Name,{%H-}PtrUInt(S2PItem^.Value)));
      Langs.Sort;
      Langs.Insert(0,LangToNodeText('',{%H-}PtrUInt(LangToCount[''])));
      Langs.Add(LangToNodeText('*',WikiHelp.Converter.Count));
    finally
      LangToCount.Free;
    end;
  end else begin
    Langs.Add(LangToNodeText(''));
    Langs.Add(LangToNodeText('*',0));
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
    LangEnabled:=LangCodeEnabled[LangNodeTextToCode(TVNode.Text)];
    if LangEnabled then
      TVNode.StateIndex:=1
    else
      TVNode.StateIndex:=0;
  end;
end;

function TWikiSearchOptsWnd.GetLangCodeEnabled(const ID: string): boolean;
begin
  Result:=FLanguages.Contains(ID);
end;

function TWikiSearchOptsWnd.GetLanguages: string;
var
  S2SItem: PStringToStringTreeItem;
begin
  Result:='';
  if not FLanguages.Contains('') then
    Result:='-';
  for S2SItem in FLanguages do begin
    if S2SItem^.Name='' then continue;
    if Result<>'' then Result+=',';
    Result+=S2SItem^.Name;
  end;
end;

end.

