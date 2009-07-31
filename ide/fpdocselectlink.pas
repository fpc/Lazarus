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
  
  Abstract:
    Dialog used by the fpdoc editor to create a link.
}
unit FPDocSelectLink;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LCLProc, LResources, Forms, Controls, Graphics, Dialogs,
  ExtCtrls, StdCtrls, ButtonPanel,
  LazarusIDEStrConsts, PackageSystem, PackageDefs;

type

  { TFPDocLinkCompletionItem }

  TFPDocLinkCompletionItem = class
  public
    Text: string;
    Description: string;
    constructor Create(const AText, ADescription: string);
  end;

  { TFPDocLinkCompletionList }

  TFPDocLinkCompletionList = class
  private
    FItems: TFPList; // list of TFPDocLinkCompletionItem
    FSelected: integer;
    FTop: integer;
    function GetCount: integer;
    function GetItems(Index: integer): TFPDocLinkCompletionItem;
    procedure SetSelected(const AValue: integer);
    procedure SetTop(const AValue: integer);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    procedure AddPackage(Pkg: TLazPackage);
    procedure Draw(Canvas: TCanvas; Width, Height: integer);
    property Count: integer read GetCount;
    property Items[Index: integer]: TFPDocLinkCompletionItem read GetItems;
    property Top: integer read FTop write SetTop;
    property Selected: integer read FSelected write SetSelected;
  end;

  { TFPDocLinkEditorDlg }

  TFPDocLinkEditorDlg = class(TForm)
    ButtonPanel1: TButtonPanel;
    CompletionBox: TPaintBox;
    TitleEdit: TEdit;
    TitleLabel: TLabel;
    LinkEdit: TEdit;
    LinkLabel: TLabel;
    procedure CompletionBoxPaint(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure LinkEditChange(Sender: TObject);
    procedure LinkEditKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState
      );
  private
    fItems: TFPDocLinkCompletionList;
    FSourceFilename: string;
    function GetLink: string;
    function GetLinkTitle: string;
    procedure SetLink(const AValue: string);
    procedure SetLinkTitle(const AValue: string);
    procedure SetSourceFilename(const AValue: string);
    procedure UpdateCompletionBox;
    procedure AddPackagesToCompletion(Prefix: string);
  public
    procedure SetLink(const ASrcFilename, ATitle, ALink: string);
    property SourceFilename: string read FSourceFilename write SetSourceFilename;
    property LinkTitle: string read GetLinkTitle write SetLinkTitle;
    property Link: string read GetLink write SetLink;
  end;

function ShowFPDocLinkEditorDialog(SrcFilename: string;
  out Link, LinkTitle: string): TModalResult;

implementation

function ShowFPDocLinkEditorDialog(SrcFilename: string;
  out Link, LinkTitle: string): TModalResult;
var
  FPDocLinkEditorDlg: TFPDocLinkEditorDlg;
begin
  Link:='';
  LinkTitle:='';
  FPDocLinkEditorDlg:=TFPDocLinkEditorDlg.Create(nil);
  try
    FPDocLinkEditorDlg.SetLink(SrcFilename,LinkTitle,Link);
    Result:=FPDocLinkEditorDlg.ShowModal;
    if Result=mrOk then begin
      Link:=FPDocLinkEditorDlg.Link;
      LinkTitle:=FPDocLinkEditorDlg.LinkTitle;
    end;
  finally
    FPDocLinkEditorDlg.Free;
  end;
end;

{ TFPDocLinkEditorDlg }

procedure TFPDocLinkEditorDlg.FormCreate(Sender: TObject);
begin
  Caption:=lisChooseAFPDocLink;
  LinkLabel.Caption:=lisLinkTarget;
  LinkLabel.Hint:=Format(lisExamplesIdentifierTMyEnumEnumUnitnameIdentifierPac,
    [#13, #13, #13, #13]);
  TitleLabel.Caption:=lisTitleLeaveEmptyForDefault;
  
  LinkEdit.Text:='';
  TitleEdit.Text:='';

  FItems:=TFPDocLinkCompletionList.Create;
end;

procedure TFPDocLinkEditorDlg.FormDestroy(Sender: TObject);
begin
  FreeAndNil(fItems);
end;

procedure TFPDocLinkEditorDlg.CompletionBoxPaint(Sender: TObject);
begin
  CompletionBox.Canvas.Brush.Color:=clInfoBk;
  CompletionBox.Canvas.Font.Color:=clInfoText;
  fItems.Draw(CompletionBox.Canvas,
              CompletionBox.ClientWidth,CompletionBox.ClientHeight);
end;

procedure TFPDocLinkEditorDlg.LinkEditChange(Sender: TObject);
begin

end;

procedure TFPDocLinkEditorDlg.LinkEditKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin

end;

procedure TFPDocLinkEditorDlg.SetSourceFilename(const AValue: string);
begin
  if FSourceFilename=AValue then exit;
  FSourceFilename:=AValue;
end;

procedure TFPDocLinkEditorDlg.UpdateCompletionBox;
{
  ToDo:
  empty  : show all packages, all units of current project/package and all identifiers of unit
  #l     : show all packages beginning with the letter l
  #lcl.  : show all units of package lcl
  f      : show all units and all identifiers beginning with the letter f
  forms. : show all identifiers of unit forms and all sub identifiers of identifier forms

  forms.tcontrol.        : show all sub identifiers of identifier tcontrol
  #lcl.forms.            : same as above
  #lcl.forms.tcontrol.   : same as above
}
var
  l: String;
begin
  if FItems=nil then exit;
  fItems.Clear;
  l:=LinkEdit.Text;
  DebugLn(['TFPDocLinkEditorDlg.UpdateCompletionBox Prefix="',l,'"']);
  if (l='') then begin
    AddPackagesToCompletion(l);
    //AddSiblingUnits(l);
  end else begin

  end;
  CompletionBox.Invalidate;
end;

procedure TFPDocLinkEditorDlg.AddPackagesToCompletion(Prefix: string);
var
  i: Integer;
  Pkg: TLazPackage;
begin
  for i:=0 to PackageGraph.Count-1 do begin
    Pkg:=PackageGraph.Packages[i];
    if Pkg.LazDocPaths='' then continue;
    if (SysUtils.CompareText(Prefix,copy(Pkg.Name,1,length(Prefix)))=0) then
      fItems.AddPackage(Pkg);
  end;
end;

procedure TFPDocLinkEditorDlg.SetLink(const ASrcFilename, ATitle, ALink: string
  );
begin
  SourceFilename:=ASrcFilename;
  LinkTitle:=ATitle;
  Link:=ALink;
  UpdateCompletionBox;
end;

function TFPDocLinkEditorDlg.GetLinkTitle: string;
begin
  Result:=TitleEdit.Text;
end;

function TFPDocLinkEditorDlg.GetLink: string;
begin
  Result:=LinkEdit.Text;
end;

procedure TFPDocLinkEditorDlg.SetLink(const AValue: string);
begin
  LinkEdit.Text:=AValue;
end;

procedure TFPDocLinkEditorDlg.SetLinkTitle(const AValue: string);
begin
  TitleEdit.Text:=AValue;
end;

{ TFPDocLinkCompletionList }

function TFPDocLinkCompletionList.GetCount: integer;
begin
  Result:=FItems.Count;
end;

function TFPDocLinkCompletionList.GetItems(Index: integer
  ): TFPDocLinkCompletionItem;
begin
  Result:=TFPDocLinkCompletionItem(FItems[Index]);
end;

procedure TFPDocLinkCompletionList.SetSelected(const AValue: integer);
begin
  if FSelected=AValue then exit;
  FSelected:=AValue;
end;

procedure TFPDocLinkCompletionList.SetTop(const AValue: integer);
begin
  if FTop=AValue then exit;
  FTop:=AValue;
end;

constructor TFPDocLinkCompletionList.Create;
begin
  FItems:=TFPList.Create;
end;

destructor TFPDocLinkCompletionList.Destroy;
begin
  Clear;
  FreeAndNil(FItems);
  inherited Destroy;
end;

procedure TFPDocLinkCompletionList.Clear;
var
  i: Integer;
begin
  for i:=0 to FItems.Count-1 do TObject(FItems[i]).Free;
  FItems.Clear;
end;

procedure TFPDocLinkCompletionList.AddPackage(Pkg: TLazPackage);
begin
  FItems.Add(TFPDocLinkCompletionItem.Create('#'+Pkg.Name,'Package '+Pkg.IDAsString));
end;

procedure TFPDocLinkCompletionList.Draw(Canvas: TCanvas; Width, Height: integer);
var
  i: LongInt;
  y: Integer;
  dy: LongInt;
  Item: TFPDocLinkCompletionItem;
  s: String;
begin
  DebugLn(['TFPDocLinkCompletionList.Draw ',Width,' ',Height,' Count=',Count]);
  i:=Top;
  y:=0;
  dy:=Canvas.TextHeight('ABCTWSMgqp')+4;
  while (y<Height) and (i<Count) do begin
    Item:=Items[i];
    Canvas.FillRect(0,y,Width,y+dy);
    s:=Item.Text;
    Canvas.TextOut(2,y+2,s);
    s:=Item.Description;
    Canvas.TextOut(152,y+2,s);
    inc(y,dy);
    inc(i);
  end;
  if y<Height then begin
    Canvas.FillRect(0,y,Width,Height);
  end;
end;

{ TFPDocLinkCompletionItem }

constructor TFPDocLinkCompletionItem.Create(const AText, ADescription: string);
begin
  Text:=AText;
  Description:=ADescription;
end;

initialization
  {$I fpdocselectlink.lrs}

end.

