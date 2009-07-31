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
  ExtCtrls, StdCtrls, ButtonPanel, FileUtil,
  PackageIntf, ProjectIntf,
  CodeHelp, LazarusIDEStrConsts, PackageSystem, PackageDefs, Laz_DOM;

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
    procedure AddProjectFile(AFile: TLazProjectFile);
    procedure AddPackageFile(AFile: TPkgFile);
    procedure AddIdentifier(Identifier: string);
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
    FDocFile: TLazFPDocFile;
    fItems: TFPDocLinkCompletionList;
    FSourceFilename: string;
    fSourceOwner: TObject;
    function GetLink: string;
    function GetLinkTitle: string;
    procedure SetDocFile(const AValue: TLazFPDocFile);
    procedure SetLink(const AValue: string);
    procedure SetLinkTitle(const AValue: string);
    procedure SetSourceFilename(const AValue: string);
    procedure UpdateCompletionBox;
    procedure AddPackagesToCompletion(Prefix: string);
    procedure AddSiblingUnits(Prefix: string);
    procedure AddPackageUnits(APackage: TLazPackage; Prefix: string);
    procedure AddIdentifiers(Prefix: string);
    procedure AddSubIdentifiers(Path: string);
  public
    procedure SetLink(const ASrcFilename, ATitle, ALink: string;
                      ADocFile: TLazFPDocFile);
    property SourceFilename: string read FSourceFilename write SetSourceFilename;
    property LinkTitle: string read GetLinkTitle write SetLinkTitle;
    property Link: string read GetLink write SetLink;
    property DocFile: TLazFPDocFile read FDocFile write SetDocFile;
  end;

function ShowFPDocLinkEditorDialog(SrcFilename: string; DocFile: TLazFPDocFile;
  out Link, LinkTitle: string): TModalResult;

implementation

function ShowFPDocLinkEditorDialog(SrcFilename: string; DocFile: TLazFPDocFile;
  out Link, LinkTitle: string): TModalResult;
var
  FPDocLinkEditorDlg: TFPDocLinkEditorDlg;
begin
  Link:='';
  LinkTitle:='';
  FPDocLinkEditorDlg:=TFPDocLinkEditorDlg.Create(nil);
  try
    FPDocLinkEditorDlg.SetLink(SrcFilename,LinkTitle,Link,DocFile);
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
var
  Owners: TFPList;
  i: Integer;
begin
  if FSourceFilename=AValue then exit;
  FSourceFilename:=AValue;
  fSourceOwner:=nil;
  Owners:=PackageEditingInterface.GetPossibleOwnersOfUnit(FSourceFilename,
    [piosfIncludeSourceDirectories]);
  if Owners=nil then exit;
  try
    for i:=0 to Owners.Count-1 do begin
      if TObject(Owners[i]) is TLazProject then begin
        fSourceOwner:=TLazProject(Owners[i]);
      end else if TObject(Owners[i]) is TLazPackage then begin
        if fSourceOwner=nil then
          fSourceOwner:=TLazPackage(Owners[i]);
      end;
    end;
  finally
    Owners.Free;
  end;
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
  AddSubIdentifiers(l);
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

procedure TFPDocLinkEditorDlg.AddSiblingUnits(Prefix: string);
var
  AProject: TLazProject;
  i: Integer;
  ProjFile: TLazProjectFile;
  APackage: TLazPackage;
  Filename: String;
begin
  if fSourceOwner=nil then exit;
  if fSourceOwner is TLazProject then begin
    AProject:=TLazProject(fSourceOwner);
    for i:=0 to AProject.FileCount-1 do begin
      ProjFile:=AProject.Files[i];
      if ProjFile.IsPartOfProject then begin
        Filename:=ProjFile.Filename;
        if FilenameIsPascalUnit(Filename) then begin
          Filename:=ExtractFileNameOnly(Filename);
          if (CompareFilenames(Prefix,copy(Filename,1,length(Prefix)))=0) then
            fItems.AddProjectFile(ProjFile);
        end;
      end;
    end;
  end else if fSourceOwner is TLazPackage then begin
    APackage:=TLazPackage(fSourceOwner);
    AddPackageUnits(APackage,Prefix);
  end;
end;

procedure TFPDocLinkEditorDlg.AddPackageUnits(APackage: TLazPackage;
  Prefix: string);
var
  i: Integer;
  PkgFile: TPkgFile;
  Filename: String;
begin
  for i:=0 to APackage.FileCount-1 do begin
    PkgFile:=APackage.Files[i];
    if FilenameIsPascalUnit(PkgFile.Filename) then begin
      Filename:=PkgFile.Filename;
      if FilenameIsPascalUnit(Filename) then begin
        Filename:=ExtractFileNameOnly(Filename);
        if (CompareFilenames(Prefix,copy(Filename,1,length(Prefix)))=0) then
          fItems.AddPackageFile(PkgFile);
      end;
    end;
  end;
end;

procedure TFPDocLinkEditorDlg.AddIdentifiers(Prefix: string);
var
  DOMNode: TDOMNode;
  ElementName: String;
begin
  if fDocFile=nil then exit;
  DOMNode:=FDocFile.GetFirstElement;
  while DOMNode<>nil do begin
    if (DOMNode is TDomElement) then begin
      ElementName:=TDomElement(DOMNode).GetAttribute('name');
      if (System.Pos('.',ElementName)<1)
      and (SysUtils.CompareText(Prefix,copy(ElementName,1,length(Prefix)))=0)
      then
        FItems.AddIdentifier(ElementName);
    end;
    DOMNode:=DOMNode.NextSibling;
  end;
end;

procedure TFPDocLinkEditorDlg.AddSubIdentifiers(Path: string);
var
  p: LongInt;
  PrePath: String;
  Pkg: TLazPackage;
  PkgFile: TPkgFile;
begin
  p:=System.Pos('.',Path);
  if p<1 then begin
    // empty  : show all packages, all units of current project/package and all identifiers of unit
    // #l     : show all packages beginning with the letter l
    // f      : show all units and all identifiers beginning with the letter f
    if (Path='') or (Path[1]='#') then
      AddPackagesToCompletion(copy(Path,2,length(Path)));
    if (Path='') or (Path[1]<>'#') then begin
      AddSiblingUnits(Path);
      AddIdentifiers(Path);
    end;
  end else begin
    // sub identifier
    // #lcl.f  : show all units of package lcl
    // forms.f : show all identifiers of unit forms and all sub identifiers of identifier forms
    PrePath:=copy(Path,1,p-1);
    Path:=copy(Path,p+1,length(Path));
    if PrePath='' then exit;
    if PrePath[1]='#' then begin
      // package
      Pkg:=PackageGraph.FindAPackageWithName(PrePath,nil);
      if Pkg=nil then exit;
      p:=System.Pos('.',Path);
      if p<1 then begin
        AddPackageUnits(Pkg,PrePath);
      end else begin
        // unit
        PrePath:=copy(Path,1,p-1);
        Path:=copy(Path,p+1,length(Path));
        if PrePath='' then exit;
        PkgFile:=Pkg.FindUnit(PrePath);
        if PkgFile=nil then exit;

      end;
    end;
  end;
end;

procedure TFPDocLinkEditorDlg.SetLink(const ASrcFilename, ATitle, ALink: string;
  ADocFile: TLazFPDocFile);
begin
  SourceFilename:=ASrcFilename;
  DocFile:=ADocFile;
  LinkTitle:=ATitle;
  Link:=ALink;
  UpdateCompletionBox;
end;

function TFPDocLinkEditorDlg.GetLinkTitle: string;
begin
  Result:=TitleEdit.Text;
end;

procedure TFPDocLinkEditorDlg.SetDocFile(const AValue: TLazFPDocFile);
begin
  if FDocFile=AValue then exit;
  FDocFile:=AValue;
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
  FItems.Add(TFPDocLinkCompletionItem.Create('#'+Pkg.Name,'package '+Pkg.IDAsString));
end;

procedure TFPDocLinkCompletionList.AddProjectFile(AFile: TLazProjectFile);
begin
  FItems.Add(TFPDocLinkCompletionItem.Create(
    ExtractFileNameOnly(AFile.Filename),'project unit'));
end;

procedure TFPDocLinkCompletionList.AddPackageFile(AFile: TPkgFile);
begin
  FItems.Add(TFPDocLinkCompletionItem.Create(
    ExtractFileNameOnly(AFile.Filename),'package unit'));
end;

procedure TFPDocLinkCompletionList.AddIdentifier(Identifier: string);
begin
  FItems.Add(TFPDocLinkCompletionItem.Create(Identifier,'identifier'));
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

