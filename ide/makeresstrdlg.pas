{  $Id$  }
{
 /***************************************************************************
                            makerestrdlg.pas
                            ----------------


 ***************************************************************************/

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
   TMakeResStrDialog is the dialog to setup how to convert a string constant
   into pascal resourcestrings.
 
}
unit MakeResStrDlg;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Buttons, ComCtrls, StdCtrls, Dialogs,
  ExtCtrls, LResources, LazarusIDEStrConsts, IDEOptionDefs, CodeToolManager,
  CodeAtom, CodeToolsStructs, CodeCache, SynHighlighterPas, SynEdit,
  EditorOptions;
  
type
  TMakeResStrDialog = class(TForm)
    MainNotebook: TNoteBook;
    SourcePage: TPage;
    IdentifierPage: TPage;
    ResStrPage: TPage;
    StringConstGroupBox: TGroupBox;
    StringConstSynEdit: TSynEdit;
    SrcPreviewGroupBox: TGroupBox;
    SrcPreviewSynEdit: TSynEdit;
    IdentPrefixGroupBox: TGroupBox;
    IdentPrefixComboBox: TComboBox;
    IdentifierListView: TListView;
    ResStrSectionGroupBox: TGroupBox;
    ResStrSectionComboBox: TComboBox;
    InsertPositionRadioGroup: TRadioGroup;
    ResStrPreviewGroupBox: TGroupBox;
    ResStrPreviewSynEdit: TSynEdit;
    OkButton: TButton;
    CancelButton: TButton;
    SynPasSyn: TSynPasSyn;
    procedure CancelButtonClick(Sender: TObject);
    procedure IdentPrefixGroupBoxResize(Sender: TObject);
    procedure IdentifierPageResize(Sender: TObject);
    procedure MakeResStrDialogResize(Sender: TObject);
    procedure OkButtonClick(Sender: TObject);
    procedure ResStrPageResize(Sender: TObject);
    procedure ResStrSectionGroupBoxResize(Sender: TObject);
    procedure SourcePageResize(Sender: TObject);
  private
    procedure SetupComponents;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure FillResourceStringSections(Positions: TCodeXYPositions);
  end;
  
function ShowMakeResStrDialog(
  const StartPos, EndPos: TPoint; Code: TCodeBuffer;
  Positions: TCodeXYPositions): TModalResult;
  

implementation

uses
  Math;

function ShowMakeResStrDialog(
  const StartPos, EndPos: TPoint; Code: TCodeBuffer;
  Positions: TCodeXYPositions): TModalResult;
var
  MakeResStrDialog: TMakeResStrDialog;
begin
  MakeResStrDialog:=TMakeResStrDialog.Create(Application);
  // string constant
  MakeResStrDialog.StringConstSynEdit.Text:=Code.GetLines(StartPos.Y,EndPos.Y);
  // reachable resourcestring sections
  MakeResStrDialog.FillResourceStringSections(Positions);
  // identifier prefixes
  // ToDo
  // identifiers and values
  // ToDo
  // resourcestrings
  // ToDo
  // new source
  // ToDo
  
  Result:=MakeResStrDialog.ShowModal;
  IDEDialogLayoutList.SaveLayout(MakeResStrDialog);
  MakeResStrDialog.Free;
end;

{ TMakeResStrDialog }

procedure TMakeResStrDialog.MakeResStrDialogResize(Sender: TObject);
begin
  with MainNotebook do begin
    SetBounds(0,0,Parent.ClientWidth,Parent.ClientHeight-45);
  end;

  with OkButton do begin
    SetBounds(Parent.ClientWidth-200,Parent.ClientHeight-35,80,25);
  end;

  with CancelButton do begin
    SetBounds(OkButton.Left+100,OkButton.Top,OkButton.Width,OkButton.Height);
  end;
end;

procedure TMakeResStrDialog.CancelButtonClick(Sender: TObject);
begin
  ModalResult:=mrCancel;
end;

procedure TMakeResStrDialog.IdentPrefixGroupBoxResize(Sender: TObject);
begin
  with IdentPrefixComboBox do begin
    SetBounds(0,0,Parent.ClientWidth,Height);
  end;
end;

procedure TMakeResStrDialog.IdentifierPageResize(Sender: TObject);
var
  NewTop: Integer;
begin
  with IdentPrefixGroupBox do begin
    SetBounds(0,0,Parent.ClientWidth,50);
  end;

  with IdentifierListView do begin
    NewTop:=IdentPrefixGroupBox.Top+IdentPrefixGroupBox.Height+5;
    SetBounds(0,NewTop,Parent.ClientWidth,Parent.ClientHeight-NewTop);
  end;
end;

procedure TMakeResStrDialog.OkButtonClick(Sender: TObject);
begin
  ModalResult:=mrOk;
end;

procedure TMakeResStrDialog.ResStrPageResize(Sender: TObject);
var
  NewTop: Integer;
begin
  with ResStrSectionGroupBox do begin
    SetBounds(0,0,Parent.ClientWidth,50);
  end;

  with InsertPositionRadioGroup do begin
    SetBounds(0,ResStrSectionGroupBox.Top+ResStrSectionGroupBox.Height+5,
              Parent.ClientWidth,50);
  end;

  with ResStrPreviewGroupBox do begin
    NewTop:=InsertPositionRadioGroup.Top+InsertPositionRadioGroup.Height+5;
    SetBounds(0,NewTop,
              Parent.ClientWidth,Max(Parent.ClientHeight-NewTop,5));
  end;
end;

procedure TMakeResStrDialog.ResStrSectionGroupBoxResize(Sender: TObject);
begin
  with ResStrSectionComboBox do begin
    SetBounds(0,0,Parent.ClientWidth,Height);
  end;
end;

procedure TMakeResStrDialog.SourcePageResize(Sender: TObject);
var
  NewTop: Integer;
begin
  with StringConstGroupBox do begin
    SetBounds(0,0,Parent.ClientWidth,(Parent.ClientHeight div 2)-5);
  end;

  with SrcPreviewGroupBox do begin
    NewTop:=StringConstGroupBox.Top+StringConstGroupBox.Height+5;
    SetBounds(StringConstGroupBox.Left,NewTop,
              StringConstGroupBox.Width,Parent.ClientHeight-NewTop);
  end;
end;

procedure TMakeResStrDialog.SetupComponents;
begin
  SynPasSyn:=TSynPasSyn.Create(Self);

  MainNotebook:=TNoteBook.Create(Self);
  with MainNotebook do begin
    Name:='MainNotebook';
    Parent:=Self;
    Pages.Add('Source');
    Pages.Add('Identifiers');
    Pages.Add('ResourceStrings');
    SourcePage:=Page[0];
    IdentifierPage:=Page[1];
    ResStrPage:=Page[2];
  end;
  SourcePage.OnResize:=@SourcePageResize;
  IdentifierPage.OnResize:=@IdentifierPageResize;
  ResStrPage.OnResize:=@ResStrPageResize;

  StringConstGroupBox:=TGroupBox.Create(Self);
  with StringConstGroupBox do begin
    Name:='StringConstGroupBox';
    Parent:=SourcePage;
    Caption:='String Constant in source';
  end;
  
  StringConstSynEdit:=TSynEdit.Create(Self);
  with StringConstSynEdit do begin
    Name:='StringConstSynEdit';
    Parent:=StringConstGroupBox;
    Align:=alClient;
    Highlighter:=SynPasSyn;
  end;

  SrcPreviewGroupBox:=TGroupBox.Create(Self);
  with SrcPreviewGroupBox do begin
    Name:='SrcPreviewGroupBox';
    Parent:=SourcePage;
    Caption:='Source preview';
  end;

  SrcPreviewSynEdit:=TSynEdit.Create(Self);
  with SrcPreviewSynEdit do begin
    Name:='SrcPreviewSynEdit';
    Parent:=SrcPreviewGroupBox;
    Align:=alClient;
    Highlighter:=SynPasSyn;
  end;

  IdentPrefixGroupBox:=TGroupBox.Create(Self);
  with IdentPrefixGroupBox do begin
    Name:='IdentPrefixGroupBox';
    Parent:=IdentifierPage;
    Caption:='Identifier Prefix';
    OnResize:=@IdentPrefixGroupBoxResize;
  end;
  
  IdentPrefixComboBox:=TComboBox.Create(Self);
  with IdentPrefixComboBox do begin
    Name:='IdentPrefixComboBox';
    Parent:=IdentPrefixGroupBox;
  end;

  IdentifierListView:=TListView.Create(Self);
  with IdentifierListView do begin
    Name:='IdentifierListView';
    Parent:=IdentifierPage;
  end;
  
  ResStrSectionGroupBox:=TGroupBox.Create(Self);
  with ResStrSectionGroupBox do begin
    Name:='ResStrSectionGroupBox';
    Parent:=ResStrPage;
    OnResize:=@ResStrSectionGroupBoxResize;
  end;

  ResStrSectionComboBox:=TComboBox.Create(Self);
  with ResStrSectionComboBox do begin
    Name:='ResStrSectionComboBox';
    Parent:=ResStrSectionGroupBox;
  end;

  InsertPositionRadioGroup:=TRadioGroup.Create(Self);
  with InsertPositionRadioGroup do begin
    Name:='InsertPositionRadioGroup';
    Parent:=ResStrPage;
    Caption:='Insert Position';
    with Items do begin
      Add('Alphabetical');
      Add('Append');
    end;
    Columns:=2;
  end;
  
  ResStrPreviewGroupBox:=TGroupBox.Create(Self);
  with ResStrPreviewGroupBox do begin
    Name:='ResStrPreviewGroupBox';
    Parent:=ResStrPage;
    Caption:='ResourceStrings preview';
  end;

  ResStrPreviewSynEdit:=TSynEdit.Create(Self);
  with ResStrPreviewSynEdit do begin
    Name:='ResStrPreviewSynEdit';
    Parent:=ResStrPreviewGroupBox;
    Align:=alClient;
    Highlighter:=SynPasSyn;
  end;

  OkButton:=TButton.Create(Self);
  with OkButton do begin
    Name:='OkButton';
    Parent:=Self;
    Caption:='Ok';
    OnClick:=@OkButtonClick;
  end;
  
  CancelButton:=TButton.Create(Self);
  with CancelButton do begin
    Name:='CancelButton';
    Parent:=Self;
    Caption:='Cancel';
    OnClick:=@CancelButtonClick;
  end;
end;

constructor TMakeResStrDialog.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  if LazarusResources.Find(Classname)=nil then begin
    Name:='MakeResStrDialog';
    Caption := 'Make ResourceStrings';
    Width:=550;
    Height:=400;
    Position:=poScreenCenter;
    OnResize:=@MakeResStrDialogResize;
    SetupComponents;
  end;
  IDEDialogLayoutList.ApplyLayout(Self,550,400);
  OnResize(nil);
  EditorOpts.GetHighlighterSettings(SynPasSyn);
  EditorOpts.GetSynEditSettings(StringConstSynEdit);
  StringConstSynEdit.ReadOnly:=true;
  EditorOpts.GetSynEditSettings(SrcPreviewSynEdit);
  SrcPreviewSynEdit.ReadOnly:=true;
  EditorOpts.GetSynEditSettings(ResStrPreviewSynEdit);
  ResStrPreviewSynEdit.ReadOnly:=true;
end;

destructor TMakeResStrDialog.Destroy;
begin
  inherited Destroy;
end;

procedure TMakeResStrDialog.FillResourceStringSections(
  Positions: TCodeXYPositions);
var
  i: Integer;
  p: PCodeXYPosition;
  s: String;
begin
  with ResStrSectionComboBox do begin
    Items.BeginUpdate;
    for i:=0 to Positions.Count-1 do begin
      p:=Positions[i];
      s:=p^.Code.Filename+' ('+IntToStr(p^.Y)+','+IntToStr(p^.X)+')';
      if i<Items.Count then
        Items[i]:=s
      else
        Items.Add(s);
    end;
    while Items.Count>Positions.Count do
      Items.Delete(Items.Count-1);
    Items.EndUpdate;
  end;
end;

end.

