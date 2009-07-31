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
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, ButtonPanel, LazarusIDEStrConsts;

type
  TFPDocLinkCompletionItem = class
  public

  end;

  { TFPDocLinkCompletionList }

  TFPDocLinkCompletionList = class
  private
    FItems: TFPList; // list of TFPDocLinkCompletionItem
    FSelected: integer;
    FTop: integer;
    function GetCount: integer;
    procedure SetCount(const AValue: integer);
    procedure SetSelected(const AValue: integer);
    procedure SetTop(const AValue: integer);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    procedure Draw(Canvas: TCanvas);
    property Count: integer read GetCount;
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
    procedure FormCreate(Sender: TObject);
    procedure LinkEditChange(Sender: TObject);
    procedure LinkEditKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState
      );
  private
  public

  end;

function ShowFPDocLinkEditorDialog(out Link, LinkTitle: string): TModalResult;

implementation

function ShowFPDocLinkEditorDialog(out Link, LinkTitle: string): TModalResult;
var
  FPDocLinkEditorDlg: TFPDocLinkEditorDlg;
begin
  Link:='';
  LinkTitle:='';
  FPDocLinkEditorDlg:=TFPDocLinkEditorDlg.Create(nil);
  try
    Result:=FPDocLinkEditorDlg.ShowModal;
    if Result=mrOk then begin
      Link:=FPDocLinkEditorDlg.LinkEdit.Text;
      LinkTitle:=FPDocLinkEditorDlg.TitleEdit.Text;
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
end;

procedure TFPDocLinkEditorDlg.LinkEditChange(Sender: TObject);
begin

end;

procedure TFPDocLinkEditorDlg.LinkEditKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin

end;

{ TFPDocLinkCompletionList }

function TFPDocLinkCompletionList.GetCount: integer;
begin
  Result:=FItems.Count;
end;

procedure TFPDocLinkCompletionList.SetCount(const AValue: integer);
begin

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

procedure TFPDocLinkCompletionList.Draw(Canvas: TCanvas);
begin

end;

initialization
  {$I fpdocselectlink.lrs}

end.

