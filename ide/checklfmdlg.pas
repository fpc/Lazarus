{
 /***************************************************************************
                            checklfmdlg.pas
                            ---------------

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
 *   Inc., 51 Franklin Street - Fifth Floor, Boston, MA 02110-1335, USA.   *
 *                                                                         *
 ***************************************************************************
}
unit CheckLFMDlg;

{$mode objfpc}{$H+}

interface

uses
  // FCL
  Classes,
  // LCL
  Forms, Controls, Buttons, StdCtrls, ExtCtrls,
  // CodeTools
  LFMTrees,
  // SynEdit
  SynHighlighterLFM, SynEdit, SynEditMiscClasses,
  // IDEIntf
  IdeIntfStrConsts, IDEWindowIntf, IDEImagesIntf,
  // IDE
  CheckerLFM, LazarusIDEStrConsts, EditorOptions, SourceMarks;

type
  { TCheckLFMDialog }

  TCheckLFMDialog = class(TForm)
    CancelButton: TBitBtn;
    ErrorsGroupBox: TGroupBox;
    ErrorsListBox: TListBox;
    NoteLabel: TLabel;
    LFMGroupBox: TGroupBox;
    LFMSynEdit: TSynEdit;
    BtnPanel: TPanel;
    RemoveAllButton: TBitBtn;
    SynLFMSyn1: TSynLFMSyn;
    procedure ErrorsListBoxClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var {%H-}CloseAction: TCloseAction);
    procedure LFMSynEditSpecialLineMarkup(Sender: TObject; Line: integer;
      var Special: boolean; AMarkup: TSynSelectedColor);
    procedure RemoveAllButtonClick(Sender: TObject);
    procedure CheckLFMDialogCREATE(Sender: TObject);
  private
    fLfmChecker: TLFMChecker;
    procedure SetupComponents;
  public
    constructor Create(AOwner: TComponent; ALfmChecker: TLFMChecker); reintroduce;
    destructor Destroy; override;
  end;


implementation

{$R *.lfm}

{ TCheckLFMDialog }

constructor TCheckLFMDialog.Create(AOwner: TComponent; ALfmChecker: TLFMChecker);
begin
  inherited Create(AOwner);
  fLfmChecker:=ALfmChecker;
end;

destructor TCheckLFMDialog.Destroy;
begin
  inherited Destroy;
end;

procedure TCheckLFMDialog.CheckLFMDialogCREATE(Sender: TObject);
begin
  Caption:=lisFixLFMFile;
  Position:=poScreenCenter;
  IDEDialogLayoutList.ApplyLayout(Self,600,400);
  SetupComponents;
end;

procedure TCheckLFMDialog.RemoveAllButtonClick(Sender: TObject);
begin
  ModalResult:=fLfmChecker.RemoveAll;
end;

procedure TCheckLFMDialog.ErrorsListBoxClick(Sender: TObject);
begin
  fLfmChecker.JumpToError(fLfmChecker.FindListBoxError);
end;

procedure TCheckLFMDialog.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  IDEDialogLayoutList.SaveLayout(Self);
end;

procedure TCheckLFMDialog.LFMSynEditSpecialLineMarkup(Sender: TObject;
  Line: integer; var Special: boolean; AMarkup: TSynSelectedColor);
var
  CurError: TLFMError;
begin
  CurError:=fLfmChecker.LFMTree.FindErrorAtLine(Line);
  if CurError = nil then Exit;
  Special := True;
  EditorOpts.SetMarkupColor(SynLFMSyn1, ahaErrorLine, AMarkup);
end;

procedure TCheckLFMDialog.SetupComponents;
begin
  NoteLabel.Caption:=lisTheLFMLazarusFormFileContainsInvalidPropertiesThis;
  ErrorsGroupBox.Caption:=lisErrors;
  LFMGroupBox.Caption:=lisLFMFile;
  RemoveAllButton.Caption:=lisRemoveAllInvalidProperties;
  IDEImages.AssignImage(RemoveAllButton, 'laz_delete');
  CancelButton.Caption:=lisCancel;
  EditorOpts.GetHighlighterSettings(SynLFMSyn1);
  EditorOpts.GetSynEditSettings(LFMSynEdit);
end;


end.

