{
/***************************************************************************
                            ClipBoardHistory.pas
                            --------------------

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

  Author: Mattias Gaertner

  Abstract:
    The TClipBoardHistory form is a frontend for the clipboard history, which
    stores the texts of the last "copy to clipboard" actions.

  ToDo:
    Everything.
}
unit ClipBoardHistory;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Buttons, SynEdit, EditorOptions, StdCtrls,
  IDEOptionDefs, Math, EnvironmentOpts, ClipBrd, LazarusIDEStrConsts;
  
type
  TClipBoardHistory = class(TForm)
    CopyToIDEBitBtn: TBitBtn;
    ClearBitBtn: TBitBtn;
    PasteFromClipboardBitBtn: TBitBtn;
    ItemsListBox: TListBox;
    PreviewSynEdit: TSynEdit;
    procedure ClipBoardHistoryResize(Sender: TObject);
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
  end;

implementation

{ TClipBoardHistory }

procedure TClipBoardHistory.ClipBoardHistoryResize(Sender: TObject);
begin
  with CopyToIDEBitBtn do begin
    Left:=0;
    Top:=0;
  end;

  with ClearBitBtn do begin
    Left:=CopyToIDEBitBtn.Left+CopyToIDEBitBtn.Width;
    Top:=0;
  end;

  with PasteFromClipboardBitBtn do begin
    Left:=ClearBitBtn.Left+ClearBitBtn.Width;
    Top:=0;
  end;

  with ItemsListBox do begin
    Left:=0;
    Top:=CopyToIDEBitBtn.Top+CopyToIDEBitBtn.Height;
    Width:=Self.ClientWidth;
    Height:=Max(Self.ClientHeight-Top-100,30);
  end;

  with PreviewSynEdit do begin
    Left:=0;
    Top:=ItemsListBox.Top+ItemsListBox.Height;
    Width:=Self.ClientWidth;
    Height:=Self.ClientHeight-Top;
  end;
end;

constructor TClipBoardHistory.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  
  Name:=NonModalIDEWindowNames[nmiwClipbrdHistory];
  Caption := 'Clipboard History';

  CopyToIDEBitBtn:=TBitBtn.Create(Self);
  with CopyToIDEBitBtn do begin
    Name:='CopyToIDEBitBtn';
    Parent:=Self;
    Left:=0;
    Top:=0;
    Caption:='Copy to IDE';
  end;
  
  ClearBitBtn:=TBitBtn.Create(Self);
  with ClearBitBtn do begin
    Name:='ClearBitBtn';
    Parent:=Self;
    Left:=CopyToIDEBitBtn.Left+CopyToIDEBitBtn.Width;
    Top:=0;
    Caption:='Clear all';
  end;
  
  PasteFromClipboardBitBtn:=TBitBtn.Create(Self);
  with PasteFromClipboardBitBtn do begin
    Name:='PasteFromClipboardBitBtn';
    Parent:=Self;
    Left:=ClearBitBtn.Left+ClearBitBtn.Width;
    Top:=0;
    Caption:='Paste from Clipboard';
  end;
  
  ItemsListBox:=TListBox.Create(Self);
  with ItemsListBox do begin
    Name:='ItemsListBox';
    Parent:=Self;
    Left:=0;
    Top:=CopyToIDEBitBtn.Top+CopyToIDEBitBtn.Height;
    Width:=Self.ClientWidth;
    Height:=Max(Self.ClientHeight-Top-100,30);
  end;
  
  PreviewSynEdit:=TSynEdit.Create(Self);
  with PreviewSynEdit do begin
    Name:='PreviewSynEdit';
    Parent:=Self;
    Left:=0;
    Top:=ItemsListBox.Top+ItemsListBox.Height;
    Width:=Self.ClientWidth;
    Height:=Self.ClientHeight-Top;
  end;
  
  OnResize:=@ClipBoardHistoryResize;
end;

destructor TClipBoardHistory.Destroy;
begin
  inherited Destroy;
end;

end.

