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
}
unit EditMsgScannersDlg;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LCLProc, Forms, Controls, Graphics, Dialogs,
  ButtonPanel, StdCtrls, ExtCtrls, Buttons, LazarusIDEStrConsts,
  IDEMsgIntf;

type

  { TEditMsgScannersDialog }

  TEditMsgScannersDialog = class(TForm)
    ButtonPanel: TButtonPanel;
    AvailableGroupBox: TGroupBox;
    AvailableListBox: TListBox;
    ScannersListBox: TListBox;
    MoveBtnPanel: TPanel;
    ScannersGroupBox: TGroupBox;
    AddSpeedButton: TSpeedButton;
    RemoveSpeedButton: TSpeedButton;
    Splitter1: TSplitter;
    procedure AddSpeedButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure RemoveSpeedButtonClick(Sender: TObject);
  private
    FScanners: TStrings;
    procedure SetScanners(const AValue: TStrings);
    procedure UpdateButtons;
  public
    procedure FillAvailableListbox;
    procedure FillScannersListbox;
    function IndexOfUsedScanner(const ScannerName: string): integer;
    property Scanners: TStrings read FScanners write SetScanners;
  end;

function ShowEditMsgScannersDialog(const Title: string;
  Scanners: TStrings): TModalResult;


implementation

{$R *.lfm}

function ShowEditMsgScannersDialog(const Title: string;
  Scanners: TStrings): TModalResult;
var
  EditMsgScannersDialog: TEditMsgScannersDialog;
begin
  EditMsgScannersDialog:=TEditMsgScannersDialog.Create(nil);
  try
    EditMsgScannersDialog.Scanners:=Scanners;
    EditMsgScannersDialog.Caption:=Title;
    Result:=EditMsgScannersDialog.ShowModal;
    if Result=mrOk then
      Scanners.Assign(EditMsgScannersDialog.Scanners);
  finally
    EditMsgScannersDialog.Free;
  end;
end;

{ TEditMsgScannersDialog }

procedure TEditMsgScannersDialog.FormCreate(Sender: TObject);
begin
  FScanners:=TStringList.Create;
  
  AvailableGroupBox.Caption:=rsAvailableScanners;
  ScannersGroupBox.Caption:=rsScanners;
  AddSpeedButton.Hint:=lisCodeTemplAdd;
  RemoveSpeedButton.Hint:=lisExtToolRemove;

  AddSpeedButton.LoadGlyphFromLazarusResource('arrow_left');
  RemoveSpeedButton.LoadGlyphFromLazarusResource('arrow_right');

  FillAvailableListbox;
end;

procedure TEditMsgScannersDialog.AddSpeedButtonClick(Sender: TObject);
var
  i: LongInt;
  ShortDesc: string;
  Scanner: TIDEMsgScannerType;
  HasChanged: Boolean;
begin
  HasChanged:=false;
  for i:=0 to AvailableListBox.Items.Count-1 do begin
    if not AvailableListBox.Selected[i] then continue;
    ShortDesc:=AvailableListBox.Items[i];
    Scanner:=IDEMsgScanners.TypeOfShortDesc(ShortDesc);
    if Scanner=nil then continue;
    if IndexOfUsedScanner(Scanner.Name)>=0 then continue;
    HasChanged:=true;
    FScanners.Add(Scanner.Name);
  end;
  if HasChanged then begin
    FillAvailableListbox;
    FillScannersListbox;
    UpdateButtons;
  end;
end;

procedure TEditMsgScannersDialog.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FScanners);
end;

procedure TEditMsgScannersDialog.RemoveSpeedButtonClick(Sender: TObject);
var
  i: Integer;
  HasChanged: Boolean;
begin
  HasChanged:=false;
  for i:=ScannersListBox.Items.Count-1 downto 0 do begin
    if not ScannersListBox.Selected[i] then continue;
    if i>=FScanners.Count then continue;
    HasChanged:=true;
    FScanners.Delete(i);
  end;
  if HasChanged then begin
    FillAvailableListbox;
    FillScannersListbox;
    UpdateButtons;
  end;
end;

procedure TEditMsgScannersDialog.SetScanners(const AValue: TStrings);
begin
  if FScanners=AValue then exit;
  FScanners.Assign(AValue);
  FillScannersListbox;
  FillAvailableListbox;
  UpdateButtons;
end;

procedure TEditMsgScannersDialog.UpdateButtons;
begin
  AddSpeedButton.Enabled:=AvailableListBox.SelCount>=0;
  RemoveSpeedButton.Enabled:=ScannersListBox.SelCount>=0;
end;

procedure TEditMsgScannersDialog.FillAvailableListbox;
var
  sl: TStringList;
  i: Integer;
begin
  sl:=TStringList.Create;
  for i:=0 to IDEMsgScanners.Count-1 do begin
    if IndexOfUsedScanner(IDEMsgScanners[i].Name)<0 then
      sl.Add(IDEMsgScanners[i].ShortDescription);
  end;
  sl.Sort;
  AvailableListBox.Items.Assign(sl);
  sl.Free;
end;

procedure TEditMsgScannersDialog.FillScannersListbox;
var
  sl: TStringList;
  i: Integer;
  Scanner: TIDEMsgScannerType;
  s: string;
begin
  sl:=TStringList.Create;
  for i:=0 to FScanners.Count-1 do begin
    s:=FScanners[i];
    Scanner:=IDEMsgScanners.TypeOfName(s);
    if Scanner<>nil then
      s:=Scanner.ShortDescription;
    sl.Add(s);
  end;
  ScannersListBox.Items.Assign(sl);
  sl.Free;
end;

function TEditMsgScannersDialog.IndexOfUsedScanner(const ScannerName: string
  ): integer;
begin
  Result:=FScanners.Count-1;
  while (Result>=0) do begin
    if SysUtils.CompareText(ScannerName,FScanners[Result])=0 then exit;
    dec(Result);
  end;
end;

end.

