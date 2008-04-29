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
  Classes, SysUtils, LCLProc, LResources, Forms, Controls, Graphics, Dialogs,
  ButtonPanel, StdCtrls, ExtCtrls, Buttons,
  IDEMsgIntf;

type

  { TEditMsgScannersDialog }

  TEditMsgScannersDialog = class(TForm)
    ButtonPanel1: TButtonPanel;
    AvailableGroupBox: TGroupBox;
    AvailableListBox: TListBox;
    ScannersListBox: TListBox;
    MoveBtnPanel: TPanel;
    ScannersGroupBox: TGroupBox;
    AddSpeedButton: TSpeedButton;
    RemoveSpeedButton: TSpeedButton;
    Splitter1: TSplitter;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    FScanners: TStrings;
    procedure SetScanners(const AValue: TStrings);
  public
    procedure FillAvailableListbox;
    function IndexOfUsedScanner(const ScannerName: string): integer;
    property Scanners: TStrings read FScanners write SetScanners;
  end;

function ShowEditMsgScannersDialog(Scanners: TStrings): TModalResult;


implementation


function ShowEditMsgScannersDialog(Scanners: TStrings): TModalResult;
var
  EditMsgScannersDialog: TEditMsgScannersDialog;
begin
  EditMsgScannersDialog:=TEditMsgScannersDialog.Create(nil);
  try
    EditMsgScannersDialog.Scanners:=Scanners;
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
end;

procedure TEditMsgScannersDialog.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FScanners);
end;

procedure TEditMsgScannersDialog.SetScanners(const AValue: TStrings);
begin
  if FScanners=AValue then exit;
  FScanners.Assign(AValue);
  ScannersListBox.Items.Assign(FScanners);
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

function TEditMsgScannersDialog.IndexOfUsedScanner(const ScannerName: string
  ): integer;
begin
  Result:=FScanners.Count;
  while (Result>=0) do begin
    if SysUtils.CompareText(ScannerName,FScanners[Result])=0 then exit;
    dec(Result);
  end;
end;

initialization
  {$I editmsgscannersdlg.lrs}

end.

