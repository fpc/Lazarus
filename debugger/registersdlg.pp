{ $Id$ }
{               ----------------------------------------------  
                 registersdlg.pp  -  Overview of registers 
                ---------------------------------------------- 
 
 @created(Sun Nov 16th WET 2008)
 @lastmod($Date$)
 @author(Marc Weustink <marc@@dommelstein.net>)                       

 This unit contains the registers debugger dialog.
 
 
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
}
unit RegistersDlg;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, Controls, Forms, Clipbrd,
  BaseDebugManager, IDEWindowIntf, DebuggerStrConst,
  ComCtrls, ActnList, Menus, Debugger, DebuggerDlg,
  LazarusIDEStrConsts, IDEImagesIntf, DbgIntfDebuggerBase;

type

  { TRegistersDlg }

  TRegistersDlg = class(TDebuggerDlg)
    actCopyName: TAction;
    actCopyValue: TAction;
    actPower: TAction;
    ActionList1: TActionList;
    ImageList1: TImageList;
    lvRegisters: TListView;
    DispDefault: TMenuItem;
    DispHex: TMenuItem;
    DispBin: TMenuItem;
    DispOct: TMenuItem;
    DispDec: TMenuItem;
    DispRaw: TMenuItem;
    PopDispDefault: TMenuItem;
    PopDispHex: TMenuItem;
    PopDispBin: TMenuItem;
    PopDispOct: TMenuItem;
    PopDispDec: TMenuItem;
    PopDispRaw: TMenuItem;
    popCopyValue: TMenuItem;
    popCopyName: TMenuItem;
    popFormat: TMenuItem;
    popL1: TMenuItem;
    PopupDispType: TPopupMenu;
    PopupMenu1: TPopupMenu;
    ToolBar1: TToolBar;
    ToolButton1: TToolButton;
    ToolButtonDispType: TToolButton;
    ToolButtonPower: TToolButton;
    procedure actCopyNameExecute(Sender: TObject);
    procedure actCopyValueExecute(Sender: TObject);
    procedure actPowerExecute(Sender: TObject);
    procedure DispDefaultClick(Sender: TObject);
    procedure lvRegistersSelectItem(Sender: TObject; Item: TListItem; Selected: Boolean);
    procedure ToolButtonDispTypeClick(Sender: TObject);
  private
    FRegisters: TIDERegisters;
    FRegistersNotification: TIDERegistersNotification;
    FPowerImgIdx, FPowerImgIdxGrey: Integer;
    procedure RegistersChanged(Sender: TObject);
    procedure SetRegisters(const AValue: TIDERegisters);
    function IndexOfName(AName: String): Integer;
  protected
    procedure DoBeginUpdate; override;
    procedure DoEndUpdate; override;
    function  ColSizeGetter(AColId: Integer; var ASize: Integer): Boolean;
    procedure ColSizeSetter(AColId: Integer; ASize: Integer);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    property Registers: TIDERegisters read FRegisters write SetRegisters;
  end;


implementation

{$R *.lfm}

var
  RegisterDlgWindowCreator: TIDEWindowCreator;

const
  COL_REGISTER_NAME   = 1;
  COL_REGISTER_VALUE  = 2;
  COL_WIDTHS: Array[0..1] of integer = ( 150, 50);

function RegisterDlgColSizeGetter(AForm: TCustomForm; AColId: Integer; var ASize: Integer): Boolean;
begin
  Result := AForm is TRegistersDlg;
  if Result then
    Result := TRegistersDlg(AForm).ColSizeGetter(AColId, ASize);
end;

procedure RegisterDlgColSizeSetter(AForm: TCustomForm; AColId: Integer; ASize: Integer);
begin
  if AForm is TRegistersDlg then
    TRegistersDlg(AForm).ColSizeSetter(AColId, ASize);
end;

{ TRegistersDlg }

constructor TRegistersDlg.Create(AOwner: TComponent);
var
  i: Integer;
begin
  inherited Create(AOwner);
  FRegistersNotification := TIDERegistersNotification.Create;
  FRegistersNotification.AddReference;
  FRegistersNotification.OnChange := @RegistersChanged;
  Caption:= lisRegisters;
  lvRegisters.Columns[0].Caption:= lisName;
  lvRegisters.Columns[1].Caption:= lisValue;

  ActionList1.Images := IDEImages.Images_16;
  ToolBar1.Images := IDEImages.Images_16;

  FPowerImgIdx := IDEImages.LoadImage(16, 'debugger_power');
  FPowerImgIdxGrey := IDEImages.LoadImage(16, 'debugger_power_grey');
  actPower.ImageIndex := FPowerImgIdx;
  //actPower.Caption := lisDbgWinPower;
  actPower.Hint := lisDbgWinPowerHint;

  actCopyName.Caption := lisLocalsDlgCopyName;
  actCopyValue.Caption := lisLocalsDlgCopyValue;

  ToolButtonDispType.Hint := regdlgDisplayTypeForSelectedRegisters;

  DispDefault.Caption := dlgPasStringKeywordsOptDefault;
  DispHex.Caption := regdlgHex;
  DispBin.Caption := regdlgBinary;
  DispOct.Caption := regdlgOctal;
  DispDec.Caption := regdlgDecimal;
  DispRaw.Caption := regdlgRaw;
  DispDefault.Tag := ord(rdDefault);
  DispHex.Tag := ord(rdHex);
  DispBin.Tag := ord(rdBinary);
  DispOct.Tag := ord(rdOctal);
  DispDec.Tag := ord(rdDecimal);
  DispRaw.Tag := ord(rdRaw);

  PopDispDefault.Caption := dlgPasStringKeywordsOptDefault;
  PopDispHex.Caption := regdlgHex;
  PopDispBin.Caption := regdlgBinary;
  PopDispOct.Caption := regdlgOctal;
  PopDispDec.Caption := regdlgDecimal;
  PopDispRaw.Caption := regdlgRaw;
  PopDispDefault.Tag := ord(rdDefault);
  PopDispHex.Tag := ord(rdHex);
  PopDispBin.Tag := ord(rdBinary);
  PopDispOct.Tag := ord(rdOctal);
  PopDispDec.Tag := ord(rdDecimal);
  PopDispRaw.Tag := ord(rdRaw);

  popFormat.Caption := regdlgFormat;

  actCopyName.Caption := lisLocalsDlgCopyName;
  actCopyValue.Caption := lisLocalsDlgCopyValue;

  for i := low(COL_WIDTHS) to high(COL_WIDTHS) do
    lvRegisters.Column[i].Width := COL_WIDTHS[i];
end;

destructor TRegistersDlg.Destroy;
begin
  SetRegisters(nil);
  FRegistersNotification.OnChange := nil;
  FRegistersNotification.ReleaseReference;
  inherited Destroy;
end;

procedure TRegistersDlg.actPowerExecute(Sender: TObject);
begin
  if ToolButtonPower.Down
  then begin
    actPower.ImageIndex := FPowerImgIdx;
    ToolButtonPower.ImageIndex := FPowerImgIdx;
    RegistersChanged(nil);
  end
  else begin
    actPower.ImageIndex := FPowerImgIdxGrey;
    ToolButtonPower.ImageIndex := FPowerImgIdxGrey;
  end;
end;

procedure TRegistersDlg.actCopyNameExecute(Sender: TObject);
begin
  Clipboard.Open;
  Clipboard.AsText := lvRegisters.Selected.Caption;
  Clipboard.Close;
end;

procedure TRegistersDlg.actCopyValueExecute(Sender: TObject);
begin
  Clipboard.Open;
  Clipboard.AsText := lvRegisters.Selected.SubItems[0];
  Clipboard.Close;
end;

procedure TRegistersDlg.DispDefaultClick(Sender: TObject);
var
  n, i: Integer;
  Item: TListItem;
begin
  ToolButtonPower.Down := True;
  FRegisters.BeginUpdate;
  try
    for n := 0 to lvRegisters.Items.Count -1 do
    begin
      Item := lvRegisters.Items[n];
      if Item.Selected then begin
        i := IndexOfName(Item.Caption);
        if i >= 0
        then FRegisters.Formats[i] := TRegisterDisplayFormat(TMenuItem(Sender).Tag);
      end;
    end;
  finally
    FRegisters.EndUpdate;
  end;
  lvRegistersSelectItem(nil, nil, True);
end;

procedure TRegistersDlg.lvRegistersSelectItem(Sender: TObject; Item: TListItem;
  Selected: Boolean);
var
  n, i, j: Integer;
  SelFormat: TRegisterDisplayFormat;
  MultiFormat: Boolean;
begin
  j := 0;
  MultiFormat := False;
  SelFormat := rdDefault;
  for n := 0 to lvRegisters.Items.Count -1 do
  begin
    Item := lvRegisters.Items[n];
    if Item.Selected then begin
      i := IndexOfName(Item.Caption);
      if i >= 0 then begin
        if j = 0
        then SelFormat := FRegisters.Formats[i];
        inc(j);
        if SelFormat <> FRegisters.Formats[i] then begin
          MultiFormat := True;
          break;
        end;
      end;
    end;
  end;
  ToolButtonDispType.Enabled := j > 0;
  popFormat.Enabled := j > 0;
  actCopyName.Enabled := j > 0;
  actCopyValue.Enabled := j > 0;

  PopDispDefault.Checked := False;
  PopDispHex.Checked := False;
  PopDispBin.Checked := False;
  PopDispOct.Checked := False;
  PopDispDec.Checked := False;
  PopDispRaw.Checked := False;
  if MultiFormat
  then ToolButtonDispType.Caption := '...'
  else begin
    case SelFormat of
      rdDefault: begin
          ToolButtonDispType.Caption := DispDefault.Caption;
          PopDispDefault.Checked := True;
        end;
      rdHex:     begin
          ToolButtonDispType.Caption := DispHex.Caption;
          PopDispHex.Checked := True;
        end;
      rdBinary:  begin
          ToolButtonDispType.Caption := DispBin.Caption;
          PopDispBin.Checked := True;
        end;
      rdOctal:   begin
          ToolButtonDispType.Caption := DispOct.Caption;
          PopDispOct.Checked := True;
        end;
      rdDecimal: begin
          ToolButtonDispType.Caption := DispDec.Caption;
          PopDispDec.Checked := True;
        end;
      rdRaw:     begin
          ToolButtonDispType.Caption := DispRaw.Caption;
          PopDispRaw.Checked := True;
        end;
    end;
  end;
end;

procedure TRegistersDlg.ToolButtonDispTypeClick(Sender: TObject);
begin
  ToolButtonDispType.CheckMenuDropdown;
end;

procedure TRegistersDlg.RegistersChanged(Sender: TObject);
var
  n, idx: Integer;
  List: TStringList;
  Item: TListItem;
  S: String;
begin
  if (not ToolButtonPower.Down) then exit;

  List := TStringList.Create;
  try
    BeginUpdate;
    try
      if FRegisters = nil
      then begin
        lvRegisters.Items.Clear;
        Exit;
      end;
    
      //Get existing items
      for n := 0 to lvRegisters.Items.Count - 1 do
      begin
        Item := lvRegisters.Items[n];
        S := Item.Caption;
        S := UpperCase(S);
        List.AddObject(S, Item);
      end;

      // add/update entries
      for n := 0 to FRegisters.Count - 1 do
      begin
        idx := List.IndexOf(Uppercase(FRegisters.Names[n]));
        if idx = -1
        then begin
          // New entry
          Item := lvRegisters.Items.Add;
          Item.Caption := FRegisters.Names[n];
          Item.SubItems.Add(FRegisters.Values[n]);
        end
        else begin
          // Existing entry
          Item := TListItem(List.Objects[idx]);
          Item.SubItems[0] := FRegisters.Values[n];
          List.Delete(idx);
        end;
        if FRegisters.Modified[n]
        then Item.ImageIndex := 0
        else Item.ImageIndex := -1;
      end;

      // remove obsolete entries
      for n := 0 to List.Count - 1 do
        lvRegisters.Items.Delete(TListItem(List.Objects[n]).Index);

    finally
      EndUpdate;
    end;
  finally
    List.Free;
  end;
  lvRegistersSelectItem(nil, nil, True);
end;

procedure TRegistersDlg.SetRegisters(const AValue: TIDERegisters);
begin
  if FRegisters = AValue then Exit;

  BeginUpdate;
  try
    if FRegisters <> nil
    then begin
      FRegisters.RemoveNotification(FRegistersNotification);
    end;

    FRegisters := AValue;

    if FRegisters <> nil
    then begin
      FRegisters.AddNotification(FRegistersNotification);
    end;
    
    RegistersChanged(FRegisters);
  finally
    EndUpdate;
  end;
end;

function TRegistersDlg.IndexOfName(AName: String): Integer;
begin
  Result := FRegisters.Count - 1;
  while (Result >= 0) and (FRegisters.Names[Result] <> AName) do dec(Result);
end;

procedure TRegistersDlg.DoBeginUpdate;
begin
  lvRegisters.BeginUpdate;
end;

procedure TRegistersDlg.DoEndUpdate;
begin
  lvRegisters.EndUpdate;
end;

function TRegistersDlg.ColSizeGetter(AColId: Integer; var ASize: Integer): Boolean;
begin
  if (AColId - 1 >= 0) and (AColId - 1 < lvRegisters.ColumnCount) then begin
    ASize := lvRegisters.Column[AColId - 1].Width;
    Result := ASize <> COL_WIDTHS[AColId - 1];
  end
  else
    Result := False;
end;

procedure TRegistersDlg.ColSizeSetter(AColId: Integer; ASize: Integer);
begin
  case AColId of
    COL_REGISTER_NAME:   lvRegisters.Column[0].Width := ASize;
    COL_REGISTER_VALUE:  lvRegisters.Column[1].Width := ASize;
  end;
end;

initialization

  RegisterDlgWindowCreator := IDEWindowCreators.Add(DebugDialogNames[ddtRegisters]);
  RegisterDlgWindowCreator.OnCreateFormProc := @CreateDebugDialog;
  RegisterDlgWindowCreator.OnSetDividerSize := @RegisterDlgColSizeSetter;
  RegisterDlgWindowCreator.OnGetDividerSize := @RegisterDlgColSizeGetter;
  RegisterDlgWindowCreator.DividerTemplate.Add('RegisterName',  COL_REGISTER_NAME,  @drsColWidthName);
  RegisterDlgWindowCreator.DividerTemplate.Add('RegisterValue', COL_REGISTER_VALUE, @drsColWidthValue);
  RegisterDlgWindowCreator.CreateSimpleLayout;

end.

