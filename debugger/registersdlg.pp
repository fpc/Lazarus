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
  SysUtils, Classes, Controls, Forms,
  BaseDebugManager, IDEWindowIntf, DebuggerStrConst,
  ComCtrls, ActnList, Menus, Debugger, DebuggerDlg,
  LazarusIDEStrConsts, IDEImagesIntf;

type

  { TRegistersDlg }

  TRegistersDlg = class(TDebuggerDlg)
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
    PopupDispType: TPopupMenu;
    ToolBar1: TToolBar;
    ToolButton1: TToolButton;
    ToolButtonDispType: TToolButton;
    ToolButtonPower: TToolButton;
    procedure actPowerExecute(Sender: TObject);
    procedure DispDefaultClick(Sender: TObject);
    procedure lvRegistersMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X,
      Y: Integer);
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
begin
  inherited Create(AOwner);
  FRegistersNotification := TIDERegistersNotification.Create;
  FRegistersNotification.AddReference;
  FRegistersNotification.OnChange := @RegistersChanged;
  Caption:= lisRegisters;
  lvRegisters.Columns[0].Caption:= lisRegistersDlgName;
  lvRegisters.Columns[1].Caption:= lisRegistersDlgValue;

  ActionList1.Images := IDEImages.Images_16;
  ToolBar1.Images := IDEImages.Images_16;

  FPowerImgIdx := IDEImages.LoadImage(16, 'debugger_power');
  FPowerImgIdxGrey := IDEImages.LoadImage(16, 'debugger_power_grey');
  actPower.ImageIndex := FPowerImgIdx;
  //actPower.Caption := lisDbgWinPower;
  actPower.Hint := lisDbgWinPowerHint;

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

procedure TRegistersDlg.lvRegistersMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if Button = mbRight then
    PopupDispType.PopUp;
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
  if MultiFormat
  then ToolButtonDispType.Caption := '...'
  else begin
    case SelFormat of
      rdDefault: ToolButtonDispType.Caption := DispDefault.Caption;
      rdHex:     ToolButtonDispType.Caption := DispHex.Caption;
      rdBinary:  ToolButtonDispType.Caption := DispBin.Caption;
      rdOctal:   ToolButtonDispType.Caption := DispOct.Caption;
      rdDecimal: ToolButtonDispType.Caption := DispDec.Caption;
      rdRaw:     ToolButtonDispType.Caption := DispRaw.Caption;
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
  Result := True;
  case AColId of
    COL_REGISTER_NAME:   ASize := lvRegisters.Column[0].Width;
    COL_REGISTER_VALUE:  ASize := lvRegisters.Column[1].Width;
    else
      Result := False;
  end;
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
  RegisterDlgWindowCreator.DividerTemplate.Add('RegisterName',  COL_REGISTER_NAME,  drsColWidthName);
  RegisterDlgWindowCreator.DividerTemplate.Add('RegisterValue', COL_REGISTER_VALUE, drsColWidthValue);
  RegisterDlgWindowCreator.CreateSimpleLayout;

end.

