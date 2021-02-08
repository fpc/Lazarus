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
 *   Inc., 51 Franklin Street - Fifth Floor, Boston, MA 02110-1335, USA.   *
 *                                                                         *
 ***************************************************************************
}
unit RegistersDlg;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, Types,
  // LCL
  Controls, Forms, Clipbrd, ComCtrls, ActnList, Menus, Grids,
  // LazUtils
  LazUTF8,
  // IdeIntf
  IDEWindowIntf, IDEImagesIntf,
  // DebuggerIntf
  DbgIntfDebuggerBase,
  // IDE
  BaseDebugManager, LazarusIDEStrConsts, DebuggerStrConst, Debugger, DebuggerDlg;

type

  { TStringGridAllowRightMouse }

  TStringGridAllowRightMouse = class(TStringGrid)
  protected
    FAllowRightButton: Boolean;
    function MouseButtonAllowed(Button: TMouseButton): boolean; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
  end;

  { TRegistersDlg }

  TRegistersDlg = class(TDebuggerDlg)
    actCopyName: TAction;
    actCopyValue: TAction;
    actCopyNameValue: TAction;
    actCopyAll: TAction;
    actPower: TAction;
    ActionList1: TActionList;
    ImageList1: TImageList;
    lvRegisters: TStringGridAllowRightMouse;
    DispDefault: TMenuItem;
    DispHex: TMenuItem;
    DispBin: TMenuItem;
    DispOct: TMenuItem;
    DispDec: TMenuItem;
    DispRaw: TMenuItem;
    popCopyAll: TMenuItem;
    popCopyNameValue: TMenuItem;
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
    procedure actCopyAllExecute(Sender: TObject);
    procedure actCopyNameExecute(Sender: TObject);
    procedure actCopyNameValueExecute(Sender: TObject);
    procedure actCopyValueExecute(Sender: TObject);
    procedure actPowerExecute(Sender: TObject);
    procedure DispDefaultClick(Sender: TObject);
    procedure lvRegistersDrawCell(Sender: TObject; aCol, aRow: Integer;
      aRect: TRect; {%H-}aState: TGridDrawState);
    procedure lvRegistersSelection(Sender: TObject; {%H-}aCol, {%H-}aRow: Integer);
    procedure ToolButtonDispTypeClick(Sender: TObject);
    function GetCurrentRegisters: TRegisters;
  private
    FNeedUpdateAgain: Boolean;
    FPowerImgIdx, FPowerImgIdxGrey: Integer;
    procedure RegistersChanged(Sender: TObject);
  protected
    procedure DoRegistersChanged; override;
    procedure DoBeginUpdate; override;
    procedure DoEndUpdate; override;
    function  ColSizeGetter(AColId: Integer; var ASize: Integer): Boolean;
    procedure ColSizeSetter(AColId: Integer; ASize: Integer);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    property RegistersMonitor;
    property ThreadsMonitor;
    property CallStackMonitor;
    //property SnapshotManager;
  end;


implementation

{$R *.lfm}

var
  RegisterDlgWindowCreator: TIDEWindowCreator;

const
  COL_REGISTER_NAME   = 1;
  COL_REGISTER_VALUE  = 2;
  COL_WIDTHS: Array[0..2] of integer = (18, 50, 350);

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

{ TStringGridAllowRightMouse }

function TStringGridAllowRightMouse.MouseButtonAllowed(Button: TMouseButton
  ): boolean;
begin
  Result := inherited MouseButtonAllowed(Button);
  if (not Result) and (Button = mbRight) then
    Result := FAllowRightButton;
end;

procedure TStringGridAllowRightMouse.MouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  p: TPoint;
begin
  FAllowRightButton := False;
  if Button = mbRight then begin
    p := MouseToCell(Point(X,Y));
    FAllowRightButton := not IsCellSelected[p.X, p.Y];
  end;
  inherited MouseDown(Button, Shift, X, Y);
end;

{ TRegistersDlg }

constructor TRegistersDlg.Create(AOwner: TComponent);
var
  i: Integer;
begin
  inherited Create(AOwner);
  ThreadsNotification.OnCurrent   := @RegistersChanged;
  CallstackNotification.OnCurrent := @RegistersChanged;
  RegistersNotification.OnChange  := @RegistersChanged;

  Caption:= lisRegisters;
  lvRegisters.Columns[1].Title.Caption:= lisName;
  lvRegisters.Columns[2].Title.Caption:= lisValue;
  lvRegisters.RangeSelectMode := rsmMulti;

  ActionList1.Images := IDEImages.Images_16;
  ToolBar1.Images := IDEImages.Images_16;

  FPowerImgIdx := IDEImages.LoadImage('debugger_power');
  FPowerImgIdxGrey := IDEImages.LoadImage('debugger_power_grey');
  actPower.ImageIndex := FPowerImgIdx;
  //actPower.Caption := lisDbgWinPower;
  actPower.Hint := lisDbgWinPowerHint;

  actCopyName.Caption := lisLocalsDlgCopyName;
  actCopyValue.Caption := lisLocalsDlgCopyValue;
  actCopyNameValue.Caption := lisLocalsDlgCopyNameValue;
  actCopyAll.Caption := lisLocalsDlgCopyAll;

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

  for i := low(COL_WIDTHS) to high(COL_WIDTHS) do
    lvRegisters.Columns[i].Width := COL_WIDTHS[i];
end;

destructor TRegistersDlg.Destroy;
begin
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
var
  s: String;
  i: Integer;
begin
  s := '';
  for i := 1 to lvRegisters.RowCount - 1 do
    if lvRegisters.IsCellSelected[0,i] then begin
      if s <> '' then
        s := s + LineEnding;
      s := s + lvRegisters.Cells[1, i];
    end;
  Clipboard.Open;
  Clipboard.AsText := s;
  Clipboard.Close;
end;

procedure TRegistersDlg.actCopyValueExecute(Sender: TObject);
var
  s: String;
  i: Integer;
begin
  s := '';
  for i := 1 to lvRegisters.RowCount - 1 do
    if lvRegisters.IsCellSelected[0,i] then begin
      if s <> '' then
        s := s + LineEnding;
      s := s + lvRegisters.Cells[2,i];
    end;
  Clipboard.Open;
  Clipboard.AsText := s;
  Clipboard.Close;
end;

procedure TRegistersDlg.actCopyNameValueExecute(Sender: TObject);
var
  s: String;
  i: Integer;
begin
  s := '';
  for i := 1 to lvRegisters.RowCount - 1 do
    if lvRegisters.IsCellSelected[0,i] then begin
      if s <> '' then
        s := s + LineEnding;
      s := s + lvRegisters.Cells[1, i] + '=' + lvRegisters.Cells[2,i];
    end;
  Clipboard.Open;
  Clipboard.AsText := s;
  Clipboard.Close;
end;

procedure TRegistersDlg.actCopyAllExecute(Sender: TObject);
var
  s: String;
  i: Integer;
begin
  s := '';
  for i := 1 to lvRegisters.RowCount - 1 do
    s := Concat(s, lvRegisters.Cells[1, i], '=', lvRegisters.Cells[2,i], sLineBreak);
  Clipboard.Open;
  Clipboard.AsText := s;
  Clipboard.Close;
end;

procedure TRegistersDlg.DispDefaultClick(Sender: TObject);
var
  n: Integer;
  Reg: TRegisters;
  RegVal: TRegisterValue;
begin
  ToolButtonPower.Down := True;
  Reg := GetCurrentRegisters;
  if Reg = nil then exit;

  for n := 1 to lvRegisters.RowCount - 1 do
    if lvRegisters.IsCellSelected[0,n] then begin
      RegVal := Reg.EntriesByName[lvRegisters.Cells[1,n]];
      if RegVal <> nil then
        RegVal.DisplayFormat := TRegisterDisplayFormat(TMenuItem(Sender).Tag);
    end;
  lvRegistersSelection(nil, -1, -1);
end;

procedure TRegistersDlg.lvRegistersDrawCell(Sender: TObject; aCol,
  aRow: Integer; aRect: TRect; aState: TGridDrawState);
begin
  if (aCol = 0) and (aRow > 0) and
     (lvRegisters.Objects[0, aRow] <> nil)
  then begin
    ImageList1.Draw(lvRegisters.Canvas, (aRect.Left + aRect.Right - ImageList1.Width) div 2,
                                        (aRect.Top + aRect.Bottom - ImageList1.Height) div 2, 0);
  end;
end;

procedure TRegistersDlg.lvRegistersSelection(Sender: TObject; aCol,
  aRow: Integer);
var
  n, j: Integer;
  SelFormat: TRegisterDisplayFormat;
  MultiFormat: Boolean;
  Reg: TRegisters;
  RegVal: TRegisterValue;
begin
  j := 0;
  MultiFormat := False;
  SelFormat := rdDefault;
  Reg := GetCurrentRegisters;
  if Reg = nil then exit;

  for n := 1 to lvRegisters.RowCount - 1 do
    if lvRegisters.IsCellSelected[0,n] then begin
      RegVal := Reg.EntriesByName[lvRegisters.Cells[1,n]];
      if RegVal <> nil then begin
        if j = 0
        then SelFormat := RegVal.DisplayFormat;
        inc(j);
        if SelFormat <> RegVal.DisplayFormat then begin
          MultiFormat := True;
          break;
        end;
      end;
    end;
  ToolButtonDispType.Enabled := j > 0;
  popFormat.Enabled := j > 0;
  actCopyName.Enabled := j > 0;
  actCopyValue.Enabled := j > 0;
  actCopyNameValue.Enabled := j > 0;
  actCopyAll.Enabled := lvRegisters.RowCount > 1;

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

function TRegistersDlg.GetCurrentRegisters: TRegisters;
var
  CurThreadId, CurStackFrame: Integer;
begin
  Result := nil;
  if (ThreadsMonitor = nil) or
     (ThreadsMonitor.CurrentThreads = nil) or
     (CallStackMonitor = nil) or
     (CallStackMonitor.CurrentCallStackList = nil) or
     (RegistersMonitor = nil)
  then
    exit;

  CurThreadId := ThreadsMonitor.CurrentThreads.CurrentThreadId;
  if (CallStackMonitor.CurrentCallStackList.EntriesForThreads[CurThreadId] = nil) then
    exit;

  CurStackFrame := CallStackMonitor.CurrentCallStackList.EntriesForThreads[CurThreadId].CurrentIndex;
  Result := RegistersMonitor.CurrentRegistersList[CurThreadId, CurStackFrame];
end;

procedure TRegistersDlg.RegistersChanged(Sender: TObject);
var
  n, i, idx, Cnt: Integer;
  List: TStringListUTF8Fast;
  Reg: TRegisters;
begin
  if (not ToolButtonPower.Down) then exit;

  if IsUpdating then begin
    FNeedUpdateAgain := True;
    exit;
  end;
  FNeedUpdateAgain := False;

  BeginUpdate;
  try
    Reg := GetCurrentRegisters;
    if (Reg = nil) or (reg.DataValidity<> ddsValid) then begin
      if (DebugBoss = nil) or not (DebugBoss.State in [dsPause, dsInternalPause, dsRun]) then
        lvRegisters.RowCount := 1;

      if (reg <> nil) then
        reg.Count;
      for n := 1 to lvRegisters.RowCount - 1 do
        lvRegisters.Cells[2, n] := '<Unavailable>';
      exit;
    end;

    List := TStringListUTF8Fast.Create;
    try
      //Get existing items
      for n := 1 to lvRegisters.RowCount - 1 do
        List.AddObject(lvRegisters.Cells[1,n], TObject(PtrUInt(n)));

      // add/update entries
      Cnt := Reg.Count;          // Count may trigger changes
      FNeedUpdateAgain := False; // changes after this point, and we must update again

      for n := 0 to Cnt - 1 do
      begin
        idx := List.IndexOf(Reg[n].Name);
        if idx = -1
        then begin
          // New entry
          i := lvRegisters.RowCount;
          lvRegisters.RowCount := i + 1;
          lvRegisters.Cells[1, i] := Reg[n].Name;
          lvRegisters.Cells[2, i] := Reg[n].Value;
        end
        else begin
          // Existing entry
          i := PtrUInt(List.Objects[idx]);
          List.Delete(idx);
          lvRegisters.Cells[1, i] := Reg[n].Name;
          lvRegisters.Cells[2, i] := Reg[n].Value;
        end;
        if Reg[n].Modified
        then lvRegisters.Objects[0, i] := TObject(ptruint(1)) //Item.ImageIndex := 0
        else lvRegisters.Objects[0, i] := nil;  //Item.ImageIndex := -1;
      end;

      // remove obsolete entries
      for n := List.Count - 1 downto 0 do
        lvRegisters.DeleteRow(PtrUInt(List.Objects[n]));
      lvRegisters.Invalidate;

    finally
      List.Free;
    end;
  finally
    EndUpdate;
  end;

  lvRegistersSelection(nil, -1, -1);
end;

procedure TRegistersDlg.DoRegistersChanged;
begin
  RegistersChanged(nil);
end;

procedure TRegistersDlg.DoBeginUpdate;
begin
  {$IFnDEF WINDOWS}
  lvRegisters.BeginUpdate;
  {$ENDIF}
end;

procedure TRegistersDlg.DoEndUpdate;
begin
  {$IFnDEF WINDOWS}
  lvRegisters.EndUpdate;
  {$ENDIF}
  if FNeedUpdateAgain then RegistersChanged(nil);
end;

function TRegistersDlg.ColSizeGetter(AColId: Integer; var ASize: Integer): Boolean;
begin
  if (AColId >= 0) and (AColId < lvRegisters.Columns.Count) then begin
    ASize := lvRegisters.Columns[AColId].Width;
    Result := ASize <> COL_WIDTHS[AColId];
  end
  else
    Result := False;
end;

procedure TRegistersDlg.ColSizeSetter(AColId: Integer; ASize: Integer);
begin
  case AColId of
    COL_REGISTER_NAME:   lvRegisters.Columns[1].Width := ASize;
    COL_REGISTER_VALUE:  lvRegisters.Columns[2].Width := ASize;
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

