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
 *   Inc., 51 Franklin Street - Fifth Floor, Boston, MA 02110-1335, USA.   *
 *                                                                         *
 ***************************************************************************

 Author: Balázs Székely
 Abstract: Show hint for meta packages
}

unit opkman_showhint;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms,
  //LCL
  Controls, Graphics, Dialogs, ExtCtrls, Menus, LCLType,
  //Interface
  LCLIntf, StdCtrls,
  //OpkMan
  laz.virtualtrees;

type

  { TShowHintFrm }

  TShowHintFrm = class(TForm)
    mDescription: TMemo;
    pnDescription: TPanel;
    pnMain: TPanel;
    pnPackageName: TPanel;
    sbLazPackages: TScrollBox;
    tmWait: TTimer;
    procedure FormCreate(Sender: TObject);
    procedure FormDeactivate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormKeyUp(Sender: TObject; var Key: Word; {%H-}Shift: TShiftState);
    procedure tmWaitTimer(Sender: TObject);
  private
    FFrames: TList;
    {$IFDEF LclGtk2}FModalShowing: Boolean;{$ENDIF}
    function IsMouseOverForm: Boolean;
  public
    procedure ShowFormAt(const AX, AY: Integer);
    procedure MoveFormTo(const AX, AY: Integer);
    procedure UpdateInfo(ANode: PVirtualNode);
    procedure SetupTimer(const AInterval: Integer);
  end;

var
  ShowHintFrm: TShowHintFrm;

implementation

{$R *.lfm}

{ TShowHintFrm }

uses opkman_visualtree, opkman_serializablepackages, opkman_showhintbase,
     opkman_options, opkman_const {$IFDEF LclGtk2}, gtk2 {$ENDIF};

procedure TShowHintFrm.FormCreate(Sender: TObject);
begin
  sbLazPackages.DoubleBuffered := True;
  FFrames := TList.Create;
end;

procedure TShowHintFrm.FormDeactivate(Sender: TObject);
begin
  tmWait.Enabled := False;
  Hide;
end;

procedure TShowHintFrm.FormDestroy(Sender: TObject);
var
  I: Integer;
begin
  for I := FFrames.Count - 1 downto 0 do
    TfrShowHint(FFrames.Items[I]).Free;
  FFrames.Clear;
  FFrames.Free;
end;

procedure TShowHintFrm.FormKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_ESCAPE then
    Hide;
  if Key = VK_SHIFT then
    SetupTimer(300);
end;

function TShowHintFrm.IsMouseOverForm: Boolean;
var
  R: TRect;
  P: TPoint;
  CH: Integer;
begin
  Result := False;
  CH := LCLIntf.GetSystemMetrics(SM_CYCAPTION);
  R.Left := Left;
  R.Top := Top;
  R.Right := Left + Width + 15;
  R.Bottom := Top + Height + CH + 15;
  P.X := 0;
  P.Y := 0;
  GetCursorPos(P);
  Result := PtInRect(R, P);
end;

procedure TShowHintFrm.tmWaitTimer(Sender: TObject);
{$IFDEF LclGtk2}
var
  AWindow: PGtkWindow;
{$ENDIF}
begin
  if not IsMouseOverForm then
    Hide
  {$IFDEF LclGtk2}
  else
  begin
    if not FModalShowing then
    begin
      if not Self.HandleAllocated then
        Exit;
      AWindow := {%H-}PGtkWindow(Self.Handle);
      gtk_window_set_modal(AWindow, True);
      FModalShowing := True;
    end;
  end;
  {$ENDIF}
end;

procedure TShowHintFrm.ShowFormAt(const AX, AY: Integer);
begin
  Left := AX + 10;
  Top := AY + 10;
  Show;
end;

procedure TShowHintFrm.MoveFormTo(const AX, AY: Integer);
begin
  Left := AX + 10;
  Top := AY + 10;
end;

procedure TShowHintFrm.UpdateInfo(ANode: PVirtualNode);
var
  Node: PVirtualNode;
  Data: PData;
  CurFrame: TfrShowHint;
  I: Integer;
  LazPackage: TLazarusPackage;
  TotHeight: Integer;
begin
  Data := VisualTree.VST.GetNodeData(ANode);
  Caption := Format(rsMainFrm_rsPackageInformation, [Data^.PackageDisplayName]);
  pnPackageName.Caption := Data^.PackageDisplayName;
  CalcHeight(mDescription, Data^.CommunityDescription);
  for I := FFrames.Count - 1  downto 0 do
  begin
    CurFrame := TfrShowHint(FFrames.Items[I]);
    CurFrame.Visible :=  False;
    CurFrame.Free;
    CurFrame := nil;
  end;
  TotHeight := 0;
  FFrames.Clear;
  Node := VisualTree.VST.GetFirstChild(ANode);
  while Assigned(Node) do
  begin
    Data := VisualTree.VST.GetNodeData(Node);
    if Data^.DataType = 2 then
    begin
      LazPackage := SerializablePackages.FindLazarusPackage(Data^.LazarusPackageName);
      CurFrame := TfrShowHint.Create(nil);
      CurFrame.Align := alTop;
      CurFrame.Init;
      CurFrame.pnPackageName.Caption := ' ' + LazPackage.Name;
      FFrames.Add(CurFrame);
      CurFrame.pnBase.BorderSpacing.Bottom := 3;
      CurFrame.Parent := sbLazPackages;
      CalcHeight(CurFrame.mDescription, Trim(LazPackage.Description));
      CalcHeight(CurFrame.mLicense, Trim(LazPackage.License));
      CurFrame.Height := CurFrame.pnPackageName.Height + CurFrame.pnDescription.Height + CurFrame.pnLicense.Height +
                         CurFrame.BorderSpacing.Top + CurFrame.pnBase.BorderSpacing.Bottom;
      TotHeight := TotHeight + CurFrame.Height;
      CurFrame.Visible := True;
    end;
    Node := VisualTree.VST.GetNextSibling(Node);
  end;
  if FFrames.Count > 1 then
  begin
    TfrShowHint(FFrames.Items[0]).pnBuffer.Visible := True;
    TfrShowHint(FFrames.Items[0]).BorderSpacing.Bottom := 0;
  end;
  TfrShowHint(FFrames.Items[FFrames.Count - 1]).pnBase.BorderSpacing.Top := 0;
  TotHeight := TotHeight + pnPackageName.Height + pnDescription.Height + 25;
  if (TotHeight < 51) or (TotHeight > 365) then
    TotHeight := 365;
  Self.Height := TotHeight;
  if (Options.HintFormOption = 0) and (Self.Top + Self.Height > Screen.WorkAreaHeight) then
    Self.Top := Screen.WorkAreaHeight - Self.Height;
  sbLazPackages.SetFocus;
end;

procedure TShowHintFrm.SetupTimer(const AInterval: Integer);
begin
  {$IFDEF LclGtk2}FModalShowing := False;{$ENDIF}
  tmWait.Enabled := False;
  tmWait.Interval := AInterval;
  tmWait.Enabled := True;
end;


end.

