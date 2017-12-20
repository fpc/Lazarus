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
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, Menus, LCLType,
  opkman_virtualtrees, opkman_showhintbase;

type

  { TShowHintFrm }

  TShowHintFrm = class(TForm)
    pnBase: TPanel;
    sbLazPackages: TScrollBox;
    tmWait: TTimer;
    procedure FormCreate(Sender: TObject);
    procedure FormDeactivate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormKeyUp(Sender: TObject; var Key: Word; {%H-}Shift: TShiftState);
    procedure tmWaitTimer(Sender: TObject);
  private
    FFrames: TList;
  public
    function IsMouseOverForm: Boolean;
    procedure ShowFormAt(const AX, AY: Integer);
    procedure MoveFormTo(const AX, AY: Integer);
    procedure UpdateInfo(ANode: PVirtualNode);
  end;

var
  ShowHintFrm: TShowHintFrm;

implementation

{$R *.lfm}

{ TShowHintFrm }

uses LCLIntf, opkman_visualtree, opkman_serializablepackages;

function TShowHintFrm.IsMouseOverForm: Boolean;
var
  P: TPoint;
begin
  Result := False;
  P.X := 0;
  P.Y := 0;
  GetCursorPos(P);
  Result := PtInRect(Self.BoundsRect, P);
end;

procedure TShowHintFrm.FormCreate(Sender: TObject);
begin
  FFrames := TList.Create;
end;

procedure TShowHintFrm.FormDeactivate(Sender: TObject);
begin
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
    tmWait.Enabled := True;
end;

procedure TShowHintFrm.tmWaitTimer(Sender: TObject);
begin
  tmWait.Enabled := False;
  if not IsMouseOverForm then
    Hide;
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
begin
  Data := VisualTree.VST.GetNodeData(ANode);
  Caption := Data^.PackageDisplayName;
  for I := FFrames.Count - 1  downto 0 do
  begin
    CurFrame := TfrShowHint(FFrames.Items[I]);
    CurFrame.Visible :=  False;
    CurFrame.Free;
    CurFrame := nil;
  end;
  FFrames.Clear;
  I := -1;
  Node := VisualTree.VST.GetFirstChild(ANode);
  while Assigned(Node) do
  begin
    Data := VisualTree.VST.GetNodeData(Node);
    if Data^.DataType = 2 then
    begin
      Inc(I);
      LazPackage := SerializablePackages.FindLazarusPackage(Data^.LazarusPackageName);
      CurFrame := TfrShowHint.Create(nil);
      CurFrame.Align := alTop;
      CurFrame.Visible := True;
      CurFrame.Init;
      CurFrame.pnPackageName.Caption := ' ' + LazPackage.Name;
      CurFrame.mDescription.Text := Trim(LazPackage.Description);
      CurFrame.mLicense.Text := Trim(LazPackage.License);
      CurFrame.Parent := sbLazPackages;
      FFrames.Add(CurFrame);
      if I >= 1 then
        TfrShowHint(FFrames.Items[I - 1]).pnBuffer.Visible := True;

    end;
    Node := VisualTree.VST.GetNextSibling(Node);
  end;
end;

end.

