{ Form for the spotter window

  Copyright (C) 2018  Michael van Canneyt  michael@freepascal.org

  This library is free software; you can redistribute it and/or modify it
  under the terms of the GNU Library General Public License as published by
  the Free Software Foundation; either version 2 of the License, or (at your
  option) any later version with the following modification:

  As a special exception, the copyright holders of this library give you
  permission to link this library with independent modules to produce an
  executable, regardless of the license terms of these independent modules,and
  to copy and distribute the resulting executable under terms of your choice,
  provided that you also meet, for each linked independent module, the terms
  and conditions of the license of that module. An independent module is a
  module which is not derived from or based on this library. If you modify
  this library, you may extend this exception to your version of the library,
  but you are not obligated to do so. If you do not wish to do so, delete this
  exception statement from your version.

  This program is distributed in the hope that it will be useful, but WITHOUT
  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
  FITNESS FOR A PARTICULAR PURPOSE. See the GNU Library General Public License
  for more details.

  You should have received a copy of the GNU Library General Public License
  along with this library; if not, write to the Free Software Foundation,
  Inc., 51 Franklin Street - Fifth Floor, Boston, MA 02110-1335, USA.
}
unit frmspotter;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LazUTF8, Forms, Controls, Graphics, Dialogs,
  StdCtrls, IDECommands, LazIDEIntf;

type

  { TSpotterForm }

  TSpotterForm = class(TForm)
    ECommand: TEdit;
    LBMatches: TListBox;
    procedure ECommandChange(Sender: TObject);
    procedure ECommandKeyUp(Sender: TObject; var Key: Word; {%H-}Shift: TShiftState);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure LBMatchesClickC(Sender: TObject);
    procedure LBMatchesEnter(Sender: TObject);
    procedure LBMatchesKeyUp(Sender: TObject; var Key: Word; {%H-}Shift: TShiftState
      );
  private
    FShowCategory: Boolean;
    FCommands: TStringList;
    FOrgCaption : String;
    procedure ExecuteSelected;
    Procedure FillCommands;
    procedure FilterCommandList(aCmd: String);
    function GetCommandCategoryString(Cmd: TIDECommand): String;
  public
    Property ShowCategory : Boolean Read FShowCategory Write FShowCategory;
  end;

var
  SpotterForm: TSpotterForm;

implementation

Uses LCLType, LCLProc;

{$R *.lfm}

{ TSpotterForm }

procedure TSpotterForm.ECommandChange(Sender: TObject);
begin
  FilterCommandList(ECommand.Text);
end;

procedure TSpotterForm.ECommandKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  Case Key of
  VK_ESCAPE: Hide;
  VK_UP:
    begin
    If LBMatches.ItemIndex>0 then
      LBMatches.ItemIndex:=LBMatches.ItemIndex-1;
    Key:=0;
    ECommand.SelStart:=Length(ECommand.Text);
    end;
  VK_DOWN:
    begin
    If LBMatches.ItemIndex<LBMatches.Items.Count-1 then
      LBMatches.ItemIndex:=LBMatches.ItemIndex+1;
    Key:=0;
    ECommand.SelStart:=Length(ECommand.Text);
    end;
  VK_RETURN :
    If LBMatches.ItemIndex>=0 then
      ExecuteSelected;
  end;
end;

procedure TSpotterForm.FormClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  CloseAction:=caHide;
end;

procedure TSpotterForm.FilterCommandList(aCmd: String);

Var
  i : Integer;
  Cmd : TIDECommand;
  pref,ks : string;

begin
  aCmd:=LowerCase(aCmd);
  LBMatches.Items.BeginUpdate;
  try
    LBMatches.Items.Clear;
    For I:=0 to FCommands.Count-1 do
      if Pos(aCmd,FCommands[I])>0 then
        begin
        Cmd:=TIDECommand(FCommands.Objects[i]);
        Pref:=GetCommandCategoryString(Cmd);
        ks:='';
        if Cmd.ShortcutA.Key1<>VK_UNKNOWN then
          begin
          ks:=' ('+KeyAndShiftStateToKeyString(Cmd.ShortcutA.Key1,Cmd.ShortcutA.Shift1);
          if Cmd.ShortcutA.Key2<>VK_UNKNOWN then
            ks:=Ks+', '+KeyAndShiftStateToKeyString(Cmd.ShortcutA.Key1,Cmd.ShortcutA.Shift1);
          ks:=ks+')';
          end;
        LBMatches.Items.AddObject(Pref+Cmd.LocalizedName+ks,Cmd);
        end;
    Caption:=FOrgCaption+Format(' (%d/%d)',[LBMatches.Items.Count,FCommands.Count]);
  finally
    LBMatches.Items.EndUpdate;
    LBMatches.Visible:=LBMatches.Items.Count>0;
    If LBMatches.Visible then
      LBMatches.ItemIndex:=0;
  end;
end;

procedure TSpotterForm.FormCreate(Sender: TObject);
begin
  FCommands:=TStringList.Create;
  FOrgCaption:=Caption;
  FillCommands;
end;

procedure TSpotterForm.FormShow(Sender: TObject);
Var
  MF : TForm;
begin
  MF:=LazarusIDE.GetMainBar;
  if MF=nil then MF:=Application.MainForm;
  if Assigned(MF) then
    begin
    Top:=40; //MF.ClientOrigin.y+MF.Height+32; // Needs some additional work ?
    Left:=MF.ClientOrigin.x+(MF.Width-Width) div 2;
    end;
  ECommand.Clear;
  LBMatches.Clear;
end;

procedure TSpotterForm.LBMatchesClickC(Sender: TObject);

begin
  ExecuteSelected;
end;

procedure TSpotterForm.ExecuteSelected;

Var
  idx: Integer;
begin
  Idx:=LBMatches.ItemIndex;
  if (Idx>=0) then
    begin
    Hide;
    TIDECommand(LBMatches.Items.Objects[Idx]).Execute(Application);
    end;
end;

procedure TSpotterForm.LBMatchesEnter(Sender: TObject);
begin

end;

procedure TSpotterForm.LBMatchesKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key=VK_ESCAPE then
    Hide;
end;

function TSpotterForm.GetCommandCategoryString(Cmd: TIDECommand): String;

Const
  Cmds = ' commands';

Var
  Cat: TIDECommandCategory;
  D : String;
  P : Integer;

begin
  Result:='';
  If Not ShowCategory then
    Exit;
  Cat:=Cmd.Category;
  While (Cat<>Nil) and (Cat.Parent<>Nil) do
    Cat:=Cat.Parent;
  If Cat<>Nil then
    begin
    D:=Cat.Description;
    P:=Pos(' commands',D);
    If (P>0) and (P=Length(D)-Length(Cmds)+1) then
      D:=Copy(D,1,P-1);
    Result:=D+': ';
    end;
end;

procedure TSpotterForm.FillCommands;
var
  I, J: Integer;
  Cmd : TIDECommand;
  Pref : String;

begin
  For I:=0 to IDECommandList.CategoryCount-1 do
    for J:=0 to IDECommandList.Categories[I].Count-1 do
      begin
      if TObject(IDECommandList.Categories[I].Items[J]) is TIDECommand then
        begin
        Cmd:=TIDECommand(IDECommandList.Categories[I].Items[J]);
        Pref:=GetCommandCategoryString(Cmd);
        FCommands.AddObject(UTF8LowerCase(Pref+Cmd.LocalizedName),Cmd);
        end;
      end;
  FCommands.Sort;
  Caption:=FOrgCaption+Format(' (%d)',[FCommands.Count]);
end;

end.

