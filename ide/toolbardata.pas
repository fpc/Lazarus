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

 Author: Balázs Székely
 Abstract:
   The implementation of IDE Coolbar.
}

unit ToolbarData;


{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LCLProc, ComCtrls, ExtCtrls, ToolWin,
  MenuIntf, IDEImagesIntf, IDECommands;

type
  TOnToolBarClick = procedure(Sender: TObject) of object;

  { TIDEToolBar }
  TIDEToolBar = class
   private
     FToolBar: TToolbar;
     FButtonList: TList;
     FButtonNames: TStringList;
     FUpdateTimer: TTimer;
     FPosition: integer;
     FBreak: Boolean;
     FOnToolbarClick: TOnToolBarClick;
     procedure UpdateBar(Sender: TObject);
     procedure DoToolBarClick(Sender: TObject);
     procedure AddDivider;
     function GetItems(Index: Integer): TIDEMenuItem;
     function GetCount: Integer;
   protected
     procedure AddButton(AMenuItem: TIDEMenuItem);
     procedure PositionAtEnd(AToolbar: TToolbar; AButton: TToolButton);
     procedure Reload;
   public
     constructor Create;
     destructor Destroy; override;
     procedure AddCustomItems(Index: Integer);
     procedure ClearToolbar;
   public
     property Position: Integer read FPosition write FPosition;
     property Break: Boolean read FBreak write FBreak;
     property ButtonNames: TStringList read FButtonNames write FButtonNames;
     property Toolbar: TToolBar read FToolBar;
     property OnToolBarClick: TOnToolbarClick read FOnToolbarClick write FOnToolbarClick;
   end;

  { TIDEToolBarList }
  TIDEToolBarList = class
  private
    FToolBarList: TList;
    function GetItems(Index: integer): TIDEToolBar;
    function GetCount: Integer;
  public
    constructor Create;
    destructor Destroy; override;
    function Add: TIDEToolBar;
    function FindByToolBar(const ToolBar: TToolBar): Integer;
    procedure Delete(const Index: Integer);
    procedure Clear;
    procedure Sort;
  public
    property Items[Index: Integer]: TIDEToolBar read GetItems;
    property Count: Integer read GetCount;
  end;

  { TIDEToolBarToolButton }
  TIDEToolBarToolButton = class(TToolButton)
  private
    FMenuItem: TIDEMenuItem;
  public
    procedure Click; override;
    property IdeMenuItem: TIDEMenuItem read FMenuItem write FMenuItem;
  end;

  function GetShortcut(AMenuItem: TIDEMenuItem): string;


implementation
uses MainBar;

{ TEditToolBarToolButton }
procedure TIDEToolBarToolButton.Click;
begin
  inherited Click;
  if assigned(FMenuItem) then
    FMenuItem.TriggerClick;
end;

{ TIDEToolBar }
procedure TIDEToolBar.UpdateBar(Sender: TObject);
var
  I, J: Integer;
  MI: TIDEMenuItem;
begin
  ToolBar.BeginUpdate;
  try
    for I := ToolBar.ButtonCount - 1 downto 0 do
    begin
      if ToolBar.Buttons[I].Tag <> 0 then
      begin
        J := ToolBar.Buttons[I].Tag - 1;
        MI := TIDEMenuItem(FButtonList.Items[J]);
        if MI <> nil then
          ToolBar.Buttons[I].Enabled := MI.Enabled;
      end;
    end;
  finally
    ToolBar.EndUpdate;
  end;
end;

procedure TIDEToolBar.DoToolBarClick(Sender: TObject);
begin
  if Assigned(FOnToolbarClick) then
    FOnToolbarClick(Toolbar);
end;

constructor TIDEToolBar.Create;
begin
  FToolBar := TToolbar.Create(nil);
  with ToolBar do
  begin
    ButtonHeight := 22;
    ButtonWidth := 22;
    Height := 22;
    Width := 0;
    Flat     := True;
    AutoSize := True;
    Transparent := True;
    EdgeInner := esNone;
    EdgeOuter := esNone;
    Images   := IDEImages.Images_16;
    ShowHint := True;
    OnClick := @DoToolBarClick;
  end;
  FButtonList := TList.Create;
  FButtonNames := TStringList.Create;

  FUpdateTimer := TTimer.Create(nil);
  with FUpdateTimer do
  begin
    Interval := 500;
    OnTimer := @UpdateBar;
    Enabled := True;
  end;
end;

destructor TIDEToolBar.Destroy;
begin
  FButtonList.Free;
  FUpdateTimer.Free;
  FToolBar.Free;
  FButtonNames.Free;
  inherited Destroy;
end;

procedure TIDEToolBar.AddButton(AMenuItem: TIDEMenuItem);
var
  B: TIDEToolBarToolButton;
  ACaption: string;
  iPos: Integer;
begin
  B := TIDEToolBarToolButton.Create(ToolBar);
  ACaption      := AMenuItem.Caption;
  DeleteAmpersands(ACaption);
  B.Caption     := ACaption;
  // Get Shortcut, if any, and append to Hint
  ACaption:= ACaption + GetShortcut(AMenuItem);
  B.Hint        := ACaption;
  // If we have a image, us it. Otherwise supply a default.
  if AMenuItem.ImageIndex <> -1 then
    B.ImageIndex := AMenuItem.ImageIndex
  else
    B.ImageIndex := IDEImages.LoadImage(16, 'execute16');

  B.Style       := tbsButton;
  if (AMenuItem.Name = 'itmFileNewForm') or (AMenuItem.Name = 'itmFileNewUnit') then
  begin
    B.PopupMenu :=  MainIDEBar.NewUnitFormPopupMenu;
    B.Name := AMenuItem.Name;
  end
  else if AMenuItem.Name = 'itmProjectBuildMode' then
  begin
    B.Style := tbsDropDown;
    B.DropdownMenu := MainIDEBar.SetBuildModePopupMenu;
  end
  else if AMenuItem.Name = 'itmFileOpen' then
  begin
    B.Style := tbsDropDown;
    B.DropdownMenu := MainIDEBar.OpenFilePopUpMenu;
  end;

  B.IdeMenuItem := AMenuItem;
  iPos := FButtonList.Add(AMenuItem);
  B.Tag := iPos + 1;
  //B.OnClick     := AMenuItem.OnClick;
  PositionAtEnd(ToolBar, B);
end;

// position the button next to the last button
procedure TIDEToolBar.PositionAtEnd(AToolbar: TToolbar; AButton: TToolButton);
var
  SiblingButton: TToolButton;
begin
  if AToolbar.ButtonCount > 0 then
  begin
    SiblingButton := AToolbar.Buttons[AToolbar.ButtonCount - 1];
    AButton.SetBounds(SiblingButton.Left + SiblingButton.Width,
      SiblingButton.Top, AButton.Width, AButton.Height);
  end;
  AButton.Parent := AToolbar;
end;

procedure TIDEToolBar.Reload;
begin
  ClearToolbar;
 // AddCustomItems;
end;


procedure TIDEToolBar.AddCustomItems(Index: Integer);
const
  cDivider = '---------------';
var
  MI: TIDEMenuItem;
  AName: string;
begin
  ToolBar.BeginUpdate;
  try
    AName := FButtonNames.Strings[Index];
    if (AName <> '') then
    begin
      if AName = cDivider then
        AddDivider
      else
      begin
        MI := IDEMenuRoots.FindByPath(AName, False);
        if Assigned(MI) then
          AddButton(MI);
      end;
    end;
    UpdateBar(nil);
  finally
    ToolBar.EndUpdate;
  end;
end;

procedure TIDEToolBar.AddDivider;
var
  B: TToolButton;
begin
  B := TToolbutton.Create(ToolBar);
  B.Style := tbsDivider;
  PositionAtEnd(ToolBar, B);
end;

procedure TIDEToolBar.ClearToolbar;
var
  I: Integer;
begin
  ToolBar.BeginUpdate;
  try
    for i := ToolBar.ButtonCount - 1 downto 0 do
      ToolBar.Buttons[I].Free
  finally
    ToolBar.EndUpdate;
  end;
end;

function TIDEToolBar.GetItems(Index: Integer): TIDEMenuItem;
begin
  Result := TIDEMenuItem(FButtonList[Index]);
end;

function TIDEToolBar.GetCount: Integer;
begin
  Result := FButtonList.Count;
end;


{ TIDEToolBarList }
function TIDEToolBarList.GetItems(Index: integer): TIDEToolBar;
begin
  Result := TIDEToolBar(FToolBarList[Index]);
end;

constructor TIDEToolBarList.Create;
begin
  inherited Create;
  FToolBarList := TList.Create;
end;

destructor TIDEToolBarList.Destroy;
begin
  Clear;
  FreeAndNil(FToolBarList);
  inherited Destroy;
end;

function TIDEToolBarList.Add: TIDEToolBar;
var
  I: Integer;
  IDEToolBar: TIDEToolBar;
begin
  IDEToolBar := TIDEToolBar.Create;
  I := FToolBarList.Add(IDEToolBar);
  Result := Items[I];
end;

function TIDEToolBarList.FindByToolBar(const ToolBar: TToolBar): Integer;
var
  I: Integer;
begin
  Result := -1;
  for I := 0 to FToolBarList.Count - 1 do
  begin
    if Items[I].Toolbar = Toolbar  then
    begin
      Result := I;
      Break;
    end;
  end;
end;

procedure TIDEToolBarList.Delete(const Index: Integer);
begin
  Items[Index].Free;
  FToolBarList.Delete(Index);
end;

procedure TIDEToolBarList.Clear;
var
  I: Integer;
begin
  for I := 0 to FToolBarList.Count - 1 do
    Items[I].Free;
  FToolBarList.Clear;
end;

function Compare(Item1, Item2: Pointer): Integer;
begin
  Result := TIDEToolBar(Item1).Position - TIDEToolBar(Item2).Position;
end;

procedure TIDEToolBarList.Sort;
begin
  FToolBarList.Sort(@Compare);
end;

function TIDEToolBarList.GetCount: Integer;
begin
  Result := FToolBarList.Count;
end;

function GetShortcut(AMenuItem: TIDEMenuItem): string;
var
  ACommand: TIDECommand;
  AShortcut: string;
begin
  Result := '';
  AShortcut := '';
  if AMenuItem is TIDEMenuCommand then
  begin
    ACommand := TIDEMenuCommand(AMenuItem).Command;
    if Assigned(ACommand) then
      AShortcut := ShortCutToText(ACommand.AsShortCut);
    if AShortcut <> '' then
      Result:= ' (' + AShortcut + ')';
  end;
end;

end.
