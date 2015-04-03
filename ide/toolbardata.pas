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
 ToDo:
   Extract an interface from here and put it to IdeIntf package.
}

unit ToolbarData;


{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LCLProc, ComCtrls, ExtCtrls, ToolWin, Controls,
  MenuIntf, IDEImagesIntf, IDECommands, Laz2_XMLCfg, fgl;

type

  // Option classes take care of saving / loading environment options data.
  // They don't contain LCL components.

  { TIDEToolBarOptions }
  TIDEToolBarOptions = class
  private
    Position: Integer;
    Break: Boolean;
    ButtonNames: TStringList;
  public
    constructor Create;
    destructor Destroy; override;
  end;

  TIDEToolBarOptionList = specialize TFPGObjectList<TIDEToolBarOptions>;

  { TIDECoolBarOptions }
  TIDECoolBarOptions = class
  private
    //coolbar
    FIDECoolBarVisible: Boolean;
    FIDECoolBarWidth: Integer;
    FIDECoolBarGrabStyle: Integer;
    FIDECoolBarGrabWidth: Integer;
    FIDECoolBarBorderStyle: Integer;
    FIDECoolBarToolBars: TIDEToolBarOptionList;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    function Load(XMLConfig: TXMLConfig): Boolean;
    function Save(XMLConfig: TXMLConfig): Boolean;
  public
    property IDECoolBarVisible: Boolean read FIDECoolBarVisible write FIDECoolBarVisible;
    property IDECoolBarWidth: Integer read FIDECoolBarWidth write FIDECoolBarWidth;
    property IDECoolBarGrabStyle: Integer read FIDECoolBarGrabStyle write FIDECoolBarGrabStyle;
    property IDECoolBarGrabWidth: Integer read FIDECoolBarGrabWidth write FIDECoolBarGrabWidth;
    property IDECoolBarBorderStyle: Integer read FIDECoolBarBorderStyle write FIDECoolBarBorderStyle;
    property IDECoolBarToolBars: TIDEToolBarOptionList read FIDECoolBarToolBars;
  end;

  // Actual Coolbar and its member Toolbars

  TOnToolBarClick = procedure(Sender: TObject) of object;

  { TIDEToolBar }
  TIDEToolBar = class
   private
     FToolBar: TToolbar;
     FButtonList: TFPList;
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
   public
     constructor Create;
     destructor Destroy; override;
     procedure AddCustomItems(Index: Integer);
     procedure ClearToolbar;
   public
     property Position: Integer read FPosition write FPosition;
     property Break: Boolean read FBreak write FBreak;
     property ButtonNames: TStringList read FButtonNames;
     property Toolbar: TToolBar read FToolBar;
     property OnToolBarClick: TOnToolbarClick read FOnToolbarClick write FOnToolbarClick;
   end;

  TIDEToolBarList = specialize TFPGObjectList<TIDEToolBar>;

  { TIDEToolBarToolButton }
  TIDEToolBarToolButton = class(TToolButton)
  private
    FMenuItem: TIDEMenuItem;
  public
    procedure Click; override;
    property IdeMenuItem: TIDEMenuItem read FMenuItem write FMenuItem;
  end;

  { TIDECoolBar }
  TIDECoolBar = class
  private
    FCoolBar: TCoolBar;       // The actual CoolBar, not owned by this class.
    FCoolBarToolBars: TIDEToolBarList;
    FIsVisible: Boolean;    //cannot hide/show the coolbar on toolbar_options,
  public                    //instead we use a variable
    constructor Create(ACoolBar: TCoolBar);
    destructor Destroy; override;
    procedure SetCoolBarDefaults;
    procedure SetToolBarDefaults;
    procedure CopyFromOptions(Options: TIDECoolBarOptions);
    procedure CopyToOptions(Options: TIDECoolBarOptions);
    function Add: TIDEToolBar;
    function FindByToolBar(const ToolBar: TToolBar): Integer;
    procedure Sort;
    function IsDefaultCoolbar: Boolean;
    function IsDefaultToolbar: Boolean;
  public
    property ToolBars: TIDEToolBarList read FCoolBarToolBars;
    property CoolBar: TCoolBar read FCoolBar;
    property IsVisible: Boolean read FIsVisible write FIsVisible;
  end;

  function GetShortcut(AMenuItem: TIDEMenuItem): string;

var
  IDECoolBar: TIDECoolBar;

implementation
uses MainBar;

const
  BasePath = 'IDECoolBarOptions/';

{ TIDEToolBarOptions }

constructor TIDEToolBarOptions.Create;
begin
  inherited Create;
  ButtonNames := TStringList.Create;
end;

destructor TIDEToolBarOptions.Destroy;
begin
  ButtonNames.Free;
  inherited Destroy;
end;

{ TIDECoolBarOptions }

constructor TIDECoolBarOptions.Create;
var
  IDEToolBarOptions: TIDEToolBarOptions;
begin
  inherited Create;
  //set default values
  FIDECoolBarVisible := True;
  FIDECoolBarWidth := 230;
  FIDECoolBarGrabStyle := 1;
  FIDECoolBarGrabWidth := 5;
  FIDECoolBarBorderStyle := 1;

  FIDECoolBarToolBars := TIDEToolBarOptionList.Create;
  IDEToolBarOptions := TIDEToolBarOptions.Create;
  IDEToolBarOptions.Position := 0;
  IDEToolBarOptions.Break := False;
  with IDEToolBarOptions.ButtonNames do
  begin
    Add('IDEMainMenu/File/itmFileNew/itmFileNewForm');
    Add('IDEMainMenu/File/itmFileNew/itmFileNewUnit');
    Add('---------------');
    Add('IDEMainMenu/File/itmFileOpenSave/itmFileOpen');
    Add('IDEMainMenu/File/itmFileOpenSave/itmFileSave');
    Add('IDEMainMenu/File/itmFileOpenSave/itmFileSaveAll');
    Add('---------------');
    Add('IDEMainMenu/View/itmViewMainWindows/itmViewToggleFormUnit');
  end;
  FIDECoolBarToolBars.Add(IDEToolBarOptions);

  //debug toolbar defaults
  IDEToolBarOptions := TIDEToolBarOptions.Create;
  IDEToolBarOptions.Position := 1;
  IDEToolBarOptions.Break := True;
  with IDEToolBarOptions.ButtonNames do
  begin
    Add('IDEMainMenu/Project/itmProjectAddRemoveSection/itmProjectViewUnits');
    Add('IDEMainMenu/Project/itmProjectAddRemoveSection/itmProjectViewForms');
    Add('---------------');
    Add('IDEMainMenu/Project/itmProjectAddRemoveSection/itmProjectBuildMode');
    Add('IDEMainMenu/Run/itmRunnning/itmRunMenuRun');
    Add('IDEMainMenu/Run/itmRunnning/itmRunMenuPause');
    Add('IDEMainMenu/Run/itmRunnning/itmRunMenuStop');
    Add('IDEMainMenu/Run/itmRunnning/itmRunMenuStepOver');
    Add('IDEMainMenu/Run/itmRunnning/itmRunMenuStepInto');
    Add('IDEMainMenu/Run/itmRunnning/itmRunMenuStepOut');
  end;
  FIDECoolBarToolBars.Add(IDEToolBarOptions);
end;

destructor TIDECoolBarOptions.Destroy;
begin
  FIDECoolBarToolBars.Free;
  inherited Destroy;
end;

procedure TIDECoolBarOptions.Clear;
begin
  FIDECoolBarToolBars.Clear;
end;

function TIDECoolBarOptions.Load(XMLConfig: TXMLConfig): boolean;
var
  ToolBarOpt: TIDEToolBarOptions;
  ToolBarCount: Integer;
  ButtonCount: Integer;
  ButtonName: string;
  SubPath: String;
  I, J: Integer;
begin
  //Coolbar
  FIDECoolBarVisible := XMLConfig.GetValue(BasePath + 'Visible/Value', True);
  FIDECoolBarWidth := XMLConfig.GetValue(BasePath + 'Width/Value', 230);
  FIDECoolBarGrabStyle := XMLConfig.GetValue(BasePath + 'GrabStyle/Value', 1);
  FIDECoolBarGrabWidth := XMLConfig.GetValue(BasePath + 'GrabWidth/Value', 5);
  FIDECoolBarBorderStyle := XMLConfig.GetValue(BasePath + 'BorderStyle/Value', 5);
  ToolbarCount := XMLConfig.GetValue(BasePath + 'ToolBarCount/Value', 0);
  if ToolBarCount > 0 then
  begin
    FIDECoolBarToolBars.Clear;
    SubPath := BasePath + 'ToolBar';
    for I := 0 to ToolbarCount - 1 do
    begin
      ToolBarOpt := TIDEToolBarOptions.Create;
      FIDECoolBarToolBars.Add(ToolBarOpt);
      ToolBarOpt.Break := XMLConfig.GetValue(SubPath + IntToStr(I) + '/Break/Value', False);
      ButtonCount := XMLConfig.GetValue(SubPath + IntToStr(I) + '/ButtonCount/Value', 0);
      for J := 0 to ButtonCount - 1 do
      begin
        ButtonName := Trim(XMLConfig.GetValue(
          SubPath + IntToStr(I) +  '/Buttons/Name' + IntToStr(J) + '/Value', ''));
        if ButtonName <> '' then
          ToolBarOpt.ButtonNames.Add(ButtonName);
      end;
    end;
  end;

  Result:=True;
end;

function TIDECoolBarOptions.Save(XMLConfig: TXMLConfig): boolean;
var
  SubPath: String;
  I, J: Integer;
begin
  //coolbar
  XMLConfig.DeletePath(BasePath);
  XMLConfig.SetDeleteValue(BasePath + 'Visible/Value', FIDECoolBarVisible, True);
  XMLConfig.SetDeleteValue(BasePath + 'Width/Value', FIDECoolBarWidth, 0);
  XMLConfig.SetDeleteValue(BasePath + 'GrabStyle/Value', FIDECoolBarGrabStyle, 1);
  XMLConfig.SetDeleteValue(BasePath + 'GrabWidth/Value', FIDECoolBarGrabWidth, 5);
  XMLConfig.SetDeleteValue(BasePath + 'BorderStyle/Value', FIDECoolBarBorderStyle, 5);
  if FIDECoolBarToolBars.Count > 0 then
  begin
    XMLConfig.SetDeleteValue(BasePath + 'ToolBarCount/Value', FIDECoolBarToolBars.Count, 0);
    SubPath := BasePath + 'ToolBar';
    for I := 0 to FIDECoolBarToolBars.Count - 1 do
    begin
      XMLConfig.SetDeleteValue(SubPath + IntToStr(I) + '/Break/Value',
                               FIDECoolBarToolBars[I].Break, False);
      XMLConfig.SetDeleteValue(SubPath + IntToStr(I) + '/ButtonCount/Value',
                               FIDECoolBarToolBars[I].ButtonNames.Count, 0);
      for J := 0 to FIDECoolBarToolBars[I].ButtonNames.Count - 1 do
        XMLConfig.SetDeleteValue(SubPath + IntToStr(I) +  '/Buttons/Name' + IntToStr(J) + '/Value',
                                 FIDECoolBarToolBars[I].ButtonNames[J], '');
    end;
  end;
  Result:=True;
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
  FButtonList := TFPList.Create;
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


{ TEditToolBarToolButton }
procedure TIDEToolBarToolButton.Click;
begin
  inherited Click;
  if assigned(FMenuItem) then
    FMenuItem.TriggerClick;
end;


{ TIDECoolBar }
constructor TIDECoolBar.Create(ACoolBar: TCoolBar);
begin
  inherited Create;
  FCoolBar := ACoolBar;
  FCoolBarToolBars := TIDEToolBarList.Create;
end;

destructor TIDECoolBar.Destroy;
begin
  FreeAndNil(FCoolBarToolBars);
  inherited Destroy;
end;

procedure TIDECoolBar.SetCoolBarDefaults;
begin
  FCoolBar.Vertical := False;
  FCoolBar.HorizontalSpacing := 1;
  FCoolBar.VerticalSpacing := 3;
  FCoolBar.FixedSize := True;
  FCoolBar.DoubleBuffered := True;
  FCoolBar.EdgeInner := esNone;
  FCoolBar.EdgeOuter := esNone;

  Coolbar.GrabStyle := TGrabStyle(1);
  Coolbar.GrabWidth := 5;
  Coolbar.BandBorderStyle := bsSingle;
end;

procedure TIDECoolBar.SetToolBarDefaults;
var
  IDEToolBar: TIDEToolBar;
begin
  FCoolBarToolBars.Clear;
  //standard toolbar defaults
  IDEToolBar := Add;
  IDEToolBar.Position := 0;
  IDEToolBar.Break := False;
  with IDEToolBar.ButtonNames do
  begin
    Add('IDEMainMenu/File/itmFileNew/itmFileNewForm');
    Add('IDEMainMenu/File/itmFileNew/itmFileNewUnit');
    Add('---------------');
    Add('IDEMainMenu/File/itmFileOpenSave/itmFileOpen');
    Add('IDEMainMenu/File/itmFileOpenSave/itmFileSave');
    Add('IDEMainMenu/File/itmFileOpenSave/itmFileSaveAll');
    Add('---------------');
    Add('IDEMainMenu/View/itmViewMainWindows/itmViewToggleFormUnit');
  end;

  //debug toolbar defaults
  IDEToolBar := Add;
  IDEToolBar.Position := 1;
  IDEToolBar.Break := True;
  with IDEToolBar.ButtonNames do
  begin
    Add('IDEMainMenu/Project/itmProjectAddRemoveSection/itmProjectViewUnits');
    Add('IDEMainMenu/Project/itmProjectAddRemoveSection/itmProjectViewForms');
    Add('---------------');
    Add('IDEMainMenu/Project/itmProjectAddRemoveSection/itmProjectBuildMode');
    Add('IDEMainMenu/Run/itmRunnning/itmRunMenuRun');
    Add('IDEMainMenu/Run/itmRunnning/itmRunMenuPause');
    Add('IDEMainMenu/Run/itmRunnning/itmRunMenuStop');
    Add('IDEMainMenu/Run/itmRunnning/itmRunMenuStepOver');
    Add('IDEMainMenu/Run/itmRunnning/itmRunMenuStepInto');
    Add('IDEMainMenu/Run/itmRunnning/itmRunMenuStepOut');
  end;
end;

procedure TIDECoolBar.CopyFromOptions(Options: TIDECoolBarOptions);
var
  I: Integer;
  IDEToolBar: TIDEToolBar;
begin
  FCoolBarToolBars.Clear;
  for I := 0 to Options.FIDECoolBarToolBars.Count - 1 do
  begin
    IDEToolBar := Add;
    IDEToolBar.Position := I;
    IDEToolBar.Break := Options.FIDECoolBarToolBars[I].Break;
    IDEToolBar.ButtonNames.Assign(Options.FIDECoolBarToolBars[I].ButtonNames);
  end;
end;

procedure TIDECoolBar.CopyToOptions(Options: TIDECoolBarOptions);
var
  I: Integer;
  Opt: TIDEToolBarOptions;
begin
  Options.FIDECoolBarToolBars.Clear;
  for I := 0 to FCoolBarToolBars.Count - 1 do
  begin
    Opt := TIDEToolBarOptions.Create;
    Options.FIDECoolBarToolBars.Add(Opt);
    Opt.Position := FCoolBarToolBars[I].Position;
    Opt.Break := FCoolBarToolBars[I].Break;
    Opt.ButtonNames.Assign(FCoolBarToolBars[I].ButtonNames);
  end;
end;

function TIDECoolBar.Add: TIDEToolBar;
begin
  Result := TIDEToolBar.Create;
  FCoolBarToolBars.Add(Result);
end;

function TIDECoolBar.FindByToolBar(const ToolBar: TToolBar): Integer;
var
  I: Integer;
begin
  Result := -1;
  for I := 0 to FCoolbarToolBars.Count - 1 do
  begin
    if ToolBars[I].Toolbar = Toolbar  then
    begin
      Result := I;
      Break;
    end;
  end;
end;

function Compare(const Item1, Item2: TIDEToolBar): Integer;
begin
  Result := Item1.Position - Item2.Position;
end;

procedure TIDECoolBar.Sort;
begin
  FCoolbarToolBars.Sort(@Compare);
end;

function TIDECoolBar.IsDefaultCoolbar: Boolean;
begin
  Result := (FIsVisible) and (FCoolBar.BandBorderStyle = bsSingle) and
            (FCoolBar.GrabStyle = gsDouble) and (FCoolBar.GrabWidth = 5);
end;

function TIDECoolBar.IsDefaultToolbar: Boolean;
var
  IDEToolBar0: TIDEToolBar;
  IDEToolBar1: TIDEToolBar;
begin
  Result := False;
  if  FCoolBarToolBars.Count <> 2 then
    Exit;
  if (FCoolBarToolBars.Items[0].ButtonNames.Count <> 8) or (FCoolBarToolBars.Items[1].ButtonNames.Count <> 10) then
    Exit;
  IDEToolBar0 := FCoolBarToolBars.Items[0];
  IDEToolBar1 := FCoolBarToolBars.Items[1];
  Result := (IDEToolBar0.ButtonNames[0] = 'IDEMainMenu/File/itmFileNew/itmFileNewForm') and
            (IDEToolBar0.ButtonNames[1] = 'IDEMainMenu/File/itmFileNew/itmFileNewUnit') and
            (IDEToolBar0.ButtonNames[2] = '---------------') and
            (IDEToolBar0.ButtonNames[3] = 'IDEMainMenu/File/itmFileOpenSave/itmFileOpen') and
            (IDEToolBar0.ButtonNames[4] = 'IDEMainMenu/File/itmFileOpenSave/itmFileSave') and
            (IDEToolBar0.ButtonNames[5] = 'IDEMainMenu/File/itmFileOpenSave/itmFileSaveAll') and
            (IDEToolBar0.ButtonNames[6] = '---------------') and
            (IDEToolBar0.ButtonNames[7] = 'IDEMainMenu/View/itmViewMainWindows/itmViewToggleFormUnit') and
               (IDEToolBar1.ButtonNames[0] = 'IDEMainMenu/Project/itmProjectAddRemoveSection/itmProjectViewUnits') and
            (IDEToolBar1.ButtonNames[1] = 'IDEMainMenu/Project/itmProjectAddRemoveSection/itmProjectViewForms') and
            (IDEToolBar1.ButtonNames[2] = '---------------') and
            (IDEToolBar1.ButtonNames[3] = 'IDEMainMenu/Project/itmProjectAddRemoveSection/itmProjectBuildMode') and
            (IDEToolBar1.ButtonNames[4] = 'IDEMainMenu/Run/itmRunnning/itmRunMenuRun') and
            (IDEToolBar1.ButtonNames[5] = 'IDEMainMenu/Run/itmRunnning/itmRunMenuPause') and
            (IDEToolBar1.ButtonNames[6] = 'IDEMainMenu/Run/itmRunnning/itmRunMenuStop') and
            (IDEToolBar1.ButtonNames[7] = 'IDEMainMenu/Run/itmRunnning/itmRunMenuStepOver') and
            (IDEToolBar1.ButtonNames[8] = 'IDEMainMenu/Run/itmRunnning/itmRunMenuStepInto') and
            (IDEToolBar1.ButtonNames[9] = 'IDEMainMenu/Run/itmRunnning/itmRunMenuStepOut');
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
