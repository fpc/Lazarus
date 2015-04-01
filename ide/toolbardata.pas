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
  Classes, SysUtils, LCLProc, ComCtrls, ExtCtrls, ToolWin,
  MenuIntf, IDEImagesIntf, IDECommands, Laz2_XMLCfg, fgl;

type

  // Option classes take care of saving / loading environment options data.
  // They don't contain LCL components.

  { TIDEToolBarOptions }
  TIDEToolBarOptions = class
  private
    Break: Boolean;
    ButtonNames: TStringList;
  end;

  TIDEToolBarOptionList = specialize TFPGObjectList<TIDEToolBarOptions>;

  { TIDECoolBarOptions }
  TIDECoolBarOptions = class
  private
    //toolbars
    FToolbarVisible: Boolean;
    FToolBarStandardVisible: Boolean;
    FToolBarStandardLeft: Integer;
    FToolBarStandardTop: Integer;
    FToolBarViewDebugVisible: Boolean;
    FToolBarViewDebugLeft: Integer;
    FToolBarViewDebugTop: Integer;
    FToolBarHighlight: Boolean;
    FToolBarRaised: Boolean;
    FToolBars: TIDEToolBarOptionList;
    //coolbar
    FIDECoolBarVisible: Boolean;
    FIDECoolBarWidth: Integer;
    FIDECoolBarGrabStyle: Integer;
    FIDECoolBarGrabWidth: Integer;
    FIDECoolBarBorderStyle: Integer;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    //procedure Assign(Source: TIDECoolBarOptions);
    function IsDefault: Boolean;
    function Load(XMLConfig: TXMLConfig): boolean;
    function Save(XMLConfig: TXMLConfig): boolean;
  public
    property ToolBars: TIDEToolBarOptionList read FToolBars;
    property IDECoolBarVisible: Boolean read FIDECoolBarVisible write FIDECoolBarVisible;
    property IDECoolBarWidth: Integer read FIDECoolBarWidth write FIDECoolBarWidth;
    property IDECoolBarGrabStyle: Integer read FIDECoolBarGrabStyle write FIDECoolBarGrabStyle;
    property IDECoolBarGrabWidth: Integer read FIDECoolBarGrabWidth write FIDECoolBarGrabWidth;
    property IDECoolBarBorderStyle: Integer read FIDECoolBarBorderStyle write FIDECoolBarBorderStyle;
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
    FToolBars: TIDEToolBarList;
  public
    constructor Create(ACoolBar: TCoolBar);
    destructor Destroy; override;
    procedure SetCoolBarDefaults;
    procedure CreateDefaultToolbars;
    procedure CopyFromOptions(Options: TIDECoolBarOptions);
    procedure CopyToOptions(Options: TIDECoolBarOptions);
    function Add: TIDEToolBar;
    function FindByToolBar(const ToolBar: TToolBar): Integer;
    procedure Sort;
  public
    property ToolBars: TIDEToolBarList read FToolBars;
    property CoolBar: TCoolBar read FCoolBar;
  end;

  function GetShortcut(AMenuItem: TIDEMenuItem): string;

var
  IDECoolBar: TIDECoolBar;

implementation
uses MainBar;

const
  BasePath = 'IDECoolBarOptions/';

{ TIDECoolBarOptions }

constructor TIDECoolBarOptions.Create;
begin
  inherited Create;
  FToolBars := TIDEToolBarOptionList.Create;
  FToolbarVisible := False;
  FToolBarStandardVisible := False;
  FToolBarStandardLeft := 0;
  FToolBarStandardTop := 0;
  FToolBarViewDebugVisible := False;
  FToolBarViewDebugLeft := 0;
  FToolBarViewDebugTop := 26;
  FToolBarHighlight := False;
  FToolBarRaised := False;
  //coolbar
  FIDECoolBarVisible := True;
  FIDECoolBarWidth := 230;
  FIDECoolBarGrabStyle := 1;
  FIDECoolBarGrabWidth := 5;
  FIDECoolBarBorderStyle := 1;
end;

destructor TIDECoolBarOptions.Destroy;
begin
  FToolBars.Free;
  inherited Destroy;
end;

procedure TIDECoolBarOptions.Clear;
begin
  FToolBars.Clear;
end;
{
procedure TIDECoolBarOptions.Assign(Source: TIDECoolBarOptions);
begin

end;
}
function TIDECoolBarOptions.IsDefault: Boolean;
begin
  // ToDo: Implement
  Result := False;
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
    FToolBars.Clear;
    SubPath := BasePath + 'ToolBar';
    for I := 0 to ToolbarCount - 1 do
    begin
      ToolBarOpt := TIDEToolBarOptions.Create;
      FToolBars.Add(ToolBarOpt);
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

  // Toolbar
  FToolBarStandardVisible := XMLConfig.GetValue(BasePath+'Toolbars/Standard/Visible/Value', True);
  FToolBarStandardLeft := XMLConfig.GetValue(BasePath+'Toolbars/Standard/Left/Value', 0);
  FToolBarStandardTop := XMLConfig.GetValue(BasePath+'Toolbars/Standard/Top/Value', 0);

  FToolBarViewDebugVisible :=XMLConfig.GetValue(BasePath+'Toolbars/ViewDebug/Visible/Value', True);
  FToolBarViewDebugLeft := XMLConfig.GetValue(BasePath+'Toolbars/ViewDebug/Left/Value', 0);
  FToolBarViewDebugTop := XMLConfig.GetValue(BasePath+'Toolbars/ViewDebug/Top/Value', 26);

  FToolbarVisible := XMLConfig.GetValue(BasePath+'Toolbars/Common/Visible/Value', True);
  FToolBarHighlight := XMLConfig.GetValue(BasePath+'Toolbars/Common/Highlight/Value', False);
  FToolBarRaised := XMLConfig.GetValue(BasePath+'Toolbars/Common/Raised/Value', False);
  Result:=True;
end;

function TIDECoolBarOptions.Save(XMLConfig: TXMLConfig): boolean;
var
  SubPath: String;
  I, J: Integer;
begin
  // toolbar
  XMLConfig.SetDeleteValue(BasePath+'Toolbars/Standard/Visible/Value',FToolBarStandardVisible, True);
  XMLConfig.SetDeleteValue(BasePath+'Toolbars/Standard/Left/Value',FToolBarStandardLeft, 0);
  XMLConfig.SetDeleteValue(BasePath+'Toolbars/Standard/Top/Value',FToolBarStandardTop, 0);

  XMLConfig.SetDeleteValue(BasePath+'Toolbars/ViewDebug/Visible/Value',FToolBarViewDebugVisible, True);
  XMLConfig.SetDeleteValue(BasePath+'Toolbars/ViewDebug/Left/Value',FToolBarViewDebugLeft, 0);
  XMLConfig.SetDeleteValue(BasePath+'Toolbars/ViewDebug/Top/Value',FToolBarViewDebugTop, 26);

  XMLConfig.SetDeleteValue(BasePath+'Toolbars/Common/Visible/Value',FToolbarVisible, True);
  XMLConfig.SetDeleteValue(BasePath+'Toolbars/Common/Highlight/Value',FToolBarHighlight, False);
  XMLConfig.SetDeleteValue(BasePath+'Toolbars/Common/Raised/Value',FToolBarRaised, False);
  //coolbar
  XMLConfig.DeletePath(BasePath);
  XMLConfig.SetDeleteValue(BasePath + 'Visible/Value', FIDECoolBarVisible, True);
  XMLConfig.SetDeleteValue(BasePath + 'Width/Value', FIDECoolBarWidth, 0);
  XMLConfig.SetDeleteValue(BasePath + 'GrabStyle/Value', FIDECoolBarGrabStyle, 1);
  XMLConfig.SetDeleteValue(BasePath + 'GrabWidth/Value', FIDECoolBarGrabWidth, 5);
  XMLConfig.SetDeleteValue(BasePath + 'BorderStyle/Value', FIDECoolBarBorderStyle, 5);
  if FToolBars.Count > 0 then
  begin
    XMLConfig.SetDeleteValue(BasePath + 'ToolBarCount/Value', FToolBars.Count, 0);
    SubPath := BasePath + 'ToolBar';
    for I := 0 to FToolBars.Count - 1 do
    begin
      XMLConfig.SetDeleteValue(SubPath + IntToStr(I) + '/Break/Value',
                               FToolBars[I].Break, False);
      XMLConfig.SetDeleteValue(SubPath + IntToStr(I) + '/ButtonCount/Value',
                               FToolBars[I].ButtonNames.Count, 0);
      for J := 0 to FToolBars[I].ButtonNames.Count - 1 do
        XMLConfig.SetDeleteValue(SubPath + IntToStr(I) +  '/Buttons/Name' + IntToStr(J) + '/Value',
                                 FToolBars[I].ButtonNames[J], '');
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
  FToolBars := TIDEToolBarList.Create;
end;

destructor TIDECoolBar.Destroy;
begin
  FreeAndNil(FToolBars);
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
end;

procedure TIDECoolBar.CreateDefaultToolbars;
var
  IDEToolBar: TIDEToolBar;
begin
  //standard toolbar defaults
  IDEToolBar := Add;
  IDEToolBar.Position := 0;
  IDEToolBar.Break := False;
  IDEToolBar.ButtonNames.Add('IDEMainMenu/File/itmFileNew/itmFileNewForm');
  IDEToolBar.ButtonNames.Add('IDEMainMenu/File/itmFileNew/itmFileNewUnit');
  IDEToolBar.ButtonNames.Add('---------------');
  IDEToolBar.ButtonNames.Add('IDEMainMenu/File/itmFileOpenSave/itmFileOpen');
  IDEToolBar.ButtonNames.Add('IDEMainMenu/File/itmFileOpenSave/itmFileSave');
  IDEToolBar.ButtonNames.Add('IDEMainMenu/File/itmFileOpenSave/itmFileSaveAll');
  IDEToolBar.ButtonNames.Add('---------------');
  IDEToolBar.ButtonNames.Add('IDEMainMenu/View/itmViewMainWindows/itmViewToggleFormUnit');
  //debug toolbar defaults
  IDEToolBar := Add;
  IDEToolBar.Position := 1;
  IDEToolBar.Break := True;
  IDEToolBar.ButtonNames.Add('IDEMainMenu/Project/itmProjectAddRemoveSection/itmProjectViewUnits');
  IDEToolBar.ButtonNames.Add('IDEMainMenu/Project/itmProjectAddRemoveSection/itmProjectViewForms');
  IDEToolBar.ButtonNames.Add('---------------');
  IDEToolBar.ButtonNames.Add('IDEMainMenu/Project/itmProjectAddRemoveSection/itmProjectBuildMode');
  IDEToolBar.ButtonNames.Add('IDEMainMenu/Run/itmRunnning/itmRunMenuRun');
  IDEToolBar.ButtonNames.Add('IDEMainMenu/Run/itmRunnning/itmRunMenuPause');
  IDEToolBar.ButtonNames.Add('IDEMainMenu/Run/itmRunnning/itmRunMenuStop');
  IDEToolBar.ButtonNames.Add('IDEMainMenu/Run/itmRunnning/itmRunMenuStepOver');
  IDEToolBar.ButtonNames.Add('IDEMainMenu/Run/itmRunnning/itmRunMenuStepInto');
  IDEToolBar.ButtonNames.Add('IDEMainMenu/Run/itmRunnning/itmRunMenuStepOut');
end;

procedure TIDECoolBar.CopyFromOptions(Options: TIDECoolBarOptions);
var
  I: Integer;
  IDEToolBar: TIDEToolBar;
begin
  FToolBars.Clear;
  for I := 0 to Options.FToolBars.Count-1 do
  begin
    IDEToolBar := Add;
    IDEToolBar.Position := I;
    IDEToolBar.Break := Options.FToolBars[I].Break;
    IDEToolBar.ButtonNames.Assign(Options.FToolBars[I].ButtonNames);
  end;
end;

procedure TIDECoolBar.CopyToOptions(Options: TIDECoolBarOptions);
var
  I: Integer;
  Opt: TIDEToolBarOptions;
begin
  Options.FToolBars.Clear;
  for I := 0 to FToolBars.Count - 1 do
  begin
    Opt := TIDEToolBarOptions.Create;
    Options.FToolBars.Add(Opt);
    Opt.Break := FToolBars[I].Break;
    Opt.ButtonNames.Assign(FToolBars[I].ButtonNames);
  end;
end;

function TIDECoolBar.Add: TIDEToolBar;
begin
  Result := TIDEToolBar.Create;
  FToolBars.Add(Result);
end;

function TIDECoolBar.FindByToolBar(const ToolBar: TToolBar): Integer;
var
  I: Integer;
begin
  Result := -1;
  for I := 0 to FToolBars.Count - 1 do
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
  FToolBars.Sort(@Compare);
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
