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

unit IdeCoolbarData;


{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LCLProc, ComCtrls, ExtCtrls, ToolWin, Controls, fgl,
  MenuIntf, IDEImagesIntf, Laz2_XMLCfg, ToolbarConfig;

type

  // Option classes take care of saving / loading environment options data.
  // They don't contain LCL components.

  { TIDEToolBarOptions }
  TIDEToolBarOptions = class
  private
    FPosition: Integer;
    FBreak: Boolean;
    FButtonNames: TStringList;
  public
    constructor Create;
    destructor Destroy; override;
    function Equals(Opts: TIDEToolBarOptions): boolean; overload;
    procedure Assign(Source: TIDEToolBarOptions);
    procedure Load(XMLConfig: TXMLConfig; SubPath: String);
    procedure Save(XMLConfig: TXMLConfig; SubPath: String);
  published
    property Position: Integer read FPosition write FPosition;
    property Break: Boolean read FBreak write FBreak;
    property ButtonNames: TStringList read FButtonNames write FButtonNames;
  end;


  TIDEToolBarOptionList = specialize TFPGObjectList<TIDEToolBarOptions>;

  { TIDECoolBarOptions }
  TIDECoolBarOptions = class
  private
    FIDECoolBarVisible: Boolean;
    FIDECoolBarWidth: Integer;
    FIDECoolBarGrabStyle: Integer;
    FIDECoolBarGrabWidth: Integer;
    FIDECoolBarBorderStyle: Integer; //TFormBorderStyle;
    FIDECoolBarToolBars: TIDEToolBarOptionList;
    procedure CreateDefaultToolbars;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    function EqualToolbars(Opts: TIDECoolBarOptions): boolean;
    procedure Load(XMLConfig: TXMLConfig);
    procedure Save(XMLConfig: TXMLConfig);
  public
    property IDECoolBarVisible: Boolean read FIDECoolBarVisible write FIDECoolBarVisible;
    property IDECoolBarWidth: Integer read FIDECoolBarWidth write FIDECoolBarWidth;
    property IDECoolBarGrabStyle: Integer read FIDECoolBarGrabStyle write FIDECoolBarGrabStyle;
    property IDECoolBarGrabWidth: Integer read FIDECoolBarGrabWidth write FIDECoolBarGrabWidth;
    property IDECoolBarBorderStyle: Integer read FIDECoolBarBorderStyle write FIDECoolBarBorderStyle;
    property IDECoolBarToolBars: TIDEToolBarOptionList read FIDECoolBarToolBars;
  end;

  { TDefaultCoolBarOptions }
  TDefaultCoolBarOptions = class(TIDECoolBarOptions)
  public
    constructor Create;
    destructor Destroy; override;
  end;

  // Actual Coolbar and its member Toolbars

  TOnToolBarClick = procedure(Sender: TObject) of object;
  TIDEMenuItemList = specialize TFPGList<TIDEMenuItem>;

  { TIDEToolBar }
  TIDEToolBar = class
   private
     FToolBar: TToolbar;
     FButtonList: TIDEMenuItemList;
     FButtonNames: TStringList;
     FUpdateTimer: TTimer;
     FPosition: integer;
     FBreak: Boolean;
     FOnToolbarClick: TOnToolBarClick;
     procedure UpdateBar(Sender: TObject);
     procedure DoToolBarClick(Sender: TObject);
     procedure AddDivider;
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
    FCoolBar: TCoolBar;  // The actual CoolBar, not owned by this class.
    FCoolBarToolBars: TIDEToolBarList;
    FIsVisible: Boolean; //cannot hide/show the coolbar on toolbar_options, instead we use a variable
    FWidth: Integer;     //same as Isvisible
    // Used for assigning and testing the default configuration.
    FDefaultOptions: TDefaultCoolBarOptions;
    procedure SetIsVisible(AValue: Boolean);
  public
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
    property IsVisible: Boolean read FIsVisible write SetIsVisible;
    property Width: Integer read FWidth write FWidth;
  end;

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

function TIDEToolBarOptions.Equals(Opts: TIDEToolBarOptions): boolean;
begin
  Result := (FPosition = Opts.FPosition) and (FBreak = Opts.FBreak)
    and FButtonNames.Equals(Opts.FButtonNames);
end;

procedure TIDEToolBarOptions.Assign(Source: TIDEToolBarOptions);
begin
  FPosition := Source.FPosition;
  FBreak := Source.FBreak;
  FButtonNames.Assign(Source.FButtonNames);
end;

procedure TIDEToolBarOptions.Load(XMLConfig: TXMLConfig; SubPath: String);
var
  ButtonCount: Integer;
  ButtonName: string;
  I: Integer;
begin
  FBreak := XMLConfig.GetValue(SubPath + 'Break/Value', False);
  ButtonCount := XMLConfig.GetValue(SubPath + 'Count', 0);
  if ButtonCount = 0 then  // Old format
    ButtonCount := XMLConfig.GetValue(SubPath + 'ButtonCount/Value', 0);
  for I := 1 to ButtonCount do
  begin
    ButtonName := XMLConfig.GetValue(SubPath + 'Button' + IntToStr(I) + '/Name', '');
    if ButtonName = '' then  // Old format
      ButtonName := XMLConfig.GetValue(SubPath + 'Buttons/Name' + IntToStr(I) + '/Value', '');
    if ButtonName <> '' then
      FButtonNames.Add(ButtonName);
  end;
end;

procedure TIDEToolBarOptions.Save(XMLConfig: TXMLConfig; SubPath: String);
var
  I: Integer;
begin
  XMLConfig.SetDeleteValue(SubPath + 'Break/Value', FBreak, False);
  XMLConfig.SetDeleteValue(SubPath + 'Count', ButtonNames.Count, 0);
  for I := 0 to ButtonNames.Count-1 do
    XMLConfig.SetDeleteValue(SubPath + 'Button' + IntToStr(I+1) + '/Name', ButtonNames[I], '');
end;

{ TIDECoolBarOptions }
constructor TIDECoolBarOptions.Create;
begin
  inherited Create;
  FIDECoolBarToolBars := TIDEToolBarOptionList.Create;
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

function TIDECoolBarOptions.EqualToolbars(Opts: TIDECoolBarOptions): boolean;
var
  I: Integer;
begin
  Result := (FIDECoolBarToolBars.Count = Opts.FIDECoolBarToolBars.Count);
  if not Result then Exit;
  for I := 0 to FIDECoolBarToolBars.Count-1 do
    if not FIDECoolBarToolBars[I].Equals(Opts.FIDECoolBarToolBars[I]) then Exit(False);
end;

procedure TIDECoolBarOptions.CreateDefaultToolbars;
var
  ToolBarOpts: TIDEToolBarOptions;
begin
  //standard toolbar defaults
  ToolBarOpts := TIDEToolBarOptions.Create;
  ToolBarOpts.Position := 0;
  ToolBarOpts.Break := False;
  with ToolBarOpts.ButtonNames do
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
  FIDECoolBarToolBars.Add(ToolBarOpts);

  //debug toolbar defaults
  ToolBarOpts := TIDEToolBarOptions.Create;
  ToolBarOpts.Position := 1;
  ToolBarOpts.Break := True;
  with ToolBarOpts.ButtonNames do
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
  FIDECoolBarToolBars.Add(ToolBarOpts);
end;

procedure TIDECoolBarOptions.Load(XMLConfig: TXMLConfig);
var
  ToolBarOpt: TIDEToolBarOptions;
  ToolBarCount: Integer;
  I: Integer;
begin
  ToolbarCount := XMLConfig.GetValue(BasePath + 'Count', 0);
  if ToolBarCount = 0 then  // Old format
    ToolbarCount := XMLConfig.GetValue(BasePath + 'ToolBarCount/Value', 0);
  FIDECoolBarVisible := XMLConfig.GetValue(BasePath + 'Visible/Value', True);
  FIDECoolBarWidth := XMLConfig.GetValue(BasePath + 'Width/Value', 230);
  FIDECoolBarGrabStyle := XMLConfig.GetValue(BasePath + 'GrabStyle/Value', 1);
  FIDECoolBarGrabWidth := XMLConfig.GetValue(BasePath + 'GrabWidth/Value', 5);
  FIDECoolBarBorderStyle := XMLConfig.GetValue(BasePath + 'BorderStyle/Value', 1);
  if ToolBarCount > 0 then
  begin
    FIDECoolBarToolBars.Clear;
    for I := 0 to ToolbarCount-1 do
    begin
      ToolBarOpt := TIDEToolBarOptions.Create;
      FIDECoolBarToolBars.Add(ToolBarOpt);
      ToolBarOpt.FPosition := I;
      ToolBarOpt.Load(XMLConfig, BasePath + 'ToolBar' + IntToStr(I+1) + '/');
    end;
  end;
  if ToolBarCount = 0 then
    CreateDefaultToolbars;
end;

procedure TIDECoolBarOptions.Save(XMLConfig: TXMLConfig);
var
  DefaultOpts: TDefaultCoolBarOptions;
  I: Integer;
begin
  DefaultOpts := TDefaultCoolBarOptions.Create;
  try
    XMLConfig.DeletePath(BasePath);
    XMLConfig.SetDeleteValue(BasePath + 'Visible/Value', FIDECoolBarVisible, True);
    XMLConfig.SetDeleteValue(BasePath + 'Width/Value', FIDECoolBarWidth, 0);
    XMLConfig.SetDeleteValue(BasePath + 'GrabStyle/Value', FIDECoolBarGrabStyle, 1);
    XMLConfig.SetDeleteValue(BasePath + 'GrabWidth/Value', FIDECoolBarGrabWidth, 5);
    XMLConfig.SetDeleteValue(BasePath + 'BorderStyle/Value', FIDECoolBarBorderStyle, 1);
    if EqualToolbars(DefaultOpts) then Exit;
    if FIDECoolBarToolBars.Count > 0 then
    begin
      XMLConfig.SetDeleteValue(BasePath + 'Count', FIDECoolBarToolBars.Count, 0);
      for I := 0 to FIDECoolBarToolBars.Count - 1 do
        FIDECoolBarToolBars[I].Save(XMLConfig, BasePath + 'ToolBar' + IntToStr(I+1) + '/');
    end;
  finally
    DefaultOpts.Free;
  end;
end;

{ TDefaultCoolBarOptions }

constructor TDefaultCoolBarOptions.Create;
begin
  inherited Create;
  //coolbar defaults
  FIDECoolBarVisible := True;
  FIDECoolBarWidth := 230;
  FIDECoolBarGrabStyle := 1;
  FIDECoolBarGrabWidth := 5;
  FIDECoolBarBorderStyle := 1;
  //toolbar defaults
  CreateDefaultToolbars;
end;

destructor TDefaultCoolBarOptions.Destroy;
begin
  inherited Destroy;
end;

{ TIDEToolBar }
procedure TIDEToolBar.UpdateBar(Sender: TObject);
var
  I, J: Integer;
begin
  ToolBar.BeginUpdate;
  try
    for I := ToolBar.ButtonCount - 1 downto 0 do
    begin
      if ToolBar.Buttons[I].Tag <> 0 then
      begin
        J := ToolBar.Buttons[I].Tag - 1;
        if FButtonList[J] <> nil then
          ToolBar.Buttons[I].Enabled := FButtonList[J].Enabled;
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
  FButtonList := TIDEMenuItemList.Create;
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
  ACaption := ACaption + GetShortcut(AMenuItem);
  B.Hint        := ACaption;
  // If we have a image, us it. Otherwise supply a default.
  if AMenuItem.ImageIndex <> -1 then
    B.ImageIndex := AMenuItem.ImageIndex
  else
    B.ImageIndex := IDEImages.LoadImage(16, 'execute');

  B.Style       := tbsButton;
  if (AMenuItem.Name = 'itmFileNewForm') or (AMenuItem.Name = 'itmFileNewUnit') then
  begin
    B.PopupMenu := MainIDEBar.NewUnitFormPopupMenu;
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
    AName := FButtonNames[Index];
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

procedure TIDECoolBar.SetIsVisible(AValue: Boolean);
begin
  FIsVisible := AValue;
  if Assigned(FCoolBar) then
    FCoolBar.Visible := AValue;
end;

{ TIDECoolBar }
constructor TIDECoolBar.Create(ACoolBar: TCoolBar);
begin
  inherited Create;
  FCoolBar := ACoolBar;
  FCoolBarToolBars := TIDEToolBarList.Create;
  FDefaultOptions := TDefaultCoolBarOptions.Create;
  if Assigned(FCoolBar) then begin
    CopyFromOptions(FDefaultOptions);
    SetCoolBarDefaults;
    SetToolBarDefaults;
  end;
end;

destructor TIDECoolBar.Destroy;
begin
  FreeAndNil(FDefaultOptions);
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

  FCoolBar.GrabStyle := TGrabStyle(1);
  FCoolBar.GrabWidth := 5;
  FCoolBar.BandBorderStyle := bsSingle;
end;

procedure TIDECoolBar.SetToolBarDefaults;
begin
  CopyFromOptions(FDefaultOptions);
end;

procedure TIDECoolBar.CopyFromOptions(Options: TIDECoolBarOptions);
var
  I: Integer;
  IDEToolBar: TIDEToolBar;
begin
  FCoolBarToolBars.Clear;
  for I := 0 to Options.FIDECoolBarToolBars.Count - 1 do
  begin
    IDEToolBar := TIDEToolBar.Create;
    FCoolBarToolBars.Add(IDEToolBar);
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
  for I := 0 to FCoolbarToolBars.Count-1 do
  begin
    if ToolBars[I].Toolbar = Toolbar then
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
            (FCoolBar.GrabStyle = gsDouble) and (FCoolBar.GrabWidth = 5) and
            (FWidth = 230);
end;

function TIDECoolBar.IsDefaultToolbar: Boolean;
var
  TempOpts: TIDECoolBarOptions;
begin
  TempOpts := TIDECoolBarOptions.Create;
  try
    CopyToOptions(TempOpts);
    Result := TempOpts.EqualToolbars(FDefaultOptions);
  finally
    TempOpts.Free;
  end;
end;

end.
