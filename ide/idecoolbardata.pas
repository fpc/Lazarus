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
 Abstract:
   The implementation of IDE Coolbar.
 ToDo:
   Extract an interface from here and put it to IdeIntf package.
}

unit IdeCoolbarData;


{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fgl,
  // LCL
  LCLProc, ComCtrls, Controls, Graphics, Dialogs, ToolWin,
  // LazUtils
  Laz2_XMLCfg,
  // IdeIntf
  IDEImagesIntf,
  // IDE
  LazarusIDEStrConsts, ToolbarConfig;

type

  // Option classes take care of saving / loading environment options data.
  // They don't contain LCL components.

  { TIDEToolBarOptions }

  TIDEToolBarOptions = class(TIDEToolBarOptionsBase)
  private
    FPosIndex: Integer;
    FBreak: Boolean;
  public
    //constructor Create;
    //destructor Destroy; override;
    function Equals(Opts: TIDEToolBarOptions): boolean; overload;
    procedure Assign(Source: TIDEToolBarOptions);
    procedure CopyPosFromBand(Band: TCoolBand);
    procedure Load(XMLConfig: TXMLConfig; SubPath: String);
    procedure Save(XMLConfig: TXMLConfig; SubPath: String);
  published
    property Break: Boolean read FBreak write FBreak;
    property PosIndex: Integer read FPosIndex write FPosIndex;
  end;


  { TIDEToolBarOptionList }

  Ttbo = specialize TFPGObjectList<TIDEToolBarOptions>;
  TIDEToolBarOptionList = class(Ttbo)
    procedure Assign(Source: TIDEToolBarOptionList);
  end;

  { TIDECoolBarOptions }

  TIDECoolBarOptions = class
  private
    FVisible: Boolean;
    FWidth: Integer;
    FGrabStyle: Integer;
    FGrabWidth: Integer;
    FBorderStyle: Integer; //TFormBorderStyle;
    FToolBars: TIDEToolBarOptionList;
    procedure CreateDefaultToolbars;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    procedure Assign(Source: TIDECoolBarOptions);
    function EqualToolbars(Opts: TIDECoolBarOptions): boolean;
    procedure Load(XMLConfig: TXMLConfig; Path: String);
    procedure Save(XMLConfig: TXMLConfig; Path: String);
  public
    property Visible: Boolean read FVisible write FVisible;
    property Width: Integer read FWidth write FWidth;
    property GrabStyle: Integer read FGrabStyle write FGrabStyle;
    property GrabWidth: Integer read FGrabWidth write FGrabWidth;
    property BorderStyle: Integer read FBorderStyle write FBorderStyle;
    property ToolBars: TIDEToolBarOptionList read FToolBars;
  end;

  { TDefaultCoolBarOptions }

  TDefaultCoolBarOptions = class(TIDECoolBarOptions)
  public
    constructor Create;
    destructor Destroy; override;
  end;

  // Actual Coolbar and its member Toolbars

  TOnToolBarClick = procedure(Sender: TObject) of object;

  { TIDEToolBar }

  TIDEToolBar = class(TIDEToolbarBase)
   private
     FCurrentOptions: TIDEToolBarOptions;
     FOnToolbarClick: TOnToolBarClick;
     procedure DoToolBarClick(Sender: TObject);
   public
     constructor Create(AOwner: TComponent); override;
     destructor Destroy; override;
     procedure ClearToolbar;
     procedure UseCurrentOptions;
   public
     property CurrentOptions: TIDEToolBarOptions read FCurrentOptions;
     property OnToolBarClick: TOnToolbarClick read FOnToolbarClick write FOnToolbarClick;
   end;

  TIDEToolBarList = specialize TFPGObjectList<TIDEToolBar>;

  { TIDECoolBar }

  TIDECoolBar = class
  private
    FCoolBar: TCoolBar;  // The actual CoolBar, not owned by this class.
    FCoolBarToolBars: TIDEToolBarList;
    FIsVisible: Boolean; //cannot hide/show the coolbar on toolbar_options, instead we use a variable
    FWidth: Integer;     //same as Isvisible
    // Used for assigning and testing the default configuration.
    FDefaultOptions: TDefaultCoolBarOptions;
    procedure DisableToolbarButtons(IDEToolbar: TIDEToolBar);
    procedure SetIsVisible(AValue: Boolean);
    procedure ToolBarClick(Sender: TObject);
  public
    constructor Create(ACoolBar: TCoolBar);
    destructor Destroy; override;
    procedure SetCoolBarDefaults;
    procedure SetToolBarDefaults;
    procedure CopyFromRealCoolbar(RealCoolbar: TCoolBar);
    procedure CopyFromOptions(Options: TIDECoolBarOptions);
    procedure CopyToOptions(Options: TIDECoolBarOptions);
    function Add: TIDEToolBar;
    function AddBand(ToolBar: TToolBar; aBreak: Boolean): TCoolBand;
    procedure AddExtra;
    procedure Delete;
    function FindByToolBar(const aToolBar: TToolBar): Integer;
    procedure Config;
    procedure Sort;
    procedure PopulateToolBar;
    function GetSelectedBand: Integer;
    function IsDefaultCoolbar: Boolean;
    function IsDefaultToolbar: Boolean;
    procedure SelectBand(const ID: integer);
    procedure SelectBandAtXY(X, Y: integer);
  public
    property ToolBars: TIDEToolBarList read FCoolBarToolBars;
    property CoolBar: TCoolBar read FCoolBar;
    property IsVisible: Boolean read FIsVisible write SetIsVisible;
    property Width: Integer read FWidth write FWidth;
  end;

var
  IDECoolBar: TIDECoolBar;

implementation

const
  BasePath = 'IDECoolBarOptions/';

{ TIDEToolBarOptions }

function TIDEToolBarOptions.Equals(Opts: TIDEToolBarOptions): boolean;
begin
  Result := inherited Equals(Opts)
      and (FPosIndex = Opts.FPosIndex) and (FBreak = Opts.FBreak);
end;

procedure TIDEToolBarOptions.Assign(Source: TIDEToolBarOptions);
begin
  inherited Assign(Source);
  FPosIndex := Source.FPosIndex;
  FBreak := Source.FBreak;
end;

procedure TIDEToolBarOptions.CopyPosFromBand(Band: TCoolBand);
begin
  FPosIndex := Band.Index;
  FBreak := Band.Break;
end;

procedure TIDEToolBarOptions.Load(XMLConfig: TXMLConfig; SubPath: String);
begin
  FBreak := XMLConfig.GetValue(SubPath + 'Break/Value', False);
  LoadButtonNames(XMLConfig, SubPath);
end;

procedure TIDEToolBarOptions.Save(XMLConfig: TXMLConfig; SubPath: String);
begin
  XMLConfig.SetDeleteValue(SubPath + 'Break/Value', FBreak, False);
  SaveButtonNames(XMLConfig, SubPath);
end;

{ TIDEToolBarOptionList }

procedure TIDEToolBarOptionList.Assign(Source: TIDEToolBarOptionList);
var
  tbo: TIDEToolBarOptions;
  i: Integer;
begin
  Clear;
  for i := 0 to Source.Count-1 do
  begin
    tbo := TIDEToolBarOptions.Create;
    tbo.Assign(Source[i]);
    Add(tbo);
  end;
end;

{ TIDECoolBarOptions }
constructor TIDECoolBarOptions.Create;
begin
  inherited Create;
  FToolBars := TIDEToolBarOptionList.Create;
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

procedure TIDECoolBarOptions.Assign(Source: TIDECoolBarOptions);
begin
  FVisible := Source.FVisible;
  FWidth := Source.FWidth;
  FGrabStyle := Source.FGrabStyle;
  FGrabWidth := Source.FGrabWidth;
  FBorderStyle := Source.FBorderStyle;
  FToolBars.Assign(Source.FToolBars);
end;

function TIDECoolBarOptions.EqualToolbars(Opts: TIDECoolBarOptions): boolean;
var
  I: Integer;
begin
  Result := (FToolBars.Count = Opts.FToolBars.Count);
  if not Result then Exit;
  for I := 0 to FToolBars.Count-1 do
    if not FToolBars[I].Equals(Opts.FToolBars[I]) then Exit(False);
end;

procedure TIDECoolBarOptions.CreateDefaultToolbars;
var
  ToolBarOpts: TIDEToolBarOptions;
begin
  //standard toolbar defaults
  ToolBarOpts := TIDEToolBarOptions.Create;
  ToolBarOpts.PosIndex := 0;
  ToolBarOpts.Break := False;
  with ToolBarOpts.ButtonNames do
  begin
    Add('NewUnit');
    Add('NewForm');
    Add(cIDEToolbarDivider);
    Add('Open');
    Add('Save');
    Add('SaveAll');
    Add(cIDEToolbarDivider);
    Add('Toggle between Unit and Form');
    Add(cIDEToolbarDivider);
    Add('Manage desktops');
  end;
  FToolBars.Add(ToolBarOpts);

  //debug toolbar defaults
  ToolBarOpts := TIDEToolBarOptions.Create;
  ToolBarOpts.PosIndex := 1;
  ToolBarOpts.Break := True;
  with ToolBarOpts.ButtonNames do
  begin
    Add('View Units');
    Add('View Forms');
    Add(cIDEToolbarDivider);
    Add('Change build mode');
    Add('Run program');
    Add('Pause program');
    Add('Stop program');
    Add('Step over');
    Add('Step into');
    Add('Step out');
  end;
  FToolBars.Add(ToolBarOpts);
end;

procedure TIDECoolBarOptions.Load(XMLConfig: TXMLConfig; Path: String);
var
  ToolBarOpt: TIDEToolBarOptions;
  ToolBarCount: Integer;
  I: Integer;
begin
  Path := Path + BasePath;
  ToolbarCount := XMLConfig.GetValue(Path + 'Count', 0);
  if ToolBarCount = 0 then  // Old format
    ToolbarCount := XMLConfig.GetValue(Path + 'ToolBarCount/Value', 0);
  FVisible := XMLConfig.GetValue(Path + 'Visible/Value', True);
  FWidth := XMLConfig.GetValue(Path + 'Width/Value', 230);
  FGrabStyle := XMLConfig.GetValue(Path + 'GrabStyle/Value', 1);
  FGrabWidth := XMLConfig.GetValue(Path + 'GrabWidth/Value', 5);
  FBorderStyle := XMLConfig.GetValue(Path + 'BorderStyle/Value', 1);
  if ToolBarCount > 0 then
  begin
    FToolBars.Clear;
    for I := 0 to ToolbarCount-1 do
    begin
      ToolBarOpt := TIDEToolBarOptions.Create;
      FToolBars.Add(ToolBarOpt);
      ToolBarOpt.PosIndex := I;
      ToolBarOpt.Load(XMLConfig, Path + 'ToolBar' + IntToStr(I+1) + '/');
    end;
  end;
  if ToolBarCount = 0 then
    CreateDefaultToolbars;
end;

procedure TIDECoolBarOptions.Save(XMLConfig: TXMLConfig; Path: String);
var
  DefaultOpts: TDefaultCoolBarOptions;
  I: Integer;
begin
  DefaultOpts := TDefaultCoolBarOptions.Create;
  try
    Path := Path + BasePath;
    XMLConfig.DeletePath(Path);
    XMLConfig.SetDeleteValue(Path + 'Visible/Value', FVisible, True);
    XMLConfig.SetDeleteValue(Path + 'Width/Value', FWidth, 0);
    XMLConfig.SetDeleteValue(Path + 'GrabStyle/Value', FGrabStyle, 1);
    XMLConfig.SetDeleteValue(Path + 'GrabWidth/Value', FGrabWidth, 5);
    XMLConfig.SetDeleteValue(Path + 'BorderStyle/Value', FBorderStyle, 1);
    if EqualToolbars(DefaultOpts) then Exit;
    if FToolBars.Count > 0 then
    begin
      XMLConfig.SetDeleteValue(Path + 'Count', FToolBars.Count, 0);
      for I := 0 to FToolBars.Count - 1 do
        FToolBars[I].Save(XMLConfig, Path + 'ToolBar' + IntToStr(I+1) + '/');
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
  FVisible := True;
  FWidth := 230;
  FGrabStyle := 1;
  FGrabWidth := 5;
  FBorderStyle := 1;
  //toolbar defaults
  CreateDefaultToolbars;
end;

destructor TDefaultCoolBarOptions.Destroy;
begin
  inherited Destroy;
end;

{ TIDEToolBar }

constructor TIDEToolBar.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FToolBar := TToolbar.Create(nil);
  with FToolBar do
  begin
    ButtonHeight := 22;
    ButtonWidth := 22;
    Height := 22;
    Width := 0;
    Flat := True;
    AutoSize := True;
    Transparent := True;
    EdgeInner := esNone;
    EdgeOuter := esNone;
    Images := IDEImages.Images_16;
    ShowHint := True;
    OnClick := @DoToolBarClick;
  end;
  FCurrentOptions := TIDEToolBarOptions.Create;
end;

destructor TIDEToolBar.Destroy;
begin
  FCurrentOptions.Free;
  FToolBar.Free;
  inherited Destroy;
end;

procedure TIDEToolBar.ClearToolbar;
var
  i: Integer;
begin
  FToolBar.BeginUpdate;
  try
    for i := FToolBar.ButtonCount - 1 downto 0 do
      FToolBar.Buttons[i].Free
  finally
    FToolBar.EndUpdate;
  end;
end;

procedure TIDEToolBar.UseCurrentOptions;
begin
  ClearToolbar;
  CopyFromOptions(FCurrentOptions);
end;

procedure TIDEToolBar.DoToolBarClick(Sender: TObject);
begin
  if Assigned(FOnToolbarClick) then
    FOnToolbarClick(FToolbar);
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

procedure TIDECoolBar.SetIsVisible(AValue: Boolean);
begin
  FIsVisible := AValue;
  if Assigned(FCoolBar) then
    FCoolBar.Visible := AValue;
end;

procedure TIDECoolBar.ToolBarClick(Sender: TObject);
var
  CoolBand: TCoolBand;
begin
  CoolBand := FCoolbar.Bands.FindBand(Sender as TToolBar);
  if CoolBand <> nil then
    SelectBand(CoolBand.Index);
end;

procedure TIDECoolBar.SetCoolBarDefaults;
begin
  FCoolBar.Vertical := False;
  FCoolBar.HorizontalSpacing := 1;
  FCoolBar.VerticalSpacing := 3;
  FCoolBar.FixedSize := True;
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

procedure TIDECoolBar.CopyFromRealCoolbar(RealCoolbar: TCoolBar);
var
  ToolBar: TToolBar;
  I, J: Integer;
begin
  for I := 0 to RealCoolbar.Bands.Count - 1 do
  begin
    if RealCoolbar.Bands[I].Control = nil then
      Continue;
    ToolBar := (RealCoolbar.Bands[I].Control as TToolBar);
    J := FindByToolBar(ToolBar);
    if J <> -1 then
      ToolBars[J].CurrentOptions.CopyPosFromBand(RealCoolbar.Bands[I]);
  end;
  Sort;
end;

procedure TIDECoolBar.CopyFromOptions(Options: TIDECoolBarOptions);
var
  I: Integer;
  IDEToolBar: TIDEToolBar;
begin
  FCoolBarToolBars.Clear;
  for I := 0 to Options.FToolBars.Count - 1 do
  begin
    IDEToolBar := Add;
    IDEToolBar.CurrentOptions.PosIndex := I;
    IDEToolBar.CurrentOptions.Break := Options.FToolBars[I].Break;
    IDEToolBar.CurrentOptions.ButtonNames.Assign(Options.FToolBars[I].ButtonNames);
  end;
end;

procedure TIDECoolBar.CopyToOptions(Options: TIDECoolBarOptions);
var
  I: Integer;
  Opt: TIDEToolBarOptions;
begin
  Options.FToolBars.Clear;
  for I := 0 to FCoolBarToolBars.Count - 1 do
  begin
    Opt := TIDEToolBarOptions.Create;
    Options.FToolBars.Add(Opt);
    Opt.PosIndex := FCoolBarToolBars[I].CurrentOptions.PosIndex;
    Opt.Break := FCoolBarToolBars[I].CurrentOptions.Break;
    Opt.ButtonNames.Assign(FCoolBarToolBars[I].CurrentOptions.ButtonNames);
  end;
end;

function TIDECoolBar.Add: TIDEToolBar;
begin
  Result := TIDEToolBar.Create(Nil);
  FCoolBarToolBars.Add(Result);
end;

function TIDECoolBar.AddBand(ToolBar: TToolBar; aBreak: Boolean): TCoolBand;
begin
  Result := FCoolBar.Bands.Add;
  Result.Break := aBreak;
  Result.Control := Toolbar;
  //Result.MinWidth := 25;
  //Result.MinHeight := 22;
  Result.FixedSize := True;
end;

procedure TIDECoolBar.AddExtra;
var
  IDEToolbar: TIDEToolBar;
begin
  IDEToolbar := Add;
  IDEToolbar.CurrentOptions.Break := False;
  IDEToolbar.OnToolBarClick := @ToolBarClick;
  IDEToolbar.ToolBar.DisabledImages := IDEToolbar.ToolBar.Images;
  SelectBand(AddBand(IDEToolbar.ToolBar, True).Index);
end;

procedure TIDECoolBar.Delete;
var
  I: integer;
  ToDelete: integer;
begin
  if FCoolbar.Bands.Count = 1 then
  begin
    MessageDlg(lisCoolbarDeleteWarning, mtInformation, [mbOk], 0);
    Exit;
  end;
  ToDelete := GetSelectedBand;
  if ToDelete > -1 then
  begin
    if MessageDlg(lisCoolbarDeleteToolBar, mtConfirmation, [mbYes, mbNo], 0) = mrYes then
    begin
      if ToDelete < FCoolBar.Bands.Count-1 then
        SelectBand(ToDelete + 1)
      else if ToDelete > 0 then
        SelectBand(ToDelete - 1);
      I := FindByToolBar((FCoolBar.Bands.Items[ToDelete].Control as TToolBar));
      Assert(I = ToDelete, 'TIDECoolBar.Delete: index mismatch.');
      ToolBars.Delete(ToDelete);
    end;
  end;
end;

function TIDECoolBar.FindByToolBar(const aToolBar: TToolBar): Integer;
var
  I: Integer;
begin
  for I := 0 to FCoolbarToolBars.Count-1 do
    if ToolBars[I].ToolBar = aToolBar then
      Exit(I);
  Result := -1;
end;

procedure TIDECoolBar.Config;
var
  ToConfig: Integer;
  ToolBar: TToolBar;
  IDEToolbar: TIDEToolBar;
begin
  ToConfig := GetSelectedBand;
  if ToConfig = -1 then
  begin
    MessageDlg(lisCoolbarSelectToolBar, mtInformation, [mbOk], 0);
    Exit;
  end;
  ToolBar := FCoolbar.Bands.Items[ToConfig].Control as TToolBar;
  Assert(Assigned(ToolBar), 'TIDECoolBar.Config: ToolBar=Nil.');
  Assert(ToConfig=FindByToolBar(ToolBar), 'TIDECoolBar.Config: Indices differ!');
  IDEToolbar := ToolBars[ToConfig];
  if ShowToolBarConfig(IDEToolbar.CurrentOptions.ButtonNames) = mrOK then
    DisableToolbarButtons(IDEToolbar);
  FCoolbar.AutosizeBands;
end;

function Compare(const Item1, Item2: TIDEToolBar): Integer;
begin
  Result := Item1.CurrentOptions.PosIndex - Item2.CurrentOptions.PosIndex;
end;

procedure TIDECoolBar.Sort;
begin
  FCoolbarToolBars.Sort(@Compare);
end;

procedure TIDECoolBar.DisableToolbarButtons(IDEToolbar: TIDEToolBar);
var
  I: Integer;
begin
  IDEToolbar.UseCurrentOptions;
  for I := 0 to Pred(IDEToolbar.ToolBar.ButtonCount) do
    IDEToolbar.ToolBar.Buttons[I].Enabled := False;
end;

procedure TIDECoolBar.PopulateToolBar;
var
  I: Integer;
  IDEToolbar: TIDEToolBar;
begin
  FCoolBar.Bands.Clear;
  for I := 0 to ToolBars.Count - 1 do
  begin
    IDEToolbar := ToolBars[I];
    IDEToolbar.OnToolBarClick := @ToolBarClick;
    IDEToolbar.ToolBar.DisabledImages := IDEToolbar.ToolBar.Images;
    AddBand(IDEToolbar.ToolBar, IDEToolbar.CurrentOptions.Break);
    DisableToolbarButtons(IDEToolbar);
  end;
  if FCoolBar.Bands.Count > 0 then
    SelectBand(0);
  FCoolbar.AutosizeBands;
end;

function TIDECoolBar.GetSelectedBand: Integer;
var
  I: Integer;
begin
  for I := 0 to FCoolBar.Bands.Count - 1 do
    if FCoolBar.Bands.Items[I].Color = clHighlight then
      Exit(I);
  Result := -1;
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

procedure TIDECoolBar.SelectBand(const ID: integer);
var
  I: integer;
  Band: TCoolBand;
begin
  FCoolbar.Color := clDefault;
  for I := 0 to FCoolBar.Bands.Count - 1 do
  begin
    Band := FCoolBar.Bands.Items[I];
    if I <> ID then
    begin
      Band.Color := clDefault;
      Band.Control.Color := clDefault;
    end
    else
    begin
      Band.Color := clHighlight;
      Band.Control.Color := clHighLight;
    end;
  end;
end;

procedure TIDECoolBar.SelectBandAtXY(X, Y: integer);
var
  ABand: integer;
  AGrabber: boolean;
begin
  FCoolBar.MouseToBandPos(X, Y, ABand, AGrabber);
  if ABand < 0 then
    Exit;
  if FCoolBar.Bands.Items[ABand].Color <> clHighlight then
    SelectBand(ABand);
end;

end.
