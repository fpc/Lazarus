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
  // IdeIntf
  IDEImagesIntf,
  // IdeConfig
  CoolBarOptions,
  // IDE
  LazarusIDEStrConsts, ToolbarConfig;

type

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
  Band: TCoolBand;
  I, J: Integer;
begin
  for I := 0 to RealCoolbar.Bands.Count - 1 do
  begin
    if RealCoolbar.Bands[I].Control = nil then
      Continue;
    ToolBar := (RealCoolbar.Bands[I].Control as TToolBar);
    J := FindByToolBar(ToolBar);
    if J <> -1 then
    begin
      Band := RealCoolbar.Bands[I];
      ToolBars[J].CurrentOptions.CopyPosFromBandValues(Band.Index, Band.Break);
    end;
  end;
  Sort;
end;

procedure TIDECoolBar.CopyFromOptions(Options: TIDECoolBarOptions);
var
  I: Integer;
  IDEToolBar: TIDEToolBar;
begin
  FCoolBarToolBars.Clear;
  for I := 0 to Options.ToolBars.Count - 1 do
  begin
    IDEToolBar := Add;
    IDEToolBar.CurrentOptions.PosIndex := I;
    IDEToolBar.CurrentOptions.Break := Options.ToolBars[I].Break;
    IDEToolBar.CurrentOptions.ButtonNames.Assign(Options.ToolBars[I].ButtonNames);
  end;
end;

procedure TIDECoolBar.CopyToOptions(Options: TIDECoolBarOptions);
var
  I: Integer;
  Opt: TIDEToolBarOptions;
begin
  Options.ToolBars.Clear;
  for I := 0 to FCoolBarToolBars.Count - 1 do
  begin
    Opt := TIDEToolBarOptions.Create;
    Options.ToolBars.Add(Opt);
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
