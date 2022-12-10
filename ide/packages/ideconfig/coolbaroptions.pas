unit CoolBarOptions;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, fgl, ToolBarOptionsBase, Laz2_XMLCfg;

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
    procedure CopyPosFromBandValues(APosIndex: Integer; ABreak: Boolean);
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

procedure TIDEToolBarOptions.CopyPosFromBandValues(APosIndex: Integer; ABreak: Boolean);
begin
  FPosIndex := APosIndex;
  FBreak := ABreak;
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

end.

