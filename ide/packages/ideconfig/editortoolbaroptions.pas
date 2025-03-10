unit EditorToolBarOptions;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  // LazUtils
  Laz2_XMLCfg, LazConfigStorage, LazLoggerBase,
  // BuildIntf
  BaseIDEIntf,
  // IdeConfig
  ToolBarOptionsBase;

type

  { TEditorToolBarOptions }

  TEditorToolBarOptions = class(TIDEToolBarOptionsBase)
  private
    FVisible: Boolean;
    FPosition: string;
  public
    const
      cDefaultVisible = true;
      cDefaultPosition = 'Top';
  public
    constructor Create;
    //destructor Destroy; override;
    procedure Clear;
    function Equals(Opts: TEditorToolBarOptions): boolean; overload;
    procedure Assign(Source: TEditorToolBarOptions);
    procedure CreateDefaults;
    procedure Load(XMLConfig: TXMLConfig; Path: String);
    procedure Save(XMLConfig: TXMLConfig; Path: String);
  published
    property Visible: Boolean read FVisible write FVisible;
    property Position: string read FPosition write FPosition;
  end;


implementation

const
  BasePath = 'EditorToolBarOptions/';
  cSettingsFile = 'editortoolbar.xml';


{ TEditorToolBarOptions }

constructor TEditorToolBarOptions.Create;
begin
  inherited Create;
  FVisible := cDefaultVisible;
  FPosition := cDefaultPosition;
  CreateDefaults;
end;

procedure TEditorToolBarOptions.Clear;
begin
  inherited Clear;
  FVisible := cDefaultVisible;
  FPosition := cDefaultPosition;
end;

function TEditorToolBarOptions.Equals(Opts: TEditorToolBarOptions): boolean;
begin
  Result := inherited Equals(Opts)
      and (FVisible = Opts.FVisible) and (FPosition = Opts.FPosition);
end;

procedure TEditorToolBarOptions.Assign(Source: TEditorToolBarOptions);
begin
  inherited Assign(Source);
  FVisible := Source.FVisible;
  FPosition := Source.FPosition;
end;

procedure TEditorToolBarOptions.CreateDefaults;
begin
  ButtonNames.Clear;
  ButtonNames.Add('IDEMainMenu/Search/itmJumpings/itmJumpToSection/itmJumpToImplementation');
  ButtonNames.Add('IDEMainMenu/Search/itmJumpings/itmJumpBack');
  ButtonNames.Add('IDEMainMenu/Search/itmJumpings/itmJumpForward');
  ButtonNames.Add(cIDEToolbarDivider);
end;

procedure TEditorToolBarOptions.Load(XMLConfig: TXMLConfig; Path: String);
var
  ButtonCount: Integer;
  ButtonName: string;
  I: Integer;
  cfg: TConfigStorage;
begin
  Path := Path + BasePath;
  if XMLConfig.HasPath(Path + 'Count', True) then
  begin
    FVisible := XMLConfig.GetValue(Path + 'Visible', cDefaultVisible);
    FPosition := XMLConfig.GetValue(Path + 'Position', cDefaultPosition);
    LoadButtonNames(XMLConfig, Path);
  end
  else begin
    // Plan B: Load the old configuration. User settings are not lost.
    cfg := GetIDEConfigStorage(cSettingsFile, True);
    try
      FVisible := cfg.GetValue('Visible',True);
      FPosition := cfg.GetValue('Position','Top');
      ButtonCount := cfg.GetValue('Count', 0);
      if ButtonCount > 0 then
      begin
        DebugLn('TEditorToolBarOptions.Load: Using old configuration in editortoolbar.xml.');
        // This used to be hard-coded in old version, add it now.
        ButtonNames.Add('IDEMainMenu/Search/itmJumpings/itmJumpToSection/itmJumpToImplementation');
        for I := 1 to ButtonCount do
        begin
          ButtonName := Trim(cfg.GetValue('Button' + Format('%2.2d', [I]) + '/Value', ''));
          if ButtonName <> '' then
            ButtonNames.Add(ButtonName);
        end;
      end
      else   // No old configuration, use defaults.
        CreateDefaults;
    finally
      cfg.Free;
    end;
  end;
end;

procedure TEditorToolBarOptions.Save(XMLConfig: TXMLConfig; Path: String);
begin
  Path := Path + BasePath;
  XMLConfig.SetDeleteValue(Path + 'Visible', FVisible, cDefaultVisible);
  XMLConfig.SetDeleteValue(Path + 'Position', FPosition, cDefaultPosition);
  SaveButtonNames(XMLConfig, Path);
end;

end.

