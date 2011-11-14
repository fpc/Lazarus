unit env_file_filters;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Grids,
  EnvironmentOpts, IDEOptionDefs,
  IDEOptionsIntf, Controls, Menus, StdCtrls;

type
  TFileFiltersOptionsFrame = class(TAbstractIDEOptionsEditor)
    grdFileFilters: TStringGrid;
    pmGrid: TPopupMenu;
    pmiAddRow: TMenuItem;
    pmiDelRow: TMenuItem;
    pmiInsRow: TMenuItem;
    lblTitle: TLabel;
    procedure grdFileFiltersKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure pmiAddRowClick(Sender: TObject);
    procedure pmiDelRowClick(Sender: TObject);
    procedure pmiInsRowClick(Sender: TObject);
  private
    FList: TStringList;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    function GetTitle: String; override;
    procedure Setup(ADialog: TAbstractOptionsEditorDialog); override;
    procedure ReadSettings(AOptions: TAbstractIDEOptions); override;
    procedure WriteSettings(AOptions: TAbstractIDEOptions); override;
    class function SupportedOptionsClass: TAbstractIDEOptionsClass; override;
  end;


procedure LoadFileFiltersList;


implementation

{$R *.lfm}

uses
  LazarusIDEStrConsts,
  BaseIDEIntf,
  LazConfigStorage,
  LCLType;

const
  cSettingsFile     = 'filefilters.xml';
  KeyFilter         = 'Filter%2.2d';
  KeyFilterCount    = 'Count';
  KeyFilterName     = 'Name';
  KeyFilterMask     = 'Mask';


procedure LoadFileFiltersList;
const
  cFilter = '%s (%s)|%s|';            // each filter is seperated by another | sign
var
  cfg: TConfigStorage;
  c: integer;
  i: integer;
  lName, lMask: string;
begin
  EnvironmentOptions.FileFilters.Clear;
  cfg := GetIDEConfigStorage(cSettingsFile, True);
  try
    c := cfg.GetValue(KeyFilterCount, 0);
    if c = 0 then
    begin
      // create default values
      EnvironmentOptions.FileFilters.Text:=
            lisLazarusUnit + ' (*.pas;*.pp)|*.pas;*.pp'
            + '|' + lisLazarusProject + ' (*.lpi)|*.lpi'
            + '|' + lisLazarusForm + ' (*.lfm;*.dfm)|*.lfm;*.dfm'
            + '|' + lisLazarusPackage + ' (*.lpk)|*.lpk'
            + '|' + lisLazarusProjectSource + ' (*.lpr)|*.lpr';
    end
    else
    begin
      // read values
      for i := 1 to c do
      begin
        lName := cfg.GetValue(Format(KeyFilter, [i]) + '/' + KeyFilterName, '');
        lMask := cfg.GetValue(Format(KeyFilter, [i]) + '/' +  KeyFilterMask, '*');
        EnvironmentOptions.FileFilters.Add(Format(cFilter, [lName, lMask, lMask]));
      end;
    end;
  finally
    cfg.Free;
  end;
end;

{ TFileFiltersOptionsFrame }

procedure TFileFiltersOptionsFrame.grdFileFiltersKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_INSERT then
    grdFileFilters.RowCount := grdFileFilters.RowCount + 1;
end;

procedure TFileFiltersOptionsFrame.pmiAddRowClick(Sender: TObject);
begin
  grdFileFilters.RowCount := grdFileFilters.RowCount + 1;
end;

procedure TFileFiltersOptionsFrame.pmiDelRowClick(Sender: TObject);
begin
  grdFileFilters.DeleteColRow(False, grdFileFilters.Row);
end;

procedure TFileFiltersOptionsFrame.pmiInsRowClick(Sender: TObject);
begin
  grdFileFilters.InsertColRow(False, grdFileFilters.Row);
end;

constructor TFileFiltersOptionsFrame.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  FList := TStringList.Create;
end;

destructor TFileFiltersOptionsFrame.Destroy;
begin
  FList.Free;
  inherited Destroy;
end;

function TFileFiltersOptionsFrame.GetTitle: String;
begin
  Result := lisFileFilters;
end;

procedure TFileFiltersOptionsFrame.Setup(ADialog: TAbstractOptionsEditorDialog);
begin
  lblTitle.Caption := lisFileFiltersTitle;
  grdFileFilters.DefaultColWidth := 40;
  grdFileFilters.RowCount := 1;

  grdFileFilters.Columns[0].Title.Caption := lisFileFiltersName;
  grdFileFilters.Columns[1].Title.Caption := lisFileFiltersMask;

  pmiAddRow.Caption := lisFileFiltersAddRow;
  pmiDelRow.Caption := lisFileFiltersDeleteRow;
  pmiInsRow.Caption := lisFileFiltersInsertRow;
end;

procedure TFileFiltersOptionsFrame.ReadSettings(AOptions: TAbstractIDEOptions);
var
  cfg: TConfigStorage;
  c: integer;
  i: integer;
  lName, lMask: string;

  procedure AddRowItem(const ARow: integer; const AName, AMask: String);
  begin
    grdFileFilters.Cells[1, ARow] := AName;
    grdFileFilters.Cells[2, ARow] := AMask;
  end;

begin
  grdFileFilters.RowCount := 1;   { don't call Clear because it will remove fixed columns too }
  cfg := GetIDEConfigStorage(cSettingsFile, True);
  try
    c := cfg.GetValue(KeyFilterCount, 0);
    if c = 0 then
    begin
      // create default vaulues
      grdFileFilters.RowCount := grdFileFilters.RowCount + 7;
      AddRowItem(1, lisLazarusFile, '*.lpi;*.lpr;*.lpk;*.pas;*.pp;*.inc;*.lfm;*.dfm');
      AddRowItem(2, lisLazarusUnit, '*.pas;*.pp');
      AddRowItem(3, lisLazarusProject, '*.lpi');
      AddRowItem(4, lisLazarusForm, '*.lfm;*.dfm');
      AddRowItem(5, lisLazarusPackage, '*.lpk');
      AddRowItem(6, lisLazarusProjectSource, '*.lpr');
    end
    else
    begin
      // read values
      grdFileFilters.RowCount := c+1;
      for i := 1 to c do
      begin
        lName := cfg.GetValue(Format(KeyFilter, [i]) + '/' + KeyFilterName, 'N' + IntToStr(i));  // N1, N2 etc if no name specified
        lMask := cfg.GetValue(Format(KeyFilter, [i]) + '/' +  KeyFilterMask, AllFilesMask);
        AddRowItem(i, lName, lMask);
      end;
    end;
  finally
    cfg.Free;
  end;
  LoadFileFiltersList;
end;

procedure TFileFiltersOptionsFrame.WriteSettings(AOptions: TAbstractIDEOptions);
var
  cfg: TConfigStorage;
  i: integer;
begin
  cfg := GetIDEConfigStorage(cSettingsFile, False);
  try
    cfg.SetValue(KeyFilterCount, grdFileFilters.RowCount-1);
    for i := 1 to grdFileFilters.RowCount-1 do
    begin
      cfg.SetValue(Format(KeyFilter, [i]) + '/' + KeyFilterName, grdFileFilters.Cells[1, i]);
      cfg.SetValue(Format(KeyFilter, [i]) + '/' + KeyFilterMask, grdFileFilters.Cells[2, i]);
    end;
    cfg.WriteToDisk;
  finally
    cfg.Free;
  end;
  LoadFileFiltersList;
end;

class function TFileFiltersOptionsFrame.SupportedOptionsClass: TAbstractIDEOptionsClass;
begin
  Result := TEnvironmentOptions;
end;

initialization
  RegisterIDEOptionsEditor(GroupEnvironment, TFileFiltersOptionsFrame, EnvOptionsFileFilters);

end.

