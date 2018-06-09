unit env_file_filters;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  // LazUtils
  LazFileUtils, LazConfigStorage,
  // LCL
  LCLProc, LCLType, LResources, Forms, Grids, Dialogs, Controls, StdCtrls, Menus,
  // IdeIntf
  IDEOptionsIntf, IDEOptEditorIntf, BaseIDEIntf, IDEDialogs,
  // IDE
  EnvironmentOpts, IDEOptionDefs, LazarusIDEStrConsts;

const
  FileDialogFilterConfigFile = 'filefilters.xml';
type

  { TFileFiltersOptionsFrame }

  TFileFiltersOptionsFrame = class(TAbstractIDEOptionsEditor)
    grdFileFilters: TStringGrid;
    MenuItem1: TMenuItem;
    SetDefaultMenuItem: TMenuItem;
    pmGrid: TPopupMenu;
    pmiAddRow: TMenuItem;
    pmiDelRow: TMenuItem;
    pmiInsRow: TMenuItem;
    lblTitle: TLabel;
    procedure grdFileFiltersKeyDown(Sender: TObject; var Key: Word; {%H-}Shift: TShiftState);
    procedure pmiAddRowClick(Sender: TObject);
    procedure pmiDelRowClick(Sender: TObject);
    procedure pmiInsRowClick(Sender: TObject);
    procedure SetDefaultMenuItemClick(Sender: TObject);
  private
    FList: TStringList;
    fLoaded: boolean;
    fSaved: boolean;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    function GetTitle: String; override;
    procedure Setup({%H-}ADialog: TAbstractOptionsEditorDialog); override;
    procedure ReadSettings({%H-}AOptions: TAbstractIDEOptions); override;
    procedure WriteSettings({%H-}AOptions: TAbstractIDEOptions); override;
    class function SupportedOptionsClass: TAbstractIDEOptionsClass; override;
  end;


procedure LoadFileDialogFilter;
procedure SaveFileDialogFilter;
function GetDefaultFileDialogFilter: string;
function GetFileDialogFilterFromGrid(Grid: TStringGrid): string;
procedure LoadGridFromFileDialogFilter(Grid: TStringGrid; Filter: string;
                                       AddEmptyRow: boolean);

implementation

{$R *.lfm}

const
  KeyFilter         = 'Filter';
  KeyFilterCount    = 'Count';
  KeyFilterName     = 'Name';
  KeyFilterMask     = 'Mask';


procedure LoadFileDialogFilter;
var
  cfg: TConfigStorage;
  cnt: integer;
  i: integer;
  lName, lMask: string;
  Filter: String;
begin
  Filter:='';
  cfg := GetIDEConfigStorage(FileDialogFilterConfigFile, True);
  try
    cnt := cfg.GetValue(KeyFilterCount, 0);
    // read values
    for i := 1 to cnt do
    begin
      lName := cfg.GetValue(KeyFilter+IntToStr(i) + '/' + KeyFilterName, '');
      lMask := cfg.GetValue(KeyFilter+IntToStr(i) + '/' + KeyFilterMask, '*');
      if (lName='') or (lMask='') then continue;
      if Filter<>'' then
        Filter:=Filter+'|';
      Filter:=Filter+lName+'|'+lMask;
    end;
    if Filter='' then
      Filter:=GetDefaultFileDialogFilter;
  finally
    cfg.Free;
  end;
  EnvironmentOptions.FileDialogFilter:=Filter;
end;

procedure SaveFileDialogFilter;
var
  cfg: TConfigStorage;
  Cnt: Integer;
  p: PChar;
  MaskStart: PChar;
  CaptionStart: PChar;
  Filter: String;
  Caption: String;
  CurMask: String;
begin
  Filter:=EnvironmentOptions.FileDialogFilter;
  if Filter=GetDefaultFileDialogFilter then
    Filter:='';
  cfg := GetIDEConfigStorage(FileDialogFilterConfigFile,false);
  try
    Cnt:=0;
    if Filter<>'' then begin
      p:=PChar(Filter);
      while p^<>#0 do
      begin
        // caption
        CaptionStart:=p;
        while not (p^ in ['|',#0]) do inc(p);
        if p^=#0 then break;
        Caption:=copy(Filter,CaptionStart-PChar(Filter)+1,p-CaptionStart);
        // parse masks
        inc(p);
        MaskStart:=p;
        while not (p^ in ['|',#0]) do inc(p);
        if p>MaskStart then begin
          inc(Cnt);
          CurMask:=copy(Filter,MaskStart-PChar(Filter)+1,p-MaskStart);
          cfg.SetValue(KeyFilter+IntToStr(Cnt) + '/' + KeyFilterName, Caption);
          cfg.SetValue(KeyFilter+IntToStr(Cnt) + '/' + KeyFilterMask, CurMask);
        end;
        inc(p);
      end;
    end;
    if Cnt>0 then begin
      cfg.SetValue(KeyFilterCount, Cnt);
      cfg.WriteToDisk;
    end else if FileExistsUTF8(cfg.GetFilename) then begin
      // delete unneeded file
      DeleteFileUTF8(cfg.GetFilename);
    end;
  finally
    cfg.Free;
  end;
end;

function GetDefaultFileDialogFilter: string;
begin
  Result := dlgFilterLazarusUnit + ' (*.pas;*.pp)|*.pas;*.pp'
    + '|' + dlgFilterLazarusProject + ' (*.lpi)|*.lpi'
    + '|' + dlgFilterLazarusForm + ' (*.lfm;*.dfm)|*.lfm;*.dfm'
    + '|' + dlgFilterLazarusPackage + ' (*.lpk)|*.lpk'
    + '|' + dlgFilterLazarusProjectSource + ' (*.lpr)|*.lpr'
    + '|' + dlgFilterLazarusOtherFile + ' (*.inc;*.lrs;*.lpl)|*.inc;*.lrs;*.lpl';
end;

function GetFileDialogFilterFromGrid(Grid: TStringGrid): string;
var
  i: Integer;
  CurCaption: String;
  CurMask: String;
begin
  Result:='';
  for i := 1 to Grid.RowCount-1 do
  begin
    CurCaption:=Grid.Cells[1, i];
    CurMask:=Grid.Cells[2, i];
    CurCaption:=StringReplace(CurCaption,'|',',',[rfReplaceAll]);
    CurMask:=StringReplace(CurMask,'|',',',[rfReplaceAll]);
    if (CurCaption='') or (CurMask='') then continue;
    if Result<>'' then
      Result:=Result+'|';
    Result:=Result+CurCaption+'|'+CurMask;
  end;
end;

procedure LoadGridFromFileDialogFilter(Grid: TStringGrid; Filter: string;
  AddEmptyRow: boolean);

  procedure ReadList(var Cnt: integer; Scan: boolean);
  var
    p: PChar;
    CaptionStart: PChar;
    CurCaption: String;
    MaskStart: PChar;
    CurMask: String;
  begin
    Cnt:=0;
    if Filter<>'' then begin
      p:=PChar(Filter);
      while p^<>#0 do
      begin
        // caption
        CaptionStart:=p;
        while not (p^ in ['|',#0]) do inc(p);
        if p^=#0 then break;
        CurCaption:=copy(Filter,CaptionStart-PChar(Filter)+1,p-CaptionStart);
        // parse masks
        repeat
          inc(p);
          MaskStart:=p;
          while not (p^ in ['|',#0]) do inc(p);
          if p>MaskStart then begin
            CurMask:=copy(Filter,MaskStart-PChar(Filter)+1,p-MaskStart);
            inc(Cnt);
            if not Scan then begin
              Grid.Cells[1, Cnt] := CurCaption;
              Grid.Cells[2, Cnt] := CurMask;
            end;
          end;
          if p^='|' then break;
        until p^=#0;
        inc(p);
      end;
    end;

  end;

var
  Cnt: Integer;
begin
  Cnt:=0;
  ReadList(Cnt,true);
  Grid.BeginUpdate;
  try
    inc(Cnt,Grid.FixedRows);
    if AddEmptyRow then inc(Cnt);
    Grid.RowCount := Cnt;
    if AddEmptyRow then begin
      Grid.Cells[1, Cnt-1] := '';
      Grid.Cells[2, Cnt-1] := '';
    end;
    ReadList(Cnt,false);
  finally
    Grid.EndUpdate(true);
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

procedure TFileFiltersOptionsFrame.SetDefaultMenuItemClick(Sender: TObject);
begin
  if IDEMessageDialog(lisConfirm,
    lisResetAllFileFiltersToDefaults, mtConfirmation, [mbCancel, mbOK])<>mrOk
  then exit;
  LoadGridFromFileDialogFilter(grdFileFilters,GetDefaultFileDialogFilter,false);
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

  grdFileFilters.Columns[0].Title.Caption := lisName;
  grdFileFilters.Columns[1].Title.Caption := lisFileFiltersMask;

  pmiAddRow.Caption := lisFileFiltersAddRow;
  pmiDelRow.Caption := lisFileFiltersDeleteRow;
  pmiInsRow.Caption := lisFileFiltersInsertRow;

  SetDefaultMenuItem.Caption := lisFileFiltersSetDefaults;
end;

procedure TFileFiltersOptionsFrame.ReadSettings(AOptions: TAbstractIDEOptions);
begin
  if fLoaded then exit;
  fLoaded:=true;

  LoadGridFromFileDialogFilter(grdFileFilters,EnvironmentOptions.FileDialogFilter,false);
end;

procedure TFileFiltersOptionsFrame.WriteSettings(AOptions: TAbstractIDEOptions);
var
  Filter: String;
begin
  if fSaved then exit;
  fSaved:=true;

  Filter:=GetFileDialogFilterFromGrid(grdFileFilters);

  if EnvironmentOptions.FileDialogFilter<>Filter then begin
    //debugln(['TFileFiltersOptionsFrame.WriteSettings ']);
    EnvironmentOptions.FileDialogFilter:=Filter;
    SaveFileDialogFilter;
  end;
end;

class function TFileFiltersOptionsFrame.SupportedOptionsClass: TAbstractIDEOptionsClass;
begin
  Result := TEnvironmentOptions;
end;

initialization
  RegisterIDEOptionsEditor(GroupEnvironment, TFileFiltersOptionsFrame, EnvOptionsFileFilters);

end.

