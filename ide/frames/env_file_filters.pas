unit env_file_filters;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Grids, Dialogs, Controls,
  LCLProc, LCLType, Menus, StdCtrls, LazConfigStorage,
  IDEOptionsIntf, BaseIDEIntf,
  EnvironmentOpts, IDEOptionDefs, LazarusIDEStrConsts;

const
  FileDialogFilterConfigFile = 'filefilters.xml';
type

  { TFileFiltersOptionsFrame }

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
    fLoaded: boolean;
    fSaved: boolean;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    function GetTitle: String; override;
    procedure Setup(ADialog: TAbstractOptionsEditorDialog); override;
    procedure ReadSettings(AOptions: TAbstractIDEOptions); override;
    procedure WriteSettings(AOptions: TAbstractIDEOptions); override;
    class function SupportedOptionsClass: TAbstractIDEOptionsClass; override;
  end;


procedure LoadFileDialogFilter;
procedure SaveFileDialogFilter;
function GetDefaultFileDialogFilter: string;


implementation

{$R *.lfm}

const
  KeyFilter         = 'Filter';
  KeyFilterCount    = 'Count';
  KeyFilterName     = 'Name';
  KeyFilterMask     = 'Mask';


procedure LoadFileDialogFilter;
const
  cFilter = '%s (%s)|%s|';            // each filter is seperated by another | sign
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
    if cnt = 0 then
    begin
      // create default values
      Filter:=GetDefaultFileDialogFilter;
    end
    else
    begin
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
    end;
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
    cfg.SetValue(KeyFilterCount, Cnt);
    cfg.WriteToDisk;
  finally
    cfg.Free;
  end;
end;

function GetDefaultFileDialogFilter: string;
begin
  Result := lisLazarusUnit + ' (*.pas;*.pp)|*.pas;*.pp'
    + '|' + lisLazarusProject + ' (*.lpi)|*.lpi'
    + '|' + lisLazarusForm + ' (*.lfm;*.dfm)|*.lfm;*.dfm'
    + '|' + lisLazarusPackage + ' (*.lpk)|*.lpk'
    + '|' + lisLazarusProjectSource + ' (*.lpr)|*.lpr'
    + '|' + lisLazarusOtherFile + ' (*.inc;*.lrs;*.lpl)|*.inc;*.lrs;*.lpl';
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

  procedure AddRowItem(const ARow: integer; const AName, AMask: String);
  begin
    grdFileFilters.Cells[1, ARow] := AName;
    grdFileFilters.Cells[2, ARow] := AMask;
  end;

  procedure ReadList(Filter: String; var Cnt: integer; Scan: boolean);
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
            if not Scan then
              AddRowItem(Cnt,CurCaption,CurMask);
          end;
          if p^='|' then break;
        until p^=#0;
        inc(p);
      end;
    end;

  end;

var
  Filter: String;
  Cnt: Integer;
begin
  if fLoaded then exit;
  fLoaded:=true;

  Filter:=EnvironmentOptions.FileDialogFilter;
  Cnt:=0;
  ReadList(Filter,Cnt,true);
  grdFileFilters.RowCount := Cnt+2; // +1 for header, +1 for a new item
  ReadList(Filter,Cnt,false);
end;

procedure TFileFiltersOptionsFrame.WriteSettings(AOptions: TAbstractIDEOptions);
var
  Filter: String;
  i: Integer;
  CurCaption: String;
  CurMask: String;
begin
  if fSaved then exit;
  fSaved:=true;

  Filter:='';
  for i := 1 to grdFileFilters.RowCount-1 do
  begin
    CurCaption:=grdFileFilters.Cells[1, i];
    CurMask:=grdFileFilters.Cells[2, i];
    CurCaption:=StringReplace(CurCaption,'|',',',[rfReplaceAll]);
    CurMask:=StringReplace(CurMask,'|',',',[rfReplaceAll]);
    if (CurCaption='') or (CurMask='') then continue;
    if Filter<>'' then
      Filter:=Filter+'|';
    Filter:=Filter+CurCaption+'|'+CurMask;
  end;

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

