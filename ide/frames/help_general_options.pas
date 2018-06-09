unit help_general_options;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  // LCL
  Forms, Controls, StdCtrls, ExtCtrls, EditBtn,
  // IdeIntf
  IDEOptionsIntf, IDEOptEditorIntf, ObjectInspector, LazHelpIntf,
  // IDE
  HelpOptions, LazarusIDEStrConsts, EnvironmentOpts;

type

  { THelpGeneralOptionsFrame }

  THelpGeneralOptionsFrame = class(TAbstractIDEOptionsEditor)
    Bevel1: TBevel;
    Bevel2: TBevel;
    Bevel3: TBevel;
    Bevel4: TBevel;
    Bevel5: TBevel;
    Bevel6: TBevel;
    DatabasesListBox: TListBox;
    DataBasesPropsGroupBox: TGroupBox;
    FPCDocHTMLEdit: TDirectoryEdit;
    FPCDocHTMLLabel: TLabel;
    lblMiddle1: TLabel;
    lblMiddle: TLabel;
    lblViewers: TLabel;
    lblDatabases: TLabel;
    ViewerPropsGroupBox: TGroupBox;
    ViewersListBox: TListBox;
    procedure DatabasesListBoxSelectionChange(Sender: TObject; {%H-}User: boolean);
    procedure ViewersListBoxSelectionChange(Sender: TObject; {%H-}User: boolean);
  private
    ViewersPropertiesGrid: TCustomPropertiesGrid;
    DatabasesPropertiesGrid: TCustomPropertiesGrid;
    procedure FillViewersList;
    procedure FillViewerPropGrid;
    procedure FillDatabasesList;
    procedure FillDatabasesPropGrid;
  public
    constructor Create(AOwner: TComponent); override;
    function GetTitle: String; override;
    procedure Setup({%H-}ADialog: TAbstractOptionsEditorDialog); override;
    procedure ReadSettings(AOptions: TAbstractIDEOptions); override;
    procedure WriteSettings(AOptions: TAbstractIDEOptions); override;
    class function SupportedOptionsClass: TAbstractIDEOptionsClass; override;
  end;

implementation

{$R *.lfm}

{ THelpGeneralOptionsFrame }

procedure THelpGeneralOptionsFrame.DatabasesListBoxSelectionChange(
  Sender: TObject; User: boolean);
begin
  FillDatabasesPropGrid;
end;

procedure THelpGeneralOptionsFrame.ViewersListBoxSelectionChange(
  Sender: TObject; User: boolean);
begin
  FillViewerPropGrid;
end;

procedure THelpGeneralOptionsFrame.FillViewersList;
var
  i: Integer;
  Viewer: THelpViewer;
begin
  if (HelpViewers = nil) then
  begin
    ViewersListBox.Items.Clear;
    Exit;
  end;
  ViewersListBox.Items.BeginUpdate;
  for i := 0 to HelpViewers.Count - 1 do
  begin
    Viewer := HelpViewers[i];
    if ViewersListBox.Items.Count > i then
      ViewersListBox.Items[i] := Viewer.GetLocalizedName
    else
      ViewersListBox.Items.Add(Viewer.GetLocalizedName);
  end;
  while ViewersListBox.Items.Count>HelpViewers.Count do
    ViewersListBox.Items.Delete(ViewersListBox.Items.Count - 1);
  if (ViewersListBox.ItemIndex < 0) and (ViewersListBox.Items.Count > 0) then
    ViewersListBox.ItemIndex := 0;
  ViewersListBox.Items.EndUpdate;
end;

procedure THelpGeneralOptionsFrame.FillViewerPropGrid;
var
  i: LongInt;
begin
  i := ViewersListBox.ItemIndex;
  if (HelpViewers = nil) or (i < 0) or (i >= HelpViewers.Count) then
    ViewersPropertiesGrid.TIObject := nil
  else
    ViewersPropertiesGrid.TIObject := HelpViewers[i];
end;

procedure THelpGeneralOptionsFrame.FillDatabasesList;
var
  i: Integer;
  HelpDB: THelpDatabase;
begin
  if (HelpDatabases = nil) then
  begin
    DatabasesListBox.Items.Clear;
    Exit;
  end;
  DatabasesListBox.Items.BeginUpdate;
  for i := 0 to HelpDatabases.Count - 1 do
  begin
    HelpDB := HelpDatabases[i];
    if DatabasesListBox.Items.Count > i then
      DatabasesListBox.Items[i] := HelpDB.GetLocalizedName
    else
      DatabasesListBox.Items.Add(HelpDB.GetLocalizedName);
  end;
  while DatabasesListBox.Items.Count>HelpDatabases.Count do
    DatabasesListBox.Items.Delete(DatabasesListBox.Items.Count - 1);
  if (DatabasesListBox.ItemIndex < 0) and (DatabasesListBox.Items.Count > 0) then
    DatabasesListBox.ItemIndex := 0;
  DatabasesListBox.Items.EndUpdate;
end;

procedure THelpGeneralOptionsFrame.FillDatabasesPropGrid;
var
  i: LongInt;
begin
  i := DatabasesListBox.ItemIndex;
  if (HelpDatabases = nil) or (i < 0) or (i>=HelpDatabases.Count) then
    DatabasesPropertiesGrid.TIObject := nil
  else
    DatabasesPropertiesGrid.TIObject := HelpDatabases[i];
end;

constructor THelpGeneralOptionsFrame.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ViewersPropertiesGrid := TCustomPropertiesGrid.Create(Self);
  with ViewersPropertiesGrid do
  begin
    Name := 'ViewersPropertiesGrid';
    Parent := ViewerPropsGroupBox;
    Align := alClient;
    BorderSpacing.Around := 6;
    PreferredSplitterX := 150;
  end;

  DatabasesPropertiesGrid := TCustomPropertiesGrid.Create(Self);
  with DatabasesPropertiesGrid do
  begin
    Name := 'DatabasesPropertiesGrid';
    Parent := DataBasesPropsGroupBox;
    Align := alClient;
    BorderSpacing.Around := 6;
    PreferredSplitterX := 150;
  end;
end;

function THelpGeneralOptionsFrame.GetTitle: String;
begin
  Result := lisHlpOptsHelpOptions;
end;

procedure THelpGeneralOptionsFrame.Setup(ADialog: TAbstractOptionsEditorDialog);
begin
  FPCDocHTMLLabel.Caption := lisHOFPCDocHTMLPath;
  lblViewers.Caption := lisHlpOptsViewers;
  ViewerPropsGroupBox.Caption := lisHlpOptsProperties;
  lblDatabases.Caption := lisHlpOptsDatabases;
  DataBasesPropsGroupBox.Caption := lisHlpOptsProperties;

  with EnvironmentOptions.ObjectInspectorOptions do
  begin
    AssignTo(ViewersPropertiesGrid);
    AssignTo(DatabasesPropertiesGrid);
  end;
end;

procedure THelpGeneralOptionsFrame.ReadSettings(AOptions: TAbstractIDEOptions);
begin
  with AOptions as THelpOptions do
  begin
    FPCDocHTMLEdit.Text := FPCDocsHTMLDirectory;
  end;
  FillViewersList;
  FillViewerPropGrid;
  FillDatabasesList;
  FillDatabasesPropGrid;
end;

procedure THelpGeneralOptionsFrame.WriteSettings(AOptions: TAbstractIDEOptions);
begin
  with AOptions as THelpOptions do
  begin
    FPCDocsHTMLDirectory := FPCDocHTMLEdit.Text;
  end;
end;

class function THelpGeneralOptionsFrame.SupportedOptionsClass: TAbstractIDEOptionsClass;
begin
  Result := THelpOptions;
end;

initialization
  RegisterIDEOptionsEditor(GroupHelp, THelpGeneralOptionsFrame, HlpOptionsGeneral);

end.

