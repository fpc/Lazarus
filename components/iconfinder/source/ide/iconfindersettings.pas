{
 **********************************************************************
  This file is part of a Lazarus Package, IconFinder.

  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 **********************************************************************

 Settings for the Icon Finder addon to the IDE:
 - add/remove folders with icons to be included
 - rearrange folders
 - define metadata (keywords, styles)
 - save keywords and styles to "metadata.xml" file in the icon folder.

 The settings are stored in the file "iconfindercfg.xml" in the 
 Lazarus user profile (primary config directory).
}

unit IconFinderSettings;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils,
  // LazUtils
  LazConfigStorage, LazFileUtils, LazLoggerBase,
  // LCL
  Controls, Graphics, Dialogs, ComCtrls, Forms,
  // BuildIntf
  IDEOptionsIntf, baseIDEIntf,
  // IdeIntf
  LazIDEIntf, IDEOptEditorIntf, IDEImagesIntf,
  // Icon Finder
  IconFinderStrConstsIDE, IconFinderCommon, IconThumbnails, IconViewer,
  IconFinderFolders, IconFinderMetadata;


{ TIconFinderSettings
  The Options Group ID, and, perhaps, a place in the Tree View }

type
  TIconFinderSettings = class(TAbstractIDEEnvironmentOptions) // needed by options group.
  private

  public
    destructor Destroy; override;
    class function GetGroupCaption: String; override;
    class function GetInstance: TAbstractIDEOptions; override;
    procedure DoAfterWrite({%H-}Restore: boolean); override;
  end;


{ TIconFinderSettingsFrame
  This is the frame displayed when the user clicks the Tree View node }

type
  TIconFinderSettingsFrame = class(TAbstractIDEOptionsEditor)
    ToolBar: TToolBar;
    tbFolders: TToolButton;
    tbEditMetadata: TToolButton;
    tbSaveMetadata: TToolButton;
    procedure tbFoldersClick(Sender: TObject);
    procedure tbEditMetadataClick(Sender: TObject);
    procedure tbSaveMetadataClick(Sender: TObject);
  private
    FViewer: TIconViewerFrame;
    procedure AddDefaultFolder;
    procedure CenterForm(AForm: TCustomForm);
    procedure EditFolders;
    procedure EditIconMetaData(AIcon: TIconItem);
    procedure IconViewerDblClick(Sender: TObject);
    procedure SaveMetadataFiles;

  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    function Check: Boolean; override;
    function GetTitle: String; override;
    procedure ReadSettings({%H-}AOptions: TAbstractIDEOptions); override;
    procedure Setup({%H-}ADialog: TAbstractOptionsEditorDialog); override;
    class function SupportedOptionsClass: TAbstractIDEOptionsClass; override;
    procedure WriteSettings({%H-}AOptions: TAbstractIDEOptions); override;
    procedure RestoreSettings({%H-}AOptions: TAbstractIDEOptions); override;
  end;

procedure ReadIconFinderSettings(AConfig: TConfigStorage; AViewer: TIconViewerFrame; ANodeName: String);
procedure WriteIconFinderSettings(AConfig: TConfigStorage; AViewer: TIconViewerFrame; ANodeName: String);

var
  IconFinderOptionsGroup: integer = 260;
  IconFinderOptionsIndex: Integer = 100;
  IconFinderOptionsFrameID: integer;    // Value obtained in registration


implementation

{$R *.lfm}

{ TIconFinderSettings }

destructor TIconFinderSettings.Destroy;
begin
  inherited Destroy;
end;

class function TIconFinderSettings.GetGroupCaption: String;
begin
  Result := RSIconFinderIDE_IconLibrary;
end;

class function TIconFinderSettings.GetInstance: TAbstractIDEOptions;
begin
  //result := TAbstractIDEOptions(self);    // Nope, it does not like that !
  result := nil;
end;

procedure TIconFinderSettings.DoAfterWrite(Restore: boolean);
begin
  inherited DoAfterWrite(Restore);
end;


{ TIconFinderSettingsFrame }

constructor TIconFinderSettingsFrame.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);

  FViewer := TIconViewerFrame.Create(self);
  FViewer.Align := alClient;
  FViewer.IconViewer.FocusedColor := clWindowText;
  FViewer.IconViewer.ThumbnailColor := clWindow;
  FViewer.ImageList := IDEImages.Images_16;
  FViewer.ImageIndex_ExecuteFilter := IDEImages.GetImageIndex('item_filter', 16);
  FViewer.ImageIndex_ClearFilter := IDEImages.GetImageIndex('menu_clean', 16);
  FViewer.OnIconDblClick := @IconViewerDblClick;
  FViewer.Parent := self;
  FViewer.UpdateLanguage;

  Toolbar.Images := IDEImages.Images_16;

  tbFolders.Caption := RSIconFinderIDE_Folders;
  tbFolders.Hint := RSIconFinderIDE_FolderHint;
  tbFolders.ImageIndex := IDEImages.GetImageIndex('laz_open');
  tbEditMetadata.Caption := RSIconFinderIDE_Metadata;
  tbEditMetadata.Hint := RSIconFinderIDE_MetadataHint;
  tbEditMetadata.ImageIndex := IDEImages.GetImageIndex('laz_edit');
  tbSaveMetadata.Caption := RSIconFinderIDE_SaveMetadata;
  tbSaveMetadata.Hint := RSIconFinderIDE_SaveMetadataHint;
  tbSaveMetadata.ImageIndex := IDEImages.GetImageIndex('laz_save');
end;

destructor TIconFinderSettingsFrame.Destroy;
begin
  inherited Destroy;
end;

procedure TIconFinderSettingsFrame.AddDefaultFolder;
var
  LazDir: String;
  i: Integer;
begin
  LazDir := AppendPathDelim(IDEEnvironmentOptions.GetParsedLazarusDirectory);
  for i := 0 to High(DEFAULT_IMAGE_FOLDERS) do
    FViewer.AddIconFolder(LazDir + DEFAULT_IMAGE_FOLDERS[i], i <> 0); // Only the 1st folder (general_purpose) is active
end;

procedure TIconFinderSettingsFrame.CenterForm(AForm: TCustomForm);
var
  P: TPoint;
begin
  P := ClientToScreen(Point(Left + (Width - AForm.Width) div 2, Top + (Height - AForm.Height) div 2));
  AForm.SetBounds(P.X, P.Y, AForm.Width, AForm.Height);
end;

function TIconFinderSettingsFrame.Check: Boolean;
begin
  Result := not FViewer.cmbFilterByKeywords.Focused;
end;

procedure TIconFinderSettingsFrame.EditFolders;
var
  F: TIconFolderForm;
  folders: TStrings;
begin
  F := TIconFolderForm.Create(nil);
  folders := TStringList.Create;
  try
    FViewer.IconViewer.WriteIconFolders(folders);
    CenterForm(F);
    F.SetIconFolders(folders);
    if F.ShowModal = mrOK then
    begin
      Screen.BeginWaitCursor;
      try
        F.GetIconFolders(folders);
        FViewer.ReadIconFolders(folders);
      finally
        Screen.EndWaitCursor;
      end;
    end;
  finally
    folders.Free;
    F.Free;
  end;
end;

{ Opens the metadata editor for specifying the keywords and the style of the
  currently selected icon. The data are copied to all icons sharing the same
  name base. }
procedure TIconFinderSettingsFrame.EditIconMetaData(AIcon: TIconItem);
var
  F: TIconMetadataForm;
begin
  if AIcon = nil then
    exit;

  F := TIconMetadataForm.Create(nil);
  try
    CenterForm(F);
    F.MetaDataToControls(AIcon);
    if F.ShowModal = mrOK then
    begin
      F.ControlsToMetaData(AIcon);
      FViewer.CopyMetadataToNameBase(AIcon);
    end;
  finally
    F.Free;
  end;
end;

function TIconFinderSettingsFrame.GetTitle: String;
begin
  Result := RSIconFinderIDE_General;
end;

procedure TIconFinderSettingsFrame.IconViewerDblClick(Sender: TObject);
begin
  EditIconMetadata(FViewer.IconViewer.SelectedIcon);
end;

procedure TIconFinderSettingsFrame.ReadSettings(AOptions: TAbstractIDEOptions);
var
  Config: TConfigStorage;
begin
  try
    Config := GetIDEConfigStorage(ICONFINDER_CONFIG_FILENAME, true);
    try
      ReadIconFinderSettings(Config, FViewer, 'IDEOptions');
      if FViewer.IconViewer.IconFolders.Count = 0 then
        AddDefaultFolder;
    finally
      Config.Free;
    end;
  except
    on E: Exception do begin
      DebugLn('[TIconFinderSettingsFrame.ReadSettings] Loading ' +  ICONFINDER_CONFIG_FILENAME + ' failed: ' + E.Message);
    end;
  end;
end;

// Maybe the initial settings before we have a config file ?  Labels and Captions.
procedure TIconFinderSettingsFrame.Setup(ADialog: TAbstractOptionsEditorDialog);
begin
  if FViewer.IconViewer.IconFolders.Count = 0 then
    AddDefaultFolder;
end;

class function TIconFinderSettingsFrame.SupportedOptionsClass: TAbstractIDEOptionsClass;
begin
  Result := nil;
end;

procedure TIconFinderSettingsFrame.tbFoldersClick(Sender: TObject);
begin
  EditFolders;
end;

procedure TIconFinderSettingsFrame.tbEditMetadataClick(Sender: TObject);
begin
  EditIconMetaData(FViewer.IconViewer.SelectedIcon);
end;

procedure TIconFinderSettingsFrame.tbSaveMetadataClick(Sender: TObject);
begin
  SaveMetadataFiles;
end;

procedure TIconFinderSettingsFrame.SaveMetadataFiles;
begin
  FViewer.IconViewer.WriteMetadataFiles;
end;

// Gets called whenever user opens Options tree.
procedure TIconFinderSettingsFrame.WriteSettings(AOptions: TAbstractIDEOptions);
var
  Config: TConfigStorage;
begin
  try
     Config := GetIDEConfigStorage(ICONFINDER_CONFIG_FILENAME, true);
     try
       WriteIconFinderSettings(Config, FViewer, 'IDEOptions');
     finally
       Config.Free;
     end;
   except
     on E: Exception do begin
       DebugLn('[TIconFinderSettingsFrame.ReadSettings] Saving ' + ICONFINDER_CONFIG_FILENAME + ' failed: ' + E.Message);
     end;
   end;
end;

procedure TIconFinderSettingsFrame.RestoreSettings(AOptions: TAbstractIDEOptions);
begin
  inherited RestoreSettings(AOptions);
end;

{------------------------------------------------------------------------------}

procedure ReadIconFinderSettings(AConfig: TConfigStorage; AViewer: TIconViewerFrame;
  ANodeName: String);
var
  folder: String;
  folderIdx: Integer;
  isHidden: Boolean;
  n, i: Integer;
  s: String;
  list: TStrings;
begin
  // Icon folder list
  n := AConfig.GetValue('IconFinder/Folders/Count', 0);
  for i := 0 to n-1 do
  begin
    folder := AConfig.GetValue(Format('IconFinder/Folders/Item%d/Value', [i]), '');
    isHidden := AConfig.GetValue(Format('IconFinder/Folders/Item%d/Hidden', [i]), false);
    if (folder <> '') and DirectoryExists(folder) then
    begin
      folderIdx := AViewer.IndexOfIconFolder(folder);
      if folderIdx = -1 then
        AViewer.AddIconFolder(folder, isHidden)
      else
        AViewer.IconViewer.IconFolders[folderIdx].Hidden := isHidden;
    end;
  end;

  // Keyword filter history list
  list := TStringList.Create;
  try
    n := AConfig.GetValue('IconFinder/FilterHistory/Count', 0);
    for i := 0 to n-1 do
    begin
      s := AConfig.GetValue(Format('IconFinder/FilterHistory/Item%d/Value', [i]), '');
      if s <> '' then list.Add(s);
    end;
    AViewer.SetKeywordsHistory(list);
  finally
    list.Free;
  end;

  // Read the icon size and style filter settings
  if (ANodeName <> '') and (ANodename[Length(ANodeName)] = '/') then Delete(ANodeName, Length(ANodeName), 1);
  if (ANodeName <> '') and (ANodeName[1] = '/') then Delete(ANodeName, 1, 1);
  if ANodeName <> '' then
  begin
    AViewer.SizeFilter := AConfig.GetValue(Format('IconFinder/%s/SizeFilter/Value', [ANodeName]), '');
    AViewer.StyleFilter := AConfig.GetValue(Format('IconFinder/%s/StyleFilter/Value', [ANodeName]), '');
  end;
end;

procedure WriteIconFinderSettings(AConfig: TConfigStorage; AViewer: TIconViewerFrame;
  ANodeName: String);
var
  i: Integer;
  list: TStrings;
  s, path: String;
  isHidden: Boolean;
begin
  list := TStringList.Create;
  try
    // Icon folder list
    AViewer.IconViewer.WriteIconFolders(list);
    AConfig.SetValue('IconFinder/Folders/Count', list.Count);
    for i := 0 to list.Count-1 do
    begin
      AConfig.SetValue(Format('IconFinder/Folders/Item%d/Value', [i]), list[i]);
      isHidden := list.Objects[i] <> nil;
      AConfig.SetValue(Format('IconFinder/Folders/Item%d/Hidden', [i]), isHidden);
    end;

    // Keyword filter history list
    list.Clear;
    AViewer.GetKeywordsHistory(list);
    AConfig.SetValue('IconFinder/FilterHistory/Count', list.Count);
    for i := 0 to list.Count-1 do
      AConfig.SetValue(Format('IconFinder/FilterHistory/Item%d/Value', [i]), list[i]);
  finally
    list.Free;
  end;

  // Write the icon size and style filter settings
  if (ANodeName <> '') and (ANodename[Length(ANodeName)] = '/') then Delete(ANodeName, Length(ANodeName), 1);
  if (ANodeName <> '') and (ANodeName[1] = '/') then Delete(ANodeName, 1, 1);
  if ANodeName <> '' then
  begin
    s := AViewer.SizeFilter;
    path := Format('IconFinder/%s/SizeFilter/Value', [ANodeName]);
    if s <> '' then
      AConfig.SetValue(path, s)
    else
      AConfig.DeleteValue(path);

    s := AViewer.StyleFilter;
    path := Format('IconFinder/%s/StyleFilter/Value', [ANodeName]);
    if s <> '' then
      AConfig.SetValue(path, s)
    else
      AConfig.DeleteValue(path);
  end;
end;


initialization
  IconFinderOptionsGroup := GetFreeIDEOptionsGroupIndex(GroupEditor);
  RegisterIDEOptionsGroup(IconFinderOptionsGroup, TIconFinderSettings, False);

end.
