unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, IniFiles,
  Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls, Buttons,
  IconViewer;

type

  { TMainForm }

  TMainForm = class(TForm)
    Bevel1: TBevel;
    cbIconFolder: TComboBox;
    ImageList: TImageList;
    lblIconFolder: TLabel;
    Panel1: TPanel;
    btnNavigateFolders: TSpeedButton;
    SelectDirectoryDialog: TSelectDirectoryDialog;
    procedure btnNavigateFoldersClick(Sender: TObject);
    procedure cbIconFolderCloseUp(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    FViewer: TIconViewerFrame;
    FFolderName: String;
    procedure LoadIconsFromFolder(AFolderName: String);
    procedure ReadIni;
    procedure WriteIni;

  public

  end;

var
  MainForm: TMainForm;

implementation

{$R *.lfm}

const
  MAX_FOLDER_COUNT = 16;

function GetIniName: String;
begin
  Result := ChangeFileExt(Application.ExeName, '.ini');
end;

{ TMainForm }

procedure TMainForm.FormCreate(Sender: TObject);
begin
  FViewer := TIconViewerFrame.Create(self);
  FViewer.Align := alClient;
  FViewer.Parent := self;
  FViewer.IconViewer.FocusedColor := clWindowText;
  FViewer.IconViewer.ThumbnailColor := clWindow;
  FViewer.ImageList := ImageList;
  FViewer.ImageIndex_ExecuteFilter := 1;
  FViewer.ImageIndex_ClearFilter := 0;
  FViewer.BorderSpacing.Top := 6;
//  FViewer.OnIconDblClick := @IconViewerDblClickHandler;
//  FViewer.OnFilter := @IconViewerFilterHandler;
  FViewer.Bevel2.Hide;

  ReadIni;
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  WriteIni;
end;

procedure TMainForm.btnNavigateFoldersClick(Sender: TObject);
begin
  if FFolderName <> '' then
    SelectDirectoryDialog.InitialDir := ExtractFileDir(FFolderName);
  if SelectDirectoryDialog.Execute then
    LoadIconsFromFolder(SelectDirectoryDialog.FileName);
end;

procedure TMainForm.cbIconFolderCloseUp(Sender: TObject);
begin
  if cbIconFolder.ItemIndex > -1 then
    LoadIconsFromFolder(cbIconFolder.Items[cbIconFolder.ItemIndex]);
end;

procedure TMainForm.LoadIconsFromFolder(AFolderName: String);
var
  L: TStrings;
  idx: Integer;
begin
  FViewer.IconViewer.Clear;

  FFolderName := AFolderName;
  Screen.BeginWaitCursor;
  try
    FViewer.AddIconFolder(FFolderName);
    FViewer.Invalidate;

    L := TStringList.Create;
    try
      L.Assign(cbIconFolder.Items);
      idx := L.IndexOf(FFolderName);
      if idx = -1 then
        L.Insert(0, FFolderName)
      else
        L.Move(idx, 0);
      while L.Count > MAX_FOLDER_COUNT do
        L.Delete(L.Count-1);

      cbIconFolder.Items.Assign(L);
      cbIconFolder.Text := FFolderName;
    finally
      L.Free;
    end;
  finally
    Screen.EndWaitCursor;
  end;
end;

procedure TMainForm.ReadIni;
var
  ini: TCustomIniFile;
  i: Integer;
  L: TStrings;
  s: String;
begin
  ini := TIniFile.Create(GetIniName);
  try
    L := TStringList.Create;
    try
      ini.ReadSection('FolderHistory', L);
      for i := 0 to L.Count-1 do
      begin
        s := ini.ReadString('FolderHistory', L[i], '');
        L[i] := s;
      end;
      for i := L.Count-1 downto 0 do
        if (L[i] = '') or not DirectoryExists(L[i]) then
          L.Delete(i);
      if L.Count > 0 then
      begin
        cbIconFolder.Items.Assign(L);
        FFolderName := L[0];
        cbIconFolder.Text := FFolderName;
        Screen.BeginWaitCursor;
        try
          FViewer.AddIconFolder(FFolderName);
        finally
          Screen.EndWaitCursor;
        end;
      end;
    finally
      L.Free;
    end;
  finally
    ini.Free;
  end;
end;

procedure TMainForm.WriteIni;
var
  ini: TCustomIniFile;
  i: Integer;
begin
  ini := TIniFile.Create(GetIniName);
  try
    ini.EraseSection('FolderHistory');
    for i := 0 to cbIconFolder.Items.Count-1 do
      ini.WriteString('FolderHistory', 'Item' + IntToStr(i), cbIconFolder.Items[i]);
  finally
    ini.Free;
  end;
end;

end.

