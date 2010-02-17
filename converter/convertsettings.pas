unit ConvertSettings;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  StdCtrls, EditBtn, Buttons, ExtCtrls, DialogProcs;

type

  { TConvertSettings }

  TConvertSettings = class
  private
    fTitle: String;       // Used for form caption.
    // Unit, Project or Package top file and path.
    fMainFilename: String;
    fMainPath: String;
    // Actual user settings.
    fBackupFiles: boolean;
    fKeepDelphiCompatible: boolean;
    fAutoMissingProperties: boolean;
    fAutoMissingComponents: boolean;
    function GetBackupPath: String;
    procedure SetMainFilename(const AValue: String);
  public
    constructor Create(const ATitle: string);
    destructor Destroy; override;
    function RunForm: TModalResult;

    // Lazarus file name based on Delphi file name, keep suffix.
    function DelphiToLazFilename(const DelphiFilename: string;
      LowercaseFilename: boolean): string; overload;
    // Lazarus file name based on Delphi file name with new suffix.
    function DelphiToLazFilename(const DelphiFilename, LazExt: string;
      LowercaseFilename: boolean): string; overload;

    // Create Lazarus file name and copy/rename from Delphi file, keep suffix.
    function RenameDelphiToLazFile(const DelphiFilename: string;
      out LazFilename: string; LowercaseFilename: boolean): TModalResult; overload;
    // Create Lazarus file name and copy/rename from Delphi file with new suffix.
    function RenameDelphiToLazFile(const DelphiFilename, LazExt: string;
      out LazFilename: string; LowercaseFilename: boolean): TModalResult; overload;

    function RenameFile(const SrcFilename, DestFilename: string): TModalResult;
    function BackupFile(const AFilename: string): TModalResult;
  public
    property MainFilename: String read fMainFilename write SetMainFilename;
    property MainPath: String read fMainPath;
    property BackupPath: String read GetBackupPath;

    property BackupFiles: boolean read fBackupFiles;
    property KeepDelphiCompatible: boolean read fKeepDelphiCompatible;
    property AutoMissingProperties: boolean read fAutoMissingProperties;
    property AutoMissingComponents: boolean read fAutoMissingComponents;
  end;


  { TConvertSettingsForm }

  TConvertSettingsForm = class(TForm)
    BackupCheckBox: TCheckBox;
    ReplacementCompsButton: TBitBtn;
    btnCancel: TBitBtn;
    btnOK: TBitBtn;
    BtnPanel: TPanel;
    DelphiCompatibleCheckBox: TCheckBox;
    HelpButton: TBitBtn;
    MainPathEdit: TLabeledEdit;
    SettingsGroupBox: TGroupBox;
    MissingStuffGroupBox: TGroupBox;
    MissingStuffLabel: TLabel;
    MissingComponentCheckBox: TCheckBox;
    MissingPropertyCheckBox: TCheckBox;
    procedure btnOKClick(Sender: TObject);
    procedure ReplacementCompsButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
  public

  end; 

var
  ConvertSettingsForm: TConvertSettingsForm;

implementation


{$R *.lfm}

{ TConvertSettings }

constructor TConvertSettings.Create(const ATitle: string);
begin
  fTitle:=ATitle;
  fMainFilename:='';
  fMainPath:='';
end;

destructor TConvertSettings.Destroy;
begin
  inherited Destroy;
end;

function TConvertSettings.RunForm: TModalResult;
var
  SettingsForm: TConvertSettingsForm;
begin
  SettingsForm:=TConvertSettingsForm.Create(nil);
  with SettingsForm do
  try
    Caption:=fTitle;
    MainPathEdit.Text:=fMainPath;
{
    // ToDo: Load from XML.
    // Settings --> UI.
    BackupCheckBox.Checked          :=fBackupFiles;
    DelphiCompatibleCheckBox.Checked:=fKeepDelphiCompatible;
    MissingPropertyCheckBox.Checked :=fAutoMissingProperties;
    MissingComponentCheckBox.Checked:=fAutoMissingComponents;
}
    Result:=ShowModal;
    if Result=mrOK then begin
      // UI --> Settings.
      fBackupFiles:=BackupCheckBox.Checked;
      fKeepDelphiCompatible:=DelphiCompatibleCheckBox.Checked;
      fAutoMissingProperties:=MissingPropertyCheckBox.Checked;
      fAutoMissingComponents:=MissingComponentCheckBox.Checked;
      // ToDo: Save to XML.
    end;
  finally
    Free;
  end;
end;

function TConvertSettings.DelphiToLazFilename(const DelphiFilename: string;
                                              LowercaseFilename: boolean): string;
begin
  Result:=DelphiToLazFilename(DelphiFilename,'',LowercaseFilename);
end;

function TConvertSettings.DelphiToLazFilename(const DelphiFilename, LazExt: string;
                                              LowercaseFilename: boolean): string;
var
  RelPath, SubPath, fn: string;
begin
  RelPath:=FileUtil.CreateRelativePath(DelphiFilename, fMainPath);
  SubPath:=ExtractFilePath(RelPath);
  if LazExt='' then                 // Include ext in filename if not defined.
    fn:=ExtractFileName(RelPath)
  else
    fn:=ExtractFileNameOnly(RelPath);
  if LowercaseFilename then
    fn:=LowerCase(fn);
  Result:=fMainPath+SubPath+fn+LazExt;
end;

function TConvertSettings.RenameDelphiToLazFile(const DelphiFilename: string;
  out LazFilename: string; LowercaseFilename: boolean): TModalResult;
begin
  Result:=RenameDelphiToLazFile(DelphiFilename,'',LazFilename,LowercaseFilename);
end;

function TConvertSettings.RenameDelphiToLazFile(const DelphiFilename, LazExt: string;
  out LazFilename: string; LowercaseFilename: boolean): TModalResult;
var
  RelPath, SubPath, fn: string;
begin
  RelPath:=FileUtil.CreateRelativePath(DelphiFilename, fMainPath);
  SubPath:=ExtractFilePath(RelPath);
  if LazExt='' then                 // Include ext in filename if not defined.
    fn:=ExtractFileName(RelPath)
  else
    fn:=ExtractFileNameOnly(RelPath);
  if LowercaseFilename then
    fn:=LowerCase(fn);
  // Rename in the same directory.
  if fBackupFiles then begin
    Result:=BackupFile(DelphiFilename); // Save before rename.
    if Result<>mrOK then exit;
  end;
  LazFilename:=fMainPath+SubPath+fn+LazExt;
  Result:=RenameFileWithErrorDialogs(DelphiFilename,LazFilename,[mbAbort]);
end;

function TConvertSettings.RenameFile(const SrcFilename, DestFilename: string): TModalResult;
begin
  Result:=mrOK;
  // Convert in place.
  if fBackupFiles then
    BackupFile(SrcFilename); // Save before rename.
  Result:=RenameFileWithErrorDialogs(SrcFilename,DestFilename,[mbAbort]);
end;

function TConvertSettings.BackupFile(const AFilename: string): TModalResult;
var
  bp, fn: String;
begin
  bp:=BackupPath;
  fn:=ExtractFileName(AFilename);
  Result:=CopyFileWithErrorDialogs(AFilename,bp+fn,[mbAbort]);
end;

procedure TConvertSettings.SetMainFilename(const AValue: String);
begin
  fMainFilename:=AValue;
  fMainPath:=ExtractFilePath(AValue);
end;

function TConvertSettings.GetBackupPath: String;
const
  BackupPathName='ConverterBackup';
begin
  Result:='';
  if fBackupFiles then begin
    Result:=fMainPath+BackupPathName+PathDelim;
    // Create backup path if needed.
    if not DirectoryExistsUTF8(Result) then
      CreateDirUTF8(Result);
  end;
end;


{ TConvertSettingsForm }

procedure TConvertSettingsForm.FormCreate(Sender: TObject);
begin
  MainPathEdit.Text:='';
end;

procedure TConvertSettingsForm.FormDestroy(Sender: TObject);
begin
  ;
end;

procedure TConvertSettingsForm.btnOKClick(Sender: TObject);
begin
  ModalResult:=mrOk;
end;

procedure TConvertSettingsForm.ReplacementCompsButtonClick(Sender: TObject);
begin
  ShowMessage('Sorry, not implemented yet!');
end;


end.

