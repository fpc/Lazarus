unit NewIDEWndDlg;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LazFileUtils, LazLoggerBase, LazUTF8, CodeCache, CodeToolManager,
  Forms, Controls, Graphics, Dialogs, StdCtrls, ProjectIntf, MacroIntf,
  IDEDialogs, CodyStrConsts;

type

  { TFileDescIDEDockableWindow }

  TFileDescIDEDockableWindow = class(TFileDescPascalUnit)
  private
    FMenuItemCaption: string;
    FTemplateImplementation: string;
    FTemplateInterface: string;
    FTemplateLFM: string;
    FTemplateUsesSection: string;
    FTemplateLoaded: boolean;
    function ExtractTemplate(Src, StartMarker, EndMarker: string): string;
    procedure LoadTemplate;
  public
    constructor Create; override;
    function Init(var NewFilename: string; NewOwner: TObject;
      var NewSource: string; Quiet: boolean): TModalResult; override;
    function GetLocalizedName: string; override;
    function GetLocalizedDescription: string; override;
    function GetInterfaceUsesSection: string; override;
    function GetInterfaceSource(const {%H-}Filename, {%H-}SourceName,
                                ResourceName: string): string; override;
    function GetImplementationSource(const {%H-}Filename, {%H-}SourceName,
                                     ResourceName: string): string; override;
    property TemplateUsesSection: string read FTemplateUsesSection write FTemplateUsesSection;
    property TemplateInterface: string read FTemplateInterface write FTemplateInterface;
    property TemplateImplementation: string read FTemplateImplementation write FTemplateImplementation;
    property TemplateLFM: string read FTemplateLFM write FTemplateLFM;
    property MenuItemCaption: string read FMenuItemCaption write FMenuItemCaption;
  end;


  { TNewIDEWndCfgDialog }

  TNewIDEWndCfgDialog = class(TForm)
    FormNameEdit: TEdit;
    FormNameLabel: TLabel;
    FormNameNoteLabel: TLabel;
    MenuItemCaptionEdit: TEdit;
    MenuItemCaptionLabel: TLabel;
    OkButton: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormNameEditChange(Sender: TObject);
    procedure OkButtonClick(Sender: TObject);
  private
    function IsFormNameValid: boolean;
    function IsMenuCaptionValid: boolean;
  public
  end;

implementation

{$R *.lfm}

{ TNewIDEWndCfgDialog }

procedure TNewIDEWndCfgDialog.FormCreate(Sender: TObject);
begin
  Caption:=crsConfigureNewIDEWindow;
  FormNameLabel.Caption:='Form.Name:';
  FormNameEdit.Text:='MyPkgCheetahEditor';
  FormNameNoteLabel.Caption:=
    crsNoteTheNameOfTheFormMustBeAValidPascalIdentifierAn;
  MenuItemCaptionLabel.Caption:=crsMenuItemCaption;
  MenuItemCaptionEdit.Text:=crsCheetahEditor;
  OkButton.Caption:=crsContinue;
end;

procedure TNewIDEWndCfgDialog.FormNameEditChange(Sender: TObject);
begin
  if IsFormNameValid then
    FormNameLabel.Font.Color:=clDefault
  else
    FormNameLabel.Font.Color:=clRed;
end;

procedure TNewIDEWndCfgDialog.OkButtonClick(Sender: TObject);
begin
  if not IsFormNameValid then begin
    IDEMessageDialog('Error','Invalid form name.',mtError,[mbCancel]);
    exit;
  end;
  if not IsMenuCaptionValid then begin
    IDEMessageDialog('Error','Invalid menu caption.',mtError,[mbCancel]);
    exit;
  end;
  ModalResult:=mrOK;
end;

function TNewIDEWndCfgDialog.IsFormNameValid: boolean;
begin
  Result:=IsValidIdent(FormNameEdit.Text);
end;

function TNewIDEWndCfgDialog.IsMenuCaptionValid: boolean;
var
  s: TCaption;
begin
  s:=UTF8Trim(MenuItemCaptionEdit.Text);
  Result:=(s<>'') and (length(s)<255);
end;

{ TFileDescIDEDockableWindow }

function TFileDescIDEDockableWindow.ExtractTemplate(Src, StartMarker,
  EndMarker: string): string;
var
  StartPos: SizeInt;
  EndPos: SizeInt;
begin
  StartPos:=system.Pos(StartMarker,Src);
  EndPos:=system.Pos(EndMarker,Src);
  if StartPos<1 then begin
    DebugLn(['ERROR: TFileDescIDEDockableWindow.ExtractTemplate marker "'+StartMarker+'" not found']);
    debugln('Source: ==================================================');
    debugln(Src);
    debugln('==========================================================');
    exit('');
  end;
  if EndPos<1 then begin
    debugln(['ERROR: TFileDescIDEDockableWindow.ExtractTemplate marker "'+EndMarker+'" not found']);
    exit('');
  end;
  inc(StartPos,length(StartMarker));
  Result:=UTF8Trim(copy(Src,StartPos,EndPos-StartPos));
end;

procedure TFileDescIDEDockableWindow.LoadTemplate;
var
  BaseDir: String;
  Filename: String;
  Code: TCodeBuffer;
begin
  if FTemplateLoaded then exit;

  BaseDir:='$PkgDir(Cody)';
  IDEMacros.SubstituteMacros(BaseDir);
  if (BaseDir='') or (not DirectoryExistsUTF8(BaseDir)) then begin
    debugln(['ERROR: TFileDescIDEDockableWindow.Create cody directory not found']);
    exit;
  end;

  // load unit source
  Filename:=AppendPathDelim(BaseDir)+'templateidedockablewindow.pas';
  if not FileExistsUTF8(Filename) then begin
    debugln(['ERROR: TFileDescIDEDockableWindow.Create template file not found: '+Filename]);
    exit;
  end;
  Code:=CodeToolBoss.LoadFile(Filename,true,false);
  if Code=nil then begin
    debugln(['ERROR: TFileDescIDEDockableWindow.Create unable to read file ',Filename]);
    exit;
  end;

  TemplateUsesSection:=ExtractTemplate(Code.Source,'// UsesStart','// UsesEnd');
  TemplateInterface:=ExtractTemplate(Code.Source,'// InterfaceStart','// InterfaceEnd');
  TemplateImplementation:=ExtractTemplate(Code.Source,'// ImplementationStart','// ImplementationEnd');

  // load lfm
  Filename:=AppendPathDelim(BaseDir)+'templateidedockablewindow.lfm';
  if not FileExistsUTF8(Filename) then begin
    debugln(['ERROR: TFileDescIDEDockableWindow.Create template file not found: '+Filename]);
    exit;
  end;
  Code:=CodeToolBoss.LoadFile(Filename,true,false);
  if Code=nil then begin
    debugln(['ERROR: TFileDescIDEDockableWindow.Create unable to read file ',Filename]);
    exit;
  end;

  TemplateLFM:=Code.Source;
end;

constructor TFileDescIDEDockableWindow.Create;
begin
  inherited Create;
  Name:='IDE window, dockable';
  ResourceClass:=TForm;
  UseCreateFormStatements:=false;
  RequiredPackages:='IDEIntf';
  AddToProject:=false;
  TemplateInterface:='failed loading template';
  MenuItemCaption:='Cheetah Editor';
end;

function TFileDescIDEDockableWindow.Init(var NewFilename: string;
  NewOwner: TObject; var NewSource: string; Quiet: boolean): TModalResult;
var
  Dlg: TNewIDEWndCfgDialog;
begin
  Result:=inherited Init(NewFilename, NewOwner, NewSource, Quiet);
  if not Quiet then begin
    Dlg:=TNewIDEWndCfgDialog.Create(nil);
    try
      Result:=Dlg.ShowModal;
      if Result<>mrOk then exit;
      DefaultResourceName:=Dlg.FormNameEdit.Text;
      MenuItemCaption:=UTF8Trim(Dlg.MenuItemCaptionEdit.Text);
    finally
      Dlg.Free;
    end;
  end;
end;

function TFileDescIDEDockableWindow.GetLocalizedName: string;
begin
  Result:=crsIDEWindowDockable;
end;

function TFileDescIDEDockableWindow.GetLocalizedDescription: string;
begin
  Result:=crsAWindowForTheLazarusIDEItCanBeDockedLikeTheCodeExp;
end;

function TFileDescIDEDockableWindow.GetInterfaceUsesSection: string;
begin
  LoadTemplate;
  Result:=TemplateUsesSection;
end;

function TFileDescIDEDockableWindow.GetInterfaceSource(const Filename,
  SourceName, ResourceName: string): string;
begin
  LoadTemplate;
  Result:=StringReplace(TemplateInterface,'TemplateName',ResourceName,[rfReplaceAll]);
end;

function TFileDescIDEDockableWindow.GetImplementationSource(const Filename,
  SourceName, ResourceName: string): string;
begin
  LoadTemplate;
  Result:=StringReplace(TemplateImplementation,'TemplateName',ResourceName,[rfReplaceAll])
          +LineEnding+LineEnding;
end;

end.

