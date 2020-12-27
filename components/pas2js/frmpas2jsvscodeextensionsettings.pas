unit frmPas2jsVSCodeExtensionSettings;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ValEdit,
  ButtonPanel, EditBtn;

type

  { TVSCodeExtensionSettingsForm }

  TVSCodeExtensionSettingsForm = class(TForm)
    ButtonPanel1: TButtonPanel;
    edtPublisher: TEdit;
    dePackage: TDirectoryEdit;
    edtDescription: TEdit;
    edtKeywords: TEdit;
    edtName: TEdit;
    edtLicense: TEdit;
    edtClassName: TEdit;
    Label1: TLabel;
    lblCommands1: TLabel;
    lblDescription: TLabel;
    lblDescription1: TLabel;
    lblLicense1: TLabel;
    lblName: TLabel;
    lblLicense: TLabel;
    lblCommands: TLabel;
    lblClassName: TLabel;
    vleContributesCommands: TValueListEditor;
    vleCommands: TValueListEditor;
    procedure edtClassNameKeyPress(Sender: TObject; var Key: char);
    procedure edtNameEditingDone(Sender: TObject);
    procedure edtNameKeyPress(Sender: TObject; var Key: char);
    procedure edtPublisherKeyPress(Sender: TObject; var Key: char);
    procedure FormShow(Sender: TObject);
  private
    Function GetValueCtl(aIndex : Integer) : TWinControl;
    function GetB(AIndex: Integer): Boolean;
    function GetS(AIndex: Integer): String;
    function GetSL(AIndex: Integer): TStrings;
    procedure SetB(AIndex: Integer; AValue: Boolean);
    procedure SetS(AIndex: Integer; AValue: String);
    procedure SetSL(AIndex: Integer; AValue: TStrings);

  public
    Property PkgDescription: String Index 0 read GetS Write SetS;
    Property PkgName : String Index 1 read GetS Write Sets;
    Property PkgKeyWords : String Index 2 read GetS Write SetS;
    Property PkgPublisher : String Index 3 read GetS Write SetS;
    Property PkgCommands : TStrings Index 4 Read GetSL Write SetSL;
    Property PkgContributesCommands : TStrings Index 5 Read GetSL Write SetSL;
    Property PkgLicense : String Index 6 read GetS Write SetS;
    Property PkgDir : String  Index 7 read GetS Write SetS;
    Property PkgClassName : String Index 8 read GetS Write SetS;
  end;

  function StripNonIdentifierChars(S : String) : string;

var
  VSCodeExtensionSettingsForm: TVSCodeExtensionSettingsForm;

implementation

{$R *.lfm}

{ TVSCodeExtensionSettingsForm }

procedure TVSCodeExtensionSettingsForm.edtNameKeyPress(Sender: TObject;
  var Key: char);
begin
  if Not (Upcase(key) in ['A'..'Z','-',#8,#127]) then
    Key:=#0;
end;

procedure TVSCodeExtensionSettingsForm.edtPublisherKeyPress(Sender: TObject;
  var Key: char);
begin
  if Not (Upcase(key) in ['A'..'Z',#8,#127]) then
    Key:=#0;
end;

procedure TVSCodeExtensionSettingsForm.FormShow(Sender: TObject);
begin
{$IFDEF WINDOWS}
  cbLink.checked:=False;
  cbLink.Enabled:=False;
{$ENDIF}
end;

procedure TVSCodeExtensionSettingsForm.edtClassNameKeyPress(Sender: TObject;
  var Key: char);
begin
  if Not (Upcase(Key) in ['A'..'Z','_',#8,#127]) then
    Key:=#0;
end;

function StripNonIdentifierChars(S : String) : string;

begin
  // Name will only contain characters and -
  Result:=StringReplace(S,'-','_',[rfReplaceAll]);
end;

procedure TVSCodeExtensionSettingsForm.edtNameEditingDone(Sender: TObject);

begin
  if (edtClassName.Text='') then
    edtClassName.Text:='T'+StripNonIdentifierChars(edtName.Text)+'Application';
end;

function TVSCodeExtensionSettingsForm.GetValueCtl(aIndex: Integer): TWinControl;
begin
  Case AIndex of
    0 : Result:=edtDescription;
    1 : Result:=edtName;
    2 : Result:=edtKeywords;
    3 : Result:=edtPublisher;
    4 : Result:=vleCommands;
    5 : Result:=vleContributesCommands;
    6 : Result:=edtLicense;
    7 : Result:=dePackage;
    8 : Result:=edtClassName;
  end;
end;

function TVSCodeExtensionSettingsForm.GetB(AIndex: Integer): Boolean;
begin
  Result:=(GetValueCtl(aIndex) as TCheckbox).Checked;
end;

function TVSCodeExtensionSettingsForm.GetS(AIndex: Integer): String;

Var
  Ctl : TWinControl;

begin
  ctl:=GetValueCtl(aIndex);
  if Ctl is TCustomEdit then
    Result:=(Ctl as TCustomEdit).text
  else
    Result:=(Ctl as TCustomEditButton).Text;
end;

function TVSCodeExtensionSettingsForm.GetSL(AIndex: Integer): TStrings;
begin
   Result:=(GetValueCtl(aIndex) as TValueListEditor).Strings;
end;

procedure TVSCodeExtensionSettingsForm.SetB(AIndex: Integer; AValue: Boolean);
begin
  (GetValueCtl(aIndex) as TCheckbox).Checked:=aValue;
end;

procedure TVSCodeExtensionSettingsForm.SetS(AIndex: Integer; AValue: String);
Var
  Ctl : TWinControl;

begin
  ctl:=GetValueCtl(aIndex);
  if Ctl is TCustomEdit then
    (Ctl as TCustomEdit).text:=aValue
  else
    (Ctl as TCustomEditButton).Text:=aValue;
end;

procedure TVSCodeExtensionSettingsForm.SetSL(AIndex: Integer; AValue: TStrings);
begin
  (GetValueCtl(aIndex) as TValueListEditor).Strings.Assign(aValue);
end;

end.

