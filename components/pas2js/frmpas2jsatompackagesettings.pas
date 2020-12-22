unit frmPas2jsAtomPackageSettings;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ValEdit,
  ButtonPanel, EditBtn;

type

  { TAtomPackageSettingsForm }

  TAtomPackageSettingsForm = class(TForm)
    ButtonPanel1: TButtonPanel;
    cbLink: TCheckBox;
    dePackage: TDirectoryEdit;
    edtDescription: TEdit;
    edtKeywords: TEdit;
    edtName: TEdit;
    edtLicense: TEdit;
    edtClassName: TEdit;
    lblCommands1: TLabel;
    lblDescription: TLabel;
    lblDescription1: TLabel;
    lblLicense1: TLabel;
    lblName: TLabel;
    lblLicense: TLabel;
    lblCommands: TLabel;
    lblClassName: TLabel;
    vleCommands: TValueListEditor;
    vleActivationCommands: TValueListEditor;
    procedure edtClassNameKeyPress(Sender: TObject; var Key: char);
    procedure edtNameEditingDone(Sender: TObject);
    procedure edtNameKeyPress(Sender: TObject; var Key: char);
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
    Property PkgLink : Boolean Index 3 read GetB Write SetB;
    Property PkgCommands : TStrings Index 4 Read GetSL Write SetSL;
    Property PkgActivationCommands : TStrings Index 5 Read GetSL Write SetSL;
    Property PkgLicense : String Index 6 read GetS Write SetS;
    Property PkgDir : String  Index 7 read GetS Write SetS;
    Property PkgClassName : String Index 8 read GetS Write SetS;
  end;

  function StripNonIdentifierChars(S : String) : string;

var
  AtomPackageSettingsForm: TAtomPackageSettingsForm;

implementation

{$R *.lfm}

{ TAtomPackageSettingsForm }

procedure TAtomPackageSettingsForm.edtNameKeyPress(Sender: TObject;
  var Key: char);
begin
  if Not (Upcase(key) in ['A'..'Z','-',#8,#127]) then
    Key:=#0;
end;

procedure TAtomPackageSettingsForm.FormShow(Sender: TObject);
begin
{$IFDEF WINDOWS}
  cbLink.checked:=False;
  cbLink.Enabled:=False;
{$ENDIF}
end;

procedure TAtomPackageSettingsForm.edtClassNameKeyPress(Sender: TObject;
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

procedure TAtomPackageSettingsForm.edtNameEditingDone(Sender: TObject);

begin
  if (edtClassName.Text='') then
    edtClassName.Text:='T'+StripNonIdentifierChars(edtName.Text)+'Application';
end;

function TAtomPackageSettingsForm.GetValueCtl(aIndex: Integer): TWinControl;
begin
  Case AIndex of
    0 : Result:=edtDescription;
    1 : Result:=edtName;
    2 : Result:=edtKeywords;
    3 : Result:=cbLink;
    4 : Result:=vleCommands;
    5 : Result:=vleActivationCommands;
    6 : Result:=edtLicense;
    7 : Result:=dePackage;
    8 : Result:=edtClassName;
  end;
end;

function TAtomPackageSettingsForm.GetB(AIndex: Integer): Boolean;
begin
  Result:=(GetValueCtl(aIndex) as TCheckbox).Checked;
end;

function TAtomPackageSettingsForm.GetS(AIndex: Integer): String;

Var
  Ctl : TWinControl;

begin
  ctl:=GetValueCtl(aIndex);
  if Ctl is TCustomEdit then
    Result:=(Ctl as TCustomEdit).text
  else
    Result:=(Ctl as TCustomEditButton).Text;
end;

function TAtomPackageSettingsForm.GetSL(AIndex: Integer): TStrings;
begin
   Result:=(GetValueCtl(aIndex) as TValueListEditor).Strings;
end;

procedure TAtomPackageSettingsForm.SetB(AIndex: Integer; AValue: Boolean);
begin
  (GetValueCtl(aIndex) as TCheckbox).Checked:=aValue;
end;

procedure TAtomPackageSettingsForm.SetS(AIndex: Integer; AValue: String);
Var
  Ctl : TWinControl;

begin
  ctl:=GetValueCtl(aIndex);
  if Ctl is TCustomEdit then
    (Ctl as TCustomEdit).text:=aValue
  else
    (Ctl as TCustomEditButton).Text:=aValue;
end;

procedure TAtomPackageSettingsForm.SetSL(AIndex: Integer; AValue: TStrings);
begin
  (GetValueCtl(aIndex) as TValueListEditor).Strings.Assign(aValue);
end;

end.

