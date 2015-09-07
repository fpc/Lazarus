unit lrEmailExportFilterSetup;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, DividerBevel, Forms, Controls, Graphics, Dialogs,
  ButtonPanel, ComCtrls, StdCtrls, lrEmailExportFilter;

type

  { TlrEmailExportFilterSetupForm }

  TlrEmailExportFilterSetupForm = class(TForm)
    ButtonPanel1: TButtonPanel;
    cbAppName: TComboBox;
    cbFilterList: TComboBox;
    cbEmailList: TComboBox;
    cbSubject: TComboBox;
    DividerBevel1: TDividerBevel;
    DividerBevel2: TDividerBevel;
    edtFromName: TEdit;
    edtFromAdress: TEdit;
    Edit3: TEdit;
    Edit4: TEdit;
    Edit5: TEdit;
    Edit6: TEdit;
    Label1: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    edtBoby: TMemo;
    PageControl1: TPageControl;
    RadioButton1: TRadioButton;
    RadioButton2: TRadioButton;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    procedure FormCreate(Sender: TObject);
    procedure RadioButton1Change(Sender: TObject);
  private
    FEmailExport:TlrEmailExportFilter;
    procedure FillFormatsList;
  public
    constructor CreateSetupForm(AEmailExport:TlrEmailExportFilter);
    procedure SetDefaultEmailApp;
    { public declarations }
  end;

implementation
uses LR_Class
{$IFDEF WINDOWS}
  , registry
{$ENDIF}
  ;

{$R *.lfm}

function GetEmailDefApp:string;
{$IFDEF WINDOWS}
var
  R: TRegistry;
begin
  Result:='';
  R:=TRegIniFile.Create;
  R.Access     := KEY_READ;
  R.RootKey    := HKEY_CURRENT_USER;
  try
    if R.OpenKeyReadOnly('Software\Clients\Mail') then
      Result:=R.ReadString('');
    R.CloseKey;
  finally
    R.Free;
  end;
end;
{$ELSE}
begin
  Result:='';
end;
{$ENDIF}
{ TlrEmailExportFilterSetupForm }

procedure TlrEmailExportFilterSetupForm.RadioButton1Change(Sender: TObject);
begin
  Label1.Enabled:=RadioButton1.Checked;
  cbAppName.Enabled:=RadioButton1.Checked;

  Label6.Enabled:=RadioButton2.Checked;
  Label7.Enabled:=RadioButton2.Checked;
  Label8.Enabled:=RadioButton2.Checked;
  Label9.Enabled:=RadioButton2.Checked;
  Label10.Enabled:=RadioButton2.Checked;
  Label11.Enabled:=RadioButton2.Checked;

  edtFromName.Enabled:=RadioButton2.Checked;
  edtFromAdress.Enabled:=RadioButton2.Checked;
  Edit3.Enabled:=RadioButton2.Checked;
  Edit4.Enabled:=RadioButton2.Checked;
  Edit5.Enabled:=RadioButton2.Checked;
  Edit6.Enabled:=RadioButton2.Checked;
end;

procedure TlrEmailExportFilterSetupForm.FormCreate(Sender: TObject);
begin
  Caption:=sSendToEmail;
  DividerBevel1.Caption:=sMessage;
  DividerBevel2.Caption:=sAttachment;

  Label2.Caption:=sAdress;
  Label3.Caption:=sSubject;
  Label4.Caption:=sText;
  Label5.Caption:=sFormat;

  RadioButton1.Caption:=sUseMailProgram;
  RadioButton2.Caption:=sDirectSend;
  Label1.Caption:=sApplicationName;
  Label6.Caption:=sFromName;
  Label7.Caption:=sFromAdress;
  Label8.Caption:=sHost;
  Label9.Caption:=sPort;
  Label10.Caption:=sLogin;
  Label11.Caption:=sPassword;

  TabSheet1.Caption:=sEmail;
  TabSheet2.Caption:=sAccount;
end;

procedure TlrEmailExportFilterSetupForm.FillFormatsList;
var
  i: Integer;
begin
  cbFilterList.Items.Clear;
  for i:=0 to ExportFilters.Count - 1 do
    if ExportFilters[i].Enabled then
      cbFilterList.Items.Add(ExportFilters[i].FilterDesc);

  if (cbFilterList.Items.Count>0) and (FEmailExport.AttachmentFormat<>'') then
    if cbFilterList.Items.IndexOf(FEmailExport.AttachmentFormat)>-1 then
      cbFilterList.ItemIndex:=cbFilterList.Items.IndexOf(FEmailExport.AttachmentFormat);
end;

constructor TlrEmailExportFilterSetupForm.CreateSetupForm(
  AEmailExport: TlrEmailExportFilter);
begin
  inherited Create(Application);
  FEmailExport:=AEmailExport;

  PageControl1.ActivePageIndex:=0;
  RadioButton1Change(nil);
  FillFormatsList;
  cbEmailList.Items.Assign(AEmailExport.EmailList);
  cbEmailList.Text:=AEmailExport.Email;
  cbSubject.Text:=AEmailExport.MessageSubject;
  edtBoby.Text:=AEmailExport.MessageBody.Text;

  edtFromName.Text:=AEmailExport.FromName;
  edtFromAdress.Text:=AEmailExport.FromAdress;
end;

procedure TlrEmailExportFilterSetupForm.SetDefaultEmailApp;
var
  S:string;
begin
  if (FEmailExport.DefEmailApp <> '') and (cbAppName.Items.IndexOf(FEmailExport.DefEmailApp)>-1) then
  begin
    cbAppName.ItemIndex:=cbAppName.Items.IndexOf(FEmailExport.DefEmailApp);
    exit;
  end;

  S:=GetEmailDefApp;
  if (S <> '') and (cbAppName.Items.IndexOf(S)>-1) then
  begin
    cbAppName.ItemIndex:=cbAppName.Items.IndexOf(S);
    exit;
  end;

  if cbAppName.Items.Count>0 then
    cbAppName.ItemIndex:=0;
end;

end.

