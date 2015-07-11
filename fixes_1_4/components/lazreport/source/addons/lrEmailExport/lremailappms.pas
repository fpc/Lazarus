unit lrEmailAppMS;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, lrEmailExportFilter;

type
  { TEmailAppMSOutlook }

  TEmailAppMSOutlook = class(TEmailApp)
  protected
  public
    procedure MakeEmail(AFilter:TlrEmailExportFilter);override;
    class function AppName:string;override;
    class function AppValid:boolean;override;
  end;

implementation
uses windows, variants, ComObj, Dialogs;

{ TEmailAppMSOutlook }

procedure TEmailAppMSOutlook.MakeEmail(AFilter: TlrEmailExportFilter);
Var
  myItem, myAttachments, myAttachment : Variant;
const
 olMailItem=0;
var
 MailItem: Variant;
 Outlook: OLEVariant;
begin
  try
    //Outlook:=GetActiveOleObject('Outlook.Application');
    Outlook:=CreateOleObject('Outlook.Application');
    MailItem:=Outlook.CreateItem(olMailItem);
    MailItem.Recipients.Add(FFilter.Email); //'Список адресов');
    MailItem.Subject:=FFilter.MessageSubject; //'Заголовок письма';
    MailItem.Body:= FFilter.MessageBody.Text; //'Тело письма';
    myAttachments := MailItem.Attachments;
    myAttachment := myAttachments.Add(FFilter.EmailAttachFileName);
    MailItem.Send;
    Outlook:=Unassigned;
    //Outlook:=Unassigned;
  Except
    ShowMessage('OLE error!');
  End;
end;

class function TEmailAppMSOutlook.AppName: string;
begin
  Result:='MS Outlook';
end;

class function TEmailAppMSOutlook.AppValid: boolean;
begin
  {$IFNDEF WINDOWS}
  Result:=false;
  {$ELSE}
  Result:=true;
  {$ENDIF}
end;

initialization
  RegisterEmailApp(TEmailAppMSOutlook);
end.

