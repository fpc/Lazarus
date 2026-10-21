unit richmemoregister; 

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, RichMemo, LResources, PropEdits, RtfEditPropDialog, Forms, Controls;
  
procedure Register;

implementation

type

  { TRichEditProperty }

  TRichEditProperty = class(TPropertyEditor)
  public
    procedure Edit; override;
    function GetAttributes: TPropertyAttributes; override;
    function GetValue: ansistring; override;
  end;


procedure Register;
begin
  RegisterComponents('Common Controls', [TRichMemo]);
  RegisterPropertyEditor(TypeInfo(AnsiString),(TRichMemo), 'Rtf', TRichEditProperty);
end;

{ TRichEditProperty }

procedure TRichEditProperty.Edit;
var
  TheDialog : TRTFEditDialog;
begin
  TheDialog :=TRTFEditDialog.Create(nil);
  try
    TheDialog.RichMemo1.Rtf:=GetStrValue;
    if (TheDialog.ShowModal = mrOK) then begin
      if TheDialog.RichMemo1.Text = '' then
        SetStrValue( '' )
      else
        SetStrValue( TheDialog.RichMemo1.Rtf );
    end;
  finally
    TheDialog.Free;
  end;
end;

function TRichEditProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog, paRevertable, paReadOnly];
end;

function TRichEditProperty.GetValue: ansistring;
begin
  Result:='(Rich Text)';
end;

initialization
  {$i richmemopackage.lrs}

end.

