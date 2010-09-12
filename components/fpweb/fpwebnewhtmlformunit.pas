unit fpwebNewHTMLFormUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ButtonPanel,
  ComCtrls, StdCtrls, EditBtn;

type

  { TfpwebNewHTMLFormForm }

  TfpwebNewHTMLFormForm = class(TForm)
    ButtonPanel1: TButtonPanel;
    cbAction: TComboBox;
    cbMetod: TComboBox;
    cbEncType: TComboBox;
    cbTarget: TComboBox;
    cbAcceptCharset: TComboBox;
    ComboBox6: TComboBox;
    ComboBox7: TComboBox;
    ComboBox8: TComboBox;
    ComboBox9: TComboBox;
    EditButton1: TEditButton;
    Label1: TLabel;
    Label10: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    TabSheet3: TTabSheet;
    procedure FormCreate(Sender: TObject);
  private
    procedure FillActionList;
  public
    function HtmlText(const S:string): string;
  end; 

var
  fpwebNewHTMLFormForm: TfpwebNewHTMLFormForm;

implementation
uses fpWeb;

{$R *.lfm}

{ TfpwebNewHTMLFormForm }

procedure TfpwebNewHTMLFormForm.FormCreate(Sender: TObject);
begin
  cbAction.Items.Clear;
  FillActionList;
end;

procedure TfpwebNewHTMLFormForm.FillActionList;
var
  i, j:integer;
  WD:TFPWebModule;
begin
  for i:=0 to Screen.DataModuleCount - 1 do
  begin
    if Screen.DataModules[i] is TFPWebModule then
    begin
      WD:=Screen.DataModules[i] as TFPWebModule;
      for j:=0 to WD.Actions.Count - 1 do
        cbAction.Items.Add(WD.ActionVar +'='+ WD.Actions[j].Name);
    end;
  end;
end;

function TfpwebNewHTMLFormForm.HtmlText(const S:string): string;
begin
  Result:='<FORM action="?'+cbAction.Text+
          '" method="'+cbMetod.Text+'"';

  if cbEncType.Text<>'' then
    Result:=Result +' enctype="' + cbEncType.Text+ '"';

  if cbTarget.Text <> '' then
    Result:=Result +' target="' + cbTarget.Text + '"';

  if cbAcceptCharset.Text <> '' then
    Result:=Result +' accept-charset="' + cbAcceptCharset.Text + '">';

  Result:=Result +'>' + LineEnding + S+ LineEnding + '</FORM>';
end;

end.

