unit frmpas2jsedithtml;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, SynEdit, ButtonPanel,
  SynHighlighterHTML;

type

  { TfrmPas2jsEditHTML }

  TfrmPas2jsEditHTML = class(TForm)
    ButtonPanel1: TButtonPanel;
    shsHTML: TSynHTMLSyn;
    seHTML: TSynEdit;
  private
    FPropertyName: String;
    function GetHTML: TStrings;
    function GetHTMLText: String;
    procedure SetHTML(AValue: TStrings);
    procedure SetHTMLText(AValue: String);
    procedure SetPropertyName(AValue: String);

  public
    Property PropertyName : String Read FPropertyName Write SetPropertyName;
    Property HTML : TStrings Read GetHTML Write SetHTML;
    Property HTMLText : String Read GetHTMLText Write SetHTMLText;
  end;


Function EditHTML(const aPropertyName : String; var aHTML : String) : Boolean;
Function EditHTML(const aPropertyName : String; aHTML : TStrings) : Boolean;

implementation

uses strpas2jscomponents;

function EditHTML(const aPropertyName : String; var aHTML: String): Boolean;
begin
  With TfrmPas2jsEditHTML.Create(Application) do
  try
    PropertyName:=aPropertyName;
    HTMLText:=aHTML;
    Result:=ShowModal=mrOK;
    if Result then
      aHTML:=HTMLText;
  finally
    Free;
  end;
end;

function EditHTML(const aPropertyName : String; aHTML: TStrings): Boolean;
begin
  With TfrmPas2jsEditHTML.Create(Application) do
  try
    PropertyName:=aPropertyName;
    HTML:=aHTML;
    Result:=ShowModal=mrOK;
    if Result then
      aHTML.Assign(HTML);
  finally
    Free;
  end;
end;

{$R *.lfm}

{ TfrmPas2jsEditHTML }

function TfrmPas2jsEditHTML.GetHTML: TStrings;
begin
  Result:=seHTML.Lines;
end;

function TfrmPas2jsEditHTML.GetHTMLText: String;
begin
  Result:=seHTML.Lines.Text;
end;

procedure TfrmPas2jsEditHTML.SetHTML(AValue: TStrings);
begin
  seHTML.Lines.Assign(aValue);
end;

procedure TfrmPas2jsEditHTML.SetHTMLText(AValue: String);
begin
  seHTML.Lines.Text:=aValue;
end;

procedure TfrmPas2jsEditHTML.SetPropertyName(AValue: String);
begin
  if FPropertyName=AValue then Exit;
  FPropertyName:=AValue;
  Caption:=Format(rsEditingHTMLProp,[aValue]);
end;

end.

