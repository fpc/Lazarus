{  $Id$  }
{
 *****************************************************************************
  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************

  Author: Michael Van Canneyt

  This unit registers the TMemDataset components of the FCL.
}
unit frmSelectDataset;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DB, LResources, Forms, Controls, Graphics, Dialogs,
  StdCtrls, Buttons, MemDS, ComponentEditors, PropEdits, LazarusPackageIntf;

Type

  { TMemDatasetEditor }

  TMemDatasetEditor = Class(TComponentEditor)
  private
    DefaultEditor: TBaseComponentEditor;
  Public
    constructor Create(AComponent: TComponent; ADesigner: TComponentEditorDesigner); override;
    destructor Destroy; override;
    procedure ExecuteVerb(Index: Integer); override;
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;
    Function GetMemDataset : TMemDataset;
    Procedure CopyDataset; virtual;
    Procedure CreateTable; virtual;
  end;

  { TSelectSrcDatasetForm }

  TSelectSrcDatasetForm = class(TForm)
    BOK: TButton;
    BCancel: TButton;
    CBMetaDataOnly: TCheckBox;
    LLBDatasets: TLabel;
    LBDatasets: TListBox;
    procedure BOKClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure LBDatasetsClick(Sender: TObject);
  private
  public
  end;

Resourcestring
  SErrComponentNotFound = 'Error: Component "%s" not found';
  SMenuCreateDataset    = 'Create dataset';
  SMenuCopyDataset      = 'Copy data from Dataset';
  SErrSelectDataset = 'Please select a dataset first';

  SCaption = 'Select dataset to copy from';
  SCopyMeta = 'Copy only metadata';
  SDataset = '&Dataset to copy from:';
  SOkBtn  = 'OK';
  SCancelBtn = 'Cancel';

var
  SelectSrcDatasetForm: TSelectSrcDatasetForm;

procedure Register;


implementation

{$R *.lfm}
{$R memdsicons.res}

procedure RegisterUnitMemDS;
begin
  RegisterComponents('Data Access',[TMemDataset]);
end;

procedure Register;
begin
  RegisterUnit('MemDS',@RegisterUnitMemDS);
  RegisterComponentEditor(TMemDataset,TMemdatasetEditor) ;
  RegisterPropertyEditor(TypeInfo(String),TMemDataset,'FileName',
                         TFileNamePropertyEditor);
end;

{ TMemDatasetEditor }

constructor TMemDatasetEditor.Create(AComponent: TComponent;
  ADesigner: TComponentEditorDesigner);
var
  CompClass: TClass;
begin
  inherited Create(AComponent, ADesigner);
  CompClass := PClass(Acomponent)^;
  try
    PClass(AComponent)^ := TDataSet;
    DefaultEditor := GetComponentEditor(AComponent, ADesigner);
  finally
    PClass(AComponent)^ := CompClass;
  end;
end;

destructor TMemDatasetEditor.Destroy;
begin
  DefaultEditor.Free;
  inherited Destroy;
end;

procedure TMemDatasetEditor.ExecuteVerb(Index: Integer);

begin
  if Index < DefaultEditor.GetVerbCount then
    DefaultEditor.ExecuteVerb(Index)
  else
  begin
    case Index - DefaultEditor.GetVerbCount of
      0 : CreateTable;
      1 : CopyDataset;
    end;
  end;
end;

function TMemDatasetEditor.GetVerb(Index: Integer): string;
begin
  if Index < DefaultEditor.GetVerbCount then
    Result := DefaultEditor.GetVerb(Index)
  else
  begin
    case Index - DefaultEditor.GetVerbCount of
      0 : Result:=SMenuCreateDataset;
      1 : Result:=SMenuCopyDataset;
    end;
  end;
end;

function TMemDatasetEditor.GetVerbCount: Integer;
begin
  Result:=DefaultEditor.GetVerbCount + 2;
end;

function TMemDatasetEditor.GetMemDataset: TMemDataset;
begin
  Result:=GetComponent as TMemDataset;
end;

procedure TMemDatasetEditor.CopyDataset;

Var
  DSN : String;
  DS  : TDataset;
  I   : integer;
  F   : TComponent;
begin
  With TSelectSrcDatasetForm.Create(Application) do
    Try
      F:=GetDesigner.Form;
      For I:=0 to F.ComponentCount-1 do
        if (F.Components[i] is TDataset) and
           (F.Components[i]<>GetComponent) then
          LBDatasets.Items.Add(F.Components[i].Name);
      If ShowModal=mrOK then
        begin
        With LBDatasets do
          DSN:=Items[ItemIndex];
        DS:=Nil;
        I:=0;
        While (DS=Nil) and (I<F.ComponentCount) do
          if (F.Components[i] is TDataset) and
             (F.Components[i].Name=DSN) then
            DS:=F.Components[i] as TDataset
          else
            Inc(I);
        If (DS=Nil) then
          Raise Exception.CreateFmt(SErrComponentNotFound,[DSN]);
        GetMemDataset.CopyFromDataSet(DS,Not CBMetaDataOnly.Checked);
        // Modified;
        end;
    Finally
      Free;
    end;
end;

procedure TMemDatasetEditor.CreateTable;
begin
  GetMemdataset.CreateTable;
end;

{ TSelectSrcDatasetForm }

procedure TSelectSrcDatasetForm.LBDatasetsClick(Sender: TObject);
begin
  if Sender=nil then ;
  BOK.Enabled:=True;
end;

procedure TSelectSrcDatasetForm.BOKClick(Sender: TObject);
begin
  if Sender=nil then ;
  If LBDatasets.ItemIndex=-1 then
    Raise Exception.Create(SErrSelectDataset)
end;

procedure TSelectSrcDatasetForm.FormCreate(Sender: TObject);
begin
  Caption := SCaption;
  CBMetaDataOnly.Caption := SCopyMeta;
  LLBDatasets.Caption := SDataset;
  BOK.Caption := SOkBtn;
  BCancel.Caption := SCancelBtn;
end;

end.

