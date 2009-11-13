unit frmSelectCodeGenerator;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  fpddcodegen,
  ButtonPanel, ldd_consts;

type

  { TSelectCodeGeneratorForm }

  TSelectCodeGeneratorForm = class(TForm)
    BPButtons: TButtonPanel;
    RGGenerators: TRadioGroup;
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    FHaveFields: Boolean;
    FHaveSQL: Boolean;
    FSelF: TCodeGeneratorItem;
    procedure ShowAvailableGenerators;
    { private declarations }
  public
    { public declarations }
    Property HaveSQL : Boolean Read FHaveSQL Write FHaveSQL;
    Property HaveFields : Boolean Read FHaveFields Write FHaveFields;
    Property SelectedGenerator : TCodeGeneratorItem Read FSelF Write FSelF;
  end; 

var
  SelectCodeGeneratorForm: TSelectCodeGeneratorForm;

implementation

{ TSelectCodeGeneratorForm }

procedure TSelectCodeGeneratorForm.FormShow(Sender: TObject);
begin
  ShowAvailableGenerators;
end;

procedure TSelectCodeGeneratorForm.FormClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  If (ModalResult=mrOK) then
    With RGGenerators do
      FSelF:=Items.Objects[ItemIndex] as TCodeGeneratorItem;
end;

procedure TSelectCodeGeneratorForm.FormCreate(Sender: TObject);
begin
  Caption := ldd_Selectcodetobegenerated;
  RGGenerators.Caption:= ldd_Availablecodegenerators;
end;

procedure TSelectCodeGeneratorForm.ShowAvailableGenerators;

  Function AllowGenerator(G : TCodeGeneratorItem) : Boolean;
  
  begin
    Result:=True;
    If G.GeneratorClass.NeedsSQL then
      Result:=HaveSQL;
    If Result then
      If G.GeneratorClass.NeedsFieldDefs then
        Result:=HaveFields;
  end;

Var
  I,J: Integer;
  G : TCodeGeneratorItem;

begin
  RGGenerators.Items.Clear;
  J:=-1;
  For I:=0 to CodeGenerators.Count-1 do
    begin
    G:=CodeGenerators[i];
    If AllowGenerator(G) then
      begin
      RGGenerators.Items.AddObject(G.Description,G);
      If (G=FSelF) then
        J:=I;
      end;
    end;
  With RGGenerators do
    If (J<>-1) then
      ItemIndex:=Items.IndexOfObject(CodeGenerators[J])
    else
      If Items.Count>0 then
        ItemIndex:=0;
end;

initialization
  {$I frmselectcodegenerator.lrs}

end.

