{
 *****************************************************************************
 *                                                                           *
 *  See the file COPYING.modifiedLGPL.txt, included in this distribution,    *
 *  for details about the copyright.                                         *
 *                                                                           *
 *  This program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
 *                                                                           *
 *****************************************************************************

  Author: Michael Van Canneyt
}
unit frmSelectProps;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LCLProc, Forms, Controls, Graphics, Dialogs,
  StdCtrls, ObjInspStrConsts, IDEWindowIntf, Buttons, ExtCtrls, TypInfo,
  RTTIUtils;

type

  { TSelectPropertiesForm }

  TSelectPropertiesForm = class(TForm)
    BAdd: TButton;
    BDelete: TButton;
    BClear: TButton;
    BOK: TButton;
    BCancel: TButton;
    LLBSelected: TLabel;
    LBComponents: TListBox;
    LComponents: TLabel;
    LBProperties: TListBox;
    LBSelected: TListBox;
    PBottom: TPanel;
    PComponents: TPanel;
    PTop: TPanel;
    PProperties: TPanel;
    LProperties: TLabel;
    VSplitter: TSplitter;
    HSplitter: TSplitter;
    procedure BAddClick(Sender: TObject);
    procedure BClearClick(Sender: TObject);
    procedure BDeleteClick(Sender: TObject);
    procedure LBComponentsSelectionChange(Sender: TObject; User: boolean);
    procedure LBPropertiesClick(Sender: TObject);
    procedure LBPropertiesDblClick(Sender: TObject);
    procedure SelectPropertiesFormClose(Sender: TObject;
      var CloseAction: TCloseAction);
    procedure SelectPropertiesFormCreate(Sender: TObject);
  private
    FSelectedComponent : TComponent;
    FPropComponent: TComponent;
    function GetSelectedProps: String;
    procedure SetPropComponent(const AValue: TComponent);
    procedure SetSelectedProps(const AValue: String);
    procedure ShowComponents;
    procedure ShowProperties(C : TComponent);
    function GetSelectedComponent: TComponent;
    procedure AddSelectedProperties;
    procedure DeleteSelectedProperties;
  public
    Property PropertyComponent : TComponent Read FPropComponent Write SetPropComponent;
    Property SelectedProperties : String Read GetSelectedProps Write SetSelectedProps;
  end; 

var
  SelectPropertiesForm: TSelectPropertiesForm;

implementation

{$R *.lfm}

{ TSelectPropertiesForm }

procedure TSelectPropertiesForm.SetPropComponent(const AValue: TComponent);
begin
  if FPropComponent=AValue then exit;
    begin
    FPropComponent:=AValue;
    ShowComponents;
    end
end;

procedure TSelectPropertiesForm.LBComponentsSelectionChange(Sender: TObject;
  User: boolean);
begin
  ShowProperties(GetSelectedComponent);
end;

procedure TSelectPropertiesForm.LBPropertiesClick(Sender: TObject);
begin
end;

procedure TSelectPropertiesForm.LBPropertiesDblClick(Sender: TObject);
begin
  AddSelectedProperties;
end;

procedure TSelectPropertiesForm.SelectPropertiesFormClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  IDEDialogLayoutList.SaveLayout(Self);
end;

procedure TSelectPropertiesForm.SelectPropertiesFormCreate(Sender: TObject);
begin
  BAdd.Caption:=ilesAdd;
  BDelete.Caption:=oisDelete;
  BClear.Caption:=oisClear;
  BOK.Caption:=oisOk;
  BCancel.Caption:=oiStdActDataSetCancel1Hint;
  LComponents.Caption:=oisComponents;
  LProperties.Caption:=oisProperties;
  IDEDialogLayoutList.ApplyLayout(Self,485,460);
end;

procedure TSelectPropertiesForm.BAddClick(Sender: TObject);
begin
  AddSelectedProperties;
end;

procedure TSelectPropertiesForm.BClearClick(Sender: TObject);
begin
  LBSelected.Items.Clear;
  ShowProperties(FSelectedComponent);
end;

procedure TSelectPropertiesForm.BDeleteClick(Sender: TObject);
begin
  DeleteSelectedProperties;
end;

function TSelectPropertiesForm.GetSelectedProps: String;
begin
  //debugln('TSelectPropertiesForm.GetSelectedProps');
  LBSelected.Items.Delimiter:=';';
  Result:=LBSelected.Items.DelimitedText;
end;

procedure TSelectPropertiesForm.SetSelectedProps(const AValue: String);
var
  L : TStringList;
  I : Integer;
begin
  //debugln('TSelectPropertiesForm.SetSelectedProps');
  L:=TStringList.Create;
  Try
    L.Delimiter:=';';
    L.DelimitedText:=AValue;
    For I:=0 to L.Count-1 do
      L[i]:=Trim(L[i]);
    L.Sort;
    LBSelected.Items.Assign(L);
  Finally
    L.Free;
  end;
end;

procedure TSelectPropertiesForm.ShowComponents;
var
  C : TComponent;
  I : Integer;
begin
  //debugln('TSelectPropertiesForm.ShowComponents');
  With LBComponents.Items do
    try
      BeginUpdate;
      Clear;
      If Assigned(FPropComponent) then
        begin
        AddObject(FPropComponent.Name,FPropComponent);
        For I:=0 to FPropComponent.ComponentCount-1 do
          begin
          C:=FPropComponent.Components[I];
          AddObject(C.Name,C);
          end;
        end;
    Finally
      EndUpdate;
    end;
  If LBComponents.Items.Count>0 then
    LBComponents.ItemIndex:=0;
  ShowProperties(GetSelectedComponent);
end;

procedure TSelectPropertiesForm.ShowProperties(C : TComponent);
var
  L : TPropInfoList;
  I : Integer;
  N,S : String;
  P : PPropInfo;
begin
  //debugln('TSelectPropertiesForm.ShowProperties ',dbgsName(C));
  With LBProperties do
    try
      Items.BeginUpdate;
      Clear;
      FSelectedComponent:=C;
      If (C<>Nil) then
        begin
        N:=C.Name;
        L:=TPropInfoList.Create(C,tkProperties);
        Try
          For I:=0 to L.Count-1 do
            begin
            P:=L[I];
            If (C<>FPropComponent) then
              S:=N+'.'+P^.Name;
            If LBSelected.Items.IndexOf(S)=-1 then
              LBProperties.Items.Add(P^.Name);
            end;
        Finally
          L.Free;
        end;
        end;
    Finally
      Items.EndUpdate;
    end;
end;

function TSelectPropertiesForm.GetSelectedComponent: TComponent;
var
  CurName: string;
begin
  Result:=nil;
  if LBComponents.ItemIndex>=0 then begin
    CurName:=LBComponents.Items[LBComponents.ItemIndex];
    if SysUtils.CompareText(CurName,FPropComponent.Name)=0 then
      Result:=FPropComponent
    else
      Result:=FPropComponent.FindComponent(CurName);
    //DebugLn(['TSelectPropertiesForm.GetSelectedComponent ItemIndex=',LBComponents.ItemIndex,' CurName=',CurName,' Result=',DbgSName(Result)]);
  end;
end;

procedure TSelectPropertiesForm.AddSelectedProperties;
var
  I : Integer;
  N : String;
begin
  //write('TSelectPropertiesForm.AddSelectedProperties A ');
  //For I:=LBProperties.Items.Count-1 downto 0 do if LBProperties.Selected[i] then write(i);
  //writeln('');
  If Assigned(FSelectedComponent) then
    With LBProperties do
      try
        Items.BeginUpdate;
        LBSelected.Items.BeginUpdate;
        //writeln('TSelectPropertiesForm.AddSelectedProperties B');
        For I:=Items.Count-1 downto 0 do
          If Selected[i] then
            begin
            //writeln('TSelectPropertiesForm.AddSelectedProperties C ',i);
            N:=Items[i];
            If (FSelectedComponent<>FPropComponent) then
              N:=FSelectedComponent.Name+'.'+N;
            LBSelected.Items.Add(N);
            Items.Delete(I);
            end;
      Finally
        LBSelected.Items.EndUpdate;
        Items.EndUpdate;
      end;
end;

procedure TSelectPropertiesForm.DeleteSelectedProperties;
var
  I : Integer;
begin
  //debugln('TSelectPropertiesForm.DeleteSelectedProperties');
  With LBSelected do
    try
      Items.BeginUpdate;
      For I:=Items.Count-1 downto 0 do
        If Selected[i] then
          Items.Delete(I);
    Finally
      Items.EndUpdate;
    end;
  ShowProperties(FSelectedComponent);
end;

end.

