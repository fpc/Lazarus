unit frmselecthtmlactions;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  ButtonPanel, stub.htmlactions, idehtml2class, ValEdit, ComCtrls, CheckLst;

type

  { TElementInfo }

  TElementInfo = Class(TTagInfoItem)
  private
    FActionClass: THTMLCustomElementActionClass;
  Public
    Procedure Assign(aSource : TPersistent); override;
    Property ActionClass : THTMLCustomElementActionClass Read FActionClass Write FActionClass;
  end;

  { TElementInfoList }

  TElementInfoList  = Class(TTagInfoList)
  private
    function GetInfo(aIndex : Integer): TElementInfo;
  Public
    Constructor Create; overload;
    Property Infos[aIndex : Integer] : TElementInfo Read GetInfo; default;
  end;

  { TfrmSelectHTMLActionClasses }

  TfrmSelectHTMLActionClasses = class(TForm)
    bpHTMLActions: TButtonPanel;
    cbUseDBAware: TCheckBox;
    clbRemove: TCheckListBox;
    Label1: TLabel;
    PageControl1: TPageControl;
    pnlTop: TPanel;
    TSAdd: TTabSheet;
    TSRemove: TTabSheet;
    VLEClasses: TValueListEditor;
    procedure cbUseDBAwareChange(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure VLEClassesGetPickList(Sender: TObject; const {%H-}KeyName: string;
      Values: TStrings);
  private
    FPreferDB: Boolean;
    FRemoveList: TFPList;
    FTags: TElementInfoList;
    procedure AllocateDefaultClasses;
    procedure DisplayRemoveList;
    function GetTagClass(const aEl: TElementInfo): THTMLCustomElementActionClass;
    procedure SaveSelectedClasses;
    procedure SetPreferDB(AValue: Boolean);
    procedure SetRemoveList(AValue: TFPList);
    procedure SetTags(AValue: TElementInfoList);

  public
    Property PreferDB : Boolean Read FPreferDB Write SetPreferDB;
    Property Tags : TElementInfoList Read FTags Write SetTags;
    // On entry, list of actions that can be removed. On close, actions that should actually be removed.
    Property RemoveList : TFPList Read FRemoveList Write SetRemoveList;
  end;

Function SelectHTMLActionClasses(aTags : TElementInfoList; aPreferDB : Boolean; aRemoveList : TFPList) : Boolean;

implementation

uses strutils, p2jselementactions, stub.data.HTMLActions, strpas2jscomponents;


{$R *.lfm}

Function SelectHTMLActionClasses(aTags : TElementInfoList; aPreferDB : Boolean; aRemoveList : TFPList) : Boolean;

begin
  With  TfrmSelectHTMLActionClasses.Create(Application) do
    try
      PreferDB:=aPreferDB;
      Tags:=aTags;
      RemoveList:=aRemoveList;
      Result:=ShowModal=mrOK
    finally
      Free;
    end;
end;

{ TElementInfoList }

function TElementInfoList.GetInfo(aIndex : Integer): TElementINfo;
begin
  Result:=Items[aIndex] as TElementInfo;
end;

constructor TElementInfoList.Create;
begin
  Inherited Create(TElementInfo);
end;

{ TElementINfo }

procedure TElementInfo.Assign(aSource: TPersistent);
begin
  inherited Assign(aSource);
end;

{ TfrmSelectHTMLActionClasses }

procedure TfrmSelectHTMLActionClasses.VLEClassesGetPickList(Sender: TObject;
  const KeyName: string; Values: TStrings);

Var
  I : Integer;

begin
  Values.Clear;
  Values.Add(rsNoControl);
  For I:=0 to TPas2JSActionRegistry.Instance.ActionCount-1 do
    Values.Add(TPas2JSActionRegistry.Instance[I].ActionClass.ClassName);
end;

procedure TfrmSelectHTMLActionClasses.cbUseDBAwareChange(Sender: TObject);
begin
  FPreferDB:=cbUseDBAware.Checked;
  AllocateDefaultClasses;
end;

procedure TfrmSelectHTMLActionClasses.FormClose(Sender: TObject;
  var CloseAction: TCloseAction);

begin
  if CloseAction=caNone then
    ;
  if ModalResult=mrOK then
    SaveSelectedClasses;
end;

procedure TfrmSelectHTMLActionClasses.SaveSelectedClasses;

Var
  I : Integer;
  N,V : String;
  aEl : TElementInfo;
  aAct : TRegisteredHTMLAction;

begin
  For I:=0 to VLEClasses.Strings.Count-1 do
    begin
    aEl:=VLEClasses.Strings.Objects[I] as TElementInfo;
    VLEClasses.Strings.GetNameValue(I,N,V);
    if (V='') or (V=rsNoControl) then
      aAct:=Nil
    else
      aAct:=TPas2JSActionRegistry.Instance.FindActionByClassName(V);
    if aAct<>Nil then
      aEl.ActionClass:=aAct.ActionClass
    else
      aEl.ActionClass:=Nil;
    end;
  if Assigned(FRemoveList) then
    For I:=CLBRemove.Count-1 downto 0 do
      if CLBRemove.Checked[i] then
        FRemoveList.Remove(CLBRemove.Items.Objects[i]);
end;

procedure TfrmSelectHTMLActionClasses.FormCreate(Sender: TObject);
begin
  Caption:=rsAddHTMLElementActions;
  cbUseDBAware.Caption:=rsUseDBAwareActions;
end;


function TfrmSelectHTMLActionClasses.GetTagClass(const aEl: TElementInfo): THTMLCustomElementActionClass;

begin
  Result:=THTMLElementAction;
  if PreferDB then
    if {%H-}IndexText(aEl.TagName,['input','select','textarea'])<>-1 then
      Result:=TDBHTMLInputElementAction
    else
      Result:=TDBHTMLElementAction;
end;


procedure TfrmSelectHTMLActionClasses.AllocateDefaultClasses;

Var
  I,Idx : Integer;
  aEl : TElementInfo;

begin
  VLEClasses.Strings.Clear;
  For I:=0 to FTags.Count-1 do
    begin
    aEl:=FTags[i];
    aEl.ActionClass:=GetTagClass(aEl);
    Idx:=VLEClasses.Strings.AddObject(aEl.ElementID+'='+aEl.ActionClass.ClassName,aEl);
    VLEClasses.ItemProps[Idx].EditStyle:=esPickList;
    end;
  VLEClasses.Strings.Sort;
end;

procedure TfrmSelectHTMLActionClasses.SetPreferDB(AValue: Boolean);
begin
  if FPreferDB=AValue then Exit;
  FPreferDB:=AValue;
  cbUseDBAware.Checked:=aValue;
  AllocateDefaultClasses;
end;

procedure TfrmSelectHTMLActionClasses.SetRemoveList(AValue: TFPList);
begin
  if FRemoveList=AValue then Exit;
  FRemoveList:=AValue;
  DisplayRemoveList;
end;

procedure TfrmSelectHTMLActionClasses.DisplayRemoveList;

Var
  I : Integer;
  A : THTMLCustomElementAction;

begin
  if (FRemoveList=Nil) or (FRemoveList.Count=0) then
    TSRemove.TabVisible:=False
  else
    begin
    For I:=0 to FRemoveList.Count-1 do
      begin
      A:=THTMLCustomElementAction(FRemoveList[i]);
      clbRemove.Items.AddObject(Format('%s (ID: %s)',[A.Name,A.ElementID]),A);
      end;
    end;
end;

procedure TfrmSelectHTMLActionClasses.SetTags(AValue: TElementInfoList);
begin
  if FTags=AValue then Exit;
  FTags:=AValue;
  AllocateDefaultClasses;
end;

end.

