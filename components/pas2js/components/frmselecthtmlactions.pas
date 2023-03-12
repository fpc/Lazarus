unit frmselecthtmlactions;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  ButtonPanel, stub.htmlactions, idehtml2class, ValEdit;

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
    function GetInfo(aIndex : Integer): TElementINfo;
  Public
    Constructor Create; overload;
    Property Infos[aIndex : Integer] : TElementINfo Read GetInfo; default;
  end;

  { TfrmSelectHTMLActionClasses }

  TfrmSelectHTMLActionClasses = class(TForm)
    bpHTMLActions: TButtonPanel;
    cbUseDBAware: TCheckBox;
    pnlTop: TPanel;
    VLEClasses: TValueListEditor;
    procedure cbUseDBAwareChange(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure VLEClassesGetPickList(Sender: TObject; const {%H-}KeyName: string;
      Values: TStrings);
  private
    FPreferDB: Boolean;
    FTags: TElementInfoList;
    procedure AllocateDefaultClasses;
    function GetTagClass(const aEl: TElementInfo): THTMLCustomElementActionClass;
    procedure SaveSelectedClasses;
    procedure SetPreferDB(AValue: Boolean);
    procedure SetTags(AValue: TElementInfoList);

  public
    Property PreferDB : Boolean Read FPreferDB Write SetPreferDB;
    // Extra info Expected to be in TTagInfo object attached to string
    Property Tags : TElementInfoList Read FTags Write SetTags;
  end;

Function SelectHTMLActionClasses(aTags : TElementInfoList; aPreferDB : Boolean) : Boolean;

implementation

uses strutils, p2jselementactions, stub.data.HTMLActions, strpas2jscomponents;

{$R *.lfm}

Function SelectHTMLActionClasses(aTags : TElementInfoList; aPreferDB : Boolean) : Boolean;

begin
  With  TfrmSelectHTMLActionClasses.Create(Application) do
    try
      PreferDB:=aPreferDB;
      Tags:=aTags;
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
    aAct:=TPas2JSActionRegistry.Instance.FindActionByClassName(V);
    if aAct<>Nil then
      aEl.ActionClass:=aAct.ActionClass;
    end;
end;

procedure TfrmSelectHTMLActionClasses.FormCreate(Sender: TObject);
begin
  Caption:=rsAddHTMLElementActions;
  cbUseDBAware.Caption:=rsUseDBAwareActions;
end;


Function TfrmSelectHTMLActionClasses.GetTagClass(Const aEl : TElementInfo) : THTMLCustomElementActionClass;

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

procedure TfrmSelectHTMLActionClasses.SetTags(AValue: TElementInfoList);
begin
  if FTags=AValue then Exit;
  FTags:=AValue;
  AllocateDefaultClasses;
end;

end.

