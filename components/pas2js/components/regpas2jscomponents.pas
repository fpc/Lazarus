unit regpas2jscomponents;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, htmlactions, PropEdits, ComponentEditors;

Type

  { THTMLElementActionListComponentEditor }

  THTMLElementActionListComponentEditor = class(TComponentEditor)
  private
    FActionList: THTMLElementActionList;
    FCompDesigner: TComponentEditorDesigner;
  protected
  public
    constructor Create(AComponent: TComponent;
                       ADesigner: TComponentEditorDesigner); override;
    destructor Destroy; override;
    procedure Edit; override;
    procedure CreateMissing;
    property ActionList: THTMLElementActionList read FActionList write FActionList;
    function GetVerbCount: Integer; override;
    function GetVerb({%H-}Index: Integer): string; override;
    procedure ExecuteVerb({%H-}Index: Integer); override;
  end;

  { TElementIDPropertyEditor }

  TElementIDPropertyEditor = class(TStringPropertyEditor)
  Public
    Function GetHTMLFileName : String;
    function GetAttributes: TPropertyAttributes; override;
    Procedure GetValues(Proc: TGetStrProc); override;
  end;

procedure register;

implementation

uses Types, IDEWindowIntf, forms, dialogs, frmHTMLActionsEditor, strpas2jscomponents, idehtmltools;

{ TElementIDPropertyEditor }

function TElementIDPropertyEditor.GetHTMLFileName: String;

var
  aComponent : TPersistent;

begin
  Result:='';
  aComponent:=GetComponent(0);
  if (aComponent is TComponent) then
    Result:=HTMLTools.GetHTMLFileForComponent(TComponent(aComponent));
end;

function TElementIDPropertyEditor.GetAttributes: TPropertyAttributes;
begin
  Result:=[paMultiSelect,paValueList];
end;

procedure TElementIDPropertyEditor.GetValues(Proc: TGetStrProc);
Var
  FN,aTag : String;
  aTags : TStringDynArray;

begin
  FN:=GetHTMLFileName;
  if FN<>'' then
    begin
    aTags:=HTMLTools.GetTagIDs(FN);
    For aTag in aTags do
      Proc(aTag);
    end;

end;


{ THTMLElementActionListComponentEditor }

constructor THTMLElementActionListComponentEditor.Create(AComponent: TComponent;
  ADesigner: TComponentEditorDesigner);
begin
  inherited Create(AComponent, ADesigner);
  FCompDesigner := ADesigner;
end;

destructor THTMLElementActionListComponentEditor.Destroy;
begin
  inherited Destroy;
end;

procedure THTMLElementActionListComponentEditor.Edit;
var
  AActionList: THTMLCustomElementActionList;
  AEditor: THTMLActionListEditorForm;
begin
  AActionList := GetComponent as THTMLCustomElementActionList;
  if Not Assigned(AActionList) then
    raise Exception.Create('THTMLElementActionListComponentEditor.Edit AActionList=nil');
  AEditor := FindActionEditor(AActionList);
  if not Assigned(AEditor) then begin
    AEditor := THTMLActionListEditorForm.Create(Application);
    AEditor.lstActionName.ItemIndex := -1;
    AEditor.ComponentDesigner := Self.FCompDesigner;
    AEditor.ComponentEditor := Self;
    AEditor.HTMLActionList:=AActionList;
  end;
  SetPopupModeParentForPropertyEditor(AEditor);
  AEditor.ShowOnTop;
end;

procedure THTMLElementActionListComponentEditor.CreateMissing;

var
  AActionList: THTMLCustomElementActionList;
  aCount : Integer;

begin
  AActionList := GetComponent as THTMLCustomElementActionList;
  if Not Assigned(AActionList) then
    raise Exception.Create('THTMLElementActionListComponentEditor.Edit AActionList=nil');
  aCount:=CreateMissingActions(Self,aActionList);
  if (aCount<>-1) then
    ShowMessage(Format(rsHTMLActionsCreated,[aCount]));
end;

function THTMLElementActionListComponentEditor.GetVerbCount: Integer;
begin
  Result := 2;
end;

function THTMLElementActionListComponentEditor.GetVerb(Index: Integer): string;
begin
  case Index of
    0 : Result := rsActionListComponentEditor;
    1 : Result := rsActionListCreateMissing;
  end;
end;

procedure THTMLElementActionListComponentEditor.ExecuteVerb(Index: Integer);
begin
  case Index of
    0 : Edit;
    1 : CreateMissing;
  end;
end;


procedure register;
begin
  RegisterComponents('Pas2js',[THTMLElementActionList]);
  RegisterComponentEditor(THTMLElementActionList,THTMLElementActionListComponentEditor);
  RegisterNoIcon([THTMLCustomElementAction,THTMLElementAction]);
  RegisterPropertyEditor(TypeInfo(String),THTMLCustomElementAction,'ElementID',TElementIDPropertyEditor);
end;

end.

