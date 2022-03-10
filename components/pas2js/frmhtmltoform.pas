unit frmhtmltoform;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, EditBtn,
  ButtonPanel, ExtCtrls, ComCtrls, ActnList, idehtml2class, Types;

type

  { TfrmHTML2Form }

  TfrmHTML2Form = class(TForm)
    aSave: TAction;
    aLoad: TAction;
    alHTMLToForm: TActionList;
    BPHTMLToForm: TButtonPanel;
    cbAddHTMLFile: TCheckBox;
    cbBindElementsInConstructor: TCheckBox;
    cbConstructorArgs: TComboBox;
    cbExtraUnits: TComboBox;
    cbEventModifiers: TComboBox;
    CBEvents: TCheckBox;
    cbEventSignature: TComboBox;
    cbGetElementFunction: TComboBox;
    cbParentName: TComboBox;
    cbBelowID: TComboBox;
    cgAddFunctions: TCheckGroup;
    cgOverrides: TCheckGroup;
    cgVirtualFunctions: TCheckGroup;
    cbDefaultElements: TCheckBox;
    edtFormClassName: TEdit;
    FEHTMLFile: TFileNameEdit;
    FETagMapFile: TFileNameEdit;
    ilHTML2Form: TImageList;
    lblExtraUnits: TLabel;
    lblExclude: TLabel;
    lblClassName: TLabel;
    lblConstructorArgs: TLabel;
    lblEventModifiers: TLabel;
    lblEventSignature: TLabel;
    lblGetElements: TLabel;
    lblHTMLFile: TLabel;
    lblParentClassName: TLabel;
    lblBelowID: TLabel;
    lblTagMap: TLabel;
    mExclude: TMemo;
    odSettings: TOpenDialog;
    PCOptions: TPageControl;
    sdSettings: TSaveDialog;
    ToolBar1: TToolBar;
    tbLoad: TToolButton;
    tbSave: TToolButton;
    TSCodeGen: TTabSheet;
    TSHTML: TTabSheet;
    procedure aLoadExecute(Sender: TObject);
    procedure aSaveExecute(Sender: TObject);
    procedure CBEventsChange(Sender: TObject);
    procedure FEHTMLFileEditingDone(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure TSHTMLContextPopup(Sender: TObject; MousePos: TPoint;
      var Handled: Boolean);

  private
    procedure CheckEventEdits;
    function GetB(AIndex: Integer): Boolean;
    Function GetCB(AIndex: Integer) : TCombobox;
    Function GetCBGroup(AIndex: Integer) : TCheckGroup;
    function GetCBValue(AIndex: Integer): String;
    function GetCheckB(AIndex: Integer): TCheckbox;
    function GetExcludeElements: TStrings;
    function GetFormClassName: String;
    function GetHTMLFileName: String;
    function GetMethods(AIndex: Integer) : TSpecialMethods;
    function GetTagMapFileName: String;
    procedure SetB(AIndex: Integer; AValue: Boolean);
    procedure SetCBValue(AIndex: Integer; AValue: String);
    procedure SetExcludeElements(AValue: TStrings);
    procedure SetFormClassName(AValue: String);
    procedure SetHTMLFileName(AValue: String);
    procedure SetMethods(AIndex: Integer; AValue: TSpecialMethods);
    procedure SetTagMapFileName(AValue: String);
  public
    Procedure LoadOptions(aOptions : THTML2ClassOptions);
    Procedure SaveOptions(aOptions : THTML2ClassOptions);
    Property Overrides : TSpecialMethods index 1 Read GetMethods Write SetMethods;
    Property AddMethods : TSpecialMethods index 2 Read GetMethods Write SetMethods;
    Property VirtualMethods : TSpecialMethods index 3 Read GetMethods Write SetMethods;
    Property HTMLFileName : String Read GetHTMLFileName Write SetHTMLFileName;
    Property TagMapFileName : String Read GetTagMapFileName Write SetTagMapFileName;
    Property FormClassname : String Read GetFormClassName Write SetFormClassName;
    Property ParentClassName : String Index 1 Read GetCBValue Write SetCBValue;
    Property GetElementFunction : String Index 2 Read GetCBValue Write SetCBValue;
    Property EventSignature : String Index 3 Read GetCBValue Write SetCBValue;
    Property EventModifiers : String Index 4 Read GetCBValue Write SetCBValue;
    Property ConstructorArgs : String Index 5 Read GetCBValue Write SetCBValue;
    Property BelowID : String Index 6 Read GetCBValue Write SetCBValue;
    Property ExtraUnits : String Index 7 Read GetCBValue Write SetCBValue;
    Property GenerateEventHandlers : Boolean Index 1 Read GetB Write SetB;
    Property UseDefaultElements : Boolean Index 2 Read GetB Write SetB;
    Property AddHTMLToProject : Boolean Index 3 Read GetB Write SetB;
    Property BindElementsInConstructor : Boolean Index 4 Read GetB Write SetB;
    Property ExcludeElements : TStrings Read GetExcludeElements Write SetExcludeElements;
  end;

var
  frmHTML2Form: TfrmHTML2Form;

implementation

uses fpjson, lazideintf;

{$R *.lfm}

{ TfrmHTML2Form }

function TfrmHTML2Form.GetCheckB(AIndex: Integer): TCheckbox;

begin
  case aIndex of
    1 : Result:=CBEvents;
    2 : Result:=cbDefaultElements;
    3 : Result:=cbAddHTMLFile;
    4 : Result:=cbBindElementsInConstructor;
  else
    Raise Exception.CreateFmt('Invalid checkbox index %d',[aIndex]);
  end;
end;

procedure TfrmHTML2Form.FormCreate(Sender: TObject);

Var
  aDir : String;

begin
  if Assigned(LazarusIDE.ActiveProject) then
    aDir:=ExtractFilePath(LazarusIDE.ActiveProject.ProjectInfoFile)
  else
    aDir:=GetUserDir;
  FEHTMLFile.InitialDir:=aDir;
  FETagMapFile.InitialDir:=aDir;
end;

procedure TfrmHTML2Form.TSHTMLContextPopup(Sender: TObject; MousePos: TPoint;
  var Handled: Boolean);
begin

end;

procedure TfrmHTML2Form.CBEventsChange(Sender: TObject);
begin
  CheckEventEdits;
end;

procedure TfrmHTML2Form.aSaveExecute(Sender: TObject);

Var
  aOptions : THTML2ClassOptions;
  aJSON : TJSONStringType;
  fd : TStringStream;

begin
  if sdSettings.Execute then
    begin
    fd:=nil;
    aOptions:=THTML2ClassOptions.Create;
    try
      SaveOptions(aOptions);
      aJSON:=aOptions.asJSON(True);
      FD:=TStringStream.Create(aJSON,CP_UTF8);
      FD.SaveToFile(sdSettings.FileName);
    finally
      FD.Free;
      aoptions.Free;
    end;
    end;
end;

procedure TfrmHTML2Form.aLoadExecute(Sender: TObject);

Var
  aOptions : THTML2ClassOptions;
  aJSON : TJSONStringType;
  fd : TStringStream;

begin
  if odSettings.Execute then
    begin
    fd:=nil;
    aOptions:=THTML2ClassOptions.Create;
    try
      FD:=TStringStream.Create('',CP_UTF8);
      FD.LoadFromFile(odSettings.FileName);
      aJSON:=FD.DataString;
      aOptions.FromJSON(aJSON);
      LoadOptions(aOptions);
    finally
      FD.Free;
      aoptions.Free;
    end;
    end;
end;

procedure TfrmHTML2Form.FEHTMLFileEditingDone(Sender: TObject);
begin
  if (FEHTMLFile.FileName<>'') and FileExists(FEHTMLFile.FileName) then
    With THTMLExtractIDS.Create(Self) do
      try
        ExtractIDS(FEHTMLFile.FileName,cbBelowID.Items);
      finally
        Free;
      end;
end;

procedure TfrmHTML2Form.CheckEventEdits;

Var
  En : Boolean;

  Procedure DoIt(CB : TCombobox);

  begin
    CB.Enabled:=En;
    if not CB.Enabled then
      CB.Text:='';
  end;

begin
  En:=CBEvents.Enabled;
  DoIt(cbEventSignature);
  DoIt(cbEventModifiers);
end;

function TfrmHTML2Form.GetB(AIndex: Integer): Boolean;
begin
  Result:=GetCheckB(aIndex).Checked;
end;

function TfrmHTML2Form.GetCB(AIndex: Integer): TCombobox;
begin
  Case aIndex of
    1 : Result:=cbParentName;
    2 : Result:=cbGetElementFunction;
    3 : Result:=cbEventSignature;
    4 : Result:=cbEventModifiers;
    5 : Result:=cbConstructorArgs;
    6 : Result:=cbBelowID;
    7 : Result:=cbExtraUnits;
  else
    Raise Exception.CreateFmt('Unknown CB index : %d',[aIndex]);
  end;
end;

function TfrmHTML2Form.GetCBGroup(AIndex: Integer): TCheckGroup;
begin
  Case aIndex of
    1 : Result:=cgOverrides;
    2 : Result:=cgAddFunctions;
    3 : Result:=cgVirtualFunctions;
  else
    Raise Exception.CreateFmt('Unknown CG index : %d',[aIndex]);
  end;
end;

function TfrmHTML2Form.GetCBValue(AIndex: Integer): String;
begin
  Result:=GetCB(aIndex).Text;
end;

function TfrmHTML2Form.GetExcludeElements: TStrings;
begin
  Result:=mExclude.Lines;
end;

function TfrmHTML2Form.GetFormClassName: String;
begin
  Result:=edtFormClassName.text;
end;

function TfrmHTML2Form.GetHTMLFileName: String;
begin
  Result:=FEHTMLFile.FileName;
end;

function TfrmHTML2Form.GetMethods(AIndex: Integer): TSpecialMethods;

Var
  gm : TSpecialMethod;
  cbg : TCheckGroup;

begin
  Result:=[];
  cbg:=GetCBGroup(aIndex);
  For gm In TSpecialMethod do
    if CBG.Checked[Ord(Gm)] then
      Include(Result,gm);
end;

function TfrmHTML2Form.GetTagMapFileName: String;
begin
  Result:=FETagMapFile.Filename;
end;

procedure TfrmHTML2Form.SetB(AIndex: Integer; AValue: Boolean);
begin
  GetCheckB(aIndex).Checked:=aValue;
end;

procedure TfrmHTML2Form.SetCBValue(AIndex: Integer; AValue: String);
begin
  GetCB(aIndex).Text:=aValue;
end;

procedure TfrmHTML2Form.SetExcludeElements(AValue: TStrings);
begin
  mExclude.Lines:=aValue;
end;

procedure TfrmHTML2Form.SetFormClassName(AValue: String);
begin
  edtFormClassName.text:=aValue;
end;

procedure TfrmHTML2Form.SetHTMLFileName(AValue: String);
begin
  FEHTMLFile.FileName:=aValue;
end;

procedure TfrmHTML2Form.SetMethods(AIndex: Integer; AValue: TSpecialMethods);
Var
  gm : TSpecialMethod;
  cbg : TCheckGroup;

begin
  cbg:=GetCBGroup(aIndex);
  For gm in TSpecialMethod do
    cbg.Checked[Ord(Gm)]:=gm in aValue;
end;

procedure TfrmHTML2Form.SetTagMapFileName(AValue: String);
begin
  FETagMapFile.FileName:=aValue;
end;

procedure TfrmHTML2Form.LoadOptions(aOptions: THTML2ClassOptions);
begin
  Overrides:=aOptions.OverrideMethods;
  AddMethods:=aOptions.AddMethods;
  VirtualMethods:=aOptions.VirtualMethods;
  ParentClassName:=aOptions.ParentClassName;
  GetElementFunction:=aOptions.GetElementFunction;
  EventSignature:=aOptions.EventSignature;
  EventModifiers:=aOptions.EventModifiers;
  ConstructorArgs:=aOptions.ConstructorArgs;
  BelowID:=aOptions.BelowID;
  HTMLFileName:=aOptions.HTMLFileName;
  TagMapFileName:=aOptions.TagMapFileName;
  FormClassname:=aOptions.FormClassname;
  UseDefaultElements:=aOptions.UseDefaultElements;
  AddHTMLToProject:=aOptions.AddHTMLToProject;
  ExcludeElements:=aOptions.ExcludeElements;
  ExtraUnits:=aOptions.ExtraUnits;
  GenerateEventHandlers:=foEvents in aOptions.FormOptions;
  BindElementsInConstructor:=foBindInConstructor in aOptions.FormOptions;
end;

procedure TfrmHTML2Form.SaveOptions(aOptions: THTML2ClassOptions);

  Procedure SetFormOption(aOption : TFormOption; aEnable : Boolean);

  begin
    If aEnable then
      aOptions.FormOptions:=aOptions.FormOptions+[aOption]
    else
      aOptions.FormOptions:=aOptions.FormOptions-[aOption]
  end;

begin
  aOptions.OverrideMethods:=Overrides;
  aOptions.AddMethods:=AddMethods;
  aOptions.VirtualMethods:=VirtualMethods;
  aOptions.ParentClassName:=ParentClassName;
  aOptions.GetElementFunction:=GetElementFunction;
  aOptions.EventSignature:=EventSignature;
  aOptions.EventModifiers:=EventModifiers;
  aOptions.ConstructorArgs:=ConstructorArgs;
  aOptions.BelowID:=BelowID;
  aOptions.HTMLFileName:=HTMLFileName;
  aOptions.TagMapFileName:=TagMapFileName;
  aOptions.FormClassname:=FormClassname;
  aOptions.UseDefaultElements:=UseDefaultElements;
  aOptions.AddHTMLToProject:=AddHTMLToProject;
  aOptions.ExcludeElements:=ExcludeElements;
  aOptions.ExtraUnits:=ExtraUnits;
  SetFormOption(foEvents,GenerateEventHandlers);
  SetFormOption(foBindInConstructor,BindElementsInConstructor);
end;

end.

