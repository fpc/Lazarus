{ JSON data viewer main form

  Copyright (C) 2010 Michael Van Canneyt michael@freepascal.org

  This source is free software; you can redistribute it and/or modify it under
  the terms of the GNU General Public License as published by the Free
  Software Foundation; either version 2 of the License, or (at your option)
  any later version.

  This code is distributed in the hope that it will be useful, but WITHOUT ANY
  WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
  FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
  details.

  A copy of the GNU General Public License is available on the World Wide Web
  at <http://www.gnu.org/copyleft/gpl.html>. You can also obtain it by writing
  to the Free Software Foundation, Inc., 51 Franklin Street - Fifth Floor,
  Boston, MA 02110-1335, USA.
}

unit frmmain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpJSON, jsonscanner, JSONParser, frarest, ExtCtrls,
  Forms, Controls, Dialogs, ActnList, Menus, ComCtrls, IniPropStorage, PropertyStorage,
  DefaultTranslator, SynEdit, SynHighlighterJScript;

type

  { TMainForm }

  { TJSONTab }
  TViewerOptions = Class(TObject)
  {$IF FPC_FULLVERSION>=30002}
    FOptions : TJSONOptions;
  {$ELSE}
    FStrict,
  {$ENDIF}
    FQuoteStrings,
    FSortObjectMembers,
    FCompact,
    FNewObject,
    FSaveFormatted: Boolean;
  end;

  TJSONTab = Class(TTabsheet)
  private
    FCurrentFind: TTreeNode;
    FDocNo: Integer;
    FFileName: String;
    FIsRequestResult: Boolean;
    FJSONData: TJSONData;
    FModified: Boolean;
    FOptions: TViewerOptions;
    FTreeView: TTreeview;
    FPageControl : TPageControl;
    FSyn : TSynEdit;
    procedure DoTabChange(Sender: TObject);
    procedure JSONFromPreview;
    procedure SetCurrentFind(AValue: TTreeNode);
    procedure SetDocNo(AValue: Integer);
    procedure SetFileName(AValue: String);
    procedure SetIsRequestResult(AValue: Boolean);
    procedure SetJSONData(AValue: TJSONData);
    procedure SetModified(AValue: Boolean);
  Protected
    procedure CreatePageControl; virtual;
    Procedure SetCaption; virtual;
  Public
    Constructor Create(AOwner : TComponent); override;
    Destructor Destroy; override;
    procedure ShowJSONData(AParent: TTreeNode; Data: TJSONData);
    procedure ShowJSONDocument;
    Procedure ShowJSONDocumentText;
    Property FileName : String read FFileName Write SetFileName;
    Property TVJSON : TTreeview Read FTreeView;
    // We own JSON
    Property Root : TJSONData Read FJSONData Write SetJSONData;
    Property CurrentFind : TTreeNode Read FCurrentFind Write SetCurrentFind;
    Property Modified : Boolean Read FModified Write SetModified;
    Property DocNo : Integer Read FDocNo Write SetDocNo;
    Property IsRequestResult : Boolean Read FIsRequestResult Write SetIsRequestResult;
    // Just a reference
    Property Options : TViewerOptions Read FOptions Write FOptions;
  end;

  TMainForm = class(TForm)
    ACopy: TAction;
    AClose: TAction;
    ACreateCode: TAction;
    AAddToFavourites: TAction;
    AFindNext: TAction;
    AFind: TAction;
    AExpandCurrentContainer: TAction;
    AExpandAll: TAction;
    APasteAsDocument: TAction;
    APaste: TAction;
    ACut: TAction;
    ADeleteValue: TAction;
    ANewBooleanValue: TAction;
    ANewNullValue: TAction;
    ANewNumberValue: TAction;
    ANewStringValue: TAction;
    ANewObject: TAction;
    ANewArray: TAction;
    AQuit: TAction;
    ASaveAs: TAction;
    ASave: TAction;
    AOpen: TAction;
    ANew: TAction;
    ALJSON: TActionList;
    FDJSON: TFindDialog;
    ILJSON: TImageList;
    MEDit: TMenuItem;
    MenuItem10: TMenuItem;
    MenuItem11: TMenuItem;
    MenuItem12: TMenuItem;
    MenuItem13: TMenuItem;
    MenuItem14: TMenuItem;
    MenuItem15: TMenuItem;
    MISaveFormatted: TMenuItem;
    MSepFavourites: TMenuItem;
    MFavourites: TMenuItem;
    MIGenCode: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    MenuItem6: TMenuItem;
    MenuItem7: TMenuItem;
    MenuItem9: TMenuItem;
    MIQuoteStrings: TMenuItem;
    MIAllowTrailingComma: TMenuItem;
    MIAllowComments: TMenuItem;
    MICompact: TMenuItem;
    MIFInd: TMenuItem;
    MIExpandCurrent: TMenuItem;
    MIExpandAll: TMenuItem;
    MIPasteAsDocument: TMenuItem;
    MIpaste: TMenuItem;
    MICut: TMenuItem;
    MICopy: TMenuItem;
    MISortMembers: TMenuItem;
    MenuItem8: TMenuItem;
    MIDelete: TMenuItem;
    PCJSON: TPageControl;
    PMTreeView: TPopupMenu;
    PSMain: TIniPropStorage;
    MenuItem1: TMenuItem;
    MINewNull: TMenuItem;
    MINewNumber: TMenuItem;
    MINewBoolean: TMenuItem;
    MINewString: TMenuItem;
    MINewArray: TMenuItem;
    MINewObject: TMenuItem;
    MIdocument: TMenuItem;
    MIStrict: TMenuItem;
    MOptions: TMenuItem;
    MIInsert: TMenuItem;
    MIQuit: TMenuItem;
    MISaveAs: TMenuItem;
    MISave: TMenuItem;
    MIOpen: TMenuItem;
    MINew: TMenuItem;
    MFile: TMenuItem;
    MMJSON: TMainMenu;
    ODJSON: TOpenDialog;
    SDJSON: TSaveDialog;
    SynJScriptSyn1: TSynJScriptSyn;
    TBJSON: TToolBar;
    TBNew: TToolButton;
    TBNewButton: TToolButton;
    TBOpen: TToolButton;
    TBSave: TToolButton;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    ToolButton4: TToolButton;
    TBNEwNull: TToolButton;
    TBNewBoolean: TToolButton;
    TBNewNumber: TToolButton;
    TBNewString: TToolButton;
    TBNewArray: TToolButton;
    ToolButton5: TToolButton;
    TBShowRest: TToolButton;
    procedure AAddToFavouritesExecute(Sender: TObject);
    procedure AAddToFavouritesUpdate(Sender: TObject);
    procedure ACloseExecute(Sender: TObject);
    procedure ACloseUpdate(Sender: TObject);
    procedure ACopyExecute(Sender: TObject);
    procedure ACopyUpdate(Sender: TObject);
    procedure ACreateCodeExecute(Sender: TObject);
    procedure ACreateCodeUpdate(Sender: TObject);
    procedure ACutExecute(Sender: TObject);
    procedure ACutUpdate(Sender: TObject);
    procedure ADeleteValueExecute(Sender: TObject);
    procedure ADeleteValueUpdate(Sender: TObject);
    procedure AExpandAllExecute(Sender: TObject);
    procedure AExpandAllUpdate(Sender: TObject);
    procedure AExpandCurrentContainerExecute(Sender: TObject);
    procedure AExpandCurrentContainerUpdate(Sender: TObject);
    procedure AFindExecute(Sender: TObject);
    procedure AFindNextExecute(Sender: TObject);
    procedure AFindNextUpdate(Sender: TObject);
    procedure ANewArrayExecute(Sender: TObject);
    procedure ANewBooleanValueExecute(Sender: TObject);
    procedure ANewNullValueExecute(Sender: TObject);
    procedure ANewNumberValueExecute(Sender: TObject);
    procedure ANewObjectExecute(Sender: TObject);
    procedure ANewStringValueExecute(Sender: TObject);
    procedure APasteAsDocumentExecute(Sender: TObject);
    procedure APasteExecute(Sender: TObject);
    procedure APasteUpdate(Sender: TObject);
    procedure AQuitExecute(Sender: TObject);
    procedure ASaveExecute(Sender: TObject);
    procedure ContainerAvailable(Sender: TObject);
    procedure ANewExecute(Sender: TObject);
    procedure AOpenExecute(Sender: TObject);
    procedure FDJSONFind(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure HaveData(Sender: TObject);
    procedure MIAllowTrailingCommaClick(Sender: TObject);
    procedure MIAllowCommentsClick(Sender: TObject);
    procedure MICompactClick(Sender: TObject);
    procedure MIdocumentClick(Sender: TObject);
    procedure MIQuoteStringsClick(Sender: TObject);
    procedure MISaveFormattedClick(Sender: TObject);
    procedure MISortMembersClick(Sender: TObject);
    procedure MIStrictClick(Sender: TObject);
    procedure PCJSONCloseTabClicked(Sender: TObject);
    procedure PSMainStoredValues0Restore(Sender: TStoredValue; var Value: TStoredType);
    procedure PSMainStoredValues1Restore(Sender: TStoredValue; var Value: TStoredType);
    procedure PSMainStoredValues2Restore(Sender: TStoredValue; var Value: TStoredType);
    procedure PSMainStoredValues3Restore(Sender: TStoredValue; var Value: TStoredType);
    procedure PSMainStoredValues6Restore(Sender: TStoredValue; var Value: TStoredType);
    procedure PSMainStoredValues7Restore(Sender: TStoredValue; var Value: TStoredType);
    procedure PSMainStoredValues8Restore(Sender: TStoredValue; var Value: TStoredType);
    procedure TBShowRestClick(Sender: TObject);
    procedure TVJSONEdited(Sender: TObject; Node: TTreeNode; var S: string);
    procedure TVJSONEditing(Sender: TObject; Node: TTreeNode;
      var AllowEdit: Boolean);
  private
    FFavouritesFileName : String;
    FOptions : TViewerOptions;
    PRest : TPanel;
    FRest : TRestFrame;
    FSplitter : TSplitter;
    FRestPanelHeight : Integer;
    procedure BuildFavourites;
    function CheckClose(T: TJSONTab): Boolean;
    procedure CloseCurrent;
    procedure DoFavouriteClick(Sender: TObject);
    procedure DoRebuildFavourites(Sender: TObject);
    procedure GetCurrentJSON(Sender: TObject; Stream: TStream);
    function GetDocNo: Integer;
    procedure ShowRequestJSON(Sender: TObject; Stream: TStream);
    Procedure ShowRestpanel;
    Procedure HideRestPanel;
    function GetCurrentFind: TTreeNode;
    function GetCurrenTJSONTab: TJSONTab;
    function GetCurrentRoot: TJSONData;
    function GetTVJSON: TTreeView;
    procedure setCurrentFind(AValue: TTreeNode);
    procedure AddDataToContainer(const AMemberName: String; D: TJSONData);
    procedure CopyCurrentData;
    procedure DeleteCurrentValue;
    function FindNode(Start: TTreeNode; const AText: String;  CaseInsensitive: Boolean; WholeWord: Boolean): TTreeNode;
    function GetNextSearchNode(Anode: TTreeNode): TTreeNode;
    procedure Modify;
    Function NewJSONTab : TJSONTab;
    procedure AddNewValue(AType: TJSONType);
    function CurrentNode: TTreeNode;
    function CurrentNodeType : TJSONType;
    function CurrentData: TJSONData;
    function CurrentContainerNode: TTreeNode;
    function CurrentContainertype: TJSONtype;
    Function CurrentContainer: TJSONData;
    function FindContainerNode(AStart: TTreeNode): TTreeNode;
    function GetSaveFileName(Force: Boolean): String;
    function IsContainerNode(ANode: TTreeNode): Boolean;
    Function NewDocument : TJSONTab;
    procedure OpenFile(const AFileName: String);
    procedure PasteJSON(DoClear: Boolean);
    procedure SaveToFile(const AFileName: string);
    procedure SetCaption;
    procedure ShowJSONDocument;
    {$IF FPC_FULLVERSION>=30002}
    procedure ToggleOption(O: TJSONOption; Enable: Boolean);
    {$ENDIF}
    Property CurrentJSONTab : TJSONTab Read GetCurrenTJSONTab;
    Property CurrentFind : TTreeNode Read GetCurrentFind Write setCurrentFind;
    Property CurrentRoot : TJSONData Read GetCurrentRoot;
    Property TVJSON : TTreeView Read GetTVJSON;
  public
  end;

var
  MainForm: TMainForm;

implementation

uses
  typinfo,  {$IF FPC_FULLVERSION>=30004} frmcreatecode, {$endif}
msgjsonviewer, lcltype, frmNewBoolean, frmNewINteger, frmNewString, clipbrd;

{$R *.lfm}
Const
  ImageTypeMap : Array[TJSONtype] of Integer =
//      jtUnknown, jtNumber, jtString, jtBoolean, jtNull, jtArray, jtObject
     (-1,8,9,7,6,5,4);
  JSONTypeNames : Array[TJSONtype] of string =
     ('Unknown','Number','String','Boolean','Null','Array','Object');

{ TJSONTab }

procedure TJSONTab.SetJSONData(AValue: TJSONData);
begin
  if (FJSONData=AValue) then Exit;
  FreeAndNil(FJSONData);
  FJSONData:=AValue;
  Modified:=True;
  ShowJSONDocument;
end;

procedure TJSONTab.SetModified(AValue: Boolean);
begin
  if FModified=AValue then Exit;
  FModified:=AValue;
  SetCaption;
end;

procedure TJSONTab.SetFileName(AValue: String);
begin
  if FFileName=AValue then Exit;
  FFileName:=AValue;
  SetCaption;
end;

procedure TJSONTab.SetIsRequestResult(AValue: Boolean);
begin
  if FIsRequestResult=AValue then Exit;
  FIsRequestResult:=AValue;
  SetCaption;
end;

procedure TJSONTab.SetCurrentFind(AValue: TTreeNode);
begin
  if FCurrentFind=AValue then Exit;
  FCurrentFind:=AValue;
end;

procedure TJSONTab.SetDocNo(AValue: Integer);
begin
  if FDocNo=AValue then Exit;
  FDocNo:=AValue;
  SetCaption;
end;



procedure TJSONTab.DoTabChange(Sender: TObject);
begin
  If ((Sender as TPageControl).ActivePageIndex=1) then
    ShowJSONDocumentText else JSONFromPreview;
end;

constructor TJSONTab.Create(AOwner: TComponent);

begin
  inherited Create(AOwner);
  CreatePageControl;
end;


Procedure TJSONTab.CreatePageControl;

Var
  TS : TTabSheet;

begin
  FPageControl:=TPageControl.Create(Self.Owner);
  FPageControl.Parent:=Self;
  FPageControl.Align:=alClient;
  FPageControl.TabPosition:=tpBottom;
  FPageControl.OnChange:=@DoTabChange;
  // Visual
  TS:=TTabsheet.Create(Self.Owner);
  TS.Caption:=STabCaptionVisual;
  TS.Parent:=FPageControl;
  FTreeView:=TTreeview.Create(Self.Owner);
  FTreeView.Parent:=TS;
  FTreeView.Options:= [tvoAutoItemHeight,tvoKeepCollapsedNodes,tvoRightClickSelect,tvoShowButtons,tvoShowLines,tvoShowRoot,tvoToolTips,tvoThemedDraw];
  FTreeView.Align:=alClient;
  // Raw
  TS:=TTabsheet.Create(Self.Owner);
  TS.Caption:=STabCaptionRaw;
  TS.Parent:=FPageControl;
  FSyn:=TSynEdit.Create(Self.Owner);
  FSyn.align:=alClient;
  FSyn.Parent:=TS;
  FSyn.Highlighter:=MainForm.SynJScriptSyn1;
  FSyn.Highlighter.Enabled:=True;
  SetCaption;
end;

destructor TJSONTab.Destroy;
begin
  FreeAndNil(FJSONData);
  inherited Destroy;
end;

procedure TJSONTab.SetCaption;

Var
  S: String;

begin
  S:=ExtractFileName(FFileName);
  if S='' then
    if IsRequestResult then
      S:=Format('Request %d result',[docNo])
    else
      S:=Format('New file %d',[docNo]);
  if Modified then
    S:=S+' *';
  Caption:=S;
end;

{ TMainForm }
{$IF FPC_FULLVERSION>=30002}
procedure TMainForm.ToggleOption(O : TJSONOption; Enable : Boolean);
Var
  S : String;
begin
  if Enable then
    Include(FOptions.Foptions,O)
  else
    Exclude(FOptions.Foptions,O);
  S:=GetEnumName(TypeInfo(TJSONOption),Ord(O));
  Delete(S,1,2);
  PSMain.StoredValue[S]:=IntToStr(Ord(Enable));
end;
{$ENDIF}

procedure TMainForm.MIStrictClick(Sender: TObject);
begin
{$IF FPC_FULLVERSION>=30002}
  ToggleOption(joStrict,(Sender as TMenuItem).Checked);
{$ELSE}
  FStrict:=(Sender as TMenuItem).Checked;
  PSMain.StoredValue['strict']:=IntToStr(Ord(Fstrict));
{$ENDIF}
end;

procedure TMainForm.PCJSONCloseTabClicked(Sender: TObject);
begin
  if CheckClose(Sender as TJSONTab) then
    Application.ReleaseComponent(Sender as TJSONTab);
end;

procedure TMainForm.PSMainStoredValues0Restore(Sender: TStoredValue;
  var Value: TStoredType);

{$IF FPC_FULLVERSION>=30002}
Var
  S : String;
  o : integer;
  JO : TJSONOption;
{$ENDIF}
begin
  {$IF FPC_FULLVERSION>=30002}
  S:=Sender.Name;
  O:=GetEnumValue(TypeInfo(TJSONOption),'jo'+S);
  if O<>-1 then
    begin
    JO:=TJSONOption(O);
    if StrToIntDef(Value,0)=1 then
        Include(FOptions.Foptions,JO)
      else
        Exclude(FOptions.Foptions,JO);
    end;
  {$ELSE}
  FStrict:=StrToIntDef(Value,0)=1
  {$ENDIF}
end;

procedure TMainForm.PSMainStoredValues1Restore(Sender: TStoredValue;
  var Value: TStoredType);
begin
  FOptions.FNewObject:=StrToIntDef(Value,0)=1
end;

procedure TMainForm.PSMainStoredValues2Restore(Sender: TStoredValue;
  var Value: TStoredType);
begin
  FOptions.FSortObjectMembers:=StrToIntDef(Value,0)=1;
end;

procedure TMainForm.PSMainStoredValues3Restore(Sender: TStoredValue;
  var Value: TStoredType);
begin
  FOptions.FCompact:=StrToIntDef(Value,0)=1;
end;

procedure TMainForm.PSMainStoredValues6Restore(Sender: TStoredValue;
  var Value: TStoredType);
begin
  FOptions.FQuoteStrings:=StrToIntDef(Value,0)=1;
end;

procedure TMainForm.PSMainStoredValues7Restore(Sender: TStoredValue; var Value: TStoredType);
begin
  FRestPanelHeight:=StrToIntDef(Value,0);
end;

procedure TMainForm.PSMainStoredValues8Restore(Sender: TStoredValue;
  var Value: TStoredType);
begin
  FOptions.FSaveFormatted:=StrToIntDef(Value,0)=1;
end;

procedure TMainForm.TBShowRestClick(Sender: TObject);
begin
  if TBShowRest.Down then
    ShowRestPanel
  else
    HideRestPanel;
end;

procedure TMainForm.TVJSONEdited(Sender: TObject; Node: TTreeNode; var S: string);

Var
  D : TJSONData;
  O : TJSONObject;
  L,I : Integer;

begin
  D:=CurrentData;
  If (Node.Data=Nil) then
    begin
    // Member name change
    O:=CurrentContainer as TJSONObject;
    I:=O.IndexOfName(S);
    If (I=-1) then
      begin
      I:=O.IndexOf(D);
      O.Extract(I);
      O.Add(S,D);
      end
    else
      begin
      if (O.Items[i]<>D) then
        begin
        ShowMessage(Format(SDuplicateMemberName,[S]));
        S:=O.Names[I];
        end
      end;
    end
  else
    begin
    // value change
    try
      L:=Length(S);
      if FOptions.FQuoteStrings and (L>=2) and (S[1]='"') and (S[L]='"') then
        D.AsString:=Copy(S,2,L-2)
      else
        D.AsString:=S;
    except
      ShowMessage(Format(SErrInvalidValue,[S]));
      S:=D.AsString;
    end
    end;
  Modify;
end;

procedure TMainForm.TVJSONEditing(Sender: TObject; Node: TTreeNode;
  var AllowEdit: Boolean);

begin
  if (Node.Data=Nil) then
    // Label node. Allow member name change for objects
    AllowEdit:=(CurrentContainerType=jtObject)
  else
    // value node. Allow change for simple not null values
    AllowEdit:=Not (CurrentNodeType in [jtNull,jtArray,jtObject]);
end;

function TMainForm.GetCurrentFind: TTreeNode;

Var
  T : TJSONTab;

begin
  T:=CurrentJSONTab;
  If Assigned(T) then
    Result:=T.CurrentFind
  else
    Result:=Nil;
end;

function TMainForm.GetCurrenTJSONTab: TJSONTab;
begin
  if PCJSON.ActivePage is TJSONTab then
    Result:=PCJSON.ActivePage as TJSONTab
  else
    Result:=Nil;
end;

function TMainForm.GetCurrentRoot: TJSONData;
Var
  T : TJSONTab;

begin
  T:=GetCurrenTJSONTab;
  if T=Nil then
    Result:=nil
  else
    Result:=T.Root;
end;

function TMainForm.GetTVJSON: TTreeView;

Var
  T : TJSONTab;

begin
  T:=GetCurrenTJSONTab;
  if T=Nil then
    Result:=nil
  else
    Result:=T.TVJSON;
end;

procedure TMainForm.setCurrentFind(AValue: TTreeNode);

Var
  T : TJSONTab;

begin
  T:=CurrentJSONTab;
  If Assigned(T) and (AValue.TreeView=T.TVJSON) then
    T.CurrentFind:=aValue
end;

procedure TMainForm.Modify;
begin
  SetCaption;
end;

function TMainForm.GetDocNo : Integer;

Var
  I,DC : Integer;

begin
  DC:=1;
  For I:=0 to PCJSON.PageCount-1 do
    If (PCJSON.Pages[i] is TJSONTab)
      and (TJSONTab(PCJSON.Pages[i]).FileName='') then
      Inc(DC);
  Result:=DC;
end;

function TMainForm.NewJSONTab: TJSONTab;

Var
  DC : Integer;

begin
  DC:=GetDocNo;
  Result:=TJSONTab.Create(Self);
  Result.PageControl:=PCJSON;
  Result.TVJSON.PopupMenu:=PMTreeView;
  Result.TVJSON.Images:=ILJSON;
  Result.TVJSON.OnEdited:=@TVJSONEdited;
  Result.TVJSON.OnEditing:=@TVJSONEditing;
  Result.Options:=FOptions;
  Result.DocNo:=DC;
  Result.FileName:='';
  Result.ImageIndex:=16;
  If FOptions.FNewObject then
    Result.Root:=TJSONObject.Create;
end;

procedure TMainForm.SetCaption;

Var
  FN : String;

begin
  If (CurrentJSONTab=Nil) or (CurrentJSONTab.FileName='') then
    FN:=SEmpty
  else
    FN:=CurrentJSONTab.FileName;
  If CurrentJSONTab.Modified then
    FN:=FN+' *';
  Caption:=SCaption+' ['+FN+']';
end;

procedure TMainForm.ANewExecute(Sender: TObject);
begin
  NewDocument;
end;

function TMainForm.NewDocument: TJSONTab;

begin
  Result:=NewJSONTab;
  PCJSON.ActivePage:=Result;
{  FreeAndNil(FRoot);
  If FNewObject then
    FRoot:=TJSONObject.Create;
  ShowJSONDocument;
  FFileName:='';
  SetCaption;}
end;

procedure TMainForm.ContainerAvailable(Sender: TObject);

begin
  (Sender as Taction).Enabled:=Assigned(TVJSON) and ((TVJSON.Items.Count=0) or (Nil<>CurrentContainer));
end;

procedure TMainForm.ASaveExecute(Sender: TObject);

Var
  S : String;

begin
  S:=GetSaveFileName(Sender=ASaveAs);
  If (S<>'') then
    SaveToFile(S);
end;

procedure TMainForm.AQuitExecute(Sender: TObject);
begin
  Close;
end;

procedure TMainForm.ANewNullValueExecute(Sender: TObject);
begin
  AddNewValue(jtNull);
end;

procedure TMainForm.ANewArrayExecute(Sender: TObject);
begin
  AddNewValue(jtArray);
end;

procedure TMainForm.ADeleteValueUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled:=(CurrentNodeType<>jtUnknown)
end;

procedure TMainForm.AExpandAllExecute(Sender: TObject);
begin
  With TVJSON do
    if (Items.Count>0) then
      Items[0].Expand(True);
end;

procedure TMainForm.AExpandAllUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled:=Assigned(CurrentJSONTab) and Assigned(CurrentJSONTab.Root);
end;

procedure TMainForm.AExpandCurrentContainerExecute(Sender: TObject);

Var
  N : TTreeNode;

begin
  N:=CurrentContainerNode;
  If Assigned(N) then
    N.Expand(True);
end;

procedure TMainForm.AExpandCurrentContainerUpdate(Sender: TObject);
begin
  (Sender as TACtion).Enabled:=(CurrentContainerType<>jtUnknown)
end;

procedure TMainForm.AFindExecute(Sender: TObject);
begin
  With FDJSON do
    Execute;
end;

procedure TMainForm.AFindNextExecute(Sender: TObject);
begin
  FDJSONFind(Sender);
end;

procedure TMainForm.AFindNextUpdate(Sender: TObject);
begin
  (Sender as TAction).ENabled:=Assigned(CurrentJSONTab) and (CurrentJSONTab.CurrentFind<>Nil)
end;

procedure TMainForm.ADeleteValueExecute(Sender: TObject);

begin
  DeleteCurrentValue;
end;

procedure TMainForm.DeleteCurrentValue;

Var
  PN : TTreeNode;
  P,D : TJSONData;

begin
  D:=CurrentData;
  If (CurrentContainerNode=CurrentNode) then
    PN:=FindContainerNode(CurrentNode.Parent)
  else
    PN:=CurrentContainerNode;
 If (PN=Nil) then
   begin
   CurrentJSONTab.Root:=Nil;
   end
 else
   begin
   P:=TJSONData(PN.Data);
   If P.JSONType=jtArray then
     TJSONArray(P).Remove(D)
   else If P.JSONType=jtObject then
     TJSONObject(P).Remove(D);
   PN:=PN.Parent;
   If PN<>Nil then
     begin
     PN.DeleteChildren;
     CurrentJSONTab.ShowJSONData(PN,P);
     end
   else
     CurrentJSONTab.ShowJSONDocument;
   end;
   Modify;
end;

procedure TMainForm.ACopyUpdate(Sender: TObject);
begin
  (Sender as Taction).Enabled:=Assigned(CurrentData);
end;

procedure TMainForm.ACreateCodeExecute(Sender: TObject);
begin
  {$IF FPC_FULLVERSION>=30004}
  CreateCodeFromJSON(CurrentRoot);
  {$endif}
end;

procedure TMainForm.ACreateCodeUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled:=Assigned(CurrentRoot);
end;

procedure TMainForm.ACutExecute(Sender: TObject);
begin
  CopyCurrentData;
  DeleteCurrentValue;
end;

procedure TMainForm.ACopyExecute(Sender: TObject);
begin
  CopyCurrentData;
end;

procedure TMainForm.ACloseUpdate(Sender: TObject);

begin
  (Sender as TAction).Enabled:=Assigned(CurrentJSONTab);
end;

procedure TMainForm.ACloseExecute(Sender: TObject);

begin
  CloseCurrent;
end;

procedure TMainForm.AAddToFavouritesUpdate(Sender: TObject);

begin
  (Sender as TAction).Enabled:=Assigned(FRest) and (FRest.HaveFavouriteData);
end;

procedure TMainForm.AAddToFavouritesExecute(Sender: TObject);

begin
  if Assigned(FRest) then
    FRest.AddtoFavourites;
end;

procedure TMainForm.CloseCurrent;

Var
  T : TJSONTab;

begin
  T:=CurrentJSONTab;
  if CheckClose(t) then
    T.Free;
end;

procedure TMainForm.GetCurrentJSON(Sender: TObject; Stream: TStream);

Var
  S : TJSONStringType;

begin
  if Assigned(CurrentData) then
    begin
    S:=CurrentData.FormatJSON();
    Stream.WriteBuffer(S[1],Length(S));
    end;
end;

procedure TMainForm.ShowRequestJSON(Sender: TObject; Stream: TStream);

Var
  D : TJSONData;
  DC : Integer;

begin
  Stream.Position:=0;
  D:=GetJSON(Stream);
  DC:=GetDocNo;
  With NewDocument do
    begin
    Root:=D;
    IsRequestResult:=True;
    DocNo:=DC;
    end;
end;

procedure TMainForm.ShowRestpanel;


begin
  if Assigned(PRest) then
    begin
    PRest.Visible:=True;
    FSplitter.Visible:=True;
    Exit;
    end;
  PRest:=TPanel.Create(Self);
  FRest:=TRestFrame.Create(self);
  PRest.BevelInner:=bvNone;
  PRest.BevelOuter:=bvNone;
  PRest.Top:=TBJSON.Top+TBJSON.Height+3;
  if FRestPanelHeight>0 then
    PRest.Height:=FRestPanelHeight
  else
    PRest.Height:=FRest.Height+1;
  PRest.Parent:=Self;
  PRest.Align:=alTop;
  FSplitter:=TSplitter.Create(Self);
  FSplitter.Align:=alTop;
  FSplitter.Top:=PRest.Top+PRest.Height+3;
  FSplitter.Parent:=Self;
  FRest.Parent:=PRest;
  FRest.Align:=alClient;
  FRest.OnSendContent:=@GetCurrentJSON;
  FRest.OnContentReceived:=@ShowRequestJSON;
  FRest.OnFavouritesChanged:=@DoRebuildFavourites;
  if FileExists(FFavouritesFileName) then
    FRest.LoadFavourites(FFavouritesFileName);
  MFavourites.Visible:=True;
end;

procedure TMainForm.HideRestPanel;

begin
  if not Assigned(PRest) then
    Exit;
  PRest.Visible:=False;
  FSplitter.Visible:=False;
  MFavourites.Visible:=False;
end;

Function TMainForm.CheckClose(T : TJSONTab) : Boolean;

begin
  Result:=Not T.Modified;
  if Result then
    Exit;
  case QuestionDlg(SDocumentModified,Format(SDocumentModifiedAction,[T.Caption]), mtWarning,[
    mrNo,SDiscard,
    mrYes,SSaveData,
    mrCancel,SCancelClose],0) of
    mrNo : Result:=True;
  mrYes :
    begin
    SaveToFile(GetSaveFileName(T.FileName=''));
    Result:=True;
    end;
  end;
end;

procedure TMainForm.CopyCurrentData;

Var
  D : TJSONData;

begin
  D:=CurrentData;
  If Not Assigned(D) then
    exit;
  Clipboard.Clear;
  ClipBoard.AsText:=D.AsJSON;
end;

procedure TMainForm.ACutUpdate(Sender: TObject);
begin
  (Sender as Taction).Enabled:=Assigned(CurrentData);
end;

procedure TMainForm.ANewBooleanValueExecute(Sender: TObject);
begin
  AddNewValue(jtBoolean);
end;

procedure TMainForm.ANewNumberValueExecute(Sender: TObject);
begin
  AddNewValue(jtNumber);
end;

procedure TMainForm.ANewObjectExecute(Sender: TObject);
begin
  AddNewValue(jtObject);
end;

procedure TMainForm.ANewStringValueExecute(Sender: TObject);
begin
  AddNewValue(jtString);
end;

procedure TMainForm.APasteAsDocumentExecute(Sender: TObject);
begin
  PasteJSON(True);
end;

procedure TMainForm.APasteExecute(Sender: TObject);
begin
  PasteJSON(False);
end;

procedure TMainForm.PasteJSON(DoClear : Boolean);

Var
  P : TJSONParser;
  D : TJSONData;
  N : String;
  T : TJSONTab;

begin
  D:=Nil;
  try
{$IF FPC_FULLVERSION>=30002}
    P:=TJSONParser.Create(Clipboard.AsText,[]);
    P.Options:=FOptions.FOptions;
{$ELSE}
    P:=TJSONParser.Create(Clipboard.AsText);
    P.Strict:=FStrict;
{$ENDIF}
    try
      D:=P.Parse;
    finally
      P.Free;
    end;
  except
    On E : Exception do
      ShowMessage(SErrNoValidJSONClipBoard)
  end;
  N:=SNewMember;
  T:=CurrentJSONTab;
  If DoClear then
    begin
    T:=NewDocument;
    Application.ProcessMessages;
    N:='';
    end
  else If CurrentContainerType=jtObject then
    if not InputQuery(SNewMember,Format(SNewMemberName,[JSONTypeNames[D.JSONType]]),N) then
      begin
      D.Free;
      Exit;
      end;
  AddDataToContainer(N,D);
end;

procedure TMainForm.APasteUpdate(Sender: TObject);
begin
//  (Sender as TAction).Enabled:=ClipBoard.HasFormat(Clipboard.FindFormatID('text/plain'));
  (Sender as TAction).Enabled:=ClipBoard.HasFormat(CF_TEXT);
end;

procedure TMainForm.AddNewValue(AType : TJSONType);

  Function NewMemberName : string;

  begin
    Case CurrentContainerType of
      jtUnknown : Result:= '';
      jtObject  : Result:=SNewMember;
      jtArray   : Result:=Format(SElement,[TJSONArray(CurrentContainer).Count]);
    end;
  end;

Var
  D : TJSONData;
  N : String;

begin
  Case AType of
    jtNull,
    jtObject,
    jtArray :
      begin
      N:=SNewMember;
      If (CurrentContainerType=jtObject) then
        if not InputQuery(SNewMember,Format(SNewMemberName,[JSONTypeNames[AType]]),N) then
           Exit;
      Case AType of
         jtNull : D:=TJSONNull.Create;
         jtObject : D:=TJSONObject.Create;
         jtArray : D:=TJSONArray.Create;
      end;
      end;
    jtBoolean:
      begin
      With TNewBooleanForm.Create(Self) do
        try
          MemberName:=NewMemberName;
          AllowName:=CurrentContainerType=jtObject;
          If (ShowModal<>mrOK) then
            Exit;
          N:=MemberName;
          D:=TJSONBoolean.Create(Value);
        finally
          Free;
        end;
      end;
    jtString:
      begin
        With TNewStringForm.Create(Self) do
          try
            MemberName:=NewMemberName;
            AllowName:=CurrentContainerType=jtObject;
            If (ShowModal<>mrOK) then
              Exit;
            N:=MemberName;
            D:=TJSONString.Create(Value);
          finally
            Free;
          end;
      end;
    jtNumber:
      begin
        With TNewNumberForm.Create(Self) do
          try
            MemberName:=NewMemberName;
            AllowName:=CurrentContainerType=jtObject;
            NumberType:=ntInteger;
            If (ShowModal<>mrOK) then
              Exit;
            N:=MemberName;
            Case NumberType of
              ntInteger : D:=TJSONIntegerNumber.Create(AsInteger);
              ntFloat   : D:=TJSONFloatNumber.Create(AsFloat);
              ntInt64   : D:=TJSONInt64Number.Create(AsInt64);
            end;
          finally
            Free;
          end;
      end;
  end;
  AddDataToContainer(N,D);
end;

procedure TMainForm.AddDataToContainer(Const AMemberName : String; D : TJSONData);

Var
  P : TTreeNode;
  I : Integer;
  T : TJSONTab;

begin
  T:=CurrentJSONTab;
  if T=Nil then
    Raise Exception.Create('Cannot determine current JSON document');
  Case CurrentContainerType of
    jtUnknown :
       begin
       T.Root:=D;
       P:=Nil;
       end;
    jtObject :
       begin
       TJSONObject(CurrentContainer).Add(AmemberName,D);
       P:=TVJSON.Items.AddChild(CurrentContainerNode,AmemberName)
       end;
    jtArray:
       begin
       I:=TJSONArray(CurrentContainer).Add(D);
       P:=TVJSON.Items.AddChild(CurrentContainerNode,IntToStr(I))
       end;
  end;
  Modify;
  If Assigned(P) then
    begin
    P.ImageIndex:=ImageTypeMap[D.JSONType];
    P.SelectedIndex:=ImageTypeMap[D.JSONType];
    P.MakeVisible;
    T.ShowJSONData(P,D);
    end;
end;

function TMainForm.CurrentNode: TTreeNode;

Var
  T : TTreeView;
begin
  T:=TVJSON;
  if Assigned(T) then
    Result:=T.Selected
  else
    Result:=Nil;
end;

function TMainForm.CurrentNodeType: TJSONType;

Var
  D : TJSONData;

begin
  D:=CurrentData;
  If (D=Nil) then
    Result:=jtUnknown
  else
    Result:=D.JSONType;
end;

Procedure TMainForm.SaveToFile(Const AFileName : string);

Var
  S : String;
  F : TFileStream;

begin
  if (AFileName<>'') then
  begin
    F:=TFileStream.Create(AFileName,fmCreate);
    try
      If Assigned(CurrentJSONTab.Root) then
      if FOptions.FSaveFormatted then
        S:=CurrentJSONTab.Root.FormatJSON else
        S:=CurrentJSONTab.Root.AsJSON;
      If length(S)>0 then
        F.WriteBuffer(S[1],Length(S));
      CurrentJSONTab.Modified:=False;
    finally
      F.Free;
    end;
    CurrentJSONTab.FileName:=AFileName;
    SetCaption;
    end;
end;

Function TMainForm.GetSaveFileName(Force : Boolean) : String;

begin
  Result:=CurrentJSONTab.FileName;
  If Force or (Result='') then
    with SDJSON do
      begin
      FileName:=Result;
      If Execute then
        Result:=FileName
      else
        Result:=''
      end;
end;


Function TMainForm.CurrentData : TJSONData;

Var
  N : TTreeNode;

begin
  N:=CurrentNode;
  If (N=Nil) then
    Result:=Nil
  else
    begin
    Result:=TJSONData(N.Data);
    If (Result=Nil) and (N.Count=1) then
      Result:=TJSONData(N.Items[0].Data);
    end;
end;

Function TMainForm.CurrentContainerType : TJSONtype;

Var
  D : TJSONData;

begin
  D:=CurrentContainer;
  If (D=Nil) then
    Result:=jtUnknown
  else
    Result:=D.JSONType;
end;

Function TMainForm.IsContainerNode(ANode : TTreeNode) : Boolean;

begin
  Result:=Assigned(ANode)
          and Assigned(ANode.Data)
          and (TJSONData(ANode.Data).JSONType in [jtArray,jtObject]);
end;

Function TMainForm.FindContainerNode(AStart : TTreeNode) : TTreeNode;

begin
  Result:=Astart;
  While (Result<>Nil) and Not IsContainerNode(Result) do
    Result:=Result.Parent;
end;

Function TMainForm.CurrentContainerNode : TTreeNode;

begin
  Result:=FindContainerNode(CurrentNode);
end;

Function TMainForm.CurrentContainer : TJSONData;

Var
  N : TTreeNode;

begin
  N:=CurrentContainerNode;
  If (N<>Nil) then
    Result:=TJSONData(N.Data)
  else
    Result:=Nil
end;

procedure TMainForm.AOpenExecute(Sender: TObject);

begin
  With ODJSON do
    begin
    if Assigned(CurrentJSONTab) then
      FileName:=CurrentJSONTab.FileName;
    If Execute then
      OpenFile(FileName)
    end;
end;

procedure TMainForm.FDJSONFind(Sender: TObject);

Var
  N : TTreeNode;

begin
  If (CurrentFind=Nil) then
    begin
    If (frEntireScope in FDJSON.Options) and (TVJSON.Items.Count>0) then
      CurrentFind:=TVJSON.Items[0]
    else
      CurrentFind:=TVJSON.Selected;
    end
  else
    CurrentFind:=GetNextSearchNode(CurrentFind);
  If (CurrentFind=Nil) then
    Exit;
  With FDJSON do
    N:=FindNode(CurrentFind,FindText,Not (frMatchCase in Options), frWholeWord in Options);
  If Assigned(N) then
    begin
    N.MakeVisible;
    TVJSON.Selected:=N;
    end
  else
    ShowMessage(SNoMoreMatches);
  CurrentFind:=N;
end;

procedure TMainForm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  if Assigned(FRest) then
    PSMain.StoredValue['RestPanelHeight']:=IntToStr(FRest.Parent.Height);
  if Assigned(FRest) then
    FRest.SaveFavourites(FFavouritesFileName);
end;

Function TMainForm.GetNextSearchNode(Anode : TTreeNode) : TTreeNode;

begin
  Result:=Nil;
  If (ANode=Nil) then
    Exit;
  If (ANode.Count>0) then
    Result:=ANode.GetFirstChild
  else
    Result:=ANode.GetNextSibling;
  While (Result=Nil) and (ANode<>Nil) do
    begin
    ANode:=ANode.Parent;
    if assigned(ANode) then
      Result:=ANode.GetNextSibling;
    end;
end;

Function TMainForm.FindNode(Start : TTreeNode; Const AText: String; CaseInsensitive : Boolean; WholeWord : Boolean) : TTreeNode;

  Function Match(Const ST : String; ANode : TTreeNode) : boolean;

  Var
    NT : String;

  begin
    If CaseInsensitive then
      NT:=Uppercase(ANode.Text)
    else
      NT:=ANode.Text;
    If WholeWord then
      Result:=(NT=ST)
    else
      Result:=(Pos(ST,NT)>0);
  end;

Var
  ST : String;

begin
  If CaseInsensitive then
    ST:=UpperCase(AText)
  else
    ST:=AText;
  Result:=Start;
  While (Result<>Nil) and not Match(ST,Result)  do
    Result:=GetNextSearchNode(Result);
end;

procedure TMainForm.FormCloseQuery(Sender: TObject; var CanClose: boolean);

Var
  I : Integer;
  T : TJSONTab;

begin
  CanClose:=True;
  I:=0;
  While CanClose and (I<PCJSON.PageCount) do
    begin
    if (PCJSON.Pages[i] is TJSONTab) then
      begin
      T:=PCJSON.Pages[i] as TJSONTab;
      CanClose:=CheckClose(T);
      end;
    Inc(I);
    end;
end;

procedure TMainForm.FormCreate(Sender: TObject);

Var
  S : String;

begin
  FOptions:=TViewerOptions.Create;
  S:=GetAppConfigFile(false,true);
  PSMain.IniFileName:=S;
  S:=ExtractFilePath(S);
  If not ForceDirectories(S) then
    ShowMessage(Format(SErrCreatingConfigDir,[S]));
  FFavouritesFileName:=S+'favourites.json';
  PSMain.Active:=True;
{$IF FPC_FULLVERSION<30002}
  MIAllowTrailingComma.Visible:=False;
  MIAllowComments.Visible:=False;
{$ENDIF}
{$IF FPC_FULLVERSION<30004}
  ACreateCode.Visible:=False;
{$endif}
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  FOptions.Free;
end;

procedure TMainForm.FormShow(Sender: TObject);
begin
  if (ParamCount>0)  and FileExists(ParamStr(1)) then
    OpenFile(ParamStr(1))
//  else
//    NewDocument;
end;

procedure TMainForm.HaveData(Sender: TObject);
begin
  (Sender as TAction).Enabled:=(CurrentRoot<>Nil);
end;

Type
   TFavouriteMenuitem = Class(TMenuItem)
     RequestData : TRequestData;
   end;

procedure TMainForm.DoFavouriteClick(Sender : TObject);

begin
  if Assigned(FRest) then
    FRest.ApplyFavourite((Sender as TFavouriteMenuitem).RequestData);
end;

procedure TMainForm.DoRebuildFavourites(Sender: TObject);
begin
  BuildFavourites;
end;

procedure TMainForm.BuildFavourites;

Var
  I : integer;
  M : TFavouriteMenuitem;
  A : Array of TMenuItem;

begin
  For I:=MFavourites.Count-1 downto 0 do
    if MFavourites.Items[i] is TFavouriteMenuitem then
      MFavourites.Items[i].Free;
  SetLength(A,FRest.Favourites.Count);
  For I:=0 to FRest.Favourites.Count-1 do
    begin
    M:=TFavouriteMenuitem.Create(Self);
    M.RequestData:=FRest.Favourites[i];
    M.Caption:=FRest.Favourites[i].Name;
    M.OnClick:=@DoFavouriteClick;
    A[i]:=M;
    end;
  MFavourites.Add(A);
end;

procedure TMainForm.MIAllowTrailingCommaClick(Sender: TObject);
begin
  {$IF FPC_FULLVERSION>=30002}
    ToggleOption(joIgnoreTrailingComma,(Sender as TMenuItem).Checked);
  {$ENDIF}
end;

procedure TMainForm.MIAllowCommentsClick(Sender: TObject);
begin
  {$IF FPC_FULLVERSION>=30002}
    ToggleOption(joComments,(Sender as TMenuItem).Checked);
  {$ENDIF}
end;

procedure TMainForm.ShowJSONDocument;

Var
  I : Integer;

begin
  For I:=0 to PCJSON.PageCount-1 do
    if PCJSON.Pages[i] is TJSONTab then
      (PCJSON.Pages[i] as TJSONTab).ShowJSONDocument;
end;

procedure TMainForm.MICompactClick(Sender: TObject);
begin
  FOptions.FCompact:=MICompact.Checked;
  PSMain.StoredValue['compact']:=IntToStr(Ord(FOptions.FCompact));
  ShowJSONDocument;
end;

procedure TMainForm.MIdocumentClick(Sender: TObject);
begin
  FOptions.FNewObject:=(Sender as TMenuItem).Checked;
  PSMain.StoredValue['object']:=IntToStr(Ord(FOptions.FNewObject));
end;

procedure TMainForm.MIQuoteStringsClick(Sender: TObject);
begin
  FOptions.FQuoteStrings:=MICompact.Checked;
  PSMain.StoredValue['QuoteStrings']:=IntToStr(Ord(FOptions.FQuoteStrings));
  ShowJSONDocument;
end;

procedure TMainForm.MISaveFormattedClick(Sender: TObject);
begin
  FOptions.FSaveFormatted :=(Sender as TMenuItem).Checked;
  PSMain.StoredValue['SaveFormatted']:=IntToStr(Ord(FOptions.FSaveFormatted));
  ShowJSONDocument;
end;

procedure TMainForm.MISortMembersClick(Sender: TObject);
begin
  FOptions.FSortObjectMembers:=(Sender as TMenuItem).Checked;
  ShowJSONDocument;
end;

procedure TMainForm.OpenFile(Const AFileName : String);

Var
  S : TFileStream;
  P : TJSONParser;
  D : TJSONData;
begin
  S:=TFileStream.Create(AFileName,fmOpenRead or fmShareDenyWrite);
  try
{$IF FPC_FULLVERSION>=30002}
    P:=TJSONParser.Create(S,[]);
{$ELSE}
    P:=TJSONParser.Create(S);
{$ENDIF}
    try
{$IF FPC_FULLVERSION>=30002}
      P.Options:=P.Options+[joStrict];
{$ENDIF}
      D:=P.Parse;
    finally
      P.Free;
    end;
  finally
    S.Free;
  end;
  With NewDocument do
      begin
      FileName:=AFileName;
      Root:=D;
      Modified:=False;
      end;
  SetCaption;
//  NewDocument.ShowJSONDocument;
end;

procedure TJSONTab.ShowJSONDocument;

begin
  ShowJSONDocumentText;
  With TVJSON.Items do
    begin
    BeginUpdate;
    try
      TVJSON.Items.Clear;
      ShowJSONData(Nil,Root);
      With TVJSON do
        If (Items.Count>0) and Assigned(Items[0]) then
          begin
          Items[0].Expand(False);
          Selected:=Items[0];
          end;
    finally
      EndUpdate;
    end;
    end;
end;

procedure TJSONTab.JSONFromPreview;
var P : TJSONParser;
D : TJSONData;
begin
  try
    {$IF FPC_FULLVERSION>=30002}
     P:=TJSONParser.Create(FSyn.Text,[]);
     P.Options:=P.Options+[joStrict, joComments];
    {$ELSE}
     P:=TJSONParser.Create(FSyn.Text);
    {$ENDIF}
    D:=P.Parse;
    Root:=D;
    Modified:=true;
  finally
    P.Free;
  end;
end;

procedure TJSONTab.ShowJSONDocumentText;
begin
  IF Assigned(Root) then FSyn.Text:=Root.FormatJSON();
end;

procedure TJSONTab.ShowJSONData(AParent : TTreeNode; Data : TJSONData);

Var
  N,N2 : TTreeNode;
  I : Integer;
  D : TJSONData;
  C : String;
  S : TStringList;

begin
  if Not Assigned(Data) then
    exit;
  if Options.FCompact and (AParent<>Nil) then
    N:=AParent
  else
    N:=TVJSON.Items.AddChild(AParent,'');
  Case Data.JSONType of
    jtArray,
    jtObject:
      begin
      If (Data.JSONType=jtArray) then
        C:=SArray
      else
        C:=SObject;
      C:=Format(C,[Data.Count]);
      S:=TstringList.Create;
      try
        For I:=0 to Data.Count-1 do
          If Data.JSONtype=jtArray then
            S.AddObject(IntToStr(I),Data.items[i])
          else
            S.AddObject(TJSONObject(Data).Names[i],Data.items[i]);
        If Options.FSortObjectMembers and (Data.JSONType=jtObject) then
          S.Sort;
        For I:=0 to S.Count-1 do
          begin
          N2:=TVJSON.Items.AddChild(N,S[i]);
          D:=TJSONData(S.Objects[i]);
          N2.ImageIndex:=ImageTypeMap[D.JSONType];
          N2.SelectedIndex:=ImageTypeMap[D.JSONType];
          ShowJSONData(N2,D);
          end
      finally
        S.Free;
      end;
      end;
    jtNull:
      C:=SNull;
  else
    C:=Data.AsString;
    if Options.FQuoteStrings and  (Data.JSONType=jtString) then
      C:='"'+C+'"';
  end;
  If Assigned(N) then
    begin
    If N.Text='' then
      N.Text:=C
    else
      N.Text:=N.Text+': '+C;
    N.ImageIndex:=ImageTypeMap[Data.JSONType];
    N.SelectedIndex:=ImageTypeMap[Data.JSONType];
    N.Data:=Data;
    end;
end;

end.

