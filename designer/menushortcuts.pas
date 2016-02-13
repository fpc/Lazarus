unit MenuShortcuts;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, strutils, types,
  ActnList, ButtonPanel, Controls, Dialogs, StdCtrls, Menus,
  Forms, Graphics, LCLType, LCLIntf, LCLProc,
  // LazUtils
  LazUTF8,
  // IdeIntf
  IDEDialogs, PropEdits,
  // IDE
  LazarusIDEStrConsts;

type
  TSCKind = (scUnknown,
             scMenuItemSC, scMenuItemKey2, scMenuItemAccel,
             scActionSC, scActionSecondary, scActionAccel,
             scOtherCompAccel);
  TDisplayType = (dtNone, dtBlack, dtBlackBold, dtGreyed, dtGreyedBold);
  TDisplayClickEvent = procedure(isHeader: boolean; index: integer) of object;

const
  Margin = 6;
  Double_Margin = Margin shl 1;
  Leading = 4;
  Double_Leading = Leading shl 1;
  Treble_Leading = Leading + Double_Leading;
  VDim = 20;
  VTextOffset = 2;
  Header_Color = TColor($00EDEFD6);

  Accelerator_Kinds = [scMenuItemAccel, scActionAccel, scOtherCompAccel];
  MenuItem_Kinds = [scMenuItemSC, scMenuItemKey2, scMenuItemAccel];
  ShortcutOnly_Kinds = [scMenuItemSC, scMenuItemKey2, scActionSC, scActionSecondary];
  //#todo extend this list, or use one from elsewhere in LCL?
  ShortCutKeys: array[0..48] of word = (VK_UNKNOWN,
    VK_0, VK_1, VK_2, VK_3, VK_4, VK_5, VK_6, VK_7, VK_8, VK_9,
    VK_A, VK_B, VK_C, VK_D, VK_E, VK_F, VK_G, VK_H, VK_I, VK_J, VK_K, VK_L,
    VK_M, VK_N, VK_O, VK_P, VK_Q, VK_R, VK_S, VK_T, VK_U, VK_V, VK_W, VK_X,
    VK_Y, VK_Z, VK_F1, VK_F2, VK_F3, VK_F4, VK_F5, VK_F6, VK_F7, VK_F8,
    VK_F9, VK_F10, VK_F11, VK_F12);


type

  { TSCInfo }

  TSCInfo = class(TObject)
  strict private
    FComponent: TComponent;
    FComponentName: string;
    FKind: TSCKind;
    FShortcut: TShortCut;
    function GetAction: TAction;
    function GetCaption: string;
    function GetMenuItem: TMenuItem;
    function GetToCompositeString: string;
  public
    constructor CreateWithParams(aComponent: TComponent; aKind: TSCKind; aSC: TShortCut);
    property Action: TAction read GetAction;
    property Caption: string read GetCaption;
    property Component: TComponent read FComponent;
    property ComponentName: string read FComponentName;
    property Kind: TSCKind read FKind;
    property MenuItem: TMenuItem read GetMenuItem;
    property Shortcut: TShortCut read FShortcut;
    property ToCompositeString: string read GetToCompositeString;
  end;

  { TSCList }

  TSCList = class(TObject)
  strict private
    FAcceleratorsInContainerCount: integer;
    FInitialDuplicates: TFPList;
    FScanList: TStringList;
    FShortcutsInContainerCount: integer;
    FUniqueList: TFPList;
    function GetInitialDuplicatesCount: integer;
    function GetScanListCompName(index: integer): string;
    function GetUniqueCount: integer;
  public
    constructor Create;
    destructor Destroy; override;
    function FindUniqueInfoForShortcut(aSC: TShortCut): TSCInfo;
    function UniqueListContainsShortcut(aSC: TShortCut): boolean;
    procedure ClearAllLists;
    procedure ScanContainerForShortcutsAndAccelerators;
    procedure ScanContainerForShortcutsOnly;
    procedure ScanSCListForDuplicates;
    procedure SortByComponentPropertyName;
    property AcceleratorsInContainerCount: integer read FAcceleratorsInContainerCount
                                                  write FAcceleratorsInContainerCount;
    property InitialDuplicates: TFPList read FInitialDuplicates;
    property InitialDuplicatesCount: integer read GetInitialDuplicatesCount;
    property ScanList: TStringList read FScanList;
    property ScanListCompName[index: integer]: string read GetScanListCompName;
    property ShortcutsInContainerCount: integer read FShortcutsInContainerCount
                                               write FShortcutsInContainerCount;
    property UniqueCount: integer read GetUniqueCount;
  end;

  { TAddShortcutDialog }

  TAddShortcutDialog = class(TForm)
  strict private
    FButtonPanel: TButtonPanel;
    FMenuItem: TMenuItem;
    FNewShortcut: TShortCut;
    FOldShortcut: TShortCut;
    FShortCutGrabBox: TShortCutGrabBox;
    procedure OKButtonClick(Sender: TObject);
    procedure OnGrabBoxCloseUp(Sender: TObject);
  public
    constructor CreateWithMenuItem(AOwner: TComponent; aMI: TMenuItem; isMainSC: boolean; aSC: TShortCut);
    property NewShortcut: TShortCut read FNewShortcut;
    property OldShortcut: TShortCut write FOldShortcut;
  end;

  TMenuShortcuts = class;

  { TEditShortcutCaptionDialog }

  TEditShortcutCaptionDialog = class(TForm)
  strict private
    FEditingCaption: boolean;
    FInfo: TSCInfo;
    FNewCaption: string;
    FNewShortcut: TShortCut;
    FOldCaption: string;
    // GUI controls
    FButtonPanel: TButtonPanel;
    FEdit: TEdit;
    FGrabBox: TCustomShortCutGrabBox;
    FGroupBox: TGroupBox;
    FShortcuts: TMenuShortcuts;
    procedure CaptionEditChange(Sender: TObject);
    procedure GrabBoxEnter(Sender: TObject);
    procedure GrabBoxExit(Sender: TObject);
    procedure OKButtonOnClick(Sender: TObject);
  protected
    procedure Activate; override;
  public
    constructor {%H-}CreateNew(aShortcuts: TMenuShortcuts; aSCInfo: TSCInfo);
    property NewCaption: string read FNewCaption;
    property NewShortcut: TShortCut read FNewShortcut;
  end;

  TDualDisplay = class;

  TContents = class(TCustomControl)
  private
    FCol1MaxTextWidth: integer;
    FCol2MaxTextWidth: integer;
    FDualDisplay: TDualDisplay;
    FOnContentsClick: TModalDialogFinished;
    FSList: TStringList;
  protected
    procedure DoContentsClick(anIndex: integer);
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure Paint; override;
    property Col1MaxTextWidth: integer read FCol1MaxTextWidth;
    property Col2MaxTextWidth: integer read FCol2MaxTextWidth;
    property SList: TStringList read FSList;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure AddToList(const aLine: string; aDisplayType: TDisplayType=dtBlack);
    procedure Clear;
    property OnContentsClick: TModalDialogFinished read FOnContentsClick write FOnContentsClick;
  end;

  { THeader }

  THeader = class(TCustomControl)
  private
    FCol1Header: string;
    FCol2Header: string;
    FColumn1TextWidth: integer;
    FDisplayType: TDisplayType;
    FDualDisplay: TDualDisplay;
    FOnHeaderClick: TModalDialogFinished;
  protected
    procedure DoHeaderClick(anIndex: integer);
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure Paint; override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure AddHeader(const aHeader: string; aDisplayType: TDisplayType);
    procedure Clear;
    property Column1TextWidth: integer read FColumn1TextWidth;
    property OnHeaderClick: TModalDialogFinished read FOnHeaderClick write FOnHeaderClick;
  end;

  { TDualDisplay }

  TDualDisplay = class(TCustomControl)
  private
    FCol1Right: integer;
    FContents: TContents;
    FHeader: THeader;
    FOnDisplayClick: TDisplayClickEvent;
    FSBox: TScrollBox;
    FUpdating: boolean;
    function GetContentsCount: integer;
    procedure HeaderContentsClick(Sender: TObject; index: integer);
    procedure SetCol1Right(AValue: integer);
  protected
    class function GetControlClassDefaultSize: TSize; override;
    function TextWidth(const aText: string): integer;
    property Updating: boolean read FUpdating;
  public
    constructor Create(AOwner: TComponent); override;
    procedure AddHeader(const aHeader: string; aDT: TDisplayType=dtBlackBold);
    procedure AddLine(const aLine: string; aDT: TDisplayType=dtBlack);
    procedure BeginUpdate;
    procedure Clear;
    procedure ClearContents;
    procedure ClearHeader;
    procedure EndUpdate;
    procedure InvalidateContents;
    property Col1Right: integer read FCol1Right write SetCol1Right;
    property ContentsCount: integer read GetContentsCount;
    property OnDisplayClick: TDisplayClickEvent read FOnDisplayClick write FOnDisplayClick;
  end;

  { TMenuShortcuts }

  TMenuShortcuts = class
  private
    FShortcutList: TSCList;
    FShortcutMenuItemsCount: integer;
    FShortcutConflictsCount: integer;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Initialize;
    procedure UpdateShortcutList(includeAccelerators: boolean=False);
    procedure ResetMenuItemsCount;
    function Statistics(aShortcutCount: integer): string;
  public
    property ShortcutList: TSCList read FShortcutList;
    property ShortcutMenuItemsCount: integer read FShortcutMenuItemsCount;
    //property ShortcutConflictsCount: integer read FShortcutConflictsCount;
  end;

function AmpersandStripped(const aText: string): string;
function AddNewOrEditShortcutDlg(aMI: TMenuItem; isMainSCut: boolean;
                                 var aShortcut: TShortCut): boolean;
function HasAccelerator(const aText: string; out aShortcut: TShortCut): boolean;
function NewShortcutOrCaptionIsValidDlg(aConflictingInfo: TSCInfo;
                                        out aNewShortcut: TShortCut;
                                        out aNewCaption: string): boolean;
function KindToPropertyName(aKind: TSCKind): string;
function SplitCommaText(const aCommaText: string; out firstBit: string): string;
function SortByComponentPropertyName(List: TStringList; Index1, Index2: Integer): Integer;


implementation

function AmpersandStripped(const aText: string): string;
var
  p: integer;
begin
  Result:=aText;
  p:=Pos('&', Result);
  while (p > 0) do begin
    Delete(Result, p, 1);
    p:=Pos('&', Result);
  end;
end;

function AddNewOrEditShortcutDlg(aMI: TMenuItem; isMainSCut: boolean;
  var aShortcut: TShortCut): boolean;
var
  dlg: TAddShortcutDialog;
begin
  dlg:=TAddShortcutDialog.CreateWithMenuItem(nil, aMI, isMainSCut, aShortcut);
  try
    if (dlg.ShowModal = mrOK) then
    begin
      aShortcut:=dlg.NewShortcut;
      Result:=True;
    end
    else
      Result:=False;
  finally
    dlg.Free;
  end;
end;

function HasAccelerator(const aText: string; out aShortcut: TShortCut): boolean;
var
  p, UTF8Len: integer;
  accelStr: string;
begin
  Result := False;
  aShortcut := 0;
  if aText = '' then Exit;
  p := 0;
  repeat
    p := PosEx('&', aText, p+1);
    if (p = 0) or (p = Length(aText)) then Break;
    if aText[p+1] <> '&' then  // '&&' is reduced to '&' by widgetset GUI.
    begin
      UTF8Len := UTF8CharacterLength(@aText[p+1]);
      accelStr := UTF8UpperCase(Copy(aText, p+1, UTF8Len)); // force uppercase
      // ToDo: Use the whole UTF-8 character in accelStr. How?
      aShortcut := KeyToShortCut(Ord(accelStr[1]),
      {$if defined(darwin) or defined(macos) or defined(iphonesim)} [ssMeta]
      {$else} [ssAlt] {$endif});
      Result := True;
      Break;
    end;
  until False;
end;
{
function GetAcceleratedItemsCount(aMenu: TMenu): integer;
var
  i: integer;

  procedure RecursiveCountAcceleratedCaptions(aMI: TMenuItem);
  var
    j: integer;
    sc: TShortCut;
  begin
    if HasAccelerator(aMI.Caption, sc) then
      Inc(Result);
    for j:=0 to aMI.Count-1 do
      RecursiveCountAcceleratedCaptions(aMI.Items[j]);
  end;

begin
  Result:=0;
  for i:=0 to aMenu.Items.Count-1 do
    RecursiveCountAcceleratedCaptions(aMenu.Items[i]);
end;
}
procedure DoShortcutAccelScanCount(const aSCList: TSCList; shortcutsOnly: boolean);
var
  dm: TDataModule;
  frm: TCustomForm;
  i, a: integer;
  aLst: TActionList;
  ac: TAction;
  sc: TShortCut;
  container: TComponent;

  procedure AddInfoToScanList(aComp: TComponent; aSC: TShortCut; aKind: TSCKind);
  var
    isAccel: boolean;
  begin
    isAccel:=(aKind in Accelerator_Kinds);
    if isAccel and not shortcutsOnly then
      aSCList.AcceleratorsInContainerCount:=aSCList.AcceleratorsInContainerCount+1
    else
      aSCList.ShortcutsInContainerCount:=aSCList.ShortcutsInContainerCount+1;
    aSCList.ScanList.AddObject(ShortCutToText(aSC), TSCInfo.CreateWithParams(aComp, aKind, aSC));
  end;

  procedure ScanMenu(aMenu: TMenu);
  var
    i: integer;

    procedure RecursiveScanItem(anItem:TMenuItem);
    var
      j: integer;
      sc: TShortCut;
    begin
      if (anItem.ShortCut <> 0) then
        AddInfoToScanList(anItem, anItem.ShortCut, scMenuItemSC);
      if (anItem.ShortCutKey2 <> 0) then
        AddInfoToScanList(anItem, anItem.ShortCutKey2, scMenuItemKey2);
      if not shortcutsOnly and HasAccelerator(anItem.Caption, sc) then
        AddInfoToScanList(anItem, sc, scMenuItemAccel);
      for j:=0 to anItem.Count-1 do
        RecursiveScanItem(anItem.Items[j]);
    end;

  begin
    for i:=0 to aMenu.Items.Count-1 do
      RecursiveScanItem(aMenu.Items[i]);
  end;

begin
  container:=GlobalDesignHook.LookupRoot as TComponent;
  aSCList.ClearAllLists;
  aSCList.AcceleratorsInContainerCount:=0;
  aSCList.ShortcutsInContainerCount:=0;
  if (container is TDataModule) then
  begin
    dm:=TDataModule(container);
    for i:=0 to dm.ComponentCount-1 do
      if (dm.Components[i] is TMenu) then
        ScanMenu(TMenu(dm.Components[i]));
  end
  else if (container is TCustomForm) then
  begin
    frm:=TCustomForm(container);
    for i:=0 to frm.ComponentCount-1 do
      if (frm.Components[i] is TMenu) then
        ScanMenu(TMenu(frm.Components[i]))
      else if (frm.Components[i] is TActionList) then begin
        aLst:=TActionList(frm.Components[i]);
        for a:=0 to aLst.ActionCount-1 do begin
          ac:=TAction(aLst.Actions[a]);
          if (ac.ShortCut > 0) then
            AddInfoToScanList(ac, ac.ShortCut, scActionSC);
          if (ac.SecondaryShortCuts.Count > 0) then
            AddInfoToScanList(ac, ac.SecondaryShortCuts.ShortCuts[0], scActionSecondary);
          if not shortcutsOnly and HasAccelerator(ac.Caption, sc) then
            AddInfoToScanList(ac, sc, scActionAccel);
        end;
      end
      else begin
        if not shortcutsOnly and (frm.Components[i] is TControl)
        and HasAccelerator(TControl(frm.Components[i]).Caption, sc) then
          AddInfoToScanList(frm.Components[i], sc, scOtherCompAccel);
      end;
  end;
  Assert(aSCList.AcceleratorsInContainerCount+aSCList.ShortcutsInContainerCount=
         aSCList.ScanList.Count,'DoShortcutAccelScanCount: internal counting error');
end;

function NewShortcutOrCaptionIsValidDlg(aConflictingInfo: TSCInfo; out
  aNewShortcut: TShortCut; out aNewCaption: string): boolean;
var
  dlg: TEditShortcutCaptionDialog;
  ok: boolean;
  sc: TShortCut;
begin
  dlg:=TEditShortcutCaptionDialog.CreateNew(nil, aConflictingInfo);
  try
    Result:=(dlg.ShowModal = mrOK);
    case (aConflictingInfo.Kind in Accelerator_Kinds) of
      True: begin
        if HasAccelerator(dlg.NewCaption, sc) then
          ok:=(sc <> aConflictingInfo.Shortcut)
        else
          ok:=True;
      end;
      False: ok:=(aConflictingInfo.Shortcut <> dlg.NewShortcut);
    end;
    Result:=Result and ok;
    if Result then
      begin
        aNewShortcut:=dlg.NewShortcut;
        aNewCaption:=dlg.NewCaption;
      end
    else
      begin
        aNewShortcut:=0;
        aNewCaption:='';
      end;
  finally
    FreeAndNil(dlg);
  end;
end;

function KindToPropertyName(aKind: TSCKind): string;
begin
   case aKind of
    scUnknown:   Result:='<unknown property>';
    scActionAccel, scMenuItemAccel, scOtherCompAccel:
                  Result:='Caption';
    scActionSC, scMenuItemSC: Result:='ShortCut';
    scActionSecondary: Result:='SecondaryShortcuts';
    scMenuItemKey2:    Result:='ShortCutKey2';
  end;
end;

function SplitCommaText(const aCommaText: string; out firstBit: string): string;
var
  p: integer;
begin
  if (aCommaText = '') then begin
    firstBit:='';
    Exit('');
  end;
  p:=Pos(',', aCommaText);
  if (p = 0) then begin
    firstBit:=aCommaText;
    Exit('');
  end;
  firstBit:=Copy(aCommaText, 1, Pred(p));
  Result:=Copy(aCommaText, Succ(p), Length(aCommaText)-p);
end;

function SortByShortcut(Item1, Item2: Pointer): Integer;
var
  inf1: TSCInfo absolute Item1;
  inf2: TSCInfo absolute Item2;
begin
  if (inf1.Shortcut > inf2.Shortcut) then
    Result:= +1
  else if (inf1.Shortcut < inf2.Shortcut) then
    Result:= -1
  else
    Result:=0;
end;

function SortFPListByComponentPropertyName(Item1, Item2: Pointer): Integer;
var
  inf1: TSCInfo absolute Item1;
  inf2: TSCInfo absolute Item2;
begin
  if (inf1.ComponentName > inf2.ComponentName) then
    Result:= +1
  else if (inf1.ComponentName < inf2.ComponentName) then
    Result:= -1
  else
    Result:=0;
end;

function SortByComponentPropertyName(List: TStringList; Index1, Index2: Integer): Integer;
var
  name1: string;
  name2: string;
begin
  name1:=TSCInfo(List.Objects[Index1]).ComponentName;
  name2:=TSCInfo(List.Objects[Index2]).ComponentName;
  if (name1 > name2) then
    Result:= +1
  else if (name2 > name1) then
    Result:= -1
  else
    Result:=0;
end;

function SortOnComponentPropertyName(List: TStringList; Index1, Index2: Integer): Integer;
var
  s1, s2: string;
begin
  s1:=TSCInfo(List.Objects[Index1]).ToCompositeString;
  s2:=TSCInfo(List.Objects[Index2]).ToCompositeString;
  Result:=AnsiCompareText(s1, s2);
end;


{ TSCInfo }

constructor TSCInfo.CreateWithParams(aComponent: TComponent; aKind: TSCKind;
  aSC: TShortCut);
begin
  FComponent:=aComponent;
  FComponentName:=aComponent.Name;
  FKind:=aKind;
  FShortcut:=aSC;
end;

function TSCInfo.GetAction: TAction;
begin
  if (FComponent is TAction) then
    Result:=TAction(FComponent)
  else
    Result:=nil;
end;

function TSCInfo.GetCaption: string;
begin
  if (FComponent is TControl) then
    Result:=TControl(FComponent).Caption
  else
    Result:=lisMenuEditorComponentIsUnexpectedKind;
end;

function TSCInfo.GetMenuItem: TMenuItem;
begin
  if (FComponent is TMenuItem) then
    Result:=TMenuItem(FComponent)
  else
    Result:=nil;
end;

function TSCInfo.GetToCompositeString: string;
begin
  Result:=FComponent.Name + ShortCutToText(FShortcut);
end;

{ TSCList }

constructor TSCList.Create;
begin
  FScanList:=TStringList.Create;
  FUniqueList:=TFPList.Create;
  FInitialDuplicates:=TFPList.Create;
  ScanContainerForShortcutsAndAccelerators;
end;

destructor TSCList.Destroy;
begin
  ClearAllLists;
  FreeAndNil(FUniqueList);
  FreeAndNil(FInitialDuplicates);
  FreeAndNil(FScanList);
  inherited Destroy;
end;

function TSCList.GetScanListCompName(index: integer): string;
var
  inf: TSCInfo;
begin
  if (index > -1) and (index < FScanList.Count) then begin
    inf:=TSCInfo(FScanList.Objects[index]);
    if (inf.ComponentName <> '') then
      Result:=inf.ComponentName
    else
      Result:=lisMenuEditorComponentIsUnnamed;
  end
  else
    Result:=Format(lisMenuEditorTSCListGetScanListCompNameInvalidIndexDForFScanLis,
                   [index]);
end;

function TSCList.GetInitialDuplicatesCount: integer;
begin
  Result:=FInitialDuplicates.Count;
end;

function TSCList.GetUniqueCount: integer;
begin
  Result:=FUniqueList.Count;
end;

procedure TSCList.ClearAllLists;
var
  i: integer;
begin
  for i:=0 to FScanList.Count-1 do
    TSCInfo(FScanList.Objects[i]).Free;
  FScanList.Clear;
  FUniqueList.Clear;
  FInitialDuplicates.Clear;
end;

function TSCList.UniqueListContainsShortcut(aSC: TShortCut): boolean;
var
  p: pointer;
  inf: TSCInfo absolute p;
begin
  for p in FUniqueList do
    if (inf.Shortcut = aSC) then
      Exit(True);
  Result:=False;
end;

function TSCList.FindUniqueInfoForShortcut(aSC: TShortCut): TSCInfo;
var
  p: pointer;
  inf: TSCInfo absolute p;
begin
  for p in FUniqueList do
    if (inf.Shortcut = aSC) then
      Exit(inf);
  Result:=nil;
end;

procedure TSCList.ScanContainerForShortcutsAndAccelerators;
begin
  DoShortcutAccelScanCount(Self, False);
  ScanSCListForDuplicates;
  if (FInitialDuplicates.Count > 0) then
    FInitialDuplicates.Sort(@SortByShortcut);
  if (FUniqueList.Count > 0) then
    FUniqueList.Sort(@SortByShortcut);
end;

procedure TSCList.ScanContainerForShortcutsOnly;
begin
  DoShortcutAccelScanCount(Self, True);
end;

procedure TSCList.ScanSCListForDuplicates;
var
  i: integer;
  inf2, inf1: TSCInfo;
begin
  FreeAndNil(FUniqueList);
  FreeAndNil(FInitialDuplicates);
  FUniqueList:=TFPList.Create;
  FInitialDuplicates:=TFPList.Create;
  for i:=0 to FScanList.Count-1 do
    if UniqueListContainsShortcut(TSCInfo(FScanList.Objects[i]).Shortcut) then
      FInitialDuplicates.Add(FScanList.Objects[i])
    else
      FUniqueList.Add(FScanList.Objects[i]);
  if (FInitialDuplicates.Count > 0) then begin
    FInitialDuplicates.Sort(@SortFPListByComponentPropertyName);
    for i:=FInitialDuplicates.Count-1 downto 1 do begin
      inf2:=TSCInfo(FInitialDuplicates[i]);
      inf1:=TSCInfo(FInitialDuplicates[i-1]);
      if (CompareText(inf2.ComponentName, inf1.ComponentName) = 0)
      and (inf2.Shortcut = inf1.Shortcut) then
        FInitialDuplicates.Delete(i);
    end;
  end;
end;

procedure TSCList.SortByComponentPropertyName;
begin
  FScanList.CustomSort(@SortOnComponentPropertyName);
end;

{ TAddShortcutDialog }

constructor TAddShortcutDialog.CreateWithMenuItem(AOwner: TComponent;
  aMI: TMenuItem; isMainSC: boolean; aSC: TShortCut);
var
  editing: boolean;
  key: word;
  shift: TShiftState;
  i: integer;
begin
  inherited CreateNew(AOwner);
  FMenuItem:=aMI;
  FOldShortcut:=aSC;
  editing:=(aSC <> 0);
  Position:=poScreenCenter;
  BorderStyle:=bsDialog;
  case editing of
    False: if isMainSC then
             Caption:=Format(lisMenuEditorEnterANewShortCutForS, [FMenuItem.Name])
           else
             Caption:=Format(lisMenuEditorEnterANewShortCutKey2ForS, [FMenuItem.Name]);
    True : if isMainSC then
             Caption:=Format(lisMenuEditorChangeTheShortCutForS, [FMenuItem.Name])
           else
             Caption:=Format(lisMenuEditorChangeTheShortCutKey2ForS, [FMenuItem.Name]);
  end;
  FButtonPanel:=TButtonPanel.Create(Self);
  FButtonPanel.ShowButtons:=[pbOK, pbCancel];
  FButtonPanel.OKButton.Name:='OKButton';
  FButtonPanel.OKButton.DefaultCaption:=True;
  FButtonPanel.OKButton.OnClick:=@OKButtonClick;
  FButtonPanel.CancelButton.Name:='CancelButton';
  FButtonPanel.CancelButton.DefaultCaption:=True;
  FButtonPanel.Parent:=Self;
  FShortCutGrabBox:=TShortCutGrabBox.Create(Self);
  FShortCutGrabBox.BorderSpacing.Around:=Margin;
  FShortCutGrabBox.GrabButton.Caption:='&Grab key';
  // this rather restricted list covers most of the common values needed
  // #todo - extend list?
  with FShortCutGrabBox.KeyComboBox.Items do
  begin
    Clear;
    BeginUpdate;
    Add(lisMenuEditorNone);
    for i:=1 to High(ShortCutKeys) do
      Add(ShortCutToText(ShortCutKeys[i]));
    EndUpdate;
  end;
  {$if defined(darwin) or defined(macos) or defined(iphonesim)}
    FShortCutGrabBox.AllowedShifts:=[ssShift, ssCtrl, ssMeta]
  {$else} FShortCutGrabBox.AllowedShifts:=[ssShift, ssCtrl, ssAlt] {$endif};
  FShortCutGrabBox.KeyComboBox.OnCloseUp:=@OnGrabBoxCloseUp;
  FShortCutGrabBox.Align:=alClient;
  FShortCutGrabBox.MainOkButton:=FButtonPanel.OKButton;
  if editing then begin
    ShortCutToKey(FOldShortcut, key, shift);
    FShortCutGrabBox.ShiftState:=shift;
    FShortCutGrabBox.Key:=key;
  end;
  FShortCutGrabBox.Parent:=Self;
  AutoSize:=True;
end;

procedure TAddShortcutDialog.OKButtonClick(Sender: TObject);
begin
  if (FShortCutGrabBox.Key <> VK_UNKNOWN) then
    FNewShortcut:=KeyToShortCut(FShortCutGrabBox.Key, FShortCutGrabBox.ShiftState)
  else
    FNewShortcut:=0;
end;

procedure TAddShortcutDialog.OnGrabBoxCloseUp(Sender: TObject);
begin
  if (FShortCutGrabBox.KeyComboBox.ItemIndex = 0) then
    FShortCutGrabBox.ShiftState:=[];
end;

{ TEditShortcutCaptionDialog }

constructor TEditShortcutCaptionDialog.CreateNew(aShortcuts: TMenuShortcuts;
  aSCInfo: TSCInfo);
var
  s: string;
  sse: TShiftStateEnum;
  i: integer;
begin
  FShortcuts:=aShortcuts;
  FInfo:=aSCInfo;
  Assert(aSCInfo<>nil,'TEditShortcutCaptionDialog.CreateNew: aSCInfo is nil');
  Assert(aSCInfo.Kind<>scUnknown,'TEditShortcutCaptionDialog.CreateNew: aSCInfo is unknown type');
  Assert(FShortcuts.ShortcutList.UniqueCount>0,'TEditShortcutCaptionDialog.CreateNew: unique list is empty');
  inherited CreateNew(Nil);
  FEditingCaption:=(FInfo.Kind in Accelerator_Kinds);
  Position:=poScreenCenter;
  BorderStyle:=bsDialog;
  Constraints.MinWidth:=300;

  FGroupBox:=TGroupBox.Create(Self);
  if FEditingCaption then
    begin
      Caption:=Format(lisMenuEditorChangeConflictingAcceleratorS,
                      [ShortCutToText(FInfo.Shortcut)]);
      if (FInfo.Kind = scMenuItemAccel) then
        FOldCaption:=FInfo.MenuItem.Caption;
      FEdit:=TEdit.Create(Self);
      with FEdit do
      begin
        Align:=alClient;
        BorderSpacing.Around:=Margin;
        AutoSize:=True;
        Text:=FOldCaption;
        OnChange:=@CaptionEditChange;
        Parent:=FGroupBox;
      end;
      s:=lisMenuEditorCaption;
    end
  else
    begin
      Caption:=Format(lisMenuEditorChangeShortcutConflictS,
                      [ShortCutToText(FInfo.Shortcut)]);
      s:=KindToPropertyName(FInfo.Kind);
      // don't set values to old shortcut since they need to be changed anyhow
      FGrabBox:=TCustomShortCutGrabBox.Create(Self);
      with FGrabBox do
      begin
        Align:=alClient;
        BorderSpacing.Around:=Margin;
        AutoSize:=True;
        GrabButton.Caption:=lisMenuEditorGrabKey;
       // this rather restricted list covers most of the common values needed
        with KeyComboBox.Items do
        begin
          Clear;
          BeginUpdate;
          for i:=Low(ShortCutKeys) to High(ShortCutKeys) do
            Add(ShortCutToText(ShortCutKeys[i]));
          EndUpdate;
        end;
        GrabButton.OnEnter:=@GrabBoxEnter; // we can't alter any grabBox OnClick event
        KeyComboBox.OnEnter:=@GrabBoxEnter;
        for sse in ShiftButtons do
          ShiftCheckBox[sse].OnEnter:=@GrabBoxEnter;
        OnExit:=@GrabBoxExit;
        FGrabBox.Caption:=Format(lisMenuEditorChangeShortcutCaptionForComponent,
                                 [s, FInfo.Component.Name]);
        Parent:=FGroupBox;
      end;
    end;
  FGroupBox.Caption:=Format(lisMenuEditorEditingSForS,[s, FInfo.Component.Name]);
  FGroupBox.Align:=alTop;
  FGroupBox.BorderSpacing.Around:=Margin;
  FGroupBox.AutoSize:=True;
  FGroupBox.Parent:=Self;

  FButtonPanel:=TButtonPanel.Create(Self);
  with FButtonPanel do
  begin
    ShowButtons:=[pbOK, pbCancel];
    Top:=1;
    Align:=alTop;
    OKButton.OnClick:=@OKButtonOnClick;
    OKButton.ModalResult:=mrNone;
    OKButton.Enabled:=False;
    ShowBevel:=False;
    Parent:=Self;
  end;
  AutoSize:=True;
end;

procedure TEditShortcutCaptionDialog.CaptionEditChange(Sender: TObject);
var
  newSC: TShortCut;
  hasAccel: boolean;
  ed: TEdit absolute Sender;
  inf: TSCInfo;
begin
  if not (Sender is TEdit) then
    Exit;
  if HasAccelerator(ed.Text, newSC) then
    begin
      if FShortcuts.ShortcutList.UniqueListContainsShortcut(newSC) then
        begin
          inf:=FShortcuts.ShortcutList.FindUniqueInfoForShortcut(newSC);
          IDEMessageDialogAb(lisMenuEditorFurtherShortcutConflict,
                     Format(lisMenuEditorSIsAlreadyInUse,
                     [ShortCutToText(newSC), inf.Component.Name]),
                     mtWarning, [mbOK], False);
          FEdit.Text:=AmpersandStripped(FOldCaption);
          FEdit.SetFocus;
        end
      else
        begin
          FNewShortcut:=newSC;
          FNewCaption:=ed.Text;
        end;
    end
  else
    begin
      FNewShortcut:=0;
      FNewCaption:=ed.Text;
    end;
  hasAccel:=HasAccelerator(FEdit.Text, newSC);
  FButtonPanel.OKButton.Enabled:=not hasAccel or (hasAccel and (newSC <> FInfo.Shortcut));
end;

procedure TEditShortcutCaptionDialog.GrabBoxEnter(Sender: TObject);
begin
  if not FButtonPanel.OKButton.Enabled then
    FButtonPanel.OKButton.Enabled:=True;
end;

procedure TEditShortcutCaptionDialog.GrabBoxExit(Sender: TObject);
var
  newSC: TShortCut;
  inf: TSCInfo;
begin
  newSC:=KeyToShortCut(FGrabBox.Key, FGrabBox.ShiftState);
  if (FInfo.Shortcut = newSC) then
    begin
      IDEMessageDialogAb(lisMenuEditorShortcutNotYetChanged,
           Format(lisMenuEditorYouHaveToChangeTheShortcutFromSStoAvoidAConflict,
                  [ShortCutToText(FInfo.Shortcut)]),
                  mtWarning, [mbOK], False);
      FGrabBox.KeyComboBox.SetFocus;
      Exit;
    end;
  if FShortcuts.ShortcutList.UniqueListContainsShortcut(newSC) then
    begin
      inf:=FShortcuts.ShortcutList.FindUniqueInfoForShortcut(newSC);
      IDEMessageDialogAb(lisMenuEditorFurtherShortcutConflict,
           Format(lisMenuEditorSIsAlreadyInUse,
                  [ShortCutToText(newSC), inf.Component.Name]),
                  mtWarning, [mbOK], False);
      FGrabBox.KeyComboBox.SetFocus;
    end
  else
    begin
      FNewShortcut:=newSC;
      FButtonPanel.OKButton.Enabled:=True;
    end;
end;

procedure TEditShortcutCaptionDialog.OKButtonOnClick(Sender: TObject);
begin
  if FEditingCaption then
  begin
    if (FEdit.Text = '') then
    begin
      IDEMessageDialogAb(lisMenuEditorCaptionShouldNotBeBlank,
                 lisMenuEditorYouMustEnterTextForTheCaption,
                 mtWarning, [mbOK], False);
      FEdit.Text:=AmpersandStripped(FOldCaption);
      FEdit.SetFocus;
    end
    else
      ModalResult:=mrOK;
  end
  else
    ModalResult:=mrOK;
end;

procedure TEditShortcutCaptionDialog.Activate;
begin
  inherited Activate;
  FButtonPanel.OKButton.Enabled:=False;
end;

{ TContents }

constructor TContents.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDualDisplay:=AOwner as TDualDisplay;
  FSList:=TStringList.Create;
  Color:=clBtnFace;
end;

destructor TContents.Destroy;
begin
  FreeAndNil(FSList);
  inherited Destroy;
end;

procedure TContents.Clear;
begin
  FSList.Clear;
  Height:=0;
end;

procedure TContents.DoContentsClick(anIndex: integer);
begin
  if Assigned(FOnContentsClick) and (anIndex < FSList.Count) then
    FOnContentsClick(Self, anIndex);
end;

procedure TContents.Paint;
var
  s, s1, s2: string;
  i: integer = 0;
  col1, col2: integer;
  dt: TDisplayType;
begin
  if FDualDisplay.Updating then
    Exit;
  Canvas.FillRect(ClientRect);
  col2:=FDualDisplay.Col1Right + Leading;
  for s in FSList do begin
    s2:=SplitCommaText(s, s1);
    col1:=FDualDisplay.Col1Right - Leading - Canvas.TextWidth(s1);
    dt:=TDisplayType(PtrUInt(FSList.Objects[i]));
    case dt of
      dtNone: begin s1:=''; s2:=''; end;
      dtBlack: begin
        if (Canvas.Font.Color <> clBlack) then Canvas.Font.Color:=clBlack;
        if (Canvas.Font.Style <> []) then Canvas.Font.Style:=[];
      end;
      dtBlackBold: begin
        if (Canvas.Font.Color <> clBlack) then Canvas.Font.Color:=clBlack;
        if (Canvas.Font.Style <> [fsBold]) then Canvas.Font.Style:=[fsBold];
      end;
      dtGreyed: begin
        if (Canvas.Font.Color <> clGrayText) then Canvas.Font.Color:=clGrayText;
        if (Canvas.Font.Style <> []) then Canvas.Font.Style:=[];
      end;
      dtGreyedBold: begin
        if (Canvas.Font.Color <> clGrayText) then Canvas.Font.Color:=clGrayText;
        if (Canvas.Font.Style <> [fsBold]) then Canvas.Font.Style:=[fsBold];
      end;
    end;
    Canvas.TextOut(col1, i*VDim + VTextOffset, s1);
    Canvas.TextOut(col2, i*VDim + VTextOffset, s2);
    Inc(i);
  end;
end;

procedure TContents.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited MouseDown(Button, Shift, X, Y);
  DoContentsClick(Y div VDim);
end;

procedure TContents.AddToList(const aLine: string; aDisplayType: TDisplayType);
var
  h, w, cw, ch: integer;
  second, first: string;
begin
  Assert(Parent<>nil,'TContents.AddToList: Parent is nil');
  Assert(aDisplayType<>dtNone,'TContents.AddToList: TDisplayType=dtNone');
  FSList.AddObject(aLine, TObject(PtrUInt(aDisplayType)));
  second:=SplitCommaText(aLine, first);
  w:=FDualDisplay.TextWidth(second);
  if (w > FCol2MaxTextWidth) then
    FCol2MaxTextWidth:=w;
  w:=FDualDisplay.TextWidth(first);
  if (w > FCol1MaxTextWidth) then
    FCol1MaxTextWidth:=w;
  w:=FCol1MaxTextWidth + FCol2MaxTextWidth + Treble_Leading;
  if (w < Parent.Width) then
    w:=Parent.Width;
  h:=FSList.Count*VDim;
  ch:=ClientHeight;
  cw:=ClientWidth;
  if (h > ch) or (w > cw) then
    SetBounds(0, 0, w, h);
end;

{ THeader }

procedure THeader.DoHeaderClick(anIndex: integer);
begin
  if Assigned(FOnHeaderClick) then
    FOnHeaderClick(Self, anIndex);
end;

procedure THeader.Paint;
begin
  Canvas.Brush.Color:=Header_Color;
  Canvas.FillRect(ClientRect);
  case FDisplayType of
    dtNone: begin FCol1Header:=''; FCol2Header:=''; end;
    dtBlack: begin
      if (Canvas.Font.Color <> clBlack) then Canvas.Font.Color:=clBlack;
      if (Canvas.Font.Style <> []) then Canvas.Font.Style:=[];
    end;
    dtBlackBold: begin
      if (Canvas.Font.Color <> clBlack) then Canvas.Font.Color:=clBlack;
      if (Canvas.Font.Style <> [fsBold]) then Canvas.Font.Style:=[fsBold];
    end;
    dtGreyed: begin
      if (Canvas.Font.Color <> clGrayText) then Canvas.Font.Color:=clGrayText;
      if (Canvas.Font.Style <> []) then Canvas.Font.Style:=[];
    end;
    dtGreyedBold: begin
      if (Canvas.Font.Color <> clGrayText) then Canvas.Font.Color:=clGrayText;
      if (Canvas.Font.Style <> [fsBold]) then Canvas.Font.Style:=[fsBold];
    end;
  end;
  Canvas.TextOut(FDualDisplay.Col1Right - Leading - FColumn1TextWidth, VTextOffset, FCol1Header);
  Canvas.TextOut(FDualDisplay.Col1Right + Leading, VTextOffset, FCol2Header);
end;

procedure THeader.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  i: integer=0;
begin
  inherited MouseDown(Button, Shift, X, Y);
  if (X > FDualDisplay.Col1Right) then
    i:=1;
  DoHeaderClick(i);
end;

constructor THeader.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDualDisplay:=AOwner as TDualDisplay;
  Align:=alTop;
  Height:=VDim;
  Canvas.Font.Style:=[fsBold];
end;

procedure THeader.AddHeader(const aHeader: string; aDisplayType: TDisplayType);
begin
  FCol2Header:=SplitCommaText(aHeader, FCol1Header);
  FDisplayType:=aDisplayType;
  FColumn1TextWidth:=FDualDisplay.TextWidth(FCol1Header);
  Repaint;
end;

procedure THeader.Clear;
begin
  FColumn1TextWidth:=0;
  FDisplayType:=dtNone;
  Invalidate;
end;

{ TDualDisplay }

constructor TDualDisplay.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Name:='DualDisplay';
  Color:=clBtnFace;
  Canvas.Font.Style:=[fsBold];
  with GetControlClassDefaultSize do
    SetInitialBounds(0, 0, cx, cy);

  FHeader:=THeader.Create(Self);
  with FHeader do begin
    Name:='Header';
    OnHeaderClick:=@HeaderContentsClick;
    Parent:=Self;
  end;

  FSBox:=TScrollBox.Create(Self);
  with FSBox do begin
    Align:=alClient;
    BorderStyle:=bsNone;
    AutoScroll:=True;
    Parent:=Self;
  end;

  FContents:=TContents.Create(Self);
  with FContents do begin
    Name:='Contents';
    SetInitialBounds(0, 0, FSBox.Width, FSBox.Height);
    OnContentsClick:=@HeaderContentsClick;
    Color:=clBtnFace;
    Parent:=FSBox;
  end;
end;

function TDualDisplay.GetContentsCount: integer;
begin
  Result:=FContents.SList.Count;
end;

procedure TDualDisplay.HeaderContentsClick(Sender: TObject; index: integer);
begin
  if Assigned(FOnDisplayClick) then begin
    Assert(Sender<>nil,'TDualDisplay.HeaderContentsClick: Sender is nil');
    Assert(index>-1,'TDualDisplay.HeaderContentsClick: index is negative');
    if (Sender is TContents) then begin
      Assert(index<GetContentsCount,'TDualDisplay.HeaderContentsClick: index exceeds contents count');
      FOnDisplayClick(False, index);
    end
    else if (Sender is THeader) then begin
      Assert(index<2,'TDualDisplay.HeaderContentsClick: index value too high');
      FOnDisplayClick(True, index);
    end
    else Assert(True,'TDualDisplay.HeaderContentsClick: Sender is invalid type');
  end;
end;

procedure TDualDisplay.SetCol1Right(AValue: integer);
begin
  if (FCol1Right <> AValue) then begin
    FCol1Right:=AValue;
    FHeader.Invalidate;
    FContents.Invalidate;
  end;
end;

class function TDualDisplay.GetControlClassDefaultSize: TSize;
begin
  Result.cx:=200;
  Result.cy:=120;
end;

function TDualDisplay.TextWidth(const aText: string): integer;
begin
  Result:=Canvas.TextWidth(aText);
end;

procedure TDualDisplay.AddHeader(const aHeader: string; aDT: TDisplayType);
var
  tmp: integer;
begin
  FHeader.AddHeader(aHeader, aDT);
  tmp:=FCol1Right - Double_Leading;
  if (FHeader.Column1TextWidth > tmp) then
    SetCol1Right(FHeader.Column1TextWidth + Double_Leading);
  tmp:=TextWidth(aHeader) + Treble_Leading;
  if (tmp > Width) then begin
    Width:=tmp;
    FHeader.Width:=tmp;
    FContents.Width:=tmp;
  end;
  FHeader.Repaint;
end;

procedure TDualDisplay.AddLine(const aLine: string; aDT: TDisplayType);
var
  tmp: integer;
begin
  FContents.AddToList(aLine, aDT);
  tmp:=FCol1Right - Double_Leading;
  if (FContents.Col1MaxTextWidth > tmp) then
    SetCol1Right(FContents.Col1MaxTextWidth + Double_Leading);
  tmp:=FContents.Width;
  if (tmp > ClientWidth) then begin
    Width:=tmp;
    FHeader.Width:=tmp;
  end;
end;

procedure TDualDisplay.BeginUpdate;
begin
  FUpdating:=True;
end;

procedure TDualDisplay.EndUpdate;
begin
  FUpdating:=False;
end;

procedure TDualDisplay.ClearHeader;
begin
  FHeader.Clear;
end;

procedure TDualDisplay.Clear;
begin
  FHeader.Clear;
  FContents.Clear;
end;

procedure TDualDisplay.ClearContents;
begin
  FContents.Clear;
end;

procedure TDualDisplay.InvalidateContents;
begin
  FContents.Invalidate;
end;

{ TMenuShortcuts }

constructor TMenuShortcuts.Create;
begin
  FShortcutList:=TSCList.Create;
end;

destructor TMenuShortcuts.Destroy;
begin
  FShortcutList.Free;
  inherited Destroy;
end;

procedure TMenuShortcuts.Initialize;
begin
  FShortcutList.ClearAllLists;
  FShortcutList.ScanContainerForShortcutsAndAccelerators;
  FShortcutConflictsCount:=FShortcutList.InitialDuplicatesCount;
end;

procedure TMenuShortcuts.UpdateShortcutList(includeAccelerators: boolean);
begin
  if includeAccelerators then
    FShortcutList.ScanContainerForShortcutsAndAccelerators
  else
    FShortcutList.ScanContainerForShortcutsOnly;
end;

procedure TMenuShortcuts.ResetMenuItemsCount;
begin
  FShortcutMenuItemsCount := -1;
end;

function TMenuShortcuts.Statistics(aShortcutCount: integer): string;
begin
  if (FShortcutMenuItemsCount <> aShortcutCount) then
  begin
    FShortcutMenuItemsCount := aShortcutCount;
    Result := Format(lisMenuEditorShortcutItemsS, [IntToStr(FShortcutMenuItemsCount)]);
  end
  else
    Result := '';
end;

end.

