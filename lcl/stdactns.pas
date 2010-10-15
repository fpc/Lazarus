{
 /***************************************************************************
                                StdActns.pas
                                ------------


 ***************************************************************************/

 *****************************************************************************
 *                                                                           *
 *  This file is part of the Lazarus Component Library (LCL)                 *
 *                                                                           *
 *  See the file COPYING.modifiedLGPL.txt, included in this distribution,    *
 *  for details about the copyright.                                         *
 *                                                                           *
 *  This program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
 *                                                                           *
 *****************************************************************************

}
unit StdActns;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ActnList, Forms, Dialogs, StdCtrls, Clipbrd;

type

  { Hint actions }

  THintAction = class(TCustomHintAction)
  end;
  
  { Edit actions }

  TEditAction = class(TAction)
  private
    FControl: TCustomEdit;
    procedure SetControl(const AValue: TCustomEdit);
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    destructor Destroy; override;
    function HandlesTarget(Target: TObject): Boolean; override;
    // limits target to the specific control
    property Control: TCustomEdit read FControl write SetControl;
  end;

  { TEditCut }

  TEditCut = class(TEditAction)
  public
    procedure ExecuteTarget(Target: TObject); override;
    procedure UpdateTarget(Target: TObject); override;
  end;

  { TEditCopy }

  TEditCopy = class(TEditAction)
  public
    procedure ExecuteTarget(Target: TObject); override;
    procedure UpdateTarget(Target: TObject); override;
  end;

  TEditPaste = class(TEditAction)
  public
    procedure UpdateTarget(Target: TObject); override;
    procedure ExecuteTarget(Target: TObject); override;
  end;

  TEditSelectAll = class(TEditAction)
  public
    procedure ExecuteTarget(Target: TObject); override;
    procedure UpdateTarget(Target: TObject); override;
  end;

  TEditUndo = class(TEditAction)
  public
    procedure ExecuteTarget(Target: TObject); override;
    procedure UpdateTarget(Target: TObject); override;
  end;

  TEditDelete = class(TEditAction)
  public
    procedure ExecuteTarget(Target: TObject); override;
    procedure UpdateTarget(Target: TObject); override;
  end;


  { Help actions }

  THelpAction = class(TAction)
  public
    constructor Create(TheOwner: TComponent); override;
    function HandlesTarget(Target: TObject): Boolean; override;
    procedure UpdateTarget(Target: TObject); override;
  end;

  THelpContents = class(THelpAction)
  public
    procedure ExecuteTarget(Target: TObject); override;
  end;

  THelpTopicSearch = class(THelpAction)
  public
    procedure ExecuteTarget(Target: TObject); override;
  end;

  THelpOnHelp = class(THelpAction)
  public
    procedure ExecuteTarget(Target: TObject); override;
  end;

  THelpContextAction = class(THelpAction)
  public
    procedure ExecuteTarget(Target: TObject); override;
    procedure UpdateTarget(Target: TObject); override;
  end;


  { TCommonDialogAction }

  TCommonDialogClass = class of TCommonDialog;

  TCommonDialogAction = class(TCustomAction)
  private
    FBeforeExecute: TNotifyEvent;
    FExecuteResult: Boolean;
    FOnAccept: TNotifyEvent;
    FOnCancel: TNotifyEvent;
  protected
    FDialog: TCommonDialog;
    procedure DoAccept;
    procedure DoBeforeExecute;
    procedure DoCancel;
    function GetDialogClass: TCommonDialogClass; virtual;
    procedure CreateDialog; virtual;
  public
    constructor Create(TheOwner: TComponent); override;
    function Handlestarget(Target: TObject): Boolean; override;
    procedure ExecuteTarget(Target: TObject); override;
    property ExecuteResult: Boolean read FExecuteResult;
    property BeforeExecute: TNotifyEvent read FBeforeExecute write FBeforeExecute;
    property OnAccept: TNotifyEvent read FOnAccept write FOnAccept;
    property OnCancel: TNotifyEvent read FOnCancel write FOnCancel;
  end;

  { File Actions }

  TFileAction = class(TCommonDialogAction)
  private
    function GetFileName: TFileName;
    procedure SetFileName(const AValue: TFileName);
  protected
    function GetDialog: TOpenDialog;
    property FileName: TFileName read GetFileName write SetFileName;
  end;

  TFileOpen = class(TFileAction)
  private
    FUseDefaultApp: Boolean;
    function GetDialog: TOpenDialog;
  protected
    function GetDialogClass: TCommonDialogClass; override;
  published
    property Caption;
    property Dialog: TOpenDialog read GetDialog;
    property Enabled;
    property HelpContext;
    property HelpKeyword;
    property HelpType;
    property Hint;
    property ImageIndex;
    property ShortCut;
    property SecondaryShortCuts;
    property UseDefaultApp: Boolean read FUseDefaultApp write FUseDefaultApp
                                                                  default False;
    property Visible;
    property BeforeExecute;
    property OnAccept;
    property OnCancel;
    property OnHint;
  end;

  TFileOpenWith = class(TFileOpen)
  private
    FAfterOpen: TNotifyEvent;
    FFileName: TFileName;
  published
    property FileName: TFileName read FFileName write FFileName;
    property AfterOpen: TNotifyEvent read FAfterOpen write FAfterOpen;
  end;

  TFileSaveAs = class(TFileAction)
  private
    function GetSaveDialog: TSaveDialog;
  protected
    function GetDialogClass: TCommonDialogClass; override;
  published
    property Caption;
    property Dialog: TSaveDialog read GetSaveDialog;
    property Enabled;
    property HelpContext;
    property Hint;
    property ImageIndex;
    property ShortCut;
    property SecondaryShortCuts;
    property Visible;
    property BeforeExecute;
    property OnAccept;
    property OnCancel;
    property OnHint;
  end;

  {TFilePrintSetup = class(TCommonDialogAction)
  private
    function GetDialog: TPrinterSetupDialog;
  protected
    function GetDialogClass: TCommonDialogClass; override;
  published
    property Caption;
    property Dialog: TPrinterSetupDialog read GetDialog;
    property Enabled;
    property HelpContext;
    property HelpKeyword;
    property HelpType;
    property Hint;
    property ImageIndex;
    property ShortCut;
    property SecondaryShortCuts;
    property Visible;
    property BeforeExecute;
    property OnAccept;
    property OnCancel;
    property OnHint;
  end;

  TFilePageSetup = class(TCommonDialogAction)
  private
    function GetDialog: TPageSetupDialog;
  protected
    function GetDialogClass: TCommonDialogClass; override;
  published
    property Caption;
    property Dialog: TPageSetupDialog read GetDialog;
    property Enabled;
    property HelpContext;
    property HelpKeyword;
    property HelpType;
    property Hint;
    property ImageIndex;
    property ShortCut;
    property SecondaryShortCuts;
    property Visible;
    property BeforeExecute;
    property OnAccept;
    property OnCancel;
    property OnHint;
  end;}

  TFileExit = class(TCustomAction)
  public
    function HandlesTarget(Target: TObject): Boolean; override;
    procedure ExecuteTarget(Target: TObject); override;
  published
    property Caption;
    property Enabled;
    property HelpContext;
    property HelpKeyword;
    property HelpType;
    property Hint;
    property ImageIndex;
    property ShortCut;
    property SecondaryShortCuts;
    property Visible;
    property OnHint;
  end;

  { Search Actions }

  { TSearchAction }

  TSearchAction = class(TCommonDialogAction)
  protected
    FControl: TCustomEdit;
    procedure CreateDialog; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure UpdateControl(NewControl: TCustomEdit);
    function PerformSearch: Boolean;
    procedure ShowNotFound; virtual;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    function HandlesTarget(Target: TObject): Boolean; override;
    procedure Search(Sender: TObject); virtual;
    procedure UpdateTarget(Target: TObject); override;
    procedure ExecuteTarget(Target: TObject); override;
  end;

  { TSearchFind }

  TSearchFind = class(TSearchAction)
  private
    function GetFindDialog: TFindDialog;
  protected
    function GetDialogClass: TCommonDialogClass; override;
  published
    property Caption;
    property Dialog: TFindDialog read GetFindDialog;
    property Enabled;
    property HelpContext;
    property HelpKeyword;
    property HelpType;
    property Hint;
    property ImageIndex;
    property ShortCut;
    property SecondaryShortCuts;
    property Visible;
    property BeforeExecute;
    property OnAccept;
    property OnCancel;
    property OnHint;
  end;

  { TSearchReplace }

  TSearchReplace = class(TSearchAction)
  private
    function GetReplaceDialog: TReplaceDialog;
  protected
    function GetDialogClass: TCommonDialogClass; override;
    procedure CreateDialog; override;
  public
    procedure Replace(Sender: TObject); virtual;
  published
    property Caption;
    property Dialog: TReplaceDialog read GetReplaceDialog;
    property Enabled;
    property HelpContext;
    property HelpKeyword;
    property HelpType;
    property Hint;
    property ImageIndex;
    property ShortCut;
    property SecondaryShortCuts;
    property Visible;
    property BeforeExecute;
    property OnAccept;
    property OnCancel;
    property OnHint;
  end;

  { TSearchFindFirst }

  TSearchFindFirst = class(TSearchFind)
  end;

  { TSearchFindNext }

  TSearchFindNext = class(TCustomAction)
  private
    FSearchFind: TSearchFind;
    procedure SetSearchFind(const AValue: TSearchFind);
  public
    constructor Create(TheOwner: TComponent); override;
    function HandlesTarget(Target: TObject): Boolean; override;
    procedure UpdateTarget(Target: TObject); override;
    procedure ExecuteTarget(Target: TObject); override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  published
    property Caption;
    property Enabled;
    property HelpContext;
    property HelpKeyword;
    property HelpType;
    property Hint;
    property ImageIndex;
    property SearchFind: TSearchFind read FSearchFind write SetSearchFind;
    property ShortCut;
    property SecondaryShortCuts;
    property Visible;
    property OnHint;
  end;

  { TFontEdit }

  TFontEdit = class(TCommonDialogAction)
  private
    function GetDialog: TFontDialog;
  protected
    function GetDialogClass: TCommonDialogClass; override;
  published
    property Caption;
    property Dialog: TFontDialog read GetDialog;
    property Enabled;
    property HelpContext;
    property HelpKeyword;
    property HelpType;
    property Hint;
    property ImageIndex;
    property ShortCut;
    property SecondaryShortCuts;
    property Visible;
    property BeforeExecute;
    property OnAccept;
    property OnCancel;
    property OnHint;
  end;

  { TColorSelect }

  TColorSelect = class(TCommonDialogAction)
  private
    function GetDialog: TColorDialog;
  protected
    function GetDialogClass: TCommonDialogClass; override;
  published
    property Caption;
    property Dialog: TColorDialog read GetDialog;
    property Enabled;
    property HelpContext;
    property HelpKeyword;
    property HelpType;
    property Hint;
    property ImageIndex;
    property ShortCut;
    property SecondaryShortCuts;
    property Visible;
    property BeforeExecute;
    property OnAccept;
    property OnCancel;
    property OnHint;
  end;


  { TPrintDlg }

  {TPrintDlg = class(TCommonDialogAction)
  private
    function GetDialog: TPrintDialog;
  protected
    function GetDialogClass: TCommonDialogClass; override;
  published
    property Caption;
    property Dialog: TPrintDialog read GetDialog;
    property Enabled;
    property HelpContext;
    property HelpKeyword;
    property HelpType;
    property Hint;
    property ImageIndex;
    property ShortCut;
    property SecondaryShortCuts;
    property Visible;
    property BeforeExecute;
    property OnAccept;
    property OnCancel;
    property OnHint;
  end;}


procedure Register;

implementation

procedure Register;
begin
  // register edit actions
  RegisterNoIcon([TEditCut, TEditCopy, TEditPaste, TEditSelectAll,
                  TEditUndo, TEditDelete]);
  // register search actions
  RegisterNoIcon([TSearchFind, TSearchReplace, TSearchFindFirst,
                  TSearchFindNext]);
  // register help actions
  RegisterNoIcon([THelpAction, THelpContents, THelpTopicSearch,
                  THelpOnHelp, THelpContextAction]);
  // register dialog actions
  RegisterNoIcon([TFontEdit, TColorSelect]);
  // register file actions
  RegisterNoIcon([TFileOpen, TFileOpenWith, TFileSaveAs, TFileExit]);
end;

{ TEditAction }

procedure TEditAction.SetControl(const AValue: TCustomEdit);
begin
  if FControl = AValue then
    Exit;
  if FControl <> nil then
    FControl.RemoveFreeNotification(Self);
  FControl := AValue;
  if FControl <> nil then
    FControl.FreeNotification(Self);
end;

procedure TEditAction.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (AComponent = FControl) then
    FControl := nil;
end;

destructor TEditAction.Destroy;
begin
  inherited Destroy;
end;

function TEditAction.HandlesTarget(Target: TObject): Boolean;
begin
  Result := Target <> nil;
  if Result then
    Result :=
      (Control = Target) or
      ((Control = nil) and (Target is TCustomEdit));
end;

{ TEditCut }

procedure TEditCut.ExecuteTarget(Target: TObject);
begin
  (Target as TCustomEdit).CutToClipboard;
end;

procedure TEditCut.UpdateTarget(Target: TObject);
begin
  Enabled := (Target as TCustomEdit).SelLength <> 0;
end;

{ TEditCopy }

procedure TEditCopy.ExecuteTarget(Target: TObject);
begin
  (Target as TCustomEdit).CopyToClipboard;
end;

procedure TEditCopy.UpdateTarget(Target: TObject);
begin
  Enabled := (Target as TCustomEdit).SelLength <> 0;
end;

{ TEditPaste }

procedure TEditPaste.UpdateTarget(Target: TObject);
begin
  Enabled := Clipboard.HasFormat(CF_TEXT);
end;

procedure TEditPaste.ExecuteTarget(Target: TObject);
begin
  (Target as TCustomEdit).PasteFromClipboard;
end;

{ TEditSelectAll }

procedure TEditSelectAll.ExecuteTarget(Target: TObject);
begin
  (Target as TCustomEdit).SelectAll;
end;

procedure TEditSelectAll.UpdateTarget(Target: TObject);
begin
  Enabled := (Target as TCustomEdit).Text <> '';
end;

{ TEditUndo }

procedure TEditUndo.ExecuteTarget(Target: TObject);
begin
  (Target as TCustomEdit).Undo;
end;

procedure TEditUndo.UpdateTarget(Target: TObject);
begin
  Enabled := (Target as TCustomEdit).CanUndo;
end;

{ TEditDelete }

procedure TEditDelete.ExecuteTarget(Target: TObject);
begin
  (Target as TCustomEdit).ClearSelection;
end;

procedure TEditDelete.UpdateTarget(Target: TObject);
begin
  Enabled := (Target as TCustomEdit).SelLength <> 0;
end;

{ THelpAction }

constructor THelpAction.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
end;

function THelpAction.HandlesTarget(Target: TObject): Boolean;
begin
  Result:=inherited HandlesTarget(Target);
end;

procedure THelpAction.UpdateTarget(Target: TObject);
begin
  inherited UpdateTarget(Target);
end;

{ THelpContents }

procedure THelpContents.ExecuteTarget(Target: TObject);
begin
  inherited ExecuteTarget(Target);
end;

{ THelpTopicSearch }

procedure THelpTopicSearch.ExecuteTarget(Target: TObject);
begin
  inherited ExecuteTarget(Target);
end;

{ THelpOnHelp }

procedure THelpOnHelp.ExecuteTarget(Target: TObject);
begin
  inherited ExecuteTarget(Target);
end;

{ THelpContextAction }

procedure THelpContextAction.ExecuteTarget(Target: TObject);
begin
  inherited ExecuteTarget(Target);
end;

procedure THelpContextAction.UpdateTarget(Target: TObject);
begin
  inherited UpdateTarget(Target);
end;

{ TCommonDialogAction }

procedure TCommonDialogAction.DoAccept;
begin
  if Assigned(FOnAccept) then
    OnAccept(Self);
end;

procedure TCommonDialogAction.DoBeforeExecute;
begin
  if Assigned(FBeforeExecute) then
    BeforeExecute(Self);
end;

procedure TCommonDialogAction.DoCancel;
begin
  if Assigned(FOnCancel) then
    OnCancel(Self);
end;

function TCommonDialogAction.GetDialogClass: TCommonDialogClass;
begin
  Result := nil;
end;

procedure TCommonDialogAction.CreateDialog;
var
  DlgClass: TCommonDialogClass;
begin
  DlgClass := GetDialogClass;
  if Assigned(DlgClass) then
  begin
    FDialog := DlgClass.Create(Self);
    FDialog.Name := DlgClass.ClassName;
    FDialog.SetSubComponent(True);
  end;
end;

constructor TCommonDialogAction.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  CreateDialog;
  
  DisableIfNoHandler := False;
  Enabled := True;
end;

function TCommonDialogAction.Handlestarget(Target: TObject): Boolean;
begin
  // no target
  Result := FDialog <> nil;
end;

procedure TCommonDialogAction.ExecuteTarget(Target: TObject);
begin
  DoBeforeExecute;
  FExecuteResult := FDialog.Execute;
  if FExecuteResult then
    DoAccept
  else
    DoCancel;
end;

{ TFileAction }

function TFileAction.GetFileName: TFileName;
begin
  Result := GetDialog.FileName;
end;

procedure TFileAction.SetFileName(const AValue: TFileName);
begin
  GetDialog.FileName := AValue;
end;

function TFileAction.GetDialog: TOpenDialog;
begin
  Result := TOpenDialog(FDialog);
end;

{ TFileOpen }

function TFileOpen.GetDialog: TOpenDialog;
begin
  Result := TOpenDialog(FDialog);
end;

function TFileOpen.GetDialogClass: TCommonDialogClass;
begin
  Result := TOpenDialog;
end;

{ TFileSaveAs }

function TFileSaveAs.GetSaveDialog: TSaveDialog;
begin
  Result := TSaveDialog(FDialog);
end;

function TFileSaveAs.GetDialogClass: TCommonDialogClass;
begin
  Result := TSaveDialog;
end;

{ TFileExit }

function TFileExit.HandlesTarget(Target: TObject): Boolean;
begin
  Result := True;
end;

procedure TFileExit.ExecuteTarget(Target: TObject);
begin
  if Assigned(Application) then
    if Assigned(Application.MainForm) then
      Application.MainForm.Close
    else
      Application.Terminate
  else
    halt(0);
end;

{ TSearchAction }

procedure TSearchAction.CreateDialog;
begin
  inherited CreateDialog;
  TFindDialog(FDialog).OnFind := @Search;
end;

procedure TSearchAction.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (AComponent = FControl) then
    FControl := nil;
end;

procedure TSearchAction.UpdateControl(NewControl: TCustomEdit);
begin
  if FControl <> nil then
    FControl.RemoveFreeNotification(Self);
  FControl := NewControl;
  if FControl <> nil then
    FControl.FreeNotification(Self);
end;

function TSearchAction.PerformSearch: Boolean;
var
  StartPos, Position, Increment, CharsToMatch: Integer;
  SearchTxt, Text: String;
  Down: Boolean;
  P: PChar;
  
  procedure RestoreSearch; inline;
  begin
    CharsToMatch := Length(SearchTxt);
    if not Down then
      P := PChar(SearchTxt) + CharsToMatch - 1
    else
      P := PChar(SearchTxt);
  end;
  
begin
  SearchTxt := UTF8Decode(TFindDialog(FDialog).FindText);
  Text := UTF8Decode(FControl.Text);
  
  Result := (SearchTxt <> '') and (Text <> '');
  if not Result then
    Exit;

  if not (frMatchCase in TFindDialog(FDialog).Options) then
  begin
    Text := LowerCase(Text);
    SearchTxt := LowerCase(SearchTxt);
  end;

  Down := frDown in TFindDialog(FDialog).Options;
  if not Down then
  begin
    Increment := -1;
    if InheritsFrom(TSearchFindFirst) then
      StartPos := Length(Text)
    else
      StartPos := FControl.SelStart - 1;
  end
  else
  begin
    Increment := 1;
    if InheritsFrom(TSearchFindFirst) then
      StartPos := 1
    else
      StartPos := FControl.SelStart + FControl.SelLength + 1;
  end;

  Result := False;
  RestoreSearch;
  Position := StartPos;
  while (Position > 0) and (Position <= Length(Text)) and (CharsToMatch > 0) do
  begin
    if Text[Position] = P^ then
    begin
      Dec(CharsToMatch);
      P := P + Increment;
    end
    else
      RestoreSearch;
    if CharsToMatch = 0 then
      break;
    Position := Position + Increment;
  end;
  Result := CharsToMatch = 0;
  
  if Result then
  begin
    if Down then
      FControl.SelStart := Position - Length(SearchTxt)
    else
      FControl.SelStart := Position - 1;
    FControl.SelLength := Length(SearchTxt);
  end;
end;

procedure TSearchAction.ShowNotFound;
begin
  MessageDlg(Format('Text "%s" is not found', [TFindDialog(FDialog).FindText]),
    mtWarning, [mbOk], 0);
end;

constructor TSearchAction.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  FControl := nil;
end;

destructor TSearchAction.Destroy;
begin
  if FControl <> nil then
    FControl.RemoveFreeNotification(Self);
  inherited Destroy;
end;

function TSearchAction.HandlesTarget(Target: TObject): Boolean;
begin
  Result := Target is TCustomEdit;
end;

procedure TSearchAction.Search(Sender: TObject);
begin
  if not PerformSearch then
    ShowNotFound;
end;

procedure TSearchAction.UpdateTarget(Target: TObject);
begin
  Enabled := (Target as TCustomEdit).Text <> '';
end;

procedure TSearchAction.ExecuteTarget(Target: TObject);
begin
  UpdateControl(Target as TCustomEdit);
  inherited ExecuteTarget(Target);
end;

{ TFontEdit }

function TFontEdit.GetDialog: TFontDialog;
begin
  Result := TFontDialog(FDialog);
end;

function TFontEdit.GetDialogClass: TCommonDialogClass;
begin
  Result := TFontDialog;
end;

{ TColorSelect }

function TColorSelect.GetDialog: TColorDialog;
begin
  Result := TColorDialog(FDialog);
end;

function TColorSelect.GetDialogClass: TCommonDialogClass;
begin
  Result := TColorDialog;
end;

{ TSearchFind }

function TSearchFind.GetFindDialog: TFindDialog;
begin
  Result := TFindDialog(FDialog);
end;

function TSearchFind.GetDialogClass: TCommonDialogClass;
begin
  Result := TFindDialog;
end;

{ TSearchReplace }

function TSearchReplace.GetReplaceDialog: TReplaceDialog;
begin
  Result := TReplaceDialog(FDialog);
end;

function TSearchReplace.GetDialogClass: TCommonDialogClass;
begin
  Result := TReplaceDialog;
end;

procedure TSearchReplace.CreateDialog;
begin
  inherited CreateDialog;
  TReplaceDialog(FDialog).OnReplace := @Replace;
end;

procedure TSearchReplace.Replace(Sender: TObject);
var
  Text, RText: String;
  p1, p2: integer;
begin
  if PerformSearch then
  begin
    Text := UTF8Decode(FControl.Text);
    RText := UTF8Decode(Dialog.ReplaceText);
    p1 := FControl.SelStart;
    p2 := FControl.SelLength;
    FControl.ClearSelection;
    Delete(Text, p1 + 1, p2);
    Insert(RText, Text, p1 + 1);
    FControl.Text := UTF8Encode(Text);
    FControl.SelStart := p1;
    FControl.SelLength := Length(RText);
  end
  else
    ShowNotFound;
end;

{ TSearchFindNext }

procedure TSearchFindNext.SetSearchFind(const AValue: TSearchFind);
begin
  if FSearchFind = AValue then
    Exit;
  if FSearchFind <> nil then
    FSearchFind.RemoveFreeNotification(Self);
  FSearchFind := AValue;
  if FSearchFind <> nil then
    FSearchFind.FreeNotification(Self);
end;

constructor TSearchFindNext.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  FSearchFind := nil;
end;

function TSearchFindNext.HandlesTarget(Target: TObject): Boolean;
begin
  Result := (Target is TCustomEdit);
end;

procedure TSearchFindNext.UpdateTarget(Target: TObject);
begin
  Enabled := ((Target as TCustomEdit).Text <> '') and
             (SearchFind <> nil) and
             (frFindNext in SearchFind.Dialog.Options);
end;

procedure TSearchFindNext.ExecuteTarget(Target: TObject);
begin
  SearchFind.UpdateControl(Target as TCustomEdit);
  SearchFind.Search(Target);
end;

procedure TSearchFindNext.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (AComponent = FSearchFind) then
    FSearchFind := nil;
end;

end.
