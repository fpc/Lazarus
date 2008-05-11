{
 /***************************************************************************
                                StdActns.pas
                                ------------


 ***************************************************************************/

 *****************************************************************************
 *                                                                           *
 *  This file is part of the Lazarus Component Library (LCL)                 *
 *                                                                           *
 *  See the file COPYING.modifiedLGPL, included in this distribution,        *
 *  for details about the copyright.                                         *
 *                                                                           *
 *  This program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
 *                                                                           *
 *****************************************************************************

  Only types, no code yet.

  ToDo: Implement the actions.
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
    procedure DoCancel;
    function GetDialogClass: TCommonDialogClass; virtual;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure SetupDialog;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
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
  public
    constructor Create(TheOwner: TComponent); override;
    procedure ExecuteTarget(Target: TObject); override;
    property Dialog: TOpenDialog read GetDialog;
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
  public
    procedure ExecuteTarget(Target: TObject); override;
  published
    property FileName: TFileName read FFileName write FFileName;
    property HelpContext;
    property HelpKeyword;
    property HelpType;
    property BeforeExecute;
    property AfterOpen: TNotifyEvent read FAfterOpen write FAfterOpen;
  end;

  TFileSaveAs = class(TFileAction)
  private
    function GetSaveDialog: TSaveDialog;
  protected
    function GetDialogClass: TCommonDialogClass; override;
  public
    constructor Create(TheOwner: TComponent); override;
    property Dialog: TSaveDialog read GetSaveDialog;
  published
    property Caption;
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

  TSearchAction = class(TCommonDialogAction)
  protected
    FControl: TCustomEdit;
    FFindFirst: Boolean;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    function HandlesTarget(Target: TObject): Boolean; override;
    procedure Search(Sender: TObject); virtual;
    procedure UpdateTarget(Target: TObject); override;
    procedure ExecuteTarget(Target: TObject); override;
  end;

  {TSearchFind = class(TSearchAction)
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

  TSearchReplace = class(TSearchAction)
  private
    function GetReplaceDialog: TReplaceDialog;
  protected
    function GetDialogClass: TCommonDialogClass; override;
  public
    procedure ExecuteTarget(Target: TObject); override;
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

  TSearchFindFirst = class(TSearchFind)
  public
    constructor Create(TheOwner: TComponent); override;
  end;

  TSearchFindNext = class(TCustomAction)
  private
    FSearchFind: TSearchFind;
  public
    constructor Create(TheOwner: TComponent); override;
    function HandlesTarget(Target: TObject): Boolean; override;
    procedure UpdateTarget(Target: TObject); override;
    procedure ExecuteTarget(Target: TObject); override;
  published
    property Caption;
    property Enabled;
    property HelpContext;
    property HelpKeyword;
    property HelpType;
    property Hint;
    property ImageIndex;
    property SearchFind: TSearchFind read FSearchFind write FSearchFind;
    property ShortCut;
    property SecondaryShortCuts;
    property Visible;
    property OnHint;
  end;}


  { TFontEdit }

  TFontEdit = class(TCommonDialogAction)
  private
    function GetDialog: TFontDialog;
  protected
    function GetDialogClass: TCommonDialogClass; override;
  public
    property Dialog: TFontDialog read GetDialog;
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
  public
    property Dialog: TColorDialog read GetDialog;
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
  RegisterNoIcon([TEditCut,TEditCopy,TEditPaste,TEditSelectAll,
                  TEditUndo,TEditDelete]);
  // register help actions
  RegisterNoIcon([THelpAction,THelpContents,THelpTopicSearch,
                  THelpOnHelp,THelpContextAction]);
  // register dialog actions
  RegisterNoIcon([TFontEdit,TColorSelect]);
  // register file actions
  RegisterNoIcon([TFileOpen,TFileOpenWith,TFileSaveAs,TFileExit]);
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

end;

procedure TCommonDialogAction.DoCancel;
begin

end;

function TCommonDialogAction.GetDialogClass: TCommonDialogClass;
begin
  Result:=nil;
end;

procedure TCommonDialogAction.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
end;

procedure TCommonDialogAction.SetupDialog;
begin

end;

constructor TCommonDialogAction.Create(TheOwner: TComponent);
var
  DlgClass: TCommonDialogClass;
begin
  inherited Create(TheOwner);
  DlgClass := GetDialogClass;
  if Assigned(DlgClass)
  then FDialog := DlgClass.Create(Self);

  DisableIfNoHandler := False;
  Enabled := True;
end;

destructor TCommonDialogAction.Destroy;
begin
  inherited Destroy;
end;

function TCommonDialogAction.Handlestarget(Target: TObject): Boolean;
begin
  Result:=inherited Handlestarget(Target);
end;

procedure TCommonDialogAction.ExecuteTarget(Target: TObject);
begin
  inherited ExecuteTarget(Target);
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
  Result:=inherited GetDialogClass;
end;

constructor TFileOpen.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
end;

procedure TFileOpen.ExecuteTarget(Target: TObject);
begin
  inherited ExecuteTarget(Target);
end;

{ TFileOpenWith }

procedure TFileOpenWith.ExecuteTarget(Target: TObject);
begin
  inherited ExecuteTarget(Target);
end;

{ TFileSaveAs }

function TFileSaveAs.GetSaveDialog: TSaveDialog;
begin
  Result := TSaveDialog(FDialog);
end;

function TFileSaveAs.GetDialogClass: TCommonDialogClass;
begin
  Result:=inherited GetDialogClass;
end;

constructor TFileSaveAs.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
end;

{ TFileExit }

function TFileExit.HandlesTarget(Target: TObject): Boolean;
begin
  Result:=inherited HandlesTarget(Target);
end;

procedure TFileExit.ExecuteTarget(Target: TObject);
begin
  inherited ExecuteTarget(Target);
end;

{ TSearchAction }

procedure TSearchAction.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
end;

constructor TSearchAction.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
end;

destructor TSearchAction.Destroy;
begin
  inherited Destroy;
end;

function TSearchAction.HandlesTarget(Target: TObject): Boolean;
begin
  Result:=inherited HandlesTarget(Target);
end;

procedure TSearchAction.Search(Sender: TObject);
begin

end;

procedure TSearchAction.UpdateTarget(Target: TObject);
begin
  inherited UpdateTarget(Target);
end;

procedure TSearchAction.ExecuteTarget(Target: TObject);
begin
  inherited ExecuteTarget(Target);
end;

{ TFontEdit }

function TFontEdit.GetDialog: TFontDialog;
begin
  Result := TFontDialog(FDialog);
end;

function TFontEdit.GetDialogClass: TCommonDialogClass;
begin
  Result:=inherited GetDialogClass;
end;

{ TColorSelect }

function TColorSelect.GetDialog: TColorDialog;
begin
  Result := TColorDialog(FDialog);
end;

function TColorSelect.GetDialogClass: TCommonDialogClass;
begin
  Result:=inherited GetDialogClass;
end;

end.
