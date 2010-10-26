{
 *****************************************************************************
 *                              QtPrivate.pp                                 *
 *                              --------------                               *
 *                                                                           *
 *                                                                           *
 *****************************************************************************

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
unit qtprivate;

{$mode objfpc}{$H+}

interface

{$I qtdefines.inc}

uses
  // Bindings
  qt4,
  // Free Pascal
  Classes, SysUtils, Types,
  // LCL
  LMessages, Forms, Controls, LCLType, LCLProc, ExtCtrls, StdCtrls, Menus,
  CheckLst,
  //Widgetset
  QtWidgets, qtproc;

type

  { TQtComboStrings }

  TQtComboStrings = class(TStringList)
  private
    FWinControl: TWinControl;
    FOwner: TQtComboBox;
  protected
    procedure Put(Index: Integer; const S: string); override;
    procedure InsertItem(Index: Integer; const S: string); override;
    procedure InsertItem(Index: Integer; const S: string; O: TObject); override;
  public
    constructor Create(AWinControl: TWinControl; AOwner: TQtComboBox);
    procedure Assign(Source: TPersistent); override;
    procedure Clear; override;
    procedure Delete(Index: Integer); override;
    procedure Sort; override;
    procedure Exchange(AIndex1, AIndex2: Integer); override;
  public
    property Owner: TQtComboBox read FOwner;
  end;


  { TQtListStrings }

  TQtListStrings = class(TStringList)
  private
    FWinControl: TWinControl;
    FOwner: TQtListWidget;
  protected
    procedure Put(Index: Integer; const S: string); override;
    procedure InsertItem(Index: Integer; const S: string); override;
    procedure InsertItem(Index: Integer; const S: string; O: TObject); override;
  public
    constructor Create(AWinControl: TWinControl; AOwner: TQtListWidget);
    procedure Assign(Source: TPersistent); override;
    procedure Clear; override;
    procedure Delete(Index: Integer); override;
    procedure Sort; override;
    procedure Exchange(AIndex1, AIndex2: Integer); override;
  public
    property Owner: TQtListWidget read FOwner;
  end;

  { TQtMemoStrings }

  TQtMemoStrings = class(TStrings)
  private
    FTextChangedHook : QTextEdit_hookH;
    FTextChanged: Boolean; // StringList and QtTextEdit out of sync
    FStringList: TStringList; // Holds the lines to show
    FOwner: TWinControl;      // Lazarus Control Owning MemoStrings
    FUpdating: Boolean;       // We're changing Qt Widget
    procedure InternalUpdate;
    procedure ExternalUpdate(var Astr: WideString; AClear: Boolean = True);
    procedure IsChanged; // OnChange triggered by program action
  protected
    function getTextEdit: QTextEditH; // QtWidget handle
    function GetTextStr: string; override;
    function GetCount: integer; override;
    function Get(Index : Integer) : string; override;
    procedure Put(Index: Integer; const S: string); override;
  public
    constructor Create(TextEdit : QTextEditH; TheOwner: TWinControl);
    destructor Destroy; override;
    procedure Assign(Source : TPersistent); override;
    procedure Clear; override;
    procedure Delete(Index : integer); override;
    procedure Insert(Index : integer; const S: string); override;
    procedure SetText(TheText: PChar); override;
  public
    property Owner: TWinControl read FOwner;
    procedure TextChangedHandler; cdecl;
  end;

implementation

{ TQtMemoStrings }

{------------------------------------------------------------------------------
  Private Method: TQtMemoStrings.InternalUpdate
  Params:  None
  Returns: Nothing

  Updates internal StringList from Qt Widget
 ------------------------------------------------------------------------------}
procedure TQtMemoStrings.InternalUpdate;
var
  W: WideString;
  TextEdit: QTextEditH;
begin
  TextEdit := getTextEdit;
  if TextEdit <> nil then
    QTextEdit_toPlainText(TextEdit, @W); // get the memo content
  FStringList.Text := UTF16ToUTF8(W);
  FTextChanged := False;
end;

{------------------------------------------------------------------------------
  Private Method: TQtMemoStrings.ExternalUpdate
  Params:  Astr: Text for Qt Widget; Clear: if we must clear first
  Returns: Nothing

  Updates Qt Widget from text - If DelphiOnChange, generates OnChange Event
 ------------------------------------------------------------------------------}
procedure TQtMemoStrings.ExternalUpdate(var Astr: WideString; AClear: Boolean = True);
var
  W: WideString;
  TextEdit: QTextEditH;
begin
  FUpdating := True;
  W := GetUtf8String(AStr);
  TextEdit := getTextEdit;
  if TextEdit <> nil then
  begin
    if AClear then
    begin
      QTextEdit_clear(TextEdit);
      QTextEdit_setPlainText(TextEdit,@W);
    end else
      QTextEdit_append(TextEdit,@W);

    if QTextEdit_alignment(TextEdit) <> AlignmentMap[TCustomMemo(FOwner).Alignment] then
      QTextEdit_setAlignment(TextEdit, AlignmentMap[TCustomMemo(FOwner).Alignment]);
  end;
    
  FUpdating := False;
  IsChanged;
  FUpdating := False;
end;

{------------------------------------------------------------------------------
  Private Method: TQtMemoStrings.IsChanged
  Params:  None
  Returns: Nothing

  Triggers the OnChange Event, with modified set to false
 ------------------------------------------------------------------------------}
procedure TQtMemoStrings.IsChanged;
begin
  if Assigned(FOwner) and Assigned((FOwner as TCustomMemo).OnChange) then
  begin
    (FOwner as TCustomMemo).Modified := False;
    (FOwner as TCustomMemo).OnChange(FOwner);
  end;
end;

function TQtMemoStrings.getTextEdit: QTextEditH;
begin
  Result := nil;
  if FOwner.HandleAllocated then
    Result := QTextEditH(TQtTextEdit(FOwner.Handle).Widget);
end;

{------------------------------------------------------------------------------
  Method: TQtMemoStrings.GetTextStr
  Params:  None
  Returns: a string

  Return the whole StringList content as a single string
 ------------------------------------------------------------------------------}
function TQtMemoStrings.GetTextStr: string;
begin
  if FTextChanged then InternalUpdate;
  Result := FStringList.Text;
end;

{------------------------------------------------------------------------------
  Method: TQtMemoStrings.GetCount
  Params:  None
  Returns: an integer

  Return the current number of strings
 ------------------------------------------------------------------------------}
function TQtMemoStrings.GetCount: integer;
begin
  if FTextChanged then InternalUpdate;
  Result := FStringList.Count;
end;

{------------------------------------------------------------------------------
  Method: TQtMemoStrings.GetCount
  Params:  String Index
  Returns: a string

  Return the string[Index], or an empty string of out of bounds.
 ------------------------------------------------------------------------------}
function TQtMemoStrings.Get(Index: Integer): string;
begin
  if FTextChanged then InternalUpdate;
  if Index < FStringList.Count then
     Result := FStringList.Strings[Index]
  else Result := '';
end;

procedure TQtMemoStrings.Put(Index: Integer; const S: string);
var
  W: WideString;
begin
  if FTextChanged then InternalUpdate;
  FStringList[Index] := S;
  W := GetUTF8String(S);
  TQtTextEdit(FOwner.Handle).setLineText(Index, W);
end;

{------------------------------------------------------------------------------
  Method: TQtMemoStrings.Create
  Params:  Qt Widget Handle and Lazarus WinControl Parent Object
  Returns: Nothing

  Constructor for the class.
 ------------------------------------------------------------------------------}
constructor TQtMemoStrings.Create(TextEdit: QTextEditH; TheOwner: TWinControl);
begin
  inherited Create;

  {$ifdef VerboseQt}
    if (TextEdit = nil) then WriteLn('TQtMemoStrings.Create Unspecified TextEdit widget');
    if (TheOwner = nil) then WriteLn('TQtMemoStrings.Create Unspecified owner');
  {$endif}

  FStringList := TStringList.Create;
  QTextEdit_clear(TextEdit);
  FOwner:=TheOwner;
  // Callback Event
  {Method := MemoChanged;   }
  FTextChangedHook := QTextEdit_hook_create(TextEdit);
  QTextEdit_hook_hook_textChanged(FTextChangedHook, @TextChangedHandler);
end;

{------------------------------------------------------------------------------
  Method: TQtMemoStrings.Destroy
  Params:  None
  Returns: Nothing

  Destructor for the class.
 ------------------------------------------------------------------------------}
destructor TQtMemoStrings.Destroy;
begin
  Clear;
  FStringList.Free;
  // don't destroy the widgets
  if FTextChangedHook <> nil then
    QTextEdit_hook_destroy(FTextChangedHook);
  inherited Destroy;
end;

{------------------------------------------------------------------------------
  Method: TQtMemoStrings.TextChangedHandler
  Params:  None
  Returns: Nothing

  Signal handler for the TextChanged Signal.
 ------------------------------------------------------------------------------}
procedure TQtMemoStrings.TextChangedHandler; cdecl;
var
  Mess: TLMessage;
begin
  if not FUpdating then
  begin
    FTextChanged := True;
    FillChar(Mess, SizeOf(Mess), #0);
    Mess.Msg := CM_TEXTCHANGED;
    FOwner.Dispatch(TLMessage(Mess));
  end;
end;

{------------------------------------------------------------------------------
  Method: TQtMemoStrings.Assign
  Params:  None
  Returns: Nothing

  Assigns from a TStrings.
 ------------------------------------------------------------------------------}
procedure TQtMemoStrings.Assign(Source: TPersistent);
var
  W: WideString;
begin
  if (Source=Self) or (Source=nil)
  then
    exit;
  if Source is TStrings then
  begin
    FStringList.Clear;
    FStringList.Text := TStrings(Source).Text;
    W := FStringList.Text;
    ExternalUpdate(W,True);
    FTextChanged := False;
    exit;
  end;
  inherited Assign(Source);
end;

{------------------------------------------------------------------------------
  Method: TQtMemoStrings.Clear
  Params:  None
  Returns: Nothing

  Clears all.
 ------------------------------------------------------------------------------}
procedure TQtMemoStrings.Clear;
var
  TextEdit: QTextEditH;
begin
  FUpdating := True;
  FStringList.Clear;
  TextEdit := getTextEdit;
  if not (csDestroying in FOwner.ComponentState)
  and not (csFreeNotification in FOwner.ComponentState)
  and (TextEdit <> nil) then
    QTextEdit_clear(TextEdit);
    
  FTextChanged := False;
  FUpdating := False;
  IsChanged;
end;

{------------------------------------------------------------------------------
  Method: TQtMemoStrings.Delete
  Params:  Index
  Returns: Nothing

  Deletes line at Index.
 ------------------------------------------------------------------------------}
procedure TQtMemoStrings.Delete(Index: integer);
begin
  if FTextChanged then InternalUpdate;
  if (Index >= 0) and (Index < FStringList.Count) then
  begin
    FStringList.Delete(Index);
    TQtTextEdit(FOwner.Handle).removeLine(Index);
  end;
end;

{------------------------------------------------------------------------------
  Method: TQtMemoStrings.Insert
  Params:  Index, string
  Returns: Nothing

  Inserts line at Index.
 ------------------------------------------------------------------------------}
procedure TQtMemoStrings.Insert(Index: integer; const S: string);
var
  W: WideString;
begin
  if FTextChanged then InternalUpdate;
  if Index < 0 then Index := 0;
  if Index <= FStringList.Count then
  begin
    FStringList.Insert(Index, S);
    if TQtTextEdit(FOwner.Handle).getBlockCount - Index <= 1 then
    begin
      if (UTF8Pos('<', S) > 0) or (UTF8Pos('>',S) > 0) then
      begin
        // workaround for qt richtext parser bug
        W := GetUTF8String(S);
        TQtTextEdit(FOwner.Handle).insertLine(Index, W);
      end else
      begin
        // append is much faster in case when we add strings
        W := S;
        ExternalUpdate(W, False);
        FTextChanged := False;
      end;
    end else
    begin
      W := GetUTF8String(S);
      TQtTextEdit(FOwner.Handle).insertLine(Index, W);
    end;
  end;
end;

{------------------------------------------------------------------------------
  Method: TQtMemoStrings.SetText
  Params:  A null terminated string
  Returns: Nothing

  Fills the memo with the string
 ------------------------------------------------------------------------------}
procedure TQtMemoStrings.SetText(TheText: PChar);
Var
  S: String;
  W: WideString;
begin
  S := StrPas(TheText);
  FStringList.Text := S;
  W := S;
  ExternalUpdate(W,True);
  FTextChanged := False;
end;

{ TQtComboStrings }

procedure TQtComboStrings.Put(Index: Integer; const S: string);
begin
  inherited Put(Index, S);
  FOwner.BeginUpdate;
  FOwner.setItemText(Index, S);
  FOwner.EndUpdate;
end;

procedure TQtComboStrings.InsertItem(Index: Integer; const S: string);
var
  FSavedIndex: Integer;
  FSavedText: WideString;
begin
  inherited InsertItem(Index, S);
  FOwner.BeginUpdate;
  FSavedText := FOwner.getText;
  FSavedIndex := FOwner.currentIndex;
  FOwner.insertItem(Index, S);
  if FOwner.getEditable then
  begin
    if (FSavedIndex <> FOwner.currentIndex) then
      FOwner.setCurrentIndex(FSavedIndex);
    FOwner.setText(FSavedText);
  end;
  FOwner.EndUpdate;
end;

procedure TQtComboStrings.InsertItem(Index: Integer; const S: string; O: TObject);
var
  FSavedIndex: Integer;
  FSavedText: WideString;
begin
  inherited InsertItem(Index, S, O);
  FOwner.BeginUpdate;
  FSavedText := FOwner.getText;
  FSavedIndex := FOwner.currentIndex;
  FOwner.insertItem(Index, S);
  if FOwner.getEditable then
  begin
    if (FSavedIndex <> FOwner.currentIndex) then
      FOwner.setCurrentIndex(FSavedIndex);
    FOwner.setText(FSavedText);
  end;
  FOwner.EndUpdate;
end;

constructor TQtComboStrings.Create(AWinControl: TWinControl;
    AOwner: TQtComboBox);
begin
  inherited Create;
  FWinControl := AWinControl;
  FOwner := AOwner;
end;

procedure TQtComboStrings.Assign(Source: TPersistent);
begin
  if Assigned(FWinControl) and (FWinControl.HandleAllocated) then
  begin
    FOwner.BeginUpdate;
    inherited Assign(Source);
    FOwner.EndUpdate;
  end;
end;

procedure TQtComboStrings.Clear;
begin
  inherited Clear;

  if Assigned(FWinControl) and (FWinControl.HandleAllocated) then
  begin
    FOwner.BeginUpdate;
    FOwner.ClearItems;
    FOwner.EndUpdate;
  end;
end;

procedure TQtComboStrings.Delete(Index: Integer);
begin
  inherited Delete(Index);
  if Assigned(FWinControl) and (FWinControl.HandleAllocated) then
  begin
    FOwner.BeginUpdate;
    FOwner.removeItem(Index);
    FOwner.EndUpdate;
  end;
end;

procedure TQtComboStrings.Sort;
var
  I: Integer;
begin
  inherited Sort;
  if Assigned(FWinControl) and (FWinControl.HandleAllocated) then
  begin
    FOwner.BeginUpdate;
    for I := 0 to Count - 1 do
      FOwner.setItemText(I, Strings[I]);
    FOwner.EndUpdate;
  end;
end;

procedure TQtComboStrings.Exchange(AIndex1, AIndex2: Integer);
var
  i: Integer;
begin
  inherited Exchange(AIndex1, AIndex2);
  if Assigned(FWinControl) and (FWinControl.HandleAllocated) then
  begin
    FOwner.BeginUpdate;
    for I := 0 to Count - 1 do
      FOwner.setItemText(I, Strings[I]);
    FOwner.EndUpdate;
  end;
end;

{ TQtListStrings }

procedure TQtListStrings.Put(Index: Integer; const S: string);
begin
  inherited Put(Index, S);
  if Assigned(FWinControl) and (FWinControl.HandleAllocated) then
    FOwner.setItemText(Index, S);
end;

procedure TQtListStrings.InsertItem(Index: Integer; const S: string);
begin
  inherited InsertItem(Index, S);
  if Assigned(FWinControl) and (FWinControl.HandleAllocated) then
    FOwner.insertItem(Index, S);
end;

procedure TQtListStrings.InsertItem(Index: Integer; const S: string; O: TObject);
begin
  inherited InsertItem(Index, S, O);
  if Assigned(FWinControl) and (FWinControl.HandleAllocated) then
    FOwner.insertItem(Index, S);
end;

constructor TQtListStrings.Create(AWinControl: TWinControl;
  AOwner: TQtListWidget);
begin
  inherited Create;
  FWinControl := AWinControl;
  FOwner := AOwner;
end;

procedure TQtListStrings.Assign(Source: TPersistent);
begin
  if Assigned(FWinControl) and (FWinControl.HandleAllocated) then
  begin
    FOwner.BeginUpdate;
    inherited Assign(Source);
    FOwner.EndUpdate;
  end;
end;

procedure TQtListStrings.Clear;
begin
  inherited Clear;

  if Assigned(FWinControl) and (FWinControl.HandleAllocated) then
  begin
    FOwner.BeginUpdate;
    FOwner.ClearItems;
    FOwner.EndUpdate;
  end;
end;

procedure TQtListStrings.Delete(Index: Integer);
begin
  inherited Delete(Index);
  if Assigned(FWinControl) and (FWinControl.HandleAllocated) then
    FOwner.removeItem(Index);
end;

procedure TQtListStrings.Sort;
var
  I: Integer;
begin
  inherited Sort;
  if Assigned(FWinControl) and (FWinControl.HandleAllocated) then
  begin
    for I := 0 to Count - 1 do
      FOwner.setItemText(I, Strings[I]);
  end;
end;

procedure TQtListStrings.Exchange(AIndex1, AIndex2: Integer);
begin
  inherited Exchange(AIndex1, AIndex2);
  if Assigned(FWinControl) and (FWinControl.HandleAllocated) then
    FOwner.exchangeItems(AIndex1, AIndex2);
end;

end.
