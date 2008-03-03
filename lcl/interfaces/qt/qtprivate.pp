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
 *  See the file COPYING.modifiedLGPL, included in this distribution,        *
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
    FOwner: TQtComboBox;
  protected
    procedure Put(Index: Integer; const S: string); override;
    procedure InsertItem(Index: Integer; const S: string); override;
    procedure InsertItem(Index: Integer; const S: string; O: TObject); override;
  public
    constructor Create(AOwner: TQtComboBox);
    procedure Assign(Source: TPersistent); override;
    procedure Clear; override;
    procedure Delete(Index: Integer); override;
    procedure Sort; override;
  public
    property Owner: TQtComboBox read FOwner;
  end;


  { TQtListStrings }

  TQtListStrings = class(TStringList)
  private
    FOwner: TQtListWidget;
  protected
    procedure Put(Index: Integer; const S: string); override;
    procedure InsertItem(Index: Integer; const S: string); override;
    procedure InsertItem(Index: Integer; const S: string; O: TObject); override;
  public
    constructor Create(AOwner: TQtListWidget);
    procedure Assign(Source: TPersistent); override;
    procedure Clear; override;
    procedure Delete(Index: Integer); override;
    procedure Sort; override;
  public
    property Owner: TQtListWidget read FOwner;
  end;

  { TQtMemoStrings }

  TQtMemoStrings = class(TStrings)
  private
    FTextChanged: Boolean; // StringList and QtTextEdit out of sync
    FStringList: TStringList; // Holds the lines to show
    FQtTextEdit: QTextEditH;  // Qt Widget
    FOwner: TWinControl;      // Lazarus Control Owning MemoStrings
    FUpdating: Boolean;       // We're changing Qt Widget
    procedure InternalUpdate;
    procedure ExternalUpdate(var Astr: WideString; AClear: Boolean = True);
    procedure IsChanged; // OnChange triggered by program action
  protected
    function GetTextStr: string; override;
    function GetCount: integer; override;
    function Get(Index : Integer) : string; override;
    //procedure SetSorted(Val : boolean); virtual;
  public
    constructor Create(TextEdit : QTextEditH; TheOwner: TWinControl);
    destructor Destroy; override;
    procedure Assign(Source : TPersistent); override;
    procedure Clear; override;
    procedure Delete(Index : integer); override;
    procedure Insert(Index : integer; const S: string); override;
    procedure SetText(TheText: PChar); override;
    //procedure Sort; virtual;
  public
    //property Sorted: boolean read FSorted write SetSorted;
    property Owner: TWinControl read FOwner;
    function TextChangedHandler(Sender: QObjectH; Event: QEventH): Boolean; cdecl;
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
  Astr: WideString;
begin
  QTextEdit_toPlainText(FQtTextEdit,@Astr); // get the memo content
  FStringList.Text := UTF8Encode(Astr);
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
  Str: WideString;
begin
  FUpdating := True;
  Str := GetUtf8String(AStr);
  if AClear then
  begin
    QTextEdit_clear(FQtTextEdit);
    QTextEdit_setPlainText(FQtTextEdit,@Str);
  end
  else
    QTextEdit_append(FQtTextEdit,@Str);
    
  FUpdating := False;
  {FillChar(Mess, SizeOf(Mess), #0);
  (FOwner as TCustomMemo).Modified := False;
  FOwner.Dispatch(TLMessage(Mess));}
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
  if Assigned((FOwner as TCustomMemo).OnChange) then
  begin
    (FOwner as TCustomMemo).Modified := False;
    (FOwner as TCustomMemo).OnChange(self);
  end;
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


{------------------------------------------------------------------------------
  Method: TQtMemoStrings.Create
  Params:  Qt Widget Handle and Lazarus WinControl Parent Object
  Returns: Nothing

  Constructor for the class.
 ------------------------------------------------------------------------------}
constructor TQtMemoStrings.Create(TextEdit: QTextEditH; TheOwner: TWinControl);
var
  Method: TMethod;
  Hook : QTextEdit_hookH;
begin
  inherited Create;

  {$ifdef VerboseQt}
    if (TextEdit = nil) then WriteLn('TQtMemoStrings.Create Unspecified TextEdit widget');
    if (TheOwner = nil) then WriteLn('TQtMemoStrings.Create Unspecified owner');
  {$endif}

  FStringList := TStringList.Create;
  FQtTextEdit := TextEdit;
  QTextEdit_clear(FQtTextEdit);
  FOwner:=TheOwner;

  // Callback Event
  {Method := MemoChanged;}
  TEventFilterMethod(Method) := @TextChangedHandler;
  Hook := QTextEdit_hook_create(FQtTextEdit);
  QTextEdit_hook_hook_textChanged(Hook, Method);
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
  inherited Destroy;
end;

{------------------------------------------------------------------------------
  Method: TQtMemoStrings.TextChangedHandler
  Params:  None
  Returns: Nothing

  Signal handler for the TextChanged Signal.
 ------------------------------------------------------------------------------}
function TQtMemoStrings.TextChangedHandler(Sender: QObjectH; Event: QEventH): Boolean; cdecl;
var
  Mess: TLMessage;
begin
  if not FUpdating then
  begin
    FTextChanged := True;
    FillChar(Mess, SizeOf(Mess), #0);
    Mess.Msg := CM_TEXTCHANGED;
    //(FOwner as TCustomMemo).Modified := True;
    FOwner.Dispatch(TLMessage(Mess));
  end;
  Result := True;
end;

{------------------------------------------------------------------------------
  Method: TQtMemoStrings.Assign
  Params:  None
  Returns: Nothing

  Assigns from a TStrings.
 ------------------------------------------------------------------------------}
procedure TQtMemoStrings.Assign(Source: TPersistent);
var
  Astr: WideString;
begin
  if (Source=Self) or (Source=nil)
  then
    exit;
  if Source is TStrings then
  begin
    FStringList.Clear;
    FStringList.Text := TStrings(Source).Text;
    Astr := FStringList.Text;
    ExternalUpdate(Astr,True);
    FTextChanged := False;
    exit;
  end;
  Inherited Assign(Source);
end;

{------------------------------------------------------------------------------
  Method: TQtMemoStrings.Clear
  Params:  None
  Returns: Nothing

  Clears all.
 ------------------------------------------------------------------------------}
procedure TQtMemoStrings.Clear;
begin
  FUpdating := True;
  FStringList.Clear;
  
  if not (csDestroying in FOwner.ComponentState)
  and not (csFreeNotification in FOwner.ComponentState)
  then
    QTextEdit_clear(FQtTextEdit);
    
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
var
  Astr: WideString;
begin
  if FTextChanged then InternalUpdate;
  if Index < FStringList.Count then
  begin
    FStringList.Delete(Index);
    Astr := FStringList.Text;
    ExternalUpdate(AStr,True);
    FTextChanged := False;
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
  Astr: WideString;
begin
  if FTextChanged then InternalUpdate;
  if Index < 0 then Index := 0;
  if Index <= FStringList.Count then
  begin
    FStringList.Insert(Index,S);
    Astr := S;
    ExternalUpdate(AStr, False);
    FTextChanged := False;
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
  str: String;
  Astr: WideString;
begin
  str := StrPas(TheText);
  FStringList.Text := str;
  AStr := Str;
  ExternalUpdate(Astr,True);
  FTextChanged := False;
end;

{ TQtComboStrings }

procedure TQtComboStrings.Put(Index: Integer; const S: string);
begin
  inherited Put(Index, S);

  FOwner.removeItem(Index);
  FOwner.insertItem(Index, S);
end;

procedure TQtComboStrings.InsertItem(Index: Integer; const S: string);
begin
  inherited InsertItem(Index, S);
  FOwner.insertItem(Index, S);
end;

procedure TQtComboStrings.InsertItem(Index: Integer; const S: string; O: TObject);
begin
  inherited InsertItem(Index, S, O);

  FOwner.insertItem(Index, S);
end;

constructor TQtComboStrings.Create(AOwner: TQtComboBox);
begin
  inherited Create;
  FOwner := AOwner;
end;

procedure TQtComboStrings.Assign(Source: TPersistent);
begin
  FOwner.BeginUpdate;
  inherited Assign(Source);
  FOwner.EndUpdate;
end;

procedure TQtComboStrings.Clear;
var
  I: Integer;
  C: Integer;
begin
  C := Count;
  inherited Clear;
  for I := C - 1 downto 0 do
    FOwner.removeItem(I);
end;

procedure TQtComboStrings.Delete(Index: Integer);
begin
  inherited Delete(Index);

  FOwner.removeItem(Index);
end;

procedure TQtComboStrings.Sort;
var
  I: Integer;
begin
  inherited Sort;

  for I := 0 to Count - 1 do
  begin
    FOwner.removeItem(I);
    FOwner.insertItem(I, Strings[I]);
  end;
end;

{ TQtListStrings }

procedure TQtListStrings.Put(Index: Integer; const S: string);
begin
  inherited Put(Index, S);

  FOwner.removeItem(Index);
  FOwner.insertItem(Index, S);
end;

procedure TQtListStrings.InsertItem(Index: Integer; const S: string);
begin
  inherited InsertItem(Index, S);
  FOwner.insertItem(Index, S);
end;

procedure TQtListStrings.InsertItem(Index: Integer; const S: string; O: TObject);
begin
  inherited InsertItem(Index, S, O);

  FOwner.insertItem(Index, S);
end;

constructor TQtListStrings.Create(AOwner: TQtListWidget);
begin
  inherited Create;
  FOwner := AOwner;
end;

procedure TQtListStrings.Assign(Source: TPersistent);
begin
  FOwner.BeginUpdate;
  inherited Assign(Source);
  FOwner.EndUpdate;
end;

procedure TQtListStrings.Clear;
var
  I: Integer;
  C: Integer;
begin
  C := Count;
  inherited Clear;
  for I := C - 1 downto 0 do
    FOwner.removeItem(I);
end;

procedure TQtListStrings.Delete(Index: Integer);
begin
  inherited Delete(Index);

  FOwner.removeItem(Index);
end;

procedure TQtListStrings.Sort;
var
  I: Integer;
begin
  inherited Sort;

  for I := 0 to Count - 1 do
  begin
    FOwner.removeItem(I);
    FOwner.insertItem(I, Strings[I]);
  end;
end;

end.
