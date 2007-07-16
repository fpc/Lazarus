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

{$mode delphi}{$H+}

interface

uses
  // Bindings
{$ifdef USE_QT_4_3}
  qt43,
{$else}
  qt4,
{$endif}
  // Free Pascal
  Classes, SysUtils, Types,
  // LCL
  LMessages, Forms, Controls, LCLType, LCLProc, ExtCtrls, StdCtrls, Menus,
  CheckLst;

type

  { TQtComboStrings }

  TQtComboStrings = class(TStrings)
  private
    FComboChanged: Boolean; // StringList and QtComboBox out of sync
    FStringList: TStringList; // Holds the items to show
    FQtComboBox: QComboBoxH;  // Qt Widget
    FOwner: TWinControl;      // Lazarus Control Owning ListStrings
    FUpdating: Boolean;       // We're changing Qt Widget
    procedure InternalUpdate;
    procedure ExternalUpdate(var Astr: TStringList; Clear: Boolean = True);
    procedure IsChanged; // OnChange triggered by program action
  protected
    function GetTextStr: string; override;
    function GetCount: integer; override;
    function Get(Index : Integer) : string; override;
    //procedure SetSorted(Val : boolean); virtual;
  public
    constructor Create(ComboBoxH : QComboBoxH; TheOwner: TWinControl);
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
  end;


  { TQtListStrings }

  TQtListStrings = class(TStrings)
  private
    FListChanged: Boolean; // StringList and QtListWidget out of sync
    FStringList: TStringList; // Holds the items to show
    FQtListWidget: QListWidgetH;  // Qt Widget
    FOwner: TWinControl;      // Lazarus Control Owning ListStrings
    FUpdating: Boolean;       // We're changing Qt Widget
    procedure InternalUpdate;
    procedure ExternalUpdate(var Astr: TStringList; Clear: Boolean = True);
    procedure IsChanged; // OnChange triggered by program action
  protected
    function GetTextStr: string; override;
    function GetCount: integer; override;
    function Get(Index : Integer) : string; override;
    //procedure SetSorted(Val : boolean); virtual;
  public
    constructor Create(ListWidgetH : QListWidgetH; TheOwner: TWinControl);
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
    procedure ExternalUpdate(var Astr: WideString; Clear: Boolean = True);
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

{ TQtListStrings }

procedure TQtListStrings.InternalUpdate;
begin

end;

procedure TQtListStrings.ExternalUpdate(var Astr: TStringList; Clear: Boolean);
var
  i: Integer;
  TmpStr: WideString;
begin
  FUpdating := True;
  if Clear then
    QListWidget_clear(FQtListWidget);
  for i := 0 to AStr.Count -1 do
  begin
    TmpStr := UTF8Decode(Astr[i]);
    QListWidget_additem(FQtListWidget, @TmpStr);
  end;
  FUpdating := False;
  IsChanged;
  FUpdating := False;
end;

procedure TQtListStrings.IsChanged;
begin

end;

function TQtListStrings.GetTextStr: string;
begin
  Result := inherited GetTextStr;
end;

function TQtListStrings.GetCount: integer;
begin
  if FListChanged then InternalUpdate;
  Result := FStringList.Count;
end;

function TQtListStrings.Get(Index: Integer): string;
begin
  if FListChanged then InternalUpdate;
  if Index < FStringList.Count then
     Result := FStringList.Strings[Index]
  else Result := '';
end;

{------------------------------------------------------------------------------
  Method: TQtListStrings.Create
  Params:  Qt Widget Handle and Lazarus WinControl Parent Object
  Returns: Nothing

  Contructor for the class.
 ------------------------------------------------------------------------------}
constructor TQtListStrings.Create(ListWidgetH: QListWidgetH; TheOwner: TWinControl);
begin
  inherited Create;

  {$ifdef VerboseQt}
    if (ListWidgetH = nil) then WriteLn('TQtMemoStrings.Create Unspecified ListWidgetH widget');
    if (TheOwner = nil) then WriteLn('TQtMemoStrings.Create Unspecified owner');
  {$endif}

  FStringList := TStringList.Create;
  FQtListWidget := ListWidgetH;
  FStringList.Text := TCustomListBox(TheOwner).Items.Text;
  FOwner:=TheOwner;
end;

destructor TQtListStrings.Destroy;
begin
  Clear;
  FStringList.Free;
  inherited Destroy;
end;

procedure TQtListStrings.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
end;

procedure TQtListStrings.Clear;
begin
  FUpdating := True;
  FStringList.Clear;
  
  if not (csDestroying in FOwner.ComponentState)
  and not (csFreeNotification in FOwner.ComponentState)
  then
    QListWidget_clear(FQtListWidget);
    
  FListChanged := False;
  FUpdating := False;
  IsChanged;
end;

procedure TQtListStrings.Delete(Index: integer);
begin
  if FListChanged then InternalUpdate;

  if Index < FStringList.Count then
  begin
    FStringList.Delete(Index);
    FUpdating := True;
    QListWidget_takeItem(FQtListWidget, Index);
    FUpdating := False;
    IsChanged;
    FUpdating := False;
    FListChanged := False;
  end;
end;

procedure TQtListStrings.Insert(Index: integer; const S: string);
var
   AStr: WideString;
   AItem: QListWidgetItemH;
begin
  if FListChanged then InternalUpdate;

  if Index < 0 then Index := 0;

  if Index <= FStringList.Count then
  begin
    FUpdating := True;
    FStringList.Insert(Index,S);
    AStr := UTF8Decode(S);
    
    AItem := QListWidgetItem_create(@AStr, FQtListWidget, Integer(QListWidgetItemType));
    
    if FOwner is TCustomCheckListBox then
      QListWidgetItem_setCheckState(AItem, QtUnchecked);
      
    QListWidget_insertItem(FQtListWidget, Index, AItem);
    FUpdating := False;
    IsChanged;
    FUpdating := False;
    FListChanged := False;
  end;
end;

procedure TQtListStrings.SetText(TheText: PChar);
begin
  inherited SetText(TheText);
end;

{ TQtMemoStrings }

{------------------------------------------------------------------------------
  Private Method: TQtMemoStrings.InternalUpdate
  Params:  None
  Returns: Nothing

  Updates internal StringList from Qt Widget
 ------------------------------------------------------------------------------}
Procedure TQtMemoStrings.InternalUpdate;
var
  Astr: WideString;
begin
  QTextEdit_toPlainText(FQtTextEdit,@Astr); // get the memo content
  FStringList.Text := Astr;
  FTextChanged := False;
end;

{------------------------------------------------------------------------------
  Private Method: TQtMemoStrings.ExternalUpdate
  Params:  Astr: Text for Qt Widget; Clear: if we must clear first
  Returns: Nothing

  Updates Qt Widget from text - If DelphiOnChange, generates OnChange Event
 ------------------------------------------------------------------------------}
procedure TQtMemoStrings.ExternalUpdate(var Astr: WideString; Clear: Boolean = True);
{var
  Mess: TLMessage;}
begin
  FUpdating := True;
  if Clear then
    QTextEdit_clear(FQtTextEdit);
  QTextEdit_append(FQtTextEdit,@Astr);
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

  Contructor for the class.
 ------------------------------------------------------------------------------}
constructor TQtMemoStrings.Create(TextEdit: QTextEditH; TheOwner: TWinControl);
var
  Method: TMethod;
  Hook : QTextEdit_hookH;
  Astr: WideString;
begin
  inherited Create;

  {$ifdef VerboseQt}
    if (TextEdit = nil) then WriteLn('TQtMemoStrings.Create Unspecified TextEdit widget');
    if (TheOwner = nil) then WriteLn('TQtMemoStrings.Create Unspecified owner');
  {$endif}

  FStringList := TStringList.Create;
  FQtTextEdit := TextEdit;
  QTextEdit_toPlainText(TextEdit,@Astr); // get the memo content
  FStringList.Text := Astr;
  FOwner:=TheOwner;

  // Callback Event
  {Method := MemoChanged;}
  TEventFilterMethod(Method) := TextChangedHandler;
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
  // just for debugging
  SenderView: QObjectH;
  EventView: QEventH;
begin
  if not FUpdating then begin
    SenderView := Sender;
    EventView := Event;
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
  if (Source=Self) or (Source=nil) then exit;
  if Source is TStrings then begin
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
{var
  Mess: TLMessage;}
begin
  FUpdating := True;
  FStringList.Clear;
  QTextEdit_clear(FQtTextEdit);
  FTextChanged := False;
  FUpdating := False;
  {FillChar(Mess, SizeOf(Mess), #0);
  FillChar(Mess, SizeOf(Mess), #0);
  Mess.Msg := CM_TEXTCHANGED;
  (FOwner as TCustomMemo).Modified := False;
  FOwner.Dispatch(TLMessage(Mess));}
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
  if Index < FStringList.Count then begin
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
  if Index <= FStringList.Count then begin
    FStringList.Insert(Index,S);
    Astr := FStringList.Text;
    ExternalUpdate(Astr,True);
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

procedure TQtComboStrings.InternalUpdate;
begin

end;

procedure TQtComboStrings.ExternalUpdate(var Astr: TStringList; Clear: Boolean);
var
  i: Integer;
  data: QVariantH;
  TmpStr: WideString;
begin

  data := QVariant_create(10); //Creates dummy data

  FUpdating := True;
  if Clear then
    QComboBox_clear(FQtComboBox);
  for i := 0 to AStr.Count -1 do
  begin
    TmpStr := UTF8Decode(Astr[i]);
    QComboBox_additem(FQtComboBox, @TmpStr, data);
  end;
  FUpdating := False;
  IsChanged;
  FUpdating := False;

  QVariant_destroy(data); // Clean up
end;

procedure TQtComboStrings.IsChanged;
begin

end;

function TQtComboStrings.GetTextStr: string;
begin
  Result:=inherited GetTextStr;
end;

function TQtComboStrings.GetCount: integer;
begin
  if FComboChanged then InternalUpdate;
  Result := FStringList.Count;
end;

function TQtComboStrings.Get(Index: Integer): string;
begin
  if FComboChanged then InternalUpdate;
  if Index < FStringList.Count then
     Result := FStringList.Strings[Index]
  else Result := '';
end;

constructor TQtComboStrings.Create(ComboBoxH: QComboBoxH; TheOwner: TWinControl);
var
  AQList: QStringListH;
  i: Integer;
  Str: WideString;
begin
  inherited Create;

  {$ifdef VerboseQt}
    if (ComboBoxH = nil) then WriteLn('TQtComboStrings.Create Unspecified ComboBoxH widget');
    if (TheOwner = nil) then WriteLn('TQtComboStrings.Create Unspecified owner');
  {$endif}

  FStringList := TStringList.Create;
  FQtComboBox := ComboBoxH;
  FStringList.Text := TCustomComboBox(TheOwner).Items.Text;

  AQList := QStringList_create;
  
  try
    for i := 0 to FStringList.Count - 1 do
    begin
      Str := UTF8Encode(FStringList.Strings[i]);
      QStringList_append(AQList, @Str);
    end;
    
    QComboBox_addItems(FQtComboBox, AQList);
    
  finally
    QStringList_destroy(AQList);
  end;
  
  FOwner := TheOwner;
end;

destructor TQtComboStrings.Destroy;
begin
  Clear;
  FStringList.Free;
  inherited Destroy;
end;

procedure TQtComboStrings.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
end;

procedure TQtComboStrings.Clear;
begin
  FUpdating := True;
  FStringList.Clear;
  if not (csDestroying in FOwner.ComponentState)
  and not (csFreeNotification in FOwner.ComponentState)
  then
    QComboBox_clear(FQtComboBox);
  FComboChanged := False;
  FUpdating := False;
  IsChanged;
end;

procedure TQtComboStrings.Delete(Index: integer);
begin
  if FComboChanged then InternalUpdate;

  if Index < FStringList.Count then
  begin
    FStringList.Delete(Index);
    QComboBox_removeItem(FQtComboBox, Index);
    FUpdating := False;
    IsChanged;
    FUpdating := False;
    FComboChanged:=False;
  end;
end;

procedure TQtComboStrings.Insert(Index: integer; const S: string);
var
  Str: WideString;
  data: QVariantH;
begin
  if FComboChanged then InternalUpdate;

  if Index < 0 then Index := 0;

  if Index <= FStringList.Count then
  begin
    FStringList.Insert(Index, S);
    Str := UTF8Encode(S);
    data := QVariant_create(0); //Creates dummy data
    try
      QComboBox_insertItem(FQtComboBox, Index, @Str, Data);
    finally
      QVariant_destroy(data);
    end;
    FUpdating := False;
    IsChanged;
    FUpdating := False;
    FComboChanged:=False;
  end;
end;

procedure TQtComboStrings.SetText(TheText: PChar);
begin
  inherited SetText(TheText);
end;

end.
