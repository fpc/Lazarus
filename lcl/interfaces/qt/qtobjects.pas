unit qtobjects;

{$mode delphi}{$H+}

interface

uses Classes, StdCtrls, Controls, Graphics, SysUtils, qt4;

type

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

  { TQtImage }

  TQtImage = class(TObject)
  private
    Handle: QImageH;
  public
    constructor Create(vHandle: QImageH); overload;
    constructor Create(data: PByte; width: Integer; height: Integer; format: QImageFormat); overload;
    destructor Destroy; override;
  public
    function height: Integer;
    function width: Integer;
    function numBytes: Integer;
  end;

Implementation

uses qtprivate, LMessages;

{ TQtListStrings }

procedure TQtListStrings.InternalUpdate;
begin

end;

procedure TQtListStrings.ExternalUpdate(var Astr: TStringList; Clear: Boolean);
var
  i: Integer;
begin
  FUpdating := True;
  if Clear then
    QListWidget_clear(FQtListWidget);
  for i := 0 to AStr.Count -1 do
    QListWidget_additem(FQtListWidget, @WideString(Astr[i]));
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
var
  Method: TMethod;
  Hook : QListWidget_hookH;
//  Astr: WideString;
  i: Integer;
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
  QListWidget_clear(FQtListWidget);
  FListChanged := False;
  FUpdating := False;
  IsChanged;
end;

procedure TQtListStrings.Delete(Index: integer);
{var
  Astr: WideString;}
begin
  if FListChanged then InternalUpdate;
  
  if Index < FStringList.Count then
  begin
    FStringList.Delete(Index);
//    Astr := FStringList.Text;
    ExternalUpdate(FStringList,True);
    FListChanged := False;
  end;
  
(*  FStringList.Delete(Index);
  QListWidget_takeitem(FQtListWidget, Index); *)
end;

procedure TQtListStrings.Insert(Index: integer; const S: string);
{var
  Astr: WideString;}
begin
  if FListChanged then InternalUpdate;
  
  if Index < 0 then Index := 0;
  
  if Index <= FStringList.Count then
  begin
    FStringList.Insert(Index,S);
//    Astr := FStringList.Text;
    ExternalUpdate(FStringList,True);
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
var
  Mess: TLMessage;
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


{ TQtImage }

{------------------------------------------------------------------------------
  Method: TQtImage.Create

  Contructor for the class.
 ------------------------------------------------------------------------------}
constructor TQtImage.Create(vHandle: QImageH);
begin
  Handle := vHandle;
end;

{------------------------------------------------------------------------------
  Method: TQtImage.Create

  Contructor for the class.
 ------------------------------------------------------------------------------}
constructor TQtImage.Create(data: PByte; width: Integer; height: Integer; format: QImageFormat);
begin
  Handle := QImage_create(data, width, height, format);
end;

{------------------------------------------------------------------------------
  Method: TQtImage.Destroy
  Params:  None
  Returns: Nothing

  Destructor for the class.
 ------------------------------------------------------------------------------}
destructor TQtImage.Destroy;
begin
  if Handle <> nil then QImage_destroy(Handle);
  
  inherited Destroy;
end;

{------------------------------------------------------------------------------
  Method: TQtImage.height
  Params:  None
  Returns: The height of the image
 ------------------------------------------------------------------------------}
function TQtImage.height: Integer;
begin
  Result := QImage_height(Handle);
end;

{------------------------------------------------------------------------------
  Method: TQtImage.width
  Params:  None
  Returns: The width of the image
 ------------------------------------------------------------------------------}
function TQtImage.width: Integer;
begin
  Result := QImage_width(Handle);
end;

{------------------------------------------------------------------------------
  Method: TQtImage.numBytes
  Params:  None
  Returns: The number of bytes the image occupies in memory
 ------------------------------------------------------------------------------}
function TQtImage.numBytes: Integer;
begin
  Result := QImage_numBytes(Handle);
end;

end.

