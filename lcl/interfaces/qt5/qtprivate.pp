{
 *****************************************************************************
 *                              QtPrivate.pp                                 *
 *                              --------------                               *
 *                                                                           *
 *                                                                           *
 *****************************************************************************

 *****************************************************************************
  This file is part of the Lazarus Component Library (LCL)

  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************
}
unit qtprivate;

{$mode objfpc}{$H+}

interface

{$I qtdefines.inc}

// todo: check if build macros can handle this and define it
// in package LCL instead (it's needed for several widgetsets)
{$if (FPC_FULLVERSION>=20701) OR (FPC_FULLVERSION>=20603)}
{$define HAS_INHERITED_INSERTITEM}
{$endif}

uses
  // Bindings
  qt5,
  // Free Pascal
  Classes, SysUtils,
  // LCL
  Forms, Controls, LCLType, LazUTF8, ExtCtrls, StdCtrls, LazUtf8Classes,
  //Widgetset
  QtWidgets, qtproc;

type

  { TQtComboStrings }

  TQtComboStrings = class(TStringList)
  private
    FSorted: Boolean;
    FWinControl: TWinControl;
    FOwner: TQtComboBox;
    FChanging: boolean;
    procedure SetSorted(AValue: Boolean);
  protected
    procedure Put(Index: Integer; const S: string); override;
    {$IFNDEF HAS_INHERITED_INSERTITEM}
    procedure InsertItem(Index: Integer; const S: string); override;
    {$ENDIF}
    procedure InsertItem(Index: Integer; const S: string; O: TObject); override;
  public
    constructor Create(AWinControl: TWinControl; AOwner: TQtComboBox);
    destructor Destroy; override;
    function Add(const S: String): Integer; override;
    procedure Assign(Source: TPersistent); override;
    procedure Clear; override;
    procedure Delete(Index: Integer); override;
    function Find(const S: String; out Index: Integer): Boolean;
    function IndexOf(const S: String): Integer; override;
    procedure Insert(Index: Integer; const S: String); override;
    procedure Sort; override;
    procedure Exchange(AIndex1, AIndex2: Integer); override;
  public
    property Owner: TQtComboBox read FOwner;
    property Sorted: Boolean read FSorted write SetSorted;
  end;


  { TQtListStrings }

  TQtListStrings = class(TStringList)
  private
    FWinControl: TWinControl;
    FOwner: TQtListWidget;
  protected
    procedure Put(Index: Integer; const S: string); override;
    {$IFNDEF HAS_INHERITED_INSERTITEM}
    procedure InsertItem(Index: Integer; const S: string); override;
    {$ENDIF}
    procedure InsertItem(Index: Integer; const S: string; O: TObject); override;
  public
    constructor Create(AWinControl: TWinControl; AOwner: TQtListWidget);
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure Clear; override;
    procedure Delete(Index: Integer); override;
    procedure Move(CurIndex, NewIndex: Integer); override;
    procedure Sort; override;
    procedure Exchange(AIndex1, AIndex2: Integer); override;
  public
    property Owner: TQtListWidget read FOwner;
  end;

  { TQtMemoStrings }

  TQtMemoStrings = class(TStrings)
  private
    FTextChanged: Boolean; // Inform TQtMemoStrings about change in TextChange event
    FStringList: TStringList; // Holds the lines to show
    FHasTrailingLineBreak: Boolean; // Indicates whether lines have trailing line break
    FOwner: TWinControl;      // Lazarus Control Owning MemoStrings
    procedure InternalUpdate;
    procedure ExternalUpdate(var AStr: WideString;
      AClear, ABlockSignals: Boolean);
    function GetInternalText: string;
    procedure SetInternalText(const Value: string);
  protected
    function GetTextStr: string; override;
    function GetCount: integer; override;
    function Get(Index : Integer) : string; override;
    procedure Put(Index: Integer; const S: string); override;
    procedure SetTextStr(const Value: string); override;
  public
    constructor Create(TheOwner: TWinControl);
    destructor Destroy; override;
    procedure Assign(Source : TPersistent); override;
    procedure Clear; override;
    procedure Delete(Index : integer); override;
    procedure Insert(Index : integer; const S: string); override;
    procedure LoadFromFile(const FileName: string); override;
    procedure SaveToFile(const FileName: string); override;
  public
    property Owner: TWinControl read FOwner;
    property TextChanged: Boolean read FTextChanged write FTextChanged;
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
  TextEdit: TQtTextEdit;
begin
  W := '';
  if FOwner.HandleAllocated then
  begin
    TextEdit := TQtTextEdit(FOwner.Handle);
    W := TextEdit.getText;
  end;
  if W <> '' then
    SetInternalText(UTF16ToUTF8(W))
  else
    SetInternalText('');
  FTextChanged := False;
end;

{------------------------------------------------------------------------------
  Private Method: TQtMemoStrings.ExternalUpdate
  Params:  AStr: Text for Qt Widget; Clear: if we must clear first
           ABlockSignals: block SignalTextChanged() so it does not send an
           message to LCL.
  Returns: Nothing

  Updates Qt Widget from text - If DelphiOnChange, generates OnChange Event
 ------------------------------------------------------------------------------}
procedure TQtMemoStrings.ExternalUpdate(var AStr: WideString;
  AClear, ABlockSignals: Boolean);
var
  W: WideString;
  TextEdit: TQtTextEdit;
begin
  if not FOwner.HandleAllocated then
    exit;
  {$ifdef VerboseQtMemoStrings}
  writeln('TQtMemoStrings.ExternalUpdate');
  {$endif}
  TextEdit := TQtTextEdit(FOwner.Handle);
  if ABlockSignals then
    TextEdit.BeginUpdate;
  W := GetUtf8String(AStr);
  if AClear then
  begin
    // never trigger changed signal when clearing text here.
    // we must clear text since QTextEdit can contain html text.
    TextEdit.BeginUpdate;
    TextEdit.ClearText;
    TextEdit.EndUpdate;
    TextEdit.setText(W);
  end else
    TextEdit.Append(W);

  if TextEdit.getAlignment <> AlignmentMap[TCustomMemo(FOwner).Alignment] then
    TextEdit.setAlignment(AlignmentMap[TCustomMemo(FOwner).Alignment]);
  if ABlockSignals then
    TextEdit.EndUpdate;
end;

function TQtMemoStrings.GetInternalText: string;
var
  TextLen: Integer;
begin
  Result := FStringList.Text;

  // Since TStringList.Text automatically adds line break to the last line,
  // we should remove it if original text does not contain it
  if not FHasTrailingLineBreak then
  begin
    TextLen := Length(Result);
    if (TextLen > 0) and (Result[TextLen] = #10) then
      Dec(TextLen);
    if (TextLen > 0) and (Result[TextLen] = #13) then
      Dec(TextLen);
    SetLength(Result, TextLen);
  end;
end;

procedure TQtMemoStrings.SetInternalText(const Value: string);
var
  TextLen: Integer;
begin
  TextLen := Length(Value);
  FHasTrailingLineBreak := (TextLen > 0) and (Value[TextLen] in [#13, #10]);
  FStringList.Text := Value;
end;

{------------------------------------------------------------------------------
  Method: TQtMemoStrings.GetTextStr
  Params:  None
  Returns: a string

  Return the whole StringList content as a single string
 ------------------------------------------------------------------------------}
function TQtMemoStrings.GetTextStr: string;
begin
  {$ifdef VerboseQtMemoStrings}
  WriteLn('TQtMemoStrings.GetTextStr');
  {$endif}
  if FTextChanged then InternalUpdate;
  Result := GetInternalText;
end;

{------------------------------------------------------------------------------
  Method: TQtMemoStrings.GetCount
  Params:  None
  Returns: an integer

  Return the current number of strings
 ------------------------------------------------------------------------------}
function TQtMemoStrings.GetCount: integer;
begin
  {$ifdef VerboseQtMemoStrings}
  WriteLn('TQtMemoStrings.GetCount');
  {$endif}
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
  {$ifdef VerboseQtMemoStrings}
  WriteLn('TQtMemoStrings.Get Index=',Index);
  {$endif}
  if FTextChanged then InternalUpdate;
  if Index < FStringList.Count then
    Result := FStringList.Strings[Index]
  else
    Result := '';
end;

procedure TQtMemoStrings.Put(Index: Integer; const S: string);
var
  W: WideString;
begin
  {$ifdef VerboseQtMemoStrings}
  WriteLn('TQtMemoStrings.Put Index=',Index,' S=',S);
  {$endif}
  if FTextChanged then InternalUpdate;
  FStringList[Index] := S;
  W := GetUTF8String(S);
  TQtTextEdit(FOwner.Handle).setLineText(Index, W);
end;

procedure TQtMemoStrings.SetTextStr(const Value: string);
var
  W: WideString;
begin
  {$ifdef VerboseQtMemoStrings}
  WriteLn('TQtMemoStrings.SetTextStr Value=',Value);
  {$endif}
  SetInternalText(Value);
  W := GetInternalText;
  ExternalUpdate(W, True, False);
  FTextChanged := False;
end;

{------------------------------------------------------------------------------
  Method: TQtMemoStrings.Create
  Params:  Qt Widget Handle and Lazarus WinControl Parent Object
  Returns: Nothing

  Constructor for the class.
 ------------------------------------------------------------------------------}
constructor TQtMemoStrings.Create(TheOwner: TWinControl);
begin
  inherited Create;
  {$ifdef VerboseQt}
  if (TheOwner = nil) then
    WriteLn('TQtMemoStrings.Create Unspecified owner');
  {$endif}
  FStringList := TStringList.Create;
  FHasTrailingLineBreak := False;
  FOwner := TheOwner;
end;

{------------------------------------------------------------------------------
  Method: TQtMemoStrings.Destroy
  Params:  None
  Returns: Nothing

  Destructor for the class.
 ------------------------------------------------------------------------------}
destructor TQtMemoStrings.Destroy;
begin
  FStringList.Free;
  FOwner := nil;
  inherited Destroy;
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
  if (Source=Self) or (Source=nil) then
    exit;
  if not FOwner.HandleAllocated then
    exit;

  if Source is TStrings then
  begin
    {$ifdef VerboseQtMemoStrings}
    writeln('TQtMemoStrings.Assign - handle ? ', FOwner.HandleAllocated);
    {$endif}
    FStringList.Clear;
    SetInternalText(TStrings(Source).Text);
    W := GetInternalText;
    ExternalUpdate(W, True, False);
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
begin
  if not Assigned(FOwner) then
    exit;
  if Assigned(FStringList) then
    FStringList.Clear;
  if not (csDestroying in FOwner.ComponentState) and
    not (csFreeNotification in FOwner.ComponentState) and
    FOwner.HandleAllocated then
  begin
    {$ifdef VerboseQtMemoStrings}
    writeln('TQtMemoStrings.Clear');
    {$endif}
    TQtTextEdit(FOwner.Handle).BeginUpdate;
    TQtTextEdit(FOwner.Handle).ClearText;
    TQtTextEdit(FOwner.Handle).EndUpdate;
    FTextChanged := False;
  end;
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
    {$ifdef VerboseQtMemoStrings}
    writeln('TQtMemoStrings.Delete');
    {$endif}
    FStringList.Delete(Index);
    TQtTextEdit(FOwner.Handle).RemoveLine(Index);
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

  {$ifdef VerboseQtMemoStrings}
  writeln('TQtMemoStrings.Insert Index=',Index,' COUNT=',FStringList.Count);
  {$endif}

  // simplified because of issue #29670
  // allow insert invalid index like others do
  if Index >= FStringList.Count then
    Index := FStringList.Add(S)
  else
    FStringList.Insert(Index, S);
  W := GetUTF8String(S);
  TQtTextEdit(FOwner.Handle).insertLine(Index, W);
  FTextChanged := False; // FStringList is already updated, no need to update from WS.
end;

procedure TQtMemoStrings.LoadFromFile(const FileName: string);
var
  TheStream: TFileStreamUTF8;
begin
  TheStream:=TFileStreamUtf8.Create(FileName,fmOpenRead or fmShareDenyWrite);
  try
    LoadFromStream(TheStream);
  finally
    TheStream.Free;
  end;
end;

procedure TQtMemoStrings.SaveToFile(const FileName: string);
var
  TheStream: TFileStreamUTF8;
begin
  TheStream:=TFileStreamUtf8.Create(FileName,fmCreate);
  try
    SaveToStream(TheStream);
  finally
    TheStream.Free;
  end;
end;

{ TQtComboStrings }

procedure TQtComboStrings.SetSorted(AValue: Boolean);
var
  i: Integer;
begin
  if FSorted=AValue then Exit;
  FSorted:=AValue;
  if not FSorted then Exit;

  for i := 0 to Count - 2 do
  begin
    if UTF8CompareText(Strings[i], Strings[i + 1]) < 0 then
    begin
      Sort;
      Break;
    end;
  end;
end;

procedure TQtComboStrings.Put(Index: Integer; const S: string);
begin
  inherited Put(Index, S);
  FOwner.BeginUpdate;
  FOwner.setItemText(Index, S);
  FOwner.EndUpdate;
end;

{$IFNDEF HAS_INHERITED_INSERTITEM}
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
{$ENDIF}

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
  end else
    FOwner.setCurrentIndex(FSavedIndex);
  FOwner.EndUpdate;
end;

constructor TQtComboStrings.Create(AWinControl: TWinControl;
    AOwner: TQtComboBox);
begin
  inherited Create;
  FWinControl := AWinControl;
  FOwner := AOwner;
  FSorted := TComboBox(AOwner.LCLObject).Sorted;
  FChanging := False;
end;

destructor TQtComboStrings.Destroy;
begin
  FWinControl := nil;
  inherited Destroy;
end;

function TQtComboStrings.Add(const S: String): Integer;
begin
  if FSorted then
  begin
    Find(S, Result);
    FChanging := True;
    Insert(Result, S);
    FChanging := False;
  end else
    Result := inherited Add(S);
end;

procedure TQtComboStrings.Assign(Source: TPersistent);
var
  AList: TStringList;
begin
  if (Source = Self) or (Source = nil) then Exit;
  if Assigned(FWinControl) and (FWinControl.HandleAllocated) then
  begin
    FOwner.BeginUpdate;
    if Sorted then
    begin
      AList := TStringList.Create;
      try
        AList.Assign(Source);
        AList.Sort;
        inherited Assign(AList);
      finally
        AList.Free;
      end;
    end else
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

function TQtComboStrings.Find(const S: String; out Index: Integer): Boolean;
var
  L, R, I: Integer;
  CompareRes: PtrInt;
begin
  Result := False;
  // Use binary search.
  L := 0;
  R := Count - 1;
  while (L <= R) do
  begin
    I := L + (R - L) div 2;
    CompareRes := UTF8CompareText(S, Strings[I]);
    if (CompareRes > 0) then
      L := I + 1
    else
    begin
      R := I - 1;
      if (CompareRes = 0) then
      begin
        Result := True;
        L := I; // forces end of while loop
      end;
    end;
  end;
  Index := L;
end;

function TQtComboStrings.IndexOf(const S: String): Integer;
begin
  Result := -1;
  if FSorted then
  begin
    //Binary Search
    if not Find(S, Result) then
      Result := -1;
  end else
    Result := inherited IndexOf(S);
end;

procedure TQtComboStrings.Insert(Index: Integer; const S: String);
begin
  if FSorted and not FChanging then
  begin
    inherited Insert(Index, S);
    Sort;
  end else
    inherited Insert(Index, S);
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
  begin
    FOwner.BeginUpdate;
    FOwner.setItemText(Index, S);
    if FOwner is TQtCheckListBox then
    begin
      FOwner.ItemFlags[Index] := FOwner.ItemFlags[Index] or QtItemIsUserCheckable;
      if TQtCheckListBox(FOwner).AllowGrayed then
        FOwner.ItemFlags[Index] := FOwner.ItemFlags[Index] or QtItemIsTristate
      else
        FOwner.ItemFlags[Index] := FOwner.ItemFlags[Index] and not QtItemIsTristate;
    end;
    FOwner.EndUpdate;
  end;
end;
{$IFNDEF HAS_INHERITED_INSERTITEM}
procedure TQtListStrings.InsertItem(Index: Integer; const S: string);
begin
  inherited InsertItem(Index, S);
  if Assigned(FWinControl) and (FWinControl.HandleAllocated) then
  begin
    FOwner.BeginUpdate;
    FOwner.insertItem(Index, S);
    if FOwner is TQtCheckListBox then
    begin
      FOwner.ItemFlags[Index] := FOwner.ItemFlags[Index] or QtItemIsUserCheckable;
      if TQtCheckListBox(FOwner).AllowGrayed then
        FOwner.ItemFlags[Index] := FOwner.ItemFlags[Index] or QtItemIsTristate
      else
        FOwner.ItemFlags[Index] := FOwner.ItemFlags[Index] and not QtItemIsTristate;
    end;
    FOwner.EndUpdate;
  end;
end;
{$ENDIF}
procedure TQtListStrings.InsertItem(Index: Integer; const S: string; O: TObject);
begin
  inherited InsertItem(Index, S, O);
  if Assigned(FWinControl) and (FWinControl.HandleAllocated) then
  begin
    FOwner.BeginUpdate;
    FOwner.insertItem(Index, S);

    if FOwner is TQtCheckListBox then
    begin
      FOwner.ItemFlags[Index] := FOwner.ItemFlags[Index] or QtItemIsUserCheckable;
      if TQtCheckListBox(FOwner).AllowGrayed then
        FOwner.ItemFlags[Index] := FOwner.ItemFlags[Index] or QtItemIsTristate
      else
        FOwner.ItemFlags[Index] := FOwner.ItemFlags[Index] and not QtItemIsTristate;
    end;
    FOwner.EndUpdate;
  end;
end;

constructor TQtListStrings.Create(AWinControl: TWinControl;
  AOwner: TQtListWidget);
begin
  inherited Create;
  FWinControl := AWinControl;
  FOwner := AOwner;
end;

destructor TQtListStrings.Destroy;
begin
  FWinControl := nil;
  inherited Destroy;
end;

procedure TQtListStrings.Assign(Source: TPersistent);
var
  i: Integer;
begin
  if Assigned(FWinControl) and (FWinControl.HandleAllocated) then
  begin
    FOwner.BeginUpdate;
    inherited Assign(Source);
    if FOwner is TQtCheckListBox then
    begin
      for i := 0 to TQtCheckListBox(FOwner).ItemCount - 1 do
      begin
        FOwner.ItemFlags[i] := FOwner.ItemFlags[i] or QtItemIsUserCheckable;
        if TQtCheckListBox(FOwner).AllowGrayed then
          FOwner.ItemFlags[i] := FOwner.ItemFlags[i] or QtItemIsTristate
        else
          FOwner.ItemFlags[i] := FOwner.ItemFlags[i] and not QtItemIsTristate;
      end;
    end;
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
  begin
    FOwner.BeginUpdate;
    FOwner.removeItem(Index);
    FOwner.EndUpdate;
  end;
end;

procedure TQtListStrings.Move(CurIndex, NewIndex: Integer);
var
  CheckState: QtCheckState;
  Selected: Boolean;
begin
  {move is calling delete, and then insert.
   we must save our item checkstate and selection}
  if Assigned(FWinControl) and (FWinControl.HandleAllocated) and
    (FOwner is TQtCheckListBox) then
  begin
    CheckState := TQtCheckListBox(FOwner).ItemCheckState[CurIndex];
    Selected := TQtCheckListBox(FOwner).Selected[CurIndex];
  end;

  inherited Move(CurIndex, NewIndex);

  {return check state to newindex}
  if Assigned(FWinControl) and (FWinControl.HandleAllocated) and
    (FOwner is TQtCheckListBox) then
  begin
    FOwner.BeginUpdate;
    TQtCheckListBox(FOwner).ItemCheckState[NewIndex] := CheckState;
    FOwner.Selected[NewIndex] := Selected;
    FOwner.EndUpdate;
  end;
end;

procedure TQtListStrings.Sort;
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

procedure TQtListStrings.Exchange(AIndex1, AIndex2: Integer);
var
  ARow: Integer;
begin
  inherited Exchange(AIndex1, AIndex2);
  if Assigned(FWinControl) and (FWinControl.HandleAllocated) then
  begin
    ARow := FOwner.currentRow;
    FOwner.BeginUpdate;
    FOwner.ExchangeItems(AIndex1, AIndex2);
    FOwner.setCurrentRow(ARow);
    FOwner.EndUpdate;
  end;
end;

end.
