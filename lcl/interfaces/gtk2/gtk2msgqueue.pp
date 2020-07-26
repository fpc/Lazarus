{***************************************************************************
                  GtkMsgQueue - Messagequeue for Gtk interface
                  --------------------------------------------

                   Initial Revision  : Thu Aug 16, 2003


 ***************************************************************************/

 *****************************************************************************
  This file is part of the Lazarus Component Library (LCL)

  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************
}
unit Gtk2MsgQueue;

{$mode objfpc}{$H+}

interface

uses
  // RTL
  Classes,
  // LCL
  LazLinkedList, LCLType, LMessages, Gtk2Globals, Gtk2Proc,
  // LazUtils
  DynHashArray
{$IFNDEF USE_GTK_MAIN_OLD_ITERATION}
, glib2
{$ENDIF}
;

type
  TFinalPaintMessageFlag=(FPMF_None,FPMF_Internal,FPMF_All);

  TGtkMessageQueueItem=class(TLinkListitem)
  private
    fMsg : PMsg;
  public
    property Msg: PMsg read fMsg write fMsg;
    function IsPaintMessage: Boolean;
    procedure DestroyMessage(ParFinalInternalOnly: TFinalPaintMessageFlag;
                             DisposeMessage: boolean);
    constructor Create;
  end;

  { TGtkMessageQueue }

  TGtkMessageQueue=class(TLinkList)
  private
    FPaintMessages: TDynHashArray; // Hash for paint messages
    FCritSec: TRTLCriticalSection;
    {$IFNDEF USE_GTK_MAIN_OLD_ITERATION}
    FMainContext: PGMainContext;
    {$ELSE}
    fLock: integer;
    {$ENDIF}
  protected
    function CreateItem : TLinkListItem;override;
    function CalculateHash(ParWnd : Hwnd):integer;
    function HashPaintMessage(p: pointer): integer;
  public
    constructor Create;
    destructor destroy;override;
    procedure  Lock;
    procedure  UnLock;
    function   FirstMessageItem: TGtkMessageQueueItem;
    function   LastMessageItem: TGtkMessageQueueItem;
    function   FirstMessage: PMsg;
    function   LastMessage: PMsg;
    procedure  AddMessage(ParMsg: PMsg);
    procedure  RemoveMessage(ParItem: TGtkMessageQueueItem;
                             ParFinalOnlyInternal: TFinalPaintMessageFlag;
                             DisposeMessage: boolean);
    function   FindPaintMessage(HandleWnd: HWnd): TGtkMessageQueueItem;
    function   HasPaintMessages:boolean;
    function   HasNonPaintMessages:boolean;
    function   NumberOfPaintMessages:integer;
    function   PopFirstMessage: PMsg;
    {$IFNDEF USE_GTK_MAIN_OLD_ITERATION}
    property   MainContext: PGMainContext read FMainContext;
    {$ENDIF}
  end;


implementation

{---(TGtkMessageQueueItem)----------------------}

function TGtkMessageQueueItem.IsPaintMessage: Boolean;
begin
  if fMsg <> nil then
    Result := (Msg^.Message = LM_PAINT) or (Msg^.Message = LM_GTKPAINT)
  else
    Result := False;
end;

constructor TGtkMessageQueueItem.Create;
begin
  inherited Create;
  fMsg := nil;
end;

procedure TGtkMessageQueueItem.DestroyMessage(
  ParFinalInternalOnly: TFinalPaintMessageFlag; DisposeMessage: boolean);
begin
  if (ParFinalInternalOnly in [FPMF_All, FPMF_Internal])
  and (fMsg^.message = LM_GTKPAINT)
  then
    FinalizePaintTagMsg(fMsg);
  if DisposeMessage then
    Dispose(fMsg);
  fMsg := nil;
end;

{---(TGtkMessageQueue )---------------------------}

constructor TGtkMessageQueue.Create;
begin
  inherited Create;
  FPaintMessages := TDynHashArray.Create(-1);
  FPaintMessages.OwnerHashFunction := @HashPaintMessage;
  InitCriticalSection(FCritSec);
  {$IFNDEF USE_GTK_MAIN_OLD_ITERATION}
  FMainContext := g_main_context_new;
  g_main_context_ref(FMainContext);
  {$ENDIF}
end;

destructor TGtkMessageQueue.destroy;
begin
  inherited Destroy;
  fPaintMessages.destroy;
  {$IFNDEF USE_GTK_MAIN_OLD_ITERATION}
  g_main_context_unref(FMainContext);
  FMainContext := nil;
  {$ENDIF}
  DoneCriticalsection(FCritSec);
end;

procedure TGtkMessageQueue.Lock;
begin
  {$IFDEF USE_GTK_MAIN_OLD_ITERATION}
  if InterlockedIncrement(fLock)=1 then
    EnterCriticalsection(FCritSec);
  {$ELSE}
  if GetCurrentThreadId = MainThreadID then
    repeat
    until g_main_context_acquire(FMainContext)  // This can return False.
  else
    EnterCriticalsection(FCritSec);
  {$ENDIF}
end;

procedure TGtkMessageQueue.UnLock;
begin
  {$IFDEF USE_GTK_MAIN_OLD_ITERATION}
  if InterlockedDecrement(fLock)=0 then
    LeaveCriticalsection(FCritSec);
  {$ELSE}
  if GetCurrentThreadId = MainThreadID then
    g_main_context_release(FMainContext)
  else
    LeaveCriticalsection(FCritSec)
  {$ENDIF}
end;

{------------------------------------------------------------------------------
  Function: FindPaintMessage
  Params: a window handle
  Returns: nil or a Paint Message to the widget

  Searches in FPaintMessages for a LM_PAINT message with HandleWnd.
 ------------------------------------------------------------------------------}
function TGtkMessageQueue.FindPaintMessage(HandleWnd: HWnd): TGtkMessageQueueItem;
var h: integer;
  HashItem: PDynHashArrayItem;
begin
  h:= CalculateHash(HandleWnd);
  HashItem:=FPaintMessages.GetHashItem(h);
  if HashItem<>nil then begin
    Result:=TGtkMessageQueueItem(HashItem^.Item);
    if Result.Msg^.hWnd=HandleWnd then
      exit;
    HashItem:=HashItem^.Next;
    while (HashItem<>nil) and (HashItem^.IsOverflow) do begin

      Result:=TGtkMessageQueueItem(HashItem^.Item);
      if Result.Msg^.hWnd=HandleWnd then
        exit;
      HashItem:=HashItem^.Next;

    end;
  end;
  Result:=nil;
end;


function TGtkMessageQueue.HashPaintMessage(p: pointer): integer;
begin
  result := CalculateHash(TGtkMessageQueueItem(p).Msg^.Hwnd);
end;

function TGtkMessageQueue.CalculateHash(ParWnd : Hwnd):integer;
var
  h:integer;
begin
  h :=ParWnd;
  if h<0 then h:=-h;
  Result:=((h mod 5364329)+(h mod 17)) mod FPaintMessages.Capacity;
end;

function TGtkMessageQueue.CreateItem : TLinkListItem;
begin
  result := TGtkMessageQueueItem.Create;
  result.ResetItem;
end;

procedure TGtkMessageQueue.AddMessage(ParMsg : PMsg);
var
  vLItem : TGtkMessageQueueItem;
begin
  vlItem := TGtkMessageQueueItem(GetNewItem);
  vlItem.fMsg := ParMsg;
  AddAsLast(vlItem);
  if vlItem.IsPaintMessage then fPaintMessages.Add(vlitem);
end;

function TGtkMessageQueue.FirstMessageItem : TGtkMessageQueueItem;
begin
  Lock;
  try
    Result :=TGtkMessageQueueItem(First);
  finally
    UnLock;
  end;
end;

function TGtkMessageQueue.FirstMessage : PMsg;
begin
  Result := nil;
  Lock;
  try
    if FirstMessageItem <> nil then  Result := FirstMessageItem.fMsg;
  finally
    UnLock;
  end;
end;

function TGtkMessageQueue.LastMessageItem : TGtkMessageQueueItem;
begin
  Lock;
  try
    Result:=TGtkMessageQueueItem(Last);
  finally
    UnLock;
  end;
end;

function TGtkMessageQueue.LastMessage : PMsg;
begin
  Lock;
  try
    Result := nil;
    if LastMessageItem <> nil then   result := LastMessageItem.fMsg;
  finally
    UnLock;
  end;
end;

{ Remove from queue and destroy message
  ParItem         : Queue Item for removel
  ParFinalOnlyInterl : finalyze message only for LM_GtkPaint }
procedure  TGtkMessageQueue.RemoveMessage(ParItem: TGtkMessageQueueItem;
  ParFinalOnlyInternal: TFinalPaintMessageFlag; DisposeMessage: boolean);
begin
  Lock;
  try
    if (ParItem.IsPaintMessage) then
      fPaintMessages.Remove(ParItem);
    ParItem.DestroyMessage(ParFinalOnlyInternal, DisposeMessage);
    Delete(ParItem);
  finally
    UnLock;
  end;
end;

function TGtkMessageQueue.HasPaintMessages:boolean;
begin
  Lock;
  try
    Result := fPaintMessages.Count > 0;
  finally
    UnLock;
  end;
end;

function TGtkMessageQueue.NumberOfPaintMessages:integer;
begin
  Lock;
  try
    Result := fPaintMessages.Count;
  finally
    UnLock;
  end;
end;

function TGtkMessageQueue.HasNonPaintMessages:boolean;
begin
  Lock;
  try
    Result := fPaintMessages.Count <> count;
  finally
    UnLock;
  end;
end;

function TGtkMessageQueue.PopFirstMessage: PMsg;
var
  vlItem : TGtkMessageQueueItem;
begin
  Lock;
  try
    vlItem := FirstMessageItem;
    if vlItem <> nil then
    begin
      Result := vlItem.Msg;
      RemoveMessage(vlItem,FPMF_none,false);
    end else
      Result := nil;
  finally
    UnLock;
  end;
end;

end.

