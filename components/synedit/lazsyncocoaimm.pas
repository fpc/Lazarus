{
  SynEdit MacOS IME Handler:
  1. various IME are fully supported, such as Chinese/Japanese/Korean and DeadKeys
  2. MultiCarets supported
  3. GroupUndo or not are both fully supported

  in order to be compatible with MultiCarets:
  1. TSynEditUndoList.BeginBlock() cannot be used directly,
     only TSynEditUndoList.GroupUndo and TSynEditUndoList.ForceGroupEnd()
     can be combined to Undo
  2. InsertTextAtCaret() cannot be used directly, only ecChar Command can be used
  3. mabye the support for MultiSelections in MultiCaretsPlugin is not perfect.
     for example, Shift+Arrow can only expand the Selection of the Main Caret,
     but not other Carets
}

unit LazSynCocoaIMM;

{$mode objfpc}{$H+}

interface

uses
  CocoaPrivate,
  Classes, SysUtils,
  SynEditMiscClasses, LazSynIMMBase, SynEditKeyCmds, SynEditTextBase;

type
  { LazSynImeCocoa }

  LazSynImeCocoa = class( LazSynIme, ICocoaIMEControl )
  private
    _undoList: TSynEditUndoList;
    _IntermediateTextBeginPos: TPoint;
  public
    procedure IMESessionBegin;
    procedure IMESessionEnd;
    procedure IMEUpdateIntermediateText( var params: TCocoaIMEParameters );
    procedure IMEInsertFinalText( var params: TCocoaIMEParameters );
    function  IMEGetTextBound( var params: TCocoaIMEParameters ) : TRect;
  private
    procedure InsertTextAtCaret_CompatibleWithMultiCarets( var params: TCocoaIMEParameters ) ;
    procedure SelectText_CompatibleWithMultiCarets( var params: TCocoaIMEParameters );
    function calcBound( var params: TCocoaIMEParameters ) : TRect;
    function PosToPixels( const pos: TPoint ) : TPoint;
  public
    constructor Create(AOwner: TSynEditBase);
    destructor Destroy; override;
  end;

implementation

uses
  LCLType, LazUTF8, SynEdit, LazSynEditText;

procedure LazSynImeCocoa.IMESessionBegin;
begin
  if FriendEdit.ReadOnly then exit;
  DoIMEStarted;
end;

procedure LazSynImeCocoa.IMESessionEnd;
begin
  if FriendEdit.ReadOnly then exit;
  ViewedTextBuffer.RedoList.Clear;    // clear Intermediate Text redo items
  DoIMEEnded;
end;

{
  update IME Intermediate Text, Key function for IME:
  1. some IME do not have a popup window and rely on the Editor
     to display the Intermediate Text
  2. use selection to simulate Intermediate Text display in this implementation,
     may be better to use TSynEditMarkup (slightly complicated)
  3. it means completely cancel the IME session if Intermediate Text is empty
  4. it's First call of IMEUpdateIntermediateText or IMEInsertFinalText
     if isFirstCall=True
  5. eat some chars if eatAmount>0 (such as DeadKeys)
}
procedure LazSynImeCocoa.IMEUpdateIntermediateText( var params: TCocoaIMEParameters );
var
  groupUndoBefore: boolean;
begin
  if FriendEdit.ReadOnly
    then exit;

  // clear last Intermediate Text
  if not params.isFirstCall then
    FriendEdit.Undo;

  // length=0 means to completely cancel the IME session
  if params.textCharLength=0 then
    exit;

  // save caret pos
  _IntermediateTextBeginPos := FriendEdit.LogicalCaretXY;

  // in order to be compatible with MultiCarets,
  // TSynEditUndoList.BeginBlock() cannot be used directly,
  // only TSynEditUndoList.GroupUndo and TSynEditUndoList.ForceGroupEnd()
  // can be combined to Undo
  groupUndoBefore:= _undoList.GroupUndo;
  _undoList.GroupUndo:= true;
  _undoList.ForceGroupEnd;

  // need to eat some chars, such as DeadKeys
  if params.eatAmount<>0 then
    TSynEdit(FriendEdit).CommandProcessor(ecDeleteLastChar, #0, nil);

  // in order to be compatible with MultiCarets,
  // InsertTextAtCaret() cannot be used directly,
  // only ecChar Command can be used indirectly
  InsertTextAtCaret_CompatibleWithMultiCarets( params );
  SelectText_CompatibleWithMultiCarets( params );

  _undoList.GroupUndo:= groupUndoBefore;
end;

{
  insert IME Final Text, Key function for IME:
  1. called only when inputting via IME, otherwise handled by UTF8KeyPress()
  2. when the IME input is finished, either IMEUpdateIntermediateText(with empty text)
     is called, or IMEInsertFinalText(with final text) is called,
     NOT the both
  3. it's First call of IMEUpdateIntermediateText or IMEInsertFinalText
     if isFirstCall=True
  4. eat some chars if eatAmount>0 (such as DeadKeys)
}
procedure LazSynImeCocoa.IMEInsertFinalText( var params: TCocoaIMEParameters );
begin
  if FriendEdit.ReadOnly then exit;

  // clear Intermediate Text
  if not params.isFirstCall then
    FriendEdit.Undo;

  // need to eat some chars, such as DeadKeys
  if params.eatAmount<>0 then
    TSynEdit(FriendEdit).CommandProcessor( ecDeleteLastChar, #0, nil );

  InsertTextAtCaret_CompatibleWithMultiCarets( params );
end;

{
  calc Intermediate Text bound:
  1. return Intermediate Text bound when in IME inut state. it's possible
     to only get the bound of the Intermediate Text in a subrange
     (selectedStart and selectedLength)
  2. return caret pos when not in IME input state
  3. in Screen Pixels
}
function LazSynImeCocoa.IMEGetTextBound( var params: TCocoaIMEParameters ) : TRect;
begin
  if not params.isFirstCall then
    Result:= calcBound( params )
  else
    Result:= TRect.Create( Point(FriendEdit.CaretXPix,FriendEdit.CaretYPix), 0, FriendEdit.LineHeight );

  Result:= FriendEdit.ClientToScreen( Result );
end;


procedure LazSynImeCocoa.InsertTextAtCaret_CompatibleWithMultiCarets( var params: TCocoaIMEParameters );
var
  i: integer;
  c: integer;
  ch: TUTF8Char;
begin
  if params.textByteLength=0 then Exit;
  i:=1;
  while( i<=params.textByteLength ) do
  begin
    c:= Utf8CodePointLen( @params.text[i], params.textByteLength-i+1, false );
    ch:= Copy( params.text, i, c );
    TSynEdit(FriendEdit).CommandProcessor( ecChar, ch, nil );
    inc( i, c );
  end;
end;

procedure LazSynImeCocoa.SelectText_CompatibleWithMultiCarets( var params: TCocoaIMEParameters );
var
  i: Integer;
  start:  Integer;
  length: Integer;
begin
  if params.selectedLength=0 then begin
    start:= 0;
    length:= params.textCharLength;
  end else begin
    start:= params.selectedStart;
    length:= params.selectedLength;
  end;

  for i:=params.textCharLength-start downto 1 do TSynEdit(FriendEdit).CommandProcessor( ecLeft, #0, nil );
  for i:=length downto 1 do TSynEdit(FriendEdit).CommandProcessor( ecSelRight, #0, nil );
end;

function LazSynImeCocoa.calcBound( var params: TCocoaIMEParameters ) : TRect;
var
  p1: TPoint;
  p2: TPoint;
begin
  // two vertexs in bytes
  p1:= _IntermediateTextBeginPos;
  p2:= p1;
  p1.X:= p1.X + UTF8CodepointToByteIndex( pchar(@params.text[1]), params.textByteLength, params.selectedStart );
  p2.X:= p2.X + UTF8CodepointToByteIndex( pchar(@params.text[1]), params.textByteLength, params.selectedStart+params.selectedLength );

  // two vertexs in pixels
  p1:= PosToPixels( p1 );
  p2:= PosToPixels( p2 );
  p2.Y:= p2.Y + FriendEdit.LineHeight;

  // client rect in pixels
  Result:= TRect.Create( p1 , p2 );
end;

function LazSynImeCocoa.PosToPixels( const pos: TPoint ) : TPoint;
begin
  Result:= FriendEdit.LogicalToPhysicalPos( pos );
  Result:= FriendEdit.TextXYToScreenXY( Result );
  Result:= TSynEdit(FriendEdit).ScreenXYToPixels( Result );
end;

constructor LazSynImeCocoa.Create( AOwner: TSynEditBase );
begin
  Inherited;
  _undoList:= ViewedTextBuffer.UndoList;
end;

destructor LazSynImeCocoa.Destroy;
begin
  _undoList:= nil;
  Inherited;
end;

end.

