{------------------------------------------------------------------------------}
{   TStatusBar Constructor                                                     }
{------------------------------------------------------------------------------}
constructor TStatusBar.Create(AOwner : TComponent);
begin
 Assert(False, 'Trace:Inside TStatusBar Constructor');
 inherited Create(AOwner);

 fCompStyle := csStatusBar;

 CreateComponent(AOwner);
 Height := 15;
 Width := 100;
 Top := 3;
 Left := 1;

 FContext := 0;
Align := alBottom;
 FMessage := -1;
end;

{------------------------------------------------------------------------------}
{   TStatusBar SetSimpleText                                                   }
{------------------------------------------------------------------------------}
procedure TStatusBar.SetSimpleText(Value : String);
var
   glabel : PChar;
   Msg : TLMSetControlText;
begin
   if FSimpleText <> value then
   begin
      FSimpleText := Value;
      Msg.Panel := 0;
      Msg.fCompStyle := fCompStyle;
      gLAbel := StrAlloc(length(value) + 1);
      StrPCopy(gLabel,Value);
      Msg.Userdata := gLabel;
      CNSendMEssage(LM_SETTEXT, Self, @Msg);
      StrDispose(gLabel);
   end;
end;

{------------------------------------------------------------------------------}
{   TStatusBar Destructor                                                      }
{------------------------------------------------------------------------------}
destructor TStatusBar.Destroy;
begin
 inherited Destroy;
end;
