{------------------------------------------------------------------------------}
{   TStatusBar Constructor                                                     }
{------------------------------------------------------------------------------}
constructor TStatusBar.Create(AOwner : TComponent);
begin
 inherited Create(AOwner);

 fCompStyle := csStatusBar;
 Align := alBottom;

 Setbounds(0,TWinControl(AOwner).Height-21,TWInControl(AOwner).Width,20);


 FContext := 0;
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
      Msg.Panel := length(Value);//0;
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
