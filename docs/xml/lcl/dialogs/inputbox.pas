uses 
  Forms, LCLType, Dialogs, Controls;

procedure TryInputBox;
var 
  userstring: string;
begin
  userstring := InputBox ('Get some text input', 
                          'Please type in some information', 
                          'Some sample text');
  ShowMessage(userstring);
end;

