 
 Uses Forms, Dialogs, LCLType;
 
 Procedure DisplayMessageBox;
   var reply, boxstyle: integer;
   begin
     with application do begin
       boxstyle :=  MB_ICONQUESTION + MB_YESNO;
       reply :=  MessageBox ('Press either button', 'MessageBoxDemo', boxstyle);
       if reply = IDYES then MessageBox ('Yes       ', 'Reply',MB_ICONINFORMATION)
       else MessageBox ('No         ', 'Reply', MB_ICONHAND)
   end;
