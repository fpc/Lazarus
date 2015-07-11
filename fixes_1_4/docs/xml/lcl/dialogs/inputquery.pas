 
 Uses forms, lcltype, dialogs, controls;
 
 procedure TryInputQuery;
 var QueryResult: boolean;
   userstring: string;
 begin
   if InputQuery ('Question', 'Type in some data', TRUE, userstring)
   then ShowMessage (userstring)
   else 
   begin
     InputQuery ('Don''t be silly', 'Please try again', userstring);
     ShowMessage (userstring)
   end
 end;
