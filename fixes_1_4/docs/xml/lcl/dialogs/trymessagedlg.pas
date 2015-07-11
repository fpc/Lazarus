 
 Uses forms, dialogs, lcltype, controls;
 
 procedure TryMessageDlg;
 begin
   if MessageDlg ('Question', 'Do you wish to Execute?', mtConfirmation, 
                  [mbYes, mbNo, mbIgnore],0) = mrYes
   then { Execute rest of Program };
  end;
