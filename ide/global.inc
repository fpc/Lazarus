(******************************************************************************
                                     global.inc


 ******************************************************************************)


constructor TUnitInfo.Create;
begin
  inherited Create;
  Source := TStringList.Create;
  Fpage := -1;
  FName := '';
  FFormName := '';
end;

destructor TUnitInfo.Destroy;
begin
  Source.Destroy;
  inherited destroy;
end;

procedure TUnitInfo.SetFormName(Value : String);
var
  I : Integer;
  Texts : String;
  TempNum : Integer;
  Replace : Boolean;
  TempEdit : TmwCustomEdit;
begin
  if (FFormName <> Value) and (FFormName <> '') and (FSource.Count > 0) then
  begin
    {
     run through the source and replace FFormName with Value
     this needs to first check for FFormName, then see if the character before it
     and after it are either space or non valid characters.
     if so, then replace it, otherwise it's simply part of a word.
    }
    for I := 0 to FSource.Count-1 do
    begin
      Texts := FSource.Strings[i];
      TempNum := 1;
      while TempNum <> 0 do
      begin
        Replace := True;
        TempNum := pos(FFormName,Texts);
        if TempNum > 0 
        then begin
          //check the surrounding characters for valid ones.  If valid, leave it.
          if (TempNum > 1) 
          and ((Texts[Tempnum - 1] in ['a'..'z']) or (Texts[TempNum - 1] in ['A'..'Z']) or (Texts[Tempnum - 1] in ['0'..'1'])) 
          then Replace := False;
    
          if Replace 
          and (((TempNum-1) + Length(FFormName)) <= Length(Texts)) //doesn't extend to the last character in the line
          and ((Texts[Tempnum+ Length(FFormName)] in ['a'..'z']) or  (Texts[TempNum+ Length(FFormName)] in ['A'..'Z']) or (Texts[Tempnum+ Length(FFormName)] in ['0'..'1']))
          then Replace := False;
  
          if Replace 
          then begin
            Delete(Texts,TempNum,Length(FFormName));
            Insert(Texts,Value,Tempnum);
          end;	      	
        end;  //Tempnum > 0 ...
      end;  //while ...
      FSource.Strings[I] := Texts;
    end; //for I ...
  
    //update the editor with the new source if the page <> -1;
    if FPage <> -1 
    then begin
      TempEdit := IdeEditor1.GetEditorFromPage(FPage);	
      TempEdit.Lines.Assign(FSource);
    end;
  end;  //if (FFormname <> value) and ...

  //Update FFormname
  FFormName := Value;
end;

function TUnitInfo.GetFormName : String;
begin
  Result := FFormName;
end;

procedure TUnitInfo.SetPage(Value : Integer);
begin
  FPage := Value;
end;

function TUnitInfo.GetPage : Integer;
begin
  Result := FPage;
end;

{This function returns the index line number where the class definition starts}
function TUnitInfo.FindStartClass(cName: String; LineStart: Integer) : Integer;
var
  I : Integer;
  Texts : String;
begin
  for I := LineStart to Source.Count-1 do
  begin
    Texts := Source.Strings[i];
    if  (pos(cName, Texts) > 0) 
    and (pos('class', Texts) > 0) 
    then begin
      //validate it but for now, just break;
      Result := i;
      Break;
    end;
  end;
end;


procedure TUnitInfo.AddProcedureLine(value : String);
var
  TempEdit : TmwCustomEdit;
  num,i : Integer;
begin
  for num := 0 to Source.Count-1 do
  begin
    if (pos('END.',uppercase(Source.Strings[num])) <> 0) 
    then begin
      //validate that this is an actual procedure line
      break;
    end;
  end;
  I := num - 1;
  if I < Source.Count 
  then begin  //found it
    Source.Insert(I + 1, Value);
    Source.Insert(I + 2, 'Begin');
    Source.Insert(I + 3, '');
    Source.Insert(I + 4, 'end;');
    Source.Insert(I + 5, '');

    //reassign source to editor.
    if FPage <> -1 
    then begin
      TempEdit := IdeEditor1.GetEditorFromPage(FPage);
      if TempEdit <> nil 
      then begin
        TempEdit.Lines.Assign(Source);
        IdeEditor1.Notebook1.pages.Strings[Page] := Name;

        //move cursor to the right spot between begin end
        TempEdit.CaretX := 1;
        TempEdit.CaretY := i+3;
        IdeEditor1.Notebook1.Pageindex := page;
        IdeEditor1.Bringtofront;
        TempEdit.SetFocus;
      end;
    end;
  end
  else
      Writeln('!!!!!!!!!!!no END. found!!!!!!!!!!!!!!!!');
end;


procedure TUnitInfo.AddControlLine(value : String); //adds the "Edit1 : TEdit" line
var
  TempEdit : TmwCustomEdit;
  I : Integer;
  Texts : String;
  SpaceCount : Integer;
  Num : Integer;
  END_Found,PROC_Found, PUBLIC_Found, PRIVATE_Found : Boolean;
begin
  END_Found := False;
  PROC_Found := False;
  PUBLIC_Found := False;
  PRIVATE_Found := False;
  //parse the file until either procedures appear or the private section starts
  I := findStartClass(Formname,0);
  //repeat until you find a procedure, private, public, protected, published, or an end
  for num := i to Source.Count-1 do
  begin
    if ( pos('PROCEDURE',uppercase(Source.Strings[num])) <> 0) 
    then begin
      //validate that this is an actual procedure line
      PROC_Found := True;
      break;
    end;

    if ( pos('PRIVATE',uppercase(Source.Strings[num])) <> 0) 
    then begin
      //validate that this is the actual PRIVATE section
      PRIVATE_Found := True;
      break;
    end;

    if ( pos('PUBLIC',uppercase(Source.Strings[num])) <> 0) 
    then begin
      //validate that this is the actual PUBLIC section
      PUBLIC_Found := True;
      break;
    end;

    if ( pos('END;',uppercase(Source.Strings[num])) <> 0) 
    then begin
      //validate that this is the actual END of the class
      END_Found := True;
      break;
    end;
  end;

  I := num-1;
  if I < Source.Count 
  then begin  //found it
    //figure out how many spaces are in front of it.
    SpaceCount := length(Source.Strings[i+1]) - length(trimleft(Source.Strings[i+1]));
    //Should drop down into the section before any procedures
    //and private section but for now, just add the line
    Value := TrimLeft(Value);

    //Add 1+spacecount spaces of it's a private, public or end
    if not(PROC_FOUND) then Inc(SpaceCount);
    for num := 1 to SpaceCount do Value := ' ' + Value;
    Source.Insert(I + 1, Value);

    //reassign source to editor.
    if Fpage <> -1 then
    begin
      TempEdit := IdeEditor1.GetEditorFromPage(Page);
      if TempEdit <> nil 
      then begin
        TempEdit.Lines.Assign(Source);
        IdeEditor1.Notebook1.pages.Strings[Page] := Name;
      end;
    end;
  end;
end;

{ =============================================================================

  $Log$
  Revision 1.1  2000/07/13 10:27:46  michael
  + Initial import

  Revision 1.6  2000/07/09 20:18:55  lazarus
  MWE:
    + added new controlselection
    + some fixes
    ~ some cleanup

  Revision 1.5  2000/06/16 13:33:20  lazarus
  Created a new method for adding controls to the toolbar to be dropped onto the form!
  Shane

  Revision 1.4  2000/06/14 18:04:44  lazarus
  Now adds the code (genericly) when you drop a component on the form
  Shane

  Revision 1.3  2000/03/06 12:28:45  lazarus
  Committed the real Global.inc file.
  Shane

  Revision 1.2  2000/03/06 00:05:05  lazarus
  MWE: Added changes from Peter Dyson <peter@skel.demon.co.uk> for a new
    release of mwEdit (0.92)

  Revision 1.1  2000/03/03 22:47:31  lazarus
  MWE:
    A quick replacement for the original global.inc


}

