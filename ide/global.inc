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
end;

{ =============================================================================

  $Log$
  Revision 1.2  2000/12/19 18:43:12  lazarus
  Removed IDEEDITOR.  This causes the PROJECT class to not function.
  Saving projects no longer works.

  I added TSourceNotebook and TSourceEditor.  They do all the work for saving/closing/opening units.  Somethings work but they are in early development.
  Shane

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

