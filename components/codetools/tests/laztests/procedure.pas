unit &procedure;
{$mode ObjFPC}{$H+}
interface
type
{$ScopedEnums on}
  &var = (&array, &end, foo, &bar);
{$ScopedEnums off}
  &end = array of (&abc, def, &uses,new);
  &interface = set of (&set, &_);

function other(&type: &var; &begin: &end): &interface;
function more(&type: &procedure.&var; &begin: &procedure.&end): &procedure.&interface;

implementation

function other(&type: &var; &begin: &end): &interface;
begin
end;

function more(&type: &procedure.&var; &begin: &procedure.&end): &procedure.&interface;
begin
end;

end.

