program &tampersand{ TODO findrefs:0,0};
{$mode ObjFPC}{$H+}
uses
  &procedure {completion:procedure|completion:-1=procedure|completion:+1=procedure|declaration:procedure/procedure}
  ;

type
  xyz = integer;
  &var{declaration:procedure/&procedure.var} // previous declaration
  =
    (&array{declaration:tampersand/var},
     &begin{declaration:tampersand/var},
     foo{declaration:tampersand/var},
     &bar{declaration:tampersand/var}
    );
  &end{declaration:procedure/&procedure.end} // previous declaration
  =
    array of (&abc{declaration:tampersand/end},
              def{declaration:tampersand/end},
              &uses{declaration:tampersand/end}
             );
  &program =
    set of (&set{declaration:tampersand/program},
            &_{declaration:tampersand/program}
           );

  &for = boolean;
  &if =
  class
  public type
    &for{declaration:tampersand/for} // previous declaration
    = integer;
  end;

  &record = record
    abc:
      integer;
    &array:
      record
        x,y,&case:
          &if{declaration:tampersand/if}.&for{completion:&for|completion:-1=&for|declaration:tampersand/if.for};
      end
  end;


function &function(
  &type: &var{completion:&var,xyz|completion:-1=&var,xyz|declaration:tampersand/var};
  &begin: &end{declaration:tampersand/end}
): &program{completion:&program,xyz|completion:-1=&program,xyz|declaration:tampersand/program};
var
  &then: &var;
  &if: &program;
begin
  &then {guesstype:&tampersand/var}
  := &type;
  &if{guesstype:&tampersand/program}
  := &result;
end;

function fun(
  &type: &tampersand.&var{completion:&var,xyz|completion:-1=&var,xyz|declaration:tampersand/var};
  &begin: tampersand.&end{declaration:tampersand/end}
): &tampersand{completion:&tampersand,&program,xyz|completion:-1=&tampersand,&program,xyz|declaration!:&tampersand}
   .&program{completion:&program,xyz|completion:-1=&program,xyz|declaration:tampersand/program};
var
  &then: &var;
  &if: &program;
begin
  &then {guesstype:&tampersand/var}
  := &type;
  &if{guesstype:&tampersand/program}
  := &result;
end;

function &name(
  &type: &procedure.&var{declaration:procedure/&procedure.var};
  &begin: &procedure.&end{declaration:procedure/&procedure.end}
): &procedure.&interface{declaration!:procedure/&procedure.interface};
var
  &then: &procedure.&var;
  &if: &procedure.&interface;
begin
  &then {guesstype:&procedure/var}
  := &type;
  &if{guesstype:&procedure/interface}
  := &result;
end;


var
  &class:
    &program{completion:&tampersand,&program,xyz|completion:-1=&tampersand,&program,xyz|declaration:tampersand/program};
  fun1:
    &tampersand.&program{declaration:tampersand/program};
  fun2:
    &program{declaration:tampersand/program};
  n1, n2:
    &procedure.&interface{declaration!:procedure/&procedure.interface};
  &const:
    &end{declaration:tampersand/end};
  &case, cast:
    &record{declaration:tampersand/record};
  &then:
    integer;
begin
  &class{guesstype:&tampersand/program}
  :=
    &function{completion:&tampersand,&function,fun,&program,xyz|completion:-1=&tampersand,&function,fun,&program,xyz|completion:+11=&function,fun,!&program,!program,!xyz|declaration!:tampersand/&function}
    (&array{completion:&tampersand,&function,fun,&array,xyz|completion:-1=&tampersand,&function,fun,&array,xyz|completion:+1=!&function,!function,!fun,&array,!xyz|declaration!:tampersand/var},
     &const
    );
  fun1
  :=
  &fun{completion:&tampersand,&function,fun,&program,xyz|completion:-1=&tampersand,&function,fun,&program,xyz|completion:+11=&function,fun,!&program,!program,!xyz|declaration!:tampersand/fun}
  (
    &var.&begin,
    [&tampersand.&uses, &uses]
  );
  fun2
  :=
  fun(
    &foo,
    [def, &def, abc, &abc]
  );
  &name(
    &procedure.&var.&foo,
    [&procedure.&def]
  );
  name(
    &procedure.&var.foo,
    [&procedure.def]
  );
  n1
  :=
  &other{completion:&procedure,&tampersand,other,more,&program,xyz|completion:-1=&procedure,&tampersand,other,more,&program,xyz|completion:+11=other|declaration!:procedure/&procedure.other}
  (
    &procedure.&var.&end,
    [&procedure.def]
  );
  n2
  :=
  other(
    &procedure.&var.&end,
    [&procedure.&def]
  );
  &more(
    &procedure.&var.&foo,
    [new]
  );

  cast{completion:&case,cast,n1|completion:-1=&case,cast,n1|completion:+1=&case,&const,!then,!&then}.
    abc :=&2;
  cast.&abc :=2;
  cast.&array:=&case.&array;
  cast.&array.x:=2;
  cast.&array.&case:=2;

  &cast{completion:&case,cast,n1|completion:-1=&case,cast,n1|completion:+1=&case,&const,!then,!&then}.
    abc :=2;
  &cast.&abc :=2;
  &cast.&array:=&case.&array;
  &cast.&array.x:=2;
  &cast.&array.&case:=2;

  &case.abc :=2;
  &case.&abc :=&2;
  &case.&array:=cast.&array;
  &case.&array.x:=2;
  &then { TODO: guesstype:tampersand/if.for}
  :=
  &case{completion:&case|completion:-1=&case|completion:+1=&case,&const,!then,!&then}.
    &array{completion:&array,abc|completion:-1=&array,abc|completion:+1=&array,abc|completion:+2=&array,!abc|declaration:tampersand/record.array}.
      &case{completion:&case,x,y|completion:-1=&case,x,y|completion:+1=&case,!x,!y|declaration:tampersand/record.array.case}
      ;

end.

