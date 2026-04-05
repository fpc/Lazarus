unit test_nested_result_refs;

{$mode ObjFPC}{$H+}

interface
function Simple: boolean;

implementation

function Simple: boolean;
   procedure Sub1;
     procedure Sub_Sub1;
     begin
       if Result{findrefs:11,14;9,17;11,31;4,42} then ;
     end;
   begin
     if Result then ;
   end;
   function Sub2: word;
     function Sub_Sub2: integer;
     begin
       Result{findrefs:8,22}:=1;
     end;
   begin
     Result{findrefs:6,25}:=3;
   end;
   procedure Sub3;

     procedure Sub_sub3_1;
     begin
       if Result{findrefs:11,14;9,17;11,31;4,42} then ;
     end;
   var Result{findrefs:8,33;11,36;6,39}: integer;
     procedure Sub_sub3_2;
     begin
       if Result{findrefs:8,33;11,36;6,39}>1 then ;
     end;
   begin
     Result{findrefs:8,33;11,36;6,39}:=-1;
   end;
 begin
   Result{findrefs:11,14;9,17;11,31;4,42}:=false;
 end;

 end.
