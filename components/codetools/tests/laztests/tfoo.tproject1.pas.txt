program tfoo.tproject1{findrefs:14,1;13,36;13,38;11,88;11,90;8,110;8,125;8,126};
{$mode ObjFPC}{$ModeSwitch advancedrecords}{$H+}
uses
  tproject1.unit1{declaration:tproject1.unit1/tproject1.unit1|completion:unit1,!tbar,!tfoo,!tsome|findrefs:6,89}, // TODO: findrefs missing 8,37 and 3,4
  tbar.unit1{declaration:tbar.unit1/tbar.unit1|completion:unit1,!tbar,!tfoo,!tsome|findrefs:}, // TODO: findrefs missing 3,5
  tfoo.unit1{declaration:tfoo.unit1/tfoo.unit1|completion:unit1,!tbar,!tfoo,!tsome|findrefs:8,23;8,33;8,34;8,36;8,38;6,85;6,86;6,88;6,90;34,105;34,106;3,110;3,125;3,126}, // TODO: findrefs WRONG too much: 8,36;8,38;6,88;6,90;3,110;3,125;3,126
  tbar{declaration:tbar/tbar|completion:+1=tbar,!tsome|findrefs:3,7;22,16;22,17;8,39;8,40;6,91;6,92};

var
  v_u1: unit1{declaration:tbar/tbar.unit1|completion:+1=unit1};
  v_p1: project1{completion:+1=project1};
  tsome: integer;

const
  ref1_tbar_tfoo_a = tbar_tfoo_a{declaration:tbar/tbar.tfoo|completion:tbar_tfoo_a,tbar_tfoo_b};
  ref2_tbar_tfoo_a = tbar.tbar_tfoo_a{declaration:tbar/tbar.tfoo|completion:tbar_tfoo_a,tbar_tfoo_b,!u1,bar,abc};
  ref3_tbar_tfoo_a = tbar.tfoo{declaration:tbar/tbar.tfoo}.tbar_tfoo_a{declaration!:tbar/tbar.tfoo|completion:tbar_tfoo_a,tbar_tfoo_b,!u1,!bar,!abc,!unit1};

type
  abc = (p1,p2);

  unit1{findrefs:3,22;23,36;9,54;8,56;41,59;16,70;3,72;3,73;11,76;3,80;6,87;21,88;9,93;11,119;3,123;3,124;18,125;18,126} = class // TODO: findrefs missing 11,41;  WRONG/too much: 3,80
    x: tfoo.unit1.bar{declaration:tfoo.unit1/tfoo.unit1.bar|completion:bar,abc,u1,!unit1,!tfoo,!tproject1};
    unit1{findrefs:5,24;8,35;11,41;8,60;43,72;43,123;58,125}: record // TODO: WRONG 8,35;11,41  missing 3,80
      bar: (x_u1_b_a, x_u1_b_b);
    end; static;
    tproject1: record
      bar: (x_p1_b_a, x_p1_b_b);
    end; static;
    type
    abc = boolean;
  public
    b: tfoo.unit1.bar{declaration:tfoo.unit1/tfoo.unit1.bar|completion:bar,abc,u1,!unit1,!tfoo,!tproject1};
    c: tfoo.unit1.abc{declaration:tfoo.unit1/tfoo.unit1.abc|completion:bar,abc,u1,!unit1,!tfoo,!tproject1};
    d: unit1{ TODO: declaration:tfoo.tproject1/unit1}.abc{ TODO: declaration:tfoo.tproject1/unit1.abc|completion:x,unit1,tproject1,abc,!bar,!u1};
    e: tfoo.tproject1.unit1{declaration:tfoo.tproject1/unit1|completion:abc,unit1,!u1,!tfoo,!tbar,ref1_tbar_tfoo_a}.abc{declaration:tfoo.tproject1/unit1.abc|completion:abc,unit1,!u1,!tfoo,!tbar,!ref1_tbar_tfoo_a};
    f: tproject1.unit1.abc{ TODO declaration:tproject1.unit1/tproject1.unit1.abc|completion:bar,abc,!u1,unit1,!tfoo,!ref1_tbar_tfoo_a};
    g: tfoo.tproject1.abc{declaration:tfoo.tproject1/abc|completion:abc,unit1,!u1,!tfoo,!tbar,ref1_tbar_tfoo_a};
    h: tbar.abc{declaration:tbar/tbar.abc|completion:abc,unit1,!u1,!tbar,!ref1_tbar_tfoo_a};
    i: tbar.tfoo{declaration:tbar/tbar.tfoo|completion:abc,unit1,!u1,!tbar,!ref1_tbar_tfoo_a};
    v_u2: unit1{ TODO: declaration:tfoo.tproject1/unit1};

  procedure M1;
  public
    tfoo: record
      tproject1: (fp1, fp2);
    end;
  end;

  project1 =boolean;

procedure bar;
var
  tfoo: unit1{declaration:tfoo.tproject1/unit1};
  //tt: unit1;
  bar: unit1.abc{declaration:tfoo.tproject1/unit1.abc};
  xyz: u1{declaration:tfoo.unit1/tfoo.unit1.u1};
begin
  tt{guesstype:tfoo.tproject1/unit1} := unit1{declaration:tfoo.tproject1/unit1}.Create{completion:create};
  tfoo.unit1{declaration:tfoo.tproject1/unit1.unit1}.bar{declaration:tfoo.tproject1/unit1.unit1.bar|completion:bar,!unit1,!tfoo,!abc} := x_u1_b_a{declaration!:tfoo.tproject1/unit1.unit1.bar|completion:x_u1_b_a,x_u1_b_b,!bar,!abc};
  tfoo.tproject1{declaration:tfoo.tproject1/unit1.tproject1}.bar{declaration:tfoo.tproject1/unit1.tproject1.bar} := x_p1_b_a{declaration!:tfoo.tproject1/unit1.tproject1.bar|completion:x_p1_b_a,x_p1_b_b,!x_u1_b_a,!x_u1_b_b,!bar,!abc};
  tfoo.b := fu1_b_a{declaration:tfoo.unit1/tfoo.unit1.bar};
  tfoo.c := fu1_a_a{declaration:tfoo.unit1/tfoo.unit1.abc};
  tfoo.d := true;
  tfoo.e := true;
  tfoo.f := a1{declaration:tproject1.unit1/tproject1.unit1.abc};
  tfoo.g := p1{declaration:tfoo.tproject1/abc};
  tfoo.h := xa1{declaration:tbar/tbar.abc};
  tfoo.i := tbar_tfoo_a{declaration:tbar/tbar.tfoo};
  tfoo.v_u2 := unit1.Create;

  unit1{declaration:tfoo.tproject1/unit1}.unit1{declaration:tfoo.tproject1/unit1.unit1}.bar{declaration:tfoo.tproject1/unit1.unit1.bar} := x_u1_b_a{declaration:tfoo.tproject1/unit1.unit1.bar};
  unit1{declaration:tfoo.tproject1/unit1}.tproject1{declaration:tfoo.tproject1/unit1.tproject1}.bar{declaration:tfoo.tproject1/unit1.tproject1.bar} := x_p1_b_a{declaration:tfoo.tproject1/unit1.tproject1.bar};
end;

procedure unit1.M1{declaration:tfoo.tproject1/unit1.M1};
begin
  tfoo{declaration:tfoo.tproject1/unit1.tfoo}.tproject1{declaration:tfoo.tproject1/unit1.tfoo.tproject1|completion:tproject1,!abc,!tfoo,!unit1} := fp1{declaration:tfoo.tproject1/unit1.tfoo.tproject1};

  unit1{ TODO: declaration:tfoo.tproject1/unit1.unit1}.bar{ TODO: declaration:tfoo.tproject1/unit1.unit1.bar|completion:bar,!tproject1,!abc,!tfoo,!unit1} := x_u1_b_a{declaration:tfoo.tproject1/unit1.unit1.bar};
  tproject1{declaration:tfoo.tproject1/unit1.tproject1}.bar{declaration:tfoo.tproject1/unit1.tproject1.bar|completion:bar,!tproject1,!abc,!tfoo,!unit1} := x_p1_b_a{declaration:tfoo.tproject1/unit1.tproject1.bar};
end;

var
  b: tfoo.unit1.bar{declaration:tfoo.unit1/tfoo.unit1.bar|completion:bar,abc,u1,!unit1,!tfoo,!tproject1};
  c: tfoo.unit1.abc{declaration:tfoo.unit1/tfoo.unit1.abc|completion:bar,abc,u1,!unit1,!tfoo,!tproject1};
  d: unit1{declaration:tfoo.tproject1/unit1}.abc{declaration:tfoo.tproject1/unit1.abc|completion:x,unit1,tproject1,abc,!bar,!u1};
  e: tfoo.tproject1.unit1{declaration:tfoo.tproject1/unit1|completion:abc,unit1,!u1,!tfoo,!tbar,ref1_tbar_tfoo_a}.abc{declaration:tfoo.tproject1/unit1.abc|completion:abc,unit1,!u1,!tfoo,!tbar,!ref1_tbar_tfoo_a};
  f: tproject1.unit1.abc{declaration:tproject1.unit1/tproject1.unit1.abc|completion:bar,abc,!u1,unit1,!tfoo,!ref1_tbar_tfoo_a};
  g: tfoo.tproject1.abc{declaration:tfoo.tproject1/abc|completion:abc,unit1,!u1,!tfoo,!tbar,ref1_tbar_tfoo_a};
  h: tbar.abc{declaration:tbar/tbar.abc|completion:abc,unit1,!u1,!tbar,!ref1_tbar_tfoo_a};
  i: tbar.tfoo{declaration:tbar/tbar.tfoo|completion:abc,unit1,!u1,!tbar,!ref1_tbar_tfoo_a};
  v_u2: unit1{declaration:tfoo.tproject1/unit1};
  v_p2: project1{declaration:tfoo.tproject1/project1};
type
  tbar=boolean;
  {$SCOPEDENUMS On}
  tproject1=(unit1, tbar);
var
  h2: tbar;
  j: tproject1;

begin
  b{guesstype:tfoo.unit1/bar} := fu1_b_a{declaration:tfoo.unit1/tfoo.unit1.bar};
  b{guesstype:tfoo.unit1/bar} := tfoo.unit1.bar.fu1_b_a{declaration!:tfoo.unit1/tfoo.unit1.bar};
  b{guesstype:tfoo.unit1/bar} := tfoo.unit1.fu1_b_a{declaration:tfoo.unit1/tfoo.unit1.bar};
  c := fu1_a_a{declaration:tfoo.unit1/tfoo.unit1.abc};
  d := true;
  e := true;
  tfoo.tproject1.bar{declaration:tfoo.tproject1/bar};
  f := a1{declaration:tproject1.unit1/tproject1.unit1.abc};
  g := p1{declaration:tfoo.tproject1/abc};
  h := xa1{declaration:tbar/tbar.abc};
  h2 := true;
  i := tbar_tfoo_a{declaration:tbar/tbar.tfoo};
  j{guesstype:tfoo.tproject1/tproject1} := tproject1.unit1{declaration!:tfoo.tproject1/tproject1:98};
  j := tproject1.tbar{declaration!:tfoo.tproject1/tproject1:98};
  v_u1 := tbar_unit1_a{declaration:tbar/tbar.unit1};
  v_u2 := unit1{declaration:tfoo.tproject1/unit1}.Create;
  v_p1 := tbar_project1_a{declaration:tbar/tbar.project1};
  v_p2 := true;

  unit1{declaration:tfoo.tproject1/unit1}.unit1{declaration:tfoo.tproject1/unit1.unit1}.bar{declaration:tfoo.tproject1/unit1.unit1.bar} := x_u1_b_a{declaration:tfoo.tproject1/unit1.unit1.bar};
  unit1{declaration:tfoo.tproject1/unit1}.tproject1{declaration:tfoo.tproject1/unit1.tproject1}.bar{declaration:tfoo.tproject1/unit1.tproject1.bar} := x_p1_b_a{declaration:tfoo.tproject1/unit1.tproject1.bar};
  tfoo.tproject1.unit1{declaration:tfoo.tproject1/unit1}.unit1{declaration:tfoo.tproject1/unit1.unit1}.bar{declaration:tfoo.tproject1/unit1.unit1.bar} := x_u1_b_a{declaration:tfoo.tproject1/unit1.unit1.bar};
  tfoo.tproject1.unit1{declaration:tfoo.tproject1/unit1}.tproject1{declaration:tfoo.tproject1/unit1.tproject1}.bar{declaration:tfoo.tproject1/unit1.tproject1.bar} := x_p1_b_a{declaration:tfoo.tproject1/unit1.tproject1.bar};
end.

