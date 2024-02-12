unit tbar.Unit1;
{$mode ObjFPC}{$H+}
interface

type
  bar = (tbu1_b1, tbu1_b2);
  abc = (tbu1_a1, tbu1_a2);

  unit1 = (tbu1__unit1_a, tbu1__unit1_b);
  project1 = (tbu1__project1_a, tbu1__project1_b);
  tfoo = (tbu1__tfoo_a, tbu1__tfoo_b);

  wrong_member_unit1 = (tbar_unit1_a, tbar_unit1_b);
  wrong_member_project1 = (tbar_project1_a, tbar_project1_b);
  wrong_member_tfoo = (tbar_tfoo_a, tbar_tfoo_b);


implementation

end.

