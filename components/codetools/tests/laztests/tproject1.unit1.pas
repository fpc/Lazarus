unit tproject1.Unit1;
{$mode ObjFPC}{$H+}
interface
type
  bar = (b1, b2);
  abc = (a1, a2);

  unit1 = (p1u1_unit1_a, p1u1_unit1_b);
  project1 = (p1u1_project1_a, p1u1_project1_b);
  tfoo = (p1u1_tfoo_a, p1u1_tfoo_b);

  wrong_member_unit1 = (tbar_unit1_a, tbar_unit1_b);
  wrong_member_project1 = (tbar_project1_a, tbar_project1_b);
  wrong_member_tfoo = (tbar_tfoo_a, tbar_tfoo_b);


implementation

end.

