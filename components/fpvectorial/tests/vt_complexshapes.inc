{ Include file for "Complex shapes" tests. Used by vtmain.pas. }

// Path with hole (solid fill)
node := Tree.Items.AddChild(mainnode, 'Path with hole (solid fill)');
Tree.Items.AddChildObject(node, 'rotated around (10,10) by 30deg CCW',
  TRenderParams.Create(@Render_Path_Hole, 'path_hole_solid.png_rot30ccw.png', $00010000));
Tree.Items.AddChildObject(node, 'normal',
  TRenderParams.Create(@Render_Path_Hole, 'path_hole_solid.png', $00000000));
Tree.Items.AddChildObject(node, 'rotated around (10,10) by 30deg CW',
  TRenderParams.Create(@Render_Path_Hole, 'path_hole_solid.png_rot30cw.png', $00020000));

// Path with hole (gradient fill)
node := Tree.Items.AddChild(mainnode, 'Path with hole (gradient fill)');
Tree.Items.AddChildObject(node, 'rotated around (10,10) by 30deg CCW',
  TRenderParams.Create(@Render_Path_Hole, 'path_hole_gradient_rot30ccw.png', $00010003));
Tree.Items.AddChildObject(node, 'normal',
  TRenderParams.Create(@Render_Path_Hole, 'path_hole_gradient.png', $00000003));
Tree.Items.AddChildObject(node, 'rotated around (10,10) by 30deg CW',
  TRenderParams.Create(@Render_Path_Hole, 'path_hole_gradient_rot30cw.png', $00020003));

// Self-intersecting polygon (solid fill, even-odd rule) - tip at bottom
node := Tree.Items.AddChild(mainnode, 'Self-intersecting polygon (solid fill, even-odd rule) - tip at bottom');
Tree.Items.AddChildObject(node, 'rotated around (10,10) by 30deg CCW',
  TRenderParams.Create(@Render_SelfIntersectingPoly, 'selfintersecting_poly_solid_eo_rot30ccw.png', $00010000));
Tree.Items.AddChildObject(node, 'normal',
  TRenderParams.Create(@Render_SelfIntersectingPoly, 'selfintersecting_poly_solid_eo.png', $00000000));
Tree.Items.AddChildObject(node, 'rotated around (10,10) by 30deg CW',
  TRenderParams.Create(@Render_SelfIntersectingPoly, 'selfintersecting_poly_solid_eo_rot30cw.png', $00020000));

// Self-intersecting polygon (gradient fill, even-odd rule) - tip at bottom
node := Tree.Items.AddChild(mainnode, 'Self-intersecting polygon (gradient fill, even-odd rule) - tip at bottom');
Tree.Items.AddChildObject(node, 'rotated around (10,10) by 30deg CCW',
  TRenderParams.Create(@Render_SelfIntersectingPoly, 'selfintersecting_poly_gradient_eo_rot30ccw.png', $00010003));
Tree.Items.AddChildObject(node, 'normal',
  TRenderParams.Create(@Render_SelfIntersectingPoly, 'selfintersecting_poly_gradient_eo.png', $00000003));
Tree.Items.AddChildObject(node, 'rotated around (10,10) by 30deg CW',
  TRenderParams.Create(@Render_SelfIntersectingPoly, 'selfintersecting_poly_gradient_eo_rot30cw.png', $00020003));

// Self-intersecting polygon (solid fill, nonzero winding rule) - tip at bottom
node := Tree.Items.AddChild(mainnode, 'Self-intersecting polygon (solid fill, nonzero winding rule) - tip at bottom');
Tree.Items.AddChildObject(node, 'rotated around (10,10) by 30deg CCW',
  TRenderParams.Create(@Render_SelfIntersectingPoly, 'selfintersecting_poly_solid_nzw_rot30ccw.png', $00010100));
Tree.Items.AddChildObject(node, 'normal',
  TRenderParams.Create(@Render_SelfIntersectingPoly, 'selfintersecting_poly_solid_nzw.png', $00000100));
Tree.Items.AddChildObject(node, 'rotated around (10,10) by 30deg CW',
  TRenderParams.Create(@Render_SelfIntersectingPoly, 'selfintersecting_poly_solid_nzw_rot30cw.png', $00020100));

// Self-intersecting polygon (gradient fill, nonzero winding rule) - tip at bottom
node := Tree.Items.AddChild(mainnode, 'Self-intersecting polygon (gradient fill, nonzero winding rule) - tip at bottom');
Tree.Items.AddChildObject(node, 'rotated around (10,10) by 30deg CCW',
  TRenderParams.Create(@Render_SelfIntersectingPoly, 'selfintersecting_poly_gradient_nzw_rot30ccw.png', $00010103));
Tree.Items.AddChildObject(node, 'normal',
  TRenderParams.Create(@Render_SelfIntersectingPoly, 'selfintersecting_poly_gradient_nzw.png', $00000103));
Tree.Items.AddChildObject(node, 'rotated around (10,10) by 30deg CW',
  TRenderParams.Create(@Render_SelfIntersectingPoly, 'selfintersecting_poly_gradient_nzw_rot30cw.png', $00020103));

