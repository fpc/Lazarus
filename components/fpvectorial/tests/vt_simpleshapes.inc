{ Include file for "Simple shapes" tests. Used by vtmain.pas. }

// Circle
node := Tree.Items.AddChild(mainnode, 'Circle (solid) - moved up');
Tree.Items.AddChildObject(node, 'rotated around (10,10) by 30deg CCW',
  TRenderParams.Create(@Render_Shape, 'circle_solid_rot30ccw.png', $00010100));
Tree.Items.AddChildObject(node, 'normal',
  TRenderParams.Create(@Render_Shape, 'circle_solid.png', $0100));
Tree.Items.AddChildObject(node, 'rotated around (10,10) by 30deg CW',
  TRenderParams.Create(@Render_Shape, 'circle_solid_rot30cw.png', $00020100));

// Ellipse
node := Tree.Items.AddChild(mainnode, 'Ellipse (solid) - moved up');
Tree.Items.AddChildObject(node, 'rotated around (10,10) by 30deg CCW',
  TRenderParams.Create(@Render_Shape, 'ellipse_solid_rot30ccw.png', $00010200));
Tree.Items.AddChildObject(node, 'normal',
  TRenderParams.Create(@Render_Shape, 'ellipse_solid.png', $0200));
Tree.Items.AddChildObject(node, 'rotated around (10,10) by 30deg CW',
  TRenderParams.Create(@Render_Shape, 'ellipse_solid_rot30cw.png', $00020200));

// Rectangle
node := Tree.Items.AddChild(mainnode, 'Rectangle (solid) - moved up');
Tree.Items.AddChildObject(node, 'rotated around (10,10) by 30deg CCW',
  TRenderParams.Create(@Render_Shape, 'rect_solid_rot30ccw.png', $00010300));
Tree.Items.AddChildObject(node, 'normal',
  TRenderParams.Create(@Render_Shape, 'rect_solid.png', $0300));
Tree.Items.AddChildObject(node, 'rotated around (10,10) by 30deg CW',
  TRenderParams.Create(@Render_Shape, 'rect_solid_rot30cw.png', $00020300));

// Rounded rectangle
node := Tree.Items.AddChild(mainnode, 'Rounded rectangle (solid) - moved up');
Tree.Items.AddChildObject(node, 'rotated around (10,10) by 30deg CCW',
  TRenderParams.Create(@Render_Shape, 'rounded_rect_solid_rot30ccw.png', $00010400));
Tree.Items.AddChildObject(node, 'normal',
  TRenderParams.Create(@Render_Shape, 'rounded_rect_solid.png', $0400));
Tree.Items.AddChildObject(node, 'rotated around (10,10) by 30deg CW',
  TRenderParams.Create(@Render_Shape, 'rounded_rect_solid_rot30cw.png', $00020400));

// Polygon
node := Tree.Items.AddChild(mainnode, 'Polygon (solid) - moved up');
Tree.Items.AddChildObject(node, 'rotated around (10,10) by 30deg CCW',
  TRenderParams.Create(@Render_Shape, 'polygon_solid_rot30ccw.png', $000100500));
Tree.Items.AddChildObject(node, 'normal',
  TRenderParams.Create(@Render_Shape, 'polygon_solid.png', $0500));
Tree.Items.AddChildObject(node, 'rotated around (10,10) by 30deg CW',
  TRenderParams.Create(@Render_Shape, 'polygon_solid_rot30cw.png', $000200500));
(*

node := Tree.Items.AddChild(node0, 'normal');
Tree.Items.AddChildObject(node, 'Circle (solid) - moved up',
  TRenderParams.Create(@Render_Shape, 'circle_solid.png', $0100));
Tree.Items.AddChildObject(node, 'Ellipse (solid) - moved up',
  TRenderParams.Create(@Render_Shape, 'ellipse_solid.png', $0200));
Tree.Items.AddChildObject(node, 'Rectangle(solid) - moved up',
  TRenderParams.Create(@Render_Shape, 'rect_solid.png', $0300));
Tree.Items.AddChildObject(node, 'Rounded rectangle (solid) - moved up',
  TRenderParams.Create(@Render_Shape, 'rounded_rect_solid.png', $0400));
Tree.Items.AddChildObject(node, 'Polygon (solid) - moved up',
  TRenderParams.Create(@Render_Shape, 'polygon_solid.png', $0500));

                      *)

