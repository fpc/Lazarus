{ Include file for "Gradients" tests. Used by vtmain.pas. }

node := Tree.Items.AddChild(mainnode, 'horizontal');
Tree.Items.AddChildObject(node, 'Circle',
  TRenderParams.Create(@Render_Shape, 'circle_gradienthor.png', $0101));
Tree.Items.AddChildObject(node, 'Ellipse',
  TRenderParams.Create(@Render_Shape, 'ellipse_gradienthor.png', $0201));
Tree.Items.AddChildObject(node, 'Rectangle',
  TRenderParams.Create(@Render_Shape, 'rect_gradienthor.png', $0301));
Tree.Items.AddChildObject(node, 'Rounded rectangle',
  TRenderParams.Create(@Render_Shape, 'rounded_rect_gradienthor.png', $0401));
Tree.Items.AddChildObject(node, 'Polygon',
  TRenderParams.Create(@Render_Shape, 'polygon_gradienthor.png', $0501));

node := Tree.Items.AddChild(mainnode, 'vertical');
Tree.Items.AddChildObject(node, 'Circle',
  TRenderParams.Create(@Render_Shape, 'circle_gradientvert.png', $0102));
Tree.Items.AddChildObject(node, 'Ellipse',
  TRenderParams.Create(@Render_Shape, 'ellipse_gradientvert.png', $0202));
Tree.Items.AddChildObject(node, 'Rectangle',
  TRenderParams.Create(@Render_Shape, 'rect_gradientvert.png', $0302));
Tree.Items.AddChildObject(node, 'Rounded rectangle',
  TRenderParams.Create(@Render_Shape, 'rounded_rect_gradientvert.png', $0402));
Tree.Items.AddChildObject(node, 'Polygon',
  TRenderParams.Create(@Render_Shape, 'polygon_gradientvert.png', $0502));

node := Tree.Items.AddChild(mainnode, 'linear');
Tree.Items.AddChildObject(node, 'Circle',
  TRenderParams.Create(@Render_Shape, 'circle_gradientlinear.png', $0103));
Tree.Items.AddChildObject(node, 'Ellipse',
  TRenderParams.Create(@Render_Shape, 'ellipse_gradientlinear.png', $0203));
Tree.Items.AddChildObject(node, 'Rectangle',
  TRenderParams.Create(@Render_Shape, 'rect_gradientlinear.png', $0303));
Tree.Items.AddChildObject(node, 'Rounded rectangle',
  TRenderParams.Create(@Render_Shape, 'rounded_rect_gradientlinear.png', $0403));
Tree.Items.AddChildObject(node, 'Polygon',
  TRenderParams.Create(@Render_Shape, 'polygon_gradientlinear.png', $0503));

node := Tree.Items.AddChild(mainnode, 'radial');
Tree.Items.AddChildObject(node, 'Circle',
  TRenderParams.Create(@Render_Shape, 'circle_gradientradial.png', $0104));
Tree.Items.AddChildObject(node, 'Ellipse',
  TRenderParams.Create(@Render_Shape, 'ellipse_gradientradial.png', $0204));
Tree.Items.AddChildObject(node, 'Rectangle',
  TRenderParams.Create(@Render_Shape, 'rect_gradientradial.png', $0304));
Tree.Items.AddChildObject(node, 'Rounded rectangle',
  TRenderParams.Create(@Render_Shape, 'rounded_rect_gradientradial.png', $0404));
Tree.Items.AddChildObject(node, 'Polygon',
  TRenderParams.Create(@Render_Shape, 'polygon_gradientradial.png', $0504));

node := Tree.Items.AddChild(mainnode, 'radial off-center');
Tree.Items.AddChildObject(node, 'Circle',
  TRenderParams.Create(@Render_Shape, 'circle_gradientradial_offc.png', $0105));
Tree.Items.AddChildObject(node, 'Ellipse',
  TRenderParams.Create(@Render_Shape, 'ellipse_gradientradial_offc.png', $0205));
Tree.Items.AddChildObject(node, 'Rectangle',
  TRenderParams.Create(@Render_Shape, 'rect_gradientradial_offc.png', $0305));
Tree.Items.AddChildObject(node, 'Rounded rectangle',
  TRenderParams.Create(@Render_Shape, 'rounded_rect_gradientradial_offc.png', $0405));
Tree.Items.AddChildObject(node, 'Polygon',
  TRenderParams.Create(@Render_Shape, 'polygon_gradientradial_offc.png', $0505));

