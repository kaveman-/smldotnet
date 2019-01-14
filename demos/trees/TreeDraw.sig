signature TreeDraw =
sig

  datatype Orientation = Portrait | Landscape

  val makeInterface : 
    Orientation -> System.Windows.Forms.UserControl*System.Drawing.Rectangle -> 
    string Draw.Interface

  val makeExtent : 
    Orientation -> System.Windows.Forms.UserControl -> 
    string -> Tree.Extent

  val extentToSize : 
    Orientation -> Tree.Extent -> System.Drawing.Size

end