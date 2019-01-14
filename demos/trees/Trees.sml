structure Trees =
struct

local
val currenttree = 
  ref (NONE : string Tree.Tree option)
val currentlayout = 
  ref (NONE : ((string*real) Tree.Tree * Tree.Extent list) option)
val orientation = ref TreeDraw.Landscape

open System.Windows.Forms System.Drawing System.ComponentModel 

(* The tree viewer control, with minimal interface *)
_classtype TreeControl() : UserControl ()
with 
  OnPaint(SOME (p : PaintEventArgs)) =
  let
    val rect = p.#get_ClipRectangle ()
  in
    (case !currentlayout of
      NONE => 
	let val SOME g = p.#get_Graphics ()
        in
           g.#DrawString(SOME "Right-click for menu", 
			 SOME (Font("Lucida Sans Unicode", 10.0 : Real32.real)),
			 System.Drawing.SystemBrushes.get_ControlText(), 
			 0.0: Real32.real,
			 0.0: Real32.real);
           g.#Dispose()
	end	
    | SOME (tree, extent) =>
      let
        val {left,right,height} = Tree.collapseExtent extent
        val interface = TreeDraw.makeInterface (!orientation) (this:>UserControl, rect)
      in
        ignore(Draw.draw2 interface (~left, 0, extent) tree)
      end);
      p.#Dispose()
  end
end

(* A dialog for selecting a type or directory *)
_classtype SelectDialog (title:string, labeltext) : Form ()
with
local
  val SOME controls = this.#get_Controls()
  val textBox = TextBox()
  val label = Label()
  val buttonOK = Button()
  val buttonCancel = Button()
  val _ =
  (
    this.#set_FormBorderStyle FormBorderStyle.FixedDialog;
    this.#set_MinimizeBox false;
    this.#set_MaximizeBox false;
    this.#set_CancelButton buttonCancel;
    this.#set_AcceptButton buttonOK;
    buttonOK.#set_DialogResult DialogResult.OK;
    buttonCancel.#set_DialogResult DialogResult.Cancel;
    controls.#Add textBox;
    controls.#Add label;
    controls.#Add buttonOK;
    controls.#Add buttonCancel;
    label.#set_Location(Point(4,8));
    label.#set_Text (labeltext ^ ":");
    textBox.#set_Location(Point(60,4));
    textBox.#set_Size(Size(140,24));
    buttonOK.#set_Text "OK";
    buttonCancel.#set_Text "Cancel";
    buttonOK.#set_Location(Point(24,36));
    buttonCancel.#set_Location(Point(128,36));
    this.#set_Size(Size(220,92));
    this.#set_Text title
  )
in
      get_Result() = textBox.#get_Text()
end

(* The top-level window, containing just the tree *)
_classtype MyForm () : Form ()
with
local

  (* Get the controls, ready to add the tree viewer *)
  val SOME controls = this.#get_Controls()

  (* Create and add a tree viewer control *)
  val treeControl = TreeControl()
  val _ = controls.#Add treeControl

  (* Redraw the tree with a particular orientation *)
  fun redrawTree (t,orientation) =
  let
    val (tree, extent) = Tree.design (t, TreeDraw.makeExtent orientation (treeControl:> UserControl))
    val wholeextent = Tree.collapseExtent extent
  in
    currentlayout := SOME (tree, extent);
    treeControl.#set_Size(TreeDraw.extentToSize orientation wholeextent);
    this.#Refresh ()
  end

  (* Replace the current tree with a new one *)
  fun replaceTree (t,name) =
  (
    currenttree := SOME t;
    redrawTree (t,!orientation);
    this.#set_Text("Tree view of " ^ name)
  )
     
  (* Select an XML file *)
  fun selectXML () =
  let
    val fileDialog = OpenFileDialog()
  in
    fileDialog.#set_DefaultExt("XML");
    fileDialog.#set_InitialDirectory(".");
    fileDialog.#set_Filter("XML files (*.xml) |*.xml");
    if fileDialog.#ShowDialog() = DialogResult.OK
    then 
    case fileDialog.#get_FileName() of
      NONE => ()
    | SOME name =>
      replaceTree (ReadXML.make name, "XML file '" ^ name ^ "'")
    else ()
  end

  (* Select a type *)
  fun selectType () =
  let
    val typeDialog = SelectDialog("Select type", "Type")
  in
    if typeDialog.#ShowDialog() = DialogResult.OK
    then
    case typeDialog.#get_Result() of
      NONE => ()
    | SOME name =>
      case ReadTypes.make name of
        Result.Failure message =>
        ignore(MessageBox.Show(this :> IWin32Window, message, "Error"))

      | Result.Success tree =>
        replaceTree (tree, "Type '" ^ name ^ "'")
    else ()
  end

  (* Select a directory *)
  fun selectDirectory () =
  let
    val dirDialog = SelectDialog("Select directory", "Directory")
  in
    if dirDialog.#ShowDialog() = DialogResult.OK
    then
    case dirDialog.#get_Result() of
      NONE => ()
    | SOME name =>
      case ReadDirs.make name of
        Result.Failure message =>
        ignore(MessageBox.Show(this :> IWin32Window, message, "Error"))

      | Result.Success tree =>
        replaceTree (tree, "Directory '" ^ name ^ "'")
    else ()
  end

  val orientationMenuItem = MenuItem "Landscape?"
  val _ = orientationMenuItem.#set_Checked true
  fun flip () =
  let
    val landscape = not (orientationMenuItem.#get_Checked())
  in
    orientation := (if landscape then TreeDraw.Landscape else TreeDraw.Portrait);
    orientationMenuItem.#set_Checked(landscape);
    case !currenttree of
      NONE => ()
    | SOME tree => redrawTree (tree, !orientation)
  end

  val _ = orientationMenuItem.#add_Click(System.EventHandler 
    (fn _ => flip ())) 


  (* Create a right-click menu for picking the data source *)
  val contextMenu = 
  ContextMenu(Array.fromList (map SOME 
  [MenuItem("Directory...", System.EventHandler (fn _ => selectDirectory ())),
   MenuItem("Type...", System.EventHandler (fn _ => selectType ())),
   MenuItem("XML...", System.EventHandler (fn _ => selectXML ())),
   MenuItem("Random", System.EventHandler 
     (fn _ => replaceTree (RandomTree.make (), "random tree"))),
   MenuItem("-"),
   orientationMenuItem
   ]))

  val _ =
  (
    (* This is required if scroll bars are to appear *)
    this.#set_AutoScroll(true);

    (* The initial state of the title bar *)
    this.#set_Text("Tree viewer");

    (* The right-click menu *)
    this.#set_ContextMenu(contextMenu);

    this.#set_AutoScaleBaseSize(Size(5, 13));

    (* Initial size of whole window *)
    this.#set_ClientSize(Size(300, 300));

    (* Want min/max boxes and icon in task bar *)
    this.#set_MaximizeBox(true);
    this.#set_MinimizeBox(true);
    this.#set_ShowInTaskbar(true)

  )

in  (* no new methods *)

end

in

fun main () = Application.Run(MyForm())

end

end


