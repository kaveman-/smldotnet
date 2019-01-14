using System;
using System.Windows.Forms; 
using System.IO;
using System.Drawing;
using System.Drawing.Drawing2D;
using System.Drawing.Imaging;

namespace Client {
    public class Interrupt : System.Exception{};

    public class Display : Form {

        // constants
        private static Color BackgroundColor = Color.Red;
        private static Color ImageColor = Color.Blue;
        private static int RefreshRate = 256;
        private static int Magnification = 2;
        private static float MagnificationF = Magnification;
	private static Size InitialSize = new Size(Magnification*350,Magnification*350);


	// local variables
        private MenuItem openMenuItem = null;
        private MenuItem interruptMenuItem = null;

	private static bool interrupt = false;
        private static Display display = null;

        private static Classes.FactoryClass factoryobject = null; 

        private static Bitmap image = null;

	private static Brush BackgroundBrush = null;

        public class DisplayImageClass : Classes.ImageClass {
	    private int maxX,minX,maxY,minY;
	    private int count = 0;
	    
	    // DisplayImage extends the ML class Classes.ImageClass
	    public DisplayImageClass(string name,int width,int height): base(name,width,height){
		image = NewBitmap(width,height);
		maxX=0;minX=width;
		maxY=0;minY=height;
		display.Invalidate(); 
		display.Update(); 
	    }
        
	    public override void Set(int i, int j,int r, int g, int b){
		base.Set(i,j,r,g,b);
		Color c = Color.FromArgb(r,g,b);

	        image.SetPixel(i,j,c);

		maxX = Math.Max(i,maxX);minX = Math.Min(i,minX);
		maxY = Math.Max(j,maxY);minY = Math.Min(j,minY);

		this.count++;
		if (this.count == RefreshRate){
		    this.count = 0;
		    Application.DoEvents();
		    Graphics graphics = display.CreateGraphics();
		    graphics.DrawImage(image,
				       new Rectangle((minX-1)*Magnification,
						     (minY-1)*Magnification,
						     ((maxX-minX)+3)*Magnification,
						     ((maxY-minY)+3)*Magnification),              
				       new Rectangle(minX-1,minY-1,(maxX-minX)+3,(maxY-minY)+3),
				       GraphicsUnit.Pixel);
		    graphics.Dispose();
		    maxX=0;minX=image.Width;
		    maxY=0;minY=image.Height;
		    if (interrupt) {
			interrupt = false; 
			throw new Interrupt();
		    };
		};
	    }
	    
	    public override void Commit(){
		Graphics graphics = display.CreateGraphics();
		graphics.DrawImage(image,
				   new Rectangle(0,0,image.Width*Magnification,image.Height*Magnification),
				   new Rectangle(0,0,image.Width,image.Height),
				   GraphicsUnit.Pixel);
   	        graphics.Dispose();
		/* uncomment to send output to a .ppm file as well */
		// base.Commit();  
	    }
	    
        }; 
	
        public class DisplayFactoryClass : Classes.FactoryClass {

	    public DisplayFactoryClass(){}
	    
	    public override Classes.ImageClass MakeImage(string name,int x,int y){
		return new DisplayImageClass(name,x,y);
	    }
        };

        private static Bitmap NewBitmap(int Width,int Height) {

            Bitmap bitmap = 
		new Bitmap(Width,Height,PixelFormat.Format24bppRgb); 
            for(int i=0;i<Width;i++){  		
		for(int j=0;j<Height;j++){
	    bitmap.SetPixel(i,j,ImageColor);			
		}
	    };
            return bitmap;
        }
        
	
        public Display() {

            SetStyle(ControlStyles.Opaque, true); 
            Size = InitialSize;
	    BackgroundBrush = new SolidBrush(BackgroundColor);
            Text = "Raytrace Client";
	    SetStyle(ControlStyles.ResizeRedraw, true);
	    
	    // Create the main menu.
	    Menu = new MainMenu();
	    // add a File menu
            MenuItem FileMenu = Menu.MenuItems.Add("&File");
	    // add Open and Interrupt Menu items --- named, so we can disable them
	    openMenuItem = new MenuItem("&Open...", 
					new EventHandler(this.Open),
					Shortcut.CtrlO);
	    FileMenu.MenuItems.Add(openMenuItem);
	    interruptMenuItem = new MenuItem("&Interrupt", 
		                	     new EventHandler(this.Interrupt),
					     Shortcut.CtrlC);
    	    interruptMenuItem.Enabled = false;
	    FileMenu.MenuItems.Add(interruptMenuItem);

	    // add a separator
            FileMenu.MenuItems.Add("-");     
	    // add an Exit item
            FileMenu.MenuItems.Add(new MenuItem("E&xit", 
                                                new EventHandler(this.Exit), 
                                                Shortcut.CtrlX));
	    // add an Options menu and an item to double the scale of the image
            MenuItem OptionsMenu = Menu.MenuItems.Add("&Options");
	    MenuItem ScaleMenuItem = new MenuItem ("&Scale 2:1",new EventHandler(this.Scale));
	    ScaleMenuItem.Checked = true;
	    OptionsMenu.MenuItems.Add(ScaleMenuItem);

            image = NewBitmap(Size.Width,Size.Height);
        }

        private void Open(object sender, EventArgs e){
	    OpenFileDialog openFileDialog = new OpenFileDialog();
	    openFileDialog.InitialDirectory = "gml" ;
	    openFileDialog.Filter = "gml files (*.gml)|*.gml";
	    openFileDialog.FilterIndex = 1 ;
	    if (openFileDialog.ShowDialog() == DialogResult.OK &
		openFileDialog.FileName!= null){
		image = null;
		Invalidate();
		Update();
		Cursor.Current = Cursors.WaitCursor;
		openMenuItem.Enabled = false;
	 	interruptMenuItem.Enabled = true;
		try {Server.raytrace(factoryobject,openFileDialog.FileName);}
		catch (Client.Interrupt) {};
	 	interruptMenuItem.Enabled = false;
		openMenuItem.Enabled = true;
	    }
	}

        private void Exit(object sender, System.EventArgs e) {
	    this.Close();
        }

        private void Interrupt(object sender, System.EventArgs e) {
	    if (!openMenuItem.Enabled) {openMenuItem.Enabled = true;	
				        interruptMenuItem.Enabled = false;
	                                interrupt = true;};
        }

        private void Scale(object sender, System.EventArgs e) {
	    MenuItem ScaleMenuItem = (MenuItem) sender;
	    if (ScaleMenuItem.Checked){
	      Magnification = 1;
	    } else {
	      Magnification = 2;
	    };	
	    MagnificationF = (float) Magnification;
	    ScaleMenuItem.Checked = !ScaleMenuItem.Checked;
	    Invalidate();	
	    Update();
        }

        protected override void OnPaint(PaintEventArgs e) {
	    Graphics g = e.Graphics;
	    Rectangle cliprect = e.ClipRectangle;
            g.FillRectangle(BackgroundBrush, cliprect);
	    if (image != null){
		g.DrawImage(image,cliprect,
			    (cliprect.X)/Magnification,
	                    (cliprect.Y)/Magnification,
			    (cliprect.Width)/Magnification,   			               
	                    (cliprect.Height)/Magnification,
	                    GraphicsUnit.Pixel);
	    };
	    g.Dispose();
            base.OnPaint(e);
        }


        protected override void OnResize(EventArgs e) {
            base.OnResize(e);
        }

        public static void Main() {
	    display = new Display();
            factoryobject = new DisplayFactoryClass(); 
            Application.Run(display);
        }
    }

}






