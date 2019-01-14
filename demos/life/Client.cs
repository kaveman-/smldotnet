using System;
using System.Windows.Forms; 
using System.IO;
//using System.Net;
using System.Drawing;
using System.Drawing.Drawing2D;
using System.Drawing.Imaging;

namespace Client {
    public class Interrupt : System.Exception{};

    public class Display : Form {

        // constants
        private static Color BackgroundColor = Color.Black;
        private static Color LiveColor = Color.HotPink;
        private static int RefreshRate = 32;
        private static int Magnification = 8;
        private static float MagnificationF = Magnification;
        private static int DisplaySize = 32;
        private static Size InitialSize = 
           new Size(Magnification*DisplaySize,Magnification*DisplaySize);


        // local variables
        private MenuItem goMenuItem = null;
        private MenuItem interruptMenuItem = null;

        private static bool interrupt = false;
        private static bool exit = false;
        private static Display display = null;

        private static Classes.FactoryClass factoryobject = null; 

        private static Bitmap image = null;

        private static Brush BackgroundBrush = null;

        public class DisplayImageClass : Classes.ImageClass {
            private int maxX,minX,maxY,minY;
            private int count = 0;

            
            // DisplayImage extends the ML class Classes.ImageClass
            public DisplayImageClass(){
                image = NewBitmap();
                maxX=0;minX=DisplaySize;
                maxY=0;minY=DisplaySize;
                display.Invalidate(); 
                display.Update(); 
            }

            /* this can be simplified ... */
            private void Plot(int i, int j,Color c){ 
                if ((i > 0 & i<DisplaySize) &
                    (j > 0 & j<DisplaySize)) {
         
                image.SetPixel(i,j,c);

                maxX = Math.Max(i,maxX);minX = Math.Min(i,minX);
                maxY = Math.Max(j,maxY);minY = Math.Min(j,minY);

                this.count++;
                if (this.count == RefreshRate){
                    this.count = 0;
                    Application.DoEvents();
                    if (!exit) {
                        Graphics graphics = display.CreateGraphics();
                        graphics.DrawImage(image,
                                           new Rectangle((minX-1)*Magnification,
                                                         (minY-1)*Magnification,
                                                         ((maxX-minX)+3)*Magnification,
                                                         ((maxY-minY)+3)*Magnification),              
                                           new Rectangle(minX-1,minY-1,(maxX-minX)+3,(maxY-minY)+3),
                                           GraphicsUnit.Pixel);
                        graphics.Dispose();
                        maxX=0;minX=DisplaySize;
                        maxY=0;minY=DisplaySize;
                    };
                    if (interrupt) {
                        interrupt = false; 
                        throw new Interrupt();
                    };
                };
               };       
            }

            public override void Set(int i, int j){
                Plot(i,j,LiveColor);
            }

            public override void Clear(int i, int j){
                Plot(i,j,BackgroundColor);
            }

            public override int Size(){
                return DisplaySize;
            }
            
        }; 
        
        public class DisplayFactoryClass : Classes.FactoryClass {

            public DisplayFactoryClass(){}
            
            public override Classes.ImageClass MakeImage(){
                return new DisplayImageClass();
            }
        };

        private static Bitmap NewBitmap() {

            Bitmap bitmap = 
                new Bitmap(DisplaySize,DisplaySize,PixelFormat.Format24bppRgb); 
            for(int i=0;i<DisplaySize;i++){             
                for(int j=0;j<DisplaySize;j++){
            bitmap.SetPixel(i,j,BackgroundColor);                       
                }
            };
            return bitmap;
        }
        
        
        public Display() {

            SetStyle(ControlStyles.Opaque, true); 
            Size = InitialSize;
            BackgroundBrush = new SolidBrush(BackgroundColor);
            Text = "Life";
            FormBorderStyle = FormBorderStyle.FixedToolWindow;
            
            // Create the main menu.
            Menu = new MainMenu();
            // add a File menu
            MenuItem FileMenu = Menu.MenuItems.Add("&File");
            // add Go and Interrupt Menu items --- named, so we can disable them
            goMenuItem = new MenuItem("&Go", 
                                        new EventHandler(this.Go),
                                        Shortcut.CtrlG);
            FileMenu.MenuItems.Add(goMenuItem);
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
            image = NewBitmap();
        }

        private void Go(object sender, EventArgs e){
                goMenuItem.Enabled = false;
                interruptMenuItem.Enabled = true;
                try {Server.life(factoryobject);}
                catch (Client.Interrupt) {};
                interruptMenuItem.Enabled = false;
                goMenuItem.Enabled = true;
        }

        protected override void OnClosing(System.ComponentModel.CancelEventArgs e) {
	    interrupt = true;
            exit = true;
        }
        
        private void Exit(object sender, System.EventArgs e) {
	    interrupt = true;
            this.Close();
        }

        private void Interrupt(object sender, System.EventArgs e) {
            if (!goMenuItem.Enabled) {goMenuItem.Enabled = true;        
                                      interruptMenuItem.Enabled = false;
                                      interrupt = true;};
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






