using System; 
namespace Classes {
public struct Pair {
        public int x; /*  mutable! */
        public int y; /*  mutable! */

        /* constructor */
        public Pair(int i,int j){
            x = i;
            y = j;
        }

        /* a functional swap, returns a new pair */
        public Pair swap(){
            return new Pair(y,x);     
        }

        /* destructive swap, modifies an existing pair called this */
        public void invert(){
            int t; 
            t = this.x;
            this.x = y;
            this.y = t;
            return;
        }

}
}








