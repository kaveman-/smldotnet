structure TextIO = struct
	open TextIO 
	val crlf = "\r\n";
	fun lf2crlf s = String.translate (fn #"\n" => crlf | c => String.str c) s
	val print = fn s => print (lf2crlf s);
	val output = fn (os,s) => output(os,lf2crlf s)
	val output1 = fn (os,c) => if c = #"\n" then output(os,crlf) else output1(os,c);
end 


