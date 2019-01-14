structure Date :> DATE =
struct
  (*@TODO: test in time zones other than GMT *)
   open Int
   open General
   open Option
   open Bool
   open List
   open Datatypes (*@HACK *)
   val op= = Prim.=

   val op^ = String.^
   val valOf = Option.valOf

   datatype weekday = 
      Mon
   |  Tue
   |  Wed
   |  Thu
   |  Fri
   |  Sat
   |  Sun

   datatype month =
      Jan
   |  Feb
   |  Mar
   |  Apr
   |  May
   |  Jun
   |  Jul
   |  Aug
   |  Sep
   |  Oct
   |  Nov
   |  Dec
   
   fun monthToInt month =
   (case month of
      Jan => 1
   |  Feb => 2
   |  Mar => 3
   |  Apr => 4
   |  May => 5
   |  Jun => 6
   |  Jul => 7
   |  Aug => 8
   |  Sep => 9
   |  Oct => 10
   |  Nov => 11
   |  Dec => 12
   )   

   fun intToMonth i =
   case i of
      1 => Jan
   |  2 => Feb 
   |  3 => Mar 
   |  4 => Apr 
   |  5 => May 
   |  6 => Jun 
   |  7 => Jul 
   |  8 => Aug 
   |  9 => Sep 
   |  10 => Oct 
   |  11 => Nov
   |  12 => Dec

   fun intToDay i =
   case i of
      0 => Sun
   |  1 => Mon
   |  2 => Tue
   |  3 => Wed
   |  4 => Thu
   |  5 => Fri
   |  6 => Sat

  type date = System.DateTime * Time.time option 
            



  exception Date


  fun date {year,month,day,hour,minute,second,offset=offsetOpt} =
      let val ci = valOf(System.Globalization.CultureInfo.get_InvariantCulture())        
          val cal = valOf(ci.#get_Calendar()) 
          val dt = System.DateTime(year,monthToInt month,1,0,0,0)
          val dt = cal.#AddDays(dt,day-1)
          val dt = cal.#AddHours(dt,hour)
          val dt = cal.#AddMinutes(dt,minute)
          val dt = cal.#AddSeconds(dt,second)
      in case offsetOpt of 
           NONE => (dt,NONE)
         | SOME offset =>
               let
                   val seconds = Time.toSeconds(offset)
                   val twentyFourHours = Int64.fromInt(86400)
                   val normOffset = Time.fromSeconds(Int64.rem(seconds,twentyFourHours))
                   val dt = cal.#AddDays
                       (dt,Int64.toInt(Int64.quot(seconds,
                                                  twentyFourHours)))
               in
                   (dt,SOME normOffset)
               end
      end handle _ => raise Date
    
  fun year ((d,_) : date) = d.#get_Year()
  fun month ((d,_) : date) = intToMonth (d.#get_Month())
  fun day ((d,_) : date) = d.#get_Day()
  fun minute ((d,_) : date) = d.#get_Minute()
  fun second ((d,_) : date) = d.#get_Second()
  fun hour ((d,_) : date) = d.#get_Hour()
  fun yearDay ((d,_) : date) = d.#get_DayOfYear()-1
  fun offset ((_,offset) : date) = offset

  (* we only return DST information for a time in the local time zone *)
  fun isDst ((d,offset) : date) = 
      case offset of
        NONE => (case System.TimeZone.get_CurrentTimeZone() of
                       SOME timeZone => (* should always succeed *)
                         SOME (timeZone.#IsDaylightSavingTime(d))
                     | NONE => 
                         NONE)
      | SOME time => NONE
      
  (*TODO: review *)
  fun weekDay ((d,_) : date) = intToDay (case d.#get_DayOfWeek() of 
                                         System.DayOfWeek i => i)

  (* its the user's responsibility to ensure the offsets are equal *)
  fun compare ((d1,_) : date, (d2,_) : date) = 
    let val n = System.DateTime.Compare(d1,d2)
    in if n < 0 then LESS else if n = 0 then EQUAL else GREATER end

  (*@TODO: review *)
  fun localOffset () = 
      let val SOME tz = System.TimeZone.get_CurrentTimeZone()
          val timespan = tz.#GetUtcOffset(System.DateTime(0:Int64.int))
          val offset = Int64.div(timespan.#get_Ticks(),10)
      in
          if Int64.>(offset,0) then 
              let val twentyFourHours = Time.fromSeconds(86400) 
              in
                  Time.-(twentyFourHours,Time.fromMicroseconds(offset))
              end
          else 
              Time.fromMicroseconds(Int64.~(offset))
      end


  fun fromTimeUniv t = 
      (System.DateTime(Prim.add(PrimUtils_.epoch.#get_Ticks(),
                                Prim.mul(Time.toMicroseconds t,
                                         10))),
       SOME Time.zeroTime)

  (*@TODO: review *)
  fun fromTimeLocal t = 
      (System.DateTime(Prim.add(PrimUtils_.epoch.#get_Ticks(),
                                Prim.mul(Time.toMicroseconds t,
                                         10)))
       .#ToLocalTime(),
       NONE)



   val YearOverflow=Fail 
      "In Date structure, years before 0 or after 9999 cannot be handled"

   (* Functions for appending a 1,2,3 or 4 digit number to a StringBuffer_,
      including the zeros.  Only int4 does bounds checking, because
      it is used for years. *)
   fun int1(output,n)=
      StringBuffer_.appendChar(output,Char.chr(Char.ord(#"0") + n))
   fun int2(output,n)=(int1(output,Int.quot(n,10));int1(output,Int.rem(n,10)))
   fun int3(output,n)=
      (int1(output,Int.quot(n,100));int2(output,Int.rem(n,100)))
   fun int4(output,n)=
   let
      val ()= 
         if n<0 orelse n>=10000
         then
            raise YearOverflow
         else
            ()
   in
      (int1(output,Int.quot(n,1000));int3(output,Int.rem(n,1000)))
   end 

   fun weekDayInt ((d,offset):date) = case d.#get_DayOfWeek() of 
                                       System.DayOfWeek i => i

   fun weekYear ((d,offset):date) =
        let val ci = valOf(
                 System.Globalization.CultureInfo.get_InvariantCulture())     
            val cal = valOf(ci.#get_Calendar()) 
        in
            cal.#GetWeekOfYear(d,
                               System.Globalization.CalendarWeekRule.FirstFullWeek,
                               System.DayOfWeek.Sunday
                               )
        end

   fun weekYearMonday ((d,offset):date) =
        let val ci = valOf(
                 System.Globalization.CultureInfo.get_InvariantCulture())     
            val cal = valOf(ci.#get_Calendar()) 
        in
            cal.#GetWeekOfYear(d,
                               System.Globalization.CalendarWeekRule.FirstFullWeek,
                               System.DayOfWeek.Monday
                               )
        end



   fun fmt (s:string) (date as (datetime,offset):date)=
   let
      (* A % at the end of the format string is ignored. *)

      val guessed_length=String.size s + Int.quot(String.size s,2)
      val output=StringBuffer_.emptyWith(guessed_length)


      fun out string=StringBuffer_.appendString(output,string)
      fun outOpt (SOME string) =StringBuffer_.appendString(output,string)
        | outOpt  NONE = ()
      fun do_escape ch=
         (case ch of
            #"a" => outOpt(datetime.#ToString("ddd"))
         |  #"A" => outOpt(datetime.#ToString("dddd"))
         |  #"b" => outOpt(datetime.#ToString("MMM"))
         |  #"B" => outOpt(datetime.#ToString("MMMM"))
         |  #"c" => outOpt(datetime.#ToString("g"))
         |  #"d" => outOpt(datetime.#ToString("dd"))
         |  #"H" => outOpt(datetime.#ToString("H"))
         |  #"I" => outOpt(datetime.#ToString("h"))
         |  #"j" => int3(output,datetime.#get_DayOfYear())
         |  #"m" => outOpt(datetime.#ToString("MM"))
         |  #"M" => outOpt(datetime.#ToString("M"))
         |  #"p" => outOpt(datetime.#ToString("tt"))
         |  #"S" => outOpt(datetime.#ToString("s"))
         |  #"U" => int2(output,weekYear date)
         |  #"w" => int1(output,weekDayInt date)
         |  #"W" => int2(output,weekYearMonday date)
         |  #"x" => outOpt(datetime.#ToString("D")) (* long date pattern *)
         |  #"X" => outOpt(datetime.#ToString("T")) (* long time pattern *)
         |  #"y" => outOpt(datetime.#ToString("yy"))
         |  #"Y" => outOpt(datetime.#ToString("yyyy"))
         |  #"Z" => (case System.TimeZone.get_CurrentTimeZone() of
                       NONE => ()
                     | SOME tz => outOpt(tz.#get_StandardName()))
         |  c    => StringBuffer_.appendChar(output,c)
            (* also covers %% *)
         )

      fun dochar i=
      (* Handle from char i of the string. Raise subscript when finished.
         *)
      let
         val ch=String.sub(s,i)
      in
         if ch= #"%"
         then
           (do_escape(String.sub(s,i+1));
            dochar(i+2))
         else
           (StringBuffer_.appendChar(output,ch);
            dochar(i+1))
      end

      val ()= dochar 0 handle Subscript => ()
   in
      StringBuffer_.toString output
   end
   
   val format = "ddd MMM dd HH:mm:ss yyyy" 
   
   fun toString ((datetime,offset):date) = 
       let val ci = valOf(System.Globalization.CultureInfo.get_InvariantCulture())
       in
           valOf(datetime.#ToString(format,ci))
       end

   fun fromStringSansWS(s) = 
       let val ci = valOf(System.Globalization.CultureInfo.get_InvariantCulture())
       in
           (case System.DateTime.ParseExact(s,format,ci) of
              datetime => SOME ((datetime,NONE):date)
            )
       end handle _ => NONE


   exception readDateEx (* Raised when we fail to read a date *)

   fun fromString s=
   let
      fun first_nonspace i=
         if Char.isSpace(String.sub(s,i))
         then
            first_nonspace(i+1)
         else
            i
      val s = String.substring(s, first_nonspace 0,String.size s)
   in
      fromStringSansWS(s)
   end handle readDateEx => NONE | Subscript => NONE

   fun scan getc src=
   let
      val src=StringCvt.skipWS getc src
      (* Get 24 characters *)
      val output=StringBuffer_.emptyWith 24
      fun doget(src,i)=
         if i=0 
         then
            src
         else
            (case getc src of
               NONE => raise readDateEx
            |  SOME(c,src) => 
                 (StringBuffer_.appendChar(output,c);
                  doget(src,i-1)
                  )
            )
      val src=doget(src,24)
   in
      case fromStringSansWS(StringBuffer_.toString output) of
             SOME date => SOME (date,src)
           | NONE  => NONE
   end handle readDateEx => NONE

  fun toTime ((datetime,offset):date) = 
      let val offset = case offset of NONE => localOffset() | SOME t => t
          val twelveHours = Time.fromSeconds(43200)
          val twentyFourHours = Time.fromSeconds(86400)
          val t =  Time.fromMicroseconds(Int64.div
                                         (Prim.sub(datetime.#get_Ticks(),
                                                   PrimUtils_.epoch.#get_Ticks()),
                                                                                  10))
          val t =  
              if Time.<=(Time.zeroTime,offset)
                         andalso Time.<(offset,twelveHours)
                         then Time.+(t,offset)
                      else Time.-(t,Time.-(twentyFourHours,offset))
      in        
          t
      end


end (* struct *)
