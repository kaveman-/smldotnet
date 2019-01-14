let bib = document("docs\bib.xml")
do let reviews = document("docs\reviews.xml")
   do let prices = document("docs\prices.xml")
      do let writtenby(b;a) = 
            if empty(for w in b/author do where w=a do a) then false else true
         in 
         for a in distinct(bib/book/author) do
            result[a, for b in bib/book do where writtenby(b;a) do b/title]

