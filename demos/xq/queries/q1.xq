let bib = document("docs\bib.xml")
do let reviews = document("docs\reviews.xml")
   do let prices = document("docs\prices.xml")
      do bib[for b in bib/book do
              where b/publisher/data() = "Addison-Wesley" do
                book[year[b/year/data()], b/title/data()]]
