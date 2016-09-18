cities.csv : historical-us-city-populations/data/1790-2010_MASTER.csv	data-prep.R
	Rscript --vanilla data-prep.R

census.csv: aggregate-data.R
	Rscript --vanilla aggregate-data.r 

clobber:
	rm -f census.csv cities.csv midwest.csv

deploy:
	rsync --progress --delete -avz \
		*.json *.html *.css *.js *.csv \
		reclaim:~/public_html/jasonheppler/projects/midwest/

.PHONY : default clean clobber deploy