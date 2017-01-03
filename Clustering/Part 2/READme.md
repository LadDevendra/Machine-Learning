The required args to the R file:
tweets_kmeans.R <number_of_clusters> <initial_seeds_textfile> <tweets_jsonfile> <output_file>

Packages Used: jsonlite, stringr

How to run:
Rscript --vanilla tweets_kmeans.R 25 InitialSeeds.txt Tweets.json output_check.txt