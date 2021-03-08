This submission is  a little out of my comfort zone. 
I have made a story to tell a journey through the exploration of missing data. 
To do this, I used a scrolly-telling technique.

the mechanics of scrolly-telling are new to me, and as such, i understand it could be more visually appealing. 
HoweverI thought i would try it out as a new medium to me, for exploring the combination of graphs for telling an evolving story.

I have also attached my rough R code and png files.
You will see in this that the multiple imputation part is far from perfect, too. 
MI usually runs the analysis on the multiple datasets and combines the estimates arising from this. 
However here as I was just trying to describe the data on a graph, i found that the pool() option was inapropriate. 
Therefore although i present this as MI, the graphed data is actially based on single imputation. 
I didnt have the time to figure out how to combine raw "imputed" datasets - so that section stands as an example.

I hope you enjoy this, but this readme stands as a explaination of me knowing that there are many aspects which could have been better immplemented. 
I hope you are able to bear this in mind with your comments on the graphics.

one graphic which could certainly be improved is the MAR exploration graphic. 
This function makes a HUGE crosstabs of ALL possible results. I instead extracted a few graphs and ggarrang()ed them into something sensible. 
However this has limitations in its own right. I needed to remove the key from each graph and axis title and them ggtext some titles over the top. 
There are easy criticisms to make for this graph, but I hope yopu can overlook the obvious.

thanks

Pip

Vis Link:
https://wonderful-wednesday-missing-data.webflow.io/

Extra link for LOLs:
https://youtu.be/sIlNIVXpIns 