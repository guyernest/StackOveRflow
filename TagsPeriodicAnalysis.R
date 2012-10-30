# In: http://data.stackexchange.com/
#SELECT --TOP 20 
#TagName,
#COUNT(*) AS UpVotes, 
#datediff(month, 0, Posts.CreationDate) as [Month]
#FROM Tags
#INNER JOIN PostTags ON PostTags.TagId = Tags.id
#INNER JOIN Posts ON Posts.ParentId = PostTags.PostId
#INNER JOIN Votes ON Votes.PostId = Posts.Id and VoteTypeId = 2
#GROUP BY TagName, datediff(month, 0, Posts.CreationDate)
#ORDER BY UpVotes DESC

topTagCount <- topTagCount[with(topTagCount, order(-popularity)),]
progTags <- c("vb.net", "c#", "c++", "java", "javascript", "php", "python", "c", "objective-c", "ruby", "perl", "delphi", "r")
dbTags <- c("mysql", "oracle", "sql-server", "mongodb", "postgresql")
awsTags <- c("amazon-s3", "amazon-ec2", "amazon-simpledb", "amazon-cloudfront", "amazon-dynamodb", "amazon-rds")
topTagsPerMonth <- subset(TagsPerMonth, TagName %in% awsTags)

library(plyr)
monthTotal <- ddply(topTagsPerMonth, ~Month, summarize, totalMonth = sum(UpVotes))
tagTotal <- ddply(topTagsPerMonth, ~TagName, summarize, totalTag = sum(UpVotes))
topTagsPerMonth <- merge(topTagsPerMonth, monthTotal)
topTagsPerMonth <- merge(topTagsPerMonth, tagTotal)
topTagsPerMonth$Percent <- topTagsPerMonth$UpVotes / topTagsPerMonth$totalMonth
topTagsPerMonth <- topTagsPerMonth[topTagsPerMonth$Month > 1303,]
topTagsPerMonth$MonthDate <- as.Date(as.yearmon(1900+topTagsPerMonth$Month/12))

library(ggplot2)
tile <- ggplot(aes(x = reorder(TagName, totalTag), y = MonthDate), data = topTagsPerMonth)
tile <- tile + labs(x="Tag",y="Date")
tile <- tile + geom_tile(aes(fill = Percent)) + scale_fill_gradient(low="white", high="blue")
#tile <- tile + scale_y_continuous(name="Date")
tile + opts(axis.text.x=theme_text(angle=90)) + coord_flip()
