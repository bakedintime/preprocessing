host <- "localhost:27017"
username <- ""
password <- ""
db <- "mp"
mongo <- mongo.create(host=host , db=db, username=username, password=password)

collection <- "preprocess"
namespace <- paste(db, collection, sep=".")

mongo.count(mongo, namespace, mongo.bson.empty())

#lets create a document to insert
b <- mongo.bson.from.list(list(platform="Compose", language="R", number=1))

#insert the document into our namespace
ok <- mongo.insert(mongo, namespace, b)
