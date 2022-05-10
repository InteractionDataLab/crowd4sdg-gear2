#Custom functions - including but not limited to plotting and data processing - RATHIN


########################################################################################

#Functions to process Slack JSON

#Cleans to .CSV:
  
# a. texts
# b. emoticon reactions
# c. text mentions
# d. replies
# e. channel joins
# f. attachments (to do)

#Note: Replies, Reactions are listed only when the source message was created by a user and not a bot. 
#That case will be included when analysing such an event is relevant.

processSlack_Daily = function(content)
{
  fields = colnames(content)
  
  texts = data.frame()
  emoticons = data.frame()
  text_mentions = data.frame()
  channel_joins = data.frame()
  bot_joins = data.frame()
  replies = data.frame()
  
  if ("subtype" %in% fields)
  {
    channel_joins = content %>% filter(subtype == "channel_join")
    if (nrow(channel_joins) > 0)
    {
      channel_joins = channel_joins  %>% select( "ts", "user") 
      colnames(channel_joins) = c("timestamp", "user")
    }
    
    # #if the workspace has a bot to greet users when they join - define the greet bot IDs at the beginning
    # #If there are no greet bots - this is skipped.
    # 
    # bot_joins = content %>%  filter(subtype == "bot_message")
    # 
    # if (nrow(bot_joins) > 0)
    # {
    #   
    #   if ("bot_id" %in% colnames(bot_joins))
    #   {  
    #     bot_joins = bot_joins %>% select("ts", "bot_id", "text") %>% filter(bot_id %in% greet_bots)
    #     
    #     if (nrow(bot_joins) > 0)
    #     {
    #       bot_joins = bot_joins %>% mutate(mentions = list(process_texts(text))) %>% as_tibble()
    #       bot_joins = bot_joins %>% select(-c("user", "text", "bot_id"))
    #       colnames(bot_joins) = c("timestamp", "mentions")  
    #       channel_joins = rbind(channel_joins, bot_joins)
    #     }
    #   }  
    # }
    # 
    # #This confusing block under the two comment lines is a special case to hande the "Greetbot" in a workspace I had to analyze.
    # #In my sample workspace - the greet bot pings the new user using '@' in a message. The many "if" cases are because the default
    # #slack helper is a bot - but does not have a bot id - it has a user id instead (which is weird)
    
    content_clean =  content %>% filter(is.na(subtype)) 
    
  }
  
  if (!"subtype" %in% fields)
  {
    content_clean = content
  }
  
  if (nrow(content_clean) > 0)
  {
    texts = content_clean %>% select("ts", "user", "text") 
    colnames(texts) = c("timestamp", "user", "message")       
    
    text_mentions = content_clean %>% select("ts", "user", "text") %>% rowwise() %>% mutate(cited = list(process_texts(text))) %>% as_tibble()
    text_mentions = unnest(text_mentions, cols = "cited") %>% select(-text)
    colnames(text_mentions) = c("timestamp", "message_creator", "cited_user")
    
    if ("reactions" %in% fields)
    {
      emoticons = content_clean %>% select("ts", "user", "reactions")
      emoticons = unnest(emoticons, cols = "reactions")
      
      if (nrow(emoticons) > 0)
      {
        colnames(emoticons) = c("timestamp", "message_creator", "emoticon_name", "reacted_by", "count")  
        emoticons = as.data.frame(emoticons)
        emoticons = unnest(emoticons, cols = "reacted_by") %>% select(-c("count"))
      }
    }
    
    if ("replies" %in% fields)
    {
      replies = content_clean %>% select("user", "ts", "replies") 
      colnames(replies) = c("parent_user", "parent_message_timestamp", "replies")
      replies = unnest(replies, cols = c("replies"))
      colnames(replies) = c("parent_user", "parent_message_timestamp", "reply_user", "reply_message_timestamp")
    } 
  }
  
  list = list()
  list[[1]] = as.data.frame(texts)
  list[[2]] = as.data.frame(emoticons)
  list[[3]] = as.data.frame(text_mentions)
  list[[4]] = as.data.frame(channel_joins)
  list[[5]] = as.data.frame(replies)
  
  return(list)
  
}

process_texts = function(text)
{
  list = c()
  
  list = c(list, str_extract_all(text, regex("<@[:alnum:]+>"))[[1]])
  list = c(list, str_extract_all(text, regex("<![:alnum:]+>"))[[1]])
  
  list = gsub("<", "", list)
  list = gsub(">", "", list)
  list = gsub("@", "", list)
  list = gsub("!", "", list)
  
  return(list)
}

######################################################################################

#Function to create a Bipartite Projection

library(igraph)

biproj_custom = function(df)
{
  colnames(df) = c("id", "variable")
  df = df[!is.na(df$variable),]
  
  vertices = data.frame(node = unique(df$id), type = TRUE)
  vertices = rbind(vertices, data.frame(node = unique(df$variable), type = FALSE))
  
  g = graph_from_data_frame(df, vertices = vertices)
  proj = bipartite.projection(g)
  
  return(proj)
}

#######################################################################################

#Function to clean multiple responses

clean_split_mcq = function(df)
{
  colnames(df) = c("id", "variable")
  
  df$variable = gsub("\\s*\\([^\\)]+\\)","",as.character(df$variable)) #Remove all text inside brackets - this is a specific case in the dataset
  df$variable = as.character(df$variable) #Convert to character
  
  proc = df %>% rowwise() %>% mutate(variable = list(trimws(strsplit(variable, ",")[[1]])))
  proc = unnest(proc, cols = c("variable"))
  
  return(proc)
}

###################################################################################

#Variation of Marc's plotNetwork

library(qgraph)

plotNetwork <- function(Gsub, layout='default',maine='', coms=NULL, hide = NULL, label = '', colors = FALSE){
  
  
  if (is.null(coms)){
    com <- cluster_walktrap(Gsub)
    V(Gsub)$coms = com
    V(Gsub)$color <- com$membership+1
  } else if(colors == FALSE) {
    #color = fish(n = length(unique(coms)), option = "Koumansetta_rainfordi", end = 0.9)
    color = pals::alphabet(length(unique(coms)))
    
    names(color) = unique(coms)
    
    if (!is.null(hide))
    {
      color[hide] = NA 
    }
    
    V(Gsub)$color <- color[coms]
  }
  else{
    V(Gsub)$color = coms
  }
  
  vertex.frame.color <- V(Gsub)$color
  
  size <- abs(strength(Gsub, loops=T))
  if (max(size, na.rm=T)>0){ 
    size <- 30 * size / max(size, na.rm=T)
  } else {
    size <- 1
  }
  
  if (is.null(E(Gsub)$weight)){
    width <- 1
    edge.color <- NA 
    l <- layout_with_fr
    
  } else{
    weight <- abs(E(Gsub)$weight)
    width <- .01 + 2 * weight / max(weight, na.rm=T)
    
    if ("hidden" %in% names(edge.attributes(Gsub)))
      edge.color <- ifelse(E(Gsub)$hidden == TRUE, rgb(alpha = 0, red = 0, green = 0, blue = 0), 'grey')
    else
      edge.color = 'grey'
    
    e <- get.edgelist(Gsub, names=F)
    e <- cbind(e, E(Gsub)$weight)
    l <- qgraph.layout.fruchtermanreingold(e,
                                           vcount=vcount(Gsub),
                                           area=1*(vcount(Gsub)^2),
                                           repulse.rad=5*(vcount(Gsub)^2))
    
  }
  
  if (layout!='default'){
    l <- layout
  }
  
  plot(Gsub,
       layout=l,
       edge.width = width,
       edge.color=edge.color,
       vertex.label = label,
       vertex.label.color = "black",
       vertex.label.cex = 0.7,
       vertex.size = 3 * sqrt(size),
       edge.curved=0.1,
       vertex.frame.color = vertex.frame.color, 
       edge.arrow.width = 0.1,
       edge.arrow.size = 0.1,
       #main=maine,
       #main.cex = 0.3,
       col='gray'
  )
  
  title(main = maine , cex.main = 0.7)
  #legend(x=-1.5, y=-1.1, legend = names(color), pch=21,
  #       col="#777777", pt.bg=color, pt.cex=2, cex=.8, bty="n", ncol=3)
  
}


########################################################

#Interactive Network

library(visNetwork)

visPlot = function(subgraph, communities = rep(1, length(V(subgraph))), color = "auto", nodesize = 10, edgewidth = 0, title="", 
                   textsize = nodesize, layout = "layout_nicely", directed = TRUE, hidden = rep(FALSE, ecount(subgraph)), seed = 123)
{
  nodes <- data.frame(id = V(subgraph)$name, name = V(subgraph)$name, group = communities)
  nodes$font.size<-textsize 
  if (!color == "auto")
    nodes$color = communities
  nodes$font.color = "grey"
  nodes$font.face = "calibri"
  nodes$label = nodes$name
  nodes$value = nodesize
  edges <- data.frame(get.edgelist(subgraph))
  edges$width = edgewidth
  
  hidden[hidden == FALSE] = "lightblue"
  hidden[hidden == TRUE] = "white"
  edges$color = hidden
  
  colnames(edges)<-c("from","to","width", "color")
  plt = visNetwork(nodes, edges, height = "1000px", width = "100%",main = title)%>%
    visIgraphLayout(layout = layout) %>% visLayout(randomSeed = seed) %>% 
    visPhysics(solver = "repulsion", hierarchicalRepulsion = "nodeDistance") %>%
    visOptions(highlightNearest = TRUE) %>%
    visNodes(scaling = list(min = 10, max = 50)) %>%
    visEdges(smooth = T) %>% #, color = list(color = "lightblue", highlight = "blue")) %>% 
    visGroups(groupname = "2", color = list(background = "lightgray",border = "black")) %>%
    visInteraction(keyboard = T,
                   dragNodes = T, 
                   dragView = T, 
                   zoomView = T)
  
  if (directed == TRUE)
    plt = plt %>% visEdges(arrows ="to")
  
  return(plt)
}

#################################################################

#Finding edges to hide in a network

calculate_hidden = function(g, key = "")
{
  vec = ends(g, es = E(g))
  list = c() 
  
  for (i in 1:nrow(vec))
  {
    if (vec[i,1] %in% key | vec[i,2] %in% key)
      list = c(list, TRUE)
    else
      list = c(list, FALSE)
  }
  
  return(list)
}

####################################################################

#Standard Error

se = function(list)
{
  return(sd(list, na.rm = TRUE)/sqrt(length(list[!is.na(list)])))
}

#####################################################################

