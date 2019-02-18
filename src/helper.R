##' closest.match
##' @param string string to look for
##' @param stringVector vector in which closest match needs to be found
##' @details Finds the string in a vector that matches best with the one provided.
##' @export
##' @importFrom stringdist amatch
closest.match <- function(string, stringVector){
    stringVector[amatch(string, stringVector, maxDist=Inf)]
}

##' translate.word
##' @param df data.frame
##' @param x word to be replace
##' @param y replacement
##' @details Replaces a word with another word in entire data frame.
##' @export
translate.word <- function(df,x,y){data.frame(lapply(df, function(z) {if(length(grep(x,z)>0)){gsub(x, y, z)}else{z}}))} # if statement to keep original class (no transform to character/factor)

##' translate.df
##' @param df data.frame
##' @details Function for online survey. Translates answers from French to English and corrects English typos.
##' @export
translate.df <- function(df, to='eng'){
    if(to=='eng'){
        df <- translate.word(df,"Non","No")
        df <- translate.word(df,"Oui","Yes")
        
        df <- translate.word(df,"ligne ? main","rod and line")
        df <- translate.word(df,"ligne à main","rod and line")
        df <- translate.word(df,"filet maillant","gillnet")
        df <- translate.word(df,"turlutte","jigger")
        df <- translate.word(df,"\\(|\\)","")
        df <- translate.word(df," including mechanized jigger","")
        df <- translate.word(df,"jigger ?lectronique","jigger")
        df <- translate.word(df,"jigger électronique","jigger")
        df <- translate.word(df,"chalut p?lagique","pelagic trawl")
        df <- translate.word(df,"chalut pélagique","pelagic trawl")
        df <- translate.word(df,"trappe","trap")
        df <- translate.word(df,"autres sennes","other seine")
    
        df <- translate.word(df,"Je ne me rappelle pas","I do not remember")
    
        df <- translate.word(df,"homard","lobster")
        df <- translate.word(df,"thon","tuna")
        df <- translate.word(df,"fl?tan","halibut")
        df <- translate.word(df,"flétan","halibut")
        df <- translate.word(df,"fletan","halibut")
        df <- translate.word(df,"groundfish","ground fish")
        df <- translate.word(df,"poisson de fond","ground fish")
        df <- translate.word(df,"crabe de neige","snow crab")
        df <- translate.word(df,"buccin","whelk")
        df <- translate.word(df,"bar rayé","striped bass")
        
        df <- translate.word(df,"Récréative","Recreational")
        df <- translate.word(df,"Une combinaison","A combination of these")
        df <- translate.word(df,"Appât Boëtte","Bait")
        df <- translate.word(df,"Appât (Boëtte)","Bait")
        df <- translate.word(df,"Commerciale","Commercial")
    }
    if(to=='fr'){
        df <- translate.word(df,"jigger","turlutte")
        df <- translate.word(df,"rod and line","canne à pêche")
        df <- translate.word(df,"gillet","filet maillant")
        df <- translate.word(df,"trap","trappe")
        df <- translate.word(df,"seiner","senne")
        df <- translate.word(df,"handline","ligne à main")
        df <- translate.word(df,"Recreational","Récréative")
        df <- translate.word(df,"Educational","Educative")
        df <- translate.word(df,"Commercial","Commerciale")
        df <- translate.word(df,"Bait","Appât")
        df <- translate.word(df,"A combination of these","Combinaison")
        df <- translate.word(df,'PEI','IPE')
        df <- translate.word(df,'NL','TN')
        df <- translate.word(df,'NS','NE')
        df <- translate.word(df,'TOTAL','TOTALE')
        df <- translate.word(df,"lobster","homard")
        df <- translate.word(df,"tuna","thon")
        df <- translate.word(df,"snow crab","crabe de neige")
        df <- translate.word(df,"halibut","rock crab")
        df <- translate.word(df,"striped bass","bar rayé")
        df <- translate.word(df,"green crab","crabe vert")
        df <- translate.word(df,"shrimp","crevette")
        df <- translate.word(df,"whelk","buccin")
        df <- translate.word(df,"ground fish","poisson de fond")
        df <- translate.word(df,'Unknown','Inconnu')
        df <- translate.word(df,'Undeclared','Non declarées')
        df <- translate.word(df,'Declared','Declarées')
        df <- translate.word(df,'Other','Autre')
        df <- translate.word(df,'Yes','Oui')
        df <- translate.word(df,'No','Non')
        df <- translate.word(df,'I do not remember','Je ne me rappelle pas')
    }
    return(df)
}

##' get.stats
##' @param df data.frame
##' @param id factor
##' @details gives list of numbers and percentage of id by Year
##' @export
##' @importFrom stringdist amatch
get.stats <- function(df,id){
    x = count(df,c(id,'Year'))
    names(x)[1] = 'id'
    counts = dcast(x,Year~id,value.var = 'freq')
    perc = cbind(Year=counts[,1],round(counts[,-1]/rowSums(counts[,-1],na.rm = T)*100,0))
    list('count' = counts, 'percentage' = perc)
}

##' count.char
##' @param pattern character pattern
##' @param x character string
##' @details gives list of numbers and percentage of id by Year
##' @export
##' @importFrom stringdist amatch
count.char <- function(pattern,x){length(grep(pattern,x))}

##' Maximum numbers
##' @param x vector
##' @param N Numbers of top
##' @details Gives the top N values of a vector
##' @export
maxN <- function(x, N=2){
    x <- x[!is.na(x)]
    len <- length(x)
    if(N>len){
        warning('N greater than length(x).  Setting N=length(x)')
        N <- length(x)
    }
    head(sort(x,decreasing = T),N)
}

