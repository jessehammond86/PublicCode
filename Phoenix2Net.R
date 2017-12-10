#' @param start_date start date of time period as Ymd-format integer (ex:
#'          June 1, 2014 as 20140601).
#' @param end_date end date of time period as Ymd-format integer (ex:
#'          June 1, 2014 as 20140601).
#' @param level level of event granularity ('eventcode', 'root_code',
#'           'quad_class', or 'any'). 'Eventcode' creates a network
#'           for each of the 226 sub-codes in CAMEO. 'root_code' creates a network
#'           for each of the 20 event root codes in CAMEO. 'quad_class' creates a
#'           network for each of the 0-4 quad_class codes in CAMEO. 'Any'
#'           indicates that all connections are used as ties.
#' @param source data source used. Valid arguments include 'nyt' (New York Times)
#'          , 'fbis' (Foreign Broadcast Info Service), 'swb' (Summary of
#'          World Broadcasts), or 'all' (everything). Defaults to 'all'.
#' @param actorset set of actors for which to create event-networks. Defaults
#'          to the 255 ISO-coded states in the international system. Specifying
#'          a specific state or set of states (as 3-character ISO codes) will
#'          extract all the 'major' actors within that state/states.
#' @param code_set subset of event codes as specified by 'level'. This is useful
#'          if you desire to extract only a portion of interactions recorded
#'          by CAMEO, but has to align with the code aggregation specified
#'          in the 'level' argument. For example, if you specify 'root_code',
#'          the 'code_set' you specify has to be one or more root codes between
#'          1 and 20. Defaults to 'all'.
#' @param code_subset subset of EVENTCODES that can be aggregated up to higher
#'          order interactions. For example, you might want to only look at
#'          event codes below 100, but then aggregate those event codes to
#'          root_code or quad_class.
#' @param time_window temporal window to build event-networks. Valid
#'          entries are 'day', 'week', 'month', or 'year'.
#' @param tie_type type of ties to return. Default is binarized ties where
#'          a tie represents the presence of one OR MORE interactions in the
#'          time period specified. Valid entries are 'binary', 'count'
#'          (count of events), 'meangoldstein' (mean Goldstein score),
#'          'sepgoldstein' (mean positive/negative Goldstein scores separated).
#' @param sources use only Phoenix or ICEWS data in creating event networks.
#'          Valid entries are 'phoenix', 'icews', or 'both' (default).

Phoenix2Net <- function(start_date, end_date
                        , input_data
                        , source = 'all'
                        , level = 'quad_class'
                        , actorset = 'states'
                        , code_set = 'all'
                        , code_subset = 'all'
                        , tie_type = 'count'){



  ######
  #
  # Pre-process PHOENIX data variables
  #
  ######

  input_data <- input_data[!is.na(year)]
  input_data[, eventdate := as.Date(paste(year, month, day), format = '%Y %m %d')]


  ######
  #
  # De-duplicate entries
  #
  ######

  input_data <- input_data[!duplicated(input_data[, list(eventdate, source, target, code)])]

  ######
  #
  # Set up some initial values: Time windows
  #
  ######
  ## Date objects
  if (class(start_date) %in% c('numeric', 'integer')
      | class(end_date) %in% c('numeric', 'integer')){
    start_date <- as.Date(lubridate::ymd(start_date))
    end_date <- as.Date(lubridate::ymd(end_date))
  }

  ######
  #
  # Subset Phoenix data by desired date range
  #
  ######

  input_data <- input_data[eventdate >= start_date & eventdate <= end_date]


  ######
  #
  # Set up some initial values: Actors
  #
  ######

  ## Paste-function that can handle NA entries
  ## (http://stackoverflow.com/questions/13673894/suppress-nas-in-paste)
  paste3 <- function(...,sep=", ") {
    L <- list(...)
    L <- lapply(L,function(x) {x[is.na(x)] <- ""; x})
    ret <-gsub(paste0("(^",sep,"|",sep,"$)"),"",
               gsub(paste0(sep,sep),sep,
                    do.call(paste,c(L,list(sep=sep)))))
    is.na(ret) <- ret==""
    ret
  }

  ## Default actors: 255 ISO-coded countries
  if('states' %in% actorset){
    # Set up set of primary actor codes
    statelist <- unique(countrycode::countrycode_data$iso3c)
    statelist <- statelist[!is.na(statelist)]
    statelist <- c(statelist, 'KSV')
    actors <- as.factor(sort(statelist))
    n <- length(actors)

  } else {
    ## Set up set of secondary actor codes
    secondary_actors <- c('GOV', 'MIL', 'REB', 'OPP', 'PTY', 'COP', 'JUD'
                          , 'SPY', 'MED', 'EDU', 'BUS', 'CRM', 'CVL')
    statelist <- countrycode::countrycode_data$iso3c
    statelist <- statelist[!is.na(statelist)]
    actors <- unique(statelist[statelist %in% actorset])
    actors <- actors[!is.na(actors)]
    actors <- unique(as.vector(outer(actors, secondary_actors, paste, sep = '')))
    actors <- as.factor(sort(actors))
    n <- length(actors)
  }

  ######
  #
  # Set up some initial values: Event codes
  #
  ######

  ## Factor variables describing CAMEO categories
  if(level == 'root_code'){
    codes <- factor(1:20)
    levels(codes) <- as.character(1:20)
  } else if(level == 'eventcode'){
    codes <- factor(1:298)
    levels(codes) <- as.character(
      c(10:21, 211:214, 22:23, 231:234, 24, 241:244, 25, 251:256, 26:28, 30:31
        , 311:314, 32:33, 331:334, 34, 341:344, 35, 351:356, 36:46, 50:57
        , 60:64, 70:75, 80:81, 811:814, 82:83, 831:834, 84, 841:842, 85:86
        , 861:863, 87, 871:874, 90:94, 100:101, 1011:1014, 102:103, 1031:1034
        , 104, 1041:1044, 105, 1051:1056, 106:108, 110:112, 1121:1125, 113:116
        , 120:121, 1211:1214, 122, 1221:1224, 123, 1231:1234, 124, 1241:1246
        , 125:129, 130:131, 1311:1313, 132, 1321:1324, 133:138, 1381:1385
        , 139:141, 1411:1414, 142, 1421:1424, 143, 1431:1434, 144, 1441:1444
        , 145, 1451:1454, 150:155, 160:162, 1621:1623, 163:166, 1661:1663
        , 170:171, 1711:1712, 172, 1721:1724, 173:176, 180:182, 1821:1823, 183
        , 1831:1834, 184:186, 190:195, 1951:1952, 196, 200:204, 2041:2042)
    )
  } else if(level == 'quad_class'){
    codes <- factor(0:4)
    levels(codes) <- as.character(0:4)
  }

  ## Subset of event codes
  if(!any('all' %in% code_set)){
    if(sum(!code_set %in% codes) > 0){
      message('Warning: some event codes do not match specified event class. Proceeding with valid event codes.')
    }
    codes <- codes[codes %in% code_set]
    if(length(codes) == 0){
      stop('Please enter a valid set of event codes or quad_class values.')
    }
  }


  ######
  #
  # Parse the Phoenix data
  #
  ######

  ## Drop any missing data
  input_data <- input_data[!is.na(code)]
  input_data <- input_data[!is.na(root_code)]
  input_data <- input_data[!is.na(goldstein)]
  input_data <- input_data[!is.na(quad_class)]


  ######
  #
  # Subset Phoenix data by desired event codes
  #
  ######

  ## Subset events: if a subset of EVENTCODES are specified, keep only that
  ##  set of events and aggregate up from there.
  if(!any('all' %in% code_subset)){
    input_data <- input_data[quad_class %in% code_subset]
  }


  ######################################
  ## IMPORTANT ASSUMPTION HERE:
  ## I am *ASSUMING* that NULL/NA entries after a state code
  ##  implies that the actor is the GOVERNMENT. As such I am replacing
  ##  all such missing entries with 'GOV'.
  ######################################
  input_data[source_root %in%  statelist
               & source_agent == ''
               , source_agent := 'GOV']
  input_data[target_root %in%  statelist
               & target_agent == ''
               , target_agent := 'GOV']


  ######
  #
  # Pre-format data by de-duplicating, cleaning dates and actors,
  # and dropping unused columns
  #
  ######

  ## Subset events and columns: only events that:
  ##  1. involve specified actor set on both side (as ENTITIES)
  ##  2. involve TWO DIFFERENT actors (i.e. no self-interactions
  ##      as specified by user)
  if(('states' %in% actorset)){
    input_data <- input_data[
      source_root %in% statelist & source_agent %in% c('GOV', 'MIL', '')
      & target_root %in% statelist & target_agent %in% c('GOV', 'MIL', '')
      & source_root != target_root
      ]

    ## Set actor codes to state-level factors
    input_data[, source_root := factor(source_root, levels = levels(actors))]
    input_data[, target_root := factor(target_root, levels = levels(actors))]

  } else{
    input_data[source %in% statelist, source := paste0(source_root, source_agent)]
    input_data[target %in% statelist, target := paste0(source_root, target_agent)]
    input_data[, substate_source := substr(source, 1, 6)]
    input_data[, substate_target := substr(target, 1, 6)]
    input_data <- input_data[(substate_source %in% actors
                                & substate_target %in% actors
                                & substate_source != substate_target)]

    input_data[, substate_source := factor(substate_source, levels = levels(actors))]
    input_data[, substate_target := factor(substate_target, levels = levels(actors))]
  }

  ## Aggregate to sum of events by code for time period
  if(level == 'any'){
    input_data[, tie := 1]
  }

  if('states' %in% actorset){
    input_data <- input_data[, c('aid', 'source_root', 'target_root', paste(level), 'goldstein'), with = F]
  } else {
    input_data <- input_data[, c('aid', 'substate_source', 'substate_target', paste(level), 'goldstein'), with = F]
  }
  setnames(input_data, c('aid', 'source', 'target', paste(level), 'goldstein'))


  ## Return as individual arrays for each code, OR as single array with mean
  ## Goldstein score
  if('meangoldstein' %in% tie_type){
    input_data <- input_data[, list(mean(goldstein)), by = list(source, target)]
    setnames(input_data, 'V1', 'mean_goldstein')
    output_data <- matrix(nrow = length(actors), ncol = length(actors), 0)
    dimnames(output_data)[[1]] <- actors
    dimnames(output_data)[[2]] <- actors
    in_data <- as.matrix(input_data[, list(as.integer(source), as.integer(target), mean_goldstein)])
    output_data[in_data[, c(1:2)]] <- in_data[, 3]

  } else if ('sepgoldstein' %in% tie_type){
    input_data[, posneg := 0]
    input_data[goldstein > 0, posneg := 1]
    input_data <- input_data[, list(mean(goldstein)), by = list(source, target, posneg)]
    setnames(input_data, 'V1', 'goldstein')

    output_data <- array(dim = c(length(actors), length(actors), 2), 0)
    dimnames(output_data)[[1]] <- actors
    dimnames(output_data)[[2]] <- actors
    dimnames(output_data)[[3]] <- c('pos_goldstein', 'neg_goldstein')

    in_data_pos <- as.matrix(input_data[posneg == 1, list(as.integer(source), as.integer(target), goldstein)])
    new_slice_pos <- output_data[, , 1]
    new_slice_pos[in_data_pos[, c(1:2)]] <- in_data_pos[,3]
    output_data[, , 1] <- new_slice_pos

    in_data_neg <- as.matrix(input_data[posneg == 0, list(as.integer(source), as.integer(target), goldstein)])
    new_slice_neg <- output_data[, , 2]
    new_slice_neg[in_data_neg[, c(1:2)]] <- in_data_neg[,3]
    output_data[, , 2] <- new_slice_neg


  } else {
    input_data <- input_data[, list(.N), by = c('source', 'target', paste(level))]
    setnames(input_data, 'N', 'eventcount')
    setkeyv(input_data, c('source', 'target', paste(level)))

    output_data <- array(dim = c(length(actors), length(actors), length(codes)), 0)
    dimnames(output_data)[[1]] <- actors
    dimnames(output_data)[[2]] <- actors
    dimnames(output_data)[[3]] <- codes

    for(code in codes){
      mat_data <- as.matrix(input_data[get(level) == code, list(as.integer(source), as.integer(target), eventcount)])
      if ('binary' %in% tie_type){
        mat_data[mat_data[,3] > 0, 3] <- 1
      }
      new_slice <- output_data[, , match(code, codes)]
      new_slice[mat_data[, c(1:2)]] <- mat_data[,3]
      output_data[, , match(code, codes)] <- new_slice
    }
  }

  return(output_data)
}



