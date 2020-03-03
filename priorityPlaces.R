## Everything in this file gets sourced during `simInit()`,
## and all functions and objects are put into the `simList`.
## To use objects, use `sim$xxx` (they are globally available to all modules).
## Functions can be used without `sim$` as they are namespaced to the module,
## just like functions in R packages.
## If exact location is required, functions will be: `sim$<moduleName>$FunctionName`.
defineModule(sim, list(
  name = "priorityPlaces",
  description = paste0("This module has been designed to create a raster of priority places for",
                       " conservation using spatial optimization"),
  keywords = c("priority places", "multispecies"),
  authors = c(person("Tati", "Micheletti", email = "tati.micheletti@gmail.com", role = c("aut", "cre")),
              person("Alex", "Chubaty", email = "achubaty@for-cast.ca", role = "aut")),
  childModules = character(0),
  version = list(SpaDES.core = "1.0.0.9000", priorityPlaces = "0.0.0.9000"),
  timeframe = as.POSIXlt(c(NA, NA)),
  timeunit = "year",
  citation = list("citation.bib"),
  documentation = deparse(list("README.txt", "priorityPlaces.Rmd")),
  reqdPkgs = list("raster", "data.table", "slam", "prioritizr", "crayon"), #"gurobi", 
  parameters = rbind(
    defineParameter(".plotInitialTime", "numeric", end(sim), NA, NA,
                    "Describes the simulation time at which the first plot event should occur."),
    defineParameter(".plotInterval", "numeric", NA, NA, NA,
                    "Describes the simulation time interval between plot events."),
    defineParameter(".saveInitialTime", "numeric", NA, NA, NA,
                    "Describes the simulation time at which the first save event should occur."),
    defineParameter(".saveInterval", "numeric", NA, NA, NA,
                    "This describes the simulation time interval between save events."),
    defineParameter(".useCache", "logical", FALSE, NA, NA,
                    paste0("Should this entire module be run with caching activated?",
                          "This is generally intended for data-type modules, where stochasticity",
                          "and time are not relevant")),
    defineParameter("targets", "numeric", 0.3, NA, NA,
                    paste0("create a problem with targets which specify that we need X% of each",
                          "feature. Can be one value, or exactly the length of the featureID's")),
    defineParameter("penalty", "numeric", NULL, NA, NA,
                    paste0("Penalties that favor combinations of planning units with", 
                          "high connectivity. The connectivity_matrix function will create a",
                          "matrix showing the average strength of connectivity between",
                          "adjacent planning units using the data in the input importantAreas")),
    defineParameter("gap", "numeric", 0.01, NA, NA,
                    paste0("numeric gap to optimality. This gap is relative when solving",
                           " problems using gurobi, and will cause ",
                           "the optimizer to terminate when the difference between the upper and",
                           " lower objective function bounds", 
                           "is less than the gap times the upper bound. For example, a value of ",
                           "0.01 will result in the", 
                           "optimizer stopping when the difference between the bounds is 1 percent ",
                           "of the upper bound.")),
    defineParameter("timeLimit", "numeric", 3600, NA, NA,
                    paste0("numeric time limit in seconds to run the optimizer. The solver will",
                           " return the current best solution when this time limit is exceeded.", 
                           "Default is one hour, 3600 seconds.")),
    defineParameter("presolve", "numeric", 2, NA, NA,
                    paste0("integer number indicating how intensively the solver should ",
                           "try to simplify the problem before solving it. The default ",
                           "value of 2 indicates to that the solver should be very ",
                           "aggressive in trying to simplify the problem.")),
    defineParameter("threads", "numeric | character", "AUTO", NA, NA,
                    paste0("character string AUTO, or integer number of threads ",
                           "to use for the optimization ",
                           "algorithm. The default 'AUTO' calculates the ideal number ", 
                           "of threads based on your system.")),
    defineParameter("verbose", "logical", TRUE, NA, NA,
                    paste0("logical should information be printed while ",
                           "solving optimization problems?")),
    defineParameter("solutions", "numeric", 10, NA, NA,
                    paste0("integer number of attempts to generate different solutions. ",
                           "Defaults to 10")),
    defineParameter("constraintType", "list", "", NA, NA,
                    paste0("Available types of constraints:",
                           "lockedIn: ensure that certain planning units are prioritized in the solution",
                           "neighbor: guarantee a minimum size for each protected area",
                           "contiguity: make sure all units are contiguous",
                           "featureContiguity: make sure all units ",
                           "are contiguous for each feature!",
                           "lockedOut: can be used to lock areas out ",
                           "of the solution. Potentially for",
                           "scenario planning -- i.e. mining")),
    defineParameter("constraintInformation", "list", "", NA, NA,
                    paste0("Needed only for the specific constraints, NAMED with:",
                           "lockedIn: numeric vector, matching the 'id' column from plannigUnit",
                           "neighbor: numeric. Number of minimun neighbours",
                           "contiguity: NULL",
                           "featureContiguity: NULL ",
                           "lockedOut: numeric vector, matching the 'id' column from plannigUnit ")),
    defineParameter("firstFeasible", "logical", FALSE, NA, NA,
                    paste0("logical should the first feasible solution be be returned? ",
                           "If firstFeasible is set to TRUE, the solver will return ",
                           "the first solution it encounters that meets all the constraints, ",
                           "regardless of solution quality. Note that the first feasible ",
                           "solution is not an arbitrary solution, rather it is derived from ",
                           "the relaxed solution, and is therefore often reasonably close ",
                           "to optimality. Defaults to FALSE."))
  ),
  inputObjects = bind_rows(
    expectsInput(objectName = "planningUnit", objectClass = "RasterLayer | data.frame", 
                 desc = paste0("Planning unit is the spatial area (study area) that should be", 
                               "either a raster or data.frame. If the last, calculations",
                               " are faster. If the last, each row in the planning unit table must",
                               " correspond to a different planning unit. The table must also have ",
                               " an 'id' column to provide a unique integer identifier for each",
                               " planning unit (i.e. pixelID -- used as `pu` in featuresData. see below),",
                               " and it must also have columns wit xloc, yloc, and one that",
                               " indicates the cost of each planning unit ('cost'). If the first, the module", 
                               " will convert it to data.frame with the necessary adjustments"),
                 sourceURL = NA),
    expectsInput(objectName = "featuresID", objectClass = "rasterStack | data.frame", 
                 desc = paste0("This is the rasterStack or relative data.frame of the features to be ",
                               "assessed: caribouRSF, specific birds density, species richness, etc",
                               "If a data.frame, feature data must have an 'id' column containing ", 
                               "a unique identifier (i.e. matching 'species' in featuresData), and `name`", 
                               " character name for each feature."),
                 sourceURL = NA),
    expectsInput(objectName = "featuresData", objectClass = "data.frame", 
                 desc = paste0("Only expected if featuresID is a data.frame. Has to have", 
                               "'pu': integer planning unit identifier (corresponds to 'id' in", 
                               "planningUnit)",
                               "'species': integer feature identifier (i.e. 'id' in featuresID)",
                               "'amount': numeric amount of the feature in the planning unit (i.e.",
                               "RSF value, bird densisity or richness index)."),
                 sourceURL = NA),
    expectsInput(objectName = "protectedAreas", objectClass = "RasterLayer | data.frame", 
                 desc = paste0("Raster of protected areas, or table indicating which pixels from",
                               " planningUnit id correspond to protected areas"),
                 sourceURL = NA),
    expectsInput(objectName = "importantAreas", objectClass = "RasterLayer", 
                 desc = paste0("Raster of areas that are of importance for one or more species, ",
                               "(i.e. coming from Indigenous knowldge)",
                               " planningUnit id correspond to penalize solutions that chose these"),
                 sourceURL = NA),
    expectsInput(objectName = "planningUnitRaster", objectClass = "RasterLayer", 
                 desc = paste0("Raster of planning unit to be used to penalize solutions"))
  ),
  outputObjects = bind_rows(
    createsOutput(objectName = "conservationProblem", objectClass = "ConservationProblem-class", 
                  desc = paste0("Defined conservation problem to be solved", 
                                " using spatial optimization")),
    createsOutput(objectName = "priorityAreas", objectClass = "list", 
                  desc = paste0("List of raster layer showing which areas should be prioritized", 
                                " showing optimal/near optimal areas"))
  )
))

## event types
#   - type `init` is required for initialization

doEvent.priorityPlaces = function(sim, eventTime, eventType) {
  switch(
    eventType,
    init = {
      # 1. Checking data: if rasters, need to match. If data frame, need to match with featuresData
      if (is(sim$planningUnit, "RasterLayer")){
        if (any(is(sim$featuresID, "RasterLayer"), is(sim$featuresID, "RasterStack"))){
          # Make sure all layers match, if all raster layers
          tryCatch({
           rstStk <- raster::stack(sim$planningUnit,
                                   sim$featuresID)
           }, error = function(e){
             message(crayon::red("The planningUnit raster's extent, alignment or projection does not match the featuresID raster. A postprocessing of planningUnit will be tried"))
             sim$planningUnit <- reproducible::postProcess(sim$planningUnit, rasterToMatch = sim$featuresID[[1]], format = "GTiff", filename2 = NULL)
           })
          rstStk <- raster::stack(sim$planningUnit,
                                  sim$featuresID)
          if (any(is(rstStk, "RasterStack"),
                  is(rstStk, "RasterBrick")))
            isOK <- TRUE
          if (!isTRUE(isOK))
            stop("The rasters planningUnit and featuresID do not match even after post processing the first. Please make sure these rasters align and try again.")
        } else {
          # featureID is data.frame
          if (!is(sim$featuresID, "data.frame")) {
            stop("'featuresID' needs to be a raster, or data.frame. Shapefile not yet implemented in the SpaDES module")
          }
          # Check if the data.frame has 'id' column: unique identifier (i.e. matching 'species' in featuresData), and `name`"
          if (!all(c("id", "name") %in% names(sim$featuresID))){
            stop("'featuresID' data.frame needs to have both 'id' and 'name' columns")
          }
          # Then check if  featuresData has been supplied. If not, stop
          if (is.null(sim$featuresData)){
            stop("'featuresID' is supplied as data.frame it is necessary to supply 'featuresData' as well")
          }
          # If featuresData has been supplied, test it has the same NROW (id's) as the planning unit's ncell
          if (NROW(sim$featuresData) != raster::ncell(sim$planningUnit))
            stop("'planningUnit' number of cells and 'featuresData' number of rows need to match")
          
          # Then test the names of the data.frame contain: pu, species and amount
          if (!all(c("pu", "species", "amount") %in% names(sim$featuresID)))
            stop("'featuresData' data.frame needs to have: 'pu' (corresponding to 'id' in planningUnit), 
                 'species' (corresponding to 'id' in featuresID) and 'amount' (numeric amount of the feature)")
          
          # Then test if species matches in sim$featuresData matches id in featuresID
          id <- unique(sim$featuresID$id)
          sp <- unique(sim$featuresData$species)
          
          if (!all(id %in% sp))
            stop("'featuresID$id' needs to match 'featuresData$species'")
        }

      } else 
        { # If the planningUnit is NOT a rasterLayer, we can have the features being a rasterLayer or data.frame.
        if (!is(sim$planningUnit, "data.frame")) {
          stop("'planningUnit' needs to be a raster, or data.frame. Shapefile not yet implemented in the SpaDES module")
        }
        # Check if the df has the needed columns
        if (!all(c("id", "xloc", "yloc", "cost") %in% names(sim$planningUnit)))
          stop("'featuresData' data.frame needs to have: 'id' (corresponding to the pixel/unit id), 
               'xloc' and 'yloc' (corresponding to the spatial location) and 'cost' (numeric cost of implementation of conservation unit)")
        
        # Then check what is featuresID. If rasterLayer, check that it has the same ncell that NROW in the data.frame of pU
        if (any(is(sim$featuresID, "RasterLayer"), is(sim$featuresID, "RasterStack"))){
          if (raster::ncell(sim$featuresID) != NROW(sim$planningUnit))
            stop("'featuresID' number of cells and 'planningUnit' number of rows must match")
          
          # Then check if  featuresData has been supplied. If not, stop
          if (is.null(sim$featuresData)){
            stop("'featuresID' is supplied as data.frame it is necessary to supply 'featuresData' as well")
          }

          # If featuresData has been supplied, test it has the same NROW (id's) as the planning unit df
          if (NROW(sim$featuresData) != NROW(sim$planningUnit))
            stop("'planningUnit' number of rows and 'featuresData' number of rows must match")

        } else {
          if (!is(sim$featuresID, "data.frame")) {
            stop("'featuresID' needs to be a raster, or data.frame. Shapefile not yet implemented in the SpaDES module")
          }
          # If featuresID. is a data.frame, check for existing featuresData. If not, stop.
          if (is.null(sim$featuresData)){
            stop("'featuresID' is supplied as data.frame it is necessary to supply 'featuresData' as well")
          }
          
          # If featuresData has been supplied, test it has the same NROW (id's) as the planning unit df
          if (NROW(sim$featuresData) != NROW(sim$planningUnit))
            stop("'planningUnit' number of rows and 'featuresData' number of rows must match")
          
          # If featuresData has been supplied, test it has the same id's as the planning unit
          if (NROW(sim$featuresData) != NROW(sim$planningUnit))
            stop("'planningUnit' number of rows and 'featuresData' number of rows need to match") 

          # Then test if species matches in sim$featuresData matches id in featuresID
          id <- unique(sim$featuresID$id)
          sp <- unique(sim$featuresData$species)
          
          if (!all(id %in% sp))
            stop("'featuresID$id' needs to match 'featuresData$species'")
            
            if (!all(c("pu", "species", "amount") %in% names(sim$featuresData)))
              stop("'featuresData' data.frame needs to have: 'pu' (corresponding to 'id' in planningUnit), 
                   'species' (corresponding to 'id' in featuresID) and 'amount' (numeric amount of the feature)")
        }
      }
      
      # if sim$planningUnit or sim$featuresID is a raster, needs to be extracted to a data.frame:
      # 2a. Convert planningUnit raster to table:

      xy <- coordinates(sim$planningUnit)
      sim$planningUnit <- data.table(id = 1:ncell(sim$planningUnit), 
                                            cost = raster::getValues(sim$planningUnit),
                                            xloc = xy[,"x"],
                                            yloc = xy[,"y"])
      # 'id' column == pixel id
      #  xloc, yloc == pixel location
      #  cost == values

      # 2b. Convert featuresID rasterStack to 2 tables: featuresID and featuresData:
browser() # <~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ HERE
      #  featuresID:
      #  'id' featureID == 'species' in featureData --> NUMERIC
      #  'name' == layers names

      #  featuresData: NROW --> pu * species(feature layers)
      #  'pu' column == PU's 'id' == pixelID
      #  'species' == featuresID numbers
      #  'amount'
        
      sim$featuresID

      sim$featuresData
      
      # 2. 
      
      
      # schedule future event(s)
     # 3. Add paramters to turn on and off each event 
      sim <- scheduleEvent(sim, end(sim), "priorityPlaces", "createProblem")
      sim <- scheduleEvent(sim, end(sim), "priorityPlaces", "setObjectives")
      sim <- scheduleEvent(sim, end(sim), "priorityPlaces", "setTargets")
      sim <- scheduleEvent(sim, end(sim), "priorityPlaces", "addConstraints")
      sim <- scheduleEvent(sim, end(sim), "priorityPlaces", "definePenalties")
      sim <- scheduleEvent(sim, end(sim), "priorityPlaces", "defineDecisionType")
      sim <- scheduleEvent(sim, end(sim), "priorityPlaces", "createPortfolio")
      sim <- scheduleEvent(sim, end(sim), "priorityPlaces", "initializeSolver")
      sim <- scheduleEvent(sim, end(sim), "priorityPlaces", "definePriorityPlaces", 
                           eventPriority = .last())
      sim <- scheduleEvent(sim, P(sim)$.plotInitialTime, "priorityPlaces", "plot", 
                           eventPriority = .last())
    },
    createProblem = {
      
      sim$conservationProblem <- problem(x = sim$planningUnit,
                                         features = sim$featuresID, rij = sim$featuresData)
    },
    setObjectives = {
      # ! ----- EDIT BELOW ----- ! #
      # do stuff for this event
      
      #plotFun(sim) # uncomment this, replace with object to plot
      # schedule future event(s)
      
      # e.g.,
      #sim <- scheduleEvent(sim, time(sim) + P(sim)$.plotInterval, "priorityPlaces", "plot")
      
      # ! ----- STOP EDITING ----- ! #
    },
    
    setTargets = {
      # ! ----- EDIT BELOW ----- ! #
      # do stuff for this event
      
      #plotFun(sim) # uncomment this, replace with object to plot
      # schedule future event(s)
      
      # e.g.,
      #sim <- scheduleEvent(sim, time(sim) + P(sim)$.plotInterval, "priorityPlaces", "plot")
      
      # ! ----- STOP EDITING ----- ! #
    },
    addConstraints = {
      # ! ----- EDIT BELOW ----- ! #
      # do stuff for this event
      
      #plotFun(sim) # uncomment this, replace with object to plot
      # schedule future event(s)
      
      # e.g.,
      #sim <- scheduleEvent(sim, time(sim) + P(sim)$.plotInterval, "priorityPlaces", "plot")
      
      # ! ----- STOP EDITING ----- ! #
    },
    definePenalties = {
      # ! ----- EDIT BELOW ----- ! #
      # do stuff for this event
      
      #plotFun(sim) # uncomment this, replace with object to plot
      # schedule future event(s)
      
      # e.g.,
      #sim <- scheduleEvent(sim, time(sim) + P(sim)$.plotInterval, "priorityPlaces", "plot")
      
      # ! ----- STOP EDITING ----- ! #
    },
    defineDecisionType = {
      # ! ----- EDIT BELOW ----- ! #
      # do stuff for this event
      
      #plotFun(sim) # uncomment this, replace with object to plot
      # schedule future event(s)
      
      # e.g.,
      #sim <- scheduleEvent(sim, time(sim) + P(sim)$.plotInterval, "priorityPlaces", "plot")
      
      # ! ----- STOP EDITING ----- ! #
    },
    createPortfolio = {
      # ! ----- EDIT BELOW ----- ! #
      # do stuff for this event
      
      #plotFun(sim) # uncomment this, replace with object to plot
      # schedule future event(s)
      
      # e.g.,
      #sim <- scheduleEvent(sim, time(sim) + P(sim)$.plotInterval, "priorityPlaces", "plot")
      
      # ! ----- STOP EDITING ----- ! #
    },
    initializeSolver = {
      # ! ----- EDIT BELOW ----- ! #
      # do stuff for this event
      
      #plotFun(sim) # uncomment this, replace with object to plot
      # schedule future event(s)
      
      # e.g.,
      #sim <- scheduleEvent(sim, time(sim) + P(sim)$.plotInterval, "priorityPlaces", "plot")
      
      # ! ----- STOP EDITING ----- ! #
    },
    definePriorityPlaces = {
      # ! ----- EDIT BELOW ----- ! #
      # do stuff for this event
      
      #plotFun(sim) # uncomment this, replace with object to plot
      # schedule future event(s)
      
      # e.g.,
      #sim <- scheduleEvent(sim, time(sim) + P(sim)$.plotInterval, "priorityPlaces", "plot")
      
      # ! ----- STOP EDITING ----- ! #
    },
    plot = {
      # ! ----- EDIT BELOW ----- ! #
      # do stuff for this event
      
      #plotFun(sim) # uncomment this, replace with object to plot
      # schedule future event(s)
      
      # e.g.,
      #sim <- scheduleEvent(sim, time(sim) + P(sim)$.plotInterval, "priorityPlaces", "plot")
      
      # ! ----- STOP EDITING ----- ! #
    },
    warning(paste("Undefined event type: \'", current(sim)[1, "eventType", with = FALSE],
                  "\' in module \'", current(sim)[1, "moduleName", with = FALSE], "\'", sep = ""))
  )
  return(invisible(sim))
}

## event functions
#   - keep event functions short and clean, modularize by calling subroutines from section below.

.inputObjects <- function(sim) {
 
  cacheTags <- c(currentModule(sim), "function:.inputObjects") ## uncomment this if Cache is being used
  dPath <- asPath(getOption("reproducible.destinationPath", dataPath(sim)), 1)
  message(currentModule(sim), ": using dataPath '", dPath, "'.")

  if (!suppliedElsewhere("planningUnit", sim = sim)){
    ras <- raster(ncol = 6, nrow = 6, xmn = -3, xmx = 3, ymn = -3, ymx = 3)
    ras[] <- c(1:6, 10:15, 20:25, 30:35, 40:45, 50:55)
    sim$planningUnit <- ras
  }
  
  if (!suppliedElsewhere("featuresID", sim = sim)){
    ras <- raster(ncol = 10, nrow = 10, xmn = -3, xmx = 3, ymn = -3, ymx = 3)
    crs(ras) <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
    ras  <- gaussMap(ras)
    crs(ras) <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
    names(ras) <- "species1"
    ras2  <- gaussMap(ras)
    crs(ras2) <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
    names(ras2) <- "species2"
    ras3  <- gaussMap(ras)
    names(ras3) <- "species3"
    crs(ras3) <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
    sim$featuresID <- raster::stack(ras, ras2, ras3)
  }
  
  return(invisible(sim))
}
