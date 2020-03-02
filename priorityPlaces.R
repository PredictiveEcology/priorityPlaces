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
  reqdPkgs = list("raster", "data.table", "slam", "gurobi", "prioritizr"),
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
    defineParameter("penalty", "numeric", 5, NA, NA,
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
    defineParameter("constraintType", "character", "", NA, NA, # CHECK HRE. NEED ANOTHER PARAMETER TO DEAL WITH ALL THESE OPTIONS
                    paste0("Available types of constraints:",
                           "neighbor: guarantee a minimum size for each protected area",
                           "contiguity: make sure all units are contiguous",
                           "featureContiguity: make sure all units ",
                           "are contiguous for each feature!",
                           "lockedOut: can be used to lock areas out ",
                           "of the solution. Potentially for",
                           "scenario planning -- i.e. mining")),
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
                               "either a raster, shapefile or data.frame. If the last, calculations",
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
      ### check for more detailed object dependencies:
      ### (use `checkObject` or similar)

      # do stuff for this event
      # 1. Make sure all layers
      # 2. Extract needed tables. This module should NOT have the option of working with rasters...
      # 3. Add paramters to turn on and off each event! Use ifelse(P(sim)$addConstraints, start(sim), NA)
      # 4. 

      # schedule future event(s)
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
  # Any code written here will be run during the simInit for the purpose of creating
  # any objects required by this module and identified in the inputObjects element of defineModule.
  # This is useful if there is something required before simulation to produce the module
  # object dependencies, including such things as downloading default datasets, e.g.,
  # downloadData("LCC2005", modulePath(sim)).
  # Nothing should be created here that does not create a named object in inputObjects.
  # Any other initiation procedures should be put in "init" eventType of the doEvent function.
  # Note: the module developer can check if an object is 'suppliedElsewhere' to
  # selectively skip unnecessary steps because the user has provided those inputObjects in the
  # simInit call, or another module will supply or has supplied it. e.g.,
  # if (!suppliedElsewhere('defaultColor', sim)) {
  #   sim$map <- Cache(prepInputs, extractURL('map')) # download, extract, load file from url in sourceURL
  # }

  #cacheTags <- c(currentModule(sim), "function:.inputObjects") ## uncomment this if Cache is being used
  dPath <- asPath(getOption("reproducible.destinationPath", dataPath(sim)), 1)
  message(currentModule(sim), ": using dataPath '", dPath, "'.")

  # ! ----- EDIT BELOW ----- ! #

  # ! ----- STOP EDITING ----- ! #
  return(invisible(sim))
}
