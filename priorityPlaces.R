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
  reqdPkgs = list("crayon", "parallel", "prioritizr", "raster"
                  ## "slam", "gurobi" ## slam and gurobi are best but OPTIONAL
                  ## TODO: how to add Rsymphony and lpsymphony as optional packages?
  ),
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
                    paste("Should this entire module be run with caching activated?",
                          "This is generally intended for data-type modules, where stochasticity",
                          "and time are not relevant")),
    defineParameter("binaryDecision", "logical", FALSE, NA, NA,
                    paste("Add a binary decision to a conservation planning problem.",
                          "This is the classic decision of either prioritizing or not prioritizing",
                          "a planning unit. Typically, this decision has the assumed action of",
                          "buying the planning unit to include in a protected area network.",
                          "If no decision is added to a problem object, then this decision class",
                          "will be used by default.",
                          "If FALSE, Add a proportion decision to a problem. This is a relaxed",
                          "decision where a part of a planning unit can be prioritized, as opposed",
                          "to the default of the entire planning unit. Typically, this decision has",
                          "the assumed action of buying a fraction of a planning unit to include",
                          "in a protected area network. Generally, problems can be solved much",
                          "faster with proportion-type decisions than binary-type decisions.")),
    defineParameter("constraintType", "list", "", NA, NA,
                    paste0("NAMED list with the following",
                           "add_locked_in_constraints: locked_in = numeric vector, matching the 'id' column from plannigUnit.",
                           "It ensures that certain planning units are prioritized in the solution",
                           "add_contiguity_constraints:  NULL. Make sure all units are contiguous",
                           "add_feature_contiguity_constraints: NULL. Make sure all units ",
                           "are contiguous for each feature!",
                           "add_locked_out_constraints: locked_out = numeric vector, matching the 'id' column from plannigUnit",
                           "Can be used to lock areas out of the solution. Potentially for",
                           "scenario planning -- i.e. mining",
                           "add_neighbor_constraints: k = numeric. Make sure each unit has ",
                           "a minimun number of neighbors")),
    defineParameter("fasterOptimization", "logical", FALSE, NA, NA,
                    paste0("Will convert rasters into tables and speed up optimization ",
                           "with the cost of some constraints not being implemented")),
    defineParameter("firstFeasible", "logical", FALSE, NA, NA,
                    paste0("logical should the first feasible solution be be returned? ",
                           "If firstFeasible is set to TRUE, the solver will return ",
                           "the first solution it encounters that meets all the constraints, ",
                           "regardless of solution quality. Note that the first feasible ",
                           "solution is not an arbitrary solution, rather it is derived from ",
                           "the relaxed solution, and is therefore often reasonably close ",
                           "to optimality. Defaults to FALSE.")),
    defineParameter("gap", "numeric", 0.01, NA, NA,
                    paste0("numeric gap to optimality. This gap is relative when solving",
                           " problems using gurobi, and will cause ",
                           "the optimizer to terminate when the difference between the upper and",
                           " lower objective function bounds",
                           "is less than the gap times the upper bound. For example, a value of ",
                           "0.01 will result in the",
                           "optimizer stopping when the difference between the bounds is 1 percent ",
                           "of the upper bound.")),
    defineParameter("nCores", "numeric", parallel::detectCores() * 0.9, NA, NA,
                    paste0("number of cores to use for optimizer solution")),
    defineParameter("penalty", "numeric", NULL, NA, NA,
                    paste0("Penalties that favor combinations of planning units with",
                          "high connectivity. The connectivity_matrix function will create a",
                          "matrix showing the average strength of connectivity between",
                          "adjacent planning units using the data in the input importantAreas")),
    defineParameter("presolve", "numeric", 2, NA, NA,
                    paste0("integer number indicating how intensively the solver should ",
                           "try to simplify the problem before solving it. The default ",
                           "value of 2 indicates to that the solver should be very ",
                           "aggressive in trying to simplify the problem.")),
    defineParameter("solutions", "numeric", 10, NA, NA,
                    paste0("integer number of attempts to generate different solutions. ",
                           "Defaults to 10")),
    defineParameter("solver", "character", NULL, NA, NA,
                    paste("Which optimizer/solver to use.",
                          "One of 'gurobi', 'rsymphony', 'lpsymphony', or NULL.",
                          "If NULL, attempt to figure out best solver to use. See ?prioritizr::solvers.")),
    defineParameter("targets", "numeric", 0.3, NA, NA,
                    paste0("create a problem with targets which specify that we need X% of each",
                           "feature. Can be one value, or exactly the length of the featureID's")),
    defineParameter("threads", "numeric | character", "AUTO", NA, NA,
                    paste0("character string AUTO, or integer number of threads ",
                           "to use for the optimization ",
                           "algorithm. The default 'AUTO' calculates the ideal number ",
                           "of threads based on your system.")),
    defineParameter("timeLimit", "numeric", 3600, NA, NA,
                    paste0("numeric time limit in seconds to run the optimizer. The solver will",
                           " return the current best solution when this time limit is exceeded.",
                           "Default is one hour, 3600 seconds.")),
    defineParameter("verbose", "logical", FALSE, NA, NA,
                    paste0("logical should information be printed while ",
                           "solving optimization problems?"))
  ),
  inputObjects = bind_rows(
    expectsInput(objectName = "featuresData", objectClass = "data.frame",
                 desc = paste0("Only expected if featuresID is a data.frame. Has to have",
                               "'pu': integer planning unit identifier (corresponds to 'id' in",
                               "planningUnit)",
                               "'species': integer feature identifier (i.e. 'id' in featuresID)",
                               "'amount': numeric amount of the feature in the planning unit (i.e.",
                               "RSF value, bird densisity or richness index)."),
                 sourceURL = NA),
    expectsInput(objectName = "featuresID", objectClass = "rasterStack | data.frame",
                 desc = paste0("This is the rasterStack or relative data.frame of the features to be ",
                               "assessed: caribouRSF, specific birds density, species richness, etc",
                               "If a data.frame, feature data must have an 'id' column containing ",
                               "a unique identifier (i.e. matching 'species' in featuresData), and `name`",
                               " character name for each feature."),
                 sourceURL = NA),
    expectsInput(objectName = "importantAreas", objectClass = "RasterLayer",
                 desc = paste0("Raster of areas that are of importance for one or more species, ",
                               "(i.e. coming from Indigenous knowldge)",
                               " planningUnit id correspond to penalize solutions that chose these",
                               "This will be filtered for non-na values (i.e. important are = 1,",
                               "non-important areas need to be 0"),
                 sourceURL = NA),
    expectsInput(objectName = "planningUnit", objectClass = "RasterLayer",
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
    expectsInput(objectName = "planningUnitRaster", objectClass = "RasterLayer",
                 desc = paste0("Raster of planning unit to be used to penalize solutions.")),
    expectsInput(objectName = "problemEnv", objectClass = "environment",
                 desc = paste0("Environment to contain LP problem. The simList does not",
                               " support it")),
    expectsInput(objectName = "protectedAreas", objectClass = "RasterLayer | data.frame",
                 desc = paste0("Raster of protected areas, or table indicating which pixels from",
                               " planningUnit id correspond to protected areas. If raster, ",
                               "it will filter for non-na values (i.e. all but protected areas need",
                               "to be NA"),
                 sourceURL = NA)
  ),
  outputObjects = bind_rows(
    createsOutput(objectName = "featuresData", objectClass = "data.frame",
                  desc = paste0("Only expected if featuresID is a data.frame. Has to have",
                                "'pu': integer planning unit identifier (corresponds to 'id' in",
                                "planningUnit)",
                                "'species': integer feature identifier (i.e. 'id' in featuresID)",
                                "'amount': numeric amount of the feature in the planning unit (i.e.",
                                "RSF value, bird densisity or richness index).")),
    createsOutput(objectName = "featuresID", objectClass = "data.frame",
                  desc = paste0("This is the rasterStack or relative data.frame of the features to be ",
                                "assessed: caribouRSF, specific birds density, species richness, etc",
                                "If a data.frame, feature data must have an 'id' column containing ",
                                "a unique identifier (i.e. matching 'species' in featuresData), and `name`",
                                " character name for each feature.")),
    createsOutput(objectName = "planningUnit", objectClass = "data.frame",
                  desc = paste0("Planning unit is the spatial area (study area) that should be",
                                "either a raster or data.frame. If the last, calculations",
                                " are faster. If the last, each row in the planning unit table must",
                                " correspond to a different planning unit. The table must also have ",
                                " an 'id' column to provide a unique integer identifier for each",
                                " planning unit (i.e. pixelID -- used as `pu` in featuresData. see below),",
                                " and it must also have columns wit xloc, yloc, and one that",
                                " indicates the cost of each planning unit ('cost'). If the first, the module",
                                " will convert it to data.frame with the necessary adjustments")),
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
      mod$solver <- if(is.null(P(sim)$solver)) {
        tolower(prioritizr:::default_solver_name())
      } else if (tolower(P(sim)$solver) %in% c("gurobi", "rsymphony", "lpsymphony")) {
        tolower(P(sim)$solver)
      } else {
        stop("'solver' must be one of 'gurobi', 'rsymphony', 'lpsymphony' or NULL")
      }

      # schedule future event(s)
      # 3. Add parameters to turn on and off each event
      sim <- scheduleEvent(sim, time(sim), "priorityPlaces", "dataSanityCheck")
      sim <- scheduleEvent(sim, time(sim), "priorityPlaces", "createProblem")
      sim <- scheduleEvent(sim, time(sim), "priorityPlaces", "setObjectives")
      sim <- scheduleEvent(sim, time(sim), "priorityPlaces", "setTargets")
      if (any(P(sim)$constraintType != ""))
        sim <- scheduleEvent(sim, time(sim), "priorityPlaces", "addConstraints")
      if (!is.null(P(sim)$penalty))
        sim <- scheduleEvent(sim, time(sim), "priorityPlaces", "definePenalties")
      sim <- scheduleEvent(sim, time(sim), "priorityPlaces", "defineDecisionType")
      sim <- scheduleEvent(sim, time(sim), "priorityPlaces", "createPortfolio")
      sim <- scheduleEvent(sim, time(sim), "priorityPlaces", "initializeSolver")
      sim <- scheduleEvent(sim, time(sim), "priorityPlaces", "definePriorityPlaces",
                           eventPriority = .last())
      sim <- scheduleEvent(sim, P(sim)$.plotInitialTime, "priorityPlaces", "plot",
                           eventPriority = .last())
    },
    dataSanityCheck = {
      # 1. Checking data: if rasters, need to match. If data frame, need to match with featuresData
      if (is(sim$planningUnit, "RasterLayer")) {
        if (any(is(sim$featuresID[[paste0("Year", time(sim))]][[1]], "RasterLayer"),
                is(sim$featuresID[[paste0("Year", time(sim))]][[1]], "RasterStack"))) {
          # Make sure all layers match, if all raster layers
          tryCatch({
            rstStk <- raster::stack(sim$planningUnit, sim$featuresID[[paste0("Year", time(sim))]])
          }, error = function(e) {
            message(crayon::red(paste0("The planningUnit raster's extent, alignment or projection ",
                                       "does not match the featuresID raster. A postprocessing of ",
                                       "planningUnit will be tried")))
            sim$planningUnit <- Cache(reproducible::postProcess, sim$planningUnit,
                                      destinationPath = dataPath(sim),
                                      rasterToMatch = sim$featuresID[[paste0("Year", time(sim))]][[1]],
                                      filename2 = NULL,
                                      userTags = c("init", "goal:aligningPUandFID"))
          })
          rstStk <- raster::stack(sim$planningUnit,
                                  sim$featuresID[[paste0("Year", time(sim))]])
          if (any(is(rstStk, "RasterStack"), is(rstStk, "RasterBrick")))
            isOK <- TRUE
          if (!isTRUE(isOK))
            stop(paste0("The rasters planningUnit and featuresID do not match even after post ",
                        "processing the first. Please make sure these rasters align and try again."))
        } else {
          # featureID is data.frame
          if (!is(sim$featuresID[[paste0("Year", time(sim))]], "data.frame")) {
            stop("'featuresID' needs to be a raster, or data.frame. Shapefile not yet implemented in the SpaDES module")
          }
          # Check if the data.frame has 'id' column: unique identifier (i.e. matching 'species' in featuresData), and `name`"
          if (!all(c("id", "name") %in% names(sim$featuresID[[paste0("Year", time(sim))]]))) {
            stop("'featuresID' data.frame needs to have both 'id' and 'name' columns")
          }
          # Then check if  featuresData has been supplied. If not, stop
          if (is.null(sim$featuresData)) {
            stop("'featuresID' is supplied as data.frame it is necessary to supply 'featuresData' as well")
          }
          # If featuresData has been supplied, test it has the same NROW (id's) as the planning unit's ncell
          if (NROW(sim$featuresData) != raster::ncell(sim$planningUnit))
            stop("'planningUnit' number of cells and 'featuresData' number of rows need to match")

          # Then test the names of the data.frame contain: pu, species and amount
          if (!all(c("pu", "species", "amount") %in% names(sim$featuresID[[paste0("Year", time(sim))]])))
            stop("'featuresData' data.frame needs to have: 'pu' (corresponding to 'id' in planningUnit),
                 'species' (corresponding to 'id' in featuresID) and 'amount' (numeric amount of the feature)")

          # Then test if species matches in sim$featuresData matches id in featuresID
          id <- unique(sim$featuresID[[paste0("Year", time(sim))]]$id)
          sp <- unique(sim$featuresData$species)

          if (!all(id %in% sp))
            stop("'featuresID$id' needs to match 'featuresData$species'")
        }

      } else { # If the planningUnit is NOT a rasterLayer, we can have the features being a rasterLayer or data.frame.
        if (!is(sim$planningUnit, "data.frame")) {
          stop(paste0("'planningUnit' needs to be a raster, or data.frame. Shapefiles or other formats ",
               "not yet implemented in the SpaDES module"))
        }
        # Check if the df has the needed columns
        if (!all(c("id", "xloc", "yloc", "cost") %in% names(sim$planningUnit)))
          stop(paste0("'featuresData' data.frame needs to have: 'id' (corresponding to the ",
                      "pixel/unit id), 'xloc' and 'yloc' (corresponding to the spatial location) ",
                      "and 'cost' (numeric cost of implementation of conservation unit)"))

        # Then check what is featuresID. If rasterLayer, check that it has the same ncell
        # that NROW in the data.frame of pU
        if (any(is(sim$featuresID[[paste0("Year", time(sim))]], "RasterLayer"),
                is(sim$featuresID[[paste0("Year", time(sim))]], "RasterStack"))) {
          if (raster::ncell(sim$featuresID[[paste0("Year", time(sim))]]) != NROW(sim$planningUnit))
            stop("'featuresID' number of cells and 'planningUnit' number of rows must match")

          # Then check if  featuresData has been supplied. If not, stop
          if (is.null(sim$featuresData)) {
            stop("'featuresID' is supplied as data.frame it is necessary to supply 'featuresData' as well")
          }

          # If featuresData has been supplied, test it has the same NROW (id's) as the planning unit df
          if (NROW(sim$featuresData) != NROW(sim$planningUnit))
            stop("'planningUnit' number of rows and 'featuresData' number of rows must match")
        } else {
          if (!is(sim$featuresID[[paste0("Year", time(sim))]], "data.frame")) {
            stop("'featuresID' needs to be a raster, or data.frame. Shapefile not yet implemented in the SpaDES module")
          }
          # If featuresID. is a data.frame, check for existing featuresData. If not, stop.
          if (is.null(sim$featuresData)) {
            stop("'featuresID' is supplied as data.frame it is necessary to supply 'featuresData' as well")
          }

          # If featuresData has been supplied, test it has the same NROW (id's) as the planning unit df
          if (NROW(sim$featuresData) != NROW(sim$planningUnit))
            stop("'planningUnit' number of rows and 'featuresData' number of rows must match")

          # If featuresData has been supplied, test it has the same id's as the planning unit
          if (NROW(sim$featuresData) != NROW(sim$planningUnit))
            stop("'planningUnit' number of rows and 'featuresData' number of rows need to match")

          # Then test if species matches in sim$featuresData matches id in featuresID
          id <- unique(sim$featuresID[[paste0("Year", time(sim))]]$id)
          sp <- unique(sim$featuresData$species)

          if (!all(id %in% sp))
            stop("'featuresID$id' needs to match 'featuresData$species'")

          if (!all(c("pu", "species", "amount") %in% names(sim$featuresData)))
            stop(paste0("'featuresData' data.frame needs to have: 'pu' (corresponding to 'id' in",
                        " planningUnit), 'species' (corresponding to 'id' in featuresID) and ",
                        "'amount' (numeric amount of the feature)"))
        }
      }

      # 1b. If sim$planningUnit is a raster, and sim$planningUnitRaster is not matching it, overwrite
      tryCatch({
        raster::stack(sim$planningUnit, sim$planningUnitRaster)
      }, error = function(e) {
        message(crayon::red(paste0("planningUnitRaster does not match sim$planningUnit. The first will ",
                                   "be replaced by the last")))
        sim$planningUnitRaster <- sim$planningUnit
      })

      # if sim$planningUnit or sim$featuresID is a raster, needs to be extracted to a data.frame:
      # 2a. Convert planningUnit raster to table:
      # 'id' column == pixel id
      #  xloc, yloc == pixel location
      #  cost == values

      if (P(sim)$fasterOptimization) {
        message(crayon::yellow(paste0("fasterOptimization is TRUE. Certain contraints ",
                                      "as contiguity and neighbor ",
                                      "will be ignored if 'data' is not passed")))
        xy <- coordinates(sim$planningUnit)
        sim$planningUnit <- data.frame(id = 1:ncell(sim$planningUnit),
                                       cost = raster::getValues(sim$planningUnit),
                                       xloc = xy[, "x"],
                                       yloc = xy[, "y"])

        # 2b. Convert featuresID rasterStack to 2 tables: featuresID and featuresData:
        #  featuresData:
        # NROW --> pu * species(feature layers)
        #  'pu' column == PU's 'id' == pixelID
        #  'species' == featuresID numbers
        #  'amount'
        amountTable <- data.frame(pu = 1:ncell(sim$featuresID[[paste0("Year", time(sim))]]),
                                  getValues(sim$featuresID[[paste0("Year", time(sim))]]))
        names(amountTable) <- c("pu", 1:raster::nlayers(sim$featuresID[[paste0("Year", time(sim))]]))
        amountTableMelted <- reshape2::melt(amountTable, id.vars = "pu")
        amountTableMelted[["variable"]] <- as.numeric(amountTableMelted[["variable"]])
        names(amountTableMelted)[names(amountTableMelted) == "variable"] <- "species"
        names(amountTableMelted)[names(amountTableMelted) == "value"] <- "amount"
        sim$featuresData <- amountTableMelted
        sim$featuresData$amount[is.na(sim$featuresData$amount)] <- 0 # Amount can't be 0, so we convert NA's into 0's
        #  featuresID:
        #  'id' featureID == 'species' in featureData --> NUMERIC
        #  'name' == layers names
        sim$featuresID[[paste0("Year", time(sim))]] <- data.frame(name = names(sim$featuresID[[paste0("Year", time(sim))]]),
                                     id = 1:raster::nlayers(sim$featuresID[[paste0("Year", time(sim))]]))
      }

      # Converting threads from AUTO to optimal number of threads:
      if (P(sim)$threads == "AUTO")
        params(sim)$priorityPlaces$threads <- floor(P(sim)$nCores)
    },
    createProblem = {
      sim$problemEnv <- new.env(parent = emptyenv())
      if (P(sim)$fasterOptimization) {
      assign("conservationProblem", value = problem(x = sim$planningUnit,
                                                    features = sim$featuresID[[paste0("Year", time(sim))]],
                                                    rij = sim$featuresData,
                                                    cost_column = "cost"),
             envir = sim$problemEnv)

      } else {
        assign("conservationProblem", value = problem(x = sim$planningUnit,
                                                      features = sim$featuresID[[paste0("Year", time(sim))]]),
               envir = sim$problemEnv)
      }
    },
    setObjectives = {
      # Minimum set objective: Minimize the cost of the solution whilst ensuring
      # that all targets are met (Rodrigues et al. 2000). This objective is similar to that used in Marxan
      # (Ball et al. 2009). For example, we can add a minimum set objective to a problem using the
      # following code. It is possible to add more objectives with time.
      conservationProblem <- get("conservationProblem", envir = sim$problemEnv)
      assign("conservationProblem", value = add_min_set_objective(conservationProblem),
             envir = sim$problemEnv)
    },
    setTargets = {
      # Rules/Targets:
      # create a problem with targets which specify that we need 10 % of the habitat
      # for the first feature, 15 % for the second feature, 20 % for the third feature
      # 25 % for the fourth feature and 30 % of the habitat for the fifth feature
      # Assertions
      if (length(P(sim)$targets) != NROW(sim$featuresID[[paste0("Year", time(sim))]]))
        stop("Length of targets needs to match the length of features")
      conservationProblem <- get("conservationProblem", envir = sim$problemEnv)
      assign("conservationProblem",
             value = add_relative_targets(conservationProblem, P(sim)$targets),
             envir = sim$problemEnv)
    },
    addConstraints = {
      lapply(names(P(sim)$constraintType), function(const) {
        conservationProblem <- get("conservationProblem", envir = sim$problemEnv)
        fun <- get(const)
        args <- P(sim)$constraintType[[const]]
        args$x <- conservationProblem
        tryCatch({
          assign("conservationProblem", value = do.call(fun, args = args),
                 envir = sim$problemEnv)
        }, error = function(e) {
          message(crayon::red(paste0(const, " will not be implemented. Argument 'data' needed ",
                                     "when P(sim)$fastOptimization == TRUE")))
        })
      })
    },
    definePenalties = {
      conservationProblem <- get("conservationProblem", envir = sim$problemEnv)
      tryCatch({
        raster::stack(sim$planningUnitRaster, sim$importantAreas)
      }, error = function(e) {
        message(crayon::red(paste0("The importantAreas raster's extent, alignment or projection ",
                                   "does not match the planningUnitRaster. A postprocessing of ",
                                   "importantAreas will be tried")))
        sim$importantAreas <- Cache(reproducible::postProcess, sim$importantAreas,
                                    destinationPath = dataPath(sim),
                                    rasterToMatch = sim$planningUnitRaster,
                                    filename2 = NULL,
                                    userTags = c("event:definePenalties", "goal:aligningIAandPUR"))
      })
      assign("conservationProblem",
             value = add_boundary_penalties(conservationProblem,
                                            penalty = P(sim)$penalty,
                                            data = connectivity_matrix(sim$planningUnitRaster,
                                                                       sim$importantAreas)),
             envir = sim$problemEnv)
    },
    defineDecisionType = {
      conservationProblem <- get("conservationProblem", envir = sim$problemEnv)
      if (P(sim)$binaryDecision) {
        assign("conservationProblem", value = add_binary_decisions(conservationProblem),
               envir = sim$problemEnv)
      } else {
        assign("conservationProblem", value = add_proportion_decisions(conservationProblem),
               envir = sim$problemEnv)
      }
    },
    createPortfolio = {
      conservationProblem <- get("conservationProblem", envir = sim$problemEnv)

      if (P(sim)$solver == "gurobi") {
        # There are various methods for constructing the solution pool, but in most cases, setting
        # the method argument to 2 is recommended because this will mean that the solution pool will
        # contain a set number of solutions that are nearest to optimality (e.g. the top 10 solutions
        # nearest to optimality). This method is the fastest for generating a portfolio of solutions,
        # but it requires that the Gurobi optimization solver to be specified for solving problems.
        assign("conservationProblem", value = add_pool_portfolio(conservationProblem,
                                                                 method = 2,
                                                                 number_solutions = P(sim)$solutions),
               envir = sim$problemEnv)
      } else {
        # Generate a portfolio of solutions by randomly reordering the data prior to attempting to
        # solve the problem. If the Gurobi optimization solver is not available, this method is the
        # fastest method for generating a set number of solutions within a specified distance from optimality.
        assign("conservationProblem", value = add_shuffle_portfolio(conservationProblem,
                                                                    number_solutions = P(sim)$solutions,
                                                                    remove_duplicates = FALSE),
               envir = sim$problemEnv)
      }
    },
    initializeSolver = {
      conservationProblem <- get("conservationProblem", envir = sim$problemEnv)

      if (P(sim)$solver == "gurobi") {
        assign("conservationProblem", value = add_gurobi_solver(conservationProblem,
                                                                gap = P(sim)$gap,
                                                                time_limit = P(sim)$timeLimit,
                                                                presolve = P(sim)$presolve,
                                                                threads = P(sim)$threads,
                                                                first_feasible = P(sim)$firstFeasible,
                                                                verbose = P(sim)$verbose),
               envir = sim$problemEnv)
      } else if (P(sim)$solver == "rsymphony") {
        assign("conservationProblem", value = add_rsymphony_solver(conservationProblem,
                                                                   gap = P(sim)$gap,
                                                                   time_limit = P(sim)$timeLimit,
                                                                   #presolve = P(sim)$presolve,
                                                                   #threads = P(sim)$threads,
                                                                   first_feasible = P(sim)$firstFeasible,
                                                                   verbose = P(sim)$verbose),
               envir = sim$problemEnv)
      } else if (P(sim)$solver == "lpsymphony") {
        assign("conservationProblem", value = add_lpsymphony_solver(conservationProblem,
                                                                    gap = P(sim)$gap,
                                                                    time_limit = P(sim)$timeLimit,
                                                                    #presolve = P(sim)$presolve,
                                                                    #threads = P(sim)$threads,
                                                                    first_feasible = P(sim)$firstFeasible,
                                                                    verbose = P(sim)$verbose),
               envir = sim$problemEnv)
      }
    },
    definePriorityPlaces = {
      conservationProblem <- get("conservationProblem", envir = sim$problemEnv)
      sim$priorityAreas <- prioritizr::solve(conservationProblem)
      solutionsVector <- names(sim$priorityAreas)[grep(names(sim$priorityAreas),
                                                       pattern = "solution")]
      priorityAreasList <- lapply(solutionsVector, function(solutionNumber) {
        if (P(sim)$fasterOptimization) {
          rasSolution <- setValues(x = sim$planningUnitRaster,
                                   values = sim$priorityAreas[[solutionNumber]])
        } else {
          rasSolution <- sim$priorityAreas[[solutionNumber]]
        }
        names(rasSolution) <- solutionNumber
        return(rasSolution)
      })
      sim$priorityAreas <- priorityAreasList
      names(sim$priorityAreas) <- solutionsVector
    },
    plot = {
      quickPlot::Plot(sim$priorityAreas)
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

  if (!suppliedElsewhere("planningUnit", sim = sim)) {
    ras <- raster(ncol = 6, nrow = 6, xmn = -3, xmx = 3, ymn = -3, ymx = 3)
    ras[] <- c(1:6, 10:15, 20:25, 30:35, 40:45, 50:55)
    sim$planningUnit <- ras
  }

  if (!suppliedElsewhere("planningUnitRaster", sim = sim)) {
    if (!is(sim$planningUnit, "RasterLayer"))
      stop(paste0("If planningUnit is NOT a RasterLayer ",
                  "you need to provide planningUnitRaster"))
    sim$planningUnitRaster <- sim$planningUnit
  }

  if (!is.null(P(sim)$penalty)) {
    if (!suppliedElsewhere("importantAreas", sim = sim)) {
      if (!is(sim$planningUnitRaster, "RasterLayer"))
        stop(paste0("If planningUnitRaster is NULL, or not a RasterLayer, and penalty is not NULL ",
                    "you need to provide importantAreas"))
      sim$importantAreas <- sim$planningUnitRaster
      sim$importantAreas[] <- NA
      sim$importantAreas[runif(10, 1:ncell(sim$planningUnit))] <- 1
    }
  }
  if (!suppliedElsewhere("featuresID", sim = sim)) {
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
