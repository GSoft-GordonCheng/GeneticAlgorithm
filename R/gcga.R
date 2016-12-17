# Genetic Algorithm
#
# This is an example function named 'hello'
# which prints 'Hello, world!'.
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Build and Reload Package:  'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'
source("R/GAFlow.R");
gcga <- function(iters           = 10,
                 population.size = 20,
                 chromosome.size = 5,
                 onInit          = NA,
                 onFitness       = NA,
                 onSelection     = NA,
                 onCrossover     = NA,
                 onMutation      = NA,
                 onFinalize      = NA
) {
  print("get parameters...");
  #
  iters <- ifelse(is.na(iters)||iters<1, gcga.config$iters, iters);
  popSize <- ifelse(is.na(population.size)||population.size<1, gcga.config$population.size, population.size);
  chromSize <- ifelse(is.na(chromosome.size)||chromosome.size<1, gcga.config$chromosome.size, chromosome.size);
  onEvent <- list(
    Init = onInit,
    Fitness = onFitness,
    Selection = onSelection,
    Crossover = onCrossover,
    Mutation = onMutation,
    Finalize = onFinalize
  );
  if (!is.function(onEvent$Init)) {
    onEvent$Init <- gcga.config$onInit;
  }
  if (!is.function(onEvent$Fitness)) {
    onEvent$Fitness <- gcga.config$onFitness;
  }
  if (!is.function(onEvent$Selection)) {
    onEvent$Selection <- gcga.config$onSelection;
  }
  if (!is.function(onEvent$Crossover)) {
    onEvent$Crossover <- gcga.config$onCrossover;
  }
  if (!is.function(onEvent$Mutation)) {
    onEvent$Mutation <- gcga.config$onMutation;
  }
  if (!is.function(onEvent$Finalize)) {
    onEvent$Finalize <- gcga.config$onFinalize;
  }
  #
  print("check parameters...");
  if (!is.function(onEvent$Init)) {
    stop("A initialize function must be provided. See the onInit parameter.");
  }
  if (!is.function(onEvent$Fitness)) {
    stop("A fitness function must be provided. See the onFitness parameter.");
  }
  #
  print("set parameters...");
  gcga.config$iters <<- iters;
  gcga.config$population.size <<- popSize;
  gcga.config$chromosome.size <<- chromSize;
  gcga.config$onInit <<- onEvent$Init;
  gcga.config$onFitness <<- onEvent$Fitness;
  gcga.config$onSelection<<- onEvent$Selection;
  gcga.config$onCrossover <<- onEvent$Crossover;
  gcga.config$onMutation <<- onEvent$Mutation;
  gcga.config$onFinalize <<- onEvent$Finalize;
  # eval
  startTime <- Sys.time(); #proc.time();
  print(startTime);
  ds <- gcga.eval();
  result <- list(iters=gcga.config$iters,
                 population.size =gcga.config$population.size,
                 chromosome.size =gcga.config$chromosome.size,
                 dataset=ds
  );
  difTime <- difftime(Sys.time(), startTime, units="auto");
  print(difTime);
  #print(sprintf("end...%s", format(endTime, "%a %b %d %X %Y")));
  # output..............................................................
  class(result) = "gcga";
  return(result);
}







