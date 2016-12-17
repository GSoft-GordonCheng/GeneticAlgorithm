#
# Genetic Algorithm
#
gcga.config <- list(
  iters           = 10,
  population.size = 20,
  chromosome.size = 5,
  onInit          = NA,
  onFitness       = NA,
  onSelection     = NA,
  onCrossover     = NA,
  onMutation      = NA,
  onFinalize      = NA
);
# Elitism

#
gcga.eval <- function(onFinalize = NA
) {
  iters<- gcga.config$iters;
  rows <- gcga.config$population.size;
  cols <- gcga.config$chromosome.size+1;
  # container
  dsIters <- array(NA, dim=c(iters));
  dsPopulation <- matrix(NA, iters*rows, cols);
  dsElitism <- NA; #rep(NA, cols);
  # initialize
  current <- Population.initialize();
  #
  idx <- 1;
  # 1.forEach iters/Generation
  for (iter in 1:iters) {
    # 2.calculate each object
    current <- Population.calculate(current, function(chrom, value){
      # save chromosome object & value
      dsPopulation[idx, 1] <<- value;
      dsPopulation[idx, 2:cols] <<- chrom;
      idx <<- idx + 1;
    });
    dsIters[iter] <- list(current);
    #
    # 3.create the next generation
    if (iter < iters) {
      scope <- dsPopulation[1:idx-1, ];
      current <- Generation.new(iters=iter, template=current, dataset=scope, function(bestObj){
        # for Elitism Policy
        if (!is.na(bestObj)) {
          bestObj <- as.vector(bestObj);
          tempObj <- rbind(bestObj, bestObj);
          if (iter>1) {
            rows <- nrow(dsElitism);
            dsElitism <<- rbind(dsElitism[1:rows, ], tempObj[1, ]);
          } else {
            dsElitism <<- unique(tempObj);
          }
        }
      });
    }
  }
  # callback
  Generation.finalize(dsPopulation, current, onFinalize, function(bestObj){
    # Last Elitism Object
    if (!is.na(bestObj) && (!is.na(dsElitism))) {
      bestObj <- as.vector(bestObj);
      tempObj <- rbind(bestObj, bestObj);
      rows <- nrow(dsElitism);
      dsElitism <<- rbind(dsElitism[1:rows, ], tempObj[1, ]);
    }
  });
  # output
  list(
    Generation = dsIters,
    Population = dsPopulation,
    Elitism = dsElitism
  );
}

#Population.create <- function(rows, cols){
#  rows <- gcga.config$population.size;
#  cols <- gcga.config$chromosome.size + 1;
#  matrix(NA, rows, cols);
#}

Population.initialize <- function(
) {
  rows <- gcga.config$population.size;
  cols <- gcga.config$chromosome.size + 1;
  population <- matrix(NA, rows, cols);#result <- Population.create();
  onInitEvent <- gcga.config$onInit;
  if (is.function(onInitEvent)) {
    for (row in 1:rows) {
      # Get first chromosome object with callback function
      chrom <- onInitEvent();
      population[row, 2:cols] <- as.vector(chrom);
    }
  }
  return(population);
}

Population.calculate <- function(population, callback
) {
  rows <- gcga.config$population.size;
  cols <- gcga.config$chromosome.size+1;
  fitness <- gcga.config$onFitness;
  current <- population;
  for (row in 1:rows) {
    # get chromosome object
    chrom <- current[row, 2:cols];
    # get fitness value
    value <- current[row, 1] <- fitness(chrom);
    # sent message with callback function
    callback(chrom, value);
  }
  return(current);
}

# create new generation
Generation.new <- function(iters,
                           template,
                           dataset,
                           callback
) {
  # clone template into
  ObjN <- template;
  rows <- nrow(ObjN);
  cols <- ncol(ObjN);
  # fill fitness values (column.1)
  ObjN[, 1] <- NA;
  # find this population of generation
  sIdx <- (iters-1)*rows+1;
  eIdx <- sIdx+rows-1;
  population <- dataset[sIdx:eIdx, ]; # this population
  # 1. select chromosomes
  parents <- Generation.selection(dataset=dataset, iters=iters, current=population, callback);
  for (row in 1:rows){
    self <- ObjN[row, 2:cols];
    # 2. crossover chromosomes
    child  <- Generation.crossover(parents, self);
    # 3. mutate chromosome
    chrom <- Generation.mutation(chromosome=child, iters=iters);
    # 4. save new chromosome
    ObjN[row, 2:cols] <- as.vector(chrom);
  }
  return (ObjN);
}

# Selection Adapter/interface
# return chromosome dataset
Generation.selection <- function(dataset=NA,
                                 iters,
                                 current=NA,
                                 callback=NA
) {
  # selection function
  onEvent <- gcga.config$onSelection;
  if (is.function(onEvent)) {
    onEvent(dataset=dataset, iters=iters, current=current, callback);
  } else {
    # default/simple function
    rows <- nrow(dataset);
    cols <- ncol(dataset);
    c2 <- c1 <- dataset[rows, 2:cols];
    if (rows>1) {
      c2 <- dataset[rows-1, 2:cols];
    }
    if (is.function(callback)){
      callback(dataset[rows, ]);
    }
    rbind(c1[1:cols-1], c2[1:cols-1]);
  }
}

# Crossover Adapter/interface
# return a chromosome
Generation.crossover <- function(parents, self
) {
  # crossover function
  family <- rbind(parents, self);
  onEvent <- gcga.config$onCrossover;
  if (is.function(onEvent)) {
    onEvent(dataset=family);
  } else {
    # default
    family[1, ];
  }
}

# Mutation Adapter/interface
# return a chromosome
Generation.mutation <- function(chromosome,
                                iters
) {
  # mutation function
  onEvent <- gcga.config$onMutation;
  if (is.function(onEvent)) {
    onEvent(chromosome, iters);
  } else {
    # not mutate
    chromosome;
  }
}

# Finalize Adapter/interface
Generation.finalize <- function(dataset,
                                current,
                                onFinalize = NA,
                                callback=NA
) {
  # Finalize function
  onEndEvent <- onFinalize;
  if (!is.function(onEndEvent)){
    onEndEvent <- gcga.config$onFinalize;
  }
  if (is.function(onEndEvent)){
    onEndEvent(dataset, current, callback);
 }
}
#
gcga.getChromosome <- function(item, callback=NA) {
  cols <- length(item);
  value <- item[1];
  chrom <- item[2:cols];
  if (is.function(callback)){
    callback(value);
  }
  return(chrom);
}

